package econetcons

import org.jgrapht.graph.{DefaultDirectedGraph, DefaultEdge}

import scala.jdk.CollectionConverters.*
import scala.util.Random

/**
 * Individual-based energy flow (abundanceModel = "emergent"). A population is n individuals of its
 * species' body mass M at a point:
 *
 *   supply    S_j = TE · n_j · m_j                    (TE = transfer efficiency)
 *   demand    D_i = n_i · M_i^0.75                    (Kleiber)
 *   foraging  d_ij = how i spreads D_i over its prey (Diet; WorldParameters.foragingRule):
 *               supply : d_ij = D_i · S_j / Σ_k S_k                 (∝ what each prey population offers)
 *               equal  : D_i split equally over the prey SPECIES present in reach, then ∝ S_j within a species
 *               niche  : as equal, species weighted by a Gaussian niche preference (Diet.niche)
 *   split     each prey's S_j is shared out entirely, in proportion to d_ij over its predators
 *   intake    I_i = Σ_j S_j · d_ij / Σ_k d_kj
 *   size      n_i = min(cap_i, ⌊I_i / M_i^0.75⌋)      (individuals the intake sustains)
 *
 * Basal populations keep their size. Consumers are relaxed to that fixed point (worklist,
 * Gauss-Seidel); then, while any consumer has fewer than n_min individuals, the one furthest below
 * (ties broken at random) is removed and the rest re-relaxed — a predator freed from competition
 * grows back towards its cap (rescue). Mutates `web`.
 */
object EnergyFlow:

  /**
   * How a consumer spreads its demand over its prey. bySpecies = false: ∝ prey supply (rule "supply").
   * bySpecies = true: over prey species present in reach in proportion to pref(predator, prey species),
   * then ∝ supply among the populations of each prey species.
   */
  final case class Diet(rule: String, bySpecies: Boolean, pref: (Species, Species) => Double)

  object Diet:
    val supply: Diet = Diet("supply", false, (_, _) => 1.0)
    val equal: Diet = Diet("equal", true, (_, _) => 1.0)

    /**
     * Niche-model preference. A predator's metaweb prey are exactly the species whose niche value
     * (linear in body mass) lies in its feeding interval, so its centre c and half-width h are recovered
     * from its prey masses (midpoint and half-span; exact up to one niche-value gap). Preference
     * exp(−u²/(2·0.5²)), u = (m_prey − c)/h ∈ [−1, 1]: 1 at the centre, e^−2 ≈ 0.14 at the edges.
     */
    def niche(metaWeb: DefaultDirectedGraph[Species, DefaultEdge]): Diet =
      val interval: Map[Int, (Double, Double)] = metaWeb.vertexSet().asScala.iterator.flatMap { sp =>
        val ms = metaWeb.outgoingEdgesOf(sp).asScala.toSeq.map(e => metaWeb.getEdgeTarget(e).bodySize)
        if ms.isEmpty then None else Some(sp.id -> ((ms.min + ms.max) / 2, (ms.max - ms.min) / 2))
      }.toMap
      Diet("niche", true, (pred, prey) =>
        interval.get(pred.id) match
          case Some((c, h)) if h > 0 => val u = (prey.bodySize - c) / h; math.exp(-u * u / (2 * 0.25))
          case _ => 1.0)

    def apply(wp: WorldParameters, metaWeb: DefaultDirectedGraph[Species, DefaultEdge]): Diet = wp.foragingRule match
      case "supply" => supply
      case "equal"  => equal
      case "niche"  => niche(metaWeb)
      case other    => throw new IllegalArgumentException(s"unknown foragingRule '$other'")

  /** Demand split of a consumer with demand `demand` over prey `ps` (supplies `sup`), per `diet`. */
  private[econetcons] def split(pred: Species, ps: IndexedSeq[Population], sup: IndexedSeq[Double], demand: Double,
      diet: Diet): IndexedSeq[Double] =
    if !diet.bySpecies then
      val tot = sup.sum
      sup.map(x => if tot > 0 then demand * x / tot else 0.0)
    else
      val sBySp = ps.indices.groupMapReduce(k => ps(k).species)(sup)(_ + _)
      val prefs = sBySp.collect { case (sp, sTot) if sTot > 0 => sp -> diet.pref(pred, sp) }
      val w = prefs.values.sum
      ps.indices.map { k =>
        val sp = ps(k).species
        if w > 0 && sBySp(sp) > 0 then demand * prefs(sp) / w * sup(k) / sBySp(sp) else 0.0
      }

  final case class Result(individuals: Map[Int, Double], removed: Set[Int], removals: Int, converged: Boolean)

  def relax(
    web: DefaultDirectedGraph[Population, DefaultEdge],
    cap: Population => Double,
    init: Population => Double,
    te: Double,
    nMin: Double,
    order: Population => Double,
    rnd: Random,
    diet: Diet = Diet.supply,
  ): Result =
    val vs = web.vertexSet().asScala.toArray.sortBy(_.id)
    val n = vs.length
    val index = vs.zipWithIndex.toMap
    val mass = vs.map(_.species.bodySize)
    val basal = vs.map(_.species.trophicLevel == 0)
    val capA = vs.map(cap)
    val ind = vs.map(init)
    val alive = Array.fill(n)(true)
    // prey of each population, grouped by species (contiguous), with each group's diet preference
    // (grouping only under by-species diets, so the supply rule keeps its edge order and summation order)
    val prey = vs.map { p =>
      val ps = web.outgoingEdgesOf(p).asScala.toArray.map(e => index(web.getEdgeTarget(e)))
      if diet.bySpecies then ps.sortBy(j => (vs(j).species.id, j)) else ps }
    val groupOf = prey.map { ps =>
      val g = new Array[Int](ps.length); var c = -1
      for k <- ps.indices do { if k == 0 || vs(ps(k)).species.id != vs(ps(k - 1)).species.id then c += 1; g(k) = c }
      g }
    val groupPref = vs.indices.map { i =>
      val ps = prey(i); val g = groupOf(i)
      val nG = if ps.isEmpty then 0 else g.last + 1
      val pr = new Array[Double](nG)
      for k <- ps.indices do pr(g(k)) = diet.pref(vs(i).species, vs(ps(k)).species)
      pr }.toArray
    val pred = vs.map(p => web.incomingEdgesOf(p).asScala.toArray.map(e => index(web.getEdgeSource(e))))
    val d = prey.map(ps => new Array[Double](ps.length))
    val dTot = new Array[Double](n)
    def s(j: Int) = if alive(j) then te * ind(j) * mass(j) else 0.0

    val queue = new java.util.ArrayDeque[Int]()
    val queued = new Array[Boolean](n)
    def enqueue(i: Int): Unit = if alive(i) && !basal(i) && !queued(i) then { queued(i) = true; queue.add(i) }

    // Re-spread consumer i's demand over its prey; co-predators of any prey whose total changed are dirty.
    def refresh(i: Int): Unit =
      val ps = prey(i)
      var sPrey = 0.0
      var k = 0
      while k < ps.length do { sPrey += s(ps(k)); k += 1 }
      val demand = if alive(i) then ind(i) * math.pow(mass(i), 0.75) else 0.0
      val g = groupOf(i)
      val pr = groupPref(i)
      val sG = if diet.bySpecies then new Array[Double](pr.length) else null
      var wPresent = 0.0
      if diet.bySpecies then
        k = 0
        while k < ps.length do { sG(g(k)) += s(ps(k)); k += 1 }
        var q = 0
        while q < pr.length do { if sG(q) > 0.0 then wPresent += pr(q); q += 1 }
      k = 0
      while k < ps.length do
        val j = ps(k)
        val dn =
          if !diet.bySpecies then (if sPrey > 0.0 then demand * s(j) / sPrey else 0.0)
          else if wPresent > 0.0 && sG(g(k)) > 0.0 then demand * pr(g(k)) / wPresent * s(j) / sG(g(k))
          else 0.0
        val delta = dn - d(i)(k)
        if delta != 0.0 then
          dTot(j) += delta
          d(i)(k) = dn
          if math.abs(delta) > 1e-12 * math.max(dTot(j), 1e-300) then pred(j).foreach(c => if c != i then enqueue(c))
        k += 1

    def target(i: Int): Double =
      val ps = prey(i)
      var intake = 0.0
      var k = 0
      while k < ps.length do
        val j = ps(k)
        if dTot(j) > 0.0 then intake += s(j) * d(i)(k) / dTot(j)
        k += 1
      math.min(capA(i), math.floor(intake / math.pow(mass(i), 0.75)))

    // Size of i changed: its own demand split (which also moves its own share, so i is re-queued),
    // its co-predators' shares, and the supply its predators see.
    def changed(i: Int): Unit =
      refresh(i)
      enqueue(i)
      prey(i).foreach(j => pred(j).foreach(c => if c != i then enqueue(c)))
      pred(i).foreach(enqueue)

    val consumers = (0 until n).filter(i => !basal(i)).sortBy(i => (order(vs(i)), vs(i).id)).toArray
    val removed = scala.collection.mutable.ArrayBuffer.empty[Int]
    // Non-viable alive consumers keyed by (ind / n_min, position in `consumers`): the first key is the
    // worst, and equal ratios come out in consumer order (exactly the tie list a full scan would build).
    val pos = new Array[Int](n)
    consumers.indices.foreach(k => pos(consumers(k)) = k)
    val nonViable = new java.util.TreeSet[(Double, Int)](
      (a: (Double, Int), b: (Double, Int)) => { val c = java.lang.Double.compare(a._1, b._1); if c != 0 then c else Integer.compare(a._2, b._2) })
    val key = new Array[Double](n)   // ratio under which i is in nonViable (NaN = not in it)
    java.util.Arrays.fill(key, Double.NaN)
    def track(i: Int): Unit =
      if !key(i).isNaN then { nonViable.remove((key(i), pos(i))); key(i) = Double.NaN }
      if alive(i) && !basal(i) then
        val r = ind(i) / nMin
        if r < 1.0 then { key(i) = r; nonViable.add((r, pos(i))) }
    // n ≤ cap, so a consumer capped below n_min can never be viable: drop them all at once.
    consumers.foreach(i => if capA(i) < nMin then { alive(i) = false; ind(i) = 0.0; removed += i })
    consumers.foreach(refresh)
    queue.clear(); java.util.Arrays.fill(queued, false)

    val maxSteps = 200L * math.max(1, n)
    var steps = 0L
    var converged = true
    def settle(): Unit =
      while !queue.isEmpty && steps < maxSteps do
        val i = queue.poll(); queued(i) = false; steps += 1
        if alive(i) then
          refresh(i)
          val t = target(i)
          if t != ind(i) then { ind(i) = t; track(i); changed(i) }
      if !queue.isEmpty then converged = false

    consumers.foreach(track)
    consumers.foreach(enqueue)
    settle()

    var go = true
    while go do
      if nonViable.isEmpty then go = false
      else
        val worst = nonViable.first()._1
        val ties = nonViable.tailSet((worst, -1)).iterator().asScala.takeWhile(_._1 == worst).map(t => consumers(t._2)).toArray
        val i = if ties.length == 1 then ties.head else ties(rnd.nextInt(ties.length))
        alive(i) = false
        ind(i) = 0.0
        track(i)
        removed += i
        changed(i)
        settle()

    removed.foreach(i => web.removeVertex(vs(i)))
    val individuals = (0 until n).iterator.filter(alive).map(i => vs(i).id -> ind(i)).toMap
    Result(individuals, removed.iterator.map(vs(_).id).toSet, removed.size, converged)

  /** Intake I_i of every consumer in `web` for population sizes `ind` (individuals; checks and diagnostics). */
  def intakes(web: DefaultDirectedGraph[Population, DefaultEdge], ind: Int => Double, te: Double,
      diet: Diet = Diet.supply): Map[Int, Double] =
    val vs = web.vertexSet().asScala.toSeq
    def preyOf(p: Population) = web.outgoingEdgesOf(p).asScala.toIndexedSeq.map(web.getEdgeTarget)
    val d = demandSplits(web, ind, te, diet)
    val dTot = d.groupMapReduce(_._1._2)(_._2)(_ + _)
    vs.filter(_.species.trophicLevel != 0).map { p =>
      p.id -> preyOf(p).map { q =>
        val t = dTot.getOrElse(q.id, 0.0)
        if t > 0 then te * ind(q.id) * q.species.bodySize * d((p.id, q.id)) / t else 0.0
      }.sum
    }.toMap

  /**
   * Realized (flow-based) trophic position of every population: basal = 1, consumer
   * TP_i = 1 + Σ_j w_ij TP_j with w_ij = share of i's intake that comes from prey j (Levine on the realized
   * energy flows). Also returns the share of each consumer's intake that comes straight from basal prey.
   */
  /** d_ij for every consumer–prey pair of `web` (population sizes `ind`), per `diet`. */
  private def demandSplits(web: DefaultDirectedGraph[Population, DefaultEdge], ind: Int => Double, te: Double,
      diet: Diet): Map[(Int, Int), Double] =
    web.vertexSet().asScala.toSeq.filter(_.species.trophicLevel != 0).flatMap { p =>
      val ps = web.outgoingEdgesOf(p).asScala.toIndexedSeq.map(web.getEdgeTarget)
      val sup = ps.map(q => te * ind(q.id) * q.species.bodySize)
      val ds = split(p.species, ps, sup, ind(p.id) * math.pow(p.species.bodySize, 0.75), diet)
      ps.indices.map(k => (p.id, ps(k).id) -> ds(k))
    }.toMap

  def flowTrophicPositions(web: DefaultDirectedGraph[Population, DefaultEdge], ind: Int => Double, te: Double,
      diet: Diet = Diet.supply): (Map[Int, Double], Map[Int, Double]) =
    val vs = web.vertexSet().asScala.toSeq
    def preyOf(p: Population) = web.outgoingEdgesOf(p).asScala.toSeq.map(web.getEdgeTarget)
    def sup(q: Population) = te * ind(q.id) * q.species.bodySize
    val consumers = vs.filter(_.species.trophicLevel != 0)
    val d = demandSplits(web, ind, te, diet)
    val dTot = d.groupMapReduce(_._1._2)(_._2)(_ + _)
    // weights w_ij (normalized intake shares)
    val w: Map[Int, Seq[(Population, Double)]] = consumers.map { p =>
      val c = preyOf(p).map { q => val t = dTot.getOrElse(q.id, 0.0); q -> (if t > 0 then sup(q) * d((p.id, q.id)) / t else 0.0) }
      val tot = c.map(_._2).sum
      p.id -> (if tot > 0 then c.map((q, x) => (q, x / tot)) else Seq.empty)
    }.toMap
    val tp = scala.collection.mutable.HashMap.from(vs.map(p => p.id -> 1.0))
    var delta = Double.MaxValue
    var it = 0
    while delta > 1e-9 && it < 10000 do
      delta = 0.0
      consumers.foreach { p =>
        val ws = w(p.id)
        val t = if ws.isEmpty then Double.NaN else 1.0 + ws.map((q, x) => x * tp(q.id)).sum
        if !t.isNaN then delta = math.max(delta, math.abs(t - tp(p.id)))
        tp(p.id) = t
      }
      it += 1
    val basalShare = consumers.map(p => p.id -> w(p.id).filter(_._1.species.trophicLevel == 0).map(_._2).sum).toMap
    (tp.toMap, basalShare)

  /**
   * Seed potential (individuals) of each population: what it could sustain with all its reachable
   * lower-level prey to itself, computed up the trophic positions (basal = their fixed size).
   * Consumers whose potential is below n_min can never be viable and get 0.
   */
  def seedCaps(web: DefaultDirectedGraph[Population, DefaultEdge], tp: Map[Int, Double], basalIndividuals: Population => Double,
      te: Double, nMin: Double): Map[Int, Double] =
    val caps = scala.collection.mutable.HashMap.empty[Int, Double]
    web.vertexSet().asScala.toSeq.sortBy(p => (tp(p.species.id), p.id)).foreach { p =>
      if p.species.trophicLevel == 0 then caps(p.id) = basalIndividuals(p)
      else
        val intake = web.outgoingEdgesOf(p).asScala.iterator.map(web.getEdgeTarget)
          .filter(q => tp(q.species.id) < tp(p.species.id))
          .map(q => te * caps.getOrElse(q.id, 0.0) * q.species.bodySize).sum
        val c = math.floor(intake / math.pow(p.species.bodySize, 0.75))
        caps(p.id) = if c >= nMin then c else 0.0
    }
    caps.toMap

end EnergyFlow
