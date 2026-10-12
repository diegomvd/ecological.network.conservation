package econetcons

import org.jgrapht.graph.{DefaultDirectedGraph, DefaultEdge}
import scala.jdk.CollectionConverters.*
import scala.util.Random

/**
 * Single-run observables for a conservation simulation, designed to be called once per
 * (parameter point, seed, strategy-pair) by an OpenMOLE exploration which handles
 * replication and aggregation. `runPair` builds ONE world for a seed and evaluates both
 * the SpeciesRichness and InteractionRichness strategies on that identical world, so the
 * two are directly comparable and the strategy-pair metrics (Jaccard, rank correlation,
 * complementarity, representation) are well defined.
 *
 * All outcome rates are expressed as % of the POST-PHASE-0 baseline (the stabilized world
 * before any strategy-driven extinction), so the initial-stabilization loss common to all
 * strategies does not contaminate the comparison. Phase-0 loss is reported separately.
 */
object Observables:

  // ---- network helpers -------------------------------------------------------

  private def connectance(vCount: Int, eCount: Int): Double =
    val denom = vCount.toDouble * (vCount - 1)
    if denom <= 0 then 0.0 else eCount / denom

  /** Species-level realized network (unique species pairs) as (V, E). */
  private def speciesNet(w: World): (Int, Int) =
    val g = w.getRealizedWeb
    (g.vertexSet().size(), g.edgeSet().size())

  /** Population network as (V, E) counting only non-extinct populations and their edges. */
  private def popNet(w: World): (Int, Int) =
    val web = w.populationWeb
    val aliveV = web.vertexSet().asScala.count(!_.extinct)
    val aliveE = web.edgeSet().asScala.count { e =>
      !web.getEdgeSource(e).extinct && !web.getEdgeTarget(e).extinct
    }
    (aliveV, aliveE)

  private def aliveSpecies(w: World): Set[Int] =
    w.populations.iterator.filter(!_.extinct).map(_.species.id).toSet

  private def aliveSpeciesObjs(w: World): Set[Species] =
    w.populations.iterator.filter(!_.extinct).map(_.species).toSet

  // ---- Spearman rank correlation --------------------------------------------

  private def ranks(xs: Seq[Double]): Seq[Double] =
    // average ranks (ties shared) — standard for Spearman
    val sorted = xs.zipWithIndex.sortBy(_._1)
    val r = Array.ofDim[Double](xs.size)
    var i = 0
    while i < sorted.size do
      var j = i
      while j + 1 < sorted.size && sorted(j + 1)._1 == sorted(i)._1 do j += 1
      val avgRank = (i + j) / 2.0 + 1.0
      var k = i
      while k <= j do { r(sorted(k)._2) = avgRank; k += 1 }
      i = j + 1
    r.toSeq

  private def spearman(a: Seq[Double], b: Seq[Double]): Double =
    if a.size < 2 || a.size != b.size then 0.0
    else pearson(ranks(a), ranks(b))

  private def pearson(a: Seq[Double], b: Seq[Double]): Double =
    val n = a.size
    val ma = a.sum / n; val mb = b.sum / n
    var num = 0.0; var da = 0.0; var db = 0.0
    var i = 0
    while i < n do
      val xa = a(i) - ma; val xb = b(i) - mb
      num += xa * xb; da += xa * xa; db += xb * xb
      i += 1
    if da <= 0 || db <= 0 then 0.0 else num / math.sqrt(da * db)

  // ---- single-shock robustness ----------------------------------------------

  /**
   * Remove `frac` of the currently-alive populations of `finalWorld` at random, run the
   * cascade once, and return the ADDITIONAL loss triggered as (dSpecies%, dSpeciesInt%,
   * dPopInt%), each relative to the finalWorld alive counts. Higher = less robust.
   * Uses a dedicated Random so the knockout is independent of the strategy that produced
   * the network (reproducible from `shockSeed`).
   */
  private def robustnessShock(finalWorld: World, frac: Double, shockSeed: Long): (Double, Double, Double) =
    val shocked = finalWorld.deepCopyForExtinction(new Random(shockSeed))
    val web = shocked.populationWeb
    val alive = web.vertexSet().asScala.filter(!_.extinct).toVector
    if alive.isEmpty then return (0.0, 0.0, 0.0)

    val nRemove = math.round(alive.size * frac).toInt
    val rnd = new Random(shockSeed)
    val toRemove = rnd.shuffle(alive).take(nRemove).toSet

    val spBefore = aliveSpecies(shocked).size
    val (_, spIntBefore) = speciesNet(shocked)
    val (_, popIntBefore) = popNet(shocked)

    // Knock out the chosen populations, then let the cascade settle.
    web.removeAllVertices(toRemove.asJavaCollection)
    val knocked = shocked.copy(populations = shocked.populations.map(p =>
      if toRemove.exists(r => r.id == p.id && r.species.id == p.species.id) then p.extinguish else p))
    val (settled, _) = knocked.phase2Cascades()

    val spAfter = aliveSpecies(settled).size
    val (_, spIntAfter) = speciesNet(settled)
    val (_, popIntAfter) = popNet(settled)

    def pct(before: Int, after: Int): Double = if before > 0 then (before - after).toDouble / before * 100.0 else 0.0
    (pct(spBefore, spAfter), pct(spIntBefore, spIntAfter), pct(popIntBefore, popIntAfter))

  // ---- per-strategy outcome --------------------------------------------------

  /** Body-size quartile thresholds from the initial species pool (ids -> quartile 1..4). */
  private def bodyQuartiles(species: Seq[Species]): Map[Int, Int] =
    val sorted = species.sortBy(_.bodySize)
    val n = sorted.size
    sorted.zipWithIndex.map { case (s, i) =>
      val q = math.min(4, (i * 4) / math.max(1, n) + 1)
      s.id -> q
    }.toMap

  case class StrategyOutcome(
    protectedTiles: Set[Int],
    metrics: Map[String, Double]
  )

  /**
   * Run one strategy on a shared, freshly-built base world (before its protection plan is
   * applied). Returns the protected-tile set plus all per-strategy observables.
   */
  private def runStrategy(
    baseWorld: World,
    strat: ConservationStrategy,
    frac: Double,
    seed: Long,
    bodyQ: Map[Int, Int]
  ): StrategyOutcome =
    val cp = ConservationParameters(fractionProtected = frac, strategy = Some(strat))
    // Apply this strategy's protection plan on a copy of the shared base landscape.
    val planned = baseWorld.copy(
      managementLandscape = baseWorld.managementLandscape.applyProtectionPlan(cp, new Random(seed)),
      conservationParameters = cp
    ).deepCopyForExtinction(new Random(seed))

    val protectedTiles = planned.managementLandscape.managementAreas
      .collect { case a if a.status == ProtectionStatus.Protected => a.id }.toSet

    // Phase 0 baseline (stabilize), then primary, then secondary — mirroring extinctionsDetailed
    // but keeping the intermediate worlds so we can read both network levels at each phase.
    val (stable, _) = planned.phase2Cascades()
    val baseSp   = aliveSpecies(stable)
    val (baseSpV, baseSpE) = speciesNet(stable)
    val (basePopV, basePopE) = popNet(stable)
    val baseSpN = baseSp.size

    val afterPrimary = stable.primaryExtinctions()
    val primSp   = aliveSpecies(afterPrimary)
    val (_, primSpE) = speciesNet(afterPrimary)
    val (_, primPopE) = popNet(afterPrimary)

    val (finalW, nCasc) = afterPrimary.phase2Cascades()
    val finalSp  = aliveSpecies(finalW)
    val (finSpV, finSpE) = speciesNet(finalW)
    val (finPopV, finPopE) = popNet(finalW)

    def pctOf(base: Int, now: Int): Double = if base > 0 then (base - now).toDouble / base * 100.0 else 0.0

    // Species loss, primary/secondary/total (relative to post-Phase-0 baseline)
    val spExtPrimary   = pctOf(baseSpN, primSp.size)
    val spExtSecondary = if primSp.size > 0 then (primSp.size - finalSp.size).toDouble / baseSpN * 100.0 else 0.0
    val spExtTotal     = pctOf(baseSpN, finalSp.size)

    val spIntExtPrimary   = pctOf(baseSpE, primSpE)
    val spIntExtSecondary = if baseSpE > 0 then (primSpE - finSpE).toDouble / baseSpE * 100.0 else 0.0
    val spIntExtTotal     = pctOf(baseSpE, finSpE)

    val popIntExtPrimary   = pctOf(basePopE, primPopE)
    val popIntExtSecondary = if basePopE > 0 then (primPopE - finPopE).toDouble / basePopE * 100.0 else 0.0
    val popIntExtTotal     = pctOf(basePopE, finPopE)

    // Stratified species extinction (extinct within bin / bin size at baseline)
    val extinctSpecies = baseSp.diff(finalSp)
    val allSpeciesObjs = stable.populations.iterator.filter(!_.extinct).map(_.species).toSet
    def trophicExt(level: Int): Double =
      val inLevel = allSpeciesObjs.filter(_.trophicLevel == level).map(_.id)
      if inLevel.isEmpty then 0.0 else inLevel.count(extinctSpecies.contains).toDouble / inLevel.size * 100.0
    def bodyExt(q: Int): Double =
      val inQ = allSpeciesObjs.filter(s => bodyQ.getOrElse(s.id, -1) == q).map(_.id)
      if inQ.isEmpty then 0.0 else inQ.count(extinctSpecies.contains).toDouble / inQ.size * 100.0

    // Robustness of the FINAL surviving network to random population knockout.
    // Each shock does a deepCopy + full cascade settle; 2 shocks × N strategies = the dominant
    // cost at large home ranges. Callers that don't need robustness can skip it via
    // -Decn.skipRobustness=true (robust_* columns are then emitted as NaN).
    val skipRobustness = sys.props.get("econetcons.skipRobustness").contains("true")
    val (r10sp, r10spi, r10pi, r30sp, r30spi, r30pi) =
      if skipRobustness then
        (Double.NaN, Double.NaN, Double.NaN, Double.NaN, Double.NaN, Double.NaN)
      else
        val (a, b, c) = robustnessShock(finalW, 0.10, seed * 31 + 10)
        val (d, e, f) = robustnessShock(finalW, 0.30, seed * 31 + 30)
        (a, b, c, d, e, f)

    val m = Map(
      "phase0_sp_lost"        -> (initialAliveSpecies(planned) - baseSpN).toDouble,
      "sp_ext_primary"        -> spExtPrimary,
      "sp_ext_secondary"      -> spExtSecondary,
      "sp_ext_total"          -> spExtTotal,
      "spint_ext_primary"     -> spIntExtPrimary,
      "spint_ext_secondary"   -> spIntExtSecondary,
      "spint_ext_total"       -> spIntExtTotal,
      "popint_ext_primary"    -> popIntExtPrimary,
      "popint_ext_secondary"  -> popIntExtSecondary,
      "popint_ext_total"      -> popIntExtTotal,
      "dConnectance_species"  -> (connectance(finSpV, finSpE) - connectance(baseSpV, baseSpE)),
      "dConnectance_pop"      -> (connectance(finPopV, finPopE) - connectance(basePopV, basePopE)),
      "sp_ext_basal"          -> trophicExt(0),
      "sp_ext_intermediate"   -> trophicExt(1),
      "sp_ext_top"            -> trophicExt(2),
      "sp_ext_bodyQ1"         -> bodyExt(1),
      "sp_ext_bodyQ2"         -> bodyExt(2),
      "sp_ext_bodyQ3"         -> bodyExt(3),
      "sp_ext_bodyQ4"         -> bodyExt(4),
      "robust_sp_ext_10"      -> r10sp,
      "robust_spint_ext_10"   -> r10spi,
      "robust_popint_ext_10"  -> r10pi,
      "robust_sp_ext_30"      -> r30sp,
      "robust_spint_ext_30"   -> r30spi,
      "robust_popint_ext_30"  -> r30pi,
      "nCascades"             -> nCasc.toDouble,
      "baseline_sp"           -> baseSpN.toDouble,
      "baseline_spint"        -> baseSpE.toDouble,
      "baseline_popint"       -> basePopE.toDouble,
    )
    StrategyOutcome(protectedTiles, m)

  private def initialAliveSpecies(w: World): Int =
    w.populations.iterator.filter(!_.extinct).map(_.species.id).toSet.size

  // ---- multi-strategy driver -------------------------------------------------

  /**
   * The strategies compared within a single sample. All are evaluated on the SAME base
   * world (identical metaweb + spatial structure), so per-strategy outcomes and every
   * pairwise metric (Jaccard, rank correlation, complementarity) are directly comparable.
   * Labels prefix the emitted columns. `scoreOf` is the per-tile score the strategy ranks
   * on, used for the pairwise Spearman rank correlation.
   */
  final case class StrategySpec(
    label: String,
    strategy: ConservationStrategy,
    scoreOf: (ManagementArea, World, Map[Int, Population]) => Double
  )

  val defaultStrategies: Seq[StrategySpec] = Seq(
    StrategySpec("SR", ConservationStrategy.SpeciesRichness,
      (a, _, _) => a.getSpeciesRichness.toDouble),
    StrategySpec("IR", ConservationStrategy.InteractionRichness,
      (a, w, idToPop) => a.getInteractionRichness(w.populationWeb, idToPop).toDouble),
    StrategySpec("EIR", ConservationStrategy.EgocentricInteractionRichness,
      (a, w, _) => a.getEgocentricInteractionRichness(w.populationWeb).toDouble),
  )

  /** Strategies compared in the paper: SR vs egocentric interaction richness, labelled "IR". */
  val paperStrategies: Seq[StrategySpec] = Seq(
    defaultStrategies.head,
    StrategySpec("IR", ConservationStrategy.EgocentricInteractionRichness,
      (a, w, _) => a.getEgocentricInteractionRichness(w.populationWeb).toDouble),
  )

  /**
   * Build one world for `seed`, evaluate every strategy in `strategies` on that identical
   * world, and return a flat map of all observables: per-strategy columns prefixed with the
   * strategy label, plus pairwise columns prefixed `pair_<A>_<B>_`. Intended to be the
   * OpenMOLE task body for one replicate (sweeping strategies inside a single sample keeps
   * them on the exact same metaweb and spatial structure).
   */
  def run(
    numberOfSpecies: Int,
    connectanceMetaWeb: Double,
    nPopulations: Int,
    medianHomeRange: Double,
    avgPopulationsPerTile: Int,
    preyPerPredator: Double,
    fractionProtected: Double,
    seed: Long,
    strategies: Seq[StrategySpec] = defaultStrategies,
    epsilon: Double = 0.0,
    // Optional Saltelli inputs — default to the WorldParameters defaults so existing
    // positional callers (PPPCheck, TileStrategySweep) are unaffected.
    extinctionProbabilityUnprotected: Double = 0.5,
    bodySizeSigma: Double = 3.0,
    meanPatchesPerSpecies: Double = 5.0,
    // Assembly / cascade model (WorldParameters.abundanceModel / cascadeModel); defaults = legacy behaviour.
    abundanceModel: String = "damuth",
    transferEfficiency: Double = 0.1,
    cascadeModel: String = "metabolic",
    energyTolerance: Double = 1.0,
  ): Map[String, Double] =
    val wp = WorldParameters(
      numberOfSpecies = numberOfSpecies,
      connectanceMetaWeb = connectanceMetaWeb,
      nPopulations = nPopulations,
      medianHomeRange = medianHomeRange,
      avgPopulationsPerTile = avgPopulationsPerTile,
      preyPerPredator = preyPerPredator,
      epsilon = epsilon,
      extinctionProbabilityUnprotected = extinctionProbabilityUnprotected,
      bodySizeSigma = bodySizeSigma,
      meanPatchesPerSpecies = meanPatchesPerSpecies,
      abundanceModel = abundanceModel,
      transferEfficiency = transferEfficiency,
      cascadeModel = cascadeModel,
      energyTolerance = energyTolerance,
    )
    // Shared base world (protection plan NOT yet applied — applyProtectionPlan runs inside
    // runStrategy on a copy of this landscape, so all strategies start from the same world).
    val baseCp = ConservationParameters(fractionProtected = 0.0, strategy = Some(ConservationStrategy.SpeciesRichness))
    val baseWorld = World(wp, baseCp, new Random(seed))

    val allSpecies = baseWorld.populations.map(_.species).distinct
    val bodyQ = bodyQuartiles(allSpecies)

    val outcomes: Seq[(StrategySpec, StrategyOutcome)] =
      strategies.map(s => s -> runStrategy(baseWorld, s.strategy, fractionProtected, seed, bodyQ))

    val areas = baseWorld.managementLandscape.managementAreas
    val idToPop = baseWorld.populationWeb.vertexSet().asScala.map(p => p.id -> p).toMap

    def speciesOf(tiles: Set[Int]): Set[Int] =
      areas.iterator.filter(a => tiles.contains(a.id)).flatMap(_.getSpecies).toSet
    def interactionsOf(tiles: Set[Int]): Set[(Int, Int)] =
      areas.iterator.filter(a => tiles.contains(a.id)).flatMap(_.getInteractions(baseWorld.populationWeb)).toSet

    val totalSpecies = areas.flatMap(_.getSpecies).toSet
    val totalInts    = areas.flatMap(a => a.getInteractions(baseWorld.populationWeb)).toSet
    def safePct(n: Int, d: Int): Double = if d > 0 then n.toDouble / d * 100.0 else 0.0

    // Per-strategy columns (label-prefixed) + coverage of the protected set.
    val perStrategy: Map[String, Double] = outcomes.flatMap { case (spec, out) =>
      val sp = speciesOf(out.protectedTiles)
      val in = interactionsOf(out.protectedTiles)
      val cover = Map(
        s"${spec.label}_cover_species" -> safePct(sp.size, totalSpecies.size),
        s"${spec.label}_cover_ints"    -> safePct(in.size, totalInts.size),
      )
      out.metrics.map { case (k, v) => s"${spec.label}_$k" -> v } ++ cover
    }.toMap

    // Pairwise metrics for every unordered strategy pair.
    val pairwise: Map[String, Double] = (
      for
        i <- outcomes.indices
        j <- (i + 1) until outcomes.size
      yield
        val (specA, outA) = outcomes(i)
        val (specB, outB) = outcomes(j)
        val pfx = s"pair_${specA.label}_${specB.label}_"

        val scoreA = areas.map(a => specA.scoreOf(a, baseWorld, idToPop))
        val scoreB = areas.map(a => specB.scoreOf(a, baseWorld, idToPop))

        val inter = outA.protectedTiles.intersect(outB.protectedTiles).size.toDouble
        val union = outA.protectedTiles.union(outB.protectedTiles).size.toDouble

        val spA = speciesOf(outA.protectedTiles); val spB = speciesOf(outB.protectedTiles)
        val inA = interactionsOf(outA.protectedTiles); val inB = interactionsOf(outB.protectedTiles)

        Seq(
          s"${pfx}rankCorr_spearman" -> spearman(scoreA, scoreB),
          s"${pfx}jaccard_protected" -> (if union == 0 then 1.0 else inter / union),
          s"${pfx}uniqueSpecies_${specA.label}" -> spA.diff(spB).size.toDouble,
          s"${pfx}uniqueSpecies_${specB.label}" -> spB.diff(spA).size.toDouble,
          s"${pfx}uniqueInts_${specA.label}"    -> inA.diff(inB).size.toDouble,
          s"${pfx}uniqueInts_${specB.label}"    -> inB.diff(inA).size.toDouble,
        )
    ).flatten.toMap

    perStrategy ++ pairwise

  // Stable column order for CSV emission.
  def columns(sample: Map[String, Double]): Seq[String] = sample.keys.toSeq.sorted
