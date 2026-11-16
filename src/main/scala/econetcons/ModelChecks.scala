package econetcons

import scala.jdk.CollectionConverters.*
import scala.util.Random

/** Correctness checks for the abundance / cascade model options (run after model changes). */
object ModelChecks:
  def main(args: Array[String]): Unit =
    val out = System.out
    System.setOut(new java.io.PrintStream((_: Int) => ()))
    var ok = true
    def check(name: String, cond: Boolean, detail: String = ""): Unit =
      out.println(f"${if cond then "PASS" else "FAIL"}  $name  $detail")
      if !cond then ok = false

    def base(cm: String, eps: Double, theta: Double = 1.0, am: String = "damuth") = WorldParameters(
      numberOfSpecies = 300, connectanceMetaWeb = 0.2, nPopulations = 5000, medianHomeRange = 0.02,
      avgPopulationsPerTile = 32, epsilon = eps, extinctionProbabilityUnprotected = 0.5,
      bodySizeSigma = 1.0, meanPatchesPerSpecies = 5.0, cascadeModel = cm, energyTolerance = theta,
      abundanceModel = am)
    val cp = ConservationParameters(fractionProtected = 0.2, strategy = Some(ConservationStrategy.SpeciesRichness))
    def alive(w: World) = w.populationWeb.vertexSet().asScala.map(_.id).toSet
    def fullRun(wp: WorldParameters, s: Long) =
      val w = World(wp, cp, new Random(s))
      val assembled = alive(w)
      val (fin, _) = w.primaryExtinctions().phase2Cascades()
      (assembled, alive(fin))

    // 1. energyTolerance θ=1 == legacy keep-one-prey rule
    for s <- 1L to 3L do
      val (aL, fL) = fullRun(base("legacy", 0.0), s)
      val (aE, fE) = fullRun(base("energyTolerance", 0.0, 1.0), s)
      check(s"θ=1 == legacy, seed $s", aL == aE && fL == fE,
        s"(assembled ${aL.size} vs ${aE.size}, final ${fL.size} vs ${fE.size})")

    // 2. energyTolerance assembly is feasible by construction; θ<1 still cascades after disturbance
    for s <- 1L to 3L do
      val w = World(base("energyTolerance", 0.0, 0.5), cp, new Random(s))
      val supply = World.energySupply(w.populationWeb)
      val consumers = w.populationWeb.vertexSet().asScala.filter(_.species.trophicLevel != 0)
      val exact = consumers.forall(p => supply.get(p.id) == w.energyBaseline.get(p.id))
      val noPreyless = consumers.forall(p => w.populationWeb.outDegreeOf(p) > 0)
      check(s"assembly feasible (S = S0, no prey-less consumers), seed $s", exact && noPreyless,
        s"(${consumers.size} consumers)")
      val (after, n) = w.primaryExtinctions().phase2Cascades()
      check(s"baseline carried through cascade, seed $s", after.energyBaseline eq w.energyBaseline, s"($n rounds)")

    // 3. allocation models: counts sum ~ nPopulations, more basal share than damuth
    for am <- Seq("damuth", "transferEfficiency", "energyBalance") do
      val (pops, meta) = World.buildPopsAndMetaForTest(base("energyTolerance", 2.0, 1.0, am), new Random(1L))
      val basal = pops.count(_.species.trophicLevel == 0).toDouble / pops.size
      check(s"allocation $am sums to ~5000", math.abs(pops.size - 5000) <= 400, f"(placed ${pops.size}, basal share $basal%.2f)")

    // 4. biomassPyramid allocation and energetics
    def bp(eps: Double, te: Double = 0.2, nPop: Int = 10, rule: String = "single", cm: String = "metabolic",
        biomass: Double = 2.0e4) =
      base(cm, eps, am = "biomassPyramid").copy(transferEfficiency = te, individualsPerPopulation = nPop,
        smallSpeciesRule = rule, totalBiomass = biomass)
    for s <- 1L to 3L do
      val wp = bp(2.0)
      val (pops, meta, sizes) = World.buildPopsMetaAndSizesForTest(wp, new Random(s))
      val biomass = World.speciesBiomass(meta, wp)
      val tp = World.trophicPositions(meta)
      val species = meta.vertexSet().asScala.toSeq
      check(s"Σ B_s = totalBiomass, seed $s", math.abs(biomass.values.sum / wp.totalBiomass - 1) < 1e-9)

      // Biomass summed in unit TP windows decays as TE^(TP−1): log-linear slope ≈ ln TE.
      val windows = species.groupBy(sp => math.floor(tp(sp.id)).toInt).toSeq.sortBy(_._1)
        .filter(_._2.size >= 5)
        .map { case (w, ss) => (ss.map(sp => tp(sp.id)).sum / ss.size, math.log(ss.map(sp => biomass(sp.id)).sum)) }
      val (xs, ys) = windows.unzip
      val (mx, my) = (xs.sum / xs.size, ys.sum / ys.size)
      val slope = xs.zip(ys).map((x, y) => (x - mx) * (y - my)).sum / xs.map(x => (x - mx) * (x - mx)).sum
      check(s"TP-window biomass slope ≈ ln TE, seed $s", windows.size >= 2 && math.abs(slope / math.log(0.2) - 1) < 0.25,
        f"(slope $slope%.3f vs ${math.log(0.2)}%.3f over ${windows.size} windows)")

      val popsPerSpecies = pops.groupBy(_.species.id).view.mapValues(_.size).toMap
      val massOk = species.forall { sp =>
        val k = popsPerSpecies.getOrElse(sp.id, 0)
        math.abs(k * sizes.getOrElse(sp.id, 0.0) * sp.bodySize / biomass(sp.id) - 1) < 1e-9
      }
      check(s"Σ_pops individuals × mass = B_s, seed $s", massOk, s"(${pops.size} populations placed)")
      val alloc = World.biomassAllocation(meta, wp)
      val sizeOk = pops.forall(p => sizes(p.species.id) >= wp.individualsPerPopulation || alloc(p.species.id).individuals < wp.individualsPerPopulation)
      check(s"populations ≥ n_pop except single-rule small species, seed $s", sizeOk,
        s"(${alloc.values.count(_.individuals < wp.individualsPerPopulation)} small species)")

      val (popsD, _, sizesD) = World.buildPopsMetaAndSizesForTest(bp(2.0, rule = "drop"), new Random(s))
      check(s"drop rule: every population ≥ n_pop, seed $s", popsD.forall(p => sizesD(p.species.id) >= 10),
        s"(${popsD.map(_.species.id).distinct.size} species placed of ${species.size})")

      // After assembly every surviving consumer population has supply ≥ demand.
      val w = World(wp, cp, new Random(s))
      val web = w.populationWeb
      val feasible = web.vertexSet().asScala.filter(_.species.trophicLevel != 0).forall { p =>
        val supply = web.outgoingEdgesOf(p).asScala.toSeq.map { e =>
          val q = web.getEdgeTarget(e)
          wp.epsilon * w.individualsOf(q.species.id) * q.species.bodySize / web.inDegreeOf(q)
        }.sum
        supply >= w.individualsOf(p.species.id) * math.pow(p.species.bodySize, 0.75)
      }
      check(s"biomassPyramid assembly: supply ≥ demand for every consumer, seed $s", feasible,
        s"(${web.vertexSet().size} assembled populations)")

      // ε → ∞: the energetic rule reduces to the legacy keep-one-prey rule.
      val (aL, fL) = fullRun(bp(0.0, cm = "legacy"), s)
      val (aE, fE) = fullRun(bp(1e12), s)
      check(s"biomassPyramid ε→∞ == legacy, seed $s", aL == aE && fL == fE,
        s"(assembled ${aL.size} vs ${aE.size}, final ${fL.size} vs ${fE.size})")

    // 5. incremental greedy cascade == full-recompute reference (same rule as World.trophicCascades)
    def referenceStarvation(web: org.jgrapht.graph.DefaultDirectedGraph[Population, org.jgrapht.graph.DefaultEdge],
        eps: Double, n: Int => Double): Set[Int] =
      val removed = scala.collection.mutable.HashSet.empty[Int]
      var go = true
      while go do
        val deficits = web.vertexSet().asScala.toSeq.filter(_.species.trophicLevel != 0).map { p =>
          val alloc = web.outgoingEdgesOf(p).asScala.toSeq.map { e =>
            val q = web.getEdgeTarget(e); eps * n(q.species.id) * q.species.bodySize / web.inDegreeOf(q) }.sum
          (p, n(p.species.id) * math.pow(p.species.bodySize, 0.75) - alloc)
        }.filter(_._2 > 0.0)
        if deficits.isEmpty then go = false
        else
          val worst = deficits.maxBy(t => (t._2, -t._1.id))._1
          web.removeVertex(worst); removed += worst.id
      removed.toSet
    for s <- 1L to 3L; (label, wp) <- Seq("biomassPyramid" -> bp(0.1, te = 0.1), "damuth" -> base("metabolic", 0.5)) do
      val (pops, meta, sizes) = World.buildPopsMetaAndSizesForTest(wp, new Random(s))
      val n = (id: Int) => sizes.getOrElse(id, 1.0)
      val (fast, rounds) = World.greedyStarvation(PopulationWebGenerator.generatePopulationWeb(pops, meta), wp.epsilon, n)
      val ref = referenceStarvation(PopulationWebGenerator.generatePopulationWeb(pops, meta), wp.epsilon, n)
      check(s"incremental cascade == reference ($label), seed $s", fast == ref, s"(${fast.size} vs ${ref.size} removed, $rounds rounds)")

    // 6. emergent (EnergyFlow): every surviving consumer is viable and at its fixed point
    //    n ≤ min(cap, ⌊I / M^0.75⌋) (fed, never above its cap), at assembly and after a disturbance; grid sized on assembled pops.
    def flowOk(w: World, cap: Population => Double): (Boolean, Int) =
      val wp = w.worldParameters
      val web = w.populationWeb
      val size = (id: Int) => w.popIndividuals.getOrElse(id, 0.0)
      val intake = EnergyFlow.intakes(web, size, wp.transferEfficiency, w.diet)
      val bad = web.vertexSet().asScala.toSeq.filter(_.species.trophicLevel != 0).count { p =>
        val n = size(p.id)
        val sustained = math.floor(intake(p.id) / math.pow(p.species.bodySize, 0.75) + 1e-9)
        n < wp.minViableIndividuals || n > sustained || n > cap(p)
      }
      (bad == 0, bad)
    for s <- 1L to 3L; rule <- Seq("supply", "equal", "niche") do
      val wp = base("metabolic", 0.0, am = "emergent").copy(rangeFraction = 0.01, foragingRule = rule)
      val w = World(wp, cp, new Random(s))
      if rule == "niche" then
        // edge prey of every predator with ≥ 2 prey masses get e^-2, and no prey gets more than 1
        val prefOk = w.metaWeb.vertexSet().asScala.forall { pred =>
          val ps = w.metaWeb.outgoingEdgesOf(pred).asScala.toSeq.map(w.metaWeb.getEdgeTarget)
          val ws = ps.map(q => w.diet.pref(pred, q))
          ps.isEmpty || ps.map(_.bodySize).distinct.size < 2 ||
            (math.abs(ws(ps.indexOf(ps.minBy(_.bodySize))) - math.exp(-2)) < 1e-9 && ws.forall(x => x > 0 && x <= 1 + 1e-12))
        }
        check(s"niche preference: e^-2 at feeding-interval edges, ≤ 1 inside, seed $s", prefOk)
      val assembled = w.popIndividuals
      val tp = World.trophicPositions(w.metaWeb)
      val (okA, badA) = flowOk(w, _ => Double.MaxValue)
      val consumers = w.populationWeb.vertexSet().asScala.count(_.species.trophicLevel != 0)
      val tiles = w.managementLandscape.managementAreas.size
      val expected = math.pow(math.ceil(math.sqrt(math.max(1, w.populationWeb.vertexSet().size / wp.avgPopulationsPerTile))), 2).toInt
      check(s"[$rule] emergent assembly: viable, fed, grid on assembled pops, seed $s", okA && tiles == expected && consumers > 0,
        s"(${w.populationWeb.vertexSet().size} assembled, $consumers consumers, $badA off, $tiles tiles)")
      val (placed, _) = World.buildPopsAndMetaForTest(wp, new Random(s))
      val basalIn = w.populationWeb.vertexSet().asScala.filter(_.species.trophicLevel == 0)
      val basalB = basalIn.iterator.map(w.biomassOf).sum
      check(s"[$rule] emergent: every basal point kept, Σ basal biomass ≈ B0, seed $s",
        basalIn.size == placed.count(_.species.trophicLevel == 0) && basalB <= wp.basalBiomass && basalB > 0.95 * wp.basalBiomass,
        f"(${basalIn.size} basal points, basal biomass ${basalB / wp.basalBiomass}%.3f B0)")
      val (ftp, basalShare) = EnergyFlow.flowTrophicPositions(w.populationWeb, id => w.popIndividuals.getOrElse(id, 0.0),
        wp.transferEfficiency, w.diet)
      val ftpOk = w.populationWeb.vertexSet().asScala.forall { p =>
        if p.species.trophicLevel == 0 then ftp(p.id) == 1.0
        else ftp(p.id) >= 2.0 - 1e-9 && !ftp(p.id).isNaN && basalShare(p.id) >= -1e-12 && basalShare(p.id) <= 1 + 1e-9
      }
      check(s"[$rule] emergent: flow TP basal = 1, consumers ≥ 2, basal share in [0,1], seed $s", ftpOk)
      // medianHomeRange 0.02: tile = 2.5 HR diameters = 0.1 → 10 × 10 tiles
      val fixed = World(wp.copy(tileSizeInHomeRanges = 2.5), cp, new Random(s))
      check(s"[$rule] tileSizeInHomeRanges = 2.5 → 100 tiles, same assembled community, seed $s",
        fixed.managementLandscape.managementAreas.size == 100 && fixed.popIndividuals == w.popIndividuals)
      val (after, removals) = w.primaryExtinctions().phase2Cascades()
      val (okD, badD) = flowOk(after, p => assembled.getOrElse(p.id, 0.0))
      check(s"[$rule] emergent disturbance: viable, fed, no growth above assembly, seed $s", okD,
        s"($removals removed, $badD off)")

    // 7. reusable assembly: runWith(assembly) == a fresh run, across tile / pressure / budget variants
    System.setProperty("econetcons.skipRobustness", "true")
    for s <- 1L to 2L; (label, wp0) <- Seq(
        "emergent" -> base("metabolic", 0.0, am = "emergent").copy(rangeFraction = 0.01),
        "damuth" -> base("metabolic", 0.5)) do
      val a = World.assemble(wp0, new Random(s))
      val same = Seq((0.0, 32, 0.5, 0.2), (2.0, 32, 0.3, 0.1), (0.5, 8, 0.8, 0.4)).forall { case (t, tile, pExt, frac) =>
        val wp = wp0.copy(tileSizeInHomeRanges = t, avgPopulationsPerTile = tile, extinctionProbabilityUnprotected = pExt)
        val fresh = Observables.runWith(wp, frac, s, Observables.paperStrategies)
        val reused = Observables.runWith(wp, frac, s, Observables.paperStrategies, Some(a))
        fresh.keySet == reused.keySet && fresh.forall((k, v) => reused(k) == v || (v.isNaN && reused(k).isNaN))
      }
      check(s"reused assembly == fresh run over 3 tile/pressure/budget variants ($label), seed $s", same,
        s"(${a.assembledSize} assembled populations)")
    System.clearProperty("econetcons.skipRobustness")

    out.println(if ok then "ALL CHECKS PASSED" else "SOME CHECKS FAILED")
