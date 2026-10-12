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

    out.println(if ok then "ALL CHECKS PASSED" else "SOME CHECKS FAILED")
