package econetcons

import scala.util.Random

/**
 * Fine-grained protection fraction sweep: 30% to 60% in 5% steps
 * World: High-variance hotspot
 * Conservation: Random protection
 * Goal: Identify potential percolation transition
 */
object FineProtectionSweep extends App:

  val seed = 42L

  println("\n" + "=" * 80)
  println("FINE-GRAINED PROTECTION FRACTION SWEEP")
  println("World: High-variance hotspot (variance 0.3)")
  println("Conservation: Random protection")
  println("Testing: 30% to 60% protected in 5% steps")
  println("Investigating potential percolation transition")
  println("=" * 80)

  case class Result(
    protectionFraction: Double,
    initialSpecies: Int,
    finalSpecies: Int,
    speciesExtinctionRate: Double,
    speciesSurvivalRate: Double,
    initialInteractions: Int,
    finalInteractions: Int,
    interactionExtinctionRate: Double,
    interactionSurvivalRate: Double,
    cascades: Int,
    initialPopulations: Int,
    finalPopulations: Int,
    populationExtinctionRate: Double
  )

  def runSimulation(protectionFraction: Double, seed: Long): Result = {

    val worldParameters = WorldParameters(
      numberOfSpecies = 200,
      connectanceMetaWeb = 0.4,
      areaOverlap = 20.0,
      medianHomeRange = 0.05,
      avgPopulationsPerTile = 5,
      useThinning = false,
      spreadingFactor = 0.5,
      useHotspot = true,
      hotspotVariance = 0.3  // High variance
    )

    val conservationParameters = ConservationParameters(
      fractionProtected = protectionFraction,
      strategy = Some(ConservationStrategy.Random)
    )

    val initialWorld = World(
      worldParameters = worldParameters,
      conservationParameters = conservationParameters,
      rnd = Random(seed)
    )

    val initialSR = initialWorld.getSpeciesRichness()
    val initialIR = initialWorld.getInteractionsRichness()
    val initialPops = initialWorld.populations.count(!_.extinct)

    val (finalWorld, nCascades) = initialWorld.extinctions()

    val finalSR = finalWorld.getSpeciesRichness()
    val finalIR = finalWorld.getInteractionsRichness()
    val finalPops = finalWorld.populations.count(!_.extinct)

    val speciesExtinctionRate = (initialSR - finalSR).toDouble / initialSR * 100.0
    val speciesSurvivalRate = finalSR.toDouble / initialSR * 100.0
    val interactionExtinctionRate = (initialIR - finalIR).toDouble / initialIR * 100.0
    val interactionSurvivalRate = finalIR.toDouble / initialIR * 100.0
    val populationExtinctionRate = (initialPops - finalPops).toDouble / initialPops * 100.0

    Result(
      protectionFraction = protectionFraction,
      initialSpecies = initialSR,
      finalSpecies = finalSR,
      speciesExtinctionRate = speciesExtinctionRate,
      speciesSurvivalRate = speciesSurvivalRate,
      initialInteractions = initialIR,
      finalInteractions = finalIR,
      interactionExtinctionRate = interactionExtinctionRate,
      interactionSurvivalRate = interactionSurvivalRate,
      cascades = nCascades,
      initialPopulations = initialPops,
      finalPopulations = finalPops,
      populationExtinctionRate = populationExtinctionRate
    )
  }

  println("\nRunning simulations...")

  val results = (30 to 60 by 5).map { percentage =>
    val fraction = percentage / 100.0
    print(f"  ${fraction * 100}%.0f%% protected... ")
    val result = runSimulation(fraction, seed)
    println(f"Done (${result.finalSpecies}/${result.initialSpecies} species, ${result.finalInteractions}/${result.initialInteractions} interactions)")
    result
  }

  println("\n" + "=" * 80)
  println("DETAILED RESULTS")
  println("=" * 80)

  println(f"\n${"Protection%"}%-15s ${"Species"}%-20s ${"Sp Surv%"}%-12s ${"Interactions"}%-25s ${"Int Surv%"}%-12s ${"Cascades"}%-10s")
  println("-" * 100)

  results.foreach { r =>
    println(f"${r.protectionFraction * 100}%-15.0f ${r.finalSpecies}/${r.initialSpecies}%-20s ${r.speciesSurvivalRate}%-12.1f ${r.finalInteractions}/${r.initialInteractions}%-25s ${r.interactionSurvivalRate}%-12.1f ${r.cascades}%-10d")
  }

  println("\n" + "=" * 80)
  println("PERCOLATION ANALYSIS")
  println("=" * 80)

  println(f"\n${"Protection%"}%-15s ${"Sp Extinct%"}%-15s ${"Int Extinct%"}%-15s ${"Pop Extinct%"}%-15s")
  println("-" * 65)

  results.foreach { r =>
    println(f"${r.protectionFraction * 100}%-15.0f ${r.speciesExtinctionRate}%-15.1f ${r.interactionExtinctionRate}%-15.1f ${r.populationExtinctionRate}%-15.1f")
  }

  // Calculate gradients (rate of change)
  println("\n" + "=" * 80)
  println("GRADIENT ANALYSIS (Rate of Improvement per 5% Protection Increase)")
  println("=" * 80)

  println(f"\n${"Protection Range"}%-20s ${"ΔSp Extinct"}%-20s ${"ΔInt Extinct"}%-20s")
  println("-" * 65)

  results.sliding(2).foreach { case Seq(prev, curr) =>
    val deltaSpecies = prev.speciesExtinctionRate - curr.speciesExtinctionRate
    val deltaInteractions = prev.interactionExtinctionRate - curr.interactionExtinctionRate
    val range = f"${prev.protectionFraction * 100}%.0f%% → ${curr.protectionFraction * 100}%.0f%%"
    println(f"${range}%-20s ${deltaSpecies}%-20.2f ${deltaInteractions}%-20.2f")
  }

  // Identify steepest gradient (potential transition point)
  val gradients = results.sliding(2).map { case Seq(prev, curr) =>
    val deltaSpecies = prev.speciesExtinctionRate - curr.speciesExtinctionRate
    val deltaInteractions = prev.interactionExtinctionRate - curr.interactionExtinctionRate
    ((prev.protectionFraction + curr.protectionFraction) / 2, deltaSpecies, deltaInteractions)
  }.toSeq

  val maxSpeciesGradient = gradients.maxBy(_._2)
  val maxInteractionGradient = gradients.maxBy(_._3)

  println("\n" + "=" * 80)
  println("POTENTIAL PERCOLATION TRANSITION POINTS")
  println("=" * 80)

  println(f"\nSpecies network: Steepest improvement at ~${maxSpeciesGradient._1 * 100}%.0f%% protection")
  println(f"  Gradient: ${maxSpeciesGradient._2}%.2f%% reduction per 5%% protection increase")

  println(f"\nInteraction network: Steepest improvement at ~${maxInteractionGradient._1 * 100}%.0f%% protection")
  println(f"  Gradient: ${maxInteractionGradient._3}%.2f%% reduction per 5%% protection increase")

  println("\n" + "=" * 80)

end FineProtectionSweep
