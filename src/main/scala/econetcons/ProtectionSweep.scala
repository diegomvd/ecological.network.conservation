package econetcons

import scala.util.Random

/**
 * Protection fraction sweep: 10% to 80% in 10% steps
 * World: High-variance hotspot
 * Conservation: Random protection
 */
object ProtectionSweep extends App:

  val seed = 42L

  println("\n" + "=" * 80)
  println("PROTECTION FRACTION SWEEP")
  println("World: High-variance hotspot (variance 0.3)")
  println("Conservation: Random protection")
  println("Testing: 10% to 80% protected in 10% steps")
  println("=" * 80)

  case class Result(
    protectionFraction: Double,
    initialSpecies: Int,
    finalSpecies: Int,
    speciesExtinctionRate: Double,
    initialInteractions: Int,
    finalInteractions: Int,
    interactionExtinctionRate: Double,
    cascades: Int
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

    val (finalWorld, nCascades) = initialWorld.extinctions()

    val finalSR = finalWorld.getSpeciesRichness()
    val finalIR = finalWorld.getInteractionsRichness()

    val speciesExtinctionRate = (initialSR - finalSR).toDouble / initialSR * 100.0
    val interactionExtinctionRate = (initialIR - finalIR).toDouble / initialIR * 100.0

    Result(
      protectionFraction = protectionFraction,
      initialSpecies = initialSR,
      finalSpecies = finalSR,
      speciesExtinctionRate = speciesExtinctionRate,
      initialInteractions = initialIR,
      finalInteractions = finalIR,
      interactionExtinctionRate = interactionExtinctionRate,
      cascades = nCascades
    )
  }

  println("\nRunning simulations...")

  val results = (1 to 8).map { i =>
    val fraction = i * 0.1
    print(f"  ${fraction * 100}%.0f%% protected... ")
    val result = runSimulation(fraction, seed)
    println(f"Done (${result.finalSpecies} species survived)")
    result
  }

  println("\n" + "=" * 80)
  println("RESULTS")
  println("=" * 80)

  println(f"\n${"Protection%"}%-15s ${"Initial Sp"}%-12s ${"Final Sp"}%-12s ${"Sp Extinct%"}%-15s ${"Int Extinct%"}%-15s ${"Cascades"}%-10s")
  println("-" * 85)

  results.foreach { r =>
    println(f"${r.protectionFraction * 100}%-15.0f ${r.initialSpecies}%-12d ${r.finalSpecies}%-12d ${r.speciesExtinctionRate}%-15.1f ${r.interactionExtinctionRate}%-15.1f ${r.cascades}%-10d")
  }

  println("\n" + "=" * 80)
  println("ANALYSIS")
  println("=" * 80)

  println(f"\nSpecies extinction rates:")
  results.foreach { r =>
    println(f"  ${r.protectionFraction * 100}%3.0f%% protected → ${r.speciesExtinctionRate}%5.1f%% extinct")
  }

  println(f"\nInteraction extinction rates:")
  results.foreach { r =>
    println(f"  ${r.protectionFraction * 100}%3.0f%% protected → ${r.interactionExtinctionRate}%5.1f%% extinct")
  }

  println("\n" + "=" * 80)

end ProtectionSweep
