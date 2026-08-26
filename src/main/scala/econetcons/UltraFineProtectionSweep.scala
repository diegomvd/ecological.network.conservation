package econetcons

import scala.util.Random
import java.io.PrintWriter
import java.io.File

/**
 * Ultra-fine protection fraction sweep: 10% to 80% in 0.2% steps
 * World: High-variance hotspot
 * Conservation: Random protection
 * Goal: Map detailed percolation transition curve
 */
object UltraFineProtectionSweep extends App:

  val seed = 42L

  println("\n" + "=" * 80)
  println("ULTRA-FINE PROTECTION FRACTION SWEEP")
  println("World: High-variance hotspot (variance 0.3)")
  println("Conservation: Random protection")
  println("Testing: 10% to 80% protected in 0.2% steps (351 simulations)")
  println("Mapping percolation transition in detail")
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

  println("\nRunning simulations (this will take a while)...")
  print("Progress: ")

  // Generate protection fractions: 0.10, 0.102, 0.104, ..., 0.798, 0.80
  val protectionFractions = (100 to 800 by 2).map(_ / 1000.0)
  val totalSims = protectionFractions.size

  val results = protectionFractions.zipWithIndex.map { case (fraction, idx) =>
    if ((idx + 1) % 50 == 0) {
      print(f"${idx + 1}/$totalSims ")
    }
    val result = runSimulation(fraction, seed)
    result
  }

  println(s"\nCompleted $totalSims simulations!")

  // Write results to CSV file for analysis
  val csvFile = new File("protection_sweep_results.csv")
  val writer = new PrintWriter(csvFile)

  writer.println("protection_fraction,initial_species,final_species,species_extinction_rate,species_survival_rate,initial_interactions,final_interactions,interaction_extinction_rate,interaction_survival_rate,cascades,initial_populations,final_populations,population_extinction_rate")

  results.foreach { r =>
    writer.println(s"${r.protectionFraction},${r.initialSpecies},${r.finalSpecies},${r.speciesExtinctionRate},${r.speciesSurvivalRate},${r.initialInteractions},${r.finalInteractions},${r.interactionExtinctionRate},${r.interactionSurvivalRate},${r.cascades},${r.initialPopulations},${r.finalPopulations},${r.populationExtinctionRate}")
  }

  writer.close()
  println(f"\nResults saved to: ${csvFile.getAbsolutePath}")

  // Summary statistics
  println("\n" + "=" * 80)
  println("SUMMARY STATISTICS")
  println("=" * 80)

  val minSpeciesExtinction = results.minBy(_.speciesExtinctionRate)
  val maxSpeciesExtinction = results.maxBy(_.speciesExtinctionRate)
  val minInteractionExtinction = results.minBy(_.interactionExtinctionRate)
  val maxInteractionExtinction = results.maxBy(_.interactionExtinctionRate)

  println(f"\nSpecies Extinction:")
  println(f"  Best: ${minSpeciesExtinction.speciesExtinctionRate}%.2f%% at ${minSpeciesExtinction.protectionFraction * 100}%.1f%% protection")
  println(f"  Worst: ${maxSpeciesExtinction.speciesExtinctionRate}%.2f%% at ${maxSpeciesExtinction.protectionFraction * 100}%.1f%% protection")

  println(f"\nInteraction Extinction:")
  println(f"  Best: ${minInteractionExtinction.interactionExtinctionRate}%.2f%% at ${minInteractionExtinction.protectionFraction * 100}%.1f%% protection")
  println(f"  Worst: ${maxInteractionExtinction.interactionExtinctionRate}%.2f%% at ${maxInteractionExtinction.protectionFraction * 100}%.1f%% protection")

  // Find steepest gradient (percolation transition)
  println("\n" + "=" * 80)
  println("PERCOLATION TRANSITION DETECTION")
  println("=" * 80)

  val speciesGradients = results.sliding(2).map { case Seq(prev, curr) =>
    val deltaFraction = curr.protectionFraction - prev.protectionFraction
    val deltaExtinction = curr.speciesExtinctionRate - prev.speciesExtinctionRate
    val gradient = deltaExtinction / deltaFraction
    ((prev.protectionFraction + curr.protectionFraction) / 2, gradient, deltaExtinction)
  }.toSeq

  val interactionGradients = results.sliding(2).map { case Seq(prev, curr) =>
    val deltaFraction = curr.protectionFraction - prev.protectionFraction
    val deltaExtinction = curr.interactionExtinctionRate - prev.interactionExtinctionRate
    val gradient = deltaExtinction / deltaFraction
    ((prev.protectionFraction + curr.protectionFraction) / 2, gradient, deltaExtinction)
  }.toSeq

  // Find largest negative gradient (steepest improvement)
  val speciesTransition = speciesGradients.minBy(_._2)
  val interactionTransition = interactionGradients.minBy(_._2)

  println(f"\nSpecies Network Critical Point:")
  println(f"  Protection level: ${speciesTransition._1 * 100}%.2f%%")
  println(f"  Gradient: ${speciesTransition._2}%.2f%% extinction change per 1%% protection")
  println(f"  Drop: ${-speciesTransition._3}%.2f%% extinction reduction in 0.2%% step")

  println(f"\nInteraction Network Critical Point:")
  println(f"  Protection level: ${interactionTransition._1 * 100}%.2f%%")
  println(f"  Gradient: ${interactionTransition._2}%.2f%% extinction change per 1%% protection")
  println(f"  Drop: ${-interactionTransition._3}%.2f%% extinction reduction in 0.2%% step")

  // Sample detailed output around critical points
  println("\n" + "=" * 80)
  println("DETAILED VIEW AROUND SPECIES TRANSITION")
  println("=" * 80)

  val speciesCritical = speciesTransition._1
  val speciesWindow = results.filter { r =>
    math.abs(r.protectionFraction - speciesCritical) < 0.05
  }

  println(f"\n${"Protection%"}%-15s ${"Sp Extinct%"}%-15s ${"Int Extinct%"}%-15s")
  println("-" * 50)
  speciesWindow.foreach { r =>
    println(f"${r.protectionFraction * 100}%-15.1f ${r.speciesExtinctionRate}%-15.2f ${r.interactionExtinctionRate}%-15.2f")
  }

  println("\n" + "=" * 80)
  println(f"Analysis complete! See ${csvFile.getName} for full results.")
  println("=" * 80)

end UltraFineProtectionSweep
