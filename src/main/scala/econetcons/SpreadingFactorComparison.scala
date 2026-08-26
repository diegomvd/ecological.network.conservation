package econetcons

import scala.util.Random

/**
 * Compare different spreading factors to understand species spatial extents
 * Focus on finding balance between spread and co-occurrence
 */
object SpreadingFactorComparison extends App:

  val seed = 42L

  println("\n" + "=" * 90)
  println("SPREADING FACTOR COMPARISON - Finding Optimal Balance")
  println("Goal: Species should spread enough for co-occurrence but not cover entire landscape")
  println("=" * 90)

  // Base parameters
  val baseParams = WorldParameters(
    numberOfSpecies = 200,
    connectanceMetaWeb = 0.4,
    areaOverlap = 20.0,
    medianHomeRange = 0.05,
    avgPopulationsPerTile = 5,
    useThinning = false,
    useHotspot = false  // No hotspot to isolate spreading factor effect
  )

  println("\n### TESTING DIFFERENT SPREADING FACTORS ###\n")

  val spreadingFactors = Seq(0.05, 0.1, 0.2, 0.5, 1.0, 2.0)

  val results = spreadingFactors.map { spreading =>
    SpatialDiagnostics.analyze(
      f"Spreading $spreading%.2f",
      baseParams.copy(spreadingFactor = spreading),
      seed
    )
  }

  // Print detailed reports
  results.foreach(_.printReport())

  // Comparative summary
  println("\n" + "=" * 90)
  println("### COMPARATIVE SUMMARY ###")
  println("=" * 90)

  println(f"\n${"Spreading"}%-12s ${"Pops"}%-8s ${"InNet%"}%-8s ${"Tiles%"}%-8s ${"CoOccur"}%-10s ${"AvgDiam"}%-10s ${"Covering"}%-10s")
  println("-" * 90)

  results.foreach { r =>
    val spreading = r.scenario.split(" ")(1).toDouble
    val netPct = r.populationsInNetwork.toDouble / r.totalPopulations * 100
    println(f"$spreading%-12.2f ${r.totalPopulations}%-8d ${netPct}%-8.1f ${r.occupancyRate}%-8.1f ${r.speciesCoOccurrence}%-10.3f ${r.avgSpeciesDiameter}%-10.3f ${r.speciesCoveringLandscape}%-10d")
  }

  println("\n" + "=" * 90)
  println("INTERPRETATION:")
  println("  • Spreading: How far populations scatter from species center")
  println("  • AvgDiam: Average spatial extent diameter per species (0-1.41 landscape units)")
  println("  • Covering: Number of species with diameter > 0.5 (covering >35% of landscape)")
  println("  • CoOccur: Species pairs sharing tiles (higher = better for conservation)")
  println("\nIDEAL BALANCE:")
  println("  • AvgDiam: 0.3-0.6 (species occupy portion of landscape, not all or nothing)")
  println("  • CoOccur: >0.2 (good species overlap for interactions)")
  println("  • Covering: <50 species (not everyone everywhere)")
  println("  • Tiles%: 30-50% (reasonable coverage, not too sparse)")
  println("=" * 90)

  // Find optimal
  val optimal = results.minBy { r =>
    // Scoring function: penalize extremes
    val diamScore = math.abs(r.avgSpeciesDiameter - 0.45) // Target ~0.45
    val coOccurScore = if (r.speciesCoOccurrence < 0.2) 1.0 - r.speciesCoOccurrence else 0.0
    val coveringPenalty = if (r.speciesCoveringLandscape > 50) r.speciesCoveringLandscape / 200.0 else 0.0
    diamScore + coOccurScore + coveringPenalty
  }

  println(f"\n*** RECOMMENDED: ${optimal.scenario} ***")
  println(f"  • Best balance of spread and co-occurrence")
  println(f"  • Avg species diameter: ${optimal.avgSpeciesDiameter}%.3f")
  println(f"  • Species co-occurrence: ${optimal.speciesCoOccurrence}%.3f")
  println(f"  • Species covering >50%%: ${optimal.speciesCoveringLandscape}")

end SpreadingFactorComparison
