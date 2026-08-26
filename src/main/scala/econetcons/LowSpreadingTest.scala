package econetcons

import scala.util.Random

/**
 * Test low spreading factor (0.1) with different hotspot configurations
 */
object LowSpreadingTest extends App:

  val seed = 42L

  println("\n" + "=" * 80)
  println("LOW SPREADING FACTOR (0.1) WITH DIFFERENT HOTSPOT CONFIGURATIONS")
  println("=" * 80)

  // Base parameters with low spreading
  val baseParams = WorldParameters(
    numberOfSpecies = 200,
    connectanceMetaWeb = 0.4,
    areaOverlap = 20.0,
    medianHomeRange = 0.05,
    avgPopulationsPerTile = 5,
    useThinning = false,
    spreadingFactor = 0.1  // LOW - tight clustering around species centers
  )

  println("\n### SPATIAL DIAGNOSTICS ###\n")

  // 1. No hotspot (baseline)
  val noHotspot = SpatialDiagnostics.analyze(
    "No Hotspot (uniform)",
    baseParams.copy(useHotspot = false),
    seed
  )
  noHotspot.printReport()

  // 2. Single hotspot with HIGH variance (diffuse)
  val highVarianceHotspot = SpatialDiagnostics.analyze(
    "Single Hotspot (high var 0.3)",
    baseParams.copy(useHotspot = true, numHotspots = 1, hotspotVariance = 0.3),
    seed
  )
  highVarianceHotspot.printReport()

  // 3. Multiple hotspots (3) with moderate variance
  val multipleHotspots3 = SpatialDiagnostics.analyze(
    "3 Hotspots (var 0.1)",
    baseParams.copy(useHotspot = true, numHotspots = 3, hotspotVariance = 0.1),
    seed
  )
  multipleHotspots3.printReport()

  // 4. Multiple hotspots (5) with moderate variance
  val multipleHotspots5 = SpatialDiagnostics.analyze(
    "5 Hotspots (var 0.1)",
    baseParams.copy(useHotspot = true, numHotspots = 5, hotspotVariance = 0.1),
    seed
  )
  multipleHotspots5.printReport()

  // 5. Single tight hotspot (for comparison)
  val tightHotspot = SpatialDiagnostics.analyze(
    "Single Hotspot (tight var 0.05)",
    baseParams.copy(useHotspot = true, numHotspots = 1, hotspotVariance = 0.05),
    seed
  )
  tightHotspot.printReport()

  println("\n### COMPARATIVE SUMMARY ###")
  println("=" * 80)

  val allReports = Seq(
    noHotspot,
    tightHotspot,
    highVarianceHotspot,
    multipleHotspots3,
    multipleHotspots5
  )

  println(f"\n${"Scenario"}%-35s ${"Pops"}%-8s ${"In Net%"}%-10s ${"Tiles%"}%-10s ${"Gini"}%-8s ${"CoOccur"}%-10s")
  println("-" * 90)

  allReports.foreach { r =>
    val netPct = r.populationsInNetwork.toDouble / r.totalPopulations * 100
    println(f"${r.scenario}%-35s ${r.totalPopulations}%-8d ${netPct}%-10.1f ${r.occupancyRate}%-10.1f ${r.tileGiniCoefficient}%-8.3f ${r.speciesCoOccurrence}%-10.2f")
  }

  println("\n" + "=" * 80)
  println("KEY INSIGHTS:")
  println("  • Low spreading (0.1) = tight population clustering around species centers")
  println("  • Single tight hotspot = all species in one place (high Gini)")
  println("  • Single high-variance hotspot = species spread but still prefer center")
  println("  • Multiple hotspots = multiple biodiversity centers across landscape")
  println("  • Higher CoOccur = better for species richness-based conservation")
  println("=" * 80)

end LowSpreadingTest
