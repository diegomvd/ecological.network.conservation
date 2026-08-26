package econetcons

import scala.util.Random

/**
 * Test different parameter configurations and characterize their spatial distributions
 */
object SpatialDiagnosticsTest extends App:

  val seed = 42L

  println("\n" + "=" * 80)
  println("SPATIAL DISTRIBUTION CHARACTERIZATION")
  println("Comparing different spreading factors and hotspot configurations")
  println("=" * 80)

  // Base parameters
  val baseParams = WorldParameters(
    numberOfSpecies = 200,
    connectanceMetaWeb = 0.4,
    areaOverlap = 20.0,
    medianHomeRange = 0.05,
    avgPopulationsPerTile = 5,
    useThinning = false
  )

  println("\n### VARYING SPREADING FACTOR (No Hotspot) ###")

  val spreading05 = SpatialDiagnostics.analyze(
    "Spreading Factor 0.5",
    baseParams.copy(spreadingFactor = 0.5),
    seed
  )
  spreading05.printReport()

  val spreading10 = SpatialDiagnostics.analyze(
    "Spreading Factor 1.0",
    baseParams.copy(spreadingFactor = 1.0),
    seed
  )
  spreading10.printReport()

  val spreading20 = SpatialDiagnostics.analyze(
    "Spreading Factor 2.0",
    baseParams.copy(spreadingFactor = 2.0),
    seed
  )
  spreading20.printReport()

  println("\n### VARYING HOTSPOT INTENSITY (Spreading = 1.0) ###")

  val noHotspot = SpatialDiagnostics.analyze(
    "No Hotspot (uniform)",
    baseParams.copy(spreadingFactor = 1.0, useHotspot = false),
    seed
  )
  noHotspot.printReport()

  val tightHotspot = SpatialDiagnostics.analyze(
    "Tight Hotspot (var 0.01)",
    baseParams.copy(spreadingFactor = 1.0, useHotspot = true, hotspotVariance = 0.01),
    seed
  )
  tightHotspot.printReport()

  val mediumHotspot = SpatialDiagnostics.analyze(
    "Medium Hotspot (var 0.05)",
    baseParams.copy(spreadingFactor = 1.0, useHotspot = true, hotspotVariance = 0.05),
    seed
  )
  mediumHotspot.printReport()

  val sparseHotspot = SpatialDiagnostics.analyze(
    "Sparse Hotspot (var 0.15)",
    baseParams.copy(spreadingFactor = 1.0, useHotspot = true, hotspotVariance = 0.15),
    seed
  )
  sparseHotspot.printReport()

  println("\n### COMPARATIVE SUMMARY ###")
  println("=" * 80)

  val allReports = Seq(
    spreading05, spreading10, spreading20,
    noHotspot, tightHotspot, mediumHotspot, sparseHotspot
  )

  println(f"\n${"Scenario"}%-30s ${"Pops"}%-8s ${"In Net"}%-8s ${"Tiles%"}%-10s ${"Gini"}%-8s ${"CoOccur"}%-10s")
  println("-" * 80)

  allReports.foreach { r =>
    val netPct = r.populationsInNetwork.toDouble / r.totalPopulations * 100
    println(f"${r.scenario}%-30s ${r.totalPopulations}%-8d ${netPct}%-8.1f ${r.occupancyRate}%-10.1f ${r.tileGiniCoefficient}%-8.3f ${r.speciesCoOccurrence}%-10.2f")
  }

  println("\n" + "=" * 80)
  println("INTERPRETATION GUIDE:")
  println("  • Pops: Total populations created")
  println("  • In Net: % of populations that make it into interaction network")
  println("  • Tiles%: % of tiles that are occupied")
  println("  • Gini: Clustering measure (0=equal, 1=highly clustered)")
  println("  • CoOccur: Avg species pairs sharing tiles (higher = more co-occurrence)")
  println("=" * 80)

end SpatialDiagnosticsTest
