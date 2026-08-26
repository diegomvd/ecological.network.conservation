package econetcons

import scala.util.Random

/**
 * Diagnostic tool for analyzing and validating WorldParameters
 * Helps calibrate initial conditions without empirical data
 */
object ParameterDiagnostics:

  case class DiagnosticReport(
    // Input parameters
    params: WorldParameters,

    // Derived quantities
    numberOfPopulations: Int,
    nTiles: Int,
    gridSideLength: Int,
    avgPopsPerSpecies: Double,
    avgPopsPerTile: Double,  // actual average populations per tile
    tileSize: Double,  // in same units as landscape (0-1)
    tileSizeInHomeRanges: Double,  // tile size / median home range diameter

    // Ecological reasonableness checks
    checks: Map[String, (Boolean, String)]
  ):

    def printReport(): Unit =
      println("=" * 80)
      println("PARAMETER DIAGNOSTICS REPORT")
      println("=" * 80)

      println("\n--- INPUT PARAMETERS ---")
      println(f"  Number of species: ${params.numberOfSpecies}")
      println(f"  Target metaweb connectance: ${params.connectanceMetaWeb}%.3f")
      println(f"  Number of populations: ${params.nPopulations}")
      println(f"  Median home range (radius): ${params.medianHomeRange}%.4f")
      println(f"  Avg populations per tile: ${params.avgPopulationsPerTile}")

      println("\n--- DERIVED QUANTITIES ---")
      println(f"  Total populations: $numberOfPopulations")
      println(f"  Populations per species (avg): ${avgPopsPerSpecies}%.1f")
      println(f"  Populations per tile (avg): ${avgPopsPerTile}%.1f")
      println(f"  Number of tiles: $nTiles")
      println(f"  Grid dimensions: ${gridSideLength} × ${gridSideLength}")
      println(f"  Tile size (landscape units): ${tileSize}%.4f")
      println(f"  Tile size (home range diameters): ${tileSizeInHomeRanges}%.2f")

      println("\n--- ECOLOGICAL REASONABLENESS CHECKS ---")
      checks.foreach { case (checkName, (passed, message)) =>
        val status = if passed then "✓ PASS" else "✗ FAIL"
        println(f"  $status%-8s $checkName: $message")
      }

      println("\n--- RECOMMENDATIONS ---")
      val failedChecks = checks.filter(!_._2._1)
      if failedChecks.isEmpty then
        println("  All checks passed! Parameters appear ecologically reasonable.")
      else
        println("  Some checks failed. Consider adjusting:")
        failedChecks.foreach { case (checkName, (_, message)) =>
          println(f"    • $checkName")
        }

      println("=" * 80)

  /**
   * Run diagnostics on a set of WorldParameters
   */
  def diagnose(params: WorldParameters): DiagnosticReport =

    // Derived quantities (same as in World.apply): population count is now a direct
    // parameter; management-grid resolution derives from it.
    val numberOfPopulations = params.nPopulations

    val nTiles = (numberOfPopulations.toDouble / params.avgPopulationsPerTile.toDouble).toInt

    // Grid side length (assuming square grid)
    val gridSideLength = math.ceil(math.sqrt(nTiles)).toInt

    // Tile size in landscape units [0,1]
    val tileSize = 1.0 / gridSideLength

    // Tile size in units of home range diameters
    val homeRangeDiameter = 2.0 * params.medianHomeRange
    val tileSizeInHomeRanges = tileSize / homeRangeDiameter

    // Average populations per species
    val avgPopsPerSpecies = numberOfPopulations.toDouble / params.numberOfSpecies

    // Average populations per tile (actual, not the parameter)
    val avgPopsPerTile = numberOfPopulations.toDouble / nTiles

    // Run ecological reasonableness checks
    val checks = runChecks(params, numberOfPopulations, avgPopsPerSpecies,
                          avgPopsPerTile, tileSizeInHomeRanges, nTiles)

    DiagnosticReport(
      params = params,
      numberOfPopulations = numberOfPopulations,
      nTiles = nTiles,
      gridSideLength = gridSideLength,
      avgPopsPerSpecies = avgPopsPerSpecies,
      avgPopsPerTile = avgPopsPerTile,
      tileSize = tileSize,
      tileSizeInHomeRanges = tileSizeInHomeRanges,
      checks = checks
    )

  private def runChecks(
    params: WorldParameters,
    numberOfPopulations: Int,
    avgPopsPerSpecies: Double,
    avgPopsPerTile: Double,
    tileSizeInHomeRanges: Double,
    nTiles: Int
  ): Map[String, (Boolean, String)] =

    Map(
      "Populations per species" -> {
        val reasonable = avgPopsPerSpecies >= 10.0 && avgPopsPerSpecies <= 100.0
        val msg = if reasonable then
          f"${avgPopsPerSpecies}%.1f is in reasonable range [10, 100]"
        else if avgPopsPerSpecies < 10.0 then
          f"${avgPopsPerSpecies}%.1f is low - species may go extinct easily"
        else
          f"${avgPopsPerSpecies}%.1f is high - consider reducing for performance"
        (reasonable, msg)
      },

      "Total populations" -> {
        val reasonable = numberOfPopulations >= 500 && numberOfPopulations <= 10000
        val msg = if reasonable then
          f"$numberOfPopulations is in reasonable range [500, 10000]"
        else if numberOfPopulations < 500 then
          f"$numberOfPopulations is low - may lack spatial complexity"
        else
          f"$numberOfPopulations is high - may be computationally expensive"
        (reasonable, msg)
      },

      "Populations per tile (avg)" -> {
        val avgPopsPerTile = numberOfPopulations.toDouble / nTiles
        val reasonable = avgPopsPerTile >= 5.0 && avgPopsPerTile <= 100.0
        val msg = if reasonable then
          f"${avgPopsPerTile}%.1f populations per tile - good balance"
        else if avgPopsPerTile < 5.0 then
          f"${avgPopsPerTile}%.1f - tiles too sparse, many will be nearly empty"
        else
          f"${avgPopsPerTile}%.1f - tiles very dense, consider more tiles for finer decisions"
        (reasonable, msg)
      },

      "Grid size" -> {
        val reasonable = nTiles >= 16 && nTiles <= 400
        val msg = if reasonable then
          f"$nTiles tiles is in reasonable range [16, 400]"
        else if nTiles < 16 then
          f"$nTiles tiles is too few - consider increasing"
        else
          f"$nTiles tiles is many - may be computationally expensive"
        (reasonable, msg)
      },

      "Home range scale" -> {
        val reasonable = params.medianHomeRange >= 0.005 && params.medianHomeRange <= 0.1
        val msg = if reasonable then
          f"${params.medianHomeRange}%.4f is in reasonable range [0.005, 0.1]"
        else if params.medianHomeRange < 0.005 then
          f"${params.medianHomeRange}%.4f is very small - populations very localized"
        else
          f"${params.medianHomeRange}%.4f is large - populations very widespread"
        (reasonable, msg)
      }
    )

  /**
   * Suggest improved parameters based on target constraints
   */
  def suggestParameters(
    targetSpecies: Int = 100,
    targetPopsPerSpecies: Int = 20,
    targetTileSize: Double = 5.0,  // in home range diameters
    medianHomeRange: Double = 0.01
  ): WorldParameters =

    // Work backwards from targets: population count is now a direct parameter.
    val numberOfPopulations = targetSpecies * targetPopsPerSpecies

    // Calculate grid size such that tile size = targetTileSize × home range diameter
    val homeRangeDiameter = 2.0 * medianHomeRange
    val tileSize = targetTileSize * homeRangeDiameter
    val gridSideLength = math.ceil(1.0 / tileSize).toInt
    val nTiles = gridSideLength * gridSideLength

    // Calculate avgPopulationsPerTile
    val avgPopulationsPerTile = math.max(1, numberOfPopulations / nTiles)

    WorldParameters(
      numberOfSpecies = targetSpecies,
      connectanceMetaWeb = 0.2,  // Keep moderate
      nPopulations = numberOfPopulations,
      medianHomeRange = medianHomeRange,
      avgPopulationsPerTile = avgPopulationsPerTile
    )

  /**
   * Test a range of parameter values and show which pass checks
   */
  def parameterSweep(): Unit =
    println("\n" + "=" * 80)
    println("PARAMETER SWEEP - Testing different configurations")
    println("=" * 80)

    val nPopulationsValues = Seq(500, 1000, 2500, 5000, 10000)
    val speciesValues = Seq(50, 100, 200)
    val homeRangeValues = Seq(0.005, 0.01, 0.02, 0.05)

    println(f"\n${"nPops"}%-12s ${"Species"}%-10s ${"HomeRange"}%-12s ${"Pops"}%-8s ${"PopsPerSp"}%-12s ${"Tiles"}%-8s ${"TileSize(HR)"}%-15s ${"Checks"}%-8s")
    println("-" * 110)

    for
      nPops <- nPopulationsValues
      species <- speciesValues
      homeRange <- homeRangeValues
    do
      val params = WorldParameters(
        numberOfSpecies = species,
        nPopulations = nPops,
        medianHomeRange = homeRange,
        avgPopulationsPerTile = 20
      )

      val report = diagnose(params)
      val passedChecks = report.checks.count(_._2._1)
      val totalChecks = report.checks.size
      val checkStatus = f"$passedChecks/$totalChecks"

      println(f"${nPops}%-12d ${species}%-10d ${homeRange}%-12.4f ${report.numberOfPopulations}%-8d ${report.avgPopsPerSpecies}%-12.1f ${report.nTiles}%-8d ${report.tileSizeInHomeRanges}%-15.1f $checkStatus%-8s")

end ParameterDiagnostics
