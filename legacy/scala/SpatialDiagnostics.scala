package econetcons

import scala.util.Random
import scala.jdk.CollectionConverters.*

/**
 * Diagnostic tool to characterize spatial distribution of species and populations
 * Provides statistics on clustering, tile occupancy, and spatial autocorrelation
 */
object SpatialDiagnostics:

  case class SpatialReport(
    scenario: String,

    // Population statistics
    totalPopulations: Int,
    populationsInNetwork: Int,
    avgPopsPerSpecies: Double,

    // Tile-level statistics
    totalTiles: Int,
    occupiedTiles: Int,
    occupancyRate: Double,
    avgPopsPerTile: Double,
    avgPopsPerOccupiedTile: Double,
    emptyTiles: Int,

    // Species distribution across tiles
    avgSpeciesPerTile: Double,
    avgSpeciesPerOccupiedTile: Double,
    maxSpeciesInTile: Int,

    // Spatial clustering metrics
    tileGiniCoefficient: Double,  // Inequality of population distribution
    speciesCoOccurrence: Double,  // How often species share tiles

    // Species spatial extent metrics
    avgSpeciesRangeX: Double,  // Average X-range (max-min) per species
    avgSpeciesRangeY: Double,  // Average Y-range (max-min) per species
    avgSpeciesDiameter: Double,  // Average spatial extent diameter per species
    minSpeciesDiameter: Double,
    maxSpeciesDiameter: Double,
    speciesCoveringLandscape: Int,  // Number of species with range > 0.5

    // Species bounding-box area (rangeX * rangeY, periodic) as fraction of landscape.
    // A more direct read on how much landscape each species occupies (hence overlap)
    // than the bounding-box diagonal (diameter).
    avgSpeciesBBoxArea: Double,
    minSpeciesBBoxArea: Double,
    maxSpeciesBBoxArea: Double,

    // Network connectivity
    networkDensity: Double,
    avgInteractionsPerPop: Double
  ):

    def printReport(): Unit =
      println("=" * 80)
      println(s"SPATIAL DISTRIBUTION REPORT: $scenario")
      println("=" * 80)

      println("\n--- POPULATION OVERVIEW ---")
      println(f"  Total populations created: $totalPopulations")
      println(f"  Populations in network: $populationsInNetwork (${populationsInNetwork.toDouble/totalPopulations*100}%.1f%%)")
      println(f"  Avg populations per species: $avgPopsPerSpecies%.1f")

      println("\n--- TILE OCCUPANCY ---")
      println(f"  Total tiles: $totalTiles")
      println(f"  Occupied tiles: $occupiedTiles ($occupancyRate%.1f%%)")
      println(f"  Empty tiles: $emptyTiles")
      println(f"  Avg populations per tile (all): $avgPopsPerTile%.2f")
      println(f"  Avg populations per tile (occupied): $avgPopsPerOccupiedTile%.2f")

      println("\n--- SPECIES DISTRIBUTION ---")
      println(f"  Avg species per tile (all): $avgSpeciesPerTile%.2f")
      println(f"  Avg species per tile (occupied): $avgSpeciesPerOccupiedTile%.2f")
      println(f"  Max species in single tile: $maxSpeciesInTile")

      println("\n--- SPATIAL CLUSTERING ---")
      println(f"  Tile Gini coefficient: $tileGiniCoefficient%.3f (0=equal, 1=clustered)")
      println(f"  Species co-occurrence: $speciesCoOccurrence%.3f (avg species pairs per tile)")

      println("\n--- SPECIES SPATIAL EXTENT ---")
      println(f"  Avg species range X: $avgSpeciesRangeX%.3f")
      println(f"  Avg species range Y: $avgSpeciesRangeY%.3f")
      println(f"  Avg species diameter: $avgSpeciesDiameter%.3f")
      println(f"  Species diameter range: $minSpeciesDiameter%.3f - $maxSpeciesDiameter%.3f")
      println(f"  Species covering >50%% landscape: $speciesCoveringLandscape")
      println(f"  Avg species bbox area: $avgSpeciesBBoxArea%.4f (fraction of landscape)")
      println(f"  Species bbox area range: $minSpeciesBBoxArea%.4f - $maxSpeciesBBoxArea%.4f")

      println("\n--- NETWORK METRICS ---")
      println(f"  Network density: $networkDensity%.4f")
      println(f"  Avg interactions per population: $avgInteractionsPerPop%.2f")

      println("=" * 80)

  def analyze(
    scenario: String,
    worldParameters: WorldParameters,
    seed: Long
  ): SpatialReport = {

    println(s"\nAnalyzing: $scenario...")

    // Build world without conservation to see pure spatial distribution
    val conservationParameters = ConservationParameters(
      fractionProtected = 0.0,  // No protection for pure spatial analysis
      strategy = Some(ConservationStrategy.Random)
    )

    val world = World(
      worldParameters = worldParameters,
      conservationParameters = conservationParameters,
      rnd = Random(seed)
    )

    // Use the world's own grid (as built from avgPopulationsPerTile)
    analyzeWorld(scenario, world, world.managementLandscape.grid)
  }

  /**
   * Analyze an already-built world, re-tiling it onto a grid with the requested
   * number of populations per tile. This lets a single (expensive) community be
   * characterized at several tile sizes without rebuilding metaweb/populations/web.
   * The tile-independent metrics (population counts, species extent, network density)
   * are unaffected by tileSize; only tile-occupancy/clustering metrics change.
   */
  def analyzeWorld(
    scenario: String,
    world: World,
    avgPopulationsPerTile: Int
  ): SpatialReport = {
    // Tile count is derived from the populations that are actually in the network,
    // matching DiagnosticMetrics.betaForGrid so both use an identical grid for a
    // given tile size.
    val numberOfPopulations = world.populationWeb.vertexSet().size()
    val nTiles = math.max(1, (numberOfPopulations.toDouble / avgPopulationsPerTile.toDouble).toInt)
    val grid = SquareGrid.fromArea(nTiles)
    analyzeWorld(scenario, world, grid)
  }

  /**
   * Core analysis against an explicit grid. Re-buckets the world's populations into
   * the given grid (a cheap coordinate-to-cell pass) and computes the full report.
   */
  def analyzeWorld(
    scenario: String,
    world: World,
    grid: SquareGrid
  ): SpatialReport = {

    val allPopulations = world.populations
    val networkPopulations = world.populationWeb.vertexSet().asScala.toSeq

    // Re-tile the (already placed) populations onto the requested grid
    val landscape = ManagementLandscape(grid, networkPopulations, world.populationWeb, Random(0))
    val nTiles = landscape.managementAreas.size
    val tilePopulations: Seq[Int] = landscape.managementAreas.map { area =>
      area.populations.size
    }

    val occupiedTiles = tilePopulations.count(x => x > 0)
    val emptyTiles = tilePopulations.count(x => x == 0)

    // Species distribution across tiles
    val tileSpecies: Seq[Int] = landscape.managementAreas.map { area =>
      area.populations.values.toSet.size
    }

    val maxSpeciesInTile = if (tileSpecies.nonEmpty) tileSpecies.max else 0
    val avgSpeciesPerTile = if (nTiles > 0) tileSpecies.sum.toDouble / nTiles else 0.0
    val avgSpeciesPerOccupiedTile = if (occupiedTiles > 0) tileSpecies.filter(x => x > 0).sum.toDouble / occupiedTiles else 0.0

    // Gini coefficient for population distribution across tiles
    val gini = calculateGini(tilePopulations)

    // Species co-occurrence (how many species pairs share tiles)
    val coOccurrence = calculateCoOccurrence(landscape.managementAreas)

    // Species spatial extent analysis
    val speciesExtents = calculateSpeciesExtents(allPopulations)

    // Network metrics
    val numEdges = world.populationWeb.edgeSet().size()
    val numNodes = world.populationWeb.vertexSet().size()
    val networkDensity = if (numNodes > 1) {
      numEdges.toDouble / (numNodes * (numNodes - 1))
    } else 0.0

    val avgInteractions = if (numNodes > 0) numEdges.toDouble / numNodes else 0.0

    SpatialReport(
      scenario = scenario,
      totalPopulations = allPopulations.size,
      populationsInNetwork = networkPopulations.size,
      avgPopsPerSpecies = allPopulations.size.toDouble / world.worldParameters.numberOfSpecies,
      totalTiles = nTiles,
      occupiedTiles = occupiedTiles,
      occupancyRate = occupiedTiles.toDouble / nTiles * 100,
      avgPopsPerTile = tilePopulations.sum.toDouble / nTiles,
      avgPopsPerOccupiedTile = if (occupiedTiles > 0) tilePopulations.filter(x => x > 0).sum.toDouble / occupiedTiles else 0.0,
      emptyTiles = emptyTiles,
      avgSpeciesPerTile = avgSpeciesPerTile,
      avgSpeciesPerOccupiedTile = avgSpeciesPerOccupiedTile,
      maxSpeciesInTile = maxSpeciesInTile,
      tileGiniCoefficient = gini,
      speciesCoOccurrence = coOccurrence,
      avgSpeciesRangeX = speciesExtents._1,
      avgSpeciesRangeY = speciesExtents._2,
      avgSpeciesDiameter = speciesExtents._3,
      minSpeciesDiameter = speciesExtents._4,
      maxSpeciesDiameter = speciesExtents._5,
      speciesCoveringLandscape = speciesExtents._6,
      avgSpeciesBBoxArea = speciesExtents._7,
      minSpeciesBBoxArea = speciesExtents._8,
      maxSpeciesBBoxArea = speciesExtents._9,
      networkDensity = networkDensity,
      avgInteractionsPerPop = avgInteractions
    )
  }

  /**
   * Calculate spatial extent for each species
   * Returns: (avgRangeX, avgRangeY, avgDiameter, minDiameter, maxDiameter,
   *           numCoveringLandscape, avgBBoxArea, minBBoxArea, maxBBoxArea)
   */
  private def calculateSpeciesExtents(populations: Seq[Population]): (Double, Double, Double, Double, Double, Int, Double, Double, Double) = {
    if (populations.isEmpty) return (0.0, 0.0, 0.0, 0.0, 0.0, 0, 0.0, 0.0, 0.0)

    val populationsBySpecies = populations.groupBy(_.species.id)

    val extents = populationsBySpecies.map { case (speciesId, pops) =>
      if (pops.isEmpty) {
        (0.0, 0.0, 0.0, 0.0)
      } else {
        val xs = pops.map(_.coordinates.x)
        val ys = pops.map(_.coordinates.y)

        // Calculate range considering periodic boundaries
        val rangeX = calculatePeriodicRange(xs)
        val rangeY = calculatePeriodicRange(ys)

        // Diameter = diagonal of bounding box
        val diameter = math.sqrt(rangeX * rangeX + rangeY * rangeY)

        // Bounding-box area as fraction of the unit landscape
        val bboxArea = rangeX * rangeY

        (rangeX, rangeY, diameter, bboxArea)
      }
    }.toSeq

    val rangesX = extents.map(_._1)
    val rangesY = extents.map(_._2)
    val diameters = extents.map(_._3)
    val bboxAreas = extents.map(_._4)

    val avgRangeX = if (rangesX.nonEmpty) rangesX.sum / rangesX.size else 0.0
    val avgRangeY = if (rangesY.nonEmpty) rangesY.sum / rangesY.size else 0.0
    val avgDiameter = if (diameters.nonEmpty) diameters.sum / diameters.size else 0.0
    val minDiameter = if (diameters.nonEmpty) diameters.min else 0.0
    val maxDiameter = if (diameters.nonEmpty) diameters.max else 0.0
    val covering = diameters.count(_ > 0.5)
    val avgBBoxArea = if (bboxAreas.nonEmpty) bboxAreas.sum / bboxAreas.size else 0.0
    val minBBoxArea = if (bboxAreas.nonEmpty) bboxAreas.min else 0.0
    val maxBBoxArea = if (bboxAreas.nonEmpty) bboxAreas.max else 0.0

    (avgRangeX, avgRangeY, avgDiameter, minDiameter, maxDiameter, covering, avgBBoxArea, minBBoxArea, maxBBoxArea)
  }

  /**
   * Calculate range considering periodic boundaries [0,1]
   * Takes minimum of direct range and wrapped range
   */
  private def calculatePeriodicRange(values: Seq[Double]): Double = {
    if (values.size <= 1) return 0.0

    val sorted = values.sorted
    val min = sorted.head
    val max = sorted.last

    // Direct range
    val directRange = max - min

    // Wrapped range (going the other way around periodic boundary)
    val wrappedRange = (1.0 - max) + min

    // Take smaller range
    math.min(directRange, wrappedRange)
  }

  /**
   * Calculate Gini coefficient for distribution inequality
   * 0 = perfectly equal, 1 = maximally unequal
   */
  private def calculateGini(values: Seq[Int]): Double = {
    if (values.isEmpty || values.sum == 0) return 0.0

    val n = values.size
    val sorted = values.sorted

    val numerator = (1 to n).zip(sorted).map { case (i, x) => i * x }.sum * 2.0
    val denominator = n * values.sum.toDouble

    (numerator / denominator) - (n + 1.0) / n
  }

  /**
   * Calculate average species co-occurrence (species pairs per tile)
   */
  private def calculateCoOccurrence(areas: Seq[ManagementArea]): Double = {
    val coOccurrences = areas.map { area =>
      val speciesInTile = area.populations.values.toSet.size
      if (speciesInTile > 1) {
        // Number of species pairs = n choose 2
        speciesInTile * (speciesInTile - 1) / 2.0
      } else 0.0
    }

    if (areas.nonEmpty) coOccurrences.sum / areas.size else 0.0
  }

end SpatialDiagnostics
