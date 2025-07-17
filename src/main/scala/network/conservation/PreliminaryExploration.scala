package network.conservation
import scala.util.Random
import org.locationtech.jts.geom.Geometry
import scala.collection.mutable.{Map => MutableMap}
import java.io.{File, PrintWriter}
import org.jgrapht.graph.DefaultDirectedGraph
import org.jgrapht.graph.DefaultEdge
import scala.jdk.CollectionConverters.*

object NetworkAnalysis extends App {
  // Analysis parameters to track
  case class NetworkMetrics(
    populationInteractions: Int,
    avgInteractionsPerPopulation: Double,
    speciesInteractions: Int,
    avgInteractionsPerSpecies: Double,
    isolatedPopulations: Int,
    maxInteractionsPerPopulation: Int,
    possibleSpeciesInteractions: Int,
    potentialPopulationInteractions: Int,
    realizationRatio: Double
  )
  
  case class SpatialMetrics(
    avgPopulationsPerHex: Double,
    stdDevPopulationsPerHex: Double,
    maxPopulationsPerHex: Int,
    emptyHexagons: Int
  )

  case class SimulationResult(
    basalHomeRange: Double,
    decayDistance: Double,
    beforeExtinction: (NetworkMetrics, SpatialMetrics),
    afterExtinction: (NetworkMetrics, SpatialMetrics)
  )

  case class SimulationResultC(
    connectivity: Double,
    beforeExtinction: (NetworkMetrics, SpatialMetrics),
    afterExtinction: (NetworkMetrics, SpatialMetrics)
  )

  def analyzeWorld(world: World): (NetworkMetrics, SpatialMetrics) = {
    // Network analysis
    val popWeb = world.populationWeb
    val metaWeb = world.getRealizedWeb
    val nonExtinctPops = world.populations.filterNot(_.extinct)
    
    val populationInteractions = popWeb.edgeSet().size()
    val populations = nonExtinctPops.size
    val avgInteractionsPerPop = if(populations > 0) populationInteractions.toDouble / populations else 0
    
    val speciesInteractions = metaWeb.edgeSet().size()
    val species = metaWeb.vertexSet().size()
    val avgInteractionsPerSpecies = if(species > 0) speciesInteractions.toDouble / species else 0
    
    val isolatedPops = nonExtinctPops.count(p => !popWeb.containsVertex(p))
    
    val maxInteractions = if(populations > 0) 
      nonExtinctPops.map(p => if(popWeb.containsVertex(p)) popWeb.degreeOf(p) else 0).max 
    else 0

    // Spatial analysis - FIXED VERSION
    val hexCounts = MutableMap[Int, Int]().withDefaultValue(0)
    
    // Initialize all hexagons with 0
    world.managementLandscape.managementAreas.foreach(area => 
      hexCounts(area.id) = 0
    )
    
    // Count populations per hexagon
    world.managementLandscape.managementAreas.foreach { area =>
      val nonExtinctCount = area.populations.count { case (popId, _) =>
        world.populations.exists(p => p.id == popId && !p.extinct)
      }
      hexCounts(area.id) = nonExtinctCount
    }
    
    val counts = hexCounts.values.toSeq
    val totalHexagons = world.managementLandscape.managementAreas.size
    val avgPerHex = populations.toDouble / totalHexagons
    val stdDev = math.sqrt(
      counts.map(c => math.pow(c - avgPerHex, 2)).sum / totalHexagons
    )
    val maxPerHex = counts.max
    val emptyHexes = counts.count(_ == 0)  // Now correctly counts hexagons with zero populations

    // Calculate potential interactions
    val possibleSpeciesInteractions = calculatePossibleSpeciesInteractions(world.metaWeb)
    val (potentialPopulationInteractions, realizationRatio) = calculateInteractionPotential(world)

    (
      NetworkMetrics(
        populationInteractions,
        avgInteractionsPerPop,
        speciesInteractions,
        avgInteractionsPerSpecies,
        isolatedPops,
        maxInteractions,
        possibleSpeciesInteractions,
        potentialPopulationInteractions,
        realizationRatio
      ),
      SpatialMetrics(
        avgPerHex,
        stdDev,
        maxPerHex,
        emptyHexes
      )
    )
  }

  // Helper method to calculate possible species interactions
  private def calculatePossibleSpeciesInteractions(metaWeb: DefaultDirectedGraph[Species, DefaultEdge]): Int = {
    val species = metaWeb.vertexSet().asScala.toSet
    species.flatMap(s1 => 
      species.filter(s2 => s1 != s2 && metaWeb.containsEdge(s1, s2))
    ).size
  }

  // Helper method to calculate interaction potential
  private def calculateInteractionPotential(world: World): (Int, Double) = {
    val popWeb = world.populationWeb
    val metaWeb = world.metaWeb
    val nonExtinctPops = world.populations.filterNot(_.extinct)
    
    // Calculate potential interactions based on species interactions in metaweb
    val potentialInteractions = nonExtinctPops.flatMap(p1 => 
      nonExtinctPops.filter(p2 => 
        p1 != p2 && 
        metaWeb.containsEdge(p1.species, p2.species)
      )
    ).size / 2  // Divide by 2 to avoid double counting
    
    val realizationRatio = if (potentialInteractions > 0) {
      popWeb.edgeSet().size().toDouble / potentialInteractions
    } else 0.0
    
    (potentialInteractions, realizationRatio)
  }

  def exportJSON(results: Seq[SimulationResult], filename: String): Unit = {
    val json = results.map { result =>
      s"""{
        "parameters": {
          "basalHomeRange": ${result.basalHomeRange},
          "decayDistance": ${result.decayDistance}
        },
        "beforeExtinction": {
          "network": {
            "populationInteractions": ${result.beforeExtinction._1.populationInteractions},
            "avgInteractionsPerPop": ${result.beforeExtinction._1.avgInteractionsPerPopulation},
            "speciesInteractions": ${result.beforeExtinction._1.speciesInteractions},
            "avgInteractionsPerSpecies": ${result.beforeExtinction._1.avgInteractionsPerSpecies},
            "isolatedPopulations": ${result.beforeExtinction._1.isolatedPopulations},
            "maxInteractionsPerPop": ${result.beforeExtinction._1.maxInteractionsPerPopulation}
          },
          "spatial": {
            "avgPopulationsPerHex": ${result.beforeExtinction._2.avgPopulationsPerHex},
            "stdDevPopulationsPerHex": ${result.beforeExtinction._2.stdDevPopulationsPerHex},
            "maxPopulationsPerHex": ${result.beforeExtinction._2.maxPopulationsPerHex},
            "emptyHexagons": ${result.beforeExtinction._2.emptyHexagons}
          }
        },
        "afterExtinction": {
          "network": {
            "populationInteractions": ${result.afterExtinction._1.populationInteractions},
            "avgInteractionsPerPop": ${result.afterExtinction._1.avgInteractionsPerPopulation},
            "speciesInteractions": ${result.afterExtinction._1.speciesInteractions},
            "avgInteractionsPerSpecies": ${result.afterExtinction._1.avgInteractionsPerSpecies},
            "isolatedPopulations": ${result.afterExtinction._1.isolatedPopulations},
            "maxInteractionsPerPop": ${result.afterExtinction._1.maxInteractionsPerPopulation}
          },
          "spatial": {
            "avgPopulationsPerHex": ${result.afterExtinction._2.avgPopulationsPerHex},
            "stdDevPopulationsPerHex": ${result.afterExtinction._2.stdDevPopulationsPerHex},
            "maxPopulationsPerHex": ${result.afterExtinction._2.maxPopulationsPerHex},
            "emptyHexagons": ${result.afterExtinction._2.emptyHexagons}
          }
        }
      }"""
    }.mkString("[\n", ",\n", "\n]")

    val writer = new PrintWriter(new File(filename))
    writer.write(json)
    writer.close()
  }

  def exportCSV(results: Seq[SimulationResult], filename: String): Unit = {
    val header = Seq(
      "basalHomeRange",
      "decayDistance",
      "before_populationInteractions",
      "before_avgInteractionsPerPop",
      "before_speciesInteractions",
      "before_avgInteractionsPerSpecies",
      "before_isolatedPopulations",
      "before_maxInteractionsPerPop",
      "before_avgPopulationsPerHex",
      "before_stdDevPopulationsPerHex",
      "before_maxPopulationsPerHex",
      "before_emptyHexagons",
      "after_populationInteractions",
      "after_avgInteractionsPerPop",
      "after_speciesInteractions",
      "after_avgInteractionsPerSpecies",
      "after_isolatedPopulations",
      "after_maxInteractionsPerPop",
      "after_avgPopulationsPerHex",
      "after_stdDevPopulationsPerHex",
      "after_maxPopulationsPerHex",
      "after_emptyHexagons"
    ).mkString(",")

    val rows = results.map { r =>
      Seq(
        r.basalHomeRange,
        r.decayDistance,
        r.beforeExtinction._1.populationInteractions,
        r.beforeExtinction._1.avgInteractionsPerPopulation,
        r.beforeExtinction._1.speciesInteractions,
        r.beforeExtinction._1.avgInteractionsPerSpecies,
        r.beforeExtinction._1.isolatedPopulations,
        r.beforeExtinction._1.maxInteractionsPerPopulation,
        r.beforeExtinction._2.avgPopulationsPerHex,
        r.beforeExtinction._2.stdDevPopulationsPerHex,
        r.beforeExtinction._2.maxPopulationsPerHex,
        r.beforeExtinction._2.emptyHexagons,
        r.afterExtinction._1.populationInteractions,
        r.afterExtinction._1.avgInteractionsPerPopulation,
        r.afterExtinction._1.speciesInteractions,
        r.afterExtinction._1.avgInteractionsPerSpecies,
        r.afterExtinction._1.isolatedPopulations,
        r.afterExtinction._1.maxInteractionsPerPopulation,
        r.afterExtinction._2.avgPopulationsPerHex,
        r.afterExtinction._2.stdDevPopulationsPerHex,
        r.afterExtinction._2.maxPopulationsPerHex,
        r.afterExtinction._2.emptyHexagons
      ).mkString(",")
    }.mkString("\n")

    val writer = new PrintWriter(new File(filename))
    writer.write(s"$header\n$rows")
    writer.close()
  }

  def printAnalysis(
    params: WorldParameters,
    beforeExtinction: (NetworkMetrics, SpatialMetrics),
    afterExtinction: (NetworkMetrics, SpatialMetrics)
  ): Unit = {
    println(s"""
    |=== Parameter Settings ===
    |Basal Home Range: ${params.basalHomeRange}
    |Decay Distance: ${params.decayDistancePopulations}
    |
    |=== Before Extinctions ===
    |Network Properties:
    |- Population interactions: ${beforeExtinction._1.populationInteractions}
    |- Potential population interactions: ${beforeExtinction._1.potentialPopulationInteractions}
    |- Interaction realization ratio: ${f"${beforeExtinction._1.realizationRatio}%.2f"}
    |- Possible species interactions: ${beforeExtinction._1.possibleSpeciesInteractions}
    |- Avg interactions per population: ${f"${beforeExtinction._1.avgInteractionsPerPopulation}%.2f"}
    |- Species interactions: ${beforeExtinction._1.speciesInteractions}
    |- Avg interactions per species: ${f"${beforeExtinction._1.avgInteractionsPerSpecies}%.2f"}
    |- Isolated populations: ${beforeExtinction._1.isolatedPopulations}
    |- Max interactions per population: ${beforeExtinction._1.maxInteractionsPerPopulation}
    |
    |Spatial Properties:
    |- Avg populations per hexagon: ${f"${beforeExtinction._2.avgPopulationsPerHex}%.2f"}
    |- StdDev populations per hexagon: ${f"${beforeExtinction._2.stdDevPopulationsPerHex}%.2f"}
    |- Max populations in a hexagon: ${beforeExtinction._2.maxPopulationsPerHex}
    |- Empty hexagons: ${beforeExtinction._2.emptyHexagons}
    |
    |=== After Extinctions ===
    |Network Properties:
    |- Population interactions: ${afterExtinction._1.populationInteractions}
    |- Avg interactions per population: ${f"${afterExtinction._1.avgInteractionsPerPopulation}%.2f"}
    |- Species interactions: ${afterExtinction._1.speciesInteractions}
    |- Avg interactions per species: ${f"${afterExtinction._1.avgInteractionsPerSpecies}%.2f"}
    |- Isolated populations: ${afterExtinction._1.isolatedPopulations}
    |- Max interactions per population: ${afterExtinction._1.maxInteractionsPerPopulation}
    |
    |Spatial Properties:
    |- Avg populations per hexagon: ${f"${afterExtinction._2.avgPopulationsPerHex}%.2f"}
    |- StdDev populations per hexagon: ${f"${afterExtinction._2.stdDevPopulationsPerHex}%.2f"}
    |- Max populations in a hexagon: ${afterExtinction._2.maxPopulationsPerHex}
    |- Empty hexagons: ${afterExtinction._2.emptyHexagons}
    |""".stripMargin)
  }
  // Main execution
  val homeRanges = Seq(0.01, 0.05, 0.1)
  val decayDistances = Seq(1000.0, 10000.0, 100000.0)
  val connectivities = Seq(0.0,0.2,0.4,0.6,0.8,1.0)
  
  val results = for {
    //hr <- homeRanges
    //dd <- decayDistances
    c <- connectivities
  } yield {
    val params = WorldParameters(
      numberOfSpecies = 30,
      connectanceMetaWeb = 0.2,
      numberOfPopulations = 1000,
      basalHomeRange = 0.01,
      landscapeRadius = 3,
      fractionProtected = 0.3,
      connectivity = c,
      decayDistancePopulations = 1000,
      wSpRichness = 0.0,
      wInteractionRichness = 0.0, 
      wAbundance = 0.0    
    )
    
    // println(s"\nAnalyzing with basalHomeRange=$hr, decayDistance=$dd")
    println(s"\nAnalyzing with connectivity=$c")

    
    val initialWorld = World(
      numberOfSpecies = params.numberOfSpecies,
      connectanceMetaWeb = params.connectanceMetaWeb,
      numberOfPopulations = params.numberOfPopulations,
      basalHomeRange = params.basalHomeRange,
      landscapeRadius = params.landscapeRadius,
      fractionProtected = params.fractionProtected,
      connectivity = params.connectivity,
      decayDistancePopulations = params.decayDistancePopulations,
      wSpRichness = params.wSpRichness,
      wInteractionRichness = params.wInteractionRichness,
      wAbundance = params.wAbundance,
      rnd = Random(12L)
    )
    
    val beforeMetrics = analyzeWorld(initialWorld)
    val (finalWorld, _) = initialWorld.extinctions()
    
    val afterMetrics = analyzeWorld(finalWorld)
    
    printAnalysis(params, beforeMetrics, afterMetrics)
    // SimulationResult(hr, dd, beforeMetrics, afterMetrics)
    SimulationResultC(c, beforeMetrics, afterMetrics)

  }
  
  // Export results
  // exportJSON(results, "network_analysis_results.json")
  // exportCSV(results, "network_analysis_results.csv")
}