package econetcons

import scala.util.Random
import org.jgrapht.*
import org.jgrapht.graph.*
import org.locationtech.jts.geom.Coordinate
import scala.jdk.CollectionConverters.*

object PopulationWebGenerator:

  /**
   * Generate population web using species-first + simple spatial indexing strategy
   * Cleaned up version without legacy maxHomeRange parameter
   */
  def generatePopulationWeb(
    populations: Seq[Population], 
    metaWeb: DefaultDirectedGraph[Species,DefaultEdge]
  ): DefaultDirectedGraph[Population,DefaultEdge] = {
    
   // println(s"Generating population web for ${populations.size} populations...")
    
    // PHASE 1: Species organization (O(n))
    val populationsBySpecies = populations.groupBy(_.species)
    val interactingSpeciesPairs = extractInteractingPairs(metaWeb)
    
    //println(s"Found ${interactingSpeciesPairs.size} interacting species pairs out of ${metaWeb.vertexSet().size() * metaWeb.vertexSet().size()} possible")
    //println(s"Species distribution: ${populationsBySpecies.map(p => s"${p._1.id}→${p._2.size}").mkString(", ")}")
    
    // PHASE 2: Spatial preparation (O(n)) — grid partitioned by cell AND species so the
    // edge search below only ever touches populations of the target species in nearby cells.
    val typicalDistance = calculateTypicalDistance(populationsBySpecies)
    val spatialIndex = buildSingleGrid(populations, typicalDistance)

    // PHASE 3: grid-driven edge search. For each interacting (A,B) species pair, for each
    // popA pull only species-B populations from cells within the interaction radius — no
    // scan of the full popsB list, no large-home-range brute-force fallback. O(pairs·neighbors).
    val allInteractingPairs = scala.collection.mutable.ListBuffer[(Population, Population)]()

    interactingSpeciesPairs.foreach { case (speciesA, speciesB) =>
      val popsA = populationsBySpecies.getOrElse(speciesA, Seq.empty)
      if (popsA.nonEmpty && populationsBySpecies.get(speciesB).exists(_.nonEmpty)) {
        val maxDistance = speciesA.homeRange + speciesB.homeRange
        popsA.foreach { popA =>
          spatialIndex.foreachNeighborOfSpecies(popA, speciesB, maxDistance) { popB =>
            // Preserve original dedup (id ordering) and exact interaction test.
            if (popA.id < popB.id && canInteract(popA, popB))
              allInteractingPairs += ((popA, popB))
          }
        }
      }
    }

    // PHASE 4: Build graph (O(pairs))
    buildPopulationGraph(allInteractingPairs.toSeq, metaWeb)
  }
  
  /**
   * Extract all species pairs that can potentially interact from the metaWeb
   */
  private def extractInteractingPairs(metaWeb: DefaultDirectedGraph[Species,DefaultEdge]): Set[(Species, Species)] = {
    val pairs = scala.collection.mutable.Set[(Species, Species)]()
    
    metaWeb.vertexSet().asScala.foreach { predator =>
      metaWeb.outgoingEdgesOf(predator).asScala.foreach { edge =>
        val prey = metaWeb.getEdgeTarget(edge)
        // Add both directions since we want to check spatial proximity regardless of predator/prey role
        pairs += ((predator, prey))
        pairs += ((prey, predator))
      }
    }
    
    pairs.toSet
  }
  
  /**
   * Calculate typical interaction distance to size the spatial grid optimally
   */
  private def calculateTypicalDistance(populationsBySpecies: Map[Species, Seq[Population]]): Double = {
    val species = populationsBySpecies.keys.toSeq
    
    if (species.size < 2) {
      0.1 // Default fallback
    } else {
      val allInteractionDistances = for {
        speciesA <- species
        speciesB <- species
        if speciesA != speciesB
      } yield {
        (speciesA.homeRange + speciesB.homeRange)
      }
      
      val sortedDistances = allInteractionDistances.sorted
      val median = sortedDistances(sortedDistances.size / 2)
      
      //println(f"Home range distribution: ${species.map(_.homeRange).sorted.mkString(", ")}")
      //println(f"Interaction distances: min=${sortedDistances.head}%.4f, median=${median}%.4f, max=${sortedDistances.last}%.4f")
      
      median
    }
  }
  
  /**
   * Build a uniform cell-list grid that tiles [0,1)² exactly (cellSize = 1/gridSize).
   * `typicalDistance` sizes the cells for locality, but correctness does not depend on it:
   * the neighbour search below expands its cell radius to cover each pair's actual
   * interaction distance, so no real interaction is ever missed.
   */
  private def buildSingleGrid(populations: Seq[Population], typicalDistance: Double): SimpleSpatialIndex = {
    // ~2 cells per typical interaction distance, clamped to a sane grid size. Exact tiling
    // (cellSize = 1/gridSize) so getCellId never clamps coordinates into a wrong cell.
    val targetCells = math.ceil(2.0 / math.max(typicalDistance, 1e-6)).toInt
    val gridSize = math.max(3, math.min(100, targetCells))
    val cellSize = 1.0 / gridSize

    val spatialIndex = SimpleSpatialIndex(cellSize, gridSize)
    populations.foreach { pop =>
      spatialIndex.addPopulation(spatialIndex.getCellId(pop.coordinates), pop)
    }
    spatialIndex
  }
  
  /**
   * Check if two populations can interact based on distance and home ranges
   * Cleaned up version without legacy maxHomeRange parameter
   */
  private def canInteract(p1: Population, p2: Population): Boolean = {
    val distance = SquareGrid.periodicDistance(p1.coordinates, p2.coordinates)
    val interactionRadius = (p1.species.homeRange + p2.species.homeRange)
    distance < interactionRadius
  }
  
  /**
   * Build the population web from interacting pairs
   */
  private def buildPopulationGraph(
    interactingPairs: Seq[(Population, Population)], 
    metaWeb: DefaultDirectedGraph[Species, DefaultEdge]
  ): DefaultDirectedGraph[Population, DefaultEdge] = {
    
    // Pre-compute species relationships for O(1) lookup
    val speciesRelationships = scala.collection.mutable.Map[(Species, Species), Boolean]()
    
    metaWeb.vertexSet().asScala.foreach { predator =>
      val preySpecies = metaWeb.outgoingEdgesOf(predator).asScala.map(metaWeb.getEdgeTarget).toSet
      
      metaWeb.vertexSet().asScala.foreach { potentialPrey =>
        speciesRelationships((predator, potentialPrey)) = preySpecies.contains(potentialPrey)
      }
    }
    
    val populationWeb = DefaultDirectedGraph[Population, DefaultEdge](classOf[DefaultEdge])
    
    interactingPairs.foreach { case (p1, p2) =>
      populationWeb.addVertex(p1)
      populationWeb.addVertex(p2)
      
      // Add directed edges based on predator-prey relationships
      if (speciesRelationships.getOrElse((p1.species, p2.species), false)) {
        populationWeb.addEdge(p1, p2, DefaultEdge())
      }
      
      if (speciesRelationships.getOrElse((p2.species, p1.species), false)) {
        populationWeb.addEdge(p2, p1, DefaultEdge())
      }
    }
    
   // println(s"Built population web with ${populationWeb.vertexSet().size()} vertices and ${populationWeb.edgeSet().size()} edges")
    populationWeb
  }

end PopulationWebGenerator

/**
 * Simple spatial index optimized for ecological interactions
 * Single-tier grid with Moore neighborhood support
 */
case class SimpleSpatialIndex(cellSize: Double, gridSize: Int):

  // cellId -> (speciesId -> populations in that cell). Partitioning by species lets the
  // edge search pull only the target species' populations from nearby cells, so the inner
  // loop scales with local neighbours rather than the full population list of that species.
  private val cells =
    scala.collection.mutable.Map[Int, scala.collection.mutable.Map[Int, scala.collection.mutable.ListBuffer[Population]]]()

  /**
   * Get cell ID for given coordinates with periodic boundary handling
   */
  def getCellId(coord: Coordinate): Int = {
    // Handle periodic boundaries - wrap coordinates to [0,1)
    val wrappedX = coord.x - math.floor(coord.x)
    val wrappedY = coord.y - math.floor(coord.y)

    val col = math.min((wrappedX / cellSize).toInt, gridSize - 1)
    val row = math.min((wrappedY / cellSize).toInt, gridSize - 1)
    row * gridSize + col
  }

  /**
   * Add population to a cell, bucketed by its species id.
   */
  def addPopulation(cellId: Int, population: Population): Unit = {
    val bySpecies = cells.getOrElseUpdate(cellId, scala.collection.mutable.Map.empty)
    bySpecies.getOrElseUpdate(population.species.id, scala.collection.mutable.ListBuffer.empty) += population
  }

  /**
   * Get all occupied cell IDs
   */
  def getOccupiedCells: Seq[Int] = cells.keys.toSeq

  /**
   * Visit every population of `species` located in a cell within `maxDistance` of `popA`,
   * applying `f`. Only species-partitioned buckets in the relevant cells are touched.
   */
  def foreachNeighborOfSpecies(popA: Population, species: Species, maxDistance: Double)(f: Population => Unit): Unit = {
    val centerCellId = getCellId(popA.coordinates)
    // Cap the radius at what's needed to cover the whole grid once. Without this cap, a
    // maxDistance large enough that 2*cellRadius+1 > gridSize would wrap around and visit
    // the same cell (and thus the same candidate) more than once, producing duplicate pairs.
    val rawRadius = math.ceil(maxDistance / cellSize).toInt
    val cellRadius = math.min(rawRadius, gridSize / 2)
    val (centerCol, centerRow) = cellIdToCoord(centerCellId)
    val speciesId = species.id

    // When the capped box already spans the whole grid, iterate each distinct column/row once.
    val fullSpan = (2 * cellRadius + 1) >= gridSize
    val colOffsets = if fullSpan then 0 until gridSize else ((centerCol - cellRadius) to (centerCol + cellRadius)).map(c => ((c % gridSize) + gridSize) % gridSize)
    val rowOffsets = if fullSpan then 0 until gridSize else ((centerRow - cellRadius) to (centerRow + cellRadius)).map(r => ((r % gridSize) + gridSize) % gridSize)

    rowOffsets.foreach { wrappedRow =>
      colOffsets.foreach { wrappedCol =>
        val cellId = wrappedRow * gridSize + wrappedCol
        cells.get(cellId).flatMap(_.get(speciesId)).foreach(_.foreach(f))
      }
    }
  }

  /**
   * Convert cell ID back to (col, row) coordinates
   */
  private def cellIdToCoord(cellId: Int): (Int, Int) = {
    val row = cellId / gridSize
    val col = cellId % gridSize
    (col, row)
  }