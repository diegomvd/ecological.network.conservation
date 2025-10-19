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
    
    // PHASE 2: Spatial preparation (O(n))  
    val typicalDistance = calculateTypicalDistance(populationsBySpecies)
    val spatialIndex = buildSingleGrid(populations, typicalDistance)
    
    //println(f"Typical interaction distance: ${typicalDistance}%.4f")
    
    // PHASE 3: Species-first processing 
    val allInteractingPairs = scala.collection.mutable.ListBuffer[(Population, Population)]()
    var totalChecks = 0
    var spatiallyFiltered = 0
    var bruteForceUsed = 0
    
    interactingSpeciesPairs.foreach { case (speciesA, speciesB) =>
      val popsA = populationsBySpecies.getOrElse(speciesA, Seq.empty)
      val popsB = populationsBySpecies.getOrElse(speciesB, Seq.empty)
      
      if (popsA.nonEmpty && popsB.nonEmpty) {
        val (pairs, checks, spatial, brute) = findInteractingPopulations(popsA, popsB, spatialIndex)
        allInteractingPairs ++= pairs
        totalChecks += checks
        spatiallyFiltered += spatial  
        bruteForceUsed += brute
      }
    }
    
    //println(s"Distance checks: ${totalChecks} (spatially filtered: ${spatiallyFiltered}, brute force: ${bruteForceUsed})")
    //println(s"Found ${allInteractingPairs.size} interacting population pairs")
    
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
   * Build single adaptive spatial grid optimized for typical interactions
   */
  private def buildSingleGrid(populations: Seq[Population], typicalDistance: Double): SimpleSpatialIndex = {
    // Size cells so that typical interactions span ~2×2 = 4 cells (good spatial locality)
    val cellSize = typicalDistance / 2.0
    val gridSize = math.max(5, math.min(100, math.ceil(1.0 / cellSize).toInt))
    
    //println(f"Building spatial index: ${gridSize}x${gridSize} cells, cell size ${cellSize}%.4f")
    
    val spatialIndex = SimpleSpatialIndex(cellSize, gridSize)
    
    populations.foreach { pop =>
      val cellId = spatialIndex.getCellId(pop.coordinates)
      spatialIndex.addPopulation(cellId, pop)
    }
    
    val occupiedCells = spatialIndex.getOccupiedCells.size
    //println(s"Populations distributed across ${occupiedCells}/${gridSize * gridSize} cells")
    
    spatialIndex
  }
  
  /**
   * Find interacting population pairs between two species using spatial optimization
   * Returns: (pairs, totalChecks, spatiallyFiltered, bruteForceCount)
   */
  private def findInteractingPopulations(
    popsA: Seq[Population], 
    popsB: Seq[Population], 
    spatialIndex: SimpleSpatialIndex
  ): (Seq[(Population, Population)], Int, Int, Int) = {
    
    val interactingPairs = scala.collection.mutable.ListBuffer[(Population, Population)]()
    var totalChecks = 0
    var spatiallyFiltered = 0
    var bruteForceCount = 0
    
    // Calculate max home range in species B for spatial strategy decisions
    val maxHomeRangeB = if (popsB.nonEmpty) popsB.map(_.species.homeRange).max else 0.0
    
    popsA.foreach { popA =>
      
      // Calculate maximum possible interaction distance with species B
      val maxDistance = (popA.species.homeRange + maxHomeRangeB)
      
      val candidatesB = if (maxDistance > 0.3) {
        // Brute force: interaction range too large for spatial optimization
        bruteForceCount += 1
        popsB
      } else {
        // Spatial filtering: get candidates from nearby cells
        spatiallyFiltered += 1
        getSpatialCandidates(popA, popsB, spatialIndex, maxDistance)
      }
      
      // Check exact distances for all candidates
      candidatesB.foreach { popB =>
        if (popA.id < popB.id) { // Avoid double-counting pairs
          totalChecks += 1
          if (canInteract(popA, popB)) {
            interactingPairs += ((popA, popB))
          }
        }
      }
    }
    
    (interactingPairs.toSeq, totalChecks, spatiallyFiltered, bruteForceCount)
  }
  
  /**
   * Get spatial candidates using Moore neighborhood (8 surrounding cells)
   */
  private def getSpatialCandidates(
    popA: Population, 
    popsB: Seq[Population], 
    spatialIndex: SimpleSpatialIndex, 
    maxDistance: Double
  ): Seq[Population] = {
    
    val cellId = spatialIndex.getCellId(popA.coordinates)
    val relevantCells = spatialIndex.getCellsWithinDistance(cellId, maxDistance)
    val spatialCandidates = relevantCells.flatMap(spatialIndex.getPopulations).toSet
    
    // Filter popsB to only those that are spatially nearby
    popsB.filter(spatialCandidates.contains)
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
  
  private val cells = scala.collection.mutable.Map[Int, scala.collection.mutable.ListBuffer[Population]]()
  
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
   * Add population to a cell
   */
  def addPopulation(cellId: Int, population: Population): Unit = {
    cells.getOrElseUpdate(cellId, scala.collection.mutable.ListBuffer.empty) += population
  }
  
  /**
   * Get all populations in a cell
   */
  def getPopulations(cellId: Int): Seq[Population] = {
    cells.getOrElse(cellId, scala.collection.mutable.ListBuffer.empty).toSeq
  }
  
  /**
   * Get all occupied cell IDs
   */
  def getOccupiedCells: Seq[Int] = cells.keys.toSeq
  
  /**
   * Get all cells within a given distance using Moore neighborhood
   * Handles periodic boundaries correctly
   */
  def getCellsWithinDistance(centerCellId: Int, maxDistance: Double): Seq[Int] = {
    val cellRadius = math.ceil(maxDistance / cellSize).toInt
    val (centerCol, centerRow) = cellIdToCoord(centerCellId)
    
    val result = scala.collection.mutable.ListBuffer[Int]()
    
    // Use Moore neighborhood (8-connected)
    for {
      dRow <- -cellRadius to cellRadius
      dCol <- -cellRadius to cellRadius
    } {
      // Handle periodic boundaries
      val wrappedCol = ((centerCol + dCol) % gridSize + gridSize) % gridSize
      val wrappedRow = ((centerRow + dRow) % gridSize + gridSize) % gridSize
      val cellId = wrappedRow * gridSize + wrappedCol
      
      // Check if this cell center is actually within the distance
      val cellCenter = getCellCenter(cellId)
      val centerCellCenter = getCellCenter(centerCellId)
      val actualDistance = SquareGrid.periodicDistance(centerCellCenter, cellCenter)
      
      if (actualDistance <= maxDistance + cellSize * 0.5) { // Add buffer for cell size
        result += cellId
      }
    }
    
    result.toSeq
  }
  
  /**
   * Convert cell ID back to (col, row) coordinates
   */
  private def cellIdToCoord(cellId: Int): (Int, Int) = {
    val row = cellId / gridSize
    val col = cellId % gridSize
    (col, row)
  }
  
  /**
   * Get center coordinate of a cell
   */
  private def getCellCenter(cellId: Int): Coordinate = {
    val (col, row) = cellIdToCoord(cellId)
    new Coordinate(
      (col + 0.5) * cellSize,
      (row + 0.5) * cellSize,
      0.0
    )
  }