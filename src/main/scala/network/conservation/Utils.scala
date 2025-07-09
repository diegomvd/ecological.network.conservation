package network.conservation

import org.locationtech.jts.geom.{Coordinate, Envelope, Geometry, GeometryFactory}

import scala.jdk.CollectionConverters.*
import org.locationtech.jts.triangulate.VoronoiDiagramBuilder
import org.locationtech.jts.operation.distance.DistanceOp

import scala.annotation.tailrec
import scala.language.postfixOps
import scala.util.Random
import scala.math.sqrt


object Utils:

  case class GridCell(x: Int, y: Int)
  
  def fastDiskPacking(
    lambda: Double, 
    populationSeq: Seq[Population], 
    rnd: Random, 
    species: Species, 
    landscapeBoundary: Geometry
  ): Coordinate =
    // Domain is normalized to [0,1] × [0,sqrt(3)/2]
    val width = 1.0
    val height = width * sqrt(3.0)/2.0
    
    // Scale home range to normalized coordinates
    val homeRange = species.homeRange
    val cellSize = 1.8 * homeRange
    
    // Number of grid cells for spatial indexing
    val nCellsX = math.max(1, (width / cellSize).toInt)
    val nCellsY = math.max(1, (height / cellSize).toInt)
    
    case class GridCell(x: Int, y: Int)
    val gridIndex = scala.collection.mutable.Map[GridCell, scala.collection.mutable.ArrayBuffer[Population]]()
    
    def getGridCell(coord: Coordinate): GridCell =
      GridCell(
        ((coord.x / width) * nCellsX).toInt,
        ((coord.y / height) * nCellsY).toInt
      )
        
    // Initialize index with existing populations of same species
    populationSeq.filter(_.species == species).foreach { pop =>
      val cell = getGridCell(pop.coordinates)
      if (!gridIndex.contains(cell)) 
        gridIndex(cell) = scala.collection.mutable.ArrayBuffer[Population]()
      gridIndex(cell) += pop
    }
    
    def getImmediateNeighbors(cell: GridCell): Array[GridCell] =
      Array(
        GridCell(cell.x + 1, cell.y),
        GridCell(cell.x - 1, cell.y),
        GridCell(cell.x, cell.y + 1),
        GridCell(cell.x, cell.y - 1),
        GridCell(cell.x + 1, cell.y + 1),
        GridCell(cell.x - 1, cell.y - 1)
      ).filter(c => 
        c.x >= 0 && c.x < nCellsX && 
        c.y >= 0 && c.y < nCellsY
      )
    
    def quickValidityCheck(coord: Coordinate): Boolean =
      if (!landscapeBoundary.contains(GeometryFactory().createPoint(coord))) 
        false
      else
        val cell = getGridCell(coord)
        if (cell.x < 0 || cell.x >= nCellsX || cell.y < 0 || cell.y >= nCellsY)
          false
        else {
          var valid = true
          gridIndex.get(cell).foreach { pops =>
            pops.exists { pop =>
              if (euclidianDistance(coord, pop.coordinates) <= 1.9 * homeRange)
                valid = false
              !valid
            }
          }
          
          if (valid)
            getImmediateNeighbors(cell).exists { neighbor =>
              gridIndex.get(neighbor).exists { pops =>
                pops.exists { pop =>
                  if (euclidianDistance(coord, pop.coordinates) <= 1.9 * homeRange)
                    valid = false
                  !valid
                }
              }
            }
          valid
        }
    
    def generateInitialReference(): Coordinate = 
      populationSeq.filter(_.species == species).headOption.map(_.coordinates).getOrElse {
        // Generate initial point in normalized space that's within the landscape
        @annotation.tailrec
        def findValidStartPoint(): Coordinate = {
          val x = rnd.nextDouble() * width
          val y = rnd.nextDouble() * height
          val point = new Coordinate(x, y, 0.0)
          if (landscapeBoundary.contains(GeometryFactory().createPoint(point))) point
          else findValidStartPoint()
        }
        findValidStartPoint()
      }
    
    def generateCandidate(referenceCoord: Coordinate, currentLambda: Double): Coordinate =
      val angle = rnd.nextDouble() * 2 * math.Pi
      val distance = exponentialDistribution(currentLambda, rnd) + 1.9 * homeRange
      
      @annotation.tailrec
      def attemptPosition(attempts: Int = 0): Coordinate = {
        if (attempts >= 10) null  // Give up after too many attempts
        else {
          val newX = referenceCoord.x + distance * math.cos(angle)
          val newY = referenceCoord.y + distance * math.sin(angle)
          
          // Handle periodic boundary conditions
          val wrappedX = if (newX < 0) newX + width else if (newX > width) newX - width else newX
          val wrappedY = if (newY < 0) newY + height else if (newY > height) newY - height else newY
          
          val candidate = new Coordinate(wrappedX, wrappedY, 0.0)
          if (landscapeBoundary.contains(GeometryFactory().createPoint(candidate))) candidate
          else attemptPosition(attempts + 1)
        }
      }
      
      attemptPosition()
    
    def findValidLocation(maxAttempts: Int = 50): Coordinate =
      var attempts = 0
      var location: Coordinate = null
      var currentLambda = lambda
      val initialReference = generateInitialReference()
      
      while (attempts < maxAttempts && location == null) {
        val candidate = generateCandidate(initialReference, currentLambda)
        if (candidate != null && quickValidityCheck(candidate))
          location = candidate
        else {
          attempts += 1
          if (attempts % 10 == 0 && currentLambda > lambda * 0.1)
            currentLambda *= 0.8  // Gradually reduce lambda to search wider
        }
      }
      
      if (location == null) {
        // Final fallback: find any valid point
        @annotation.tailrec
        def findValidPoint(): Coordinate = {
          val x = rnd.nextDouble() * width
          val y = rnd.nextDouble() * height
          val point = new Coordinate(x, y, 0.0)
          if (landscapeBoundary.contains(GeometryFactory().createPoint(point))) point
          else findValidPoint()
        }
        findValidPoint()
      } else location
      
    val finalLocation = findValidLocation()
    // Update spatial index
    val cell = getGridCell(finalLocation)
    if (!gridIndex.contains(cell))
      gridIndex(cell) = scala.collection.mutable.ArrayBuffer[Population]()
      
    finalLocation

  // def fastDiskPacking(
  //   lambda: Double, 
  //   populationSeq: Seq[Population], 
  //   rnd: Random, 
  //   species: Species, 
  //   landscapeBoundary: Geometry
  // ): Coordinate =
  //   val homeRange = species.homeRange
  //   val cellSize = 1.8 * homeRange
  //   val lambdaExpansionFactor = 0.8  // Factor to progressively reduce lambda
  //   val minLambda = lambda * 0.1     // Don't let lambda get too small
    
  //   val gridIndex = scala.collection.mutable.Map[GridCell, scala.collection.mutable.ArrayBuffer[Population]]()
    
  //   def getGridCell(coord: Coordinate): GridCell =
  //     GridCell(
  //       (coord.x / cellSize).toInt,
  //       (coord.y / cellSize).toInt
  //     )
      
  //   // Initialize index with existing populations of same species
  //   populationSeq.filter(_.species == species).foreach { pop =>
  //     val cell = getGridCell(pop.coordinates)
  //     if (!gridIndex.contains(cell)) 
  //       gridIndex(cell) = scala.collection.mutable.ArrayBuffer[Population]()
  //     gridIndex(cell) += pop
  //   }
    
  //   def getImmediateNeighbors(cell: GridCell): Array[GridCell] =
  //     Array(
  //       GridCell(cell.x + 1, cell.y),
  //       GridCell(cell.x - 1, cell.y),
  //       GridCell(cell.x, cell.y + 1),
  //       GridCell(cell.x, cell.y - 1)
  //     )
    
  //   def quickValidityCheck(coord: Coordinate): Boolean =
  //     if (!landscapeBoundary.contains(GeometryFactory().createPoint(coord))) 
  //       false
  //     else
  //       val cell = getGridCell(coord)
  //       var valid = true
        
  //       gridIndex.get(cell).foreach { pops =>
  //         pops.exists { pop =>
  //           if (euclidianDistance(coord, pop.coordinates) <= 1.9 * homeRange)
  //             valid = false
  //           !valid
  //         }
  //       }
            
  //       if (valid)
  //         getImmediateNeighbors(cell).exists { neighbor =>
  //           gridIndex.get(neighbor).exists { pops =>
  //             pops.exists { pop =>
  //               if (euclidianDistance(coord, pop.coordinates) <= 1.9 * homeRange)
  //                 valid = false
  //               !valid
  //             }
  //           }
  //         }
  //       valid
    
  //   def generateCandidate(referenceCoord: Coordinate, currentLambda: Double): Coordinate =
  //     val angle = rnd.nextDouble() * 2 * math.Pi
  //     val distance = exponentialDistribution(currentLambda, rnd) + 1.9 * homeRange
  //     new Coordinate(
  //       referenceCoord.x + distance * math.cos(angle),
  //       referenceCoord.y + distance * math.sin(angle),
  //       0.0
  //     )

    
  //   def findValidLocation(maxAttempts: Int = 50): Coordinate =
  //     var attempts = 0
  //     var location: Coordinate = null
  //     var currentLambda = lambda
      
  //     // Get initial reference coordinate from existing populations or landscape center
  //     val initialReference = 
  //       populationSeq.filter(_.species == species).headOption.map(_.coordinates).getOrElse {
  //         val env = landscapeBoundary.getEnvelopeInternal
  //         new Coordinate((env.getMinX + env.getMaxX) / 2, (env.getMinY + env.getMaxY) / 2)
  //       }
      
  //     while (attempts < maxAttempts && location == null)
  //       // Generate candidate based on current lambda
  //       val candidate = generateCandidate(initialReference, currentLambda)

  //       if (quickValidityCheck(candidate))
  //         location = candidate
  //       else
  //         attempts += 1
  //         // Reduce lambda every 10 attempts, but not below minLambda
  //         if (attempts % 10 == 0 && currentLambda > minLambda)
  //           currentLambda *= lambdaExpansionFactor
      
  //     if (location == null)
  //       // Final fallback: try one last time with minimum lambda
  //       var fallbackLocation: Coordinate = null
  //       var fallbackAttempts = 0
  //       while (fallbackLocation == null && fallbackAttempts < 10)
  //         val candidate = generateCandidate(initialReference, minLambda)

  //         if (landscapeBoundary.contains(GeometryFactory().createPoint(candidate)))
  //           fallbackLocation = candidate
  //         fallbackAttempts += 1
        
  //       // If all else fails, return a point that's at least within the boundary
  //       if (fallbackLocation == null)
  //         val env = landscapeBoundary.getEnvelopeInternal
  //         new Coordinate(
  //           env.getMinX + rnd.nextDouble() * env.getWidth,
  //           env.getMinY + rnd.nextDouble() * env.getHeight
  //         )
  //       else
  //         fallbackLocation
  //     else
  //       location
    
  //   val finalLocation = findValidLocation()
  //   // Update index
  //   val cell = getGridCell(finalLocation)
  //   if (!gridIndex.contains(cell))
  //     gridIndex(cell) = scala.collection.mutable.ArrayBuffer[Population]()
    
  //   finalLocation

  

  def randomPoint(rnd: Random): Coordinate =
    val x = rnd.nextDouble()
    val y = rnd.nextDouble()
    Coordinate(x, y,0.0)

  private def exponentialDistribution(lambda: Double, rnd: Random): Double =
    math.log(1 - rnd.nextDouble()) / (-lambda)

  @tailrec    
  private def drawPopulationCoordinates(
    referenceCoordinate: Coordinate,
    lambda: Double,
    rnd: Random,
    homeRange: Double,
    landscapeBoundary: Geometry
  ): Coordinate =
    // First pick an angle with uniform probability
    val angle = rnd.nextDouble()*math.Pi*2.0
    // Now pick a distance from an exponential distribution and starting at twice the species' home range
    val distance =  exponentialDistribution(lambda, rnd) + 2*homeRange

    val x = referenceCoordinate.x + distance*math.cos(angle)
    val y = referenceCoordinate.y + distance*math.sin(angle)

    val coordinate = Coordinate(x,y)    
    if landscapeBoundary.contains(GeometryFactory().createPoint(coordinate)) then 
      coordinate 
    else
      drawPopulationCoordinates(referenceCoordinate,lambda,rnd,homeRange,landscapeBoundary)

  private def euclidianDistance(c1: Coordinate, c2: Coordinate): Double =
    val point1 = GeometryFactory().createPoint(c1)
    val point2 = GeometryFactory().createPoint(c2)
    DistanceOp(point1, point2).distance()

  def diskPacking(lambda: Double, populationSeq: Seq[Population], rnd: Random, species: Species, landscapeBoundary: Geometry): Coordinate =

    val speciesHomeRange = species.homeRange

    def getReferenceCoordinate(populationSeq: Seq[Population], rnd: Random): Coordinate =
      if populationSeq.isEmpty then randomPoint(rnd) else rnd.shuffle(populationSeq).head.coordinates

    @tailrec
    def rec(i: Int, lambdaRec: Double): Coordinate =

      // Pick a population at random and get its location
      val referenceCoordinate: Coordinate = getReferenceCoordinate(populationSeq, rnd)

      val newCoordinate = drawPopulationCoordinates(referenceCoordinate,lambdaRec,rnd,speciesHomeRange,landscapeBoundary)

      // Now check that this population does not overlap with another one.
      val noOverlap: Boolean = populationSeq.forall{
        pop => euclidianDistance(newCoordinate,pop.coordinates)>2*speciesHomeRange
      }

      if noOverlap then {
        newCoordinate
      } else{
        if i>1000 then{
          newCoordinate
        } else {
          rec(i+1, lambdaRec)
        }
      }

    rec(0, lambda)

  def getVoronoiTesselation(numberOfAreas: Int, rnd: Random): Geometry =
    val sites: java.util.Collection[Coordinate] = (0 until numberOfAreas).map {
      n =>
        Utils.randomPoint(rnd)
    }.asJavaCollection

    val vDB: VoronoiDiagramBuilder = VoronoiDiagramBuilder()
    vDB.setSites(sites)
    vDB.setClipEnvelope(Envelope(0.0, 1.0, 0.0, 1.0))
    vDB.getDiagram(GeometryFactory())


  def chooseStochasticEvent(probabilityMap: Map[Int,Double],rnd: Random): Int =
    
    val rankedProbabilities: Map[Int,Double] = probabilityMap.toSeq.sortWith(_._2 > _._2).toMap

    val cummulativeProbability: Map[Int,Double] = 
      probabilityMap.scanLeft((0,0.0)){
        case ((_,preProb),(tile,prob)) => (tile, preProb + prob)
      }.toMap

    val totalProb: Double = cummulativeProbability.last._2 

    val normalizedCumProbability: Map[Int, Double] = cummulativeProbability.map(p => (p._1, p._2/totalProb))

    val dice = rnd.nextDouble()
    
    normalizedCumProbability.find(p => p._2 > dice).getOrElse(-1,0.0)._1

end Utils
