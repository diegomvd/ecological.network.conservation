package econetcons

import org.locationtech.jts.geom.Coordinate
import scala.annotation.tailrec
import scala.util.Random


object Utils:

  /**
   * Generate Thomas process candidates with periodic boundaries
   * No landscape boundary parameter needed - always [0,1]²!
   */
  def generateThomasProcessCandidates(
    numCandidates: Int,
    numCenters: Int,
    clusterRadius: Double,
    rnd: Random
  ): Seq[Coordinate] = {
    
    val candidatesPerCenter = numCandidates / numCenters
    
    // Generate cluster centers - trivial with periodic boundaries!
    val clusterCenters = (0 until numCenters).map(_ => SquareGrid.randomPoint(rnd))
    
    // Generate candidates around each center
    clusterCenters.flatMap { center =>
      (0 until candidatesPerCenter).map(_ => 
        generateThomasCandidate(center, clusterRadius, rnd)
      )
    }
  }

  /**
   * Generate Thomas process candidate with automatic wrapping
   * No rejection sampling needed!
   */
  private def generateThomasCandidate(
    center: Coordinate,
    clusterRadius: Double,
    rnd: Random
  ): Coordinate = {
    
    val angle = rnd.nextDouble() * 2 * math.Pi
    val distance = exponentialDistribution(1.0 / clusterRadius, rnd)
    
    val x = center.x + distance * math.cos(angle)
    val y = center.y + distance * math.sin(angle)
    
    val candidate = new Coordinate(x, y, 0.0)
    
    // Wrap back to [0,1]² - no rejection needed!
    SquareGrid.wrapCoordinate(candidate)
  }

  /**
   * Random point in [0,1]² - 100% success rate
   */
  def randomPoint(rnd: Random): Coordinate = SquareGrid.randomPoint(rnd)

  private def exponentialDistribution(lambda: Double, rnd: Random): Double =
    math.log(1 - rnd.nextDouble()) / (-lambda)

  @tailrec
  def adaptiveKRandomThinning(
    candidates: Seq[Coordinate], 
    targetCount: Int, 
    minDistance: Double, 
    checkFraction: Double,
    rnd: Random,
    relaxationFactor: Double = 1.0,
    currentlySelected: Seq[Coordinate] = Seq.empty
  ): Seq[Coordinate] = {
    
    if (currentlySelected.size >= targetCount || candidates.isEmpty || relaxationFactor < 0.5) {
      currentlySelected
    } else {
      val newlySelected = singlePassThinning(
        candidates, 
        targetCount - currentlySelected.size, 
        minDistance * relaxationFactor, 
        checkFraction, 
        currentlySelected,
        rnd
      )
      
      val remainingCandidates = candidates.filterNot(newlySelected.contains)
      
      adaptiveKRandomThinning(
        remainingCandidates,
        targetCount,
        minDistance,
        checkFraction,
        rnd,
        relaxationFactor - 0.1,
        currentlySelected ++ newlySelected
      )
    }
  }

  @tailrec
  private def singlePassThinning(
    candidates: Seq[Coordinate],
    targetCount: Int,
    currentMinDistance: Double,
    checkFraction: Double,
    alreadySelected: Seq[Coordinate],
    rnd: Random,
    selected: Seq[Coordinate] = Seq.empty
  ): Seq[Coordinate] = {
      
    if (selected.size >= targetCount || candidates.isEmpty) {
      selected
    } else {
      val candidate = candidates.head
      val k = math.max(1, (targetCount * checkFraction).toInt)
      val allSelected = alreadySelected ++ selected
      val randomSubset = if (allSelected.size <= k) allSelected else rnd.shuffle(allSelected).take(k)
      val minDistanceSquared = currentMinDistance * currentMinDistance
      
      // Use periodic distance for correct spacing
      val tooClose = randomSubset.exists { existing =>
        val distance = SquareGrid.periodicDistance(candidate, existing)
        distance * distance < minDistanceSquared
      }
      
      if (tooClose) {
        singlePassThinning(candidates.tail, targetCount, currentMinDistance, checkFraction, alreadySelected, rnd, selected)
      } else {
        singlePassThinning(candidates.tail, targetCount, currentMinDistance, checkFraction, alreadySelected, rnd, selected :+ candidate)
      }
    }
  }

  def chooseStochasticEvent(probabilityMap: Map[Int,Double], rnd: Random): Int = {
    val cummulativeProbability: Map[Int,Double] = 
      probabilityMap.scanLeft((0,0.0)){
        case ((_,preProb),(tile,prob)) => (tile, preProb + prob)
      }.toMap

    val totalProb: Double = cummulativeProbability.values.max

    val normalizedCumProbability: Map[Int, Double] = cummulativeProbability.map(p => (p._1, p._2/totalProb))

    val dice = rnd.nextDouble()
    
    normalizedCumProbability.find(p => p._2 > dice).getOrElse(-1,0.0)._1
  }

end Utils
