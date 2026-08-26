package econetcons

import org.locationtech.jts.geom.Coordinate
import scala.annotation.tailrec
import scala.util.Random


object Utils:

  /**
   * Generate Thomas process candidates with periodic boundaries.
   * No landscape boundary parameter needed - always [0,1]²!
   *
   * @param hotspotCenters  Pre-generated landscape-level hotspot centers (empty = random landscape)
   * @param aggregationStrength  Probability [0,1] that a cluster center is drawn near a hotspot center
   * @param hotspotVariance  Gaussian spread around the chosen hotspot center
   */
  def generateThomasProcessCandidates(
    numCandidates: Int,
    numCenters: Int,
    clusterRadius: Double,
    rnd: Random,
    hotspotCenters: Seq[Coordinate] = Seq.empty,
    aggregationStrength: Double = 0.0,
    hotspotVariance: Double = 0.1
  ): Seq[Coordinate] = {

    val candidatesPerCenter = numCandidates / numCenters

    val clusterCenters = (0 until numCenters).map { _ =>
      if hotspotCenters.nonEmpty && rnd.nextDouble() < aggregationStrength then
        val h = hotspotCenters(rnd.nextInt(hotspotCenters.size))
        sampleGaussian2D(h.x, h.y, hotspotVariance, rnd)
      else
        SquareGrid.randomPoint(rnd)
    }

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

  /**
   * Sample a point from 2D Gaussian distribution with periodic boundary wrapping
   * Uses Box-Muller transform for Gaussian sampling
   */
  def sampleGaussian2D(
    centerX: Double,
    centerY: Double,
    variance: Double,
    rnd: Random
  ): Coordinate = {
    // Box-Muller transform for 2D Gaussian
    val u1 = rnd.nextDouble()
    val u2 = rnd.nextDouble()
    val stdDev = math.sqrt(variance)

    val x = centerX + stdDev * math.sqrt(-2 * math.log(u1)) * math.cos(2 * math.Pi * u2)
    val y = centerY + stdDev * math.sqrt(-2 * math.log(u1)) * math.sin(2 * math.Pi * u2)

    // Wrap to [0,1]² with periodic boundaries
    SquareGrid.wrapCoordinate(new Coordinate(x, y, 0.0))
  }

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
