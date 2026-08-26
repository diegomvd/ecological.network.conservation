package econetcons

import org.locationtech.jts.geom.Coordinate
import scala.annotation.tailrec
import scala.util.Random


object Utils:

  /**
   * DEPRECATED — legacy single/multi-center Thomas process with periodic boundaries, kept
   * only for calibration comparison against generateBranchingWalk. Points radiate from
   * uniformly random cluster centers. No landscape boundary parameter needed — always [0,1]².
   */
  def generateThomasProcessCandidates(
    numCandidates: Int,
    numCenters: Int,
    clusterRadius: Double,
    rnd: Random
  ): Seq[Coordinate] = {

    val candidatesPerCenter = numCandidates / numCenters

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

  /**
   * Eden-growth-style branching random walk.
   *
   * Each new individual is placed a FIXED `step` away (random angle) from a *randomly chosen*
   * individual already placed — not a fixed center, not the most recent one — producing
   * organic, irregular clusters. Strictly O(N), single-pass, no pairwise distance checks.
   * Cluster extent grows ~ step·sqrt(N) as an emergent property of the walk.
   *
   * @param n       number of points to generate (>= 1)
   * @param anchor  coordinate of the first individual (a uniform random point)
   * @param step    fixed parent->child step length (see WorldParameters.placementStep)
   */
  def generateBranchingWalk(
    n: Int,
    anchor: Coordinate,
    step: Double,
    rnd: Random
  ): Seq[Coordinate] = {
    val points = scala.collection.mutable.ArrayBuffer[Coordinate](anchor)
    while (points.size < n) {
      val parent = points(rnd.nextInt(points.size))
      val angle = rnd.nextDouble() * 2 * math.Pi
      val x = parent.x + step * math.cos(angle)
      val y = parent.y + step * math.sin(angle)
      points += SquareGrid.wrapCoordinate(new Coordinate(x, y, 0.0))
    }
    points.toSeq
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
