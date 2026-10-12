package econetcons

import org.locationtech.jts.geom.Coordinate
import scala.annotation.tailrec
import scala.util.Random


object Utils:

  /**
   * Random point in [0,1]² - 100% success rate
   */
  def randomPoint(rnd: Random): Coordinate = SquareGrid.randomPoint(rnd)

  /**
   * Matérn cluster process for one species, with periodic ([0,1]²) boundaries.
   *
   * Parents (patch centers): the number of patches is drawn Poisson(meanPatches), floored at
   * 1 so no species vanishes, and each parent is placed uniformly at random on the torus.
   *
   * Offspring: the species' `n` populations are thrown into the patches by a uniform
   * multinomial (each population picks a patch uniformly at random), so patch counts k_i vary.
   *
   * Radius: each patch is a disk sized to hold its own k_i individuals at a home-range-set
   * density — R_i = homeRange · √(k_i) · packingFactor. Bigger patches for patches that drew
   * more individuals; abundant species end up blanketing the landscape while rare species stay
   * patchy, an emergent ecological range-size gradient. `packingFactor` tunes tightness
   * (small = dense/territorial, large = loose/landscape-filling).
   *
   * Within a patch: uniform-in-disk placement (even fill, hard edge), then wrapped to the torus.
   *
   * @param n            number of populations to place (>= 1)
   * @param homeRange    species home range (sets the packing density scale)
   * @param meanPatches  Poisson mean number of patches for this species (λ)
   * @param packingFactor dimensionless radius multiplier
   */
  def generateMaternProcess(
    n: Int,
    homeRange: Double,
    meanPatches: Double,
    packingFactor: Double,
    rnd: Random
  ): Seq[Coordinate] = {
    if n <= 0 then Seq.empty
    else {
      val numPatches = math.max(1, poissonSample(meanPatches, rnd))
      val centers = Array.fill(numPatches)(SquareGrid.randomPoint(rnd))

      // Multinomial: assign each population to a patch uniformly at random.
      val counts = Array.fill(numPatches)(0)
      var i = 0
      while (i < n) { counts(rnd.nextInt(numPatches)) += 1; i += 1 }

      val out = scala.collection.mutable.ArrayBuffer.empty[Coordinate]
      var p = 0
      while (p < numPatches) {
        val k = counts(p)
        if (k > 0) {
          // Radius sized to hold k individuals at the home-range density.
          val radius = homeRange * math.sqrt(k.toDouble) * packingFactor
          val center = centers(p)
          var j = 0
          while (j < k) {
            out += uniformInDisk(center, radius, rnd)
            j += 1
          }
        }
        p += 1
      }
      out.toSeq
    }
  }

  /** Uniform point inside a disk of given radius around center, wrapped to the torus. */
  private def uniformInDisk(center: Coordinate, radius: Double, rnd: Random): Coordinate = {
    val angle = rnd.nextDouble() * 2 * math.Pi
    // sqrt(u) gives area-uniform (not radius-uniform) sampling inside the disk.
    val r = radius * math.sqrt(rnd.nextDouble())
    val x = center.x + r * math.cos(angle)
    val y = center.y + r * math.sin(angle)
    SquareGrid.wrapCoordinate(new Coordinate(x, y, 0.0))
  }

  /** Knuth's Poisson sampler; adequate for the small means used here. */
  private def poissonSample(lambda: Double, rnd: Random): Int = {
    if lambda <= 0.0 then 0
    else {
      val l = math.exp(-lambda)
      var k = 0
      var prod = rnd.nextDouble()
      while (prod > l) { k += 1; prod *= rnd.nextDouble() }
      k
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
