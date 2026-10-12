package econetcons

import scala.jdk.CollectionConverters.*
import scala.util.Random

/**
 * Throwaway: joint (nSpecies × medianHomeRange) calibration of the realized food-web degree
 * distribution, so the "web structure" axis of the next basin sweep uses empirically plausible
 * webs (~2-4 directed links/species, ~80-150 surviving species) instead of HR 0.01/0.03/0.05.
 *
 * Reports, per (nSpecies, HR) cell (world construction + stabilization only, no extinction
 * pipeline, frac=0):
 *   surviving species, realized links, mean/CV/max out-degree (predator diet breadth),
 *   realized connectance (links / S^2), fraction of species that are basal / top.
 *
 * DELETE after the calibration point(s) are chosen.
 */
object WebCalibration:
  def main(args: Array[String]): Unit =
    val realOut = System.out
    def silence[T](b: => T): T =
      System.setOut(new java.io.PrintStream((_: Int) => ()))
      try b finally System.setOut(realOut)

    val nSpeciesGrid = Seq(150, 200, 300, 400, 600, 800)
    val hrGrid       = Seq(0.008, 0.012, 0.016, 0.020, 0.025, 0.030)
    val seeds        = 1L to 8L

    realOut.println("nSpecies,hr,surv_sp,links,links_per_sp,degree_cv,degree_max,"
      + "realized_connectance,frac_basal,frac_top,pct_pool_surviving")

    for
      nS <- nSpeciesGrid
      hr <- hrGrid
    do
      val stats = seeds.map { s =>
        silence {
          val wp = WorldParameters(
            numberOfSpecies = nS, connectanceMetaWeb = 0.20, nPopulations = 5000,
            medianHomeRange = hr, avgPopulationsPerTile = 32, epsilon = 2.0,
            extinctionProbabilityUnprotected = 0.5, bodySizeSigma = 1.0,
            meanPatchesPerSpecies = 5.0,
          )
          val cp = ConservationParameters(fractionProtected = 0.0,
            strategy = Some(ConservationStrategy.SpeciesRichness))
          val w = World(wp, cp, new Random(s))
          val (stable, _) = w.phase2Cascades()
          val g = stable.getRealizedWeb
          val verts = g.vertexSet().asScala.toVector
          val nSurv = verts.size
          val links = g.edgeSet().size()
          // out-degree = number of prey species (diet breadth); in-degree = number of predators
          val outDeg = verts.map(v => g.outDegreeOf(v).toDouble)
          val inDeg  = verts.map(v => g.inDegreeOf(v).toDouble)
          val meanDeg = if nSurv > 0 then links.toDouble / nSurv else 0.0
          val allDeg = verts.map(v => (g.outDegreeOf(v) + g.inDegreeOf(v)).toDouble)
          val mu = if allDeg.nonEmpty then allDeg.sum / allDeg.size else 0.0
          val sd = if allDeg.size > 1 then
            math.sqrt(allDeg.map(d => (d - mu) * (d - mu)).sum / (allDeg.size - 1)) else 0.0
          val cv = if mu > 0 then sd / mu else 0.0
          val degMax = if allDeg.nonEmpty then allDeg.max else 0.0
          val conn = if nSurv > 0 then links.toDouble / (nSurv.toDouble * nSurv) else 0.0
          val fracBasal = if nSurv > 0 then outDeg.count(_ == 0.0).toDouble / nSurv else 0.0
          val fracTop   = if nSurv > 0 then inDeg.count(_ == 0.0).toDouble / nSurv else 0.0
          (nSurv.toDouble, links.toDouble, meanDeg, cv, degMax, conn, fracBasal, fracTop, nSurv.toDouble / nS)
        }
      }
      def avg(f: ((Double,Double,Double,Double,Double,Double,Double,Double,Double)) => Double) =
        stats.map(f).sum / stats.size
      realOut.println(f"$nS,$hr%.3f,${avg(_._1)}%.0f,${avg(_._2)}%.0f,${avg(_._3)}%.2f,"
        + f"${avg(_._4)}%.2f,${avg(_._5)}%.0f,${avg(_._6)}%.4f,${avg(_._7)}%.2f,"
        + f"${avg(_._8)}%.2f,${avg(_._9)}%.2f")
