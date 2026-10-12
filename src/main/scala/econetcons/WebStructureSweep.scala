package econetcons

import java.io.{BufferedWriter, File, FileOutputStream, OutputStreamWriter}
import java.nio.charset.StandardCharsets
import java.util.concurrent.{ConcurrentLinkedQueue, Executors}
import java.util.concurrent.atomic.AtomicInteger
import scala.jdk.CollectionConverters.*
import scala.util.Random
import org.jgrapht.graph.{DefaultDirectedGraph, DefaultEdge}

/**
 * Realized food-web structure BEFORE any protection or extinction: world assembly + energetic
 * stabilization only. Explores how nSpecies, epsilon, medianHomeRange and metaweb connectance
 * shape the realized web, to design a "realized connectance" knob for the SR-vs-IR comparison.
 *
 * Per world (one seed) it reports, for the RAW web (straight from the spatial interaction rule)
 * and the STABLE web (after World.apply's energetic cascade):
 *   species level : S, L, L/S, connectance L/S^2, degree CV, basal/top fractions,
 *                   mean diet breadth (prey species per consumer species),
 *                   fraction of consumer species with a single prey species
 *   population    : nodes, edges, mean prey nodes per consumer node,
 *                   mean predator nodes per prey node (resource sharing)
 *
 * Usage: sbt "runMain econetcons.WebStructureSweep [--out web_structure_sweep.csv] [--seeds 5] [--threads 20]"
 */
object WebStructureSweep:

  private val NSpecies = Seq(150, 200, 300, 450, 600)
  private val Eps      = Seq(2.0, 3.0, 4.0, 6.0, 8.0, 10.0)
  private val Hr       = Seq(0.010, 0.013, 0.016, 0.020, 0.025, 0.030)
  private val CMeta    = Seq(0.05, 0.10, 0.20, 0.30)

  private[econetcons] type PWeb = DefaultDirectedGraph[Population, DefaultEdge]

  /** Structure metrics of a population web (species level derived from its edges). */
  private[econetcons] def metrics(web: PWeb): Seq[Double] =
    val nodes = web.vertexSet().asScala.toVector
    val nNodes = nodes.size
    val nEdges = web.edgeSet().size()
    // species-level links from population edges (predator -> prey)
    val links = web.edgeSet().asScala.iterator
      .map(e => (web.getEdgeSource(e).species.id, web.getEdgeTarget(e).species.id)).toSet
    val species = nodes.map(_.species).distinctBy(_.id)
    val sIds = species.map(_.id).toSet
    val s = sIds.size.toDouble
    val l = links.size.toDouble
    val outDeg = links.groupBy(_._1).view.mapValues(_.size).toMap   // diet breadth
    val inDeg  = links.groupBy(_._2).view.mapValues(_.size).toMap
    val deg = sIds.toVector.map(i => (outDeg.getOrElse(i, 0) + inDeg.getOrElse(i, 0)).toDouble)
    val mu = if deg.nonEmpty then deg.sum / deg.size else 0.0
    val sd = if deg.size > 1 then math.sqrt(deg.map(d => (d - mu) * (d - mu)).sum / (deg.size - 1)) else 0.0
    val consumers = sIds.filter(i => outDeg.getOrElse(i, 0) > 0)
    val basal = sIds.count(i => outDeg.getOrElse(i, 0) == 0)
    val top = sIds.count(i => inDeg.getOrElse(i, 0) == 0)
    val dietMean = if consumers.nonEmpty then consumers.toVector.map(outDeg(_).toDouble).sum / consumers.size else 0.0
    val singlePrey = if consumers.nonEmpty then consumers.count(outDeg(_) == 1).toDouble / consumers.size else 0.0
    // population level
    val consNodes = nodes.filter(p => web.outDegreeOf(p) > 0)
    val preyNodes = nodes.filter(p => web.inDegreeOf(p) > 0)
    val preyPerCons = if consNodes.nonEmpty then consNodes.map(p => web.outDegreeOf(p).toDouble).sum / consNodes.size else 0.0
    val predPerPrey = if preyNodes.nonEmpty then preyNodes.map(p => web.inDegreeOf(p).toDouble).sum / preyNodes.size else 0.0
    Seq(s, l, if s > 0 then l / s else 0.0, if s > 0 then l / (s * s) else 0.0,
        if mu > 0 then sd / mu else 0.0,
        if s > 0 then basal / s else 0.0, if s > 0 then top / s else 0.0,
        dietMean, singlePrey,
        nNodes.toDouble, nEdges.toDouble, preyPerCons, predPerPrey)

  private[econetcons] val MetricNames = Seq("S", "L", "L_per_S", "connectance", "degree_cv", "frac_basal", "frac_top",
    "diet_breadth", "frac_single_prey", "pop_nodes", "pop_edges", "prey_per_consumer_node", "pred_per_prey_node")

  private def parseArgs(args: Array[String]): Map[String, String] =
    args.sliding(2, 2).collect { case Array(k, v) if k.startsWith("--") => k.drop(2) -> v }.toMap

  def main(args: Array[String]): Unit =
    val opts    = parseArgs(args)
    val outPath = opts.getOrElse("out", "web_structure_sweep.csv")
    val nSeeds  = opts.get("seeds").map(_.toInt).getOrElse(5)
    val threads = opts.get("threads").map(_.toInt).getOrElse(Runtime.getRuntime.availableProcessors)
    val realErr = System.err
    System.setOut(new java.io.PrintStream((_: Int) => ()))   // silence model printlns

    val tasks = new ConcurrentLinkedQueue[(Int, Double, Double, Double, Long)]()
    for nS <- NSpecies; e <- Eps; h <- Hr; c <- CMeta; s <- 1L to nSeeds.toLong do tasks.add((nS, e, h, c, s))
    val total = tasks.size
    realErr.println(s"[WebStructureSweep] $total worlds, $threads threads -> $outPath")

    val writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(new File(outPath)), StandardCharsets.UTF_8))
    val header = Seq("nSpecies", "epsilon", "hr", "c_meta", "seed", "meta_links") ++
      MetricNames.map("raw_" + _) ++ MetricNames.map("stable_" + _)
    writer.write(header.mkString(",")); writer.newLine(); writer.flush()
    val lock = new AnyRef
    val done = new AtomicInteger(0)
    val failed = new AtomicInteger(0)
    val t0 = System.currentTimeMillis()

    val worker: Runnable = () =>
      var t = tasks.poll()
      while t != null do
        val (nS, e, h, c, s) = t
        try
          val wp = WorldParameters(numberOfSpecies = nS, connectanceMetaWeb = c, nPopulations = 5000,
            medianHomeRange = h, avgPopulationsPerTile = 32, epsilon = e,
            extinctionProbabilityUnprotected = 0.5, bodySizeSigma = 1.0, meanPatchesPerSpecies = 5.0)
          // raw web: same RNG sequence as World.apply up to web generation (metaweb, then populations)
          val (pops, meta) = World.buildPopsAndMetaForTest(wp, new Random(s))
          val raw = PopulationWebGenerator.generatePopulationWeb(pops, meta)
          val cp = ConservationParameters(fractionProtected = 0.0, strategy = Some(ConservationStrategy.SpeciesRichness))
          val stable = World(wp, cp, new Random(s)).populationWeb
          val row = Seq(nS.toString, f"$e%.1f", f"$h%.3f", f"$c%.2f", s.toString, meta.edgeSet().size().toString) ++
            (metrics(raw) ++ metrics(stable)).map(v => f"$v%.5g")
          lock.synchronized { writer.write(row.mkString(",")); writer.newLine(); writer.flush() }
        catch
          case ex: Throwable =>
            failed.incrementAndGet()
            realErr.println(s"[WebStructureSweep] FAILED nS=$nS eps=$e hr=$h c=$c seed=$s: ${ex.getClass.getSimpleName}: ${ex.getMessage}")
        val n = done.incrementAndGet()
        if n % 200 == 0 || n == total then
          val el = (System.currentTimeMillis() - t0) / 1000.0
          realErr.println(f"[WebStructureSweep] $n%d/$total%d  ${el}%.0fs  ETA ${el / n * (total - n) / 60}%.1f min  (failed ${failed.get}%d)")
        t = tasks.poll()

    val pool = Executors.newFixedThreadPool(threads)
    (1 to threads).map(_ => pool.submit(worker)).foreach(_.get())
    pool.shutdown()
    writer.close()
    realErr.println(s"[WebStructureSweep] DONE -> $outPath  (${done.get} worlds, ${failed.get} failed)")
