package econetcons

import java.io.{BufferedWriter, File, FileOutputStream, OutputStreamWriter}
import java.nio.charset.StandardCharsets
import java.util.concurrent.{ConcurrentLinkedQueue, Executors}
import java.util.concurrent.atomic.AtomicInteger
import scala.jdk.CollectionConverters.*
import scala.util.Random

/**
 * Compares population-allocation x cascade rules on the assembled starting community and on its
 * response to one standard disturbance (SR plan, tile 32, 20% protected). Used to choose the
 * assembly model before the paper sweeps (plan: make-a-plan-for-snug-hejlsberg.md).
 *
 * Per world: placed / in-web / assembled populations by coarse trophic group (basal, intermediate,
 * top), realized web structure (WebStructureSweep.metrics), realized populations per occupied tile
 * at the finest grain (10 nominal populations per tile), and SR's primary / secondary species loss
 * and cascade rounds after the disturbance.
 *
 * Usage: sbt "runMain econetcons.AssemblyDiagnostic [--out assembly_diagnostic.csv] [--seeds 5] [--threads 20]"
 */
object AssemblyDiagnostic:

  // (label, abundanceModel, cascadeModel, epsilon, theta, transferEfficiency)
  private val Configs = Seq(
    ("S1_eps2",            "damuth",             "metabolic",       2.0, 1.0, 0.1),
    ("legacy",             "damuth",             "legacy",          0.0, 1.0, 0.1),
    ("C1_th0.3",           "damuth",             "energyTolerance", 0.0, 0.3, 0.1),
    ("C1_th0.5",           "damuth",             "energyTolerance", 0.0, 0.5, 0.1),
    ("C1_th0.7",           "damuth",             "energyTolerance", 0.0, 0.7, 0.1),
    ("C1_th1.0",           "damuth",             "energyTolerance", 0.0, 1.0, 0.1),
    ("C1_th0.5+TE0.1",     "transferEfficiency", "energyTolerance", 0.0, 0.5, 0.1),
    ("C1_th0.5+EB_eps2",   "energyBalance",      "energyTolerance", 2.0, 0.5, 0.1),
    ("C2_EB_eps2",         "energyBalance",      "metabolic",       2.0, 1.0, 0.1),
    ("C2_EB_eps6",         "energyBalance",      "metabolic",       6.0, 1.0, 0.1),
    ("C3_TE0.05",          "transferEfficiency", "metabolic",       2.0, 1.0, 0.05),
    ("C3_TE0.1",           "transferEfficiency", "metabolic",       2.0, 1.0, 0.1),
    ("C3_TE0.2",           "transferEfficiency", "metabolic",       2.0, 1.0, 0.2),
  )
  private val CMeta = Seq(0.05, 0.20)
  private val Hr    = Seq(0.016, 0.025)

  private def parseArgs(args: Array[String]): Map[String, String] =
    args.sliding(2, 2).collect { case Array(k, v) if k.startsWith("--") => k.drop(2) -> v }.toMap

  private def groups(pops: Iterable[Population]): Seq[Double] =
    Seq(pops.size, pops.count(_.species.trophicLevel == 0), pops.count(_.species.trophicLevel == 1),
      pops.count(_.species.trophicLevel == 2)).map(_.toDouble)

  def main(args: Array[String]): Unit =
    val opts    = parseArgs(args)
    val outPath = opts.getOrElse("out", "assembly_diagnostic.csv")
    val nSeeds  = opts.get("seeds").map(_.toInt).getOrElse(5)
    val threads = opts.get("threads").map(_.toInt).getOrElse(Runtime.getRuntime.availableProcessors)
    val realErr = System.err
    System.setOut(new java.io.PrintStream((_: Int) => ()))
    System.setProperty("econetcons.skipRobustness", "true")

    val tasks = new ConcurrentLinkedQueue[(Int, Double, Double, Long)]()
    for ci <- Configs.indices; c <- CMeta; h <- Hr; s <- 1L to nSeeds.toLong do tasks.add((ci, c, h, s))
    val total = tasks.size
    realErr.println(s"[AssemblyDiagnostic] $total worlds, $threads threads -> $outPath")

    val header = Seq("config", "abundance", "cascade", "epsilon", "theta", "te", "c_meta", "hr", "seed") ++
      Seq("placed", "placed_basal", "placed_inter", "placed_top") ++
      Seq("inweb", "inweb_basal", "inweb_inter", "inweb_top") ++
      Seq("assembled", "assembled_basal", "assembled_inter", "assembled_top") ++
      WebStructureSweep.MetricNames.map("stable_" + _) ++
      Seq("tiles_occupied_finest", "pops_per_occupied_tile_finest",
        "SR_sp_ext_primary", "SR_sp_ext_secondary", "SR_sp_ext_total", "SR_nCascades",
        "IR_sp_ext_total", "IR_spint_ext_total", "SR_spint_ext_total")
    val writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(new File(outPath)), StandardCharsets.UTF_8))
    writer.write(header.mkString(",")); writer.newLine(); writer.flush()
    val lock = new AnyRef
    val done = new AtomicInteger(0)
    val failed = new AtomicInteger(0)
    val t0 = System.currentTimeMillis()

    val worker: Runnable = () =>
      var t = tasks.poll()
      while t != null do
        val (ci, c, h, s) = t
        val (label, am, cm, eps, theta, te) = Configs(ci)
        try
          def wp(tile: Int) = WorldParameters(numberOfSpecies = 300, connectanceMetaWeb = c, nPopulations = 5000,
            medianHomeRange = h, avgPopulationsPerTile = tile, epsilon = eps,
            extinctionProbabilityUnprotected = 0.5, bodySizeSigma = 1.0, meanPatchesPerSpecies = 5.0,
            abundanceModel = am, transferEfficiency = te, cascadeModel = cm, energyTolerance = theta)
          val (pops, meta) = World.buildPopsAndMetaForTest(wp(32), new Random(s))
          val raw = PopulationWebGenerator.generatePopulationWeb(pops, meta)
          val cp0 = ConservationParameters(fractionProtected = 0.0, strategy = Some(ConservationStrategy.SpeciesRichness))
          val stable = World(wp(32), cp0, new Random(s)).populationWeb
          val fine = World(wp(10), cp0, new Random(s)).managementLandscape.managementAreas.filter(_.populations.nonEmpty)
          val m = Observables.run(300, c, 5000, h, 32, 0.0, 0.2, s, strategies = Observables.paperStrategies,
            epsilon = eps, extinctionProbabilityUnprotected = 0.5, bodySizeSigma = 1.0, meanPatchesPerSpecies = 5.0,
            abundanceModel = am, transferEfficiency = te, cascadeModel = cm, energyTolerance = theta)
          val vals = groups(pops) ++ groups(raw.vertexSet().asScala) ++ groups(stable.vertexSet().asScala) ++
            WebStructureSweep.metrics(stable) ++
            Seq(fine.size.toDouble, fine.map(_.populations.size).sum.toDouble / math.max(1, fine.size)) ++
            Seq("SR_sp_ext_primary", "SR_sp_ext_secondary", "SR_sp_ext_total", "SR_nCascades",
              "IR_sp_ext_total", "IR_spint_ext_total", "SR_spint_ext_total").map(k => m.getOrElse(k, Double.NaN))
          val row = Seq(label, am, cm, f"$eps%.1f", f"$theta%.2f", f"$te%.2f", f"$c%.2f", f"$h%.3f", s.toString) ++
            vals.map(v => f"$v%.5g")
          lock.synchronized { writer.write(row.mkString(",")); writer.newLine(); writer.flush() }
        catch
          case ex: Throwable =>
            failed.incrementAndGet()
            realErr.println(s"[AssemblyDiagnostic] FAILED $label c=$c hr=$h seed=$s: ${ex.getClass.getSimpleName}: ${ex.getMessage}")
        val n = done.incrementAndGet()
        if n % 20 == 0 || n == total then
          val el = (System.currentTimeMillis() - t0) / 1000.0
          realErr.println(f"[AssemblyDiagnostic] $n%d/$total%d  ${el}%.0fs  ETA ${el / n * (total - n) / 60}%.1f min  (failed ${failed.get}%d)")
        t = tasks.poll()

    val pool = Executors.newFixedThreadPool(threads)
    (1 to threads).map(_ => pool.submit(worker)).foreach(_.get())
    pool.shutdown()
    writer.close()
    realErr.println(s"[AssemblyDiagnostic] DONE -> $outPath  (${done.get} worlds, ${failed.get} failed)")
