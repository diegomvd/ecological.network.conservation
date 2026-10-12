package econetcons

import java.io.{File, FileOutputStream, OutputStreamWriter, BufferedWriter}
import java.nio.charset.StandardCharsets
import java.util.concurrent.{Executors, ConcurrentLinkedQueue}
import java.util.concurrent.atomic.AtomicInteger

/**
 * Round-2 basin sweep: SR vs IR vs EIR across a 3-D spatial-scale grid.
 *
 *   axis 1  avgPopulationsPerTile  — LOG-spaced steps, 10 .. 200  (decision grain;
 *                                    nTiles = nPopulations / tile, so this spans 500 .. 25 units)
 *   axis 2  fractionProtected      — LINEAR steps, 0.05 .. 0.40   (protection budget)
 *   axis 3  medianHomeRange        — 3 levels: 0.016 / 0.020 / 0.025  (web structure: sparse / medium / dense,
 *                                    calibrated at nSpecies=300, eps=2 to ~1.6 / 2.4 / 3.6 links per species)
 *
 * Robustness metrics are DISABLED (-Decn.skipRobustness) — 2 network shocks × 3 strategies were
 * the dominant cost at large home range, and the dRobust_* contrasts were unidentifiable in
 * round 1. robust_* / dRobust_* columns are therefore NOT emitted.
 *
 * FIXED (screened out or deliberately pinned in round 1):
 *   nPopulations = 5000, connectanceMetaWeb = 0.20, epsilon = Epsilon, pExt = PExt,
 *   numberOfSpecies = 300, meanPatchesPerSpecies = 5.0, bodySizeSigma = 1.0, preyPerPredator = 0.0.
 *
 * Default 100 seeds per cell. One Observables.run evaluates SR/IR/EIR on the SAME base world,
 * so one run == one (cell, seed) sample for the whole contrast.
 * Default total: 30*30*3*100 = 810,000 runs.
 *
 * Output: one CSV, one row per (tile, frac, hr, seed). Raw per-strategy anchors + the paired
 * Δ contrasts are all emitted so any aggregation / bootstrap / basin classification is done
 * offline. RESUMABLE: on restart, (tileIdx, fracIdx, hrIdx, seed) tuples already in the file
 * are skipped. Rows are flushed as produced, so a killed run leaves a valid partial file.
 *
 * Usage:
 *   sbt "runMain econetcons.BasinSweep [--out basin_sweep.csv] [--seeds 100] [--threads N]
 *                                      [--tile-steps 30] [--frac-steps 30]"
 */
object BasinSweep:

  // ---- fixed factors --------------------------------------------------------
  private val NPop            = 5000
  private val C               = 0.20
  private val Epsilon         = 2.0     // S1 supply (eps * m_prey); cascade response is on a plateau for eps >= 2 (EpsilonProbe 2026-10-08)
  private val PExt            = 0.5
  private val NumberOfSpecies = 300
  private val MeanPatches     = 5.0
  private val BodySizeSigma   = 1.0
  private val PreyPerPredator = 0.0

  // ---- grid ---------------------------------------------------------------
  private val TileLo   = 10.0
  private val TileHi   = 200.0
  private val FracLo   = 0.05
  private val FracHi   = 0.40
  private val HrLevels = Vector(0.016, 0.020, 0.025)

  /** n log-spaced values in [lo, hi], rounded to distinct Ints (dedup collapses the coarse tail). */
  private def logIntSteps(lo: Double, hi: Double, n: Int): Vector[Int] =
    val ll = math.log(lo); val lh = math.log(hi)
    (0 until n)
      .map(i => math.round(math.exp(ll + (lh - ll) * i / (n - 1).toDouble)).toInt)
      .distinct.toVector

  private def linSteps(lo: Double, hi: Double, n: Int): Vector[Double] =
    (0 until n).map(i => lo + (hi - lo) * i / (n - 1).toDouble).toVector

  // ---- output schema ----------------------------------------------------
  private val Anchors = Seq(
    "sp_ext_primary", "sp_ext_secondary", "sp_ext_total",
    "spint_ext_primary", "spint_ext_secondary", "spint_ext_total",
    "sp_ext_basal", "sp_ext_intermediate", "sp_ext_top",
    "nCascades", "baseline_sp", "baseline_spint",
    "cover_species", "cover_ints",
  )
  private val Strategies = Seq("SR", "IR", "EIR")
  private val Contrasts = Seq(
    "dSp_IR_SR", "dSpInt_IR_SR",
    "dSp_EIR_SR", "dSpInt_EIR_SR",
  )
  private val GridCols = Seq("tile_idx", "frac_idx", "hr_idx", "tile", "nTiles", "frac", "hr", "seed")

  private def outputHeader: Seq[String] =
    GridCols ++
      Contrasts ++
      Strategies.flatMap(s => Anchors.map(a => s"${s}_$a")) ++
      Seq("pair_SR_IR_jaccard_protected", "pair_SR_IR_rankCorr_spearman")

  private def parseArgs(args: Array[String]): Map[String, String] =
    args.sliding(2, 2).collect { case Array(k, v) if k.startsWith("--") => k.drop(2) -> v }.toMap

  def main(args: Array[String]): Unit =
    val opts       = parseArgs(args)
    val outPath    = opts.getOrElse("out", "basin_sweep.csv")
    val nSeeds     = opts.get("seeds").map(_.toInt).getOrElse(50)
    val tileSteps  = opts.get("tile-steps").map(_.toInt).getOrElse(20)
    val fracSteps  = opts.get("frac-steps").map(_.toInt).getOrElse(20)
    val threads    = opts.get("threads").map(_.toInt).getOrElse(Runtime.getRuntime.availableProcessors)

    val tiles = logIntSteps(TileLo, TileHi, tileSteps)
    val fracs = linSteps(FracLo, FracHi, fracSteps)

    System.err.println(s"[BasinSweep] tiles(${tiles.size}, log, dedup)=${tiles.mkString(",")}")
    System.err.println(f"[BasinSweep] fracs(${fracs.size}, lin)=${fracs.map(f => f"$f%.3f").mkString(",")}")
    System.err.println(s"[BasinSweep] hr=${HrLevels.mkString(",")}  seeds=$nSeeds  threads=$threads  eps=$Epsilon pExt=$PExt")

    val allTasks: Vector[(Int, Int, Int, Int)] =
      for
        ti <- tiles.indices.toVector
        fi <- fracs.indices
        hi <- HrLevels.indices
        s  <- 1 to nSeeds
      yield (ti, fi, hi, s)

    // ---- resume ----
    val outFile    = new File(outPath)
    val header     = outputHeader
    val done       = scala.collection.mutable.HashSet.empty[(Int, Int, Int, Int)]
    val fileExists = outFile.exists() && outFile.length() > 0
    if fileExists then
      val src = scala.io.Source.fromFile(outFile, "UTF-8")
      try
        val it = src.getLines()
        if it.hasNext then it.next()
        val idx = header.zipWithIndex.toMap
        for line <- it if line.nonEmpty do
          val c = line.split(",", -1)
          try done += ((c(idx("tile_idx")).toInt, c(idx("frac_idx")).toInt,
                        c(idx("hr_idx")).toInt, c(idx("seed")).toInt))
          catch case _: Throwable => ()
      finally src.close()
      System.err.println(s"[BasinSweep] resume: ${done.size} rows already present")

    val todo = allTasks.filterNot(done.contains)
    System.err.println(s"[BasinSweep] ${todo.size} / ${allTasks.size} tasks to run")
    if todo.isEmpty then
      System.err.println("[BasinSweep] nothing to do — exiting.")
      return

    Option(outFile.getParentFile).foreach(_.mkdirs())
    val writer = new BufferedWriter(new OutputStreamWriter(
      new FileOutputStream(outFile, /* append = */ fileExists), StandardCharsets.UTF_8))
    if !fileExists then { writer.write(header.mkString(",")); writer.newLine(); writer.flush() }
    val writeLock = new AnyRef

    // Robustness shocks are the dominant cost at large home range and the dRobust_* contrasts
    // were unidentifiable in round 1 — skip them.
    System.setProperty("econetcons.skipRobustness", "true")

    def silence[T](b: => T): T =
      val prev = System.out
      System.setOut(new java.io.PrintStream((_: Int) => ()))
      try b finally System.setOut(prev)

    // warm JIT once, single-threaded
    silence {
      Observables.run(NumberOfSpecies, C, NPop, 0.03, 32, PreyPerPredator, 0.2, 7L,
        epsilon = Epsilon, extinctionProbabilityUnprotected = PExt,
        bodySizeSigma = BodySizeSigma, meanPatchesPerSpecies = MeanPatches)
    }

    val queue   = new ConcurrentLinkedQueue[(Int, Int, Int, Int)]()
    todo.foreach(queue.add)
    val counter = new AtomicInteger(0)
    val failed  = new AtomicInteger(0)
    val total   = todo.size
    val t0      = System.currentTimeMillis()
    val pool    = Executors.newFixedThreadPool(threads)

    val workers = (1 to threads).map { _ =>
      pool.submit(new Runnable {
        def run(): Unit =
          var task = queue.poll()
          while task != null do
            val (ti, fi, hi, seed) = task
            val tile = tiles(ti); val frac = fracs(fi); val hr = HrLevels(hi)
            val nTiles = NPop / tile
            try
              val m = silence {
                Observables.run(
                  numberOfSpecies = NumberOfSpecies, connectanceMetaWeb = C, nPopulations = NPop,
                  medianHomeRange = hr, avgPopulationsPerTile = tile, preyPerPredator = PreyPerPredator,
                  fractionProtected = frac, seed = seed.toLong,
                  epsilon = Epsilon, extinctionProbabilityUnprotected = PExt,
                  bodySizeSigma = BodySizeSigma, meanPatchesPerSpecies = MeanPatches,
                )
              }
              def g(k: String): Double = m.getOrElse(k, Double.NaN)
              val contrastVals = Seq(
                g("IR_sp_ext_total")     - g("SR_sp_ext_total"),
                g("IR_spint_ext_total")  - g("SR_spint_ext_total"),
                g("EIR_sp_ext_total")    - g("SR_sp_ext_total"),
                g("EIR_spint_ext_total") - g("SR_spint_ext_total"),
              )
              val anchorVals = Strategies.flatMap(s => Anchors.map(a => g(s"${s}_$a")))
              val pairVals   = Seq(g("pair_SR_IR_jaccard_protected"), g("pair_SR_IR_rankCorr_spearman"))
              val row =
                (Seq(ti, fi, hi, tile, nTiles).map(_.toString) ++
                 Seq(f"$frac%.4f", f"$hr%.4f", seed.toString) ++
                 (contrastVals ++ anchorVals ++ pairVals).map(v => f"$v%.5f")).mkString(",")
              writeLock.synchronized { writer.write(row); writer.newLine(); writer.flush() }
            catch
              case e: Throwable =>
                failed.incrementAndGet()
                System.err.println(s"[BasinSweep] FAILED tile=$tile frac=$frac hr=$hr seed=$seed: " +
                  s"${e.getClass.getSimpleName}: ${e.getMessage}")

            val n = counter.incrementAndGet()
            if n % 500 == 0 || n == total then
              val el     = (System.currentTimeMillis() - t0) / 1000.0
              val perRun = if n > 0 then el / n else 0.0
              val etaMin = perRun * (total - n) / 60.0
              System.err.println(
                f"[BasinSweep] $n%d/$total%d  ${el}%.0fs elapsed  ${perRun * 1000}%.0f ms/run(wall)  ETA ${etaMin}%.1f min  (failed ${failed.get}%d)")
            task = queue.poll()
      })
    }
    workers.foreach(_.get())
    pool.shutdown()
    writer.close()
    System.err.println(s"[BasinSweep] DONE -> $outPath  (${counter.get} runs, ${failed.get} failed)")
