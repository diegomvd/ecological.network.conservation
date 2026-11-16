package econetcons

import java.io.{BufferedWriter, File, FileOutputStream, OutputStreamWriter}
import java.nio.charset.StandardCharsets
import java.util.concurrent.{ConcurrentLinkedQueue, Executors}
import java.util.concurrent.atomic.AtomicInteger
import scala.jdk.CollectionConverters.*
import scala.util.Random

/**
 * Paper sweep: SR vs IR (= EgocentricInteractionRichness, Observables.paperStrategies) on the emergent
 * assembly (paper parameter set, MODEL_PARAMETERS.md), across
 *
 *   connectanceMetaWeb C        — web structure (default 0.05, 0.1, 0.2)
 *   tileSizeInHomeRanges t      — management grain, tile side in median home-range diameters (0.5, 1, 2, 4)
 *   fractionProtected           — budget, linear steps (default 20 in 0.05 .. 0.40)
 *   extinctionProbabilityUnprotected — pressure (0.3, 0.5, 0.8)
 *   seed                        — replication (default 30)
 *
 * The community is assembled ONCE per (C, seed) (World.assemble) and reused for every (t, frac, pExt)
 * variant via Observables.runWith(…, Some(assembly)); results are identical to fresh runs (ModelChecks §7).
 * Robustness shocks are disabled (as in BasinSweep).
 *
 * Output: one row per (C, t, frac, pExt, seed): grid columns, the assembled community (shared by all
 * variants of a (C, seed)), the paired contrasts dSp_IR_SR / dSpInt_IR_SR (IR − SR, percentage points of
 * the post-stabilization baseline), per-strategy anchors and pairwise metrics. RESUMABLE: rows already in
 * the file are skipped (a (C, seed) whose variants are all done is not re-assembled). Rows are flushed
 * as produced.
 *
 * Usage:
 *   sbt -J-Xmx40g "runMain econetcons.PaperSweep [--out data/paper_sweep/paper_sweep.csv] [--seeds 30] [--threads 20]
 *        [--cMeta 0.05,0.1,0.2] [--tileSize 0.5,1,2,4] [--fracSteps 20] [--fracLo 0.05] [--fracHi 0.4]
 *        [--pExt 0.3,0.5,0.8] [--rho 0.03] [--hr 0.02]"
 */
object PaperSweep:

  // ---- paper parameter set (fixed) ---------------------------------------
  private[econetcons] def worldParameters(c: Double, rho: Double, hr: Double): WorldParameters = WorldParameters(
    numberOfSpecies = 300, connectanceMetaWeb = c, medianHomeRange = hr, bodySizeSigma = 1.0,
    meanPatchesPerSpecies = 5.0, abundanceModel = "emergent", transferEfficiency = 0.1,
    rangeFraction = rho, rangeFractionSigma = 1.0, rangeMassExponent = 0.0, basalBiomass = 5.0e6,
    minViableIndividuals = 10.0, homeRangeExponent = 0.5, foragingRule = "supply")

  private val Anchors = Seq(
    "sp_ext_primary", "sp_ext_secondary", "sp_ext_total",
    "spint_ext_primary", "spint_ext_secondary", "spint_ext_total",
    "popint_ext_total",
    "sp_ext_basal", "sp_ext_intermediate", "sp_ext_top",
    "sp_ext_bodyQ1", "sp_ext_bodyQ2", "sp_ext_bodyQ3", "sp_ext_bodyQ4",
    "nCascades", "baseline_sp", "baseline_spint", "baseline_popint",
    "cover_species", "cover_ints",
  )
  private val Strategies = Seq("SR", "IR")
  private val GridCols = Seq("c_meta", "tile_size", "tiles_per_side", "frac", "p_ext", "seed")
  private val AssemblyCols = Seq("assembled_pops") ++ WebStructureSweep.MetricNames.map("assembled_" + _)
  private val PairCols = Seq("pair_SR_IR_jaccard_protected", "pair_SR_IR_rankCorr_spearman",
    "pair_SR_IR_uniqueSpecies_SR", "pair_SR_IR_uniqueSpecies_IR", "pair_SR_IR_uniqueInts_SR", "pair_SR_IR_uniqueInts_IR")
  private val Header = GridCols ++ AssemblyCols ++ Seq("dSp_IR_SR", "dSpInt_IR_SR") ++
    Strategies.flatMap(s => Anchors.map(a => s"${s}_$a")) ++ PairCols

  private def parseArgs(args: Array[String]): Map[String, String] =
    args.sliding(2, 2).collect { case Array(k, v) if k.startsWith("--") => k.drop(2) -> v }.toMap

  private def key(c: Double, t: Double, frac: Double, pExt: Double, seed: Long): String =
    f"$c%.4f|$t%.4f|$frac%.4f|$pExt%.4f|$seed"

  def main(args: Array[String]): Unit =
    val opts = parseArgs(args)
    def list(k: String, d: Seq[Double]) = opts.get(k).map(_.split(",").toSeq.map(_.trim.toDouble)).getOrElse(d)
    val outPath   = opts.getOrElse("out", "data/paper_sweep/paper_sweep.csv")
    val nSeeds    = opts.get("seeds").map(_.toInt).getOrElse(30)
    val threads   = opts.get("threads").map(_.toInt).getOrElse(Runtime.getRuntime.availableProcessors)
    val cs        = list("cMeta", Seq(0.05, 0.1, 0.2))
    val tileSizes = list("tileSize", Seq(0.5, 1.0, 2.0, 4.0))
    val fracSteps = opts.get("fracSteps").map(_.toInt).getOrElse(20)
    val (fracLo, fracHi) = (opts.get("fracLo").map(_.toDouble).getOrElse(0.05), opts.get("fracHi").map(_.toDouble).getOrElse(0.40))
    val fracs     = if fracSteps == 1 then Seq(fracLo) else (0 until fracSteps).map(i => fracLo + (fracHi - fracLo) * i / (fracSteps - 1))
    val pExts     = list("pExt", Seq(0.3, 0.5, 0.8))
    val rho       = opts.get("rho").map(_.toDouble).getOrElse(0.03)
    val hr        = opts.get("hr").map(_.toDouble).getOrElse(0.02)
    val err = System.err
    System.setProperty("econetcons.skipRobustness", "true")

    // ---- resume ----
    val outFile = new File(outPath)
    val done = scala.collection.mutable.HashSet.empty[String]
    if outFile.exists() && outFile.length() > 0 then
      val src = scala.io.Source.fromFile(outFile, "UTF-8")
      try
        val lines = src.getLines()
        val idx = lines.next().split(",").zipWithIndex.toMap
        lines.foreach { l =>
          val c = l.split(",", -1)
          if c.length == Header.size then
            done += key(c(idx("c_meta")).toDouble, c(idx("tile_size")).toDouble, c(idx("frac")).toDouble,
              c(idx("p_ext")).toDouble, c(idx("seed")).toLong)
        }
      finally src.close()
    val appending = done.nonEmpty
    Option(outFile.getAbsoluteFile.getParentFile).foreach(_.mkdirs())
    val writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(outFile, appending), StandardCharsets.UTF_8))
    if !appending then { writer.write(Header.mkString(",")); writer.newLine(); writer.flush() }
    val lock = new AnyRef

    // ---- work: one task per (C, seed), heaviest webs first ----
    val variants = for t <- tileSizes; f <- fracs; p <- pExts yield (t, f, p)
    val groups = (for c <- cs.sortBy(-_); s <- 1L to nSeeds.toLong yield (c, s))
      .map { case (c, s) => (c, s, variants.filterNot { case (t, f, p) => done.contains(key(c, t, f, p, s)) }) }
      .filter(_._3.nonEmpty)
    val total = groups.map(_._3.size).sum
    err.println(s"[PaperSweep] ${groups.size} assemblies × ≤${variants.size} variants = $total runs " +
      s"(${done.size} already done), $threads threads -> $outPath")
    err.println(s"[PaperSweep] C=${cs.mkString(",")} t=${tileSizes.mkString(",")} frac=${fracs.map(f => f"$f%.3f").mkString(",")} " +
      s"pExt=${pExts.mkString(",")} rho=$rho hr=$hr")

    val queue = new ConcurrentLinkedQueue(groups.asJava)
    val counter = new AtomicInteger(0)
    val failed = new AtomicInteger(0)
    val assemblySec = new java.util.concurrent.atomic.DoubleAdder()
    val t0 = System.currentTimeMillis()
    val worker: Runnable = () =>
      var g = queue.poll()
      while g != null do
        val (c, seed, todo) = g
        val wp0 = worldParameters(c, rho, hr)
        try
          val ta = System.nanoTime()
          val a = World.assemble(wp0, new Random(seed))
          assemblySec.add((System.nanoTime() - ta) / 1e9)
          val asm = Seq(a.assembledSize.toDouble) ++ WebStructureSweep.metrics(a.populationWeb)
          todo.foreach { case (t, frac, pExt) =>
            val wp = wp0.copy(tileSizeInHomeRanges = t, extinctionProbabilityUnprotected = pExt)
            try
              val m = Observables.runWith(wp, frac, seed, Observables.paperStrategies, Some(a))
              def v(k: String) = m.getOrElse(k, Double.NaN)
              val vals = asm ++ Seq(v("IR_sp_ext_total") - v("SR_sp_ext_total"), v("IR_spint_ext_total") - v("SR_spint_ext_total")) ++
                Strategies.flatMap(s => Anchors.map(an => v(s"${s}_$an"))) ++ PairCols.map(v)
              val row = Seq(f"$c%.4f", f"$t%.4f", wp.tilesPerSide.toString, f"$frac%.4f", f"$pExt%.4f", seed.toString) ++
                vals.map(x => f"$x%.6g")
              lock.synchronized { writer.write(row.mkString(",")); writer.newLine(); writer.flush() }
            catch
              case e: Throwable =>
                failed.incrementAndGet()
                err.println(s"[PaperSweep] FAILED C=$c t=$t frac=$frac pExt=$pExt seed=$seed: ${e.getClass.getSimpleName}: ${e.getMessage}")
            val n = counter.incrementAndGet()
            if n % 500 == 0 || n == total then
              val el = (System.currentTimeMillis() - t0) / 1000.0
              err.println(f"[PaperSweep] $n%d/$total%d  ${el}%.0fs  ${n / el}%.2f runs/s  ETA ${el / n * (total - n) / 60}%.1f min  " +
                f"(failed ${failed.get}%d, assembly ${assemblySec.sum}%.0f thread-s)")
          }
        catch
          case e: Throwable =>
            failed.addAndGet(todo.size); counter.addAndGet(todo.size)
            err.println(s"[PaperSweep] FAILED assembly C=$c seed=$seed: ${e.getClass.getSimpleName}: ${e.getMessage}")
        g = queue.poll()

    val prevOut = System.out
    System.setOut(new java.io.PrintStream((_: Int) => ()))
    val pool = Executors.newFixedThreadPool(threads)
    try (1 to threads).map(_ => pool.submit(worker)).foreach(_.get())
    finally
      pool.shutdown(); writer.close(); System.setOut(prevOut)
    err.println(s"[PaperSweep] DONE -> $outPath  (${counter.get} runs, ${failed.get} failed, " +
      f"${(System.currentTimeMillis() - t0) / 60000.0}%.1f min)")

