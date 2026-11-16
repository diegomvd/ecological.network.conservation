package econetcons

import java.io.{BufferedWriter, FileOutputStream, OutputStreamWriter}
import java.nio.charset.StandardCharsets
import java.util.concurrent.{ConcurrentLinkedQueue, Executors}
import scala.jdk.CollectionConverters.*
import scala.util.Random

/**
 * Species loss by trophic-position class, SR vs IR, over the budget axis of the paper sweep.
 *
 * Each species gets a trophic-position class from the post-stabilization (baseline) web: the biomass-weighted mean
 * flow trophic position of its populations, counting plants (herbivores = 2), rounded to 2, 3, 4 or 5 (5 = ≥ 4.5).
 * The baseline web does not depend on the protection plan or the tiling, so the classes are computed once per
 * (C, seed) and every replay is checked to start from the same baseline species set.
 *
 * Runs are replays of Observables.runStrategy (ProtectedSetExport.replay), so they are the same runs as
 * PaperSweep at the shared grid points. One row per (C, t, frac, pExt, seed, strategy, class):
 * n_baseline species of that class and n_lost of them extinct at the end. With --verify, the totals over classes
 * are checked against Observables.runWith (sp_ext_total).
 *
 * Usage:
 *   sbt -J-Xmx40g "runMain econetcons.TrophicLossSweep [--out data/paper_sweep/trophic_loss.csv]
 *        [--cMeta 0.05,0.1,0.2] [--tileSize 0.25,0.5,2] [--fracSteps 20] [--fracLo 0.05] [--fracHi 0.4]
 *        [--pExt 0.8] [--seeds 50] [--threads 20] [--verifySeeds 0]"
 */
object TrophicLossSweep:

  private val Header = Seq("c_meta", "tile_size", "frac", "p_ext", "seed", "strategy", "tp_class", "n_baseline", "n_lost")

  private def parseArgs(args: Array[String]): Map[String, String] =
    args.sliding(2, 2).collect { case Array(k, v) if k.startsWith("--") => k.drop(2) -> v }.toMap

  /** Trophic-position class (2..5) of every species alive in the baseline web `w`. */
  private def speciesClasses(w: World): Map[Int, Int] =
    val web = w.populationWeb
    val size = (id: Int) => w.popIndividuals.getOrElse(id, 0.0)
    val (tp, _) = EnergyFlow.flowTrophicPositions(web, size, w.worldParameters.transferEfficiency, w.diet)
    web.vertexSet().asScala.toSeq.groupBy(_.species.id).map { case (sid, pops) =>
      val b = pops.map(w.biomassOf)
      val meanTp = pops.zip(b).map((p, x) => tp(p.id) * x).sum / b.sum + 1.0
      sid -> math.min(5, math.round(meanTp).toInt)
    }

  private def aliveSpecies(w: World): Set[Int] = w.populations.iterator.filter(!_.extinct).map(_.species.id).toSet

  def main(args: Array[String]): Unit =
    val opts = parseArgs(args)
    def list(k: String, d: Seq[Double]) = opts.get(k).map(_.split(",").toSeq.map(_.trim.toDouble)).getOrElse(d)
    val outPath   = opts.getOrElse("out", "data/paper_sweep/trophic_loss.csv")
    val cs        = list("cMeta", Seq(0.05, 0.1, 0.2))
    val tileSizes = list("tileSize", Seq(0.25, 0.5, 2.0))
    val fracSteps = opts.get("fracSteps").map(_.toInt).getOrElse(20)
    val (fracLo, fracHi) = (opts.get("fracLo").map(_.toDouble).getOrElse(0.05), opts.get("fracHi").map(_.toDouble).getOrElse(0.40))
    val fracs     = if fracSteps == 1 then Seq(fracLo) else (0 until fracSteps).map(i => fracLo + (fracHi - fracLo) * i / (fracSteps - 1))
    val pExts     = list("pExt", Seq(0.8))
    val nSeeds    = opts.get("seeds").map(_.toInt).getOrElse(50)
    val threads   = opts.get("threads").map(_.toInt).getOrElse(Runtime.getRuntime.availableProcessors)
    val verifySeeds = opts.get("verifySeeds").map(_.toInt).getOrElse(0)
    val rho       = opts.get("rho").map(_.toDouble).getOrElse(0.03)
    val hr        = opts.get("hr").map(_.toDouble).getOrElse(0.02)
    System.setProperty("econetcons.skipRobustness", "true")
    val err = System.err

    new java.io.File(outPath).getAbsoluteFile.getParentFile.mkdirs()
    val writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(outPath), StandardCharsets.UTF_8))
    writer.write(Header.mkString(",")); writer.newLine()
    val lock = new AnyRef

    val tasks = new ConcurrentLinkedQueue((for c <- cs.sortBy(-_); s <- 1L to nSeeds.toLong yield (c, s)).asJava)
    val total = tasks.size
    val done = new java.util.concurrent.atomic.AtomicInteger(0)
    val failed = new java.util.concurrent.atomic.AtomicInteger(0)
    val t0 = System.currentTimeMillis()
    err.println(s"[TrophicLossSweep] $total assemblies × ${tileSizes.size * fracs.size * pExts.size} variants × 2 strategies, " +
      s"C=${cs.mkString(",")} t=${tileSizes.mkString(",")} pExt=${pExts.mkString(",")} -> $outPath")

    val worker: Runnable = () =>
      var task = tasks.poll()
      while task != null do
        val (c, seed) = task
        try
          val wp0 = PaperSweep.worldParameters(c, rho, hr)
          val a = World.assemble(wp0, new Random(seed))
          var classes: Map[Int, Int] = null
          val out = Seq.newBuilder[String]
          for t <- tileSizes; frac <- fracs; pExt <- pExts do
            val wp = wp0.copy(tileSizeInHomeRanges = t, extinctionProbabilityUnprotected = pExt)
            val baseCp = ConservationParameters(fractionProtected = 0.0, strategy = Some(ConservationStrategy.SpeciesRichness))
            val baseWorld = World.fromAssembly(a, wp, baseCp)
            val totals = Observables.paperStrategies.map { spec =>
              var base = Set.empty[Int]
              val finalW = ProtectedSetExport.replay(baseWorld, spec.strategy, frac, seed) { stable =>
                base = aliveSpecies(stable)
                if classes == null then classes = speciesClasses(stable)
                require(base == classes.keySet, s"baseline species differ across replays (C=$c seed=$seed t=$t frac=$frac)")
              }
              val lost = base.diff(aliveSpecies(finalW))
              (2 to 5).foreach { k =>
                val inK = base.filter(classes(_) == k)
                out += f"$c%.4f,$t%.4f,$frac%.4f,$pExt%.4f,$seed,${spec.label},$k,${inK.size},${inK.count(lost.contains)}"
              }
              spec.label -> 100.0 * lost.size / base.size
            }.toMap
            if seed <= verifySeeds then
              val m = Observables.runWith(wp, frac, seed, Observables.paperStrategies, Some(a))
              totals.foreach { case (l, v) =>
                require(math.abs(m(s"${l}_sp_ext_total") - v) < 1e-9, s"replay mismatch C=$c t=$t frac=$frac seed=$seed $l: $v vs ${m(s"${l}_sp_ext_total")}")
              }
          val lines = out.result()
          lock.synchronized { lines.foreach { l => writer.write(l); writer.newLine() }; writer.flush() }
        catch
          case e: Throwable =>
            failed.incrementAndGet()
            err.println(s"[TrophicLossSweep] FAILED C=$c seed=$seed: ${e.getClass.getSimpleName}: ${e.getMessage}")
        val n = done.incrementAndGet()
        if n % 10 == 0 || n == total then
          val el = (System.currentTimeMillis() - t0) / 1000.0
          err.println(f"[TrophicLossSweep] $n/$total  ${el}%.0fs  ETA ${el / n * (total - n) / 60}%.1f min  (failed ${failed.get})")
        task = tasks.poll()

    val prevOut = System.out
    System.setOut(new java.io.PrintStream((_: Int) => ()))
    val pool = Executors.newFixedThreadPool(threads)
    try (1 to threads).map(_ => pool.submit(worker)).foreach(_.get())
    finally
      pool.shutdown(); writer.close(); System.setOut(prevOut)
    err.println(s"[TrophicLossSweep] DONE -> $outPath (${done.get} assemblies, ${failed.get} failed)")
