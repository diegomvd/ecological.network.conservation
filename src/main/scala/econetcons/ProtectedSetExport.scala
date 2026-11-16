package econetcons

import java.io.{BufferedWriter, FileOutputStream, OutputStreamWriter}
import java.nio.charset.StandardCharsets
import java.util.concurrent.{ConcurrentLinkedQueue, Executors}
import java.util.zip.GZIPOutputStream
import scala.jdk.CollectionConverters.*
import scala.util.Random

/**
 * Population-level export for the paper sweep: what each strategy protects and what is left.
 *
 * For each (C, seed) the community is assembled once (paper parameter set, PaperSweep.worldParameters)
 * and, for each tile size, SR and IR are run exactly as in Observables.runStrategy (same RNG streams),
 * at one budget and pressure. One row per population:
 *
 *   stage = baseline   post-stabilization web (shared by strategies and tile sizes; written once per
 *                      (C, seed), strategy and tile_size empty)
 *   stage = protected  populations in the strategy's protected tiles (post-stabilization)
 *   stage = final      populations surviving primary extinctions + cascade
 *
 * Columns: c_meta, tile_size, seed, stage, strategy, pop_id, species_id, basal, body_mass, flow_tp, biomass.
 * flow_tp is the flow trophic position on that stage's web (basal herbivores = 1, plants not counted),
 * biomass = individuals × body mass on that web.
 *
 * With --verify, each case is also run through Observables.runWith and the species / interaction losses
 * are checked to be identical (the export replays the sweep, not an approximation of it).
 *
 * Usage:
 *   sbt -J-Xmx40g "runMain econetcons.ProtectedSetExport [--out data/paper_sweep/protected_sets.csv.gz]
 *        [--cMeta 0.05,0.1,0.2] [--tileSize 0.25,2] [--frac 0.15] [--pExt 0.8] [--seeds 50] [--threads 20]
 *        [--verify false]"
 */
object ProtectedSetExport:

  private val Header = Seq("c_meta", "tile_size", "seed", "stage", "strategy", "pop_id", "species_id", "basal",
    "body_mass", "flow_tp", "biomass")

  private def parseArgs(args: Array[String]): Map[String, String] =
    args.sliding(2, 2).collect { case Array(k, v) if k.startsWith("--") => k.drop(2) -> v }.toMap

  /** One row per population of `w`'s web (or of the subset `keep`). */
  private def rows(prefix: String, w: World, keep: Population => Boolean): Seq[String] =
    val web = w.populationWeb
    val size = (id: Int) => w.popIndividuals.getOrElse(id, 0.0)
    val (tp, _) = EnergyFlow.flowTrophicPositions(web, size, w.worldParameters.transferEfficiency, w.diet)
    web.vertexSet().asScala.toSeq.filter(keep).sortBy(_.id).map { p =>
      val s = p.species
      f"$prefix,${p.id},${s.id},${if s.trophicLevel == 0 then 1 else 0},${s.bodySize}%.6g,${tp(p.id)}%.6g,${w.biomassOf(p)}%.6g"
    }

  /**
   * Replays Observables.runStrategy for one strategy (same steps and RNG streams). `atStable` sees the
   * post-stabilization world before primary extinctions mutate its web in place; returns the final world.
   */
  private[econetcons] def replay(baseWorld: World, strategy: ConservationStrategy, frac: Double, seed: Long)
      (atStable: World => Unit): World =
    val cp = ConservationParameters(fractionProtected = frac, strategy = Some(strategy))
    val planned = baseWorld.copy(
      managementLandscape = baseWorld.managementLandscape.applyProtectionPlan(cp, new Random(seed)),
      conservationParameters = cp
    ).deepCopyForExtinction(new Random(seed))
    val (stable, _) = planned.phase2Cascades()
    atStable(stable)
    stable.primaryExtinctions().phase2Cascades()._1

  def main(args: Array[String]): Unit =
    val opts = parseArgs(args)
    def list(k: String, d: Seq[Double]) = opts.get(k).map(_.split(",").toSeq.map(_.trim.toDouble)).getOrElse(d)
    val outPath   = opts.getOrElse("out", "data/paper_sweep/protected_sets.csv.gz")
    val cs        = list("cMeta", Seq(0.05, 0.1, 0.2))
    val tileSizes = list("tileSize", Seq(0.25, 2.0))
    val frac      = opts.get("frac").map(_.toDouble).getOrElse(0.15)
    val pExt      = opts.get("pExt").map(_.toDouble).getOrElse(0.8)
    val nSeeds    = opts.get("seeds").map(_.toInt).getOrElse(50)
    val threads   = opts.get("threads").map(_.toInt).getOrElse(Runtime.getRuntime.availableProcessors)
    val verify    = opts.get("verify").contains("true")
    val rho       = opts.get("rho").map(_.toDouble).getOrElse(0.03)
    val hr        = opts.get("hr").map(_.toDouble).getOrElse(0.02)
    System.setProperty("econetcons.skipRobustness", "true")
    val err = System.err

    new java.io.File(outPath).getAbsoluteFile.getParentFile.mkdirs()
    val raw = new FileOutputStream(outPath)
    val writer = new BufferedWriter(new OutputStreamWriter(
      if outPath.endsWith(".gz") then new GZIPOutputStream(raw, 1 << 16) else raw, StandardCharsets.UTF_8))
    writer.write(Header.mkString(",")); writer.newLine()
    val lock = new AnyRef

    val tasks = new ConcurrentLinkedQueue((for c <- cs.sortBy(-_); s <- 1L to nSeeds.toLong yield (c, s)).asJava)
    val total = tasks.size
    val done = new java.util.concurrent.atomic.AtomicInteger(0)
    val failed = new java.util.concurrent.atomic.AtomicInteger(0)
    err.println(s"[ProtectedSetExport] $total assemblies, C=${cs.mkString(",")} t=${tileSizes.mkString(",")} frac=$frac pExt=$pExt verify=$verify -> $outPath")

    val worker: Runnable = () =>
      var task = tasks.poll()
      while task != null do
        val (c, seed) = task
        try
          val wp0 = PaperSweep.worldParameters(c, rho, hr)
          val a = World.assemble(wp0, new Random(seed))
          val out = Seq.newBuilder[String]
          tileSizes.zipWithIndex.foreach { case (t, ti) =>
            val wp = wp0.copy(tileSizeInHomeRanges = t, extinctionProbabilityUnprotected = pExt)
            val baseCp = ConservationParameters(fractionProtected = 0.0, strategy = Some(ConservationStrategy.SpeciesRichness))
            val baseWorld = World.fromAssembly(a, wp, baseCp)
            val losses = Observables.paperStrategies.map { spec =>
              var baseSp, baseInts = 0
              val finalW = replay(baseWorld, spec.strategy, frac, seed) { stable =>
                if ti == 0 && spec.label == "SR" then out ++= rows(f"$c%.4f,,$seed,baseline,", stable, _ => true)
                val prot = stable.managementLandscape.managementAreas.iterator
                  .filter(_.status == ProtectionStatus.Protected).flatMap(_.populations).toSet
                out ++= rows(f"$c%.4f,$t%.4f,$seed,protected,${spec.label}", stable, p => prot.contains((p.id, p.species.id)))
                baseSp = stable.getSpeciesRichness()
                baseInts = stable.getRealizedWeb.edgeSet().size()
              }
              out ++= rows(f"$c%.4f,$t%.4f,$seed,final,${spec.label}", finalW, _ => true)
              spec.label -> (
                100.0 * (baseSp - finalW.getSpeciesRichness()) / baseSp,
                100.0 * (baseInts - finalW.getRealizedWeb.edgeSet().size()) / baseInts)
            }.toMap
            if verify then
              val m = Observables.runWith(wp, frac, seed, Observables.paperStrategies, Some(a))
              losses.foreach { case (l, (sp, in)) =>
                require(math.abs(m(s"${l}_sp_ext_total") - sp) < 1e-9 && math.abs(m(s"${l}_spint_ext_total") - in) < 1e-9,
                  s"replay mismatch C=$c t=$t seed=$seed $l: export ($sp, $in) vs runWith (${m(s"${l}_sp_ext_total")}, ${m(s"${l}_spint_ext_total")})")
              }
          }
          val lines = out.result()
          lock.synchronized { lines.foreach { l => writer.write(l); writer.newLine() } }
        catch
          case e: Throwable =>
            failed.incrementAndGet()
            err.println(s"[ProtectedSetExport] FAILED C=$c seed=$seed: ${e.getClass.getSimpleName}: ${e.getMessage}")
        val n = done.incrementAndGet()
        if n % 10 == 0 || n == total then err.println(s"[ProtectedSetExport] $n/$total (failed ${failed.get})")
        task = tasks.poll()

    val prevOut = System.out
    System.setOut(new java.io.PrintStream((_: Int) => ()))
    val pool = Executors.newFixedThreadPool(threads)
    try (1 to threads).map(_ => pool.submit(worker)).foreach(_.get())
    finally
      pool.shutdown(); writer.close(); System.setOut(prevOut)
    err.println(s"[ProtectedSetExport] DONE -> $outPath (${done.get} assemblies, ${failed.get} failed)")
