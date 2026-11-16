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
 * --grid biomass (default): the biomassPyramid calibration grid (uniform placement, TE × n_pop × ε,
 * totalBiomass set per world so that ~--targetPops populations are placed) plus the S1 and legacy
 * baselines. --grid assembly: the earlier allocation × cascade comparison. --grid all: both.
 *
 * Usage: sbt -J-Xmx40g "runMain econetcons.AssemblyDiagnostic [--out assembly_diagnostic.csv] [--seeds 5]
 *          [--threads 20] [--grid biomass|assembly|all] [--targetPops 10000] [--tpBandwidth 0.5] [--nPop 10,30,100]
 *          [--te ..] [--eps ..] [--rho 0.01,0.03,0.1] [--rhoSigma 1] [--hrExp 0.75,0.5] [--basalBiomass 5e6]
 *          [--assemblyOnly true]
 *          [--tileSize 0] [--cMeta 0.05,0.2] [--hr 0.016,0.025]"
 * --tileSize t > 0: management tiles of side t median home-range diameters (WorldParameters.tileSizeInHomeRanges)
 * for the SR/IR disturbance runs instead of 32 populations per tile.
 * --grid emergent: abundanceModel=emergent (EnergyFlow: range fraction ρ_s ~ --rho · LogNormal(0, --rhoSigma),
 * k_s ~ Poisson(ρ_s / π r_s²) Matérn seeds per species, total basal biomass --basalBiomass, TE --te, default 0.1),
 * plus the baselines, each for every home-range exponent --hrExp (r ∝ M^b).
 * --assemblyOnly true: skip the disturbance runs (SR/IR columns are NaN) and the finest-grain world.
 * Biomass columns (Σ individuals × mass of assembled populations): by trophic group and by Levine
 * trophic position rounded to the nearest integer (tp1 … tp5, tp5 = TP ≥ 4.5).
 */
object AssemblyDiagnostic:

  private final case class Config(label: String, abundance: String, cascade: String, epsilon: Double,
    theta: Double, te: Double, nPop: Int = 1, rho: Double = 0.03,
    rhoSigma: Double = 1.0, basalBiomass: Double = 5.0e6, hrExp: Double = 0.5, foraging: String = "supply")

  private val Baselines = Seq(
    Config("S1_eps2", "damuth", "metabolic", 2.0, 1.0, 0.1),
    Config("legacy",  "damuth", "legacy",    0.0, 1.0, 0.1),
  )
  private val AssemblyConfigs = Baselines ++ Seq(
    Config("C1_th0.3",           "damuth",             "energyTolerance", 0.0, 0.3, 0.1),
    Config("C1_th0.5",           "damuth",             "energyTolerance", 0.0, 0.5, 0.1),
    Config("C1_th0.7",           "damuth",             "energyTolerance", 0.0, 0.7, 0.1),
    Config("C1_th1.0",           "damuth",             "energyTolerance", 0.0, 1.0, 0.1),
    Config("C1_th0.5+TE0.1",     "transferEfficiency", "energyTolerance", 0.0, 0.5, 0.1),
    Config("C1_th0.5+EB_eps2",   "energyBalance",      "energyTolerance", 2.0, 0.5, 0.1),
    Config("C2_EB_eps2",         "energyBalance",      "metabolic",       2.0, 1.0, 0.1),
    Config("C2_EB_eps6",         "energyBalance",      "metabolic",       6.0, 1.0, 0.1),
    Config("C3_TE0.05",          "transferEfficiency", "metabolic",       2.0, 1.0, 0.05),
    Config("C3_TE0.1",           "transferEfficiency", "metabolic",       2.0, 1.0, 0.1),
    Config("C3_TE0.2",           "transferEfficiency", "metabolic",       2.0, 1.0, 0.2),
  )
  private def biomassConfigs(tes: Seq[Double], nPops: Seq[Int], epss: Seq[Double]) =
    for te <- tes; nPop <- nPops; eps <- epss
    yield Config(f"BP_TE${te}%.1f_n${nPop}_eps${eps}%.2g", "biomassPyramid", "metabolic", eps, 1.0, te, nPop)

  private def emergentConfigs(rhos: Seq[Double], sigmas: Seq[Double], basalBiomass: Seq[Double], tes: Seq[Double],
      hrExps: Seq[Double], foragings: Seq[String]) =
    (for b <- hrExps; c <- Baselines yield c.copy(label = f"${c.label}_b$b%.2f", hrExp = b)) ++
    (for b <- hrExps; r <- rhos; sg <- sigmas; bb <- basalBiomass; te <- tes; fr <- foragings
     yield Config(f"EM_${fr}_rho${r}%.3g_sd${sg}%.2g_b$b%.2f_B${bb}%.2g_TE${te}%.2g", "emergent", "metabolic", 0.0, 1.0, te,
       rho = r, rhoSigma = sg, basalBiomass = bb, hrExp = b, foraging = fr))

  private val BiomassNames = Seq("biomass_total", "biomass_basal", "biomass_inter", "biomass_top", "biomass_consumers") ++
    (1 to 5).map(k => s"biomass_tp$k") ++ (1 to 5).map(k => s"biomass_ftp$k") ++ (2 to 5).map(k => s"intake_ftp$k") ++
    Seq("consumer_basal_intake_share", "consumer_mean_ftp", "consumer_mean_levine_tp") ++
    // unweighted, per consumer species (flow TP and basal share averaged over its populations)
    (2 to 5).map(k => s"species_ftp$k") ++ Seq("species_mean_ftp", "species_mean_basal_share", "species_frac_basal_share_lt_half",
      "species_max_ftp")

  /** Σ biomass of `pops` by trophic group, all consumers, and by TP rounded to the nearest integer (capped at 5). */
  /** biomass by trophic group, Levine TP bin (metaweb) and realized flow-TP bin; intake by flow-TP bin;
   *  biomass-weighted consumer basal-intake share, mean flow TP and mean Levine TP (emergent only, else NaN). */
  private def biomassByLevel(w: World, tp: Map[Int, Double]): Seq[Double] =
    val pops = w.populationWeb.vertexSet().asScala
    val biomass = w.biomassOf
    def bin(x: Double) = math.min(5, math.max(1, math.round(x).toInt))
    def sum(f: Population => Boolean) = pops.iterator.filter(f).map(biomass).sum
    val base = Seq(sum(_ => true), sum(_.species.trophicLevel == 0), sum(_.species.trophicLevel == 1),
      sum(_.species.trophicLevel == 2), sum(_.species.trophicLevel != 0)) ++
      (1 to 5).map(k => sum(p => bin(tp(p.species.id)) == k))
    if w.popIndividuals.isEmpty then base ++ Seq.fill(5 + 4 + 3 + 8)(Double.NaN)
    else
      val te = w.worldParameters.transferEfficiency
      val size = (id: Int) => w.popIndividuals.getOrElse(id, 0.0)
      val (ftp, basalShare) = EnergyFlow.flowTrophicPositions(w.populationWeb, size, te, w.diet)
      val intake = EnergyFlow.intakes(w.populationWeb, size, te, w.diet)
      val cons = pops.filter(_.species.trophicLevel != 0)
      val bc = cons.iterator.map(biomass).sum
      def wmean(f: Population => Double) = cons.iterator.map(p => biomass(p) * f(p)).sum / bc
      base ++ (1 to 5).map(k => sum(p => bin(ftp(p.id)) == k)) ++
        (2 to 5).map(k => cons.iterator.filter(p => bin(ftp(p.id)) == k).map(p => intake(p.id)).sum) ++
        Seq(wmean(p => basalShare(p.id)), wmean(p => ftp(p.id)), wmean(p => tp(p.species.id))) ++ {
          val bySp = cons.toSeq.groupBy(_.species.id).values.map(ps =>
            (ps.map(p => ftp(p.id)).sum / ps.size, ps.map(p => basalShare(p.id)).sum / ps.size)).toSeq
          (2 to 5).map(k => bySp.count(t => bin(t._1) == k).toDouble) ++
            Seq(bySp.map(_._1).sum / bySp.size, bySp.map(_._2).sum / bySp.size,
              bySp.count(_._2 < 0.5).toDouble / bySp.size, bySp.map(_._1).max)
        }


  private def parseArgs(args: Array[String]): Map[String, String] =
    args.sliding(2, 2).collect { case Array(k, v) if k.startsWith("--") => k.drop(2) -> v }.toMap

  private def groups(pops: Iterable[Population]): Seq[Double] =
    Seq(pops.size, pops.count(_.species.trophicLevel == 0), pops.count(_.species.trophicLevel == 1),
      pops.count(_.species.trophicLevel == 2)).map(_.toDouble)

  private def speciesGroups(pops: Iterable[Population]): Seq[Double] =
    val sp = pops.map(_.species).toSet
    Seq(sp.size, sp.count(_.trophicLevel == 0), sp.count(_.trophicLevel == 1), sp.count(_.trophicLevel == 2)).map(_.toDouble)

  def main(args: Array[String]): Unit =
    val opts    = parseArgs(args)
    val outPath = opts.getOrElse("out", "assembly_diagnostic.csv")
    val nSeeds  = opts.get("seeds").map(_.toInt).getOrElse(5)
    def list(k: String) = opts.get(k).map(_.split(",").toSeq.map(_.trim))
    val targetPops = opts.get("targetPops").map(_.toInt).getOrElse(10000)
    val tpBandwidth = opts.get("tpBandwidth").map(_.toDouble).getOrElse(0.5)
    val BiomassConfigs = biomassConfigs(list("te").map(_.map(_.toDouble)).getOrElse(Seq(0.1, 0.2, 0.3)),
      list("nPop").map(_.map(_.toInt)).getOrElse(Seq(10, 30, 100)), list("eps").map(_.map(_.toDouble)).getOrElse(Seq(1.0, 2.0, 4.0)))
    val Configs = opts.getOrElse("grid", "biomass") match
      case "biomass"  => Baselines ++ BiomassConfigs
      case "assembly" => AssemblyConfigs
      case "all"      => AssemblyConfigs ++ BiomassConfigs
      case "emergent" => emergentConfigs(list("rho").map(_.map(_.toDouble)).getOrElse(Seq(0.01, 0.03, 0.1)),
        list("rhoSigma").map(_.map(_.toDouble)).getOrElse(Seq(1.0)),
        list("basalBiomass").map(_.map(_.toDouble)).getOrElse(Seq(5.0e6)),
        list("te").map(_.map(_.toDouble)).getOrElse(Seq(0.1)),
        list("hrExp").map(_.map(_.toDouble)).getOrElse(Seq(0.5)),
        list("foraging").getOrElse(Seq("supply")))
      case other      => throw new IllegalArgumentException(s"unknown --grid '$other'")
    val assemblyOnly = opts.get("assemblyOnly").exists(_.toBoolean)
    val tileSize = opts.get("tileSize").map(_.toDouble).getOrElse(0.0)
    val CMeta = list("cMeta").map(_.map(_.toDouble)).getOrElse(Seq(0.05, 0.20))
    val Hr = list("hr").map(_.map(_.toDouble)).getOrElse(Seq(0.016, 0.025))
    val threads = opts.get("threads").map(_.toInt).getOrElse(Runtime.getRuntime.availableProcessors)
    val realErr = System.err
    System.setOut(new java.io.PrintStream((_: Int) => ()))
    System.setProperty("econetcons.skipRobustness", "true")

    val tasks = new ConcurrentLinkedQueue[(Int, Double, Double, Long)]()
    for ci <- Configs.indices; c <- CMeta; h <- Hr; s <- 1L to nSeeds.toLong do tasks.add((ci, c, h, s))
    val total = tasks.size
    realErr.println(s"[AssemblyDiagnostic] $total worlds, $threads threads -> $outPath")

    val header = Seq("config", "abundance", "cascade", "epsilon", "theta", "te", "n_pop", "placement", "c_meta", "hr", "seed",
        "total_biomass", "small_species", "small_species_dropped", "rho", "rho_sigma", "basal_biomass", "hr_exp", "foraging") ++
      Seq("placed", "placed_basal", "placed_inter", "placed_top") ++
      Seq("inweb", "inweb_basal", "inweb_inter", "inweb_top") ++
      Seq("assembled", "assembled_basal", "assembled_inter", "assembled_top") ++
      Seq("assembled_sp", "assembled_sp_basal", "assembled_sp_inter", "assembled_sp_top") ++
      Seq("spatial_filtering_pct", "energetic_pruning_pct", "assembly_seconds") ++ BiomassNames ++
      WebStructureSweep.MetricNames.map("stable_" + _) ++
      Seq("tiles", "tile_side_in_hr_diameters", "tiles_occupied_finest", "pops_per_occupied_tile_finest",
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
        val cfg = Configs(ci)
        val Config(label, am, cm, eps, theta, te, nPop, rho, rhoSigma, basalBiomass, hrExp, foraging) = cfg
        try
          def wp(tile: Int, biomass: Double) = WorldParameters(numberOfSpecies = 300, connectanceMetaWeb = c, nPopulations = 5000,
            medianHomeRange = h, avgPopulationsPerTile = tile, tileSizeInHomeRanges = if tile == 32 then tileSize else 0.0, epsilon = eps,
            extinctionProbabilityUnprotected = 0.5, bodySizeSigma = 1.0, meanPatchesPerSpecies = 5.0,
            abundanceModel = am, transferEfficiency = te, cascadeModel = cm, energyTolerance = theta,
            totalBiomass = biomass, individualsPerPopulation = nPop, tpBandwidth = tpBandwidth,
            rangeFraction = rho, rangeFractionSigma = rhoSigma, basalBiomass = basalBiomass, homeRangeExponent = hrExp,
            foragingRule = foraging)
          val biomassMode = am == "biomassPyramid"
          // biomassPyramid: totalBiomass chosen on this seed's metaweb so ~targetPops populations are placed
          val (biomass, nSmall, nDropped) =
            if !biomassMode then (Double.NaN, 0, 0)
            else
              val probe = wp(32, 1.0)
              val meta0 = World.buildMetaWebForTest(probe, new Random(s))
              val b = World.totalBiomassForPopulations(meta0, probe, targetPops)
              val alloc = World.biomassAllocation(meta0, probe.copy(totalBiomass = b))
              val small = alloc.values.filter(_.individuals < nPop)
              (b, small.size, small.count(_.nPopulations == 0))
          val (pops, meta) = World.buildPopsAndMetaForTest(wp(32, biomass), new Random(s))
          val raw = PopulationWebGenerator.generatePopulationWeb(pops, meta)
          // emergent keeps every basal point (World.apply adds the isolated ones too)
          if am == "emergent" then pops.iterator.filter(_.species.trophicLevel == 0).foreach(raw.addVertex)
          val cp0 = ConservationParameters(fractionProtected = 0.0, strategy = Some(ConservationStrategy.SpeciesRichness))
          val tw0 = System.nanoTime()
          val stableWorld = World(wp(32, biomass), cp0, new Random(s))
          val stable = stableWorld.populationWeb
          val assemblySec = (System.nanoTime() - tw0) / 1e9
          val fine =
            if assemblyOnly then Seq.empty
            else World(wp(10, biomass), cp0, new Random(s)).managementLandscape.managementAreas.filter(_.populations.nonEmpty)
          val m = if assemblyOnly then Map.empty[String, Double] else Observables.run(300, c, 5000, h, 32, 0.0, 0.2, s, strategies = Observables.paperStrategies,
            epsilon = eps, extinctionProbabilityUnprotected = 0.5, bodySizeSigma = 1.0, meanPatchesPerSpecies = 5.0,
            abundanceModel = am, transferEfficiency = te, cascadeModel = cm, energyTolerance = theta,
            totalBiomass = biomass, individualsPerPopulation = nPop, tpBandwidth = tpBandwidth,
            rangeFraction = rho, rangeFractionSigma = rhoSigma, basalBiomass = basalBiomass, homeRangeExponent = hrExp,
            tileSizeInHomeRanges = tileSize, foragingRule = foraging)
          val nPlaced = pops.size.toDouble
          val nInWeb = raw.vertexSet().size().toDouble
          val nAssembled = stable.vertexSet().size().toDouble
          val vals = groups(pops) ++ groups(raw.vertexSet().asScala) ++ groups(stable.vertexSet().asScala) ++
            speciesGroups(stable.vertexSet().asScala) ++
            Seq(100.0 * (1 - nInWeb / nPlaced), 100.0 * (1 - nAssembled / math.max(1.0, nInWeb)), assemblySec) ++
            biomassByLevel(stableWorld, World.trophicPositions(meta)) ++
            WebStructureSweep.metrics(stable) ++
            Seq(stableWorld.managementLandscape.managementAreas.size.toDouble,
              1.0 / math.sqrt(stableWorld.managementLandscape.managementAreas.size) / (2 * h)) ++
            Seq(fine.size.toDouble, fine.map(_.populations.size).sum.toDouble / math.max(1, fine.size)) ++
            Seq("SR_sp_ext_primary", "SR_sp_ext_secondary", "SR_sp_ext_total", "SR_nCascades",
              "IR_sp_ext_total", "IR_spint_ext_total", "SR_spint_ext_total").map(k => m.getOrElse(k, Double.NaN))
          val wp32 = wp(32, biomass)
          val row = Seq(label, am, cm, f"$eps%.1f", f"$theta%.2f", f"$te%.2f",
              if biomassMode then nPop.toString else "NA", wp32.resolvedPlacement, f"$c%.2f", f"$h%.3f", s.toString,
              f"$biomass%.6g", nSmall.toString, nDropped.toString,
              if am == "emergent" then f"$rho%.4g" else "NA", if am == "emergent" then f"$rhoSigma%.3g" else "NA",
              if am == "emergent" then f"$basalBiomass%.4g" else "NA", f"$hrExp%.2f", if am == "emergent" then foraging else "NA") ++
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
