package econetcons

import scala.util.Random

object SpotCheck extends App:

  val seeds                            = Seq(42, 123, 999)
  val fractionProtected                = 0.3
  val avgPopulationsPerTile            = 5
  val extinctionProbabilityUnprotected = 0.15
  val preyPerPredatorValues            = Seq(0.2, 0.5, 1.0)
  val strategies = Seq(
    ConservationStrategy.Random,
    ConservationStrategy.SpeciesRichness,
    ConservationStrategy.InteractionRichness,
    ConservationStrategy.EgocentricInteractionRichness,
    ConservationStrategy.Connectivity,
    ConservationStrategy.ConnectivityInteraction,
    ConservationStrategy.SpeciesComplementarity,
    ConservationStrategy.EgocentricInteractionComplementarity
  )
  // Blended strategies
  val blendConnSp = (fp: Double) => ConservationParameters(
    fractionProtected        = fp,
    strategy                 = None,
    strategyConnectivity     = 0.5,
    strategySpeciesRichness  = 0.5
  )
  val blendSpInt = (fp: Double) => ConservationParameters(
    fractionProtected           = fp,
    strategy                    = None,
    strategySpeciesRichness     = 0.5,
    strategyInteractionRichness = 0.5
  )

  for preyPerPredator <- preyPerPredatorValues do

    println("\n" + "=" * 110)
    println(f"  preyPerPredator=$preyPerPredator, fractionProtected=$fractionProtected, p_unprotected=$extinctionProbabilityUnprotected")
    println("=" * 110)
    println(f"  ${"seed"}%6s  ${"strategy"}%20s  ${"sp_ext%"}%8s  ${"int_ext%"}%8s  ${"ph0_sp"}%7s  ${"prim_sp"}%7s  ${"sec_sp"}%7s  ${"cascades"}%8s  sanity")
    println("-" * 110)

    for myseed <- seeds do

      val worldParameters = WorldParameters(
        numberOfSpecies                  = 200,
        connectanceMetaWeb               = 0.4,
        areaOverlap                      = 20.0,
        medianHomeRange                  = 0.05,
        avgPopulationsPerTile            = avgPopulationsPerTile,
        useThinning                      = false,
        spreadingFactor                  = 0.5,
        useHotspot                       = false,
        extinctionProbabilityUnprotected = extinctionProbabilityUnprotected,
        preyPerPredator                  = preyPerPredator,
        observationNoise                 = 0.0,
        falsePositiveRate                = 0.0
      )

      println(s"\n--- seed=$myseed ---")
      val baseWorld = World(
        worldParameters        = worldParameters,
        conservationParameters = ConservationParameters(fractionProtected = 0.0, strategy = Some(ConservationStrategy.SpeciesRichness)),
        rnd                    = Random(myseed)
      )

      // Build all strategy worlds; snapshot SR/IR BEFORE extinctions mutate the graphs
      val strategyWorlds = strategies.map { strat =>
        val params = ConservationParameters(fractionProtected = fractionProtected, strategy = Some(strat))
        val world = baseWorld.copy(
          managementLandscape    = baseWorld.managementLandscape.applyProtectionPlan(params, Random(myseed)),
          conservationParameters = params
        ).deepCopyForExtinction(Random(myseed))
        (strat.toString, world, world.getSpeciesRichness(), world.getInteractionsRichness())
      }

      // SR/IR tile correlation from the base world (all tiles unprotected, clean landscape scores)
      val areas  = baseWorld.managementLandscape.managementAreas
      val popWeb = baseWorld.populationWeb
      val ids    = areas.map(_.id).sorted
      val srVals = ids.map(id => areas.find(_.id == id).get.getSpeciesRichness.toDouble)
      val irVals = ids.map(id => areas.find(_.id == id).get.getInteractionRichness(popWeb).toDouble)
      val srIrPearson  = DiagnosticMetrics.pearsonPublic(srVals, irVals)
      val srIrSpearman = DiagnosticMetrics.spearmanPublic(srVals, irVals)
      println(f"  Landscape SR/IR tile correlation — Pearson=$srIrPearson%.3f  Spearman=$srIrSpearman%.3f")

      val blendCSParams = blendConnSp(fractionProtected)
      val blendCSWorld = baseWorld.copy(
        managementLandscape    = baseWorld.managementLandscape.applyProtectionPlan(blendCSParams, Random(myseed)),
        conservationParameters = blendCSParams
      ).deepCopyForExtinction(Random(myseed))

      val blendSIParams = blendSpInt(fractionProtected)
      val blendSIWorld = baseWorld.copy(
        managementLandscape    = baseWorld.managementLandscape.applyProtectionPlan(blendSIParams, Random(myseed)),
        conservationParameters = blendSIParams
      ).deepCopyForExtinction(Random(myseed))

      val allWorlds = strategyWorlds
        :+ ("Conn+SR(0.5/0.5)",  blendCSWorld, blendCSWorld.getSpeciesRichness(), blendCSWorld.getInteractionsRichness())
        :+ ("SR+Int(0.5/0.5)",   blendSIWorld, blendSIWorld.getSpeciesRichness(), blendSIWorld.getInteractionsRichness())

      // Identity check
      val initSRs = allWorlds.map(_._3)
      val initIRs = allWorlds.map(_._4)
      println(s"  Identity SR: ${initSRs.mkString("/")} ${if initSRs.forall(_ == initSRs.head) then "OK" else "FAIL"}")
      println(s"  Identity IR: ${initIRs.mkString("/")} ${if initIRs.forall(_ == initIRs.head) then "OK" else "FAIL"}")

      // Protected-set similarity: pairwise Jaccard
      val protectedSets: Map[String, Set[Int]] = allWorlds.map { (name, world, _, _) =>
        name -> world.managementLandscape.managementAreas
          .collect { case a if a.status == ProtectionStatus.Protected => a.id }.toSet
      }.toMap
      val names = allWorlds.map(_._1)

      println("  Protected-set Jaccard:")
      for
        i <- names.indices
        j <- (i + 1) until names.size
      do
        val a = names(i); val b = names(j)
        val inter = protectedSets(a).intersect(protectedSets(b)).size.toDouble
        val union  = protectedSets(a).union(protectedSets(b)).size.toDouble
        val jval   = if union == 0.0 then 1.0 else inter / union
        println(f"    $a%22s vs $b%-22s  Jaccard=$jval%.3f")

      // Run extinctions and print results
      println()
      allWorlds.foreach { (name, world, initSR, initIR) =>
        val result  = world.extinctionsDetailed()
        val finalSR = result.finalWorld.getSpeciesRichness()
        val finalIR = result.finalWorld.getInteractionsRichness()
        val spExt   = if initSR > 0 then (initSR - finalSR).toDouble / initSR * 100.0 else 0.0
        val intExt  = if initIR > 0 then (initIR - finalIR).toDouble / initIR * 100.0 else 0.0
        val total   = initSR - finalSR
        val sanity  = result.primarySpeciesLost + result.secondarySpeciesLost
        val sanityStr = if sanity == total then "OK" else s"FAIL($sanity!=$total)"
        println(f"  $myseed%6d  $name%22s  $spExt%8.1f  $intExt%8.1f  ${result.phase0SpeciesLost}%7d  ${result.primarySpeciesLost}%7d  ${result.secondarySpeciesLost}%7d  ${result.nCascades}%8d  $sanityStr")
      }

  println("\n" + "=" * 110)

  // ── Observation noise sweep ───────────────────────────────────────────────
  // Three panels: false negatives only, false positives only, both combined.
  // SpeciesRichness and InteractionRichness strategies only.
  // Each row rebuilds the base world with the given noise so planning uses the
  // perturbed view; extinctions always run on the true landscape.

  val noiseValues  = Seq(0.0, 0.1, 0.2, 0.3, 0.5)
  val noiseSeed    = 42
  val noisePPP     = 0.5
  val noiseStrats  = Seq(ConservationStrategy.SpeciesRichness, ConservationStrategy.InteractionRichness, ConservationStrategy.EgocentricInteractionRichness)

  def runNoiseSweepRow(fn: Double, fp: Double, strat: ConservationStrategy): (Double, Double, String) =
    val wp = WorldParameters(
      numberOfSpecies                  = 200,
      connectanceMetaWeb               = 0.4,
      areaOverlap                      = 20.0,
      medianHomeRange                  = 0.05,
      avgPopulationsPerTile            = avgPopulationsPerTile,
      useThinning                      = false,
      spreadingFactor                  = 0.5,
      useHotspot                       = false,
      extinctionProbabilityUnprotected = extinctionProbabilityUnprotected,
      preyPerPredator                  = noisePPP,
      observationNoise                 = fn,
      falsePositiveRate                = fp
    )
    val baseWorld = World(
      worldParameters        = wp,
      conservationParameters = ConservationParameters(fractionProtected = 0.0, strategy = Some(ConservationStrategy.SpeciesRichness)),
      rnd                    = Random(noiseSeed)
    )
    val params = ConservationParameters(fractionProtected = fractionProtected, strategy = Some(strat))
    val w = baseWorld.copy(
      managementLandscape    = baseWorld.managementLandscape.applyProtectionPlan(params, Random(noiseSeed)),
      conservationParameters = params
    ).deepCopyForExtinction(Random(noiseSeed))
    val initSR  = w.getSpeciesRichness()
    val initIR  = w.getInteractionsRichness()
    val res     = w.extinctionsDetailed()
    val finalSR = res.finalWorld.getSpeciesRichness()
    val finalIR = res.finalWorld.getInteractionsRichness()
    val spExt   = if initSR > 0 then (initSR - finalSR).toDouble / initSR * 100.0 else 0.0
    val intExt  = if initIR > 0 then (initIR - finalIR).toDouble / initIR * 100.0 else 0.0
    val sanity  = res.primarySpeciesLost + res.secondarySpeciesLost
    val sanityStr = if sanity == initSR - finalSR then "OK" else s"FAIL($sanity!=${initSR - finalSR})"
    (spExt, intExt, sanityStr)

  val W = 100
  println("\n\n" + "=" * W)
  println(f"  OBSERVATION NOISE SWEEP — seed=$noiseSeed, preyPerPredator=$noisePPP, fractionProtected=$fractionProtected, p_unprotected=$extinctionProbabilityUnprotected")
  println("=" * W)
  println(f"  ${"rate"}%6s  ${"strategy"}%20s  ${"FN only sp%"}%12s  ${"FN only int%"}%13s  ${"FP only sp%"}%12s  ${"FP only int%"}%13s  ${"FN+FP sp%"}%10s  ${"FN+FP int%"}%11s")
  println("-" * W)

  for rate <- noiseValues do
    println()
    for strat <- noiseStrats do
      val (fnSp, fnInt, fnSanity)     = runNoiseSweepRow(rate, 0.0,  strat)
      val (fpSp, fpInt, fpSanity)     = runNoiseSweepRow(0.0,  rate, strat)
      val (bothSp, bothInt, bothSanity) = runNoiseSweepRow(rate, rate, strat)
      val sanityAll = Seq(fnSanity, fpSanity, bothSanity).mkString("/")
      println(f"  $rate%6.2f  $strat%20s  $fnSp%12.1f  $fnInt%13.1f  $fpSp%12.1f  $fpInt%13.1f  $bothSp%10.1f  $bothInt%11.1f  $sanityAll")

  println("\n" + "=" * W)

  // ── Spatial aggregation sweep ─────────────────────────────────────────────
  // Vary numHotspots and aggregationStrength; compare SpeciesRichness vs
  // SpeciesComplementarity: Jaccard(protected sets) expected to decrease as
  // aggregation increases (complementarity finds more dispersed tiles).

  val aggSeed     = 42
  val aggPPP      = 0.5
  val aggHotspots = Seq(1, 4, 9)
  val aggStrengths = Seq(0.0, 0.3, 0.7, 1.0)
  val aggStratSR  = ConservationStrategy.SpeciesRichness
  val aggStratSC  = ConservationStrategy.SpeciesComplementarity

  println("\n\n" + "=" * W)
  println(f"  SPATIAL AGGREGATION SWEEP — seed=$aggSeed, preyPerPredator=$aggPPP, fractionProtected=$fractionProtected, p_unprotected=$extinctionProbabilityUnprotected")
  println("=" * W)
  println(f"  ${"hotspots"}%8s  ${"strength"}%9s  ${"sp_ext_SR%"}%11s  ${"sp_ext_SC%"}%11s  ${"Jaccard(SR,SC)"}%14s  sanity")
  println("-" * W)

  for
    numH <- aggHotspots
    agg  <- aggStrengths
  do
    val wp = WorldParameters(
      numberOfSpecies                  = 200,
      connectanceMetaWeb               = 0.4,
      areaOverlap                      = 20.0,
      medianHomeRange                  = 0.05,
      avgPopulationsPerTile            = avgPopulationsPerTile,
      useThinning                      = false,
      spreadingFactor                  = 0.5,
      extinctionProbabilityUnprotected = extinctionProbabilityUnprotected,
      preyPerPredator                  = aggPPP,
      numHotspots                      = numH,
      hotspotVariance                  = 0.02,
      aggregationStrength              = agg
    )
    val baseW = World(
      worldParameters        = wp,
      conservationParameters = ConservationParameters(fractionProtected = 0.0, strategy = Some(aggStratSR)),
      rnd                    = Random(aggSeed)
    )

    def buildWorld(strat: ConservationStrategy): World =
      val params = ConservationParameters(fractionProtected = fractionProtected, strategy = Some(strat))
      baseW.copy(
        managementLandscape    = baseW.managementLandscape.applyProtectionPlan(params, Random(aggSeed)),
        conservationParameters = params
      ).deepCopyForExtinction(Random(aggSeed))

    val srWorld = buildWorld(aggStratSR)
    val scWorld = buildWorld(aggStratSC)

    val initSR_sr = srWorld.getSpeciesRichness()
    val initSR_sc = scWorld.getSpeciesRichness()

    val resSR = srWorld.extinctionsDetailed()
    val resSC = scWorld.extinctionsDetailed()

    val spExtSR = if initSR_sr > 0 then (initSR_sr - resSR.finalWorld.getSpeciesRichness()).toDouble / initSR_sr * 100.0 else 0.0
    val spExtSC = if initSR_sc > 0 then (initSR_sc - resSC.finalWorld.getSpeciesRichness()).toDouble / initSR_sc * 100.0 else 0.0

    val protSR: Set[Int] = srWorld.managementLandscape.managementAreas
      .collect { case a if a.status == ProtectionStatus.Protected => a.id }.toSet
    val protSC: Set[Int] = scWorld.managementLandscape.managementAreas
      .collect { case a if a.status == ProtectionStatus.Protected => a.id }.toSet
    val inter  = protSR.intersect(protSC).size.toDouble
    val union  = protSR.union(protSC).size.toDouble
    val jaccard = if union == 0.0 then 1.0 else inter / union

    val sanitySR = resSR.primarySpeciesLost + resSR.secondarySpeciesLost == initSR_sr - resSR.finalWorld.getSpeciesRichness()
    val sanitySC = resSC.primarySpeciesLost + resSC.secondarySpeciesLost == initSR_sc - resSC.finalWorld.getSpeciesRichness()
    val sanityStr = if sanitySR && sanitySC then "OK" else "FAIL"

    println(f"  $numH%8d  $agg%9.2f  $spExtSR%11.1f  $spExtSC%11.1f  $jaccard%14.3f  $sanityStr")

  println("\n" + "=" * W)
