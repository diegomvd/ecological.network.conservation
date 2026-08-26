package econetcons

import scala.util.Random

/**
 * Compare conservation outcomes with different biodiversity hotspot intensities
 * Tests: No hotspot, Tight hotspot, Medium hotspot, Sparse hotspot
 * Conservation strategy: SpeciesRichness (purely species-driven)
 */
object HotspotComparison extends App:

  case class SimulationResult(
    scenario: String,
    hotspotVariance: Option[Double],
    initialSpecies: Int,
    finalSpecies: Int,
    speciesExtinctionRate: Double,
    initialInteractions: Int,
    finalInteractions: Int,
    interactionExtinctionRate: Double,
    cascades: Int,
    runtime: Long
  )

  def runSimulation(
    scenario: String,
    useHotspot: Boolean,
    hotspotVariance: Double,
    seed: Long
  ): SimulationResult = {

    val startTime = System.currentTimeMillis()

    // World parameters with hotspot configuration
    val worldParameters = WorldParameters(
      numberOfSpecies = 200,
      connectanceMetaWeb = 0.4,
      areaOverlap = 20.0,
      medianHomeRange = 0.05,
      avgPopulationsPerTile = 5,
      useHotspot = useHotspot,
      hotspotCenterX = 0.5,
      hotspotCenterY = 0.5,
      hotspotVariance = hotspotVariance,
      useThinning = false,       // No thinning - allow populations to overlap
      spreadingFactor = 1.0      // Balance between clustering and spread
    )

    // Conservation: purely species richness driven
    val conservationParameters = ConservationParameters(
      fractionProtected = 0.3,
      strategy = Some(ConservationStrategy.SpeciesRichness)
    )

    // Initialize and run simulation
    val initialWorld = World(
      worldParameters = worldParameters,
      conservationParameters = conservationParameters,
      rnd = Random(seed)
    )

    val initialSR = initialWorld.getSpeciesRichness()
    val initialIR = initialWorld.getInteractionsRichness()

    val (finalWorld, nCascades) = initialWorld.extinctions()

    val finalSR = finalWorld.getSpeciesRichness()
    val finalIR = finalWorld.getInteractionsRichness()

    val endTime = System.currentTimeMillis()
    val runtime = endTime - startTime

    val speciesExtinctionRate = (initialSR - finalSR).toDouble / initialSR * 100.0
    val interactionExtinctionRate = (initialIR - finalIR).toDouble / initialIR * 100.0

    SimulationResult(
      scenario = scenario,
      hotspotVariance = if (useHotspot) Some(hotspotVariance) else None,
      initialSpecies = initialSR,
      finalSpecies = finalSR,
      speciesExtinctionRate = speciesExtinctionRate,
      initialInteractions = initialIR,
      finalInteractions = finalIR,
      interactionExtinctionRate = interactionExtinctionRate,
      cascades = nCascades,
      runtime = runtime
    )
  }

  println("\n" + "=" * 80)
  println("BIODIVERSITY HOTSPOT EFFECT ON CONSERVATION OUTCOMES")
  println("Conservation Strategy: Species Richness (30% protected)")
  println("=" * 80)

  // Run simulations with different hotspot configurations
  val seed = 42L

  println("\nRunning simulations...")

  val noHotspot = runSimulation(
    "No Hotspot (uniform)",
    useHotspot = false,
    hotspotVariance = 0.0,
    seed
  )
  println(s"  ✓ No hotspot completed (${noHotspot.runtime}ms)")

  val tightHotspot = runSimulation(
    "Tight Hotspot",
    useHotspot = true,
    hotspotVariance = 0.01,  // Very tight clustering
    seed
  )
  println(s"  ✓ Tight hotspot completed (${tightHotspot.runtime}ms)")

  val mediumHotspot = runSimulation(
    "Medium Hotspot",
    useHotspot = true,
    hotspotVariance = 0.05,  // Moderate clustering
    seed
  )
  println(s"  ✓ Medium hotspot completed (${mediumHotspot.runtime}ms)")

  val sparseHotspot = runSimulation(
    "Sparse Hotspot",
    useHotspot = true,
    hotspotVariance = 0.15,  // Weak clustering
    seed
  )
  println(s"  ✓ Sparse hotspot completed (${sparseHotspot.runtime}ms)")

  // Display results
  val results = Seq(noHotspot, tightHotspot, mediumHotspot, sparseHotspot)

  println("\n" + "=" * 80)
  println("RESULTS")
  println("=" * 80)

  println(f"\n${"Scenario"}%-25s ${"Variance"}%-12s ${"Initial Sp"}%-12s ${"Final Sp"}%-12s ${"Sp Extinct%"}%-15s ${"Int Extinct%"}%-15s ${"Cascades"}%-10s")
  println("-" * 110)

  results.foreach { r =>
    val varianceStr = r.hotspotVariance.map(v => f"$v%.2f").getOrElse("N/A")
    println(f"${r.scenario}%-25s ${varianceStr}%-12s ${r.initialSpecies}%-12d ${r.finalSpecies}%-12d ${r.speciesExtinctionRate}%-15.1f ${r.interactionExtinctionRate}%-15.1f ${r.cascades}%-10d")
  }

  println("\n" + "=" * 80)
  println("ANALYSIS")
  println("=" * 80)

  val baselineExtinction = noHotspot.speciesExtinctionRate

  println(f"\nSpecies Extinction Rate Comparison (relative to no hotspot):")
  results.foreach { r =>
    val relative = r.speciesExtinctionRate - baselineExtinction
    val direction = if (relative > 0) "worse" else if (relative < 0) "better" else "same"
    println(f"  ${r.scenario}%-25s: ${r.speciesExtinctionRate}%6.1f%% (${relative}%+.1f%% - $direction)")
  }

  println(f"\nInteraction Extinction Rate Comparison (relative to no hotspot):")
  val baselineIntExtinction = noHotspot.interactionExtinctionRate
  results.foreach { r =>
    val relative = r.interactionExtinctionRate - baselineIntExtinction
    val direction = if (relative > 0) "worse" else if (relative < 0) "better" else "same"
    println(f"  ${r.scenario}%-25s: ${r.interactionExtinctionRate}%6.1f%% (${relative}%+.1f%% - $direction)")
  }

  println("\n" + "=" * 80)
  println("INTERPRETATION")
  println("=" * 80)

  println("\nBiodiversity hotspots affect conservation effectiveness because:")
  println("  • Tighter hotspots = species more spatially concentrated")
  println("  • Species richness-based conservation prioritizes high-diversity areas")
  println("  • With hotspots, protection may be more/less efficient depending on overlap")
  println("  • Spatial clustering affects how well 30% protection covers biodiversity")

  println("\n" + "=" * 80)

end HotspotComparison
