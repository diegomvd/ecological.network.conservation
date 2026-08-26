package econetcons

import scala.util.Random
import org.jgrapht.*
import org.jgrapht.graph.*
import org.apache.commons.math3.distribution.BetaDistribution
import org.apache.commons.math3.distribution.LogNormalDistribution
import org.jgrapht.alg.connectivity.{ConnectivityInspector,KosarajuStrongConnectivityInspector}
import org.locationtech.jts.geom.{GeometryFactory,MultiPolygon,Coordinate}
import org.locationtech.jts.operation.distance.DistanceOp

import scala.annotation.tailrec
import scala.jdk.CollectionConverters.*
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.Polygon


case class DetailedExtinctions(
  finalWorld: World,
  nCascades: Int,
  phase0SpeciesLost: Int,
  phase0InteractionsLost: Int,
  primarySpeciesLost: Int,
  secondarySpeciesLost: Int,
  primaryInteractionsLost: Int,
  secondaryInteractionsLost: Int,
  initialSpeciesRichness: Int,
  initialInteractionsRichness: Int
)

case class World(
  populations: Seq[Population],
  managementLandscape: ManagementLandscape, 
  metaWeb: DefaultDirectedGraph[Species, DefaultEdge], 
  populationWeb: DefaultDirectedGraph[Population, DefaultEdge], 
  worldParameters: WorldParameters, 
  conservationParameters: ConservationParameters, 
  rnd: Random
):

  def getSpeciesRichness(): Int =
    this.populations.filter(!_.extinct).map(_.species.id).toSet.size

  def getInteractionsRichness(): Int =
    val speciesInteractions = scala.collection.mutable.Set[(Int, Int)]()

    this.populationWeb.vertexSet().asScala.foreach { predatorPop =>
      // Filter out extinct populations
      if (!predatorPop.extinct) {
        this.populationWeb.outgoingEdgesOf(predatorPop).asScala.foreach { edge =>
          val preyPop = this.populationWeb.getEdgeTarget(edge)
          // Only count interaction if prey is also not extinct
          if (!preyPop.extinct) {
            speciesInteractions += ((predatorPop.species.id, preyPop.species.id))
          }
        }
      }
    }

    speciesInteractions.size
  
  def primaryExtinctions(): World =
   
    val extinguishIds =
      this.managementLandscape.managementAreas.flatMap(a => a.primaryExtinctions(this.worldParameters, this.rnd))

    val extinguishVertices = populationWeb.vertexSet().asScala.collect {
      case p if extinguishIds.contains( (p.id,p.species.id) ) => p
    }

    val newPopulationWeb: Boolean = populationWeb.removeAllVertices(extinguishVertices.asJavaCollection)
     
    // If this is slow consider making populations a Map[Id,Pop]
    val newPopulations = populations.map( p => if extinguishIds.contains((p.id,p.species.id)) then p.extinguish else p )

    val extinctPopulations = newPopulations.collect { case p if p.extinct => (p.id,p.species.id) }
    
    val newManagementLandscape = this.managementLandscape.updatePersistentPopulations(extinctPopulations)

    this.copy(populations = newPopulations, populationWeb = populationWeb, managementLandscape = newManagementLandscape)


  def secondaryExtinctions(dietBreadthThreshold: Double = 0.0): (World, Int) =

    // Compute initial prey-species counts per population before any cascade round.
    // Only needed when threshold > 0; when 0 we use the classic outDegree==0 rule.
    val initialPreySpecies: Map[Int, Int] =
      if dietBreadthThreshold == 0.0 then Map.empty
      else
        this.populationWeb.vertexSet().asScala.map { p =>
          val preySpeciesCount = this.populationWeb.outgoingEdgesOf(p).asScala
            .map(e => this.populationWeb.getEdgeTarget(e).species.id)
            .toSet.size
          p.id -> preySpeciesCount
        }.toMap

    @tailrec
    def extinctionCascades(
      populationWeb: DefaultDirectedGraph[Population, DefaultEdge],
      populations: Seq[Population],
      nCascades: Int
    ): (DefaultDirectedGraph[Population,DefaultEdge], Seq[Population], Int) =

      val (extinguishVertices, extinguishIds) = populationWeb.vertexSet().asScala
        .collect {
          case p if p.species.trophicLevel > 0 && {
            if dietBreadthThreshold == 0.0 then
              populationWeb.outDegreeOf(p) == 0
            else
              val currentPreySpecies = populationWeb.outgoingEdgesOf(p).asScala
                .map(e => populationWeb.getEdgeTarget(e).species.id)
                .toSet.size
              val initial = initialPreySpecies.getOrElse(p.id, 0)
              initial > 0 && currentPreySpecies < initial * (1.0 - dietBreadthThreshold)
          } => (p, (p.id, p.species.id))
        }.unzip

      if extinguishIds.isEmpty then
        (populationWeb, populations, nCascades)
      else {
        println(s"  Cascade ${nCascades + 1}: Removing ${extinguishVertices.size} predators ...")

        val _ = populationWeb.removeAllVertices(extinguishVertices.asJavaCollection)

        val newPopulations = populations.map { p =>
          if extinguishIds.contains((p.id, p.species.id)) then p.extinguish else p
        }

        extinctionCascades(populationWeb, newPopulations, nCascades + 1)
      }

    val (newPopulationWeb, newPopulations, nCascades) = extinctionCascades(this.populationWeb, this.populations, 0)

    val extinctPopulations = newPopulations.collect{case p if p.extinct => (p.id, p.species.id) }
    val newManagementLandscape = this.managementLandscape.updatePersistentPopulations(extinctPopulations)

    (this.copy(populations = newPopulations, populationWeb = newPopulationWeb, managementLandscape = newManagementLandscape), nCascades)
    
  /**
   * Trophic cascade model based on load/supply stress (replaces secondaryExtinctions for Phase 2).
   *
   * Each node n has a stress score = max(stress_pred, stress_prey) where:
   *   load(q)        = Σ_{p ∈ predators(q)}  1 / outDegree(p)   [predation pressure on prey q]
   *   supply(p)      = Σ_{q ∈ prey(p)}       1 / load(q)        [effective prey capacity for predator p]
   *   stress_prey(n) = max(0,  load(n) * k - 1)                  [overexploitation, >0 when load(n) > 1/k]
   *   stress_pred(n) = max(0,  1 - supply(n) / k)                [starvation, >0 when supply(n) < k]
   *   stress(n)      = max(stress_prey(n), stress_pred(n))
   *
   * Each round the single most-stressed node is removed. Basal species (trophicLevel == 0)
   * have no predator-side stress; top predators have no prey-side stress. The loop runs until
   * no node has stress > 0.
   *
   * Phase 0 (outDegree == 0 cleanup) must be run before calling this method.
   */
  def trophicCascades(preyPerPredator: Double): (World, Int) =

    @tailrec
    def cascade(
      populationWeb: DefaultDirectedGraph[Population, DefaultEdge],
      populations: Seq[Population],
      nRounds: Int
    ): (DefaultDirectedGraph[Population, DefaultEdge], Seq[Population], Int) =

      val vertices = populationWeb.vertexSet().asScala.toSeq

      // Compute load for every node (predation pressure it receives as prey)
      val load: Map[Int, Double] = vertices.map { q =>
        val l = populationWeb.incomingEdgesOf(q).asScala.foldLeft(0.0) { (acc, e) =>
          val p = populationWeb.getEdgeSource(e)
          val od = populationWeb.outDegreeOf(p)
          acc + (if od > 0 then 1.0 / od else 0.0)
        }
        q.id -> l
      }.toMap

      // Compute supply for every node (effective prey capacity it receives as predator)
      val supply: Map[Int, Double] = vertices.map { p =>
        val s = populationWeb.outgoingEdgesOf(p).asScala.foldLeft(0.0) { (acc, e) =>
          val q = populationWeb.getEdgeTarget(e)
          val l = load.getOrElse(q.id, 0.0)
          acc + (if l > 0.0 then 1.0 / l else Double.MaxValue / vertices.size)
        }
        p.id -> s
      }.toMap

      // Compute stress for every node
      val stressed = vertices.flatMap { n =>
        val stressPrey =
          if load.getOrElse(n.id, 0.0) == 0.0 then 0.0
          else math.max(0.0, load(n.id) * preyPerPredator - 1.0)
        val stressPred =
          if n.species.trophicLevel == 0 then 0.0
          else math.max(0.0, 1.0 - supply.getOrElse(n.id, 0.0) / preyPerPredator)
        val stress = math.max(stressPrey, stressPred)
        if stress > 0.0 then Some((n, stress)) else None
      }

      if stressed.isEmpty then
        (populationWeb, populations, nRounds)
      else
        val (mostStressed, stressVal) = stressed.maxBy(_._2)
        println(s"  TrophicCascade round ${nRounds + 1}: removing ${mostStressed.species.id} (stress=${"%.3f".format(stressVal)})")
        populationWeb.removeVertex(mostStressed)
        val newPopulations = populations.map { p =>
          if p.id == mostStressed.id && p.species.id == mostStressed.species.id then p.extinguish else p
        }
        cascade(populationWeb, newPopulations, nRounds + 1)

    val (newPopulationWeb, newPopulations, nRounds) = cascade(this.populationWeb, this.populations, 0)
    val extinctPopulations = newPopulations.collect { case p if p.extinct => (p.id, p.species.id) }
    val newManagementLandscape = this.managementLandscape.updatePersistentPopulations(extinctPopulations)
    (this.copy(populations = newPopulations, populationWeb = newPopulationWeb, managementLandscape = newManagementLandscape), nRounds)

  // Dispatch helper: use trophicCascades if preyPerPredator > 0, else legacy secondaryExtinctions
  private def phase2Cascades(): (World, Int) =
    if this.worldParameters.preyPerPredator > 0.0 then
      this.trophicCascades(this.worldParameters.preyPerPredator)
    else
      this.secondaryExtinctions(this.worldParameters.dietBreadthThreshold)

  def extinctions(): (World,Int) =
    // Phase 0: stabilize, Phase 1: primary, Phase 2: cascade (same model for both cascade phases).
    this.phase2Cascades()._1.primaryExtinctions().phase2Cascades()

  def extinctionsDetailed(): DetailedExtinctions =
    val spInit = this.getSpeciesRichness()
    val intInit = this.getInteractionsRichness()

    // Phase 0: stabilize initial world (same cascade model as Phase 2)
    val (stableWorld, _) = this.phase2Cascades()

    val spBefore0 = stableWorld.getSpeciesRichness()
    val intBefore0 = stableWorld.getInteractionsRichness()

    val phase0SpeciesLost = spInit - spBefore0
    val phase0InteractionsLost = intInit - intBefore0

    // Phase 1: primary extinctions
    val afterPrimary = stableWorld.primaryExtinctions()
    val spAfterPrimary = afterPrimary.getSpeciesRichness()
    val intAfterPrimary = afterPrimary.getInteractionsRichness()

    val primarySpeciesLost = spBefore0 - spAfterPrimary
    val primaryInteractionsLost = intBefore0 - intAfterPrimary

    // Phase 2: trophic cascades (new model) or legacy secondary extinctions
    val (finalWorld, nCascades) = afterPrimary.phase2Cascades()
    val spFinal = finalWorld.getSpeciesRichness()
    val intFinal = finalWorld.getInteractionsRichness()

    val secondarySpeciesLost = spAfterPrimary - spFinal
    val secondaryInteractionsLost = intAfterPrimary - intFinal

    DetailedExtinctions(
      finalWorld = finalWorld,
      nCascades = nCascades,
      phase0SpeciesLost = phase0SpeciesLost,
      phase0InteractionsLost = phase0InteractionsLost,
      primarySpeciesLost = primarySpeciesLost,
      secondarySpeciesLost = secondarySpeciesLost,
      primaryInteractionsLost = primaryInteractionsLost,
      secondaryInteractionsLost = secondaryInteractionsLost,
      initialSpeciesRichness = spInit,
      initialInteractionsRichness = intInit
    )
    
  /**
   * Returns a structurally independent copy of this World for use in extinction simulations.
   *
   * WHY THIS EXISTS — THE SHARED-MUTABLE-STATE PROBLEM:
   * `World` is a Scala case class, so `.copy()` performs a shallow copy: it copies field
   * references, not the objects they point to. `populationWeb` is a mutable JGraphT
   * `DefaultDirectedGraph`, and `rnd` is a mutable `scala.util.Random`. If two strategy
   * worlds are derived from the same base world via `.copy()`, they share the exact same
   * live instances of both objects. When the first strategy runs extinctions —
   * `primaryExtinctions()` and `secondaryExtinctions()` both call
   * `populationWeb.removeAllVertices(...)` in place — the shared graph is permanently
   * depleted, corrupting the second strategy's extinction run entirely.
   *
   * This method breaks that sharing by:
   * 1. Deep-copying the `populationWeb` graph: new graph object, same vertices and edges.
   * 2. Replacing `rnd` with the caller-supplied fresh Random instance.
   *
   * The `ManagementLandscape.populationWeb` reference (used only for tile scoring during
   * protection planning, never mutated during extinction) is intentionally left shared —
   * it is read-only in that context and copying it would be wasteful.
   */
  def deepCopyForExtinction(newRnd: Random): World =
    val copiedGraph = new DefaultDirectedGraph[Population, DefaultEdge](classOf[DefaultEdge])
    this.populationWeb.vertexSet().asScala.foreach(copiedGraph.addVertex)
    this.populationWeb.edgeSet().asScala.foreach { e =>
      copiedGraph.addEdge(
        this.populationWeb.getEdgeSource(e),
        this.populationWeb.getEdgeTarget(e)
      )
    }
    this.copy(populationWeb = copiedGraph, rnd = newRnd)

  def getRealizedWeb: DefaultDirectedGraph[Species, DefaultEdge] =

    val realizedWeb = DefaultDirectedGraph[Species, DefaultEdge](classOf[DefaultEdge])

    this.populationWeb.vertexSet().asScala.foreach(
      p =>{
        populationWeb.outgoingEdgesOf(p).asScala.foreach(
          link => {
            val prey = populationWeb.getEdgeTarget(link)
            val addedPredator = realizedWeb.addVertex(p.species)
            val addedPrey = realizedWeb.addVertex(prey.species)
            val newInteraction = realizedWeb.addEdge(p.species, prey.species, DefaultEdge())
            newInteraction
          }
        )
      }
    )

    realizedWeb

object World:
  
  def apply(
    worldParameters: WorldParameters,
    conservationParameters: ConservationParameters,
    rnd: Random
  ): World =

    val metaWeb = generateMetaWeb(
      worldParameters.numberOfSpecies,
      worldParameters.connectanceMetaWeb,
      worldParameters.medianHomeRange,
      rnd
    )

    val pi = 3.1416
    val numberOfPopulations =
      (worldParameters.areaOverlap / (pi*worldParameters.medianHomeRange * worldParameters.medianHomeRange )).toInt

    val nTiles = (numberOfPopulations.toDouble / worldParameters.avgPopulationsPerTile.toDouble).toInt

    val landscapeGrid = SquareGrid.fromArea(nTiles)

    // Pre-generate shared hotspot centers on a regular sub-lattice (used by all species)
    val hotspotCenters: Seq[Coordinate] =
      if worldParameters.aggregationStrength > 0.0 && worldParameters.numHotspots >= 1 then
        val side = math.round(math.sqrt(worldParameters.numHotspots.toDouble)).toInt
        val spacing = 1.0 / side
        for
          row <- 0 until side
          col <- 0 until side
        yield new Coordinate((col + 0.5) * spacing, (row + 0.5) * spacing, 0.0)
      else
        Seq.empty

   // println("Generating populations.")
    val populations = generatePopulations(metaWeb, numberOfPopulations, rnd, worldParameters, hotspotCenters)

    //println("Generating interaction network.")
    val populationWeb = PopulationWebGenerator.generatePopulationWeb(populations, metaWeb)

    // TODO: check if this is true  
    // Update populations given that the non interacting ones are removed from the network
    val populationsInit = populationWeb.vertexSet().asScala.toSeq
    println("npops: " + populationsInit.size)


    //println("Creating management landscape.")
    val trueLandscape = ManagementLandscape(
      landscapeGrid,
      populationsInit,
      populationWeb,
      rnd
    )
    val hasNoise = worldParameters.observationNoise > 0.0 || worldParameters.falsePositiveRate > 0.0
    val planningLandscape =
      if hasNoise then
        val allSpeciesIds = metaWeb.vertexSet().asScala.map(_.id).toSeq
        trueLandscape.perturbedCopy(worldParameters.observationNoise, worldParameters.falsePositiveRate, allSpeciesIds, rnd)
      else
        trueLandscape
    val selectedLandscape = planningLandscape.applyProtectionPlan(conservationParameters, rnd)
    val managementLandscape =
      if hasNoise then
        trueLandscape.applyProtectionStatusFrom(selectedLandscape)
      else
        selectedLandscape
    //println("Initializing world.")
    val unstableWorld = new World(populationsInit, managementLandscape, metaWeb, populationWeb, worldParameters, conservationParameters, rnd)

    val (stableWorld,nCascades) = unstableWorld.phase2Cascades()

    //println("cascades: " + nCascades)
    //println("n pops 2:" + stableWorld.populations.count(!_.extinct))

    stableWorld


  private def generateMetaWeb(numberOfSpecies: Int, connectance: Double, medianHomeRange: Double, rnd: Random):
  DefaultDirectedGraph[Species, DefaultEdge]  =

    @tailrec
    def rec(): DefaultDirectedGraph[(Int, Double), DefaultEdge] =

      // Body sizes are drawn from lognormal distribution where close to 90% of species fall within 4 orders of magnitude
      val bodySizes = Seq.fill(numberOfSpecies)(LogNormalDistribution(0,3).sample()).sorted

      // Niche values are scaled between 0 and 1 from body sizes
      val nicheValues = bodySizes.map(m => (m-bodySizes.min)/(bodySizes.max-bodySizes.min))

      val nicheValuesMax = nicheValues.max
    
      // val localDensities = nicheValues.zipWithIndex.map { (n, i) =>
      //  val neighbors = nicheValues.filter(x => math.abs(x - n) < 0.1)
      //   neighbors.size / 0.2  // Density in window around niche
      // }

      // Feeding range from beta distribution. The species with the lowest niche value is set to be a basal species: feedingRange = 0
      val beta = (0.5 / connectance - 1) // how to explain this?? 
      
      // val feedingRange = nicheValues.zip(localDensities).zipWithIndex.map { 
      //   case ((n, density), i) =>
      //     if (i > 0) {
      //       val baseRange = n * BetaDistribution(1, beta).sample()
      //       baseRange / density  // Scale by local density
      //     } else 0.0
      // }

      val feedingRange =
        nicheValues.zipWithIndex.map(
          (n, i) => if i > 0 then n * BetaDistribution(1, beta).sample() else 0.0
        )

      // val feedingRange = nicheValues.zipWithIndex.map { (n, i) =>
      //   if (i > 0) {
      //     val rawRange = n * BetaDistribution(1, adjustedBeta).sample()
          
      //     // Calculate maximum feasible range given feeding center constraints
      //     // For the feeding center calculation to work: 0.5*r ≤ min(n, 1-0.5*r)
      //     // This gives us: r ≤ min(2n, 1.0)
      //     val maxFeasibleRange = math.min(2 * n, 1.0)*0.99
          
      //     // Apply ecological constraint
      //     math.min(rawRange, maxFeasibleRange)
      //   } else {
      //     0.0  // Basal species
      //   }
      // }

      // Feeding center is drawn from uniform distribution between the feeding range.
      val feedingCenter =
        feedingRange.zip(nicheValues).map(
          (r, n) => {
            if (r == 0.0) 0.0  // No feeding range means no meaningful feeding center
            else rnd.between(0.5 * r, math.min(n, 1 - 0.5 * r))
          }
        )

      // Instantiate an empty MetaWeb
      val metaWeb = DefaultDirectedGraph[(Int, Double), DefaultEdge](classOf[DefaultEdge])

      // Populate the meta web: if a species S1 is within the feeding range of a species S2 then S2 is a predator of S1.
      val x = nicheValues.zipWithIndex.map {
        (n, i) =>
          feedingCenter.zip(feedingRange).zipWithIndex.map {
            (f, j) =>
              if n > f._1 - 0.5 * f._2 && n < f._1 + 0.5 * f._2 && i != j
              then {
                val addedPredator = metaWeb.addVertex((j,bodySizes.lift(j).getOrElse(0.0)))
                val addedPrey = metaWeb.addVertex((i,bodySizes.lift(i).getOrElse(0.0)))
                val newInteraction = metaWeb.addEdge((j,bodySizes.lift(j).getOrElse(0.0)), (i,bodySizes.lift(i).getOrElse(0.0)), DefaultEdge())
              }
          }
      }

      val isConnected = ConnectivityInspector(metaWeb).isConnected

      if !isConnected then rec() else {

        val cyclesAll = KosarajuStrongConnectivityInspector(metaWeb).stronglyConnectedSets()

        if cyclesAll.size() < numberOfSpecies then {

          val cycles = cyclesAll.asScala.toSeq.collect { case c if c.size() > 1 => c.asScala }

          val externalEnergyCycles = cycles.forall(
            cycle => cycle.exists {
              species => {
                val preySet = metaWeb.outgoingEdgesOf(species).asScala.map(link => metaWeb.getEdgeTarget(link)).toSet
                preySet.exists(!cycle.contains(_)) // Checks if there are preys outside the cycle
                //species.intersect(preySet).size > species.size + 1
              }
            }
          )

          if !externalEnergyCycles then rec() else {

            val functionallyDifferentSet = metaWeb.vertexSet().asScala.map(
                id => {
                  val predators = metaWeb.incomingEdgesOf(id).asScala.map(link => metaWeb.getEdgeSource(link)).toSet
                  val preys = metaWeb.outgoingEdgesOf(id).asScala.map(link => metaWeb.getEdgeTarget(link)).toSet
                  (predators, preys)
                }
              ).toSet
            
            val nFunctionallyDifferent = functionallyDifferentSet.size

            // Allow from 5% of species to be functionally equal
            if nFunctionallyDifferent > numberOfSpecies*0.95 then metaWeb else rec()
          }

        } else {
          val functionallyDifferentSet = metaWeb.vertexSet().asScala.map(
                id => {
                  val predators = metaWeb.incomingEdgesOf(id).asScala.map(link => metaWeb.getEdgeSource(link)).toSet
                  val preys = metaWeb.outgoingEdgesOf(id).asScala.map(link => metaWeb.getEdgeTarget(link)).toSet
                  (predators, preys)
                }
              ).toSet
            
            val nFunctionallyDifferent = functionallyDifferentSet.size

            // Allow from 5% of species to be functionally equal
            if nFunctionallyDifferent > numberOfSpecies*0.95 then metaWeb else rec()
        }
      }

    val rawMetaWeb = rec()

    // First pass: create species with raw (relative) homeRange values
    val rawSpecies = rawMetaWeb.vertexSet().asScala.toSeq.map { vertex =>
      val trophicLevel = Species.calculateTrophicLevel(vertex, rawMetaWeb)
      Species(vertex._1, vertex._2, trophicLevel, rnd)
    }

    // HOME RANGE SCALING - convert from relative to absolute values
    val relativeHomeRanges = rawSpecies.map(_.homeRange).sorted
    val currentMedianHomeRange = relativeHomeRanges(relativeHomeRanges.size / 2)
    val scalingFactor = medianHomeRange / currentMedianHomeRange
    
    // println(f"=== HOME RANGE SCALING ===")
    // println(f"Current median relative home range: ${currentMedianHomeRange}%.4f")
    // println(f"Target median home range (radius): ${medianHomeRange}%.4f") 
    // println(f"Scaling factor: ${scalingFactor}%.6f")
    // println(f"Raw home range range: ${relativeHomeRanges.head}%.4f - ${relativeHomeRanges.last}%.4f")
    
    val scaledSpecies = rawSpecies.map { species =>
      species.copy(homeRange = species.homeRange * scalingFactor)
    }
    
    val finalHomeRanges = scaledSpecies.map(_.homeRange).sorted
    // println(f"Scaled home range range: ${finalHomeRanges.head}%.6f - ${finalHomeRanges.last}%.6f")
    // println(f"Final median home range: ${finalHomeRanges(finalHomeRanges.size / 2)}%.6f")

    // Second pass: build the final metaweb with properly scaled species
    val metaWebDef = DefaultDirectedGraph[Species, DefaultEdge](classOf[DefaultEdge])

    rawMetaWeb.vertexSet().asScala.foreach { predatorVertex =>
      val predatorSpecies = scaledSpecies.find(_.id == predatorVertex._1).get
      val addedPredator = metaWebDef.addVertex(predatorSpecies)

      rawMetaWeb.outgoingEdgesOf(predatorVertex).asScala.toSeq.foreach { link =>
        val preyVertex = rawMetaWeb.getEdgeTarget(link)
        val preySpecies = scaledSpecies.find(_.id == preyVertex._1).get
        val addedPrey = metaWebDef.addVertex(preySpecies)
        val newInteraction = metaWebDef.addEdge(predatorSpecies, preySpecies, DefaultEdge())
      }
    }

    val finalConnectance = metaWebDef.edgeSet().size().toDouble / (metaWebDef.vertexSet().size() * (metaWebDef.vertexSet().size() - 1))
    println(f"Generated metaweb connectance: ${finalConnectance}%.4f (target: ${connectance}%.4f)")
    
    metaWebDef

  private def generatePopulations(
    metaWeb: DefaultDirectedGraph[Species, DefaultEdge],
    numberOfPopulations: Int,
    rnd: Random,
    worldParameters: WorldParameters,
    hotspotCenters: Seq[Coordinate] = Seq.empty
  ): Seq[Population] = {
    val abundancesSum: Double = metaWeb.vertexSet().asScala.toSeq.map(s => s.abundance).sum
    val conversionFactor: Double = numberOfPopulations / abundancesSum

    // Generate populations for each species using Thomas process
    metaWeb.vertexSet().asScala.toSeq
      .foldLeft((Seq.empty[Population], 0)) { case ((allPopulations, currentId), species) =>

        // Calculate number of populations for this species
        val speciesAbundance = math.max(1, (species.abundance * conversionFactor).toInt)

        // Create template populations with placeholder coordinates
        val templatePopulations = (0 until speciesAbundance).map { i =>
          Population(
            id = currentId + i,
            species = species,
            coordinates = new org.locationtech.jts.geom.Coordinate(0.0, 0.0, 0.0), // placeholder
            extinct = false
          )
        }

        // Use Thomas process to generate spatial coordinates for all populations of this species
        val spatiallyArrangedPopulations = Population.createPopulations(
          species              = species,
          populationSeq        = templatePopulations,
          rnd                  = rnd,
          worldParameters      = worldParameters,
          sharedHotspotCenters = hotspotCenters
        )

        (allPopulations ++ spatiallyArrangedPopulations, currentId + speciesAbundance)
      }._1 // Return only the populations
  }
    
end World
    

