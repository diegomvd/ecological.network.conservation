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
  rnd: Random,
  // energyTolerance cascade: each consumer population's energy intake at assembly (id -> S0).
  // Filled once by World.apply and carried through copies, so later cascades measure loss
  // relative to the ASSEMBLED community, not to whatever web the cascade is called on.
  energyBaseline: Map[Int, Double] = Map.empty,
  // diet-breadth rule (dietBreadthThreshold > 0): prey-species count per population at assembly.
  dietBaseline: Map[Int, Int] = Map.empty,
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

    // Initial prey-species counts per population. Only needed when threshold > 0; when 0 we use
    // the classic outDegree==0 rule. Taken from the assembly-time baseline when present (set by
    // World.apply), so a later call after primary extinctions measures diet loss relative to
    // the assembled community instead of re-baselining on the already-depleted web.
    val initialPreySpecies: Map[Int, Int] =
      if dietBreadthThreshold == 0.0 then Map.empty
      else if this.dietBaseline.nonEmpty then this.dietBaseline
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
   * Metabolic energetic cascade (Phase 2). Each population is an individual of its species'
   * body mass M = species.bodySize. For a predator individual i and prey individual j:
   *
   *   demand   D_i = M_i^0.75                 (Kleiber metabolic scaling; constant absorbed to 1)
   *   supply   S_j = ε · M_j                  (biomass energy, linear in mass)
   *   share    each prey j splits S_j EQUALLY among its current predators P_j
   *   survive  predator i lives iff  Σ_{j ∈ prey(i)} S_j / |P_j|  ≥  D_i
   *
   * Only predators starve — prey are never removed for being popular (over-predation is
   * implicit: many predators shrink each share). Removal is greedy: each round the single
   * predator with the largest deficit (D_i − allocatedSupply_i) is removed and the shares
   * recomputed, so removing one predator raises its co-predators' shares (rescue) before the
   * next removal. Basal species (trophicLevel 0) have no demand and cannot starve.
   *
   * Phase 0 (outDegree == 0 cleanup) must be run before calling this method.
   */
  def trophicCascades(epsilon: Double): (World, Int) =

    @tailrec
    def cascade(
      populationWeb: DefaultDirectedGraph[Population, DefaultEdge],
      populations: Seq[Population],
      nRounds: Int
    ): (DefaultDirectedGraph[Population, DefaultEdge], Seq[Population], Int) =

      val vertices = populationWeb.vertexSet().asScala.toSeq

      // |P_j| = number of predators currently linked to prey individual j (incoming edges).
      val predatorCount: Map[Int, Int] =
        vertices.map(q => q.id -> populationWeb.incomingEdgesOf(q).size()).toMap

      // Allocated supply for each predator: Σ over its prey of (ε · M_prey) / |predators(prey)|.
      // Only consumers (trophicLevel > 0) can starve; basal species are never removed here.
      val deficits = vertices.flatMap { p =>
        if p.species.trophicLevel == 0 then None
        else
          val allocated = populationWeb.outgoingEdgesOf(p).asScala.foldLeft(0.0) { (acc, e) =>
            val q = populationWeb.getEdgeTarget(e)
            val pc = predatorCount.getOrElse(q.id, 1)
            acc + (if pc > 0 then epsilon * q.species.bodySize / pc else 0.0)
          }
          val demand = math.pow(p.species.bodySize, 0.75)
          val deficit = demand - allocated
          if deficit > 0.0 then Some((p, deficit)) else None
      }

      if deficits.isEmpty then
        (populationWeb, populations, nRounds)
      else
        val (mostStressed, deficitVal) = deficits.maxBy(_._2)
        populationWeb.removeVertex(mostStressed)
        val newPopulations = populations.map { p =>
          if p.id == mostStressed.id && p.species.id == mostStressed.species.id then p.extinguish else p
        }
        cascade(populationWeb, newPopulations, nRounds + 1)

    val (newPopulationWeb, newPopulations, nRounds) = cascade(this.populationWeb, this.populations, 0)
    val extinctPopulations = newPopulations.collect { case p if p.extinct => (p.id, p.species.id) }
    val newManagementLandscape = this.managementLandscape.updatePersistentPopulations(extinctPopulations)
    (this.copy(populations = newPopulations, populationWeb = newPopulationWeb, managementLandscape = newManagementLandscape), nRounds)

  // Dispatch helper: the metabolic energetic cascade (epsilon > 0) is the go-forward Phase-2
  // model. Falls back to the legacy diet-breadth / outDegree==0 cleanup when epsilon == 0.
  private[econetcons] def phase2Cascades(): (World, Int) =
    this.worldParameters.cascadeModel match
      case "metabolic" =>
        if this.worldParameters.epsilon > 0.0 then
          this.trophicCascades(this.worldParameters.epsilon)
        else
          this.secondaryExtinctions(this.worldParameters.dietBreadthThreshold)
      case "legacy" =>
        this.secondaryExtinctions(this.worldParameters.dietBreadthThreshold)
      case "energyTolerance" =>
        this.energyToleranceCascades(this.worldParameters.energyTolerance)
      case other =>
        throw new IllegalArgumentException(s"unknown cascadeModel '$other'")

  /**
   * Energy-tolerance cascade. A population of a species with body mass m carries biomass
   * B = m^0.25 (Damuth density m^-0.75 × individual mass m). Each prey population splits its
   * biomass equally among its current predator populations; a consumer's intake is
   *
   *   S_i = Σ_{j ∈ prey(i)} B_j / |P_j|
   *
   * Demand is calibrated to the intake in the ASSEMBLED community (S_i⁰, `energyBaseline`), so
   * the assembled web is energetically feasible by construction. A consumer dies once
   * S_i ≤ (1 − θ)·S_i⁰, i.e. after losing more than a fraction θ of its initial intake. θ = 1 is
   * the classic rule (die only when no prey is left). Removal is greedy, one most-stressed
   * population per round with shares recomputed (co-predators get the freed prey: rescue), as in
   * `trophicCascades`. Basal species (trophicLevel 0) never starve.
   *
   * When `energyBaseline` is empty (assembly), this removes consumers with no prey (classic
   * cleanup) and then records S⁰ for every surviving consumer.
   */
  def energyToleranceCascades(theta: Double): (World, Int) =
    require(theta > 0.0 && theta <= 1.0, s"energyTolerance must be in (0, 1], got $theta")
    if this.energyBaseline.isEmpty then
      val (cleaned, n) = this.secondaryExtinctions(0.0)
      (cleaned.copy(energyBaseline = World.energySupply(cleaned.populationWeb)), n)
    else
      val baseline = this.energyBaseline

      @tailrec
      def cascade(web: DefaultDirectedGraph[Population, DefaultEdge], populations: Seq[Population], nRounds: Int)
        : (DefaultDirectedGraph[Population, DefaultEdge], Seq[Population], Int) =
        val supply = World.energySupply(web)
        val stressed = web.vertexSet().asScala.iterator.flatMap { p =>
          if p.species.trophicLevel == 0 then None
          else
            val s0 = baseline.getOrElse(p.id, 0.0)
            val s = supply.getOrElse(p.id, 0.0)
            // s == 0 (no prey left) always kills, whatever θ or baseline
            if s <= 0.0 || (s0 > 0.0 && s <= (1.0 - theta) * s0) then
              Some((p, if s0 > 0.0 then 1.0 - s / s0 else 1.0))
            else None
        }.toSeq
        if stressed.isEmpty then (web, populations, nRounds)
        else
          val (worst, _) = stressed.maxBy(_._2)
          web.removeVertex(worst)
          val newPopulations = populations.map { p =>
            if p.id == worst.id && p.species.id == worst.species.id then p.extinguish else p
          }
          cascade(web, newPopulations, nRounds + 1)

      val (newWeb, newPopulations, nRounds) = cascade(this.populationWeb, this.populations, 0)
      val extinct = newPopulations.collect { case p if p.extinct => (p.id, p.species.id) }
      val newLandscape = this.managementLandscape.updatePersistentPopulations(extinct)
      (this.copy(populations = newPopulations, populationWeb = newWeb, managementLandscape = newLandscape), nRounds)

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

  /** Biomass carried by one population of a species of body mass m: Damuth density m^-0.75 × m. */
  private[econetcons] def populationBiomass(s: Species): Double = math.pow(s.bodySize, 0.25)

  /**
   * Energy intake of every consumer population (one with ≥1 prey) in `web`: each prey population
   * splits its biomass equally among its current predators.
   */
  private[econetcons] def energySupply(web: DefaultDirectedGraph[Population, DefaultEdge]): Map[Int, Double] =
    val out = scala.collection.mutable.HashMap.empty[Int, Double]
    web.vertexSet().asScala.foreach { p =>
      if web.outDegreeOf(p) > 0 then
        out(p.id) = web.outgoingEdgesOf(p).asScala.foldLeft(0.0) { (acc, e) =>
          val q = web.getEdgeTarget(e)
          acc + populationBiomass(q.species) / web.inDegreeOf(q)
        }
    }
    out.toMap

  /**
   * Continuous (prey-averaged) trophic position, Levine (1980): basal species = 1, consumers
   * TP_i = 1 + mean TP of their metaweb prey. Solved by fixed-point iteration, which converges
   * because generateMetaWeb only accepts metawebs whose cycles have prey outside the cycle.
   */
  private[econetcons] def trophicPositions(metaWeb: DefaultDirectedGraph[Species, DefaultEdge]): Map[Int, Double] =
    val species = metaWeb.vertexSet().asScala.toVector
    val prey: Map[Int, Vector[Int]] = species.map(s =>
      s.id -> metaWeb.outgoingEdgesOf(s).asScala.toVector.map(e => metaWeb.getEdgeTarget(e).id)).toMap
    var tp: Map[Int, Double] = species.map(s => s.id -> 1.0).toMap
    var delta = Double.MaxValue
    var it = 0
    while delta > 1e-10 && it < 10000 do
      val next = species.map { s =>
        val ps = prey(s.id)
        s.id -> (if ps.isEmpty then 1.0 else 1.0 + ps.map(tp).sum / ps.size)
      }.toMap
      delta = species.map(s => math.abs(next(s.id) - tp(s.id))).max
      tp = next
      it += 1
    if delta > 1e-6 then throw new IllegalStateException(s"trophic positions did not converge (delta=$delta)")
    tp

  /**
   * Relative allocation weight per species (populations are later apportioned ∝ weight).
   * See WorldParameters.abundanceModel.
   */
  private def allocationWeights(metaWeb: DefaultDirectedGraph[Species, DefaultEdge], wp: WorldParameters): Map[Int, Double] =
    val species = metaWeb.vertexSet().asScala.toVector
    wp.abundanceModel match
      case "damuth" => species.map(s => s.id -> s.abundance).toMap
      case "transferEfficiency" =>
        val tp = trophicPositions(metaWeb)
        species.map(s => s.id -> s.abundance * math.pow(wp.transferEfficiency, tp(s.id) - 1.0)).toMap
      case "energyBalance" =>
        // N_i = Damuth density for basal species; for consumers N_i · M_i^0.75 = Σ_j ε·N_j·m_j / P_j
        // (P_j = number of predator species of j). Solved by fixed-point iteration N = b + K N.
        // Only links going UP the trophic ladder (prey trophic position < predator's) carry energy:
        // with ε > 1 a feeding loop would otherwise amplify energy without bound (no solution).
        require(wp.epsilon > 0.0, "abundanceModel=energyBalance needs epsilon > 0")
        val tp = trophicPositions(metaWeb)
        val metaPrey = species.map(s => s.id -> metaWeb.outgoingEdgesOf(s).asScala.toVector.map(metaWeb.getEdgeTarget)).toMap
        val prey = species.map(s => s.id -> metaPrey(s.id).filter(j => tp(j.id) < tp(s.id))).toMap
        val nPred = species.map(s => s.id -> metaWeb.inDegreeOf(s)).toMap
        val basal = species.filter(s => metaPrey(s.id).isEmpty).map(s => s.id -> s.abundance).toMap
        var n: Map[Int, Double] = species.map(s => s.id -> basal.getOrElse(s.id, 0.0)).toMap
        var delta = Double.MaxValue
        var it = 0
        while delta > 1e-10 && it < 10000 do
          val next = species.map { s =>
            s.id -> basal.getOrElse(s.id,
              prey(s.id).map(j => wp.epsilon * n(j.id) * j.bodySize / nPred(j.id)).sum / math.pow(s.bodySize, 0.75))
          }.toMap
          val scale = math.max(1e-300, next.values.max)
          delta = species.map(s => math.abs(next(s.id) - n(s.id))).max / scale
          if scale > 1e12 then
            throw new IllegalStateException("energyBalance allocation diverges (metaweb cycles amplify energy); lower epsilon")
          n = next
          it += 1
        n
      case other => throw new IllegalArgumentException(s"unknown abundanceModel '$other'")

  // Test-only hook: build metaweb + spatially-arranged populations (pre-web, pre-cascade).
  private[econetcons] def buildPopsAndMetaForTest(
    wp: WorldParameters, rnd: Random
  ): (Seq[Population], DefaultDirectedGraph[Species, DefaultEdge]) =
    val metaWeb = generateMetaWeb(wp.numberOfSpecies, wp.connectanceMetaWeb, wp.medianHomeRange, wp.bodySizeSigma, rnd)
    val pops = generatePopulations(metaWeb, wp.nPopulations, rnd, wp)
    (pops, metaWeb)

  def apply(
    worldParameters: WorldParameters,
    conservationParameters: ConservationParameters,
    rnd: Random
  ): World =

    val metaWeb = generateMetaWeb(
      worldParameters.numberOfSpecies,
      worldParameters.connectanceMetaWeb,
      worldParameters.medianHomeRange,
      worldParameters.bodySizeSigma,
      rnd
    )

    // Population count is a direct parameter (the density knob); management-grid resolution
    // is derived from it via the desired average populations per tile.
    val numberOfPopulations = worldParameters.nPopulations

    val nTiles = (numberOfPopulations.toDouble / worldParameters.avgPopulationsPerTile.toDouble).toInt

    val landscapeGrid = SquareGrid.fromArea(nTiles)

   // println("Generating populations.")
    val populations = generatePopulations(metaWeb, numberOfPopulations, rnd, worldParameters)

    //println("Generating interaction network.")
    val populationWeb = PopulationWebGenerator.generatePopulationWeb(populations, metaWeb)

    // TODO: check if this is true  
    // Update populations given that the non interacting ones are removed from the network
    val populationsInit = populationWeb.vertexSet().asScala.toSeq


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

    // Diet-breadth rule: freeze the assembled community's prey-species counts so later cascades
    // measure diet loss against it (energyTolerance stores its own baseline inside the cascade).
    val usesDietRule = worldParameters.dietBreadthThreshold > 0.0 &&
      (worldParameters.cascadeModel == "legacy" ||
        (worldParameters.cascadeModel == "metabolic" && worldParameters.epsilon <= 0.0))
    if usesDietRule then
      val web = stableWorld.populationWeb
      stableWorld.copy(dietBaseline = web.vertexSet().asScala.map { p =>
        p.id -> web.outgoingEdgesOf(p).asScala.map(e => web.getEdgeTarget(e).species.id).toSet.size
      }.toMap)
    else stableWorld


  private def generateMetaWeb(numberOfSpecies: Int, connectance: Double, medianHomeRange: Double, bodySizeSigma: Double, rnd: Random):
  DefaultDirectedGraph[Species, DefaultEdge]  =

    // Seed a Commons-Math RNG deterministically from the threaded `rnd` so metaweb
    // generation is reproducible for a given seed. Previously the distributions were
    // constructed with no generator, which seeded them from system entropy and made
    // the whole simulation non-reproducible. One shared generator is created here,
    // outside rec(): its state advances across retries, keeping the retry loop
    // productive while remaining fully seed-determined.
    val commonsRng: org.apache.commons.math3.random.RandomGenerator =
      new org.apache.commons.math3.random.Well19937c(rnd.nextLong())
    val bodySizeDist = new LogNormalDistribution(commonsRng, 0, bodySizeSigma)
    def betaDist(beta: Double) = new BetaDistribution(commonsRng, 1, beta)

    @tailrec
    def rec(): DefaultDirectedGraph[(Int, Double), DefaultEdge] =

      // Body sizes are drawn from lognormal distribution where close to 90% of species fall within 4 orders of magnitude
      val bodySizes = Seq.fill(numberOfSpecies)(bodySizeDist.sample()).sorted

      // Niche values are scaled between 0 and 1 from body sizes
      val nicheValues = bodySizes.map(m => (m-bodySizes.min)/(bodySizes.max-bodySizes.min))

      val nicheValuesMax = nicheValues.max
    
      // val localDensities = nicheValues.zipWithIndex.map { (n, i) =>
      //  val neighbors = nicheValues.filter(x => math.abs(x - n) < 0.1)
      //   neighbors.size / 0.2  // Density in window around niche
      // }

      // Feeding range from beta distribution. The species with the lowest niche value is set to be a basal species: feedingRange = 0
      val beta = (0.5 / connectance - 1) // how to explain this??
      val feedingRangeDist = betaDist(beta)
      
      // val feedingRange = nicheValues.zip(localDensities).zipWithIndex.map { 
      //   case ((n, density), i) =>
      //     if (i > 0) {
      //       val baseRange = n * BetaDistribution(1, beta).sample()
      //       baseRange / density  // Scale by local density
      //     } else 0.0
      // }

      val feedingRange =
        nicheValues.zipWithIndex.map(
          (n, i) => if i > 0 then n * feedingRangeDist.sample() else 0.0
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

    metaWebDef

  private def generatePopulations(
    metaWeb: DefaultDirectedGraph[Species, DefaultEdge],
    numberOfPopulations: Int,
    rnd: Random,
    worldParameters: WorldParameters
  ): Seq[Population] = {
    // Relative weight per species (WorldParameters.abundanceModel; "damuth" = s.abundance).
    val weight: Map[Int, Double] = allocationWeights(metaWeb, worldParameters)
    val abundancesSum: Double = metaWeb.vertexSet().asScala.toSeq.map(s => weight(s.id)).sum
    val conversionFactor: Double = numberOfPopulations / abundancesSum

    // Total count is apportioned across species by their allocation weight.
    // Accumulate into a mutable ArrayBuffer (O(1) amortized append per species)
    // instead of `allPopulations ++ ...` per species (O(n) per concatenation).
    val allPopulations = scala.collection.mutable.ArrayBuffer.empty[Population]
    var currentId = 0
    metaWeb.vertexSet().asScala.toSeq.foreach { species =>

      // Calculate number of populations for this species
      val speciesAbundance = math.max(1, (weight(species.id) * conversionFactor).toInt)

      // Create template populations with placeholder coordinates
      val templatePopulations = (0 until speciesAbundance).map { i =>
        Population(
          id = currentId + i,
          species = species,
          coordinates = new org.locationtech.jts.geom.Coordinate(0.0, 0.0, 0.0), // placeholder
          extinct = false
        )
      }

      // Generate spatial coordinates for all populations of this species
      val spatiallyArrangedPopulations = Population.createPopulations(
        species         = species,
        populationSeq   = templatePopulations,
        rnd             = rnd,
        worldParameters = worldParameters
      )

      allPopulations ++= spatiallyArrangedPopulations
      currentId += speciesAbundance
    }
    allPopulations.toSeq
  }
    
end World
    

