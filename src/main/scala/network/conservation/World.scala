package network.conservation

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

import network.conservation.SquareGrid


case class World(
  populations: Seq[Population], 
  managementLandscape: ManagementLandscape, 
  metaWeb: DefaultDirectedGraph[Species, DefaultEdge], 
  populationWeb: DefaultDirectedGraph[Population, DefaultEdge], 
  worldParameters: WorldParameters, 
  conservationParameters: ConservationParameters, 
  rnd: Random
):

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


  def secondaryExtinctions(): (World, Int) =

    @tailrec
    def extinctionCascades(
      populationWeb: DefaultDirectedGraph[Population, DefaultEdge], 
      populations: Seq[Population], 
      nCascades: Int
    ): (DefaultDirectedGraph[Population,DefaultEdge], Seq[Population], Int) =

      
      val (extinguishVertices, extinguishIds) = populationWeb.vertexSet().asScala
        .collect {
          case p if p.species.trophicLevel > 0 && populationWeb.outDegreeOf(p) == 0 => 
            (p, (p.id, p.species.id))  
        }.unzip

      if extinguishIds.isEmpty then
        (populationWeb, populations, nCascades)
      else {
        println(s"  Cascade ${nCascades + 1}: Removing ${extinguishVertices.size} predators without prey")
        
        val newPopulationWeb: Boolean = populationWeb.removeAllVertices(extinguishVertices.asJavaCollection)
        
        // BUG FIX #2: Use consistent tuple order (popId, speciesId)
        val newPopulations = populations.map { p => 
          if extinguishIds.contains((p.id, p.species.id)) then p.extinguish else p
        }
        
        val newNCascades = nCascades + 1

        extinctionCascades(populationWeb, newPopulations, newNCascades)
      }

    val (newPopulationWeb, newPopulations, nCascades) = extinctionCascades(this.populationWeb, this.populations, 0)

    val extinctPopulations = newPopulations.collect{case p if p.extinct => (p.id, p.species.id) }
    val newManagementLandscape = this.managementLandscape.updatePersistentPopulations(extinctPopulations)

    (this.copy(populations = newPopulations, populationWeb = newPopulationWeb, managementLandscape = newManagementLandscape), nCascades)
    
  def extinctions(): (World,Int) =
    // First extinguish populations that cannot subsist, then extinguish according to protection plan and then simulate the cascading secondary extinctions.
    this.secondaryExtinctions()._1.primaryExtinctions().secondaryExtinctions()
    
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

    val numberOfPopulations = 
      (worldParameters.areaOverlap / (worldParameters.medianHomeRange * worldParameters.medianHomeRange )).toInt 

    val nTiles = (numberOfPopulations.toDouble / worldParameters.avgPopulationsPerTile.toDouble).toInt
     
    val landscapeGrid = SquareGrid.fromArea(nTiles)

   // println("Generating populations.")
    val populations = generatePopulations(metaWeb, numberOfPopulations, rnd)

    //println("Generating interaction network.")
    val populationWeb = PopulationWebGenerator.generatePopulationWeb(populations, metaWeb)

    // TODO: check if this is true  
    // Update populations given that the non interacting ones are removed from the network
    val populationsInit = populationWeb.vertexSet().asScala.toSeq
    println("npops: " + populationsInit.size)


    //println("Creating management landscape.")
    val managementLandscape = ManagementLandscape(landscapeGrid, populationsInit, rnd).applyProtectionPlan(conservationParameters,rnd)

    //println("Initializing world.")
    val unstableWorld = new World(populationsInit, managementLandscape, metaWeb, populationWeb, worldParameters, conservationParameters, rnd)

    val (stableWorld,nCascades) = unstableWorld.secondaryExtinctions()

    println("cascades: " + nCascades)
    println("n pops 2:" + stableWorld.populations.count(!_.extinct))

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
          species = species,
          populationSeq = templatePopulations,
          rnd = rnd,
        )
        
        (allPopulations ++ spatiallyArrangedPopulations, currentId + speciesAbundance)
      }._1 // Return only the populations
  }
    
end World
    

