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

case class World(populations: Seq[Population], managementLandscape: ManagementLandscape, metaWeb: DefaultDirectedGraph[Species, DefaultEdge], populationWeb: DefaultDirectedGraph[Population, DefaultEdge], worldParameters: WorldParameters, rnd: Random):

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
    def extinctionCascades(populationWeb: DefaultDirectedGraph[Population, DefaultEdge], populations: Seq[Population], nCascades: Int):
      (DefaultDirectedGraph[Population,DefaultEdge], Seq[Population], Int) =

      val (extinguishVertices, extinguishIds) = this.populationWeb.vertexSet().asScala
        .collect {
          case p if p.species.trophicLevel>0 & this.populationWeb.outDegreeOf(p) == 0 => (p, (p.species.id, p.id))
        }.unzip

      if extinguishIds.isEmpty
      then (populationWeb, populations, nCascades)
      else {
        val newPopulationWeb: Boolean = populationWeb.removeAllVertices(extinguishVertices.asJavaCollection)
        // If this is slow consider making populations a Map[Id,Pop]
        val newPopulations = populations.map(p => if extinguishIds.contains((p.species.id,p.id)) then p.extinguish else p)
        val newNCascades = nCascades + 1

        extinctionCascades(populationWeb, newPopulations, newNCascades)
      }

    val (newPopulationWeb, newPopulations, nCascades) = extinctionCascades(this.populationWeb, this.populations, 0)

    val extinctPopulations = newPopulations.collect{case p if p.extinct => (p.species.id,p.id) }
    val newManagementLandscape = this.managementLandscape.updatePersistentPopulations(extinctPopulations)

    (this.copy(populations = newPopulations, populationWeb = newPopulationWeb, managementLandscape = newManagementLandscape) , nCascades)
    
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
    numberOfSpecies: Int,
    connectanceMetaWeb: Double,
    numberOfPopulations: Int,
    basalHomeRange: Double,
    landscapeRadius: Int,
    fractionProtected: Double,
    connectivity: Double, 
    decayDistancePopulations: Double,
    wSpRichness: Double,
    wInteractionRichness: Double,
    wAbundance: Double,  
    rnd: Random
  ): World =

    val worldParameters = WorldParameters(numberOfSpecies, connectanceMetaWeb, numberOfPopulations, basalHomeRange, landscapeRadius, fractionProtected, connectivity, decayDistancePopulations, wSpRichness, wInteractionRichness, wAbundance)

    val metaWeb = generateMetaWeb(numberOfSpecies, connectanceMetaWeb, rnd, basalHomeRange)

    val landscapeGrid = HexagonalGrid(landscapeRadius) 

    val landscapeBoundary = {
      // First create array of validated individual polygons
      val validPolygons = landscapeGrid.values.map { poly =>
        if (!poly.isValid) {
          // Try to fix with buffer(0)
          poly.buffer(0).asInstanceOf[Polygon]
        } else poly
      }.toArray

      // Create and validate MultiPolygon
      val multiPoly = GeometryFactory().createMultiPolygon(validPolygons)
      
      // Try to fix invalid MultiPolygon if needed
      if (!multiPoly.isValid) {
        multiPoly.buffer(0)
      } else multiPoly
    }

    println("Generating populations.")

    val populations = generatePopulations(metaWeb, numberOfPopulations, rnd, decayDistancePopulations, landscapeBoundary)

    println("Generating interaction network.")

    val populationWeb = generatePopulationWeb(populations, metaWeb)

    println("Removing isolated populations.")
  
    // Update populations given that the non interacting ones are removed from the network
    val populationsInit = populationWeb.vertexSet().asScala.toSeq

    println("Creating management landscape.")
    val managementLandscape = ManagementLandscape(landscapeGrid, populationsInit, rnd).applyProtectionPlan(worldParameters,rnd)

    println("Initializing world.")
    new World(populationsInit, managementLandscape, metaWeb, populationWeb, worldParameters, rnd)


  private def generateMetaWeb(numberOfSpecies: Int, connectance: Double, rnd: Random, basalHomeRange: Double):
  DefaultDirectedGraph[Species, DefaultEdge]  =


    @tailrec
    def rec(): DefaultDirectedGraph[(Int, Double), DefaultEdge] =

      // Body sizes are drawn from lognormal distribution where close to 90% of species fall within 4 orders of magnitude
      val bodySizes = Seq.fill(numberOfSpecies)(LogNormalDistribution(0,3).sample()).sorted

      // Niche values are scaled between 0 and 1 from body sizes
      val nicheValues = bodySizes.map(m => (m-bodySizes.min)/(bodySizes.max-bodySizes.min))

      // Feeding range from beta distribution. The species with the lowest niche value is set to be a basal species: feedingRange = 0
      val beta = 0.5 / connectance - 1
      val feedingRange =
        nicheValues.zipWithIndex.map(
          (n, i) => if i > 0 then n * BetaDistribution(1, beta).sample() else 0.0
        )

      // Feeding center is drawn from uniform distribution between the feeding range.
      val feedingCenter =
        feedingRange.zip(nicheValues).map(
          (r, n) => {
            rnd.between(0.5 * r, math.min(n, 1 - 0.5 * r))
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
            species => species.exists {
              s => {
                val preySet = metaWeb.outgoingEdgesOf(s).asScala.map(link => metaWeb.getEdgeTarget(link))
                species.intersect(preySet).size > species.size + 1
              }
            }
          )

          if !externalEnergyCycles then rec() else {

            val hasDuplicates =
              metaWeb.vertexSet().asScala.map(
                id => {
                  val predators = metaWeb.incomingEdgesOf(id).asScala.map(link => metaWeb.getEdgeSource(link))
                  val preys = metaWeb.outgoingEdgesOf(id).asScala.map(link => metaWeb.getEdgeTarget(link))
                  (predators, preys)
                }
              ).size < numberOfSpecies
            if !hasDuplicates then metaWeb else rec()
          }

        } else {
          val hasDuplicates =
            metaWeb.vertexSet().asScala.map(
              id => {
                val predators = metaWeb.incomingEdgesOf(id).asScala.map(link => metaWeb.getEdgeSource(link))
                val preys = metaWeb.outgoingEdgesOf(id).asScala.map(link => metaWeb.getEdgeTarget(link))
                (predators, preys)
              }
            ).size < numberOfSpecies

          if !hasDuplicates then metaWeb else rec()
        }
      }

    val metaWeb = rec()

    val metaWebDef = DefaultDirectedGraph[Species, DefaultEdge](classOf[DefaultEdge])

    metaWeb.vertexSet().asScala.foreach(
      predator => {
        val trophicLevel = Species.calculateTrophicLevel(predator, metaWeb)
        val predatorSpecies = Species(predator._1, predator._2, basalHomeRange, trophicLevel, rnd)
        // If the species was already added then the graph is left unchanged.
        val addedPredator = metaWebDef.addVertex(predatorSpecies)

        metaWeb.outgoingEdgesOf(predator).asScala.toSeq.foreach {
          link => {
            val prey = metaWeb.getEdgeTarget(link)

            val trophicLevel = Species.calculateTrophicLevel(prey, metaWeb)
            val preySpecies = Species(prey._1, prey._2, basalHomeRange, trophicLevel, rnd)
            // If the species was already added then the graph is left unchanged.
            val addedPrey = metaWebDef.addVertex(preySpecies)
            // If the interaction was already added then the graph is left unchanged.
            val newInteraction = metaWebDef.addEdge(predatorSpecies, preySpecies, DefaultEdge())
          }
        }
      }
    )
    metaWebDef

  private def generatePopulations(
    metaWeb: DefaultDirectedGraph[Species, DefaultEdge], 
    numberOfPopulations: Int, 
    rnd: Random, 
    lambda: Double, 
    landscapeBoundary: Geometry
  ): Seq[Population] = {
    val abundancesSum: Double = metaWeb.vertexSet().asScala.toSeq.map(s => s.abundance).sum
    val conversionFactor: Double = numberOfPopulations / abundancesSum

    // Fold over the species, tracking both populations and the next available ID
    metaWeb.vertexSet().asScala.toSeq
      .foldLeft((Seq.empty[Population], 0)) { case ((populationSeq, currentId), species) => 
        @tailrec
        def rec(
          populationSeq: Seq[Population], 
          id: Int, 
          species: Species, 
          speciesAbundance: Int
        ): (Seq[Population], Int) = {
          if (populationSeq.size == speciesAbundance) 
            (populationSeq, id)
          else {
            val newPopulation = Population(id, species, populationSeq, rnd, lambda, landscapeBoundary)
            val newPopulationSeq = populationSeq :+ newPopulation
            rec(newPopulationSeq, id + 1, species, speciesAbundance)
          }
        }

        // Ensure we generate the correct number of populations
        val speciesAbundance = math.max(1, (species.abundance * conversionFactor).toInt)
        val (newPopulationSeq, nextId) = rec(Seq(), currentId, species, speciesAbundance)
        
        (populationSeq ++ newPopulationSeq, nextId)
      }._1  // Return only the populations
  }

  private def generatePopulationWeb(populations: Seq[Population], metaWeb: DefaultDirectedGraph[Species,DefaultEdge]):
  DefaultDirectedGraph[Population,DefaultEdge] =

    def interact(p1:Population,p2:Population): Boolean =
      val point1 = GeometryFactory().createPoint(p1.coordinates)
      val point2 = GeometryFactory().createPoint(p2.coordinates)
      val distance = DistanceOp(point1, point2).distance()
      distance < p1.species.homeRange + p2.species.homeRange

    // From scratch filter out all the popoulations that do not have any interaction. 
    val interactingPopulations: Seq[(Population, Population)] = populations.toSet.subsets(2).collect {
      case p if interact(p.head, p.last) => (p.head, p.last)
    }.toSeq

    val populationWeb = DefaultDirectedGraph[Population, DefaultEdge](classOf[DefaultEdge])

    val interactions: Unit = interactingPopulations.foreach(
      x => {
        val p1 = x._1
        val p2 = x._2
        populationWeb.addVertex(p1)
        populationWeb.addVertex(p2)

        val s1preys = metaWeb.outgoingEdgesOf(p1.species).asScala.map{
          link => metaWeb.getEdgeTarget(link)
        }
        val s2preys = metaWeb.outgoingEdgesOf(p2.species).asScala.map {
          link => metaWeb.getEdgeTarget(link)
        }

        if s1preys.contains(p2.species)
        then {
          populationWeb.addEdge(p1, p2, DefaultEdge())
        }
        if s2preys.contains(p1.species)
        then {
          populationWeb.addEdge(p2, p1, DefaultEdge())
        }
      }
    )

    populationWeb
    
end World
    

