package econetcons

import org.locationtech.jts.geom.Geometry
import org.jgrapht.graph.{DefaultDirectedGraph, DefaultEdge}
import scala.jdk.CollectionConverters.*
import scala.util.Random

case class ManagementArea(id: Int, status: ProtectionStatus, populations: Map[Int, Int]):

  /*
  The Populations map p is such that: p._1 refers to population Id and p._2 is the species Id
  */

  def updateProtectionStatus(): ManagementArea =
    this.copy(status = ProtectionStatus.Protected)

  def primaryExtinctions(worldParameters: WorldParameters, rnd: Random): Map[Int,Int] =
    val extinctPopulations = this.populations.collect {
      case p if rnd.nextDouble() < worldParameters.extinctionProbability(status) => (p._1, p._2)
    }
    extinctPopulations

  def updatePersistentPopulations(extinctPopulations: Seq[(Int,Int)]): ManagementArea =
    val updatedPopulations = populations.collect{
      case p if !extinctPopulations.contains((p._1,p._2)) => p
    }
    this.copy(populations=updatedPopulations)

  def getSpecies: Set[Int] =
    this.populations.values.toSet

  def getSpeciesRichness: Int =
    this.getSpecies.size

  def getInteractionRichness(populationWeb: DefaultDirectedGraph[Population, DefaultEdge]): Int =
    val speciesInteractions = scala.collection.mutable.Set[(Int, Int)]()
    val populationIds = this.populations.keySet

    // Check all populations in this management area
    populationIds.foreach { popId =>
      // Find this population in the graph
      populationWeb.vertexSet().asScala.find(_.id == popId).foreach { predatorPop =>
        // Only count if predator is not extinct
        if (!predatorPop.extinct) {
          // Check its outgoing edges (prey relationships)
          populationWeb.outgoingEdgesOf(predatorPop).asScala.foreach { edge =>
            val preyPop = populationWeb.getEdgeTarget(edge)

            // Only count if:
            // 1. Prey is also in this management area
            // 2. Prey is not extinct
            if (populationIds.contains(preyPop.id) && !preyPop.extinct) {
              val predatorSpeciesId = predatorPop.species.id
              val preySpeciesId = preyPop.species.id
              speciesInteractions += ((predatorSpeciesId, preySpeciesId))
            }
          }
        }
      }
    }

    speciesInteractions.size

  def getInteractions(populationWeb: DefaultDirectedGraph[Population, DefaultEdge]): Set[(Int, Int)] =
    val speciesInteractions = scala.collection.mutable.Set[(Int, Int)]()
    val populationIds = this.populations.keySet

    populationIds.foreach { popId =>
      populationWeb.vertexSet().asScala.find(_.id == popId).foreach { predatorPop =>
        if (!predatorPop.extinct) {
          populationWeb.outgoingEdgesOf(predatorPop).asScala.foreach { edge =>
            val preyPop = populationWeb.getEdgeTarget(edge)
            if (populationIds.contains(preyPop.id) && !preyPop.extinct) {
              val predatorSpeciesId = predatorPop.species.id
              val preySpeciesId = preyPop.species.id
              speciesInteractions += ((predatorSpeciesId, preySpeciesId))
            }
          }
        }
      }
    }

    speciesInteractions.toSet

  // Ego-centric interaction richness: counts species-level interactions where at least
  // one endpoint (predator OR prey) has a population in this tile. Cross-tile interactions
  // are included — this tile is "credited" for hosting part of the interaction.
  def getEgocentricInteractionRichness(populationWeb: DefaultDirectedGraph[Population, DefaultEdge]): Int =
    val speciesInteractions = scala.collection.mutable.Set[(Int, Int)]()
    val populationIds = this.populations.keySet

    populationWeb.vertexSet().asScala.foreach { pop =>
      if !pop.extinct && populationIds.contains(pop.id) then
        // Outgoing edges: this tile hosts the predator
        populationWeb.outgoingEdgesOf(pop).asScala.foreach { edge =>
          val preyPop = populationWeb.getEdgeTarget(edge)
          if !preyPop.extinct then
            speciesInteractions += ((pop.species.id, preyPop.species.id))
        }
        // Incoming edges: this tile hosts the prey
        populationWeb.incomingEdgesOf(pop).asScala.foreach { edge =>
          val predatorPop = populationWeb.getEdgeSource(edge)
          if !predatorPop.extinct then
            speciesInteractions += ((predatorPop.species.id, pop.species.id))
        }
    }

    speciesInteractions.size

  def getEgocentricInteractions(populationWeb: DefaultDirectedGraph[Population, DefaultEdge]): Set[(Int, Int)] =
    val speciesInteractions = scala.collection.mutable.Set[(Int, Int)]()
    val populationIds = this.populations.keySet
    populationWeb.vertexSet().asScala.foreach { pop =>
      if !pop.extinct && populationIds.contains(pop.id) then
        populationWeb.outgoingEdgesOf(pop).asScala.foreach { edge =>
          val preyPop = populationWeb.getEdgeTarget(edge)
          if !preyPop.extinct then speciesInteractions += ((pop.species.id, preyPop.species.id))
        }
        populationWeb.incomingEdgesOf(pop).asScala.foreach { edge =>
          val predatorPop = populationWeb.getEdgeSource(edge)
          if !predatorPop.extinct then speciesInteractions += ((predatorPop.species.id, pop.species.id))
        }
    }
    speciesInteractions.toSet

  def getAbundance: Int =
    this.populations.size



