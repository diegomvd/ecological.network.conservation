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
        // Check its outgoing edges (prey relationships)
        populationWeb.outgoingEdgesOf(predatorPop).asScala.foreach { edge =>
          val preyPop = populationWeb.getEdgeTarget(edge)

          // Only count if the prey is also in this management area
          if (populationIds.contains(preyPop.id)) {
            val predatorSpeciesId = predatorPop.species.id
            val preySpeciesId = preyPop.species.id
            speciesInteractions += ((predatorSpeciesId, preySpeciesId))
          }
        }
      }
    }

    speciesInteractions.size

  def getAbundance: Int =
    this.populations.size


