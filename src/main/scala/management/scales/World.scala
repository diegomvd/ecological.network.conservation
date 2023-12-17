package management.scales

import scala.util.Random
import org.jgrapht.*
import org.jgrapht.graph.*

import scala.jdk.CollectionConverters.*

case class World(
                  populations: List[Population],
                  managementLandscape: ManagementLandscape,
                  metaWeb: Graph[Species,DefaultEdge],
                  populationWeb: Graph[Population,DefaultEdge]
                ):

  def primaryExtinctions(worldParameters: WorldParameters, rnd: Random): World =
   
    val managementAreas =
      this.managementLandscape.managementAreas.map(
        a => a.primaryExtinctions(worldParameters,rnd)
      )
    
    val newManagementLandscape = ManagementLandscape(managementAreas)
    
    val newPopulations =
      managementAreas.flatMap(a => a.populations)

    //TODO: update the population web
    this.copy(populations = newPopulations, managementLandscape = newManagementLandscape)


  def secondaryExtinctions(): World =
    val newPopulations = this.populationWeb.vertexSet().asScala
      .map(
        p => if this.populationWeb.outDegreeOf(p) then p else p.extinguish
      )
    
    val managementAreas = 
      this.managementLandscape.managementAreas.map(
        a => a.populations
      )

    //TODO: find a way to update the population Web
    this.copy(populations = newPopulations)
    
  def extinctions(): World =
    this 
    
  def getRealisedWeb(): Graph[Species, DefaultEdge] =
  
  object World:
  
    def apply(): World
    def generatePopulationWeb(populations: List[Populations], metaWeb: Graph[Species,DefaultEdge]): Graph[Population,DefaultEdge]
    
    def generateMetaWeb(): Graph[Species,DefaultEdge]
    
  end World
    

