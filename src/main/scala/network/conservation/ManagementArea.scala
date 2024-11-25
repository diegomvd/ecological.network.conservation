package network.conservation

import org.locationtech.jts.geom.Geometry

import scala.util.Random

case class ManagementArea(id: Int, status: ProtectionStatus, populations: Map[Int, Int]):
  
  /*
  The Populations map p is such that: p._1 refers to population Id and p._2 is the species Id
  */

  def updateProtectionStatus(): ManagementArea =
    this.copy(status = ProtectionStatus.Protected)
    
  def primaryExtinctions(worldParameters: WorldParameters, rnd: Random): Map[Int,Int] =
    
    val dice = rnd.nextDouble()
    this.populations.collect {
      case p if dice < worldParameters.extinctionProbability(status) => (p._1, p._2)
    }

  def updatePersistentPopulations(extinctPopulations: Seq[(Int,Int)]): ManagementArea =
    val updatedPopulations = populations.collect{
      case p if !extinctPopulations.contains((p._1,p._2)) => p
    }
    this.copy(populations=updatedPopulations)

  def getSpecies=
    this.populations.values.toSet

  def getSpeciesRichness=
    this.getSpecies.size 

    

