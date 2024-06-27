package network.conservation

import org.locationtech.jts.geom.Geometry

import scala.util.Random

case class ManagementArea(id: Int, shape: Geometry, status: ProtectionStatus, populations: Map[Int, Int]):
  
  def updateProtectionStatus(): ManagementArea =
    this.copy(status = ProtectionStatus.Protected)
    
  def primaryExtinctions(worldParameters: WorldParameters, rnd: Random): Map[Int,Int] =
    this.populations.collect {
      case p if rnd.nextDouble() < worldParameters.extinctionProbability(status) => (p._1, p._2)
    }

  def updatePersistentPopulations(extinctPopulations: Seq[(Int,Int)]): ManagementArea =
    val updatedPopulations = populations.collect{
      case p if !extinctPopulations.contains((p._1,p._2)) => p
    }
    this.copy(populations=updatedPopulations)



    

