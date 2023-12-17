package management.scales

import org.locationtech.jts.geom.Geometry

import scala.util.Random

case class ManagementArea(
                          id: Long,
                          shape: Geometry,
                          status: ProtectionStatus,
                          populations: List[Population]
                         ):
  
  def updateProtectionStatus(): ManagementArea =
    this.copy( status = ProtectionStatus.Protected) 
    
  def primaryExtinctions(worldParameters: WorldParameters, rnd: Random): ManagementArea =
    val newPopulations = this.populations.map(
      p => if rnd.nextDouble() < worldParameters.extinctionProbability(status) then p.extinguish else p 
    )
    this.copy(populations = newPopulations)

    

