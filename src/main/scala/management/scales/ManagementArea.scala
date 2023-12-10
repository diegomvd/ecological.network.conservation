package management.scales

import org.locationtech.jts.geom.Geometry

case class ManagementArea(
                          id: Long,
                          shape: Geometry,
                          status: ProtectionStatus,
                          populations: List[Population]
                         ):
  
  def updateProtectionStatus(): ManagementArea =
    this.copy( status = ProtectionStatus.Protected) 

