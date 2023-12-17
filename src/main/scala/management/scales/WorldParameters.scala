package management.scales

import management.scales.ProtectionStatus.{Protected, Unprotected}

case class WorldParameters(radius: Double,
                           numberOfSpecies: Long,
                           connectanceMetaWeb: Double,
                           populationsDensity: Double,
                           managementScale: Double,
                           managementClustering: Double):
  
  def extinctionProbability(status: ProtectionStatus): Double =
    status match
      case Protected => 0.1
      case Unprotected => 0.5
      case _ => 0.0
