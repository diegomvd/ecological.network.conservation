package econetcons

import ProtectionStatus.{Protected, Unprotected}

case class WorldParameters(
  numberOfSpecies: Int = 100,
  connectanceMetaWeb: Double = 0.2,
  areaOverlap: Double = 2.0, // this is the fraction of the landscape that is covered by all home ranges, it can be larger than 1
  medianHomeRange: Double = 0.01, // median home range radius relative to the landscape radius
  avgPopulationsPerTile: Int = 10, // average number of populations per tile
):

  def extinctionProbability(status: ProtectionStatus): Double =
    status match
      case Protected => 0.01
      case Unprotected => 0.75
      case null => 0.0

