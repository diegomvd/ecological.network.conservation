package network.conservation

import ProtectionStatus.{Protected, Unprotected}

case class WorldParameters(
  numberOfSpecies: Int,
  connectanceMetaWeb: Double,
  numberOfPopulations: Int,
  basalHomeRange: Double,
  landscapeRadius: Int,
  fractionProtected: Double,
  connectivity: Double,
  decayDistancePopulations: Double
):

  def extinctionProbability(status: ProtectionStatus): Double =
    status match
      case Protected => 0.0
      case Unprotected => 0.9
      case null => 0.0

