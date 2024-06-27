package network.conservation

import ProtectionStatus.{Protected, Unprotected}

case class WorldParameters(numberOfSpecies: Int, connectanceMetaWeb: Double, numberOfPopulations: Int, basalHomeRange: Double, numberOfProtectionPoles: Int, fractionProtected: Double, decayDistancePopulations: Double):

  def getNumberOfManagementAreas(basalHomeRange: Double): Int =
    (1.0 / math.pow(basalHomeRange, 2)).toInt

  def getNumberOfProtectedAreas(nAreas: Int, fractionProtected: Double): Int =
    (nAreas * fractionProtected).toInt

  def extinctionProbability(status: ProtectionStatus): Double =
    status match
      case Protected => 0.0
      case Unprotected => 0.75
      case null => 0.0

