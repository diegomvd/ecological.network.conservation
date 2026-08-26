package econetcons

import org.locationtech.jts.geom.{Coordinate, Geometry}

import scala.util.Random

case class Population(
                      id: Int,
                      species: Species,
                      coordinates: Coordinate,
                      extinct: Boolean
                     ):

  def extinguish: Population =
    this.copy(extinct=true)

object Population:

  def createPopulations(
    species: Species,
    populationSeq: Seq[Population],
    rnd: Random,
    worldParameters: WorldParameters,
    sharedHotspotCenters: Seq[Coordinate] = Seq.empty
  ): Seq[Population] =

    val targetPopulations = populationSeq.size
    val numCenters = 1

    val clusterRadius = species.homeRange * (targetPopulations.toDouble) * worldParameters.spreadingFactor

    // Generate candidates - oversample only if thinning is enabled
    val numCandidates = if (worldParameters.useThinning) targetPopulations * 4 else targetPopulations
    val candidates = Utils.generateThomasProcessCandidates(
      numCandidates,
      numCenters,
      clusterRadius,
      rnd,
      hotspotCenters      = sharedHotspotCenters,
      aggregationStrength = worldParameters.aggregationStrength,
      hotspotVariance     = worldParameters.hotspotVariance
    )

    // Apply thinning only if enabled
    val finalCoordinates = if (worldParameters.useThinning) {
      Utils.adaptiveKRandomThinning(
        candidates,
        targetPopulations,
        species.homeRange * 1.5,
        0.1, // check 10%
        rnd
      )
    } else {
      // No thinning - use candidates directly
      candidates
    }

    finalCoordinates.zip(populationSeq).map { case (coordinate, population) =>
      population.copy(coordinates = coordinate, extinct = false)
    }

end Population
