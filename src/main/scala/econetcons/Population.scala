package econetcons

import org.locationtech.jts.geom.Coordinate

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
    worldParameters: WorldParameters
  ): Seq[Population] =

    val targetPopulations = populationSeq.size

    // Matérn cluster process: Poisson number of patches per species, populations scattered
    // multinomially across them, each patch a home-range-sized disk (see Utils.generateMaternProcess).
    val finalCoordinates: Seq[Coordinate] =
      Utils.generateMaternProcess(
        targetPopulations,
        species.homeRange,
        worldParameters.meanPatchesPerSpecies,
        worldParameters.patchPackingFactor,
        rnd
      )

    finalCoordinates.zip(populationSeq).map { case (coordinate, population) =>
      population.copy(coordinates = coordinate, extinct = false)
    }

end Population
