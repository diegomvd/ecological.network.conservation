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

    val finalCoordinates: Seq[Coordinate] = worldParameters.resolvedPlacement match
      // Complete spatial randomness: every population at an independent uniform point.
      case "uniform" => Seq.fill(targetPopulations)(Utils.randomPoint(rnd))
      // Matérn cluster process: Poisson number of patches per species, populations scattered
      // multinomially across them, each patch a home-range-sized disk (see Utils.generateMaternProcess).
      case "matern" =>
        Utils.generateMaternProcess(
          targetPopulations,
          species.homeRange,
          worldParameters.meanPatchesPerSpecies,
          worldParameters.patchPackingFactor,
          rnd
        )
      case other => throw new IllegalArgumentException(s"unknown placement '$other'")

    finalCoordinates.zip(populationSeq).map { case (coordinate, population) =>
      population.copy(coordinates = coordinate, extinct = false)
    }

end Population
