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

    val finalCoordinates: Seq[Coordinate] =
      if worldParameters.useBranchingWalk then
        createPopulationsBranchingWalk(species, targetPopulations, rnd, worldParameters)
      else
        createPopulationsThomasLegacy(species, targetPopulations, rnd, worldParameters)

    finalCoordinates.zip(populationSeq).map { case (coordinate, population) =>
      population.copy(coordinates = coordinate, extinct = false)
    }

  // Go-forward model: Eden-growth branching random walk. Anchor is a uniform random point;
  // each subsequent individual is placed a fixed step (= homeRange · spatialSpread) from a
  // randomly chosen already-placed individual, producing organic, irregular clusters.
  private def createPopulationsBranchingWalk(
    species: Species,
    targetPopulations: Int,
    rnd: Random,
    worldParameters: WorldParameters
  ): Seq[Coordinate] =
    val anchor = SquareGrid.randomPoint(rnd)
    val step = worldParameters.placementStep(species.homeRange)
    Utils.generateBranchingWalk(targetPopulations, anchor, step, rnd)

  // DEPRECATED spatial model — single-center Thomas process (points radiate from one random
  // center). Retained only so it can be compared against the branching walk during
  // calibration; remove once the walk is signed off. Cluster radius uses the same
  // center-radiating geometry the original model had (~ homeRange · N · spatialSpread).
  private def createPopulationsThomasLegacy(
    species: Species,
    targetPopulations: Int,
    rnd: Random,
    worldParameters: WorldParameters
  ): Seq[Coordinate] =
    val clusterRadius = species.homeRange * targetPopulations.toDouble * worldParameters.spatialSpread
    Utils.generateThomasProcessCandidates(targetPopulations, 1, clusterRadius, rnd)

end Population
