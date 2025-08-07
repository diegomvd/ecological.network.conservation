package network.conservation

import org.locationtech.jts.geom.{Coordinate,Geometry}

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

  def createPopulations(species: Species, populationSeq: Seq[Population], rnd: Random): Seq[Population] =
  
    val targetPopulations = populationSeq.size
    val numCenters = 1
    //val numCenters = math.max(1, 1 + targetPopulations / 100 + rnd.nextInt(3)) // 1-5 centers

    val clusterRadius = species.homeRange * math.sqrt(targetPopulations.toDouble) * 1.5 // todo: Spreading factor could be parameter

    val candidates = Utils.generateThomasProcessCandidates(
      targetPopulations * 4, //TODO: 4x oversampling could be parameter 
      numCenters,
      clusterRadius,
      rnd
    )

    val finalCoordinates = Utils.adaptiveKRandomThinning(
      candidates,
      targetPopulations,
      species.homeRange * 1.5,
      0.1, // check 10%
      rnd
    )

    finalCoordinates.zip(populationSeq).map { case (coordinate, population) => 
      population.copy(coordinates = coordinate, extinct = false) 
    }
    
end Population
