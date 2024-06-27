package network.conservation

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

  def apply(id: Int, species: Species, populationSeq: Seq[Population], rnd: Random, lambda: Double): Population =
    val coordinates: Coordinate = Utils.diskPacking(lambda,populationSeq,rnd,species)
    Population(id, species, coordinates, false)

end Population
