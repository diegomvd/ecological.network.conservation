package management.scales

import org.locationtech.jts.geom.Coordinate

import scala.util.Random

case class Population(
                      id: Long,
                      species: Species,
                      coordinates: Coordinate,
                      extinct: Boolean
                     ):

  def extinguish: Population =
    this.copy(extinct=true)

object Population:

  def apply(id: Long, species: Species, landscapeRadius: Double, rnd: Random): Population =
    val coordinates: Coordinate = Utils.randomPoint(landscapeRadius, rnd)
    Population(id, species, coordinates, false)

end Population
