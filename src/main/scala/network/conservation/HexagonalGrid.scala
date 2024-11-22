package network.conservation

import scala.math.max
import scala.math.min
import scala.math.sqrt

import org.locationtech.jts.geom.{Coordinate, Envelope, Geometry, GeometryFactory}

/**
 * Implementation of hexagonal modulo coordinates. Defines all the functions needed to manipulate the modulo coordinates
 * in the hexagonal lattice. Used in EcoLandscape to create and locate the EcoUnits in different positions of an hexagonal
 * lattice and define their neighborhood using distance in this lattice to create the composition graph.
 */

object HexagonalGrid:

  private val geometryFactory = new GeometryFactory()

  /**
   * Builds all of the possible hexagonal modulo coordinates given the radius of the landscape.
   *
   * @constructor
   * @param r is the radius of the landscape
   * @return a List with each of the modulo coordinates
   * */
  def apply(
             r:Int
           ):
  Seq[Int]=
    (0 until area(r)).map(x=>x).toList

  def apply(
             r:Int,origin:(Double,Double)
           ):
  Map[Int,Geometry]=
    // Do whatever to get the geometry map 
    (0 until area(r)).map(x=>x).toList  


  def getHexGeometry(index: Int, radius: Int): Geometry =
    cubic = HexagonalGrid.toCubic(mod=index,rad=radius)
    // from cubic can get the hexagon corners and build an hexagon
    coordinates
    val shell = new LinearRing(
        new CoordinateArraySequence(coordinates.toArray),
        geometryFactory
      )  
    geometryFactory.createPolygon(shell)
    

  /**
   * Calculates area of an hexagonal landscape given its radius
   *
   * @param r is the landscape's radius
   * @return the landscape's area
   * */
  def area(
            r:Int
          ):
  Int=
    3*r*r + 3*r + 1

  def radius(
              a: Int
            ):
  Int =
    ( (sqrt(9.0 + 12.0*(a-1.0) ) - 3.0)/6 ).toInt

  /**
   * Transforms a modulo coordinate to a cubic coordinate of the hexagonal landscape
   *
   * @param mod the modulo coordinate
   * @param rad the landscape's radius
   * @return a tuple containing the cubic coordinates
   * */
  def toCubic(
               mod:Int,
               rad:Int
             ):
  (Int,Int)=
    // helper values
    val shift = 3*rad + 2
    val ms = (mod + rad) / shift
    val mcs = (mod + 2*rad) / (shift-1)

    // Need to make sure of this formula and check what is each coordinate
    // spatially
    val q = ms * (rad+1) - mcs * rad
    val r = mod + ms * (-2*rad-1) + mcs * (-rad-1)
    (q,r)

  /**
   * Transforms a cubic coordinate to a modulo coordinate of the hexagonal landscape
   *
   * @param cub the cubic coordinate
   * @param rad the landscape's radius
   * @return the modulo coordinate
   * */
  def toModulo(
                cub:(Int,Int),
                rad:Int
              ):
  Int=
    // helper values
    val area = 3*rad*rad + 3*rad + 1
    val shift = 3*rad + 2
    val div = cub(1) + shift*cub(0)
    // "%" operator is the remainder operator in scala, with the function below we obtain the modulo. This is important
    // in the case of negative cubic coordinates, as the remainder would return a negative number
    ((div%area)+area)%area

  /**
   * Calculates the hexagonal cells in the neighborhood of a cell.
   *
   * @param mod the modulo coordinate.
   * @param rad the landscape's radius.
   * @param thr the threshold distance at which cells are considered neighbors.
   * @return a List with the modulo coordinates of the neighbors.
   * */
  def neighbors(
                 mod:Int,
                 rad:Int,
                 thr:Int
               ):
  List[Int]=
    val cub = toCubic(mod, rad)
    (-thr to thr).flatMap( q =>
      (max(-thr,-q-thr) to min(thr,-q+thr)).map( r =>
        toModulo((q+cub._1,r+cub._2),rad)
      )
    ).toList

end HexagonalGrid
