package econetcons

import scala.math.max
import scala.math.min
import scala.math.sqrt

import org.locationtech.jts.geom.{Coordinate, Envelope, Geometry, GeometryFactory, LinearRing, Point, Polygon}
import org.locationtech.jts.geom.impl.CoordinateArraySequence

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
  Map[Int,Polygon]=
    (0 until area(r)).map( id => (id, getHexGeometry(id,r)) ).toMap  


  def cubicToCenterXY(cubic: (Long, Long), radius: Long): Coordinate =
    // First convert to "natural" hex coordinates
    val q = cubic._1
    val r = cubic._2
    
    // Convert to cartesian coordinates (traditional hex layout)
    val x = q * 2.0  // Horizontal spacing
    val y = (r + q/2.0) * sqrt(3.0)  // Vertical spacing with offset
    
    // Calculate scaling to normalize to [0,1] × [0,sqrt(3)/2]
    val maxExtent = 2.0 * radius + 1.0  // Maximum extent in hex units
    val scale = 1.0 / maxExtent
    
    // Center and scale the coordinates
    val normalizedX = (x * scale + 1.0) / 2.0
    val normalizedY = (y * scale + sqrt(3.0)/2.0) / 2.0
    
    Coordinate(normalizedX, normalizedY, 0.0)

  def getHexGeometry(index: Int, radius: Int): Polygon =
    val cubic = HexagonalGrid.toCubic(mod=index,rad=radius)
    val center = cubicToCenterXY(cubic, radius)
    
    // Calculate hex size in normalized coordinates
    val maxExtent = 2.0 * radius + 1.0
    val hexSize = 1.0 / maxExtent  // Size in normalized coordinates
    
    // Create vertices in clockwise order, scaling the hex appropriately
    val coordinates = (5L to 0L by -1).map { i =>
      val angle = Math.PI / 3.0 * i
      new Coordinate(
        center.x + hexSize * Math.cos(angle) / sqrt(3.0),
        center.y + hexSize * Math.sin(angle) / sqrt(3.0),
        0.0
      )
    }
    val coordinatesClosed = coordinates.appended(coordinates.head)

    val shell = new LinearRing(
      new CoordinateArraySequence(coordinatesClosed.toArray),
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
               mod:Long,
               rad:Long
             ):
  (Long,Long)=
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
                cub:(Long,Long),
                rad:Long
              ):
  Long=
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
                 mod:Long,
                 rad:Long,
                 thr:Long
               ):
  Seq[Long]=
    val cub = toCubic(mod, rad)
    (-thr to thr).flatMap( q =>
      (max(-thr,-q-thr) to min(thr,-q+thr)).map( r =>
        toModulo((q+cub._1,r+cub._2),rad)
      )
    ).toSeq

end HexagonalGrid
