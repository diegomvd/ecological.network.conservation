package network.conservation

import org.locationtech.jts.geom.{Coordinate, Envelope, Geometry, GeometryFactory}

import scala.jdk.CollectionConverters.*
import org.locationtech.jts.triangulate.VoronoiDiagramBuilder
import org.locationtech.jts.operation.distance.DistanceOp

import scala.annotation.tailrec
import scala.language.postfixOps
import scala.util.Random

object Utils:

  def randomPoint(rnd: Random): Coordinate =
    val x = rnd.nextDouble()
    val y = rnd.nextDouble()
    Coordinate(x, y)

  private def exponentialDistribution(lambda: Double, rnd: Random): Double =
    math.log(1 - rnd.nextDouble()) / (-lambda)

  private def drawPopulationCoordinates(referenceCoordinate: Coordinate, lambda: Double, rnd: Random, homeRange: Double): (Double, Double) =
    // First pick an angle with uniform probability
    val angle = rnd.nextDouble()*math.Pi*2.0
    // Now pick a distance from an exponential distribution and starting at twice the species' home range
    val distance =  exponentialDistribution(lambda, rnd) + 2*homeRange

    val x = referenceCoordinate.x + distance*math.cos(angle)
    val y = referenceCoordinate.y + distance*math.sin(angle)

    (x, y)

  private def correctOutOfBoundsCoordinate(z: Double): Double =
    if z>1.0 then{
      1.0
    } else {
      if z<0.0 then{
        0.0
      } else z
    }

  def euclidianDistance(c1: Coordinate, c2: Coordinate): Double =
    val point1 = GeometryFactory().createPoint(c1)
    val point2 = GeometryFactory().createPoint(c2)
    DistanceOp(point1, point2).distance()

  private def correctOutOfBoundCoordinate(x: Double, y: Double): Coordinate =
    val xx = correctOutOfBoundsCoordinate(x)
    val yy = correctOutOfBoundsCoordinate(y)
    Coordinate(xx,yy)

  def diskPacking(lambda: Double, populationSeq: Seq[Population], rnd: Random, species: Species): Coordinate =

    val speciesHomeRange = species.homeRange

    def getReferenceCoordinate(populationSeq: Seq[Population], rnd: Random): Coordinate =
      if populationSeq.isEmpty then randomPoint(rnd) else rnd.shuffle(populationSeq).head.coordinates

    @tailrec
    def rec(i: Int, lambdaRec: Double): Coordinate =

      // Pick a population at random and get its location
      val referenceCoordinate: Coordinate = getReferenceCoordinate(populationSeq, rnd)

      val (x, y) = drawPopulationCoordinates(referenceCoordinate,lambdaRec,rnd,speciesHomeRange)
      val newCoordinate = correctOutOfBoundCoordinate(x,y)

      // Now check that this population does not overlap with another one.
      val noOverlap: Boolean = populationSeq.forall{
        pop => euclidianDistance(newCoordinate,pop.coordinates)>2*speciesHomeRange
      }

      if noOverlap then {
        newCoordinate
      } else{
        if i>1000 then{
          newCoordinate
        } else {
          rec(i+1, lambdaRec)
        }
      }

    rec(0, lambda)

  def getVoronoiTesselation(numberOfAreas: Int, rnd: Random): Geometry =
    val sites: java.util.Collection[Coordinate] = (0 until numberOfAreas).map {
      n =>
        Utils.randomPoint(rnd)
    }.asJavaCollection

    val vDB: VoronoiDiagramBuilder = VoronoiDiagramBuilder()
    vDB.setSites(sites)
    vDB.setClipEnvelope(Envelope(0.0, 1.0, 0.0, 1.0))
    vDB.getDiagram(GeometryFactory())

end Utils
