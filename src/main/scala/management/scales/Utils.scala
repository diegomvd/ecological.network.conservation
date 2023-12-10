package management.scales

import org.locationtech.jts.geom.{Coordinate, Envelope, Geometry, GeometryCollection, GeometryFactory}
import scala.jdk.CollectionConverters.*
import org.locationtech.jts.triangulate.VoronoiDiagramBuilder

import scala.util.Random

object Utils:

  def randomPoint(landscapeRadius: Double, rnd: Random): Coordinate =
    val x = rnd.nextDouble() * landscapeRadius
    val y = rnd.nextDouble() * landscapeRadius
    Coordinate(x, y)

  def getVoronoiTesselation(landscapeRadius: Double,  numberOfAreas: Int, rnd: Random): Geometry =
    val sites: java.util.Collection[Coordinate] = (0 until numberOfAreas).map {
      n =>
        Utils.randomPoint(landscapeRadius, rnd)
    }.asJavaCollection

    val vDB: VoronoiDiagramBuilder = VoronoiDiagramBuilder()
    vDB.setSites(sites)
    vDB.setClipEnvelope(Envelope(0.0, landscapeRadius, 0.0, landscapeRadius))
    vDB.getDiagram(GeometryFactory())

end Utils
