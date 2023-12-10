package management.scales

import management.scales.ManagementLandscape.protectionProbability
import org.locationtech.jts.geom.{Geometry, GeometryFactory}
import org.locationtech.jts.operation.distance.DistanceOp

import scala.jdk.CollectionConverters.*
import scala.util.Random

case class ManagementLandscape(
                                managementAreas: List[ManagementArea]
                              ):

  def applyProtectionPlan(numberOfProtectedAreas: Int, clusteringLevel: Double, rnd: Random ): ManagementLandscape =

    val shuffledAreas: List[ManagementArea] = rnd.shuffle(this.managementAreas)
    //rnd.shuffle( this.managementAreas.zipWithIndex.map(_.swap) )
    val zipAreasWithProtectionProbability: List[(ManagementArea,Double)] = shuffledAreas.map(
      a => (a, protectionProbability(DistanceOp(a.shape.getCentroid,shuffledAreas.head.shape.getCentroid).distance(), clusteringLevel ) )
    )

    // TODO: Need to implement a real parametrized random clustering

    val managementAreas: List[ManagementArea] =
      zipAreasWithProtectionProbability
        .sortWith( _._2 < _._2 )
        .map( (a, p) => a)
        .zipWithIndex
        .map(
         (a, id) => if id < numberOfProtectedAreas then a.updateProtectionStatus() else a
        )

    this.copy(managementAreas=managementAreas)

object ManagementLandscape:

  def apply(numberOfAreas: Int, landscapeRadius: Double, populations: List[Population], rnd: Random): ManagementLandscape =

    val voronoiDiagram: Geometry = Utils.getVoronoiTesselation(landscapeRadius,numberOfAreas,rnd)

    val managementAreas: List[ManagementArea] =
      (0 until voronoiDiagram.getNumGeometries)
        .map( n =>
          ManagementArea(
            n,
            voronoiDiagram.getGeometryN(n),
            ProtectionStatus.Unprotected,
            populations
              .collect{ case p if GeometryFactory().createPoint(p.coordinates).within(voronoiDiagram.getGeometryN(n)) => p }
          )
        )
        .toList

    ManagementLandscape(managementAreas)

  def protectionProbability(distance: Double, clusteringLevel: Double): Double =
    math.exp(- clusteringLevel * distance)

end ManagementLandscape

