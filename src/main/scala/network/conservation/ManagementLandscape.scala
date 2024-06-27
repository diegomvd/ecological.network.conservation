package network.conservation

import org.locationtech.jts.geom.{Geometry, GeometryFactory}
import org.locationtech.jts.operation.distance.DistanceOp

import scala.jdk.CollectionConverters.*
import scala.util.Random

import Utils.randomPoint

case class ManagementLandscape(
                                managementAreas: Seq[ManagementArea]
                              ):

  def applyProtectionPlan(numberOfProtectedAreas: Int, nProtectionPoles: Int, rnd: Random ): ManagementLandscape =

    val shuffledAreas: Seq[ManagementArea] = rnd.shuffle(this.managementAreas)
    //rnd.shuffle( this.managementAreas.zipWithIndex.map(_.swap) )

    val polesCoordinates = (0 until nProtectionPoles).map(
      n => randomPoint(rnd)
    )

    val distanceToClosestPole: Seq[(ManagementArea,Double)] = shuffledAreas.map(
      a => {
        (a, polesCoordinates.map( c => DistanceOp(a.shape.getCentroid, GeometryFactory().createPoint(c)).distance() ).min)
      }
    )

    val managementAreas: Seq[ManagementArea] =
      distanceToClosestPole
        .sortWith( _._2 < _._2 )
        .map( (a, p) => a)
        .zipWithIndex
        .map(
         (a, id) => if id < numberOfProtectedAreas then a.updateProtectionStatus() else a
        )

    this.copy(managementAreas=managementAreas)

  def updatePersistentPopulations(extinctPopulations: Seq[(Int,Int)]): ManagementLandscape =

    val updated = this.managementAreas.map{
     ma => ma.updatePersistentPopulations(extinctPopulations = extinctPopulations)
    }
    this.copy(managementAreas = updated)

object ManagementLandscape:

  def apply(numberOfAreas: Int, populations: Seq[Population], rnd: Random): ManagementLandscape =

    val voronoiDiagram: Geometry = Utils.getVoronoiTesselation(numberOfAreas,rnd)

    val managementAreas: List[ManagementArea] =
      (0 until voronoiDiagram.getNumGeometries)
        .map( n =>
          ManagementArea(n, voronoiDiagram.getGeometryN(n), ProtectionStatus.Unprotected, populations
            .collect { case p if GeometryFactory().createPoint(p.coordinates).within(voronoiDiagram.getGeometryN(n)) => (p.species.id, p.id) }.toMap)
        )
        .toList

    ManagementLandscape(managementAreas)


end ManagementLandscape

