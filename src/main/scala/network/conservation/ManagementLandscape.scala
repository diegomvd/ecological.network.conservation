package network.conservation

import org.locationtech.jts.geom.{Geometry, GeometryFactory}
import org.locationtech.jts.operation.distance.DistanceOp

import scala.jdk.CollectionConverters.*
import scala.util.Random

import Utils.randomPoint

case class ManagementLandscape(
                                managementAreas: Seq[ManagementArea]
                              ):

  def applyProtectionPlan(totalArea: Double, connectivity: Double, rnd: Random ): ManagementLandscape =
    /*
    Protection plan to preserve maximum number of species.
    */

    @tailrec  
    def protectionRecursion(areasSpeciesRichness: Map[Int,Int], areasConnectivity: Map[Int,Int]):
      
      val biodivConservationProbability = areasSpeciesRichness.map( x => (x._1, 1-math.exp(-x._2)) )
      val connectivityConservationProbability = areasConnectivity( x => (x._1, 1-math.exp(-x._2)))

      val totalConservationProbability = biodivConservationProbability.map(
        bio => bio._2 + connectivity * connectivityConservationProbability.getOrElse(bio._1,0.0)
      )

      val cummulativeProbability(): 

    val rankedAreas: Map[Int,Int] = managementAreas.sortWith( _.getSpeciesRichness < _.getSpeciesRichness).map(ma => (ma.id,ma.getSpeciesRichness)).toMap



    val shuffledAreas: Seq[ManagementArea] = rnd.shuffle(this.managementAreas)
    //rnd.shuffle( this.managementAreas.zipWithIndex.map(_.swap) )

    

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

  def apply(landscapeRadius: Int, populations: Seq[Population], rnd: Random): ManagementLandscape =

    val landscapeGrid: Map[Int,Geometry] = HexagonalGrid(landscapeRadius,(0.0,0.0))

    val managementAreas: Seq[ManagementArea] =
        landscapeGrid.map( n =>
          ManagementArea(n._1, ProtectionStatus.Unprotected, populations
            .collect { case p if GeometryFactory().createPoint(p.coordinates).within(n._2) => (p.id, p.species.id) }.toMap)
        )
        .toList

    ManagementLandscape(managementAreas)


end ManagementLandscape

