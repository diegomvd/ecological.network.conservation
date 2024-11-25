package network.conservation

import org.locationtech.jts.geom.{Geometry, GeometryFactory}
import org.locationtech.jts.operation.distance.DistanceOp

import scala.jdk.CollectionConverters.*
import scala.util.Random

import Utils.randomPoint
import scala.annotation.tailrec

case class ManagementLandscape(
                                managementAreas: Seq[ManagementArea]
                              ):

  def applyProtectionPlan(worldParameters: WorldParameters, rnd: Random ): ManagementLandscape =
    /*
    Protection plan to preserve maximum number of species.
    */

    def updateConnectivity(areasConnectivity: Map[Int,Int], tileId: Int): Map[Int,Int] =
      val neighbors: Seq[Int] = HexagonalGrid.neighbors(tileId,HexagonalGrid.radius(this.managementAreas.size),1)
      val newConnectivity: Map[Int,Int] = areasConnectivity.collect{
        case x if neighbors.contains(x._1) => (x._1,x._2+1)
      }.toMap.removed(tileId)
      newConnectivity 

    @tailrec  
    def protectionRecursion(
      managementAreas: Seq[ManagementArea],
      areasSpeciesRichness: Map[Int,Int],
      areasConnectivity: Map[Int,Int],
      remainingTiles: Int
    ): Seq[ManagementArea] =
      
      if !(remainingTiles>0) then
        managementAreas 
      else
        val biodivConservationProbability: Map[Int,Double] = areasSpeciesRichness.map( x => (x._1, 1-math.exp(-x._2)) )
        val connectivityConservationProbability: Map[Int,Double] = areasConnectivity.map( x => (x._1, 1-math.exp(-x._2)))

        val totalConservationProbability: Map[Int,Double] = biodivConservationProbability.map(
          bio => (bio._1, bio._2 + worldParameters.connectivity * connectivityConservationProbability.getOrElse(bio._1,0.0))
        )

        val tileId: Int = Utils.chooseStochasticEvent(totalConservationProbability,rnd)

        val newAreasSpeciesRichness = areasSpeciesRichness.removed(tileId)
        val newAreasConnectivity = updateConnectivity(areasConnectivity, tileId) 
        val newManagementAreas = managementAreas.collect{
          case a if a.id == tileId => a.updateProtectionStatus()
        }

        protectionRecursion(newManagementAreas,newAreasSpeciesRichness,newAreasConnectivity,remainingTiles-1)

    val nTiles: Int = (this.managementAreas.size*worldParameters.fractionProtected).toInt 
    val areasSpeciesRichness: Map[Int,Int] = this.managementAreas.map(a => (a.id, a.getSpeciesRichness)).toMap
    val areasConnectivity: Map[Int,Int] = this.managementAreas.map(a => (a.id,0)).toMap

    val newManagementAreas: Seq[ManagementArea] = protectionRecursion(this.managementAreas,areasSpeciesRichness,areasConnectivity,nTiles)

    this.copy(managementAreas=managementAreas)

  def updatePersistentPopulations(extinctPopulations: Seq[(Int,Int)]): ManagementLandscape =

    val updated = this.managementAreas.map{
     ma => ma.updatePersistentPopulations(extinctPopulations = extinctPopulations)
    }
    this.copy(managementAreas = updated)

object ManagementLandscape:

  def apply(landscapeRadius: Int, populations: Seq[Population], rnd: Random): ManagementLandscape =

    val landscapeGrid: Map[Int,Geometry] = HexagonalGrid(landscapeRadius)

    val managementAreas: Seq[ManagementArea] =
        landscapeGrid.map( n =>
          ManagementArea(n._1, ProtectionStatus.Unprotected, populations
            .collect { case p if GeometryFactory().createPoint(p.coordinates).within(n._2) => (p.id, p.species.id) }.toMap)
        )
        .toSeq

    ManagementLandscape(managementAreas)


end ManagementLandscape

