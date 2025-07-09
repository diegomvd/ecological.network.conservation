package network.conservation

import org.locationtech.jts.geom.{Geometry, GeometryFactory, Polygon}
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
      val neighbors: Seq[Long] = HexagonalGrid.neighbors(tileId,HexagonalGrid.radius(this.managementAreas.size),1)
      
      val newConnectivity: Map[Int,Int] = areasConnectivity
      .filterNot(_._1 == tileId)  // Remove selected tile
      .map { case (areaId, currentCount) =>
        if (neighbors.contains(areaId)) 
          (areaId, currentCount + 1)  // Increment for neighbors
        else 
          (areaId, currentCount)      // Keep same count for non-neighbors
      }
      
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

        val newAreasSpeciesRichness = areasSpeciesRichness.filterNot(_._1 == tileId)
        val newAreasConnectivity = updateConnectivity(areasConnectivity, tileId) 
        val newManagementAreas = managementAreas.map{
          a => if a.id == tileId then a.updateProtectionStatus() else a
        }

        protectionRecursion(newManagementAreas,newAreasSpeciesRichness,newAreasConnectivity,remainingTiles-1)

    val nTiles: Int = (this.managementAreas.size*worldParameters.fractionProtected).toInt 
    
    val areasSpeciesRichness: Map[Int,Int] = this.managementAreas.map(a => (a.id, a.getSpeciesRichness)).toMap
    val areasConnectivity: Map[Int,Int] = this.managementAreas.map(a => (a.id,0)).toMap

    val newManagementAreas: Seq[ManagementArea] = protectionRecursion(this.managementAreas,areasSpeciesRichness,areasConnectivity,nTiles)

    this.copy(managementAreas=newManagementAreas)


  def updatePersistentPopulations(extinctPopulations: Seq[(Int,Int)]): ManagementLandscape =

    val updated = this.managementAreas.map{
     ma => ma.updatePersistentPopulations(extinctPopulations = extinctPopulations)
    }
    this.copy(managementAreas = updated)

object ManagementLandscape:

  def apply(landscapeGrid: Map[Int,Polygon], populations: Seq[Population], rnd: Random): ManagementLandscape =
    
    // Create management areas normally
    val areas = landscapeGrid.map { case (id, hex) =>
      val pops = populations.collect {
        case p if GeometryFactory().createPoint(p.coordinates).within(hex) => (p.id, p.species.id)
      }
      ManagementArea(id, ProtectionStatus.Unprotected, pops.toMap)
    }
    
    ManagementLandscape(areas.toSeq)


end ManagementLandscape

