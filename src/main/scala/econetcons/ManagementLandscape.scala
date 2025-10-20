package econetcons

import org.locationtech.jts.geom.{Geometry, GeometryFactory, Polygon}
import org.locationtech.jts.operation.distance.DistanceOp
import org.jgrapht.graph.{DefaultDirectedGraph, DefaultEdge}

import scala.jdk.CollectionConverters.*
import scala.util.Random

import Utils.randomPoint
import scala.annotation.tailrec

case class ManagementLandscape(
                                managementAreas: Seq[ManagementArea],
                                grid: SquareGrid,
                                populationWeb: DefaultDirectedGraph[Population, DefaultEdge]
                              ):

  def applyProtectionPlan(conservationParameters: ConservationParameters, rnd: Random): ManagementLandscape =
    /*
    Protection plan to preserve maximum number of species.
    */
    def updateConnectivity(areasConnectivity: Map[Int,Int], tileId: Int): Map[Int,Int] =
      // Use the stored grid instance to get neighbors
      val neighbors: Seq[Int] = grid.neighbors(tileId)

      val newConnectivity: Map[Int,Int] = areasConnectivity
        .filterNot(_._1 == tileId)  // Remove selected tile
        .map { case (areaId, currentCount) =>
          if (neighbors.contains(areaId))
            (areaId, currentCount + 1)  // Increment for neighbors
          else
            (areaId, currentCount)      // Keep same count for non-neighbors
        }

      newConnectivity

    def scalingMinMax(value: Double, min: Double, max: Double): Double =
      if (max - min < 1e-10) {
        // All values are effectively equal - return neutral score
        // This handles the connectivity=0 case and any other uniform distributions
        0.5
      } else {
        (value - min) / (max - min)
      }

    @tailrec
    def protectionRecursion(
                             managementAreas: Seq[ManagementArea],
                             areasSpeciesRichness: Map[Int,Int],
                             areasConnectivity: Map[Int,Int],
                             areasInteractionRichness: Map[Int,Int],
                             areasAbundance: Map[Int,Int],
                             remainingTiles: Int
                           ): Seq[ManagementArea] =

      // Guard: stop if no tiles remain or no more areas to protect
      if (remainingTiles <= 0 || areasSpeciesRichness.isEmpty) then
        managementAreas
      else

        // Each tile is associated with a score that is a weighted combination of
        // species richness, connectivity, interaction richness and abundance.
        // Each component is scaled to be between 0 and 1

        // Prioritization contribution of species richness
        val maxSpeciesRichness = areasSpeciesRichness.values.max.toDouble
        val minSpeciesRichness = areasSpeciesRichness.values.min.toDouble
        val biodivConservationProbability: Map[Int,Double] =
          areasSpeciesRichness.map(x => (x._1, scalingMinMax(x._2, minSpeciesRichness, maxSpeciesRichness)))

        // Prioritization contribution of connectivity
        val maxNeighbors = areasConnectivity.values.max.toDouble
        val minNeighbors = areasConnectivity.values.min.toDouble
        val connectivityConservationProbability: Map[Int,Double] =
          areasConnectivity.map(x => (x._1, scalingMinMax(x._2, minNeighbors, maxNeighbors)))

        // Prioritization contribution of interaction richness
        val maxInteractionRichness = areasInteractionRichness.values.max.toDouble
        val minInteractionRichness = areasInteractionRichness.values.min.toDouble
        val interactionConservationProbability: Map[Int,Double] =
          areasInteractionRichness.map(x => (x._1, scalingMinMax(x._2, minInteractionRichness, maxInteractionRichness)))

        // Prioritization contribution of abundance
        val maxAbundance = areasAbundance.values.max.toDouble
        val minAbundance = areasAbundance.values.min.toDouble
        val abundanceConservationProbability: Map[Int,Double] =
          areasAbundance.map(x => (x._1, scalingMinMax(x._2, minAbundance, maxAbundance)))

        val totalConservationProbability: Map[Int,Double] = biodivConservationProbability.map(
          bio =>
            (bio._1,
              bio._2 * conservationParameters.weightSpeciesRichness
                + conservationParameters.weightConnectivity * connectivityConservationProbability.getOrElse(bio._1, 0.0)
                + conservationParameters.weightInteractionRichness * interactionConservationProbability.getOrElse(bio._1, 0.0)
                + conservationParameters.weightAbundance * abundanceConservationProbability.getOrElse(bio._1, 0.0))
        )

        // Guard: ensure we have valid probabilities
        if (totalConservationProbability.isEmpty) then
          println("Warning: No valid tiles to protect. Stopping early.")
          managementAreas
        else
          val tileId: Int = Utils.chooseStochasticEvent(totalConservationProbability, rnd)

          // Guard: handle failed selection (should never happen with robust chooseStochasticEvent)
          if (tileId == -1) then
            println(s"Warning: Tile selection failed with ${remainingTiles} tiles remaining.")
            println(s"  Available areas: ${areasSpeciesRichness.size}")
            println(s"  Probability map size: ${totalConservationProbability.size}")
            println(s"  Total probability sum: ${totalConservationProbability.values.sum}")
            managementAreas
          else
            val newAreasSpeciesRichness = areasSpeciesRichness.filterNot(_._1 == tileId)
            val newAreasConnectivity = updateConnectivity(areasConnectivity, tileId)
            val newAreasInteractionRichness = areasInteractionRichness.filterNot(_._1 == tileId)
            val newAreasAbundance = areasAbundance.filterNot(_._1 == tileId)
            val newManagementAreas = managementAreas.map {
              a => if a.id == tileId then a.updateProtectionStatus() else a
            }

            protectionRecursion(
              newManagementAreas,
              newAreasSpeciesRichness,
              newAreasConnectivity,
              newAreasInteractionRichness,
              newAreasAbundance,
              remainingTiles - 1
            )

    val nTiles: Int = (this.managementAreas.size * conservationParameters.fractionProtected).toInt

    val areasSpeciesRichness: Map[Int,Int] = this.managementAreas.map(a => (a.id, a.getSpeciesRichness)).toMap
    val areasConnectivity: Map[Int,Int] = this.managementAreas.map(a => (a.id, 0)).toMap
    val areasInteractionRichness: Map[Int,Int] = this.managementAreas.map(a => (a.id, a.getInteractionRichness(this.populationWeb))).toMap
    val areasAbundance: Map[Int,Int] = this.managementAreas.map(a => (a.id, a.getAbundance)).toMap

    val newManagementAreas: Seq[ManagementArea] = protectionRecursion(
      this.managementAreas,
      areasSpeciesRichness,
      areasConnectivity,
      areasInteractionRichness,
      areasAbundance,
      nTiles
    )

    this.copy(managementAreas = newManagementAreas)

  def updatePersistentPopulations(extinctPopulations: Seq[(Int,Int)]): ManagementLandscape =
    val updated = this.managementAreas.map {
      ma => ma.updatePersistentPopulations(extinctPopulations = extinctPopulations)
    }
    this.copy(managementAreas = updated)

object ManagementLandscape:

  def apply(
             landscapeGrid: SquareGrid,
             populations: Seq[Population],
             populationWeb: DefaultDirectedGraph[Population, DefaultEdge],
             rnd: Random
           ): ManagementLandscape =

    // Create management areas using cell indices instead of polygons
    val areas = landscapeGrid.cellIndices.map { cellId =>
      // Find populations in this cell using coordinate-to-cell mapping
      val pops = populations.collect {
        case p if landscapeGrid.coordToCell(p.coordinates) == cellId => (p.id, p.species.id)
      }
      ManagementArea(cellId, ProtectionStatus.Unprotected, pops.toMap)
    }

    ManagementLandscape(areas, landscapeGrid, populationWeb)

end ManagementLandscape