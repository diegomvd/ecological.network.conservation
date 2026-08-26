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
    def complementarityRecursion(
                                  managementAreas: Seq[ManagementArea],
                                  areasInteractions: Map[Int, Set[(Int, Int)]],
                                  protectedInteractions: Set[(Int, Int)],
                                  remainingTiles: Int
                                ): Seq[ManagementArea] =

      if (remainingTiles <= 0 || areasInteractions.isEmpty) then
        managementAreas
      else
        // For each remaining tile, count how many new interactions it would add
        val marginalGains: Map[Int, Int] = areasInteractions.map { case (tileId, interactions) =>
          (tileId, (interactions -- protectedInteractions).size)
        }

        val maxGain = marginalGains.values.max
        // All tiles tied at the maximum gain — break ties randomly
        val candidates = marginalGains.filter(_._2 == maxGain).keys.toSeq
        val tileId = candidates(rnd.nextInt(candidates.size))

        val newProtectedInteractions = protectedInteractions ++ areasInteractions(tileId)
        val newAreasInteractions = areasInteractions.removed(tileId)
        val newManagementAreas = managementAreas.map {
          a => if a.id == tileId then a.updateProtectionStatus() else a
        }

        complementarityRecursion(
          newManagementAreas,
          newAreasInteractions,
          newProtectedInteractions,
          remainingTiles - 1
        )

    // Rarity recursion: select tiles by highest precomputed rarity score (stochastic proportional selection)
    @tailrec
    def rarityRecursion(
                         managementAreas: Seq[ManagementArea],
                         rarityScores: Map[Int, Double],
                         remainingTiles: Int
                       ): Seq[ManagementArea] =
      if remainingTiles <= 0 || rarityScores.isEmpty then managementAreas
      else
        val activeScores = rarityScores.filter((id, _) => managementAreas.exists(a => a.id == id && a.status != ProtectionStatus.Protected))
        if activeScores.isEmpty then managementAreas
        else
          val tileId = Utils.chooseStochasticEvent(activeScores, rnd)
          if tileId == -1 then managementAreas
          else
            val newManagementAreas = managementAreas.map(a => if a.id == tileId then a.updateProtectionStatus() else a)
            rarityRecursion(newManagementAreas, rarityScores.removed(tileId), remainingTiles - 1)

    @tailrec
    def dissimilarityComplementarityRecursion[T](
      managementAreas: Seq[ManagementArea],
      areasElements: Map[Int, Set[T]],
      protectedElements: Set[T],
      remainingTiles: Int
    ): Seq[ManagementArea] =
      if remainingTiles <= 0 || areasElements.isEmpty then managementAreas
      else
        val marginalGains = areasElements.map((id, elems) => id -> (elems -- protectedElements).size)
        val maxGain = marginalGains.values.max
        val candidates = marginalGains.filter(_._2 == maxGain).keys.toSeq
        val tileId = candidates(rnd.nextInt(candidates.size))
        dissimilarityComplementarityRecursion(
          managementAreas.map(a => if a.id == tileId then a.updateProtectionStatus() else a),
          areasElements.removed(tileId),
          protectedElements ++ areasElements(tileId),
          remainingTiles - 1
        )

    def seedAndRunComplementarity[T](
      areas: Seq[ManagementArea],
      areasElements: Map[Int, Set[T]],
      quota: Int
    ): Seq[ManagementArea] =
      if quota <= 0 || areasElements.isEmpty then areas
      else
        val seedId = areasElements.keys.toSeq.apply(rnd.nextInt(areasElements.size))
        dissimilarityComplementarityRecursion(
          areas.map(a => if a.id == seedId then a.updateProtectionStatus() else a),
          areasElements.removed(seedId),
          areasElements(seedId),
          quota - 1
        )

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

        // Calculate total conservation probability
        // Special case: if all weights are zero (Random strategy), use uniform probability
        val totalConservationProbability: Map[Int,Double] =
          if (conservationParameters.isRandomProtection) {
            // Random protection: uniform probability for all remaining tiles
            biodivConservationProbability.map(bio => (bio._1, 1.0))
          } else {
            // Strategic protection: weighted combination of criteria
            biodivConservationProbability.map(
              bio =>
                (bio._1,
                  bio._2 * conservationParameters.weightSpeciesRichness
                    + conservationParameters.weightConnectivity * connectivityConservationProbability.getOrElse(bio._1, 0.0)
                    + conservationParameters.weightInteractionRichness * interactionConservationProbability.getOrElse(bio._1, 0.0)
                    + conservationParameters.weightAbundance * abundanceConservationProbability.getOrElse(bio._1, 0.0))
            )
          }

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

    val totalTiles: Int = this.managementAreas.size
    val nTiles: Int = (totalTiles * conservationParameters.fractionProtected).toInt

    // Precompute species rarity scores across ALL areas (once, landscape-level)
    // score(tile) = sum over species s in tile of 1 / (number of tiles containing s)
    lazy val speciesRarityScores: Map[Int, Double] =
      val speciesOccupancy: Map[Int, Int] =
        this.managementAreas.flatMap(a => a.getSpecies).groupBy(identity).map((s, ts) => s -> ts.size)
      this.managementAreas.map { a =>
        val score = a.getSpecies.toSeq.map(s => 1.0 / speciesOccupancy.getOrElse(s, 1)).sum
        a.id -> score
      }.toMap

    // Precompute interaction rarity scores across ALL areas (once, landscape-level)
    // score(tile) = sum over interaction pairs (A,B) in tile of 1 / (number of tiles containing both A and B)
    lazy val interactionRarityScores: Map[Int, Double] =
      val tileInteractions: Map[Int, Set[(Int, Int)]] =
        this.managementAreas.map(a => a.id -> a.getInteractions(this.populationWeb)).toMap
      val pairOccupancy: Map[(Int, Int), Int] =
        tileInteractions.values.flatten.groupBy(identity).map((p, ps) => p -> ps.size)
      this.managementAreas.map { a =>
        val score = tileInteractions.getOrElse(a.id, Set.empty).toSeq.map(p => 1.0 / pairOccupancy.getOrElse(p, 1)).sum
        a.id -> score
      }.toMap

    // Helper: run protection for a subset of areas with a given tile quota,
    // returning the updated areas (only the subset is modified).
    def protectSubset(areas: Seq[ManagementArea], quota: Int): Seq[ManagementArea] =
      if quota <= 0 then areas
      else if conservationParameters.isComplementarityProtection then
        val areasInteractions: Map[Int, Set[(Int, Int)]] =
          areas.map(a => (a.id, a.getInteractions(this.populationWeb))).toMap
        complementarityRecursion(areas, areasInteractions, Set.empty[(Int, Int)], quota)
      else if conservationParameters.isSpeciesComplementarityProtection then
        val areasSpecies: Map[Int, Set[Int]] = areas.map(a => a.id -> a.getSpecies).toMap
        seedAndRunComplementarity(areas, areasSpecies, quota)
      else if conservationParameters.isEgocentricInteractionComplementarityProtection then
        val areasEgoInt: Map[Int, Set[(Int, Int)]] =
          areas.map(a => a.id -> a.getEgocentricInteractions(this.populationWeb)).toMap
        seedAndRunComplementarity(areas, areasEgoInt, quota)
      else if conservationParameters.isSpeciesRarityProtection then
        rarityRecursion(areas, speciesRarityScores, quota)
      else if conservationParameters.isInteractionRarityProtection then
        rarityRecursion(areas, interactionRarityScores, quota)
      else
        val areasSpeciesRichness: Map[Int,Int] = areas.map(a => (a.id, a.getSpeciesRichness)).toMap
        val areasConnectivity: Map[Int,Int]    = areas.map(a => (a.id, 0)).toMap
        val areasInteractionRichness: Map[Int,Int] =
          if conservationParameters.isEgocentricInteractionRichnessProtection then
            areas.map(a => (a.id, a.getEgocentricInteractionRichness(this.populationWeb))).toMap
          else
            areas.map(a => (a.id, a.getInteractionRichness(this.populationWeb))).toMap
        val areasAbundance: Map[Int,Int]       = areas.map(a => (a.id, a.getAbundance)).toMap
        protectionRecursion(areas, areasSpeciesRichness, areasConnectivity, areasInteractionRichness, areasAbundance, quota)

    val nRegions = conservationParameters.nAdminRegions

    val newManagementAreas: Seq[ManagementArea] =
      if nRegions <= 1 then
        // Original global behaviour
        protectSubset(this.managementAreas, nTiles)
      else
        // Partition grid into regions and allocate quotas using random rounding
        // so that the total equals nTiles exactly.
        val regions: Seq[Seq[Int]] = grid.partitionIntoRegions(nRegions)
        val exactPerRegion: Seq[Double] = regions.map(r => r.size.toDouble * conservationParameters.fractionProtected)

        // Stochastic rounding: floor each quota then distribute remainder tiles randomly
        val floored: Seq[Int] = exactPerRegion.map(_.toInt)
        val remainder: Int = nTiles - floored.sum
        val fractionalParts: Seq[(Double, Int)] = exactPerRegion.map(x => x - x.toInt).zipWithIndex.sortBy(-_._1)
        val roundUpIndices: Set[Int] = fractionalParts.take(remainder).map(_._2).toSet
        val quotas: Seq[Int] = floored.zipWithIndex.map { case (q, i) => if roundUpIndices(i) then q + 1 else q }

        // Build a mutable map for fast lookup and update
        val areaById: scala.collection.mutable.Map[Int, ManagementArea] =
          scala.collection.mutable.Map(this.managementAreas.map(a => a.id -> a)*)

        regions.zip(quotas).foreach { case (regionIds, quota) =>
          val regionAreas = regionIds.flatMap(id => areaById.get(id)).toSeq
          val updated = protectSubset(regionAreas, quota)
          updated.foreach(a => areaById(a.id) = a)
        }

        this.managementAreas.map(a => areaById.getOrElse(a.id, a))

    this.copy(managementAreas = newManagementAreas)

  // Returns a noisy planning copy of the landscape:
  //   - false negatives: each real population is independently dropped with probability `fnRate`
  //   - false positives: each tile receives Poisson(`fpRate`) ghost populations, each assigned
  //     a species drawn uniformly from `allSpeciesIds`. Ghost population IDs are negative integers
  //     so they are never found in the populationWeb — IR scoring is unaffected, only SR is inflated.
  // Used to generate the planner's view of the landscape for tile scoring.
  def perturbedCopy(fnRate: Double, fpRate: Double, allSpeciesIds: Seq[Int], rnd: Random): ManagementLandscape =
    if fnRate <= 0.0 && fpRate <= 0.0 then this
    else
      var ghostId = -1
      val perturbedAreas = this.managementAreas.map { area =>
        val afterFN =
          if fnRate > 0.0 then area.populations.filter(_ => rnd.nextDouble() >= fnRate)
          else area.populations
        val afterFP =
          if fpRate > 0.0 && allSpeciesIds.nonEmpty then
            val nGhosts = {
              // Poisson sample via Knuth's algorithm
              val l = math.exp(-fpRate)
              var k = 0; var p = 1.0
              while p > l do { k += 1; p *= rnd.nextDouble() }
              k - 1
            }
            val ghosts = (0 until nGhosts).map { _ =>
              val sp = allSpeciesIds(rnd.nextInt(allSpeciesIds.size))
              val id = ghostId; ghostId -= 1
              id -> sp
            }.toMap
            afterFN ++ ghosts
          else afterFN
        area.copy(populations = afterFP)
      }
      this.copy(managementAreas = perturbedAreas)

  // Copies protection statuses (by tile ID) from `source` to this landscape.
  // Used to apply a protection plan computed on a perturbed landscape to the true one.
  def applyProtectionStatusFrom(source: ManagementLandscape): ManagementLandscape =
    val protectedIds = source.managementAreas
      .collect { case a if a.status == ProtectionStatus.Protected => a.id }.toSet
    val updated = this.managementAreas.map { a =>
      if protectedIds.contains(a.id) then a.updateProtectionStatus() else a
    }
    this.copy(managementAreas = updated)

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