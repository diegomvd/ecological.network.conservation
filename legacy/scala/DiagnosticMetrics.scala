package econetcons

import org.jgrapht.graph.{DefaultDirectedGraph, DefaultEdge, SimpleGraph}
import org.jgrapht.alg.connectivity.ConnectivityInspector
import org.jgrapht.alg.scoring.BetweennessCentrality
import scala.jdk.CollectionConverters.*

case class BaselineMetrics(
  spAlphaMean: Double,
  spAlphaStd: Double,
  intAlphaMean: Double,
  intAlphaStd: Double,
  spBeta: Double,
  intBeta: Double
)

case class SimulationMetrics(
  // A. Landscape structure
  largestPatchFraction: Double,
  patchFragmentation: Double,
  // B. Species web metrics
  spMeanInDegree: Double,
  spMeanOutDegree: Double,
  spConnectance: Double,
  spWeakComponents: Double,
  spDegreeP10: Double,
  spDegreeP25: Double,
  spDegreeP50: Double,
  spDegreeP75: Double,
  spDegreeP90: Double,
  // C. Population web metrics
  popMeanInDegree: Double,
  popMeanOutDegree: Double,
  popConnectance: Double,
  popWeakComponents: Double,
  // D. Beta-diversity
  speciesBetaDiversity: Double,
  speciesGammaNorm: Double,
  interactionBetaDiversity: Double,
  interactionGammaNorm: Double
)

object DiagnosticMetrics:

  def computeBaseline(world: World): BaselineMetrics =
    val allAreas = world.managementLandscape.managementAreas
    val popWeb   = world.populationWeb

    val spAlphas  = allAreas.map(_.getSpeciesRichness.toDouble)
    val intAlphas = allAreas.map(_.getInteractionRichness(popWeb).toDouble)

    def mean(xs: Seq[Double]): Double = if xs.isEmpty then 0.0 else xs.sum / xs.size
    def std(xs: Seq[Double]): Double =
      if xs.size < 2 then 0.0
      else
        val m = mean(xs)
        math.sqrt(xs.map(x => (x - m) * (x - m)).sum / (xs.size - 1))

    val spMean  = mean(spAlphas)
    val spStd   = std(spAlphas)
    val intMean = mean(intAlphas)
    val intStd  = std(intAlphas)

    // Beta diversity across all tiles (same Harrison beta-2 formula)
    val spGammaAll  = allAreas.map(_.getSpecies).reduce(_ ++ _).size.toDouble
    val spBeta      = normalizedBeta(spGammaAll, spMean, allAreas.size)

    val intGammaAll = allAreas.map(_.getInteractions(popWeb)).reduce(_ ++ _).size.toDouble
    val intBeta     = normalizedBeta(intGammaAll, intMean, allAreas.size)

    BaselineMetrics(spMean, spStd, intMean, intStd, spBeta, intBeta)

  // Re-tile the world onto a grid with the requested populations-per-tile and
  // compute all-tile (baseline) beta diversity for species and interactions.
  // Lets a single community be characterized at several tile sizes without
  // rebuilding metaweb/populations/web. Returns (speciesBeta, interactionBeta).
  def betaForGrid(world: World, avgPopulationsPerTile: Int): (Double, Double) =
    val popWeb = world.populationWeb
    val networkPopulations = popWeb.vertexSet().asScala.toSeq
    val numberOfPopulations = networkPopulations.size
    val nTiles = math.max(1, (numberOfPopulations.toDouble / avgPopulationsPerTile.toDouble).toInt)
    val grid = SquareGrid.fromArea(nTiles)
    val landscape = ManagementLandscape(grid, networkPopulations, popWeb, scala.util.Random(0))
    val areas = landscape.managementAreas

    def mean(xs: Seq[Double]): Double = if xs.isEmpty then 0.0 else xs.sum / xs.size

    val spAlphas  = areas.map(_.getSpeciesRichness.toDouble)
    val intAlphas = areas.map(_.getInteractionRichness(popWeb).toDouble)
    val spGamma   = areas.map(_.getSpecies).reduce(_ ++ _).size.toDouble
    val intGamma  = areas.map(_.getInteractions(popWeb)).reduce(_ ++ _).size.toDouble

    val spBeta  = normalizedBeta(spGamma, mean(spAlphas), areas.size)
    val intBeta = normalizedBeta(intGamma, mean(intAlphas), areas.size)
    (spBeta, intBeta)

  def compute(world: World): SimulationMetrics =
    val (largestPatchFrac, patchFrag) = computeLandscapeMetrics(world)
    val spMetrics = computeWebMetrics(world.getRealizedWeb)
    val popMetrics = computeWebMetrics(world.populationWeb)
    val (spBeta, spGammaN) = computeSpeciesBetaDiversity(world)
    val (intBeta, intGammaN) = computeInteractionBetaDiversity(world)

    SimulationMetrics(
      largestPatchFraction = largestPatchFrac,
      patchFragmentation = patchFrag,
      spMeanInDegree = spMetrics.meanInDegree,
      spMeanOutDegree = spMetrics.meanOutDegree,
      spConnectance = spMetrics.connectance,
      spWeakComponents = spMetrics.weakComponents,
      spDegreeP10 = spMetrics.percentiles(0),
      spDegreeP25 = spMetrics.percentiles(1),
      spDegreeP50 = spMetrics.percentiles(2),
      spDegreeP75 = spMetrics.percentiles(3),
      spDegreeP90 = spMetrics.percentiles(4),
      popMeanInDegree = popMetrics.meanInDegree,
      popMeanOutDegree = popMetrics.meanOutDegree,
      popConnectance = popMetrics.connectance,
      popWeakComponents = popMetrics.weakComponents,
      speciesBetaDiversity = spBeta,
      speciesGammaNorm = spGammaN,
      interactionBetaDiversity = intBeta,
      interactionGammaNorm = intGammaN
    )

  private case class WebMetrics(
    meanInDegree: Double,
    meanOutDegree: Double,
    connectance: Double,
    weakComponents: Double,
    percentiles: Seq[Double] // P10, P25, P50, P75, P90
  )

  // Fixed-width binned degree histogram for a directed web.
  // Bins have length nBins: index i in [0, nBins-2] counts nodes with exactly that
  // degree; the last bin (index nBins-1) is an overflow that counts degree >= nBins-1.
  // This keeps output shape fixed so each bin can become a scalar OpenMOLE Val column
  // while still exposing the shape/tail of the degree distribution.
  case class DegreeHistogram(
    inBins: Seq[Int],
    outBins: Seq[Int],
    totalBins: Seq[Int]
  )

  def degreeHistogram[V](
    graph: DefaultDirectedGraph[V, DefaultEdge],
    nBins: Int
  ): DegreeHistogram =
    require(nBins >= 1, "nBins must be >= 1")
    val vertices = graph.vertexSet().asScala.toSeq

    def binOf(degree: Int): Int = math.min(degree, nBins - 1)

    def histogram(degrees: Seq[Int]): Seq[Int] =
      val counts = Array.fill(nBins)(0)
      degrees.foreach(d => counts(binOf(d)) += 1)
      counts.toSeq

    val inDegrees    = vertices.map(v => graph.inDegreeOf(v))
    val outDegrees   = vertices.map(v => graph.outDegreeOf(v))
    val totalDegrees = vertices.map(v => graph.inDegreeOf(v) + graph.outDegreeOf(v))

    DegreeHistogram(
      inBins    = histogram(inDegrees),
      outBins   = histogram(outDegrees),
      totalBins = histogram(totalDegrees)
    )

  private def computeLandscapeMetrics(world: World): (Double, Double) =
    val protectedIds = world.managementLandscape.managementAreas
      .collect { case a if a.status == ProtectionStatus.Protected => a.id }
      .toSet

    if protectedIds.isEmpty then return (0.0, 0.0)

    val grid = world.managementLandscape.grid

    // Build undirected graph of protected tiles with Moore adjacency
    val graph = new SimpleGraph[Int, DefaultEdge](classOf[DefaultEdge])
    protectedIds.foreach(graph.addVertex)
    protectedIds.foreach { tileId =>
      grid.neighbors(tileId).foreach { neighborId =>
        if protectedIds.contains(neighborId) && !graph.containsEdge(tileId, neighborId) then
          graph.addEdge(tileId, neighborId)
      }
    }

    val inspector = new ConnectivityInspector(graph)
    val components = inspector.connectedSets().asScala.toSeq
    val nProtected = protectedIds.size.toDouble
    val largestComponent = components.map(_.size()).max.toDouble

    val largestPatchFraction = largestComponent / nProtected
    val patchFragmentation = components.size.toDouble / nProtected

    (largestPatchFraction, patchFragmentation)

  private def computeWebMetrics[V](graph: DefaultDirectedGraph[V, DefaultEdge]): WebMetrics =
    val vertices = graph.vertexSet().asScala.toSeq
    val n = vertices.size

    if n == 0 then
      return WebMetrics(0.0, 0.0, 0.0, 0.0, Seq.fill(5)(0.0))

    val inDegrees = vertices.map(v => graph.inDegreeOf(v).toDouble)
    val outDegrees = vertices.map(v => graph.outDegreeOf(v).toDouble)
    val totalDegrees = vertices.map(v => (graph.inDegreeOf(v) + graph.outDegreeOf(v)).toDouble).sorted

    val meanIn = inDegrees.sum / n
    val meanOut = outDegrees.sum / n
    val connectance = if n > 1 then graph.edgeSet().size().toDouble / (n.toDouble * (n.toDouble - 1)) else 0.0

    val inspector = new ConnectivityInspector(graph)
    val weakComponents = inspector.connectedSets().size().toDouble

    val percentiles = Seq(0.10, 0.25, 0.50, 0.75, 0.90).map(p => percentile(totalDegrees, p))

    WebMetrics(meanIn, meanOut, connectance, weakComponents, percentiles)

  private def percentile(sorted: Seq[Double], p: Double): Double =
    if sorted.isEmpty then return 0.0
    val idx = p * (sorted.size - 1)
    val lower = idx.toInt
    val upper = math.min(lower + 1, sorted.size - 1)
    val frac = idx - lower
    sorted(lower) * (1.0 - frac) + sorted(upper) * frac

  // Harrison's beta-2 (normalized Whittaker): (gamma/mean_alpha - 1) / (N - 1)
  // Range [0, 1]: 0 = all tiles identical, 1 = complete turnover
  private def normalizedBeta(gammaProtected: Double, meanAlpha: Double, nTiles: Int): Double =
    if meanAlpha <= 0 || nTiles <= 1 then 0.0
    else (gammaProtected / meanAlpha - 1.0) / (nTiles.toDouble - 1.0)

  private def computeSpeciesBetaDiversity(world: World): (Double, Double) =
    val allAreas = world.managementLandscape.managementAreas
    val protectedAreas = allAreas.filter(_.status == ProtectionStatus.Protected)

    if protectedAreas.isEmpty then return (0.0, 0.0)

    // Alpha: species set per protected tile
    val alphas = protectedAreas.map(_.getSpecies)
    val meanAlpha = alphas.map(_.size.toDouble).sum / alphas.size

    // Gamma protected: unique species across all protected tiles
    val gammaProtected = alphas.reduce(_ ++ _).size.toDouble

    // Gamma total: unique species across ALL tiles
    val gammaTotal = allAreas.map(_.getSpecies).reduce(_ ++ _).size.toDouble

    val beta = normalizedBeta(gammaProtected, meanAlpha, protectedAreas.size)
    val gammaNorm = if gammaTotal > 0 then gammaProtected / gammaTotal else 0.0

    (beta, gammaNorm)

  private def computeInteractionBetaDiversity(world: World): (Double, Double) =
    val allAreas = world.managementLandscape.managementAreas
    val protectedAreas = allAreas.filter(_.status == ProtectionStatus.Protected)
    val popWeb = world.populationWeb

    if protectedAreas.isEmpty then return (0.0, 0.0)

    // Alpha: interaction set per protected tile
    val alphas = protectedAreas.map(_.getInteractions(popWeb))
    val meanAlpha = alphas.map(_.size.toDouble).sum / alphas.size

    // Gamma protected: unique interactions across all protected tiles
    val gammaProtected = alphas.reduce(_ ++ _).size.toDouble

    // Gamma total: unique interactions across ALL tiles
    val gammaTotal = allAreas.map(_.getInteractions(popWeb)).reduce(_ ++ _).size.toDouble

    val beta = normalizedBeta(gammaProtected, meanAlpha, protectedAreas.size)
    val gammaNorm = if gammaTotal > 0 then gammaProtected / gammaTotal else 0.0

    (beta, gammaNorm)

  case class StrategyComparisonMetrics(
    tileOverlapJaccard: Double,
    tileRankCorrelation: Double,
    spIntCorrelation: Double
  )

  case class MetawebCentralityMetrics(
    extinctMeanBetweenness: Double,
    survivingMeanBetweenness: Double,
    extinctMeanInDegree: Double,
    extinctMeanOutDegree: Double,
    survivingMeanInDegree: Double,
    survivingMeanOutDegree: Double
  )

  // Compute tile-level comparison between SpeciesRichness and InteractionRichness strategies.
  // worldSp and worldInt must be derived from the same base world (same seed, same populations)
  // but with different protection plans applied.
  def computeStrategyComparison(
    worldSp: World,
    worldInt: World
  ): StrategyComparisonMetrics =
    val areas = worldSp.managementLandscape.managementAreas
    val popWeb = worldSp.populationWeb

    val spRichness  = areas.map(a => a.id -> a.getSpeciesRichness.toDouble).toMap
    val intRichness = areas.map(a => a.id -> a.getInteractionRichness(popWeb).toDouble).toMap

    val ids = areas.map(_.id).sorted
    val spVals  = ids.map(id => spRichness.getOrElse(id, 0.0))
    val intVals = ids.map(id => intRichness.getOrElse(id, 0.0))

    val tileRankCorr = spearman(spVals, intVals)
    val spIntCorr    = pearson(spVals, intVals)

    val protectedSp  = worldSp.managementLandscape.managementAreas
      .collect { case a if a.status == ProtectionStatus.Protected => a.id }.toSet
    val protectedInt = worldInt.managementLandscape.managementAreas
      .collect { case a if a.status == ProtectionStatus.Protected => a.id }.toSet

    val intersection = protectedSp.intersect(protectedInt).size.toDouble
    val union        = protectedSp.union(protectedInt).size.toDouble
    val jaccard      = if union == 0.0 then 1.0 else intersection / union

    StrategyComparisonMetrics(
      tileOverlapJaccard = jaccard,
      tileRankCorrelation = tileRankCorr,
      spIntCorrelation = spIntCorr
    )

  // Compute betweenness centrality and degree statistics for extinct vs surviving species.
  // extinctSpeciesIds: set of species ids that went extinct
  // metaWeb: the original (pre-extinction) metaweb
  def computeMetawebCentrality(
    extinctSpeciesIds: Set[Int],
    metaWeb: DefaultDirectedGraph[Species, DefaultEdge]
  ): MetawebCentralityMetrics =
    val bc = new BetweennessCentrality(metaWeb)
    val scores: Map[Species, Double] = bc.getScores().asScala.map { case (k, v) => k -> v.toDouble }.toMap

    val allSpecies = metaWeb.vertexSet().asScala.toSeq
    val extinct    = allSpecies.filter(s => extinctSpeciesIds.contains(s.id))
    val surviving  = allSpecies.filter(s => !extinctSpeciesIds.contains(s.id))

    def safeMean(xs: Seq[Double]): Double =
      if xs.isEmpty then 0.0 else xs.sum / xs.size

    val extinctBetweenness  = extinct.map(s => scores.getOrElse(s, 0.0))
    val survivingBetweenness = surviving.map(s => scores.getOrElse(s, 0.0))

    val extinctInDegree   = extinct.map(s => metaWeb.inDegreeOf(s).toDouble)
    val extinctOutDegree  = extinct.map(s => metaWeb.outDegreeOf(s).toDouble)
    val survivingInDegree  = surviving.map(s => metaWeb.inDegreeOf(s).toDouble)
    val survivingOutDegree = surviving.map(s => metaWeb.outDegreeOf(s).toDouble)

    MetawebCentralityMetrics(
      extinctMeanBetweenness  = safeMean(extinctBetweenness),
      survivingMeanBetweenness = safeMean(survivingBetweenness),
      extinctMeanInDegree     = safeMean(extinctInDegree),
      extinctMeanOutDegree    = safeMean(extinctOutDegree),
      survivingMeanInDegree   = safeMean(survivingInDegree),
      survivingMeanOutDegree  = safeMean(survivingOutDegree)
    )

  // Compute per-strategy beta diversity metrics (before or after extinction) on the protected set.
  // Returns (speciesBeta, interactionBeta).
  def computeProtectedBetaDiversity(world: World): (Double, Double) =
    val (spBeta, _)  = computeSpeciesBetaDiversity(world)
    val (intBeta, _) = computeInteractionBetaDiversity(world)
    (spBeta, intBeta)

  private def ranks(xs: Seq[Double]): Seq[Double] =
    val indexed = xs.zipWithIndex.sortBy(_._1)
    val rankArr = Array.ofDim[Double](xs.size)
    var i = 0
    while i < indexed.size do
      // handle ties: average rank
      var j = i
      while j < indexed.size && indexed(j)._1 == indexed(i)._1 do j += 1
      val avgRank = (i + j - 1).toDouble / 2.0 + 1.0
      (i until j).foreach(k => rankArr(indexed(k)._2) = avgRank)
      i = j
    rankArr.toSeq

  def pearsonPublic(xs: Seq[Double], ys: Seq[Double]): Double = pearson(xs, ys)
  def spearmanPublic(xs: Seq[Double], ys: Seq[Double]): Double = spearman(xs, ys)

  private def pearson(xs: Seq[Double], ys: Seq[Double]): Double =
    val n = xs.size
    if n < 2 then return 0.0
    val mx = xs.sum / n
    val my = ys.sum / n
    val cov = xs.zip(ys).map { case (x, y) => (x - mx) * (y - my) }.sum
    val sx = math.sqrt(xs.map(x => (x - mx) * (x - mx)).sum)
    val sy = math.sqrt(ys.map(y => (y - my) * (y - my)).sum)
    if sx < 1e-12 || sy < 1e-12 then 0.0 else cov / (sx * sy)

  private def spearman(xs: Seq[Double], ys: Seq[Double]): Double =
    pearson(ranks(xs), ranks(ys))

end DiagnosticMetrics
