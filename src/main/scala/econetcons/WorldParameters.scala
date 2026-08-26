package econetcons

import ProtectionStatus.{Protected, Unprotected}

case class WorldParameters(
  numberOfSpecies: Int = 100,
  connectanceMetaWeb: Double = 0.2,
  // Total number of populations on the (unit-area) landscape. This is the density knob:
  // density = nPopulations / total area, and total area = 1. Orthogonal to medianHomeRange.
  nPopulations: Int = 2500,
  // Median home-range radius relative to the landscape. ECOLOGICAL SCALE ONLY — it rescales
  // the allometric per-species home-range distribution, and those home ranges in turn set
  // the interaction radius (PopulationWebGenerator) and the population-placement step size.
  // It has NO effect on how many populations exist (that is nPopulations).
  medianHomeRange: Double = 0.01,
  // Average populations per management tile. Sets ONLY the management-grid granularity:
  // nTiles = nPopulations / avgPopulationsPerTile. Does not affect population density.
  avgPopulationsPerTile: Int = 10,
  extinctionProbabilityUnprotected: Double = 0.5, // primary extinction probability for unprotected tiles
  dietBreadthThreshold: Double = 0.0,             // fraction of prey species lost that triggers cascade (0.0 = classic outDegree==0 rule)
  preyPerPredator: Double = 0.0,                  // prey populations needed per predator population (0.0 = use legacy secondaryExtinctions instead)
  observationNoise: Double = 0.0,                 // per-population false-negative probability for species occurrence data (0.0 = perfect detection)
  falsePositiveRate: Double = 0.0,               // expected number of ghost populations per tile (Poisson); each ghost is a species drawn from the full metaweb pool

  // --- Population spatial model ---
  // Go-forward model is the O(N) Eden-growth branching random walk. The legacy
  // Thomas-process placement is DEPRECATED and kept only so the two can be compared during
  // calibration; it will be removed once the walk is signed off. Setting this false selects
  // the deprecated path. NOTE: the two models produce different output for a given seed.
  useBranchingWalk: Boolean = true,
  // Optional dimensionless multiplier on the placement step size (step = medianHomeRange · spread,
  // per species using that species' home range). Leave at 1.0 — children step ~one home-range
  // radius from a neighbour. Reserved as a future exploration avenue for cluster tightness.
  spatialSpread: Double = 1.0,
):

  def extinctionProbability(status: ProtectionStatus): Double =
    status match
      case Protected => 0.01
      case Unprotected => extinctionProbabilityUnprotected
      case null => 0.0

  /**
   * Fixed parent->child step length for the branching random walk, per species.
   *
   * step = homeRange · spatialSpread. Children are placed exactly this far from a randomly
   * chosen already-placed individual (random angle). The cluster's extent grows ~ step·sqrt(N)
   * as an emergent property of the walk, so N does not appear here explicitly.
   */
  def placementStep(homeRange: Double): Double =
    homeRange * spatialSpread
