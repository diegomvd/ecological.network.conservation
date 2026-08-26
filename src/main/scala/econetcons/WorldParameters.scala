package econetcons

import ProtectionStatus.{Protected, Unprotected}

case class WorldParameters(
  numberOfSpecies: Int = 100,
  connectanceMetaWeb: Double = 0.2,
  areaOverlap: Double = 2.0, // this is the fraction of the landscape that is covered by all home ranges, it can be larger than 1
  medianHomeRange: Double = 0.01, // median home range radius relative to the landscape radius
  avgPopulationsPerTile: Int = 10, // average number of populations per tile
  useHotspot: Boolean = false, // enable biodiversity hotspot for species center clustering
  numHotspots: Int = 1, // number of hotspot centers (uniformly distributed if > 1)
  hotspotCenterX: Double = 0.5, // x-coordinate of hotspot center in [0,1] (only used if numHotspots = 1)
  hotspotCenterY: Double = 0.5, // y-coordinate of hotspot center in [0,1] (only used if numHotspots = 1)
  hotspotVariance: Double = 0.1, // variance of 2D Gaussian for species center sampling (smaller = tighter clustering)
  aggregationStrength: Double = 0.0, // 0 = random landscape, 1 = all species cluster around hotspot centers
  useThinning: Boolean = false, // enable spatial thinning to enforce minimum distance between populations
  spreadingFactor: Double = 0.5, // controls how spread out populations are around species center (lower = more clustered)
  extinctionProbabilityUnprotected: Double = 0.5, // primary extinction probability for unprotected tiles
  dietBreadthThreshold: Double = 0.0,             // fraction of prey species lost that triggers cascade (0.0 = classic outDegree==0 rule)
  preyPerPredator: Double = 0.0,                  // prey populations needed per predator population (0.0 = use legacy secondaryExtinctions instead)
  observationNoise: Double = 0.0,                 // per-population false-negative probability for species occurrence data (0.0 = perfect detection)
  falsePositiveRate: Double = 0.0,               // expected number of ghost populations per tile (Poisson); each ghost is a species drawn from the full metaweb pool
):

  def extinctionProbability(status: ProtectionStatus): Double =
    status match
      case Protected => 0.01
      case Unprotected => extinctionProbabilityUnprotected
      case null => 0.0

