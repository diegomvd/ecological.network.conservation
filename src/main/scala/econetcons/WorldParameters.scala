package econetcons

import ProtectionStatus.{Protected, Unprotected}

case class WorldParameters(
  numberOfSpecies: Int = 100,
  connectanceMetaWeb: Double = 0.2,
  // Sigma of the LogNormal(0, sigma) body-size distribution that seeds the niche axis.
  // Body sizes are min-max rescaled to [0,1] to give niche values, so a large sigma produces
  // an extreme outlier that crushes almost all niche values toward 0 (top-heavy web, ~no basal
  // species) and blows up the allometric home-range tail. Smaller sigma spreads niche values
  // more evenly and yields a proper basal layer. Historic value was 3.0.
  bodySizeSigma: Double = 3.0,
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
  preyPerPredator: Double = 0.0,                  // DEPRECATED knob for the old unit-weighted load/supply model; kept only for back-compat callers. Prefer `epsilon`.
  // Energetic cascade knob (assimilation/conversion efficiency). When > 0, Phase-2 uses the
  // metabolic load/supply model: predator individual (mass M) has demand D = M^0.75 (Kleiber);
  // each prey individual (mass m) provides supply ε·m, split equally among its current
  // predators; a predator survives iff Σ shares ≥ D. ε is the single scarcity knob (small ε =
  // scarce energy = more starvation). 0.0 disables it (falls back to preyPerPredator/legacy).
  epsilon: Double = 0.0,
  observationNoise: Double = 0.0,                 // per-population false-negative probability for species occurrence data (0.0 = perfect detection)
  falsePositiveRate: Double = 0.0,               // expected number of ghost populations per tile (Poisson); each ghost is a species drawn from the full metaweb pool

  // --- Population spatial model (Matérn cluster process) ---
  // Mean number of patches per species (Poisson λ). Each species draws Poisson(λ) patch
  // centers (floored at 1), placed uniformly on the torus; its populations are scattered
  // multinomially across them. Range-size variation across species is emergent from the
  // Poisson draw. See Utils.generateMaternProcess.
  meanPatchesPerSpecies: Double = 5.0,
  // Dimensionless multiplier on each patch's disk radius. Patch radius is sized to hold its
  // own k populations at the home-range density: R = homeRange · √k · patchPackingFactor.
  // Small → dense/territorial patches; large → loose, landscape-filling patches.
  patchPackingFactor: Double = 1.0,

  // --- Population allocation across species (how many populations each species gets) ---
  // "damuth"             : count ∝ m^-0.75 (body size only; legacy default)
  // "transferEfficiency" : count ∝ m^-0.75 · TE^(TP-1), TP = continuous trophic position (Levine)
  // "energyBalance"      : basal count ∝ m^-0.75; consumer counts solved so metaweb prey can feed
  //                        them under the metabolic rule (ε·m_prey shared among predator species
  //                        vs M^0.75 demand)
  abundanceModel: String = "damuth",
  transferEfficiency: Double = 0.1,

  // --- Cascade rule (Phase-2 secondary extinctions, also used for stabilization at assembly) ---
  // "metabolic"       : legacy dispatch — energetic cascade if epsilon > 0, else diet-breadth rule
  // "legacy"          : diet-breadth rule (dietBreadthThreshold; 0 = die when no prey left)
  // "energyTolerance" : each population carries biomass B ∝ m^0.25 (Damuth density × mass);
  //                     consumers receive Σ B_prey/|predators of prey|; demand is calibrated to the
  //                     supply at assembly, and a consumer dies once it has lost more than
  //                     `energyTolerance` (θ) of that initial intake. θ = 1 ⇔ die only with no prey.
  cascadeModel: String = "metabolic",
  energyTolerance: Double = 1.0,
):

  def extinctionProbability(status: ProtectionStatus): Double =
    status match
      case Protected => 0.01
      case Unprotected => extinctionProbabilityUnprotected
      case null => 0.0
