# Model Parameters — Reference for the Action Space

This document is the exhaustive inventory of parameters in the `econetcons` ecological-network
conservation model, written to support defining the exploration action space (OpenMOLE
sensitivity analysis or pattern-space exploration — method TBD).

For each parameter: what it is, where it acts in the code, its effect on model behaviour, its
current default, and a suggested sweep role/range where we have evidence. Calibration findings
from the diagnostic sweeps are folded in.

The model runs in three conceptual stages per world:
1. **Metaweb generation** — a niche-model food web over species (who *can* eat whom).
2. **Population placement** — each species' individuals are scattered in space (Matérn cluster
   process), and a *population web* is realised (who actually co-occurs closely enough to interact).
3. **Conservation + extinction** — a protection plan is applied to a management grid, then
   Phase 0 (stabilisation), Phase 1 (primary, protection-dependent) and Phase 2 (secondary,
   energetic cascade) extinctions run.

Observables are read on the identical base world for every strategy, so strategy comparisons are
paired/controlled (see `Observables.run`).

> **Paper configuration (decided 2026-10-14): `abundanceModel = "emergent"`.** The community is
> no longer assembled from a fixed population count. Each species fills a range (a fraction of the
> landscape) with home-range-sized seed populations, and the energy flow from a fixed basal
> biomass carves out the viable community (§B′). Under this model `nPopulations`, `epsilon`,
> `cascadeModel`, `avgPopulationsPerTile` and the biomassPyramid parameters are **not used**.
> They are kept only so legacy results can be reproduced. The fixed values and the explored axes
> are in the table below. Evidence for each choice is in `MODEL_DESIGN_AND_RESULTS.md` ("Emergent
> assembly").

| Parameter | Paper value / range | Role |
|---|---|---|
| `abundanceModel` | `"emergent"` | fixed |
| `foragingRule` | `"supply"` | fixed (equal/niche tested: no material difference) |
| `transferEfficiency` (TE) | 0.1 | fixed |
| `minViableIndividuals` (n_min) | 10 | fixed |
| `basalBiomass` (B₀) | 5·10⁶ | fixed (×10 → +15–30% richness; robustness note) |
| `homeRangeExponent` (b) | 0.5 (now the default) | fixed |
| `medianHomeRange` | 0.02 | fixed (population grain within ranges) |
| `rangeFraction` (ρ̄) | 0.03 | fixed (main web-density/richness knob → supplementary figure) |
| `rangeFractionSigma` (σ_ρ) | 1.0 | fixed |
| `rangeMassExponent` (c) | 0.0 | fixed |
| `meanPatchesPerSpecies` | 5 (Matérn placement) | fixed |
| `numberOfSpecies` | 300 | fixed |
| `bodySizeSigma` | 1.0 | fixed |
| `connectanceMetaWeb` (C) | {0.05, 0.1, 0.2} | **web-structure axis** (L/S ≈ 2.9 → 6.5, richness ≈ constant) |
| `tileSizeInHomeRanges` (t) | e.g. {0.5, 1, 2, 4} | **management-scale axis** |
| `fractionProtected` | range as before | **treatment axis** |
| `extinctionProbabilityUnprotected` | range as before | **pressure axis** |
| strategy | SR vs IR (= EIR) | paired within sample |

Runner: `PaperSweep` (one assembly per (C, seed), reused across tile size, budget and pressure;
bit-identical to fresh runs). Outcome columns `*_sp_ext_*` / `*_spint_ext_*` are **percent** of the
post-stabilization baseline.

---

## A. Metaweb / food-web structure

### `numberOfSpecies` — `WorldParameters`, default 100
Number of species in the niche-model metaweb.
- **Acts in:** `World.generateMetaWeb`.
- **Effect:** overall scale of the web. NOTE (calibration): raising this at *fixed connectance*
  does **not** proportionally raise survivors — it makes the web absolutely denser and can
  *reduce* the fraction surviving (intermediate-layer survival collapsed from 0.24→0.07 when
  going 200→400 species at C=0.10). To get "more survivors," widen the base via lower
  connectance, not by raising species count alone.
- **Suggested role:** context/scale axis (e.g. 200; 400 for statistical power at low C).

### `connectanceMetaWeb` — `WorldParameters`, default 0.2
Target niche-model connectance C. Sets the Beta feeding-range parameter β = 0.5/C − 1.
- **Acts in:** `World.generateMetaWeb` (β, feeding ranges).
- **Effect:** THE dominant knob for basal fraction and trophic depth. Realised connectance L/S²
  tracks the parameter closely at low–mid C and sags at high C:
  | param C | realised C | basal fraction |
  |---|---|---|
  | 0.05 | 0.052 | 0.140 |
  | 0.10 | 0.092 | 0.079 |
  | 0.15 | 0.136 | 0.054 |
  | 0.20 | 0.176 | 0.032 |
  | 0.40 | 0.298 | 0.012 |
  Low C → broad basal base, taller supportable pyramid, more survivors. High C → top-heavy web,
  ~2 basal species, near-total collapse.
- **Suggested role:** primary *environment* axis. Working default C = 0.10 (W&M-realistic ~8%
  basal). Sweep ~0.05–0.20.

### `bodySizeSigma` — `WorldParameters`, default 3.0 (calibrated → use 1.0)
σ of the LogNormal(0, σ) body-size distribution that seeds the niche axis (body sizes are
min-max rescaled to [0,1] to give niche values).
- **Acts in:** `World.generateMetaWeb` (`bodySizeDist`).
- **Effect:** controls niche spread and the allometric home-range/abundance tail. At σ=3 the
  distribution has an extreme outlier → home-range max ~27–45× the landscape (pathological) and
  a very skewed niche axis. σ does **not** by itself fix basal fraction (that's connectance),
  but σ=1.0 removes the pathological tail (HR max ~0.4) with no downside.
- **Suggested role:** fix at 1.0. Body size is retained (drives abundance, home range, and the
  energetic cascade masses), so this is not removed.

---

## B. Population abundance & spatial structure

### `nPopulations` — `WorldParameters`, default 2500
Total number of populations placed on the unit-area landscape. The density knob
(density = nPopulations / 1).
- **Acts in:** `World.generatePopulations` (apportioned across species by allometric abundance
  ∝ bodySize^−0.75), and sets management-grid resolution together with `avgPopulationsPerTile`.
- **Effect:** more populations → denser spatial coverage → more realised interactions; also more
  compute. Orthogonal to `medianHomeRange`.
- **Suggested role:** context axis (e.g. 5000). Larger = smoother statistics, slower runs.

### `medianHomeRange` — `WorldParameters`, default 0.01
Median home-range radius relative to the landscape. ECOLOGICAL SCALE ONLY.
- **Acts in:** rescales the allometric per-species home-range distribution; those home ranges
  set (a) the interaction radius in `PopulationWebGenerator` and (b) the Matérn patch-radius
  density scale. Does NOT change how many populations exist.
- **Effect:** larger home ranges → larger interaction radius and larger patches → more
  inter-species overlap → more realised interactions and generally more survival.
- **Suggested role:** context axis (calibration used 0.05).

### `meanPatchesPerSpecies` (λ) — `WorldParameters`, default 5.0
Poisson mean number of Matérn patches (cluster centres) per species.
- **Acts in:** `Utils.generateMaternProcess`. Each species draws Poisson(λ) centres (floored at
  1) placed uniformly on the torus; its populations are scattered multinomially across them.
- **Effect:** mild, saturating. More patches → broader landscape coverage → slightly more
  overlap/survival, but plateaus (Phase-0 survivors 48→64 across λ = 1→20 at C=0.10). Range-size
  variation across species is emergent from the Poisson draw.
- **Suggested role:** secondary/nuisance axis; default 5 sits on the plateau.

### `patchPackingFactor` — `WorldParameters`, default 1.0
Dimensionless multiplier on each patch's disk radius. Patch radius R = homeRange · √k ·
patchPackingFactor, where k = populations that landed in that patch (multinomial).
- **Acts in:** `Utils.generateMaternProcess` (`uniformInDisk` radius).
- **Effect:** cluster tightness / territoriality. √k means patches with more individuals are
  bigger (abundant basal species blanket the landscape; rare predators stay patchy — an emergent
  ecological range-size gradient). Small factor → dense/territorial; large → loose/landscape-
  filling. Survival peaks around packing ≈ 2 and *declines* by 4 as radii approach landscape
  size and spatial structure washes out. Sweet spot 1–2.
- **Suggested role:** secondary axis; default 1.0 (safe), 2.0 (survival-optimal but starts
  eroding spatial structure).

---

## B′. Emergent assembly (`abundanceModel = "emergent"`, the paper model)

A population is n individuals of its species' body mass M at a point. Demand is n·M^0.75
(Kleiber), and supply to predators is TE·n·m. Consumers shrink to the individuals their share of
prey supply sustains, never above a seed potential, and die below n_min. The weakest population is
removed first, ties at random. The same rule runs after every disturbance, capped at the assembled
sizes. Energy flow is bottom-up (donor-controlled): predation never depletes a prey population.
Basal species are the herbivores; plants are implicit. Code: `EnergyFlow.scala`; seeding in
`World.emergentSeedCounts`.

### `rangeFraction` (ρ̄) — default 0.03; `rangeFractionSigma` (σ_ρ) — default 1.0; `rangeMassExponent` (c) — default 0.0
Range size of species s as a share of the landscape:
ρ_s = ρ̄ · LogNormal(0, σ_ρ) · (M_s/M_median)^c, capped at 1. The species is seeded with
k_s ~ Poisson(ρ_s / (π r_s²)) ≥ 1 populations, so its range is filled with home ranges. They are
placed by the Matérn process, whose patch area k·π r² matches ρ_s. The same rule applies to basal
species.
- **Effect:** ρ̄ is the main web knob. At C = 0.05, ρ̄ = 0.01 / 0.03 / 0.1 gives about 110 / 185 /
  225 species and L/S 1.3 / 2.8 / 6.2. Assembly cost grows steeply with ρ̄. σ_ρ = 0.5 vs 1 makes
  little difference.
- **Suggested role:** fix ρ̄ = 0.03 (realistic L/S band), σ_ρ = 1, c = 0.

### `basalBiomass` (B₀) — default 5·10⁶
Total basal (herbivore) biomass, the system's energy input. It is split evenly over all basal
points (⌊(B₀/N_basal)/m⌋ individuals each). Basal points are kept even when no consumer reaches
them.
- **Effect:** only B₀/n_min matters. A tenfold change moves richness by 15–30%. Consumer biomass
  stays about 6–8% of B₀.
- **Suggested role:** fixed.

### `transferEfficiency` (TE) — default 0.1; `minViableIndividuals` (n_min) — default 10
TE: the share of prey biomass passed to predators (Lindeman ≈ 0.1). n_min: the viability threshold
in individuals.
- **Suggested role:** fixed.

### `foragingRule` — default "supply"
How a consumer spreads its demand over its prey:
- `"supply"`: in proportion to what each prey offers (mass action).
- `"equal"`: equally over the prey species in reach.
- `"niche"`: over prey species weighted by a Gaussian preference centred on the predator's
  niche-model feeding interval.
- **Effect:** small. Every prey's supply is shared out fully, so consumer-to-consumer energy is
  capped by consumer biomass whatever the rule. Per species, the realized (flow) trophic position
  has a mean of 2.4–2.7 and a maximum of about 4 under all three rules. Add 1 for plants.
- **Suggested role:** fixed at "supply".

### `homeRangeExponent` (b) — default 0.5 (was hard-coded 0.75 until 2026-10-14)
Home-range radius r ∝ M^b, rescaled so the median equals `medianHomeRange`. With b = 0.5,
home-range area ∝ M, matching empirical scaling; 0.75 gave area ∝ M^1.5. This applies to **all**
models: the default change breaks bit-identity with results produced before 2026-10-14. Pass 0.75
to reproduce them.
- **Effect (emergent):** about the same richness and L/S as 0.75, with half the seeds and run time.

### `medianHomeRange` under the emergent model
At fixed ρ̄, HR barely changes richness or L/S: fewer, wider-reaching seeds roughly cancel. It sets
the number of populations per range, and so primary-extinction vulnerability.
- **Suggested role:** fixed at 0.02.

### Placement
`placement = "auto"` resolves to Matérn under the emergent model, so ranges are sections of the
landscape. Uniform is used only under biomassPyramid.

---

## C. Extinction dynamics

### `epsilon` (ε) — `WorldParameters`, default 0.0 (use > 0 for the energetic model)
Assimilation/conversion efficiency; the single scarcity knob of the metabolic Phase-2 cascade.
- **Acts in:** `World.trophicCascades` (dispatched by `phase2Cascades` when ε > 0).
- **Model:** for a predator individual of mass M and prey individual of mass m
  (mass = `species.bodySize`):
  - demand  D = M^0.75  (Kleiber; constant absorbed to 1)
  - supply  S = ε · m   (biomass energy, linear in mass)
  - each prey splits its supply **equally** among its current predators
  - a predator survives iff Σ (shares over all its prey) ≥ D
  Only predators starve (prey are never removed for being popular; over-predation is implicit via
  shrinking shares). Removal is **greedy** — the single largest-deficit predator is removed each
  round and shares recomputed (so removing a predator can rescue its co-predators).
- **Effect:** long-range monotonic dial. Below ε≈0.1 all consumers starve (only basal survive);
  survival rises smoothly and does **not** saturate until ε≈16. Because supply is linear in prey
  mass but demand scales as M^0.75, the cascade is size-structured (large-bodied top predators,
  with high demand, are fed last). Post-Phase-0 baseline species at C=0.10:
  | ε | baseline species |
  |---|---|
  | 0.5 | 21 |
  | 1.0 | 31 |
  | 2.0 | 45 |
  | 4.0 | 69 |
  | 8.0 | 86 |
  | 16.0 | 92 |
- **Note on interpretation:** ε has no intrinsic (0,1) meaning yet — both proportionality
  constants are set to 1, so ε is a *relative* scarcity dial, not a literal assimilation
  efficiency. Re-anchoring to a true efficiency was discussed and deferred.
- **Suggested role:** PRIMARY scarcity/environment axis. Sweep ~0.5–16 (the whole range is
  informative; SR-vs-IR strategy differences and their robustness-ordering crossover are
  ε-dependent). Representative working point ε ≈ 1–2.

### `extinctionProbabilityUnprotected` — `WorldParameters`, default 0.5
Per-population primary (Phase-1) extinction probability on **unprotected** tiles. Protected tiles
use a hardcoded 0.01 (see `extinctionProbability`).
- **Acts in:** `World.primaryExtinctions` via `WorldParameters.extinctionProbability`.
- **Effect:** strength of the protection signal — the gap between 0.5 (unprotected) and 0.01
  (protected) is what makes protection matter. Higher → more primary loss outside reserves →
  larger consequence of *which* tiles are protected.
- **Suggested role:** context axis (strength of the disturbance regime).

### `dietBreadthThreshold` — `WorldParameters`, default 0.0
LEGACY Phase-2 rule, used only when ε = 0. Fraction of a consumer's original prey-*species*
diversity it can lose before going extinct (0.0 = classic outDegree==0 "lose all prey" rule).
- **Effect:** irrelevant once ε > 0 (energetic cascade takes over).
- **Suggested role:** keep 0.0; not part of the go-forward action space.

### `preyPerPredator` — `WorldParameters`, default 0.0
DEPRECATED. Threshold of the old unit-weighted load/supply cascade. Superseded by `epsilon`.
- **Suggested role:** leave 0.0; retained only for back-compat callers.

---

## D. Management / conservation action

### `tileSizeInHomeRanges` (t) — `WorldParameters`, default 0.0 (= use `avgPopulationsPerTile`) — the paper's tile size
Tile side in units of the median home-range **diameter** (2 · `medianHomeRange`). The grid has
n = round(1 / (t · 2 · medianHomeRange)) ≥ 1 tiles per side, realized side 1/n. For example, at
HR 0.02: t = 0.5 / 1 / 2 / 4 → 50 / 25 / 13 / 6 tiles per side (2500 / 625 / 169 / 36 tiles).
- **Why:** under the emergent model the assembled population count varies from about 1k to 60k
  between worlds. Populations-per-tile then gave 139–215 tiles (sd 37–61 between seeds) for one
  configuration, confounding strategy comparisons with tile geometry.
- **Acts in:** `World.apply` (`SquareGrid(tilesPerSide)`), `WorldParameters.tilesPerSide`.
- **Suggested role:** TREATMENT axis (management grain), log-spaced.

### `avgPopulationsPerTile` — `WorldParameters`, default 10 (legacy "tile size"; ignored when `tileSizeInHomeRanges` > 0)
Average populations per management tile. There is **no separate `tileSize` parameter** — grid
granularity is derived: `nTiles = nPopulations / avgPopulationsPerTile`, and the landscape is a
√nTiles × √nTiles grid on the unit square. Linear tile width = √(avgPopulationsPerTile /
nPopulations). (e.g. nPop=5000, value=64 → nTiles≈78 → ~9×9 grid → tiles ~0.11 wide.)
- **Acts in:** `World.apply` (nTiles, `SquareGrid.fromArea`).
- **Effect:** resolution of protection decisions. Larger value → fewer, bigger tiles (coarser
  decisions); smaller → many small tiles (finer). Earlier sweeps used {16,32,64,128,256}.
- **Suggested role:** TREATMENT/design axis (management-grain sensitivity), log-spaced.

### `fractionProtected` — `ConservationParameters`, default 0.3
Fraction of tiles protected — the conservation budget.
- **Acts in:** `ManagementLandscape.applyProtectionPlan`.
- **Effect:** more budget → more coverage → less loss. The interesting range for
  strategy-comparison is intermediate (a fully-protected or barely-protected landscape leaves
  less room for strategies to differ, though they still can — see strategy note below).
- **Suggested role:** TREATMENT axis. Earlier sweeps used 0.05–0.50.

### `strategy` / conservation strategies — within-sample paired factor
The treatment being compared. Evaluated on the **same** base world per sample (see
`Observables.run` / `defaultStrategies`), so comparisons are controlled. Three strategies:
- **SR** — `SpeciesRichness`: tile score = number of species present.
- **IR** — `InteractionRichness`: tile score = number of realised interactions present.
- **EIR** — `EgocentricInteractionRichness`: egocentric (neighbourhood) interaction richness.
- **Acts in:** scored per `ManagementArea`; protection plan built by
  `ManagementLandscape.protectionRecursionArgmax`.
- **Effect / finding:** SR, IR, EIR protect **largely different tiles at every ε** — SR-vs-IR
  protected-set Jaccard is only 0.15–0.40 across ε = 0.5–16, Spearman of tile scores 0.22–0.57,
  and SR protects 10–23 species IR doesn't. Crucially this persists even near full species
  survival (ε=16), confirming that *which network is protected* differs even when *how many
  species survive* is high. Which strategy yields the more robust final network is ε-dependent
  (the robustness ordering crosses over around ε≈2 in the C=0.10 tests).
- **Suggested role:** NOT an external sweep axis — an internal paired comparison. Include all
  three (SR, IR, EIR).

### `tileSelection` — `ConservationParameters`, default `Argmax`
Rule for picking the next tile in the weighted-criteria protection recursion.
- **`Argmax`** (default/go-forward): deterministically protect the highest-scoring tile (enables
  static score precompute).
- **`Stochastic`**: legacy roulette-wheel sampling by score magnitude.
- **Effect:** Argmax is the meaningful greedy conservation rule; Stochastic is retained for
  comparison only.
- **Suggested role:** fix at Argmax.

### `nAdminRegions` — `ConservationParameters`, default 1
Number of administrative regions (must be a perfect square ≥ 1). 1 = global selection; >1 = each
region independently selects tiles to protect.
- **Acts in:** `ManagementLandscape` (regional partitioning of the protection budget).
- **Effect:** models decentralised/fragmented governance — protecting the top tiles *within each
  region* vs globally. Can change which tiles a strategy picks and thus outcomes.
- **Suggested role:** optional TREATMENT/context axis (governance structure). Fix at 1 unless
  governance fragmentation is in scope.

---

## E. Observation / data-quality (noise)

### `observationNoise` — `WorldParameters`, default 0.0
Per-population false-negative probability for species-occurrence data (imperfect detection).
- **Effect:** the conservation planner sees a noisier picture of what's in each tile → degraded
  strategy performance. Tests robustness of SR/IR/EIR to detection error.
- **Suggested role:** robustness-to-data-quality axis. Fix at 0.0 for clean comparisons; sweep to
  test practical robustness.

### `falsePositiveRate` — `WorldParameters`, default 0.0
Expected number of ghost populations per tile (Poisson); each ghost is a species drawn from the
full metaweb pool (commission error).
- **Effect:** planner over-credits tiles with species that aren't there → degraded targeting.
- **Suggested role:** robustness-to-data-quality axis; fix at 0.0 for clean comparisons.

---

## F. Currently hardcoded constants (sweepable only with code changes — deferred)

Flagged "we'll see"; not yet promoted to parameters.

- **Protected-tile extinction probability = 0.01** (`WorldParameters.extinctionProbability`).
- **Metabolic exponents 0.75 (demand) / 1.0 (supply)** (`World.trophicCascades`). The 0.75 vs 1.0
  mismatch is intentional and drives the size-dependent cascade; candidate knobs if that
  mechanism becomes a study focus.
- **Allometric exponent −0.75 (abundance)** (`Species.speciesTraits`; unused by the emergent model).
  The home-range exponent is now the parameter `homeRangeExponent` (default 0.5).
- **Niche-preference width 0.5** (half-widths; `EnergyFlow.Diet.niche`, only for `foragingRule = "niche"`).
- **Kleiber exponent 0.75** in the emergent demand n·M^0.75 (`EnergyFlow`).
- **Robustness shock fractions 10% / 30%** (observable settings in `Observables.robustnessShock`,
  not world parameters).

---

## G. Replication

### `seed` — the RNG seed
Not a treatment; the replication axis. One seed fixes the entire world (metaweb, placement,
protection draws). All strategies share the seed's base world within a sample. Aggregate over
seeds for statistics.

---

## Summary: functional roles for the action space

| Role | Parameters |
|---|---|
| **Treatment (what we test)** | strategy (SR vs IR = EIR, paired within-sample), `fractionProtected`, `tileSizeInHomeRanges`, [`nAdminRegions`], `tileSelection`(fixed Argmax) |
| **Environment / context** | `connectanceMetaWeb`, `extinctionProbabilityUnprotected` |
| **Emergent assembly (fixed for the paper, §B′)** | `rangeFraction`=0.03, `rangeFractionSigma`=1, `rangeMassExponent`=0, `basalBiomass`=5e6, `transferEfficiency`=0.1, `minViableIndividuals`=10, `foragingRule`=supply, `homeRangeExponent`=0.5, `medianHomeRange`=0.02, `numberOfSpecies`=300 |
| **Legacy only (unused by the emergent model)** | `nPopulations`, `epsilon`, `cascadeModel`, `energyTolerance`, `avgPopulationsPerTile`, biomassPyramid params (`totalBiomass`, `individualsPerPopulation`, `smallSpeciesRule`, `densityCorrection`, `tpBandwidth`) |
| **Spatial-structure (secondary)** | `meanPatchesPerSpecies`, `patchPackingFactor` |
| **Data-quality robustness** | `observationNoise`, `falsePositiveRate` |
| **Fixed / calibrated** | `bodySizeSigma`=1.0, `dietBreadthThreshold`=0, `preyPerPredator`=0 (deprecated), hardcoded constants (§F) |
| **Replication** | `seed` |
