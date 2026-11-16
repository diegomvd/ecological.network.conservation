# Model design and results (formerly `biomass_pyramid_plan.md`)

Status (2026-10-14): **superseded by the emergent assembly below**, which is the paper model. The
biomassPyramid mode (sections "Why" to "Verification") was implemented on 2026-10-12 and stays in
the code for reference. Calibration showed that its scale parameter n_pop is degenerate with
totalBiomass, and that every imposed allocation still had to be pruned. That led to letting the
pyramid emerge instead. (The detailed biomassPyramid plan that used to open this file was removed
when the emergent design was adopted; the mode itself is still in `World.scala`.)

## Emergent assembly (decided design, 2026-10-13/14)

### Model (`abundanceModel = "emergent"`, `EnergyFlow.scala`)
- **A population is n individuals of body mass M at a point.** Demand is n·M^0.75 (Kleiber).
  Supply to predators is TE·n·m, with TE = 0.1. The same biomass split into different abundances
  has different demand, so individuals are explicit.
- **Basal species are the herbivores;** plants are implicit. Total basal biomass B₀ is the fixed
  energy input, split evenly over all basal points. Basal points are kept even when no consumer
  reaches them.
- **Foraging (`foragingRule = "supply"`):** a consumer spreads its demand over its prey in
  proportion to their supply. Each prey's supply is shared out fully among its predators, in
  proportion to their demand.
- **Bottom-up (donor control):** predation never depletes prey. Decided to keep this.
- **Assembly:** each consumer starts at its seed potential, the individuals all its reachable
  lower-level prey could sustain alone. It shrinks to ⌊intake / M^0.75⌋ individuals, never above
  that potential, by worklist relaxation to the fixed point. While any consumer is below n_min = 10,
  the one furthest below is removed (ties at random) and the rest re-relax. Removals use an ordered
  set, which gives identical results to a full scan and is about 2× faster.
- **Disturbance:** the same relaxation, with each population capped at its assembled size.
- **Seeding by range:** ρ_s = ρ̄ · LogNormal(0, σ_ρ) · (M/M_med)^c, capped at 1. The species gets
  k_s ~ Poisson(ρ_s / π r_s²) ≥ 1 seeds placed by Matérn, so its range (a section of the landscape)
  is filled with home ranges. The same rule applies to basal species. Species are **not** meant to
  be everywhere, and interacting species need not co-occur.
- **Home range:** r ∝ M^b with b = 0.5, so area ∝ M as observed empirically. This is now the
  default for all models. The old value was 0.75, giving area ∝ M^1.5.
- **Management grid:** `tileSizeInHomeRanges` t sets the tile side in median home-range
  diameters. The number of tiles no longer depends on the population count.

### Evidence (CSVs in `data/emergent_design/`; 3–5 seeds each)
- `emergent_biomass_k.csv`, `emergent_biomass_N0.csv`: under the earlier fixed-k seeding, consumer
  biomass ≈ 0.13 × the basal biomass consumers reach, for every k and N₀. The number of seeds only
  changed basal coverage. → Seeding by range instead.
- `emergent_range.csv`:
  - ρ̄ is the main web knob. At C = 0.05, ρ̄ = 0.01 / 0.03 / 0.1 gives about 110 / 185 / 225
    species and L/S 1.3 / 2.8 / 6.2.
  - At fixed ρ̄, median HR barely changes the web; it sets populations per range.
  - b = 0.5 behaves like 0.75 at half the cost.
  - σ_ρ has a minor effect.
- `emergent_checks_tiles0/13.csv`:
  - B₀ ×10 changes richness by +15–30%; only B₀/n_min matters.
  - Populations-per-tile gave 139–215 tiles for one configuration (sd 37–61 between seeds).
    → fixed tile geometry.
- `emergent_foraging.csv`, `emergent_foraging_species.csv`: the equal and niche foraging rules
  make no material difference.
  - Weighted by biomass, about 90% of consumer intake comes from herbivores. This is forced by the
    pyramid: consumer biomass is about 7% of basal biomass.
  - **Per species,** the realized flow trophic position has a mean of 2.4–2.7 and a maximum of
    about 4. 34–52% of consumer species get most of their energy from other consumers, so food
    chains are realistic. Add 1 to these positions to count plants.

### Paper parameter set
- **Fixed:**
  - TE 0.1, n_min 10, B₀ 5·10⁶, foraging rule supply
  - b 0.5, median HR 0.02, ρ̄ 0.03, σ_ρ 1, c 0
  - 300 species, body-size σ 1, Matérn with 5 patches per species
- **Axes:**
  - metaweb connectance C ∈ {0.05, 0.1, 0.2}, which moves L/S 2.9 → 6.5 at richness 187–198
  - tile size t (home-range diameters)
  - fraction protected
  - primary extinction probability
  - strategy SR vs IR (= EIR), paired
- **Robustness notes:** B₀, foraging rule, and a supplementary ρ̄ figure.
- **Cost:** `Observables.run` with SR and IR at this configuration takes about 13 s per run on one
  thread (C 0.05: 10 s, 0.2: 17 s). That is about 1.4 runs/s, or 5,000 runs/h, at 20 threads.

### First hint on SR vs IR
All numbers are **percentage points** of the post-stabilization baseline. `*_sp_ext_*` columns are
percentages, not counts.
- **One condition** (fraction protected 0.2, extinction prob. 0.5, 13×13 tiles, 5 seeds;
  `emergent_foraging.csv`): IR − SR species loss is +0.2 to +0.8 pp, interaction loss 0 to −1 pp.
  SR loses 11–15% of species, against 19–21% under the old S1 model.
- **`PaperSweep` smoke run** (7 seeds × C 3 × t {0.5, 1, 2, 4} × frac 5 × pExt {0.3, 0.8}; averaged over
  frac and pExt):
  - IR saves interactions at every grain. The saving is largest at fine tiles: about −4 pp at
    t = 0.5, falling to about −0.3 pp at t = 4.
  - The species cost of IR is about +0.5 to +0.8 pp at t ≥ 1.
  - At t = 0.5, IR's species cost is zero or negative (C = 0.05 and 0.2).
  - So the trade-off has the same direction as in the basin v2 sweep, and management grain
    clearly moves it. This needs the full sweep.

### Sweep runner (`PaperSweep.scala`)
- Assembles once per (C, seed) with `World.assemble`. Every (t, frac, pExt) variant is then built by
  `World.fromAssembly` through `Observables.runWith(…, Some(assembly))`.
- The result is bit-identical to fresh runs (ModelChecks §7): the assembly keeps a serialized RNG
  snapshot and a copy of the web.
- Resumable. Speed is about 5–7 runs/s at 20 threads, against 1.4 without reuse.
- Default grid: C {0.05, 0.1, 0.2} × t {0.5, 1, 2, 4} × frac 20 steps (0.05–0.40) × pExt {0.3, 0.5, 0.8}
  × 30 seeds = 21,600 runs, about 1–1.5 h.

## Overnight paper sweep results (2026-10-14, `data/paper_sweep/paper_sweep.csv`)

54,000 runs (C 0.05/0.1/0.2 × t 0.25..8 × 20 budgets 0.05–0.40 × pExt 0.3/0.5/0.8 × 50 seeds), 0 failed, 4.4 h.
Scripts and figures: see `analysis/README.md` (scripts in `analysis/paper_sweep/`, figures in
`analysis/figures/paper_sweep/`, data in `data/paper_sweep/`). Contrasts IR − SR in pp (negative = IR loses less).

- Grain is the dominant axis. t = 0.25: IR better on BOTH (species −1 to −2 pp, interactions −4 to −8 pp; 145/180 cells).
  t = 0.5: IR saves interactions (−3.5 to −5 pp), species ≈ tie. t = 1–4: trade-off (IR −0.5 to −2.3 pp ints, +0.4 to +1 pp species).
  t = 8 (9 tiles): contrasts small, 63% of runs pick identical plans.
- Higher pExt amplifies everything (roughly ×2 from 0.3 to 0.8); C damps the interaction advantage (t=0.25: −8.2 → −4.2 pp from C .05 → .2).
- IR's species advantage at fine grain is in top predators / large bodies (top −3.6 pp); its species cost at coarse grain is in
  basal/intermediate/small-bodied species.
- Caveats: t = 0.25 at high budget saturates (losses → 0, many ties); t ≥ 4 has coarse budget discreteness (36 / 9 tiles).

### Population-level look (2026-10-15, budget 0.15, pExt 0.8; `ProtectedSetExport`, figures in `analysis/figures/paper_sweep/`)
- At t = 0.25, 15% of the tiles hold 45–57% of all populations (clustered placement); at t = 2, 27–29%.
- Species protected: both strategies cover nearly the same species and body-mass distribution (≈165–175 of ~175–190
  species at t = 0.25). The difference is WHICH populations: IR picks fewer small-bodied herbivore populations and
  more consumers.
- Final webs at t = 0.25: SR keeps more herbivore biomass (60–83% vs 50–66%) but its retained share falls with TP;
  IR keeps a flatter profile (TP≈4: 51–78% vs 37–72%; TP ≥ 4.5: 35–70% vs 20–62%). At t = 2 the two are
  indistinguishable and both lose top-heavily.

### Species loss by trophic-position class (2026-10-15, pExt 0.8; `TrophicLossSweep`, `data/paper_sweep/trophic_loss.csv`)
Species classes from biomass-weighted flow TP (+1 for plants): ~5–20% herbivores (36/20/9 species at C .05/.1/.2),
≈3 is the largest class (~90–110), ≈4 (43–69), ≥4.5 rare (2–8). Loss rises steeply with TP under both strategies
(herbivores ≤ 5%, top class 70–95% at low budget). IR's fine-grain gain (t = 0.25) is concentrated in TP ≈ 4
(up to −10 pp) and ≥ 4.5 (up to −25 pp at C = 0.05), paid by +1–2 pp on herbivores. At t = 0.5 differences are
within ±2 pp except the noisy top class; at t = 2 IR loses +1–3 pp more of TP ≈ 3–4 (the species cost seen in averages).
