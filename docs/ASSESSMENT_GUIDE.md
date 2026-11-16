# Assessment guide — read this first

> **Note added 2026-11-16, after the repository clean-up.** This guide was written *before* the
> reorganisation and is kept unchanged below as the hand-over record. Paths have moved:
>
> | In this guide | Now |
> |---|---|
> | `MODEL_PARAMETERS.md`, `ASSESSMENT_GUIDE.md` | `docs/` |
> | `biomass_pyramid_plan.md` | `docs/MODEL_DESIGN_AND_RESULTS.md` |
> | `emergent_*.csv`, `assembly_diagnostic_emergent.*` (root) | `data/emergent_design/` |
> | legacy root outputs, `*.oms`, `plot_*.py`, `analyze_*.py`, `drago/`, `PSE_analysis.ipynb` | `legacy/<period>/` (see `legacy/README.md`) |
> | legacy Scala runners and probes (§4.3) | `legacy/scala/` (no longer compiled) |
>
> The assessment that this guide asked for is `docs/report/project_assessment.pdf`. The code
> simplification proposed in §8 has **not** been done: model code is untouched.

*Written 2026-11-16 by the Claude session that built the current model and analyses, for a new session that will
(1) assess the project's state, results and weak points with a view to a scientific article, and (2) decide which
parts of this repository are current and valuable, so that a cleaned-up version can be made without legacy material.*

The repository is messy: about two years of model versions, sweeps and figures are mixed at the root.
**Most of what you will see is legacy.** The current work lives in a small set of files, listed in §3–§5.
Everything is on branch `feature/biomass-pyramid`. The current model, analyses and paper data were committed on 2026-11-16;
**the legacy outputs at the repo root are still untracked** (see §9.1).

---

## 1. The project in one paragraph

This is a simulation framework (Scala 3, sbt) asking whether conservation plans that prioritise **biotic
interactions** protect ecological networks better than plans that prioritise **species richness**. Each run:
- builds a virtual metaweb (niche model, 300 species);
- places populations in a continuous unit square;
- keeps an interaction only where predator and prey home ranges overlap;
- lets energy flow from a fixed basal biomass decide which populations are viable;
- splits the landscape into square management tiles and protects a fraction of them with one of two greedy
  strategies;
- applies tile-dependent primary extinctions (protected 1%, unprotected `pExt`), followed by an energetic cascade.

The outputs are the % of species and % of species-level interactions lost, compared with the stable community
before extinctions. Funding: BBVA Foundation Leonardo fellowship 2024 (see `README.md`). The PI (the user) is an
ecologist; they make the design decisions and should be consulted on modelling choices.

**Paper comparison (decided 2026-10-09): SR vs IR.**
- **SR** = greedy top-k tiles by species richness.
- **IR** = greedy top-k tiles by *egocentric* interaction richness, meaning interactions with at least one end in
  the tile. In code: `ConservationStrategy.EgocentricInteractionRichness`.
- In the current outputs it is labelled `"IR"` (`Observables.paperStrategies`).
- The naive tile-internal interaction richness (`ConservationStrategy.InteractionRichness`) was dropped: SR beat it
  everywhere.

---

## 2. Model history (why most results in the repo do not transfer)

| Period | Model | Status |
|---|---|---|
| ≤ 2026-06 | Fixed number of populations, Thomas/Matérn placement, hexagonal then square tiles, 12 strategies, `preyPerPredator` load cascade | legacy; OpenMOLE sweeps `*.oms`, `tile_size_sweep*.csv`, `protection_strategy_sweep*.csv`, `PSE_analysis.ipynb`, `strategy_analysis.pdf`, `Figure_*_plos.eps` |
| 2026-09 | Same model with the "metabolic" ε-cascade; Saltelli/Sobol sensitivity analysis (OpenMOLE) | legacy; `saltelli_sensitivity*.{oms,md,csv}`, `SaltelliRunner.scala`, `drago/` (SLURM path, never used in the end) |
| 2026-09/10 | Basin sweeps (SR vs naive IR vs EIR over tile size × budget × home range) | legacy; `basin_sweep*.csv`, `basin_*png`, `BasinSweep.scala`, `analyze_basin_sweep.py` |
| 2026-10-09 | Diagnosis: the community **collapsed** at stabilisation. Populations were allocated ∝ m^-0.75 with no trophic scaling, giving an inverted energy pyramid: ~95% of consumer nodes starved and top predators were absent. Web-structure sweep. | legacy diagnostics; `web_structure_*`, `WebStructureSweep.main`, `assembly_diagnostic*.csv` |
| 2026-10-10/12 | "biomassPyramid" abundance model | **superseded**, still in the code as `abundanceModel = "biomassPyramid"` |
| **2026-10-13/14** | **"emergent" assembly**: range seeding plus bottom-up energy flow (§3) | **CURRENT, paper model** |
| 2026-10-14/15 | Paper sweep (54k runs), population-level export, trophic-class sweep | **CURRENT results** (§6) |

Earlier conclusions were drawn on the collapsed or legacy communities: "SR wins in 79% of Saltelli runs", "IR only
wins at coarse tiles" (basin v2), and so on. **Do not carry them into the article without re-checking them on the
emergent model.** Several of them reversed (§6).

---

## 3. The current model (paper configuration)

The authoritative description is **`MODEL_PARAMETERS.md`**. Its top table gives the paper parameter set, and §B′
covers the emergent assembly. The design rationale and the evidence for each choice are in
**`biomass_pyramid_plan.md`**, section "Emergent assembly (decided design, 2026-10-13/14)" onwards. The earlier
sections of that file are historical.

Pipeline for one run (`PaperSweep.worldParameters` holds the fixed values):

1. **Metaweb.** Niche model with 300 species and metaweb connectance C ∈ {0.05, 0.1, 0.2} (the web-density axis).
   Body masses are log-normal (σ = 1). The topological trophic level (0 basal / 1 / 2 top) comes from in/out
   degree. **Basal species are herbivores; plants are implicit.**
2. **Home ranges.** r ∝ M^0.5, rescaled to a median of 0.02 (landscape = unit square).
3. **Range seeding.** Each species gets a range fraction ρ_s = ρ̄·LN(0, 1), with ρ̄ = 0.03. It then receives
   k_s = max(1, Poisson(ρ_s/πr_s²)) home-range-sized seed populations, placed by a Matérn cluster process
   (5 patches on average). Species occupy *sections* of the landscape; co-occurrence of potential partners is not
   required. Spatial filtering of interactions is intended behaviour (see §9.5).
4. **Population web.** A link is realised when a predator and prey are within r_pred + r_prey of each other and
   linked in the metaweb (`PopulationWebGenerator`).
5. **Emergent energetics** (`EnergyFlow.scala`, bottom-up / donor control):
   - Total basal biomass B₀ = 5·10⁶ is split over the basal populations.
   - Each consumer population of n individuals of mass M needs n·M^0.75.
   - Prey supply is TE·n·m, with TE = 0.1, shared among predators in proportion to their demand.
   - Population size is min(cap, ⌊intake/M^0.75⌋).
   - Populations below n_min = 10 are removed greedily (worklist relaxation).
   - Foraging rule "supply" (the "equal" and "niche" alternatives were tested and made no material difference).
6. **Management grid.** Square tiles. The side is given in median home-range **diameters**,
   `tileSizeInHomeRanges` t ∈ {0.25, 0.5, 1, 2, 4, 8}, which gives 100², 50², 25², 13², 6² and 3² tiles.
7. **Protection.** ⌊frac × nTiles⌋ tiles (truncated) are chosen greedily by the strategy's tile score.
8. **Extinctions.**
   - Phase 0: stabilisation, the energy-flow cascade on the assembled web.
   - Phase 1: primary extinctions, one Bernoulli draw per population (protected 0.01, unprotected pExt).
   - Phase 2: the energy-flow cascade again, with population sizes capped at their assembled values.
9. **Outputs** (`Observables.runStrategy`): % of species and % of species-level interactions lost, relative to
   the post-Phase-0 baseline, split into primary and secondary losses and by group. Every strategy runs on the
   **same** base world, so contrasts are paired.

**Reusable assembly.** `World.assemble` keeps an RNG snapshot and a copy of the web. `World.fromAssembly`
rebuilds the world for any tiling, pExt or plan, and the results are **bit-identical** to `World.apply` (checked in
ModelChecks §7). This is what makes the sweeps fast.

---

## 4. Code map — `src/main/scala/econetcons/`

### 4.1 Current core (keep)

| File | Role | Since bfb05f7 |
|---|---|---|
| `World.scala` (1065 lines) | Metaweb, placement, assembly (`assemble`/`fromAssembly`/`apply` = `assembleInPlace` + `finish`), extinction phases. Also contains the legacy abundance and cascade models (damuth, biomassPyramid, metabolic/legacy/energyTolerance cascades), see §8. | modified |
| `EnergyFlow.scala` | Emergent energetics: `relax`, `Diet` rules, `intakes`, `flowTrophicPositions` | new |
| `WorldParameters.scala` | All parameters; `tilesPerSide`, `resolvedPlacement`, `extinctionProbability` | modified |
| `Species.scala`, `Population.scala` | Traits (HR exponent) and placement | modified |
| `PopulationWebGenerator.scala` | Spatial realisation of links | tracked |
| `ManagementLandscape.scala`, `ManagementArea.scala`, `TileSelection.scala`, `ProtectionStatus.scala` | Tiles, scoring, protection plans (also hold many legacy strategies) | tracked |
| `ConservationParameters.scala`, `ConservationStrategy.scala` | Strategy enum (12 strategies; the paper uses 2) | tracked |
| `SquareGrid.scala`, `Utils.scala` | Grid geometry, Matérn process | tracked |
| `Observables.scala` | Paired per-strategy outcomes (`runWith`, `runStrategy`, `paperStrategies`) | modified |

### 4.2 Current runners and checks (keep)

| File | Purpose |
|---|---|
| `PaperSweep.scala` | **Main paper sweep** (resumable, assembles once per (C, seed)) | 
| `ProtectedSetExport.scala` | Population-level export of protected sets and final webs. `replay()` repeats `runStrategy` exactly; `--verify` checks it against `runWith`. |
| `TrophicLossSweep.scala` | Species loss by flow-trophic-position class against budget |
| `ModelChecks.scala` | Consistency checks for the emergent model (energy balance, foraging rules, B₀ conservation, tiling, reuse == fresh). Run: `sbt "runMain econetcons.ModelChecks"` |
| `AssemblyDiagnostic.scala` | Diagnostics of assembled communities (biomass by level, flow TP, richness). Used for the design decisions. |
| `WebStructureSweep.scala` | **Only `WebStructureSweep.metrics` is current** (PaperSweep uses it); its `main` is a legacy sweep |

### 4.3 Legacy (candidates for removal; verify nothing current imports them)

- `BasinSweep`, `SaltelliRunner`, `ProtectionSweep`, `FineProtectionSweep`, `UltraFineProtectionSweep` and
  `TileStrategySweep` are runners of earlier model versions.
- `OneSimulation`, `SpotCheck`, `TestDiagnostics`, `ParameterDiagnostics`, `SpatialDiagnostics` and
  `DiagnosticMetrics` are older diagnostics. `DiagnosticMetrics` is used by the `.oms` workflows and by SpotCheck
  and SpatialDiagnostics only.
- `BoxMeanProbe`, `CornerSmokeTest`, `MemProbe`, `PPPCheck`, `TimingHarness` and `WebCalibration` were throwaway
  probes.
- `HexagonalGrid` is referenced only in a comment.
- `RegressionSnapshot` is a useful *pattern*, but it snapshots the **legacy default (damuth)** configuration, not
  the paper model. The reference snapshot lived in a temporary directory and is gone. Rebuild it for the paper
  model.

There is **no `src/test`**. Verification is done by `ModelChecks` plus the bit-identity checks above.

---

## 5. Data, analysis scripts, figures

### 5.1 Current (keep)

The layout was started 2026-10-15; see **`analysis/README.md`**, which has a table with the exact command for
every step.

```
data/paper_sweep/
  paper_sweep.csv            54,000 rows: C{.05,.1,.2} × t{.25,.5,1,2,4,8} × 20 budgets (0.05–0.40) × pExt{.3,.5,.8} × 50 seeds
  paper_sweep_summary.txt    tables printed by analysis/paper_sweep/summary.py
  paper_sweep_cellstats.csv  mean / SE / classification per (C,t,frac,pExt) cell
  protected_sets.csv.gz      per-population rows (baseline / protected / final), budget 0.15, pExt 0.8, t{.25,2}
  trophic_loss.csv           species lost per trophic-position class, t{.25,.5,2}, pExt 0.8, 50 seeds
  *.log                      run logs (0 failed runs everywhere)
analysis/paper_sweep/        summary.py, heatmaps.py, loss_curves.py, jaccard.py, protected_sets.py,
                             loss_by_trophic_class.py, paths.py (shared paths/colours)
analysis/figures/paper_sweep/  13 PNGs (listed in analysis/README.md)
```

### 5.2 Current design evidence (root; keep or move to `data/emergent_design/`)

These files back the parameter choices in `biomass_pyramid_plan.md`:
- `emergent_biomass_k.csv` and `emergent_biomass_N0.csv`: B₀ and seeding;
- `emergent_range.csv`: ρ̄, HR and b;
- `emergent_checks_tiles{0,13}.csv`: tile geometry;
- `emergent_foraging{,_species}.csv`: foraging rules;
- `assembly_diagnostic_emergent.csv`.

They are referenced from the plan, so keep them if the article needs a methods/calibration supplement.

### 5.3 Legacy (root)

All other `*.csv`, `*.log`, `*.png`, `*.pdf`, `*.eps` and `*.oms` files at the root, plus `plot_*.py`,
`analyze_{basin_sweep,web_structure,assembly_diagnostic}.py`, `PSE_analysis.ipynb`, `drago/`,
`econetcons_optimization_plan.md` and `saltelli_sensitivity_plan.md`. `metaweb-creation-steps.{pdf,png,svg}` is a
schematic of the metaweb generation and may still be useful as a methods figure; check it against the current
model.

### 5.4 Documents

| File | Status |
|---|---|
| `MODEL_PARAMETERS.md` | current (top table + §B′). Lower sections also document legacy parameters, flagged as such. |
| `biomass_pyramid_plan.md` | Mixed. The first half is the superseded biomassPyramid plan. From "Emergent assembly" onwards it is the current design and all current results. The filename is misleading; consider renaming it to something like `MODEL_DESIGN_AND_RESULTS.md`. |
| `analysis/README.md` | current; reproduction commands |
| `README.md` | outdated project description (mentions hexagonal tiles and connectivity strategies) |
| `econetcons_optimization_plan.md`, `saltelli_sensitivity_plan.md`, `drago/README.md` | legacy |

---

## 6. Current results (what the article would rest on)

Units: losses are **% of the post-stabilisation baseline**, and contrasts IR − SR are **percentage points**.
Negative means IR loses less. Read `data/paper_sweep/paper_sweep_summary.txt` and the figures.

1. **Management grain (tile size) is the dominant axis.** Averaged over budget, pExt and C:

   | t | Δ species (pp) | Δ interactions (pp) | Interaction saving as % of SR's loss |
   |---|---|---|---|
   | 0.25 | −1.4 | −6.3 | −42% |
   | 0.5 | −0.2 | −4.4 | −20% |
   | 1 | +0.5 | −2.1 | −8% |
   | 2 | +0.8 | −0.9 | −3% |
   | 4 | +0.7 | −0.6 | −2% |
   | 8 | +0.3 | −0.4 | −1% |

   - At fine grain, IR is better on both species and interactions (145 of 180 cells significant).
   - At t = 1–4 there is a genuine trade-off: IR saves interactions and costs ≤ 1 pp of species.
   - At t = 8, 63% of runs pick identical plans.
   - This **reverses** the legacy basin-v2 conclusion ("IR wins only at coarse tiles").
2. **pExt amplifies** both effects, roughly doubling them from 0.3 to 0.8. **Higher connectance damps** IR's
   interaction advantage (t = 0.25: −8.2 pp at C = 0.05, −4.2 pp at C = 0.2).
3. **Budget** matters mainly at fine grain. IR's advantage peaks at 12–25% protected and vanishes when both
   strategies lose nearly nothing (≥ 30%). Figure: `loss_vs_budget_*`.
4. **The plans differ substantially**: Jaccard dissimilarity of the protected tiles is 0.55–0.72 at a 5% budget,
   and it falls with budget and with C (`jaccard_dissimilarity_protected_tiles.png`).
5. **Mechanism** (from the population-level export, budget 0.15, pExt 0.8):
   - Both strategies protect nearly the same *species* set, with the same body-mass distribution.
   - At fine grain they protect different *populations*: IR protects fewer small-bodied herbivore populations
     and more consumers.
   - At t = 0.25, 15% of the tiles hold 45–57% of all populations (clustered placement); at t = 2 the figure is
     about 28%.
6. **Trophic structure** (`loss_by_trophic_class*`):
   - Loss rises steeply with trophic position under both strategies: herbivores ≤ 5%; the top class 70–95% at
     low budget.
   - IR's fine-grain gain is concentrated at TP ≈ 4 (up to −10 pp) and TP ≥ 4.5 (up to −25 pp at C = 0.05, but
     only 2–8 species, so it is noisy), paid for with +1–2 pp of herbivores.
   - At t = 2, IR loses 1–3 pp more of TP ≈ 3–4.
   - In the final webs at t = 0.25, SR keeps more herbivore biomass, but its retained share falls with TP. IR's
     profile is flatter (`final_biomass_share_by_trophic_position.png`).

The longer write-up is in `biomass_pyramid_plan.md`, from "Overnight paper sweep results" onwards.

---

## 7. Weak points and open questions (please assess these critically)

**Model.**
- Primary extinction is **one Bernoulli draw per population, regardless of population size**. Populations now
  have emergent sizes (10 to thousands of individuals), so a 10-individual and a 10,000-individual population die
  with the same probability. This is probably the most important modelling inconsistency.
- The protected-tile extinction probability (0.01) is hard-coded (`WorldParameters.extinctionProbability`).
- Strategies are **greedy top-k by static tile score**, with no complementarity or connectivity. SR is therefore
  "richest tiles", not a set-cover optimum; reviewers may ask for complementarity-based planning (Marxan-like).
  Complementarity variants exist in the code from the legacy era but are untested on the emergent model.
- Two notions of trophic level coexist:
  - the topological `trophicLevel` (0/1/2 from metaweb degree), used in the `sp_ext_basal/intermediate/top`
    columns;
  - the flow trophic position, used in the new figures.

  The article should use one consistently.
- The energetics are bottom-up only; there is no top-down control or dynamics. Persistence is energetic
  feasibility at a static equilibrium, not population dynamics.
- Interactions are realised by distance (r_pred + r_prey). Metaweb links between species that never co-occur
  are simply absent, which is intended (§9.5).

**Sensitivity not yet done on the emergent model.** ρ̄ (the main richness/density knob: ρ̄ 0.01/0.03/0.1 gives
about 110/185/225 species and L/S 1.3/2.8/6.2), B₀, n_min, TE and median HR are fixed in the paper sweep. Only C,
t, budget and pExt are explored. The robustness of the grain result to ρ̄ and HR is the obvious missing
supplementary analysis.

**Statistics and design.**
- Cells are classified with |mean| > 2 SE over 50 seeds; there is no multiple-comparison control.
- Budgets are truncated to whole tiles, so t ≥ 4 (36 or 9 tiles) has coarse budget steps.
- t = 0.25 saturates at high budget: both strategies lose almost nothing, which produces ties.
- Robustness metrics (random-knockout shocks) are switched off (`econetcons.skipRobustness`) in all current
  sweeps.

**Engineering.**
- There are no unit tests.
- `World.scala` carries every legacy abundance and cascade model behind string switches (`abundanceModel`,
  `cascadeModel`, `placement`).
- Strategy labels collide across eras (§9.2).

---

## 8. Suggested scope of a cleaned repository

**Keep:**
- the §4.1 core and the §4.2 runners;
- `analysis/` and `data/paper_sweep/`;
- the §5.2 design evidence, moved to `data/emergent_design/` together with the script that summarises it;
- `MODEL_PARAMETERS.md`, reduced to the paper model;
- the current half of `biomass_pyramid_plan.md`, renamed;
- `build.sbt` and `project/`.

**Remove or archive:** everything in §5.3, the legacy runners in §4.3, and `drago/`.

**Simplify the code (a separate, careful step that needs a regression check):**
- remove `abundanceModel ∈ {damuth, transferEfficiency, energyBalance, biomassPyramid}` and the non-emergent
  `cascadeModel` branches from `World.scala`;
- remove the parameters that are unused under the emergent model (`nPopulations`, `epsilon`,
  `avgPopulationsPerTile`, `preyPerPredator`, `dietBreadthThreshold`, `energyTolerance`, biomassPyramid fields);
- remove the 10 unused strategies (keep SR, EIR and perhaps Random as a null model).

Before simplifying, write a **paper-model regression snapshot**: `PaperSweep.worldParameters` + `Observables.runWith`
for a few seeds, with every output written to a file. After the cleanup the outputs must be bit-identical.
`ModelChecks` and the `--verify` modes of the exporters provide additional checks. The `assembly` / `osgi` plugin
config in `build.sbt` exists only for the SLURM fat JAR and can go with `drago/`.

---

## 9. Pitfalls

1. **What is and isn't in git.** The current code, docs, `analysis/`, `data/paper_sweep/` and the emergent
   design-evidence CSVs (`emergent_*`, `assembly_diagnostic_emergent.*`) were committed on 2026-11-16 (two commits
   after `bfb05f7`, which is the pre-emergent model). The **legacy root outputs (~110 MB of CSVs, PNGs, EPS, PDFs,
   `.oms`, `drago/`, old `plot_*.py`) are untracked**. Deleting them is irreversible, so ask the user before removing
   or archiving them.
2. **"IR" means different things in different files.** In `data/paper_sweep/*` and all current figures, `IR_*` is
   the *egocentric* IR (`paperStrategies`). In legacy CSVs (basin sweeps, Saltelli), `IR_*` is the naive IR and
   `EIR_*` is the paper's IR.
3. **Sign convention.** Δ = IR − SR losses, so Δ < 0 means IR is better. The Saltelli digest got this backwards
   once.
4. **Units.** `*_sp_ext_*` and `*_spint_ext_*` are percentages, not counts. Group columns (`sp_ext_top`,
   body-mass quartiles) are % *of that group*.
5. **Spatial filtering is intended.** Populations or links dropped because partners do not co-occur within reach
   are a design choice of the PI, not a bug. Do not propose co-locating species or keeping every drawn
   population. The PI cares that the energetic pruning *after* spatial filtering is modest and that the stable
   community is rich and realistic.
6. **Run commands.** Run them with `sbt -J-Xmx40g "runMain econetcons.<Object> ..."`. The machine has 22 cores and
   62 GB of RAM. The full paper sweep takes about 4.5 h on 20 threads; t = 0.25 (10,000 tiles) is the slow part.
7. **JAR.** `build.sbt` sets the assembly JAR's main class to `SaltelliRunner` (legacy).

---

## 10. Suggested reading order

1. This file, then `MODEL_PARAMETERS.md` (top table and §B′).
2. `biomass_pyramid_plan.md` from "Emergent assembly" to the end: design evidence and results.
3. `analysis/README.md`, then `data/paper_sweep/paper_sweep_summary.txt`, then the figures in
   `analysis/figures/paper_sweep/`. Start with `loss_vs_budget_*`, `contrast_heatmap_pext0.8.png` and
   `loss_by_trophic_class_diff_pext0.8.png`.
4. Code: `PaperSweep.scala`, then `Observables.runWith`/`runStrategy`, then `World.assembleInPlace`/`finish` and
   the extinction phases, then `EnergyFlow.relax`.
5. `ModelChecks.scala`, to see what is verified and what is not.

To sanity-check the setup without long runs:
- `sbt "runMain econetcons.ModelChecks"` (a few minutes);
- `python3 analysis/paper_sweep/summary.py`, which reproduces the summary tables from the existing CSV.
