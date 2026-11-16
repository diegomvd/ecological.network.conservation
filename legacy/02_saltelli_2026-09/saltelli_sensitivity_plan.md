# Saltelli Sensitivity Analysis — Design Plan

Status: **design LOCKED; execution pivoting OFF OpenMOLE** (2026-09-06).
Method: **Sobol / Saltelli**, first + total order indices.

The whole point of the project: understand how *local* conservation strategies based on
**interaction richness** vs **species richness** have different protection effects. So the
sensitivity analysis targets **contrasts** (strategy-difference metrics), not raw `%extinct`.

---

## ⚠️ EXECUTION PIVOT — abandon OpenMOLE, use plain SLURM job array + SALib (2026-09-06)

**Why:** OpenMOLE cannot run on the available cluster (CSIC **Drago**) — confirmed with BOTH the
OM devs and the Drago admins (Apptainer/container execution not usable there). The prior target
clusters are dead ends too: **atlas** is being decommissioned (2026) and job submission silently
stalls; **hyperion** rejects submission — the `bc3`/`integratedmodelling` association has
`GrpSubmit = 0` and no partition/QOS binding (allocation effectively closed). All OM-side
plumbing was proven correct (SSH, auth, staging/uploads, the `sample=1024` sampler ran locally);
the block is purely cluster access/policy.

**New architecture (reuses ~all the Scala — the model & `Observables.run` are untouched):**
1. **Design matrix** — generate the Saltelli sample OFFLINE with Python **SALib**
   (`saltelli.sample`), 8 inputs, N=1024 → 1024·(2·8+2)=… (SALib default gives N·(2k+2) rows for
   S1+ST+2nd order, or N·(k+2)=10,240 with `calc_second_order=False`). Write to a design CSV.
   Match the k=8 ranges below exactly.
2. **Scala CLI `main`** — thin wrapper (~30 lines) around the SAME logic as the `.oms` ScalaTask:
   read one design-point row (8 inputs) from args, run the **5-seed** `Observables.run` averaging,
   round nPop/tile to Int, emit the 17 output columns (6 Δ contrasts + 11 raw anchors) as a CSV
   line. Build a runnable JAR (`sbt assembly`, or reuse the OSGi jar + a launcher).
3. **SLURM job array on Drago** — native `sbatch --array=...`, NO container. Chunked (row-range
   per array task) for JVM-startup amortization, made SAFE by: (a) each task writes its OWN output
   file `results_${SLURM_ARRAY_TASK_ID}.csv` (no shared-file contention), (b) idempotent/resumable
   per row, (c) a final verification that every design row appears exactly once before analysis.
4. **Sobol analysis** — OFFLINE with SALib `sobol.analyze` (same problem def), per output, with
   bootstrap CIs. Cleaner/more auditable for the paper than the OM black box.

**Decisions (2026-09-06):** sample-gen + analysis in **Python/SALib**; array uses **row-range
chunks made safe** (own-file-per-task + verify-completeness), not one-row-per-task.

**Reference artifacts already built (reuse):** the `.oms` ScalaTask body in
`saltelli_sensitivity.oms` is the exact per-point logic to port into the CLI main (5-seed avg,
Int rounding, the 17 output keys). `Observables.run` already has the needed params
(`extinctionProbabilityUnprotected`, `bodySizeSigma`, `meanPatchesPerSpecies`). Debug `println`s
already removed from `World.scala`.

**NEXT SESSION TODO:** (a) write `SaltelliRunner` Scala main; (b) `sbt assembly` setup / launcher;
(c) SALib `gen_design.py` + `analyze.py`; (d) `run_array.sbatch` for Drago (need: Drago login host,
username, scratch path, Java module name, array-size limits); (e) completeness verifier.

---

## How the model runs from OpenMOLE

- Canonical entry point: **`Observables.run(...)`** in `src/main/scala/econetcons/Observables.scala`.
- One call = one replicate: builds a single base world for a `seed`, applies each conservation
  strategy (SR / IR / EIR) to the *same* world, runs the 3-phase extinction pipeline, returns a
  flat `Map[String, Double]` (~111 columns): per-strategy metrics (label-prefixed `SR_*`/`IR_*`/`EIR_*`)
  plus pairwise `pair_SR_IR_*` divergence columns.
- An OM `ScalaTask` wraps this call, maps map entries to `Val[Double]` outputs, and `pluginsOf(...)`
  exposes the `econetcons` classes. See `PPPCheck.scala` / `TileStrategySweep.scala` for the current
  calling convention.

⚠️ **The existing `.oms` files are STALE** (`tile_size_sweep*.oms`, `strategy_comparison.oms`): they
reference `WorldParameters` fields that no longer exist (`areaOverlap`, `useThinning`,
`spreadingFactor`) and use the old `extinctionsDetailed()` path. Do NOT base the new script on them.

Current `Observables.run` signature (positional):
```
run(numberOfSpecies, connectanceMetaWeb, nPopulations, medianHomeRange,
    avgPopulationsPerTile, preyPerPredator, fractionProtected, seed,
    strategies = defaultStrategies, epsilon = 0.0)
```

---

## Timing & feasibility (measured, not estimated)

Temporary harnesses used: `TimingHarness.scala`, `MemProbe.scala` (both TO BE DELETED after `.oms`
is validated).

**Per-run wall time** — CORRECTED after the corner smoke-test (the first harness only swept
nPop/nSpecies/epsilon and badly under-measured cost):
- `epsilon` / `nPopulations` / `numberOfSpecies` are NOT the dominant cost drivers.
- **`avgPopulationsPerTile` and `fractionProtected` dominate.** Large tiles (200) + high frac (0.95)
  → expensive per-tile scoring over the whole landscape.
- Measured corners (2 seeds each, all 10 corners ran clean, 0 failures):
  - cheap corners (small tiles, low frac): ~0.2–2 s
  - mid-box corners: **~14–20 s**
  - `all-high` (tile=200, frac=0.95): **~60 s**
- Representative mid-box run ≈ **~15 s** (NOT the ~0.4 s originally estimated).

**Peak heap per run:** ~320 MB at the worst corner (C=0.05 maximizes surviving edges), fully
reclaimed after (no leak; settles ~4 MB).

**Feasibility on target PC (20 cores, 64 GB RAM): CORRECTED — multi-day at full spec.**
- Full design N=512, k=10, 50 seeds → N·(k+2)·seeds = 512·12·50 = **307,200 runs**.
- Compute at ~15 s/run representative: ~1,280 h serial → **~64 h (~2.7 days) on 20 cores**.
  At an optimistic 8 s avg: ~34 h. This is a multi-day run, NOT an afternoon.
- Memory: 20 workers × ~320 MB worst case ≈ **6.4 GB live** → ~10–15% of 64 GB. Non-issue.
- **BUDGET DECISION NEEDED** (see Open items): reduce N, reduce seeds, drop total-order (use
  N·(k+1)), and/or narrow the tile/frac ranges that dominate cost. Options to weigh next session.

**Practical OM local-run notes:**
- Use `LocalEnvironment(18)` (leave 2 cores for OM/JVM coordinator + OS).
- Set a generous JVM heap ceiling (e.g. `-Xmx32g`–`40g`) to avoid GC stalls; you're far from the limit.

---

## Inputs — Saltelli varied factors (k = 8) — REVISED 2026-09-05 (cost reduction)

Revision to hit a ~12–16 h budget on 20 cores: fixed `numberOfSpecies` (200) and `bodySizeSigma`
(1.0), and trimmed `fractionProtected` ceiling 0.95 → 0.45 (high frac was a top cost driver).
`avgPopulationsPerTile` stays VARIED (user: not movable as a fixed axis).

| # | Parameter | Range | Type | Notes |
|---|---|---|---|---|
| 1 | `connectanceMetaWeb` | 0.05 – 0.25 | continuous | dominant structural knob; niche model caps ~0.5 (β = 0.5/C − 1) |
| 2 | `epsilon` | 0.5 – 5.0 | continuous | scarcity/cascade depth; **must stay > 0** (0 disables energetic model) |
| 3 | `nPopulations` | 5000 – 10000 | integer | runtime driver |
| 4 | `medianHomeRange` | 0.01 – 0.05 | continuous | avoids dense/slow 0.1 corner |
| 5 | `extinctionProbabilityUnprotected` | 0.05 – 1.0 | continuous | floor 0.05 keeps protection signal alive (protected rate = 0.01 hardcoded) |
| 6 | `fractionProtected` | **0.05 – 0.45** | continuous | trimmed from 0.95 |
| 7 | `avgPopulationsPerTile` | **10 – 100** | integer | "tile size"; capped 200→100 (TILE SIZE is the dominant cost driver, not frac) |
| 8 | `meanPatchesPerSpecies` | 1 – 10 | continuous | spatial patchiness |

**Replication:** `seed` — **5 replicas** per Saltelli point (50 → 25 → 20 → 5).
Final tradeoff: expert advice is N=1024 is recommended (512 a floor) for trustworthy k=8 Sobol
indices, so N was protected and SEEDS were cut instead — the paired Δ contrasts (SR/IR/EIR on the
same base world per seed) already cancel shared-world variance, so per-point noise is small.
N=1024 × 5 seeds → ~51,200 runs → ~8–9 h on 20 cores at measured 11.9 s/run.
Implemented as internal seed-averaging inside the ScalaTask (Saltelli needs one deterministic
value per design point; OM native Replication composition with SensitivitySaltelli is undocumented,
so internal averaging is the safe choice). VALIDATE seed count from pilot bootstrap CIs.
Rationale: Sobol already aggregates variance across the whole design (N × seeds feeds each index),
so few seeds per point suffice; and the Δ contrasts are **paired** (SR/IR/EIR evaluated on the SAME
base world per seed), so shared-world variance cancels and the difference is far less noisy than
either arm. VALIDATE from the pilot: bootstrap S1/ST over seed subsets (15 vs 25) — if rankings are
stable at 15, 25 has margin; if they wobble at 25, raise N (preferred) rather than seeds.

### Fixed parameters (NOT swept)
| Parameter | Fixed value | Reason |
|---|---|---|
| `numberOfSpecies` | **200** | fixed 2026-09-05 to cut a dimension (was 100–300) |
| `bodySizeSigma` | **1.0** | fixed 2026-09-05 to cut a dimension; calibrated value (3.0 pathological) |
| `observationNoise` | 0.0 | data-quality robustness deferred to a separate analysis |
| `falsePositiveRate` | 0.0 | same |
| `patchPackingFactor` | 1.0 | non-monotone secondary axis |
| `nAdminRegions` | 1 | must be a perfect square → not a clean continuous Saltelli input; separate governance study |
| `tileSelection` | Argmax | categorical; go-forward rule |
| `dietBreadthThreshold` | 0.0 | legacy; only active when ε=0 (never here) |
| `preyPerPredator` | 0.0 | deprecated, superseded by ε |

---

## Outputs

**Design principle:** run Sobol on **contrasts** (strategy differences), not raw `%extinct`.
**Analyze few, emit all** — output count is free (Sobol indices are computed post-hoc per output;
cost is driven by k and seeds, not #outputs).

**Strategy plan:** first design compares **naive InteractionRichness (IR) vs SpeciesRichness (SR)**.
EIR (egocentric) is computed anyway → **emit EIR raw outputs and add the EIR-vs-SR gap**, but do
NOT compare EIR-vs-IR. All contrasts are "vs SR".

**Family B (selection-divergence: `jaccard`, `spearman`) — DROPPED from the analyzed set.**
Tradeoff accepted: Family B was the sign-cancellation-proof family. Mitigation: all raw
`pair_SR_IR_*` columns are still *emitted*, so any Family-B metric can be reconstructed post-hoc
from the CSV without re-running. We just don't center the Sobol analysis on it.

### Analyzed by Sobol (curated contrasts)
| Output | Definition | Currency |
|---|---|---|
| `dSp_IR_SR` | `IR_sp_ext_total − SR_sp_ext_total` | species loss |
| `dSpInt_IR_SR` | `IR_spint_ext_total − SR_spint_ext_total` | interaction loss ★ **headline candidate** |
| `dRobust_IR_SR` | `IR_robust_sp_ext_30 − SR_robust_sp_ext_30` | robustness (30% shock) |
| `dSp_EIR_SR` | `EIR_sp_ext_total − SR_sp_ext_total` | species loss |
| `dSpInt_EIR_SR` | `EIR_spint_ext_total − SR_spint_ext_total` | interaction loss |
| `dRobust_EIR_SR` | `EIR_robust_sp_ext_30 − SR_robust_sp_ext_30` | robustness (30% shock) |

**Signed-gap caveat:** a contrast that hovers near zero and flips sign has small, noise-dominated
variance → Sobol S1/ST noisy and slow to converge (occasional slightly-negative estimates are
normal estimator variance). Mitigation: emit the raw `SR_*`/`IR_*`/`EIR_*` underlying each gap so
a low index can be read as "insensitive" vs "both move together."

### Emitted but not analyzed
All 111 raw `SR_*` / `IR_*` / `EIR_*` / `pair_*` columns — for anchoring, convergence-checking,
EIR follow-up, and post-hoc Family-B reconstruction.

---

## Decisions FINALIZED (2026-09-05)

- **All three gap currencies kept** (species / interaction / robustness) for both IR-SR and EIR-SR.
- **No single headline output** — all three currencies equally important.
- **Primary focus: naive IR vs SR.** EIR-vs-SR is the secondary/follow-up comparison.

## DONE (2026-09-05)

- ✅ **Refactor of `Observables.run`** — added optional params `extinctionProbabilityUnprotected`,
  `bodySizeSigma`, `meanPatchesPerSpecies` (defaults = WorldParameters defaults 0.5 / 3.0 / 5.0, so
  `PPPCheck` / `TileStrategySweep` are unaffected). Threaded into the `WorldParameters` construction.
- ✅ **Corner smoke-test** (`CornerSmokeTest.scala`) — 10 extreme corners × 2 seeds, **0 failures**.
  Box is numerically safe (no `Invalid bounds`). `dSpInt_IR_SR` shows real, sign-varying signal.
  Surfaced the corrected timing (TILE SIZE is the dominant cost driver; see Timing section).
- ✅ **`.oms` written** — `saltelli_sensitivity.oms`. `SensitivitySaltelli`, k=8, `samples=512`,
  20 seeds averaged internally, `LocalEnvironment(18)`, hook → `saltelli_sensitivity_results.csv`.
  Int inputs (nPop, tile) sampled as Double, rounded in-task. NOTE: doc-verified param is `samples`
  (not `sample`). Full-script parse NOT yet validated in OpenMOLE (no local `openmole` confirmed).
- ✅ **Budget resolved:** tile capped 10–100, N=512, 20 seeds → 512·10·20 ≈ 102,400 runs.
  Projected **~8–14 h on 18 cores** (in target). Confirming box-mean run time via smoke-test v3
  (random interior sample).

## Smoke-test v2 finding (2026-09-05, trimmed box)

Trimming `fractionProtected` to 0.45 did **NOT** tame the expensive corner: `all-high` with
`avgPopulationsPerTile = 200` is still **~57 s** (vs ~60 s pre-trim). **The dominant cost is TILE
SIZE (large tiles → per-tile scoring over all populations), not fraction.** Mid-box corners ~12–17 s.
Implication: at a ~12–17 s mean, N=512 × 25 seeds (~128k runs) is still MULTI-DAY. To hit 12–16 h,
the real lever is the **`avgPopulationsPerTile` upper bound** (200 → e.g. 80–100), and/or lower N.
The `.oms` as written (N=512, tile→200) will overrun the target until this is decided.

## Open items (to resolve next session)

1. **BUDGET DECISION (blocking the `.oms`):** full spec = ~2.7 days on 20 cores. Levers to weigh:
   - Sobol N: 512 → 256 (halve) or 128.
   - Seeds: 50 → 20–25 (Sobol already aggregates variance; 50 may be more than needed).
   - Indices: first + total N·(k+2) → first-order-only N·(k+1).
   - Cost is dominated by high `avgPopulationsPerTile` + high `fractionProtected` corners —
     consider whether the tile range really needs to reach 200, or frac to 0.95.
   - Could stage: cheap pilot (N=64, 10 seeds) to screen low-index inputs, then a focused run.
2. **Write the `.oms`:** `ScalaTask` wrapping `Observables.run` (now with the 3 new params), Saltelli
   sampling over the 10 inputs, seed replication, `LocalEnvironment(18)`, CSV hook. Emit all 111 raw
   columns + the 6 curated contrasts.
3. **Cleanup:** delete `TimingHarness.scala`, `MemProbe.scala`, `CornerSmokeTest.scala` after the
   `.oms` validates.

---

## Reference

- Full parameter inventory with ecological rationale and evidence-based ranges: `MODEL_PARAMETERS.md`.
- Current calling convention examples: `src/main/scala/econetcons/PPPCheck.scala`,
  `src/main/scala/econetcons/TileStrategySweep.scala`.
