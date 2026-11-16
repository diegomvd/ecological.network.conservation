# Legacy archive

Everything here was produced by **earlier versions of the model** and is kept only for the record.
None of it is used by the current (emergent-assembly) model, its runners or its analyses.
**Do not carry conclusions from this folder into the article without re-checking them on the current
model** — several reversed (see `docs/ASSESSMENT_GUIDE.md` §2 and §6).

Git tracks only the light material (scripts, OpenMOLE workflows, notes, logs, Scala sources). The
bulky outputs (`*.csv`, `*.png`, `*.pdf`, `*.eps`, `*.svg`, about 110 MB) are git-ignored and exist
only on the machine where they were generated; ask Diego if you need them.

| Folder | Period | Model | Contents |
|---|---|---|---|
| `01_openmole_sweeps_2024-2026/` | ≤ 2026-06 | Fixed number of populations, Thomas/Matérn placement, hexagonal then square tiles, 12 strategies, `preyPerPredator` cascade | OpenMOLE workflows (`*.oms`), `tile_size_sweep*`, `protection_strategy_sweep*`, `plot_*.py`, `PSE_analysis.ipynb`, PLOS-style figures, `econetcons_optimization_plan.md` |
| `02_saltelli_2026-09/` | 2026-09 | Same, with the "metabolic" ε-cascade | Saltelli/Sobol sensitivity analysis (workflow, plan, results) and `drago/` (SLURM path, never used) |
| `03_basin_sweeps_2026-09-10/` | 2026-09/10 | Same | Basin sweeps SR vs naive IR vs EIR over tile size × budget × home range, scripts and figures |
| `04_collapse_diagnosis_2026-10/` | 2026-10-09/12 | Diagnosis of the collapsed community; superseded "biomassPyramid" model | Web-structure sweep, assembly diagnostics, their scripts |
| `scala/` | all | — | Runners, diagnostics and probes of the earlier versions, removed from the build |
| `misc/` | — | — | `metaweb-creation-steps.*`: schematic of *empirical* metaweb construction (habitat / trait filters); it does not describe this model's niche-model metaweb |

## Things to know before reading anything here

1. **"IR" changed meaning.** In these files `IR_*` is the *naive* tile-internal interaction richness and
   `EIR_*` is the egocentric one. In the current data (`data/paper_sweep/`) and in the paper, **IR is
   the egocentric variant** (the old EIR). The naive IR was dropped.
2. **The communities were collapsed.** Before the emergent assembly, ~95 % of consumer populations
   starved at stabilisation and top predators were absent. Strategy rankings obtained on those
   communities are not informative.
3. **Home-range exponent.** Results before 2026-10-14 used r ∝ M^0.75; the default is now 0.5.
4. **Sign convention** in the Saltelli digest was once reported backwards; Δ = IR − SR losses.

## Running legacy material

- Scripts read and write files **in their own folder** by bare filename, so run them from inside the
  era folder (data and scripts were moved together for that reason). `plot_strategy_analysis.py` has a
  hard-coded `BASE` path pointing at the old repository root; edit it first.
- The Scala files in `scala/` are no longer compiled. To run one, copy it back to
  `src/main/scala/econetcons/` (they still compile against the current core at the time of the move,
  commit "Move legacy Scala sources out of the build"). The `.oms` workflows need `DiagnosticMetrics`
  and the OSGi bundle; the SLURM path needs `SaltelliRunner` as the assembly main class.
