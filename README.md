# Ecological networks for conservation

A simulation framework (Scala 3 / sbt, analysis in Python) asking whether conservation plans that
prioritise **biotic interactions** protect ecological networks better than plans that prioritise
**species richness**.

Each run builds a virtual food web (niche model, 300 species), places populations in a continuous
landscape, keeps an interaction only where predator and prey home ranges overlap, and lets energy flow
from a fixed basal biomass decide which populations are viable. The landscape is then split into square
management tiles; a fraction of them is protected by one of two greedy strategies, primary extinctions
hit unprotected populations harder than protected ones, and an energetic cascade follows. The outputs
are the percentage of species and of species-level interactions lost.

- **SR** — protect the tiles with the highest species richness.
- **IR** — protect the tiles with the highest *egocentric* interaction richness (interactions with at
  least one partner in the tile).

Supported by the *Beca Leonardo de Investigación Científica y Creación Cultural 2024*, Fundación BBVA.

## Where to start

| If you want to… | Read |
|---|---|
| understand the project, its results, weak points and a proposed article narrative | `docs/report/project_assessment.pdf` |
| know every parameter and the paper configuration | `docs/MODEL_PARAMETERS.md` (top table and §B′) |
| see why the model is designed as it is, with the evidence | `docs/MODEL_DESIGN_AND_RESULTS.md` |
| reproduce the sweeps and figures | `analysis/README.md` |
| read the hand-over notes written before the clean-up | `docs/ASSESSMENT_GUIDE.md` |

## Layout

```
src/main/scala/econetcons/   model, runners and checks (current model only)
analysis/paper_sweep/        Python scripts for the paper sweep
analysis/figures/            figures produced by those scripts
data/paper_sweep/            paper sweep outputs (54,000 runs), population-level export, trophic-class sweep
data/emergent_design/        small diagnostic runs behind the parameter choices
docs/                        model documentation, hand-over guide, assessment report (LaTeX + PDF)
legacy/                      earlier model versions: runners, workflows, scripts, notes (see legacy/README.md)
```

Code map (`src/main/scala/econetcons/`):

| Files | Role |
|---|---|
| `World`, `WorldParameters`, `Species`, `Population`, `PopulationWebGenerator`, `Utils`, `SquareGrid` | metaweb, placement, spatial realisation of links, extinction phases |
| `EnergyFlow` | emergent energetics (assembly and cascades) |
| `ManagementLandscape`, `ManagementArea`, `ConservationStrategy`, `ConservationParameters`, `TileSelection`, `ProtectionStatus` | tiles, tile scores, protection plans |
| `Observables` | paired per-strategy outcomes on a shared world |
| `PaperSweep`, `ProtectedSetExport`, `TrophicLossSweep` | runners that produced `data/paper_sweep/` |
| `AssemblyDiagnostic`, `WebStructureSweep` (only `metrics` is current) | community diagnostics |
| `ModelChecks` | consistency checks of the model |

`World.scala` and the strategy code still contain the abundance models, cascade rules and strategies of
earlier versions behind parameter switches. The paper uses `abundanceModel = "emergent"` and the two
strategies above (`PaperSweep.worldParameters`, `Observables.paperStrategies`).

## Quick start

Requirements: JDK 17+, sbt 1.10, Python 3 with the packages in `analysis/requirements.txt`.

```bash
# consistency checks (about 3 minutes)
sbt -J-Xmx40g "runMain econetcons.ModelChecks"

# reproduce the summary tables from the stored sweep
python3 analysis/paper_sweep/summary.py

# a small sweep (32 runs, under a minute); its rows are identical to those in data/paper_sweep/paper_sweep.csv
sbt -J-Xmx40g "runMain econetcons.PaperSweep --out /tmp/mini.csv --seeds 2 --cMeta 0.05,0.2 \
     --tileSize 1,4 --fracSteps 2 --pExt 0.3,0.8 --threads 8"
```

The full paper sweep takes about 4.5 h on 20 threads; commands for every step are in
`analysis/README.md`. Run everything from the repository root.

## Conventions that are easy to get wrong

- Losses are **percentages of the community after stabilisation**; contrasts are **IR − SR in percentage
  points**, so negative means IR loses less.
- "IR" in current data and figures is the egocentric variant. In `legacy/` files `IR_*` is an older,
  discarded variant and `EIR_*` is today's IR.
- Tile size is given in median home-range **diameters**.
- There are no unit tests; `ModelChecks` and the `--verify` modes of the exporters are the safety net.
