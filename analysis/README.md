# Analysis

Layout (started 2026-10-15 with the paper sweep; older analyses still live in the repo root, see below):

```
data/<study>/                 raw simulation outputs (CSV, logs) written by the Scala runners
analysis/<study>/             Python scripts for that study (run from anywhere; paths in paths.py)
analysis/figures/<study>/     figures produced by those scripts
```

## paper_sweep — SR vs IR on the emergent assembly (paper model)

Model and parameter set: `MODEL_PARAMETERS.md` (paper configuration), `biomass_pyramid_plan.md` (design and results).
Outcomes are % of the post-stabilization baseline; contrasts IR − SR are percentage points.

| Step | Command | Output |
|---|---|---|
| Sweep (54k runs, ~4.5 h on 20 threads) | `sbt -J-Xmx40g "runMain econetcons.PaperSweep --seeds 50 --tileSize 0.25,0.5,1,2,4,8 --threads 20"` | `data/paper_sweep/paper_sweep.csv` (resumable) |
| Population-level export (~10 min) | `sbt -J-Xmx40g "runMain econetcons.ProtectedSetExport --seeds 50 --threads 20 --verify true"` | `data/paper_sweep/protected_sets.csv.gz` |
| Species loss by trophic-position class (~40 min) | `sbt -J-Xmx40g "runMain econetcons.TrophicLossSweep --seeds 50 --threads 20 --verifySeeds 2"` | `data/paper_sweep/trophic_loss.csv` |
| Summary tables | `python3 analysis/paper_sweep/summary.py` | stdout (saved as `data/paper_sweep/paper_sweep_summary.txt`), `data/paper_sweep/paper_sweep_cellstats.csv` |
| Contrast heatmaps (needs cellstats) | `python3 analysis/paper_sweep/heatmaps.py` | `contrast_heatmap_pext*.png` |
| Loss vs budget, SR vs IR | `python3 analysis/paper_sweep/loss_curves.py [--pExt 0.8] [--tiles 0.25,0.5,2]` | `loss_vs_budget_{species,interactions}_pext0.8.png` |
| Jaccard dissimilarity of protected tiles | `python3 analysis/paper_sweep/jaccard.py` | `jaccard_dissimilarity_protected_tiles.png` |
| Species loss vs budget by trophic class | `python3 analysis/paper_sweep/loss_by_trophic_class.py` | `loss_by_trophic_class{,_diff}_pext0.8.png` |
| Protected body masses / trophic positions, final biomass | `python3 analysis/paper_sweep/protected_sets.py` | `protected_body_mass_{populations,species}.png`, `protected_trophic_position.png`, `final_biomass_by_trophic_position.png`, `final_biomass_share_by_trophic_position.png` |

`ProtectedSetExport` replays exactly the sweep pipeline (same RNG streams); `--verify true` checks its species and
interaction losses against `Observables.runWith` for every case. Default case: C ∈ {0.05, 0.1, 0.2}, tile size
∈ {0.25, 2}, budget 0.15, pExt 0.8.

## Older analyses (not yet moved)

Earlier sweeps (basin sweeps, web-structure, assembly diagnostics, Saltelli) keep their CSVs, `analyze_*.py` scripts
and PNGs in the repo root. They were produced by earlier model versions; move them into `data/<study>` /
`analysis/<study>` when they are needed again.
