#!/usr/bin/env python3
"""
Round-2 basin analysis: SR vs IR vs EIR across the tile x fractionProtected x HR grid.

Per cell (tile, frac, hr) we have 20 seeds of the paired contrasts:
  dSp_IR_SR    = IR_sp_ext_total    - SR_sp_ext_total     (>0 => IR lost MORE species => SR wins)
  dSpInt_IR_SR = IR_spint_ext_total - SR_spint_ext_total
  dSp_EIR_SR, dSpInt_EIR_SR analogous for EIR

Outputs:
  basin_sweep_cellstats.csv  — per-cell mean, sd, se, n, win-fraction, t-stat, classification
  basin_phase_<contrast>.png — 3-panel (one per HR) filled phase diagram: SR basin / IR basin / tie
  basin_margin_<contrast>.png— 3-panel signed-magnitude heatmap (mean delta, diverging)
"""
import csv
import math
import sys
from collections import defaultdict

import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.colors import TwoSlopeNorm, ListedColormap, BoundaryNorm

CSV = sys.argv[1] if len(sys.argv) > 1 else "basin_sweep.csv"
# output prefix (default keeps round-2 names) and per-HR panel labels, low -> high HR
PREFIX = sys.argv[2] if len(sys.argv) > 2 else "basin"
HRLABELS = sys.argv[3].split(",") if len(sys.argv) > 3 else ["low mixing", "mid mixing", "high mixing"]
CONTRASTS = ["dSp_IR_SR", "dSpInt_IR_SR", "dSp_EIR_SR", "dSpInt_EIR_SR"]
# which strategy is the "challenger" in each contrast (the non-SR one)
CHALLENGER = {"dSp_IR_SR": "IR", "dSpInt_IR_SR": "IR", "dSp_EIR_SR": "EIR", "dSpInt_EIR_SR": "EIR"}

rows = list(csv.DictReader(open(CSV)))
for r in rows:
    for k in ("tile_idx", "frac_idx", "hr_idx", "tile", "nTiles", "seed"):
        r[k] = int(r[k])
    for k in ("frac", "hr", *CONTRASTS):
        r[k] = float(r[k])

tiles = sorted(set(r["tile"] for r in rows))
ntiles = {r["tile"]: r["nTiles"] for r in rows}
fracs = sorted(set(round(r["frac"], 4) for r in rows))
hrs = sorted(set(r["hr"] for r in rows))
ti_of = {t: i for i, t in enumerate(tiles)}
fi_of = {f: i for i, f in enumerate(fracs)}

# ---- per-cell aggregation ----
# key: (tile, frac, hr) -> {contrast: list of seed values}
cells = defaultdict(lambda: defaultdict(list))
for r in rows:
    key = (r["tile"], round(r["frac"], 4), r["hr"])
    for c in CONTRASTS:
        cells[key][c].append(r[c])

TIE_ABS = 0.5   # |mean delta| below this (species/interaction count) => call it a tie
SIG_T = 2.0     # |t| threshold for a "significant" cell classification

out = open(f"{PREFIX}_sweep_cellstats.csv", "w", newline="")
w = csv.writer(out)
w.writerow(["tile", "nTiles", "frac", "hr", "contrast", "challenger",
            "n", "mean", "sd", "se", "t", "win_frac_challenger", "class"])

# grids for plotting: [hr][contrast] -> 2D array (frac rows x tile cols)
mean_grid = {h: {c: np.full((len(fracs), len(tiles)), np.nan) for c in CONTRASTS} for h in hrs}
class_grid = {h: {c: np.full((len(fracs), len(tiles)), 0) for c in CONTRASTS} for h in hrs}
# class codes: -1 challenger wins (sig), 0 tie/ns, +1 SR wins (sig)

for (tile, frac, hr), d in cells.items():
    for c in CONTRASTS:
        v = np.array(d[c])
        n = len(v)
        mean = v.mean()
        sd = v.std(ddof=1)
        se = sd / math.sqrt(n) if n > 1 else float("nan")
        t = mean / se if se and se > 0 else 0.0
        win_challenger = float(np.mean(v < 0))  # challenger wins when delta<0
        if abs(mean) < TIE_ABS or abs(t) < SIG_T:
            cls = "tie"
            code = 0
        elif mean > 0:
            cls = "SR_wins"
            code = 1
        else:
            cls = CHALLENGER[c] + "_wins"
            code = -1
        w.writerow([tile, ntiles[tile], frac, hr, c, CHALLENGER[c],
                    n, f"{mean:.4f}", f"{sd:.4f}", f"{se:.4f}", f"{t:.2f}",
                    f"{win_challenger:.3f}", cls])
        fi, tix = fi_of[round(frac, 4)], ti_of[tile]
        mean_grid[hr][c][fi, tix] = mean
        class_grid[hr][c][fi, tix] = code
out.close()
print(f"wrote {PREFIX}_sweep_cellstats.csv")

# ---- plotting helpers ----
def tile_ticklabels():
    # show a subset for readability
    labs = []
    for i, t in enumerate(tiles):
        labs.append(f"{t}\n({ntiles[t]})" if i % 3 == 0 else "")
    return labs

def frac_ticklabels():
    return [f"{f:.2f}" if i % 3 == 0 else "" for i, f in enumerate(fracs)]

HRNAME = dict(zip(hrs, HRLABELS))

# ---- phase diagrams (classification) ----
phase_cmap = ListedColormap(["#2166ac", "#f7f7f7", "#b2182b"])  # challenger / tie / SR
phase_norm = BoundaryNorm([-1.5, -0.5, 0.5, 1.5], phase_cmap.N)

for c in CONTRASTS:
    fig, axes = plt.subplots(1, 3, figsize=(15, 5.2), constrained_layout=True)
    for ax, hr in zip(axes, hrs):
        g = class_grid[hr][c]
        im = ax.imshow(g, origin="lower", aspect="auto", cmap=phase_cmap, norm=phase_norm)
        ax.set_title(f"{HRNAME[hr]}  (HR={hr})")
        ax.set_xticks(range(len(tiles))); ax.set_xticklabels(tile_ticklabels(), fontsize=7)
        ax.set_yticks(range(len(fracs))); ax.set_yticklabels(frac_ticklabels(), fontsize=7)
        ax.set_xlabel("avg pops / tile  (n management tiles)")
    axes[0].set_ylabel("fraction protected")
    ch = CHALLENGER[c]
    fig.suptitle(f"{c}:  blue = {ch} wins,  white = tie/ns,  red = SR wins   "
                 f"(|t|>{SIG_T}, |Δ|>{TIE_ABS})", fontsize=12)
    fig.savefig(f"{PREFIX}_phase_{c}.png", dpi=130)
    plt.close(fig)
    print(f"wrote {PREFIX}_phase_{c}.png")

# ---- margin heatmaps (signed magnitude) ----
for c in CONTRASTS:
    allv = np.concatenate([mean_grid[h][c].ravel() for h in hrs])
    vmax = np.nanpercentile(np.abs(allv), 98)
    norm = TwoSlopeNorm(vmin=-vmax, vcenter=0.0, vmax=vmax)
    fig, axes = plt.subplots(1, 3, figsize=(15, 5.2), constrained_layout=True)
    for ax, hr in zip(axes, hrs):
        im = ax.imshow(mean_grid[hr][c], origin="lower", aspect="auto",
                       cmap="RdBu_r", norm=norm)
        ax.set_title(f"{HRNAME[hr]}  (HR={hr})")
        ax.set_xticks(range(len(tiles))); ax.set_xticklabels(tile_ticklabels(), fontsize=7)
        ax.set_yticks(range(len(fracs))); ax.set_yticklabels(frac_ticklabels(), fontsize=7)
        ax.set_xlabel("avg pops / tile  (n management tiles)")
    axes[0].set_ylabel("fraction protected")
    fig.colorbar(im, ax=axes, shrink=0.8, label=f"mean {c}  (>0: SR better)")
    ch = CHALLENGER[c]
    fig.suptitle(f"{c}: mean Δ per cell ({len(rows)//(len(cells)) if cells else 0} seeds).  red = SR better, blue = {ch} better", fontsize=12)
    fig.savefig(f"{PREFIX}_margin_{c}.png", dpi=130)
    plt.close(fig)
    print(f"wrote {PREFIX}_margin_{c}.png")

# ---- text summary ----
print("\n=== BASIN SUMMARY ===")
for c in CONTRASTS:
    ch = CHALLENGER[c]
    print(f"\n{c}  (challenger = {ch}; Δ>0 => SR wins)")
    for hr in hrs:
        codes = class_grid[hr][c].ravel()
        means = mean_grid[hr][c].ravel()
        n = len(codes)
        sr = np.mean(codes == 1); chw = np.mean(codes == -1); tie = np.mean(codes == 0)
        print(f"  {HRNAME[hr]:12s} HR={hr}:  SR {sr:4.0%} | {ch} {chw:4.0%} | tie {tie:4.0%}   "
              f"mean Δ over grid = {np.nanmean(means):+.2f}")
    # where does the challenger win? describe the subregion
    win_cells = [(t, f, h) for (t, f, h), d in cells.items()
                 if np.mean(np.array(d[c]) < 0) > 0.5]
    if win_cells:
        wt = [w[0] for w in win_cells]; wf = [w[1] for w in win_cells]; wh = [w[2] for w in win_cells]
        print(f"  {ch} majority-wins in {len(win_cells)}/{len(cells)} cells; "
              f"there: tile {min(wt)}-{max(wt)} (median {int(np.median(wt))}), "
              f"frac {min(wf):.2f}-{max(wf):.2f} (median {np.median(wf):.2f}), "
              f"HR {sorted(set(wh))}")
