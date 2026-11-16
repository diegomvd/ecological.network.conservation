#!/usr/bin/env python3
"""
Delta-effectiveness maps from the v2 basin sweep: one figure per web density.

Each figure is 2x2 heatmaps over the scenario space (avg populations per tile x fraction protected):
  rows    = challenger strategy vs SR  (IR - SR, EIR - SR)
  columns = species extinction (%), interaction extinction (%)
Cell value = mean over seeds of (challenger extinction % - SR extinction %), in percentage points.
  > 0 (red)  : the challenger loses MORE  -> SR more effective
  < 0 (blue) : the challenger loses FEWER -> challenger more effective
Dotted cells: difference NOT significant (|t|<=2 or |mean|<=0.5).
Colour scales are shared across the three web figures for each panel position, so figures compare.

Usage: python3 plot_delta_maps_v2.py [basin_sweep_v2.csv] [delta_map_v2]
"""
import csv
import math
import sys
from collections import defaultdict

import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.colors import LinearSegmentedColormap, TwoSlopeNorm

CSV = sys.argv[1] if len(sys.argv) > 1 else "basin_sweep_v2.csv"
OUT = sys.argv[2] if len(sys.argv) > 2 else "delta_map_v2"
WEB = {"0.0160": "sparse", "0.0200": "medium", "0.0250": "dense"}
PANELS = [  # (contrast column, row label, column label)
    ("dSp_IR_SR", "IR − SR", "species extinction"),
    ("dSpInt_IR_SR", "IR − SR", "interaction extinction"),
    ("dSp_EIR_SR", "EIR − SR", "species extinction"),
    ("dSpInt_EIR_SR", "EIR − SR", "interaction extinction"),
]
SIG_T, MIN_ABS = 2.0, 0.5

SURFACE, INK, INK2, GRID = "#fcfcfb", "#0b0b0b", "#52514e", "#e4e3df"
# diverging: blue (challenger better) <- neutral gray -> red (SR better)
DIV = LinearSegmentedColormap.from_list("bluered", [
    "#0d366b", "#1c5cab", "#3987e5", "#86b6ef", "#f0efec", "#f2a3a2", "#e34948", "#b52f2e", "#6e1716"])

vals = defaultdict(list)
for r in csv.DictReader(open(CSV)):
    key = (r["hr"], int(r["tile"]), round(float(r["frac"]), 4))
    for c, _, _ in PANELS:
        vals[(c,) + key].append(float(r[c]))
tiles = sorted({k[2] for k in vals})
fracs = sorted({k[3] for k in vals})
n_seeds = len(next(iter(vals.values())))
ntiles = {t: 5000 // t for t in tiles}


def grids(contrast, hr):
    mean = np.full((len(fracs), len(tiles)), np.nan)
    sig = np.zeros_like(mean)
    for i, f in enumerate(fracs):
        for j, t in enumerate(tiles):
            v = np.asarray(vals[(contrast, hr, t, f)])
            m = v.mean()
            se = v.std(ddof=1) / math.sqrt(len(v))
            tt = m / se if se > 0 else 0.0
            mean[i, j] = m
            sig[i, j] = 1.0 if (abs(tt) > SIG_T and abs(m) > MIN_ABS) else 0.0
    return mean, sig


G = {(c, hr): grids(c, hr) for c, _, _ in PANELS for hr in WEB}
# shared symmetric limit per panel position across webs (98th pct of |delta|, robust to outliers)
LIM = {c: max(0.5, np.nanpercentile(np.abs(np.concatenate([G[(c, hr)][0].ravel() for hr in WEB])), 98))
       for c, _, _ in PANELS}

plt.rcParams.update({"font.size": 10, "axes.edgecolor": GRID, "axes.labelcolor": INK2,
                     "xtick.color": INK2, "ytick.color": INK2, "text.color": INK})
xt = [j for j, t in enumerate(tiles) if t in (10, 16, 26, 41, 66, 106, 200)]
yt = [i for i, f in enumerate(fracs) if i % 3 == 0 or i == len(fracs) - 1]

for hr, web in WEB.items():
    fig, axes = plt.subplots(2, 2, figsize=(12.5, 9.6), facecolor=SURFACE, constrained_layout=True)
    for ax, (c, rowlab, collab) in zip(axes.ravel(), PANELS):
        mean, sig = G[(c, hr)]
        lim = LIM[c]
        im = ax.imshow(mean, origin="lower", aspect="auto", cmap=DIV,
                       norm=TwoSlopeNorm(vmin=-lim, vcenter=0.0, vmax=lim), interpolation="nearest")
        ns_i, ns_j = np.where(sig == 0)   # stipple non-significant scenarios
        ax.scatter(ns_j, ns_i, s=4, color=INK2, alpha=0.55, linewidths=0, zorder=3)
        ax.set_xlim(-0.5, len(tiles) - 0.5)
        ax.set_ylim(-0.5, len(fracs) - 0.5)
        ax.set_title(f"{rowlab}: {collab}", fontsize=11.5, color=INK, fontweight="bold")
        ax.set_xticks(xt)
        ax.set_xticklabels([f"{tiles[j]}\n({ntiles[tiles[j]]})" for j in xt], fontsize=8.5)
        ax.set_yticks(yt)
        ax.set_yticklabels([f"{fracs[i]:.2f}" for i in yt], fontsize=8.5)
        ax.set_xlabel("avg populations per tile  (number of management tiles)")
        ax.set_ylabel("fraction protected")
        for s in ax.spines.values():
            s.set_visible(False)
        cb = fig.colorbar(im, ax=ax, shrink=0.9, pad=0.02)
        cb.set_label("Δ extinction (pp)", color=INK2)
        cb.outline.set_visible(False)
    fig.suptitle(f"{web} web: change in % extinction vs species-richness targeting (SR), mean of {n_seeds} seeds\n"
                 f"red = strategy loses more than SR (SR more effective) · blue = strategy loses fewer · "
                 f"dotted = no significant difference (|t|≤2 or |Δ|≤0.5)", fontsize=11.5, color=INK)
    out = f"{OUT}_{web}.png"
    fig.savefig(out, dpi=150, facecolor=SURFACE)
    plt.close(fig)
    print(f"wrote {out}")

for c, rl, cl in PANELS:
    print(f"  colour limit {rl:8s} {cl:23s}: ±{LIM[c]:.1f} pp")
