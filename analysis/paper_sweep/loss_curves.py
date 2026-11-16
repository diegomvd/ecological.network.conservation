#!/usr/bin/env python3
"""Fig. 1: % species / % interactions lost vs fraction protected, SR vs IR, at one pExt.

Grid: rows = metaweb connectance C, columns = tile size (median home-range diameters).
Thin lines = individual seeds (one assembled community each), thick line = mean, band = 95% CI of the mean.
Losses are % of the post-stabilization baseline (Observables: sp_ext_total / spint_ext_total).

Usage: python3 analysis/paper_sweep/loss_curves.py [--pExt 0.8] [--tiles 0.25,0.5,2]
"""
import argparse
import numpy as np, pandas as pd, matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from paths import SWEEP, FIGS, SR_IR_COLORS

ap = argparse.ArgumentParser()
ap.add_argument("--pExt", type=float, default=0.8)
ap.add_argument("--tiles", default="0.25,0.5,2")
args = ap.parse_args()
tiles = [float(t) for t in args.tiles.split(",")]

d = pd.read_csv(SWEEP)
d = d[np.isclose(d.p_ext, args.pExt) & d.tile_size.isin(tiles)]
cs = sorted(d.c_meta.unique())

for metric, name, fname in [("sp_ext_total", "species lost (%)", "species"),
                            ("spint_ext_total", "interactions lost (%)", "interactions")]:
    fig, axes = plt.subplots(len(cs), len(tiles), figsize=(4.2 * len(tiles), 3.4 * len(cs)),
                             sharex=True, sharey="row", constrained_layout=True)
    for i, c in enumerate(cs):
        for j, t in enumerate(tiles):
            ax = axes[i, j]
            s = d[(d.c_meta == c) & (d.tile_size == t)]
            for strat in ["SR", "IR"]:
                col = SR_IR_COLORS[strat]
                w = s.pivot(index="frac", columns="seed", values=f"{strat}_{metric}")
                ax.plot(w.index, w.values, color=col, lw=0.4, alpha=0.12)
                m, se = w.mean(axis=1), w.std(axis=1, ddof=1) / np.sqrt(w.shape[1])
                ax.fill_between(w.index, m - 1.96 * se, m + 1.96 * se, color=col, alpha=0.3, lw=0)
                ax.plot(w.index, m, color=col, lw=2, label=strat)
            tps = int(s.tiles_per_side.iloc[0])
            ax.set_title(f"C = {c}, tile = {t:g} HR diam. ({tps}×{tps} tiles)", fontsize=9)
            if i == len(cs) - 1: ax.set_xlabel("fraction protected")
            if j == 0: ax.set_ylabel(name)
            ax.grid(alpha=0.3)
    axes[0, 0].legend(frameon=False)
    fig.suptitle(f"{name[:-4].capitalize()}: SR vs IR, pExt = {args.pExt:g} "
                 f"(thin = seeds, thick = mean, band = 95% CI)")
    out = FIGS / f"loss_vs_budget_{fname}_pext{args.pExt:g}.png"
    fig.savefig(out, dpi=120); plt.close(fig)
    print(out)
