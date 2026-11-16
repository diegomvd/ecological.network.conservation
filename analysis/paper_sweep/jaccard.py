#!/usr/bin/env python3
"""Fig. 2: Jaccard dissimilarity (1 − |A∩B|/|A∪B|) of the SR and IR protected TILE sets, over tile size × budget,
one panel per connectance C (mean over seeds). Grey = budget below one tile (nothing protected). The plans do not depend on pExt; one pExt level is used so
each seed counts once.

Usage: python3 analysis/paper_sweep/jaccard.py [--pExt 0.8]
"""
import argparse
import numpy as np, pandas as pd, matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from paths import SWEEP, FIGS

ap = argparse.ArgumentParser()
ap.add_argument("--pExt", type=float, default=0.8)
args = ap.parse_args()

d = pd.read_csv(SWEEP)
d = d[np.isclose(d.p_ext, args.pExt)].copy()
d["jdis"] = 1.0 - d.pair_SR_IR_jaccard_protected
# Budgets below one tile protect nothing (both sets empty, J reported as 1): mask instead of plotting 0.
d.loc[np.floor(d.frac * d.tiles_per_side ** 2 + 1e-9) < 1, "jdis"] = np.nan
cs = sorted(d.c_meta.unique())

fig, axes = plt.subplots(1, len(cs), figsize=(5.2 * len(cs), 4.2), constrained_layout=True)
for ax, c in zip(axes, cs):
    M = d[d.c_meta == c].groupby(["tile_size", "frac"]).jdis.mean().unstack("frac")
    cmap = matplotlib.colormaps["viridis"].copy(); cmap.set_bad("lightgrey")
    im = ax.imshow(np.ma.masked_invalid(M.values), origin="lower", aspect="auto", cmap=cmap, vmin=0, vmax=1)
    for (yi, xi), v in np.ndenumerate(M.values):
        if xi % 3 == 0 and np.isfinite(v):
            ax.text(xi, yi, f"{v:.2f}", ha="center", va="center", fontsize=6.5, color="w" if v < 0.6 else "k")
    tps = d[d.c_meta == c].groupby("tile_size").tiles_per_side.first()
    ax.set_yticks(range(len(M.index)), [f"{t:g} ({tps[t]}²)" for t in M.index])
    ax.set_xticks(range(0, len(M.columns), 3), [f"{f:.2f}" for f in M.columns[::3]])
    ax.set_xlabel("fraction protected"); ax.set_ylabel("tile size, HR diameters (tiles)")
    ax.set_title(f"C = {c}")
fig.colorbar(im, ax=axes, shrink=0.85, label="Jaccard dissimilarity of protected tiles (SR vs IR)")
out = FIGS / "jaccard_dissimilarity_protected_tiles.png"
fig.savefig(out, dpi=120); plt.close(fig)
print(out)
