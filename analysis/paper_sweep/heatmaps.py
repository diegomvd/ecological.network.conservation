#!/usr/bin/env python3
"""Heatmaps of IR-SR contrasts (pp) over tile size x budget, per C, at each pExt; dots = |mean| <= 2 SE."""
import sys, numpy as np, pandas as pd, matplotlib; matplotlib.use("Agg"); import matplotlib.pyplot as plt
from paths import CELLSTATS, FIGS
c = pd.read_csv(sys.argv[1] if len(sys.argv) > 1 else CELLSTATS)
for p in sorted(c.p_ext.unique()):
    fig, ax = plt.subplots(2, 3, figsize=(15, 7), constrained_layout=True)
    for j, cm in enumerate(sorted(c.c_meta.unique())):
        s = c[(c.p_ext == p) & (c.c_meta == cm)]
        for i, (m, e, name, lim) in enumerate([("dSp", "dSp_se", "Δ species lost (IR−SR, pp)", 3), ("dI", "dI_se", "Δ interactions lost (IR−SR, pp)", 12)]):
            M = s.pivot(index="tile_size", columns="frac", values=m); E = s.pivot(index="tile_size", columns="frac", values=e)
            im = ax[i, j].imshow(M.values, origin="lower", aspect="auto", cmap="RdBu_r", vmin=-lim, vmax=lim)
            yy, xx = np.where(np.abs(M.values) <= 2 * E.values); ax[i, j].plot(xx, yy, "k.", ms=3)
            ax[i, j].set_yticks(range(len(M.index)), M.index); ax[i, j].set_xticks(range(0, len(M.columns), 4), [f"{f:.2f}" for f in M.columns[::4]])
            ax[i, j].set_title(f"C={cm}: {name}", fontsize=9); ax[i, j].set_xlabel("fraction protected"); ax[i, j].set_ylabel("tile size (HR diameters)")
            fig.colorbar(im, ax=ax[i, j], shrink=0.8)
    fig.suptitle(f"SR vs IR, emergent assembly, pExt={p} (blue = IR loses less)")
    fig.savefig(FIGS / f"contrast_heatmap_pext{p}.png", dpi=110); plt.close(fig)
