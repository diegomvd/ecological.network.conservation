#!/usr/bin/env python3
"""Species loss vs fraction protected, by trophic-position class, SR vs IR (TrophicLossSweep).

Species class = biomass-weighted mean flow trophic position of its populations in the baseline web, counting plants
(herbivores = 2), rounded (5 = ≥ 4.5). Loss = % of the class's baseline species extinct at the end, pooled over seeds
(Σ lost / Σ baseline); bands = 95% bootstrap CI over seeds.

  loss_by_trophic_class_pext0.8.png       SR (solid) and IR (dashed) per class
  loss_by_trophic_class_diff_pext0.8.png  IR − SR per class (pp; < 0 = IR loses fewer), paired by seed

Usage: python3 analysis/paper_sweep/loss_by_trophic_class.py [path/to/trophic_loss.csv] [--pExt 0.8]
"""
import argparse
import numpy as np, pandas as pd, matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from paths import DATA, FIGS

ap = argparse.ArgumentParser()
ap.add_argument("src", nargs="?", default=DATA / "trophic_loss.csv")
ap.add_argument("--pExt", type=float, default=0.8)
ap.add_argument("--boot", type=int, default=500)
args = ap.parse_args()

d = pd.read_csv(args.src)
d = d[np.isclose(d.p_ext, args.pExt)]
cs, ts = sorted(d.c_meta.unique()), sorted(d.tile_size.unique())
classes = [2, 3, 4, 5]
names = {2: "TP 2 (herbivores)", 3: "TP ≈3", 4: "TP ≈4", 5: "TP ≥4.5"}
colors = {2: "#66a61e", 3: "#e6ab02", 4: "#d95f02", 5: "#7570b3"}
rng = np.random.default_rng(1)


def arrays(s, strat, k):
    """(frac values, seeds × frac arrays of lost and baseline counts)."""
    x = s[(s.strategy == strat) & (s.tp_class == k)]
    lost = x.pivot(index="seed", columns="frac", values="n_lost")
    base = x.pivot(index="seed", columns="frac", values="n_baseline")
    return lost.columns.values, lost.values.astype(float), base.values.astype(float)


def pooled(lost, base, idx=None):
    if idx is not None: lost, base = lost[idx], base[idx]
    with np.errstate(invalid="ignore", divide="ignore"):
        return 100 * lost.sum(0) / base.sum(0)


def boot(fn, nseeds):
    reps = np.array([fn(rng.integers(0, nseeds, nseeds)) for _ in range(args.boot)])
    return np.nanpercentile(reps, 2.5, axis=0), np.nanpercentile(reps, 97.5, axis=0)


def grid(title, ylabel):
    fig, axes = plt.subplots(len(cs), len(ts), figsize=(4.4 * len(ts), 3.5 * len(cs)), squeeze=False,
                             sharex=True, sharey=True, constrained_layout=True)
    fig.suptitle(title)
    for i, c in enumerate(cs):
        for j, t in enumerate(ts):
            ax = axes[i, j]
            ax.set_title(f"C = {c}, tile = {t:g} HR diameters", fontsize=9); ax.grid(alpha=0.3)
            if i == len(cs) - 1: ax.set_xlabel("fraction protected")
            if j == 0: ax.set_ylabel(ylabel)
    return fig, axes


nseeds = d.seed.nunique()
fig, axes = grid(f"Species lost by trophic-position class, pExt = {args.pExt:g} (solid = SR, dashed = IR; "
                 f"pooled over {nseeds} seeds, band = 95% CI)", "species of the class lost (%)")
fig2, axes2 = grid(f"IR − SR species loss by trophic-position class, pExt = {args.pExt:g} (pp; < 0 = IR loses fewer; "
                   f"band = 95% CI)", "Δ species lost, IR − SR (pp)")
for i, c in enumerate(cs):
    for j, t in enumerate(ts):
        s = d[(d.c_meta == c) & (d.tile_size == t)]
        ax, ax2 = axes[i, j], axes2[i, j]
        for k in classes:
            fr, lsr, bsr = arrays(s, "SR", k)
            _, lir, bir = arrays(s, "IR", k)
            n = bsr[:, 0].mean()
            lab = f"{names[k]} ({n:.0f} sp.)"
            for (l, b, ls) in [(lsr, bsr, "-"), (lir, bir, "--")]:
                ax.plot(fr, pooled(l, b), ls, color=colors[k], lw=1.8, label=lab if ls == "-" else None)
                lo, hi = boot(lambda idx: pooled(l, b, idx), len(l))
                ax.fill_between(fr, lo, hi, color=colors[k], alpha=0.12, lw=0)
            diff = lambda idx=None: pooled(lir, bir, idx) - pooled(lsr, bsr, idx)
            ax2.plot(fr, diff(), color=colors[k], lw=1.8, label=lab)
            lo, hi = boot(diff, len(lsr))
            ax2.fill_between(fr, lo, hi, color=colors[k], alpha=0.15, lw=0)
        ax2.axhline(0, color="k", lw=0.8)
        if j == 0:
            ax.legend(fontsize=7, frameon=False, title="SR solid · IR dashed", title_fontsize=7)
            ax2.legend(fontsize=7, frameon=False)
for f, name in [(fig, f"loss_by_trophic_class_pext{args.pExt:g}.png"), (fig2, f"loss_by_trophic_class_diff_pext{args.pExt:g}.png")]:
    f.savefig(FIGS / name, dpi=120); plt.close(f); print(FIGS / name)
