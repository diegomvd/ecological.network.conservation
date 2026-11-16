#!/usr/bin/env python3
"""
Analysis of web_structure_sweep.csv (world assembly + stabilization only, no protection).

1. Main-effect shares: fraction of variance in each stable-web metric explained by each factor
   (nSpecies, epsilon, hr, c_meta) on its own; remainder = interactions.
2. Log-log elasticities: log(metric) ~ log(nSpecies) + log(eps) + log(hr) + log(c_meta), i.e. a
   power-law "knob formula" metric ~ nS^a eps^b hr^c C^d, with R^2.
3. Iso-structure scenarios: combinations giving a target links-per-species with species count in
   a fixed band — candidates for a clean connectance axis.
4. Figure: stable links/species and species richness over (hr x eps), rows = metaweb connectance,
   at a reference nSpecies.

Usage: python3 analyze_web_structure.py [web_structure_sweep.csv] [ref_nSpecies]
"""
import csv
import sys
from collections import defaultdict

import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

CSV = sys.argv[1] if len(sys.argv) > 1 else "web_structure_sweep.csv"
REF_NS = int(sys.argv[2]) if len(sys.argv) > 2 else 300
FACTORS = ["nSpecies", "epsilon", "hr", "c_meta"]
METRICS = ["stable_S", "stable_L_per_S", "stable_connectance", "stable_diet_breadth",
           "stable_frac_single_prey", "stable_prey_per_consumer_node", "stable_pred_per_prey_node",
           "stable_pop_nodes"]

rows = list(csv.DictReader(open(CSV)))
for r in rows:
    for k in list(r):
        r[k] = float(r[k])

# ---- cell means over seeds
cell = defaultdict(lambda: defaultdict(list))
for r in rows:
    key = tuple(r[f] for f in FACTORS)
    for m in METRICS + ["raw_S", "raw_pop_nodes", "raw_L_per_S"]:
        cell[key][m].append(r[m])
keys = sorted(cell)
M = {m: np.array([np.mean(cell[k][m]) for k in keys]) for m in METRICS + ["raw_S", "raw_pop_nodes", "raw_L_per_S"]}
X = {f: np.array([k[i] for k in keys]) for i, f in enumerate(FACTORS)}
print(f"{len(rows)} worlds, {len(keys)} parameter combinations\n")

# ---- 1. main-effect shares
print("=== 1. share of variance explained by each factor alone (main effects); rest = interactions ===")
print(f"{'metric':32s}" + "".join(f"{f:>10s}" for f in FACTORS) + f"{'interact':>10s}")
for m in METRICS:
    y = M[m]
    tot = y.var()
    shares = []
    for f in FACTORS:
        lv = np.unique(X[f])
        means = np.array([y[X[f] == v].mean() for v in lv])
        counts = np.array([(X[f] == v).sum() for v in lv])
        shares.append((counts * (means - y.mean()) ** 2).sum() / len(y) / tot if tot > 0 else 0)
    print(f"{m:32s}" + "".join(f"{s:10.2f}" for s in shares) + f"{1 - sum(shares):10.2f}")

# ---- 2. log-log elasticities
print("\n=== 2. power-law fit  metric ~ nS^a * eps^b * hr^c * C^d   (exponents, R^2) ===")
A = np.column_stack([np.ones(len(keys))] + [np.log(X[f]) for f in FACTORS])
print(f"{'metric':32s}" + "".join(f"{f:>10s}" for f in FACTORS) + f"{'R2':>8s}")
for m in METRICS:
    y = M[m]
    ok = y > 0
    coef, *_ = np.linalg.lstsq(A[ok], np.log(y[ok]), rcond=None)
    pred = A[ok] @ coef
    r2 = 1 - ((np.log(y[ok]) - pred) ** 2).sum() / ((np.log(y[ok]) - np.log(y[ok]).mean()) ** 2).sum()
    print(f"{m:32s}" + "".join(f"{c:10.2f}" for c in coef[1:]) + f"{r2:8.2f}")

# ---- energetic filter strength
print("\n=== stabilization filter: fraction of raw species / population nodes removed (mean by factor level) ===")
fs = 1 - M["stable_S"] / M["raw_S"]
fn = 1 - M["stable_pop_nodes"] / M["raw_pop_nodes"]
for f in FACTORS:
    lv = np.unique(X[f])
    print(f"  {f:9s}: " + "  ".join(f"{v:g}: sp {fs[X[f]==v].mean():.2f} / nodes {fn[X[f]==v].mean():.2f}" for v in lv))

# ---- 3. iso-structure candidates
print(f"\n=== 3. iso-structure candidates: stable species 100-160, grouped by links/species band ===")
bands = [(1.5, 2.0), (2.0, 2.5), (2.5, 3.0), (3.0, 3.5), (3.5, 4.0), (4.0, 5.0), (5.0, 6.5)]
S, LS = M["stable_S"], M["stable_L_per_S"]
for lo, hi in bands:
    idx = [i for i in range(len(keys)) if 100 <= S[i] <= 160 and lo <= LS[i] < hi]
    print(f"  L/S {lo}-{hi}: {len(idx)} combos", end="")
    if idx:
        # show up to 5 combos closest to S=130, preferring fixed nSpecies=REF_NS
        idx.sort(key=lambda i: (X["nSpecies"][i] != REF_NS, abs(S[i] - 130)))
        print(";  e.g. " + "; ".join(
            f"nS={int(X['nSpecies'][i])} eps={X['epsilon'][i]:g} hr={X['hr'][i]:g} C={X['c_meta'][i]:g} "
            f"(S={S[i]:.0f}, L/S={LS[i]:.2f}, diet={M['stable_diet_breadth'][i]:.1f}, "
            f"single-prey={M['stable_frac_single_prey'][i]:.2f})" for i in idx[:4]))
    else:
        print()

# ---- 4. figure
SURFACE, INK, INK2 = "#fcfcfb", "#0b0b0b", "#52514e"
eps_lv = np.unique(X["epsilon"]); hr_lv = np.unique(X["hr"]); c_lv = np.unique(X["c_meta"])
for metric, label, fname in [("stable_L_per_S", "links per species (stable web)", "web_structure_LperS.png"),
                             ("stable_S", "species surviving stabilization", "web_structure_S.png"),
                             ("stable_frac_single_prey", "fraction of consumers with one prey species",
                              "web_structure_singleprey.png")]:
    fig, axes = plt.subplots(1, len(c_lv), figsize=(4.2 * len(c_lv), 4.2), sharey=True,
                             facecolor=SURFACE, constrained_layout=True)
    allv = M[metric][X["nSpecies"] == REF_NS]
    vmin, vmax = np.nanmin(allv), np.nanmax(allv)
    for ax, c in zip(axes, c_lv):
        g = np.full((len(eps_lv), len(hr_lv)), np.nan)
        for i, e in enumerate(eps_lv):
            for j, h in enumerate(hr_lv):
                sel = (X["nSpecies"] == REF_NS) & (X["epsilon"] == e) & (X["hr"] == h) & (X["c_meta"] == c)
                g[i, j] = M[metric][sel][0]
        im = ax.imshow(g, origin="lower", aspect="auto", cmap="viridis", vmin=vmin, vmax=vmax)
        for i in range(len(eps_lv)):
            for j in range(len(hr_lv)):
                v = g[i, j]
                ax.text(j, i, f"{v:.2f}" if v < 10 else f"{v:.0f}", ha="center", va="center", fontsize=7.5,
                        color="white" if (v - vmin) / (vmax - vmin + 1e-12) < 0.6 else INK)
        ax.set_xticks(range(len(hr_lv))); ax.set_xticklabels([f"{h:g}" for h in hr_lv], fontsize=8)
        ax.set_yticks(range(len(eps_lv))); ax.set_yticklabels([f"{e:g}" for e in eps_lv], fontsize=8)
        ax.set_xlabel("median home range")
        ax.set_title(f"metaweb connectance {c:g}", fontsize=10.5, color=INK)
    axes[0].set_ylabel("epsilon")
    cb = fig.colorbar(im, ax=axes, shrink=0.9); cb.outline.set_visible(False)
    fig.suptitle(f"{label}  (nSpecies = {REF_NS}, mean of seeds; before any protection)", color=INK)
    fig.savefig(fname, dpi=150, facecolor=SURFACE)
    plt.close(fig)
    print(f"wrote {fname}")
