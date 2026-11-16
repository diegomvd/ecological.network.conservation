#!/usr/bin/env python3
"""
EIR vs SR trade-off figures from the v2 basin sweep.

One point per scenario (a tile-size x fraction-protected combination of the sweep grid, NOT a
management tile), mean over seeds:
  x = species cost of EIR       = dSp_EIR_SR     (pp of baseline species; >0 => EIR loses more species)
  y = interaction gain of EIR   = -dSpInt_EIR_SR (pp of baseline interactions; >0 => EIR keeps more)

Columns = web density (sparse / medium / dense). Quadrant labels give the share of scenarios whose
two contrasts are significant (|t|>2 and |mean|>0.5) in that direction.

Outputs:
  <out>_single.png  one row: colour = avg populations per tile, dot size = fraction protected
  <out>.png         two rows: colour = tile size (top), colour = fraction protected (bottom)

Usage: python3 plot_tradeoff_v2.py [basin_sweep_v2.csv] [tradeoff_EIR_SR_v2]
"""
import csv
import math
import sys
from collections import defaultdict

import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.colors import LinearSegmentedColormap, ListedColormap, LogNorm, Normalize
from matplotlib.lines import Line2D

CSV = sys.argv[1] if len(sys.argv) > 1 else "basin_sweep_v2.csv"
OUT = sys.argv[2] if len(sys.argv) > 2 else "tradeoff_EIR_SR_v2"
WEB = {"0.0160": "sparse web", "0.0200": "medium web", "0.0250": "dense web"}
SIG_T, MIN_ABS = 2.0, 0.5

# reference palette (dataviz skill): surface, ink, sequential ramps
SURFACE, INK, INK2, GRID, TINT = "#fcfcfb", "#0b0b0b", "#52514e", "#e4e3df", "#eef4fc"
BLUES = LinearSegmentedColormap.from_list("blues", ["#cde2fb", "#86b6ef", "#3987e5", "#1c5cab", "#0d366b"])
ORANGES = LinearSegmentedColormap.from_list("oranges", ["#fbd9c9", "#f3a07c", "#eb6834", "#b84a1d", "#7a2e10"])
# perceptually uniform, high contrast: viridis without its palest yellow tail (keeps every step
# visible against the light surface)
VIRIDIS = ListedColormap(plt.get_cmap("viridis")(np.linspace(0.0, 0.9, 256)), name="viridis_trunc")

# ---------------------------------------------------------------- data
cells = defaultdict(lambda: {"sp": [], "int": []})
for r in csv.DictReader(open(CSV)):
    k = (r["hr"], int(r["tile"]), float(r["frac"]))
    cells[k]["sp"].append(float(r["dSp_EIR_SR"]))
    cells[k]["int"].append(float(r["dSpInt_EIR_SR"]))
n_seeds = len(next(iter(cells.values()))["sp"])


def sign(v):
    v = np.asarray(v)
    m = v.mean()
    se = v.std(ddof=1) / math.sqrt(len(v))
    t = m / se if se > 0 else 0.0
    return 1 if (t > SIG_T and m > MIN_ABS) else (-1 if (t < -SIG_T and m < -MIN_ABS) else 0)


data = {}
for hr in WEB:
    ks = sorted(k for k in cells if k[0] == hr)
    q = defaultdict(int)
    for k in ks:
        s_sp, s_int = sign(cells[k]["sp"]), sign(cells[k]["int"])
        if s_int == -1 and s_sp <= 0:
            q["win"] += 1        # EIR better on interactions, no species cost
        elif s_int == -1 and s_sp == 1:
            q["trade"] += 1      # EIR better on interactions, worse on species
        elif s_int >= 0 and s_sp == 1:
            q["lose"] += 1       # SR better on species, no significant EIR interaction gain
        else:
            q["tie"] += 1
    data[hr] = dict(
        x=np.array([np.mean(cells[k]["sp"]) for k in ks]),
        y=np.array([-np.mean(cells[k]["int"]) for k in ks]),
        tile=np.array([k[1] for k in ks]),
        frac=np.array([k[2] for k in ks]),
        q={k: v / len(ks) for k, v in q.items()},
        n=len(ks),
    )

allx = np.concatenate([d["x"] for d in data.values()])
ally = np.concatenate([d["y"] for d in data.values()])
PAD = 0.6
XLIM = (min(allx.min(), -1) - PAD, allx.max() + PAD)
YLIM = (min(ally.min(), -1) - PAD, ally.max() + PAD)

plt.rcParams.update({"font.size": 10, "axes.edgecolor": GRID, "axes.labelcolor": INK2,
                     "xtick.color": INK2, "ytick.color": INK2, "text.color": INK})
YLABEL = "EIR interaction gain over SR\n(pp of baseline interactions)"
XLABEL = "EIR species cost vs SR (pp of baseline species)"


def frame(ax, hr, quadrant_labels=True):
    ax.set_facecolor(SURFACE)
    ax.fill_between([XLIM[0], 0], 0, YLIM[1], color=TINT, zorder=0, lw=0)
    ax.axhline(0, color=INK2, lw=0.8, zorder=1)
    ax.axvline(0, color=INK2, lw=0.8, zorder=1)
    ax.grid(color=GRID, lw=0.5, zorder=0)
    ax.set_xlim(XLIM)
    ax.set_ylim(YLIM)
    for side in ("top", "right"):
        ax.spines[side].set_visible(False)
    if quadrant_labels:
        q = data[hr]["q"]
        ax.text(0.5, 1.01, f"EIR better on both {q.get('win', 0):.0%}  ·  trade-off {q.get('trade', 0):.0%}  ·  "
                f"SR better on species {q.get('lose', 0):.0%}", transform=ax.transAxes,
                ha="center", va="bottom", fontsize=9, color=INK2)


def title(fig):
    n = data[next(iter(WEB))]["n"]
    fig.suptitle(f"EIR vs SR: interactions saved vs species lost  (each dot = one management scenario, "
                 f"mean of {n_seeds} seeds; {n} scenarios per web)\nshaded = EIR keeps more interactions without losing species; shares count significant differences (|t|>2, |Δ|>0.5), the rest are ties",
                 fontsize=11.5, color=INK)


# ---------------------------------------------------------------- single row
S_MIN, S_MAX, F_MIN, F_MAX = 10.0, 140.0, 0.05, 0.40
def size_of(f):
    return S_MIN + (S_MAX - S_MIN) * (f - F_MIN) / (F_MAX - F_MIN)   # area linear in budget

fig, axes = plt.subplots(1, 3, figsize=(15.5, 6.2), sharex=True, sharey=True,
                         facecolor=SURFACE, constrained_layout=True)
tnorm = LogNorm(vmin=10, vmax=200)
for ax, hr in zip(axes, WEB):
    d = data[hr]
    frame(ax, hr)
    order = np.argsort(-d["frac"])          # big dots first, small dots on top
    sc = ax.scatter(d["x"][order], d["y"][order], c=d["tile"][order], cmap=VIRIDIS, norm=tnorm,
                    s=size_of(d["frac"][order]), edgecolors=SURFACE, linewidths=0.6, alpha=0.9, zorder=2)
    ax.set_title(WEB[hr], fontsize=12, color=INK, fontweight="bold", pad=20)
    ax.set_xlabel(XLABEL)
axes[0].set_ylabel(YLABEL)
cb = fig.colorbar(sc, ax=axes, shrink=0.9, pad=0.01)
cb.set_label("avg populations per tile (coarser planning →)", color=INK2)
cb.set_ticks([10, 20, 50, 100, 200]); cb.set_ticklabels(["10", "20", "50", "100", "200"])
cb.outline.set_visible(False)
handles = [Line2D([], [], ls="", marker="o", markersize=math.sqrt(size_of(f)), markerfacecolor="#9a9994",
                  markeredgecolor=SURFACE, label=f"{f:.2f}") for f in (0.05, 0.15, 0.25, 0.40)]
fig.legend(handles=handles, title="dot size = fraction protected", loc="outside lower center", ncol=4,
           frameon=False, fontsize=9, title_fontsize=9, labelcolor=INK2, columnspacing=2.5)
title(fig)
fig.savefig(f"{OUT}_single.png", dpi=150, facecolor=SURFACE)
plt.close(fig)
print(f"wrote {OUT}_single.png")

# ---------------------------------------------------------------- two rows
fig, axes = plt.subplots(2, 3, figsize=(14, 8.6), sharex=True, sharey=True,
                         facecolor=SURFACE, constrained_layout=True)
rows = [
    ("tile", BLUES, LogNorm(vmin=10, vmax=200), "avg populations per tile (coarser →)"),
    ("frac", ORANGES, Normalize(vmin=0.05, vmax=0.40), "fraction protected"),
]
for ri, (var, cmap, norm, clabel) in enumerate(rows):
    for ci, hr in enumerate(WEB):
        ax = axes[ri, ci]
        d = data[hr]
        frame(ax, hr, quadrant_labels=(ri == 0))
        order = np.argsort(d[var])
        sc = ax.scatter(d["x"][order], d["y"][order], c=d[var][order], cmap=cmap, norm=norm,
                        s=30, edgecolors=SURFACE, linewidths=0.6, zorder=2)
        if ri == 0:
            ax.set_title(WEB[hr], fontsize=12, color=INK, fontweight="bold", pad=20)
        if ci == 0:
            ax.set_ylabel(YLABEL)
        if ri == 1:
            ax.set_xlabel(XLABEL)
    cb = fig.colorbar(sc, ax=axes[ri, :], shrink=0.85, pad=0.01)
    cb.set_label(clabel, color=INK2)
    cb.outline.set_visible(False)
    if var == "tile":
        cb.set_ticks([10, 20, 50, 100, 200]); cb.set_ticklabels(["10", "20", "50", "100", "200"])
title(fig)
fig.savefig(f"{OUT}.png", dpi=150, facecolor=SURFACE)
plt.close(fig)
print(f"wrote {OUT}.png")

for hr in WEB:
    q = data[hr]["q"]
    print(f"  {WEB[hr]:11s}: win {q.get('win',0):.0%}  trade {q.get('trade',0):.0%}  "
          f"SR {q.get('lose',0):.0%}  tie {q.get('tie',0):.0%}")
