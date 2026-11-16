#!/usr/bin/env python3
"""Figs. 3–5: what SR and IR protect, and what is left, from ProtectedSetExport (one row per population).

Cases: rows = connectance C, columns = tile size; budget and pExt as exported (default 0.15, 0.8). Pooled over seeds.
  3a  body mass of the populations in the protected tiles (grey = all populations of the baseline web)
  3b  same, unique species per protected set
  4   flow trophic position of the protected populations (grey = baseline web)
  5a  total biomass per trophic-position class in the FINAL webs (grey = baseline), mean over seeds ± 95% CI
  5b  share of the baseline biomass of each class that survives (per seed, then mean ± 95% CI)

Trophic positions count plants: basal herbivores = 2 (flow TP from the model + 1), consumers ≥ 3.

Usage: python3 analysis/paper_sweep/protected_sets.py [path/to/protected_sets.csv.gz]
"""
import sys
import numpy as np, pandas as pd, matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from paths import PROTECTED, FIGS, SR_IR_COLORS

src = sys.argv[1] if len(sys.argv) > 1 else PROTECTED
d = pd.read_csv(src)
d["tp"] = d.flow_tp + 1.0
d["log_m"] = np.log10(d.body_mass)
base = d[d.stage == "baseline"]
cs = sorted(d.c_meta.unique())
ts = sorted(d.tile_size.dropna().unique())
nseeds = d.seed.nunique()
STRATS = ["SR", "IR"]
GREY = "0.75"


def grid(title):
    fig, axes = plt.subplots(len(cs), len(ts), figsize=(5.0 * len(ts), 3.2 * len(cs)), squeeze=False,
                             sharex=True, constrained_layout=True)
    fig.suptitle(title)
    return fig, axes


def label(ax, c, t, i, j, xlabel, ylabel):
    ax.set_title(f"C = {c}, tile = {t:g} HR diameters", fontsize=9)
    if i == len(cs) - 1: ax.set_xlabel(xlabel)
    if j == 0: ax.set_ylabel(ylabel)
    ax.grid(alpha=0.3)


def save(fig, name):
    out = FIGS / name
    fig.savefig(out, dpi=120); plt.close(fig); print(out)


def hist_fig(value, bins, xlabel, title, name, species=False, logy=False):
    fig, axes = grid(title)
    for i, c in enumerate(cs):
        b = base[base.c_meta == c]
        if species: b = b.drop_duplicates(["seed", "species_id"])
        for j, t in enumerate(ts):
            ax = axes[i, j]
            ax.hist(b[value], bins=bins, density=True, color=GREY, label="baseline web")
            for s in STRATS:
                p = d[(d.stage == "protected") & (d.strategy == s) & (d.c_meta == c) & (d.tile_size == t)]
                if species: p = p.drop_duplicates(["seed", "species_id"])
                n = len(p) / nseeds
                ax.hist(p[value], bins=bins, density=True, histtype="step", lw=1.8, color=SR_IR_COLORS[s],
                        label=f"{s} ({n:.0f} {'species' if species else 'pops'}/seed)")
            if logy: ax.set_yscale("log")
            label(ax, c, t, i, j, xlabel, "density")
            ax.legend(fontsize=7, frameon=False)
    save(fig, name)


frac = "budget 0.15, pExt 0.8"
mbins = np.linspace(np.floor(d.log_m.min() * 4) / 4, np.ceil(d.log_m.max() * 4) / 4, 41)
hist_fig("log_m", mbins, "log10 body mass", f"Body mass of protected populations ({frac}; pooled over {nseeds} seeds)",
         "protected_body_mass_populations.png")
hist_fig("log_m", mbins, "log10 body mass", f"Body mass of protected species ({frac}; pooled over {nseeds} seeds)",
         "protected_body_mass_species.png", species=True)
tbins = np.concatenate([[1.95, 2.05], np.arange(2.9, d.tp.max() + 0.1, 0.1)])
hist_fig("tp", tbins, "flow trophic position (herbivores = 2)",
         f"Trophic position of protected populations ({frac}; pooled over {nseeds} seeds; log scale)",
         "protected_trophic_position.png", logy=True)

# ---- biomass per trophic-position class in the final webs -----------------------------------
# Flow TPs bunch near integers (most consumers feed on one level), so classes are TP rounded to the nearest integer.
edges = [1.5, 2.5, 3.5, 4.5, 99.0]
names = ["2 (herbivores)", "≈3", "≈4", "≥4.5"]
d["tp_class"] = pd.cut(d.tp, edges, labels=names, right=False)
per_seed = (d.groupby(["c_meta", "tile_size", "stage", "strategy", "seed", "tp_class"], dropna=False, observed=False)
            .biomass.sum().reset_index())
bseed = per_seed[per_seed.stage == "baseline"].drop(columns=["tile_size", "stage", "strategy"])


def ci(x):
    x = x.dropna()
    return 1.96 * x.std(ddof=1) / np.sqrt(len(x)) if len(x) > 1 else 0.0


x = np.arange(len(names)); w = 0.27
fig, axes = grid(f"Total biomass per trophic-position class, final webs ({frac}; mean over {nseeds} seeds ± 95% CI)")
fig2, axes2 = grid(f"Share of baseline biomass surviving, per trophic-position class ({frac}; mean ± 95% CI)")
for i, c in enumerate(cs):
    bb = bseed[bseed.c_meta == c].groupby("tp_class", observed=False).biomass
    for j, t in enumerate(ts):
        ax, ax2 = axes[i, j], axes2[i, j]
        ax.bar(x - w, bb.mean().values, w, yerr=bb.apply(ci).values, color=GREY, label="baseline")
        for k, s in enumerate(STRATS):
            f = per_seed[(per_seed.stage == "final") & (per_seed.strategy == s) & (per_seed.c_meta == c) & (per_seed.tile_size == t)]
            g = f.groupby("tp_class", observed=False).biomass
            ax.bar(x + k * w, g.mean().values, w, yerr=g.apply(ci).values, color=SR_IR_COLORS[s], label=s)
            r = f.merge(bseed[bseed.c_meta == c], on=["seed", "tp_class"], suffixes=("", "_base"))
            r["share"] = np.where(r.biomass_base > 0, r.biomass / r.biomass_base, np.nan)
            gr = r.groupby("tp_class", observed=False).share
            ax2.errorbar(x + (k - 0.5) * 0.15, gr.mean().values, yerr=gr.apply(ci).values, fmt="o-", ms=4,
                         color=SR_IR_COLORS[s], label=s, capsize=2)
        ax.set_yscale("log"); ax2.set_ylim(0, 1)
        for a in (ax, ax2): a.set_xticks(x, names, fontsize=8)
        label(ax, c, t, i, j, "trophic position class", "biomass (model units)")
        label(ax2, c, t, i, j, "trophic position class", "final / baseline biomass")
        ax.legend(fontsize=7, frameon=False); ax2.legend(fontsize=7, frameon=False)
save(fig, "final_biomass_by_trophic_position.png")
save(fig2, "final_biomass_share_by_trophic_position.png")
