"""
Plot the community-parametrization audit produced by community_audit.oms.

Reads community_audit.csv (OpenMOLE DirectSampling output) and characterizes the
ECOLOGICAL COMMUNITY at construction time (pre-extinction) as a function of the
community-shaping knobs. Four figures:

  1. community_audit_degree_dist.png  -- degree-distribution SHAPE (species realized
     web + population web), reconstructed from the fixed-width histogram bin columns.
  2. community_audit_spatial.png      -- spatial structure vs sweep axes.
  3. community_audit_density.png      -- home-range / density vs sweep axes.
  4. community_audit_beta.png         -- beta / gamma diversity vs sweep axes.

Conventions follow plot_allstats_sweep.py / plot_tile_size_sweep.py: pandas +
matplotlib, group by parameter, median +/- IQR band, save PNG.

Usage:
    python plot_community_audit.py [community_audit.csv]
"""

import sys
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

CSV = sys.argv[1] if len(sys.argv) > 1 else "community_audit.csv"
df = pd.read_csv(CSV)

# ── Degree histogram bin columns ────────────────────────────────────────────────
# Must match DEGREE_BINS = 16 in community_audit.oms: bins 0..14 are exact total
# degrees, bin 15 is the overflow (degree >= 15).
N_BINS = 16
SP_BINS = [f"spDegBin{i}" for i in range(N_BINS)]
POP_BINS = [f"popDegBin{i}" for i in range(N_BINS)]
BIN_LABELS = [str(i) for i in range(N_BINS - 1)] + [f">={N_BINS - 1}"]

# Faceting axis for the degree-distribution figure: which knob to vary across panels.
# spreadingFactor is the sharpest control on the population-web degree shape.
FACET = "spreadingFactor"


def agg_median_iqr(df, metric, by):
    """Median + IQR of `metric` grouped by columns in `by`."""
    g = df.groupby(by)[metric].agg(
        median="median",
        q25=lambda x: x.quantile(0.25),
        q75=lambda x: x.quantile(0.75),
    ).reset_index()
    return g


def line_vs_axis(ax, df, metric, axis, ylabel, title):
    """Plot median +/- IQR of `metric` against a swept `axis`."""
    g = agg_median_iqr(df, metric, [axis]).sort_values(axis)
    ax.plot(g[axis], g["median"], color="#2196F3", linewidth=2, marker="o")
    ax.fill_between(g[axis], g["q25"], g["q75"], color="#2196F3", alpha=0.15)
    ax.set_xlabel(axis)
    ax.set_ylabel(ylabel)
    ax.set_title(title)
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)


# ── Figure 1: degree-distribution shape ─────────────────────────────────────────
# Reconstruct the mean histogram (across all replicates/rows) for each level of the
# facet axis, and plot species-level and population-level total-degree distributions.
facet_vals = sorted(df[FACET].unique())
fig1, axes1 = plt.subplots(2, len(facet_vals), figsize=(4.5 * len(facet_vals), 8),
                           squeeze=False)

x = np.arange(N_BINS)
for j, fv in enumerate(facet_vals):
    sub = df[df[FACET] == fv]

    sp_mean = sub[SP_BINS].mean().values
    pop_mean = sub[POP_BINS].mean().values

    ax_sp = axes1[0][j]
    ax_sp.bar(x, sp_mean, color="#E91E63", alpha=0.85)
    ax_sp.set_yscale("symlog")  # symlog handles zero-count bins while exposing tails
    ax_sp.set_title(f"Species realized web\n{FACET} = {fv}")
    ax_sp.set_ylabel("mean node count")
    ax_sp.set_xticks(x)
    ax_sp.set_xticklabels(BIN_LABELS, rotation=45, fontsize=7)

    ax_pop = axes1[1][j]
    ax_pop.bar(x, pop_mean, color="#2196F3", alpha=0.85)
    ax_pop.set_yscale("symlog")
    ax_pop.set_title(f"Population web\n{FACET} = {fv}")
    ax_pop.set_ylabel("mean node count")
    ax_pop.set_xlabel("total degree")
    ax_pop.set_xticks(x)
    ax_pop.set_xticklabels(BIN_LABELS, rotation=45, fontsize=7)

fig1.suptitle("Degree-distribution shape (mean binned histograms)", fontsize=14)
fig1.tight_layout(rect=[0, 0, 1, 0.97])
fig1.savefig("community_audit_degree_dist.png", dpi=140)
print("wrote community_audit_degree_dist.png")

# ── Figure 2: spatial structure vs sweep axes ───────────────────────────────────
# Show each spatial metric against the axis that most directly controls it.
spatial_panels = [
    ("occupancyRate", "spreadingFactor", "Tile occupancy (%)"),
    ("tileGiniCoefficient", "spreadingFactor", "Tile Gini (clustering)"),
    ("speciesCoOccurrence", "spreadingFactor", "Species co-occurrence"),
    ("avgSpeciesBBoxArea", "spreadingFactor", "Avg species bbox area (frac landscape)"),
    ("avgSpeciesBBoxArea", "medianHomeRange", "Avg species bbox area (frac landscape)"),
    ("populationsInNetwork", "areaOverlap", "Populations in network"),
]
fig2, axes2 = plt.subplots(2, 3, figsize=(18, 10))
for ax, (metric, axis, ylabel) in zip(axes2.flat, spatial_panels):
    line_vs_axis(ax, df, metric, axis, ylabel, metric)
fig2.suptitle("Spatial structure vs community-shaping knobs", fontsize=14)
fig2.tight_layout(rect=[0, 0, 1, 0.97])
fig2.savefig("community_audit_spatial.png", dpi=140)
print("wrote community_audit_spatial.png")

# ── Figure 3: home-range / density vs sweep axes ────────────────────────────────
density_panels = [
    ("numberOfPopulations", "areaOverlap", "Number of populations"),
    ("avgPopsPerOccupiedTile", "avgPopulationsPerTile", "Avg pops per occupied tile"),
    ("networkDensity", "medianHomeRange", "Population-web density"),
    ("avgInteractionsPerPop", "medianHomeRange", "Avg interactions per population"),
    ("popMeanInDegree", "medianHomeRange", "Pop-web mean in-degree"),
    ("spMeanInDegree", "medianHomeRange", "Species-web mean in-degree"),
]
fig3, axes3 = plt.subplots(2, 3, figsize=(18, 10))
for ax, (metric, axis, ylabel) in zip(axes3.flat, density_panels):
    line_vs_axis(ax, df, metric, axis, ylabel, metric)
fig3.suptitle("Home range / density vs community-shaping knobs", fontsize=14)
fig3.tight_layout(rect=[0, 0, 1, 0.97])
fig3.savefig("community_audit_density.png", dpi=140)
print("wrote community_audit_density.png")

# ── Figure 4: beta / gamma diversity vs sweep axes ──────────────────────────────
beta_panels = [
    ("landscapeBetaSR", "spreadingFactor", "Species beta diversity"),
    ("landscapeBetaIR", "spreadingFactor", "Interaction beta diversity"),
    ("landscapeBetaSR", "medianHomeRange", "Species beta diversity"),
    ("landscapeBetaIR", "medianHomeRange", "Interaction beta diversity"),
]
fig4, axes4 = plt.subplots(2, 2, figsize=(12, 10))
for ax, (metric, axis, ylabel) in zip(axes4.flat, beta_panels):
    line_vs_axis(ax, df, metric, axis, f"{ylabel}", f"{metric} vs {axis}")
fig4.suptitle("Beta / gamma diversity vs community-shaping knobs", fontsize=14)
fig4.tight_layout(rect=[0, 0, 1, 0.97])
fig4.savefig("community_audit_beta.png", dpi=140)
print("wrote community_audit_beta.png")
