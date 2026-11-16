"""
plot_strategy_analysis.py
-------------------------
Unified analysis of tile-size sweep conservation simulations.
Pulls from three CSVs:
  - tile_size_sweep.csv          (SR, IR, Random)
  - tile_size_sweep_alt_strategies.csv  (EgoIR, ConnInt, SR repeated)
  - tile_size_sweep_complementarity.csv (SpComp, EgoComp, IntComp)

Figures
-------
  1  Strategy comparison: species and interaction extinctions vs protection
     fraction, faceted by tile size — all 8 strategies on common axes
  2  IR vs SR gap heatmap: where does each interaction-based strategy
     diverge from SR, for species and interaction extinction
  3  Primary vs secondary extinction decomposition: which part of the
     strategy gap is tile selection vs cascade differences
  4  SR–IR tile selection correlation vs tile size
  5  Complementarity vs local strategies: gain from greedy selection
  6  Beta diversity of the protected set: species and interaction turnover
     by strategy and tile size

Usage
-----
    python plot_strategy_analysis.py

Output
------
    strategy_analysis.pdf
"""

import numpy as np
import pandas as pd
import matplotlib as mpl
import matplotlib.pyplot as plt
import matplotlib.ticker as mticker
from matplotlib.backends.backend_pdf import PdfPages
from matplotlib.lines import Line2D
import seaborn as sns

# ---------------------------------------------------------------------------
# Paths
# ---------------------------------------------------------------------------
BASE = "/home/diego/git/ecological.network.conservation"
OUT_PDF = f"{BASE}/strategy_analysis.pdf"

# ---------------------------------------------------------------------------
# Style
# ---------------------------------------------------------------------------
mpl.rcParams.update({
    "font.size": 10,
    "axes.labelsize": 10,
    "xtick.labelsize": 8,
    "ytick.labelsize": 8,
    "legend.fontsize": 8,
    "axes.titlesize": 10,
    "figure.dpi": 130,
    "axes.spines.top": False,
    "axes.spines.right": False,
})
ALPHA_BAND = 0.18

# Consistent color/style palette for all 8 strategies
STRAT_STYLE = {
    "Random":    dict(color="#888888", ls="-",  lw=1.5, label="Random"),
    "SR":        dict(color="#2196F3", ls="-",  lw=2.2, label="Species Richness"),
    "IR":        dict(color="#E91E63", ls="-",  lw=2.2, label="Interaction Richness (intra)"),
    "EgoIR":     dict(color="#FF7043", ls="--", lw=2.2, label="Interaction Richness (ego)"),
    "ConnInt":   dict(color="#9C27B0", ls="-",  lw=1.8, label="Connectivity+IR blend"),
    "SpComp":    dict(color="#1565C0", ls=":",  lw=2.0, label="Species Complementarity"),
    "IntComp":   dict(color="#B71C1C", ls=":",  lw=2.0, label="Interaction Complementarity"),
    "EgoComp":   dict(color="#E65100", ls=":",  lw=2.0, label="Ego Complementarity"),
}

def despine(ax):
    sns.despine(ax=ax)


# ---------------------------------------------------------------------------
# Load data
# ---------------------------------------------------------------------------
def load_data():
    d_main = pd.read_csv(f"{BASE}/tile_size_sweep.csv")
    d_alt  = pd.read_csv(f"{BASE}/tile_size_sweep_alt_strategies.csv")
    d_comp = pd.read_csv(f"{BASE}/tile_size_sweep_complementarity.csv")

    ntiles_map = (
        d_main[["avgPopulationsPerTile", "nTiles"]]
        .drop_duplicates()
        .sort_values("nTiles")
        .set_index("avgPopulationsPerTile")["nTiles"]
        .to_dict()
    )

    # Build a unified long-form dataframe: one row per (avgPPT, fp, seed, strategy)
    # with columns: speciesExtinctions, interactionExtinctions,
    #               primarySpeciesExt, secondarySpeciesExt,
    #               primaryInteractionExt, secondaryInteractionExt,
    #               speciesBetaPre, interactionBetaPre
    records = []

    def add_rows(df, strat_key, suffix, has_primary_secondary=True):
        sub = df[["avgPopulationsPerTile", "nTiles", "fractionProtected", "myseed"]].copy()
        sub["strategy"] = strat_key
        sub["speciesExtinctions"]     = df[f"speciesExtinctions_{suffix}"]
        sub["interactionExtinctions"] = df[f"interactionExtinctions_{suffix}"]
        sub["speciesBetaPre"]         = df[f"speciesBetaDiversity_pre_{suffix}"]
        sub["interactionBetaPre"]     = df[f"interactionBetaDiversity_pre_{suffix}"]
        if has_primary_secondary:
            sub["primarySpeciesExt"]       = df[f"primarySpeciesExtinctions_{suffix}"]
            sub["secondarySpeciesExt"]     = df[f"secondarySpeciesExtinctions_{suffix}"]
            sub["primaryInteractionExt"]   = df[f"primaryInteractionExtinctions_{suffix}"]
            sub["secondaryInteractionExt"] = df[f"secondaryInteractionExtinctions_{suffix}"]
        else:
            for c in ["primarySpeciesExt","secondarySpeciesExt",
                      "primaryInteractionExt","secondaryInteractionExt"]:
                sub[c] = np.nan
        records.append(sub)

    add_rows(d_main, "Random",  "random")
    add_rows(d_main, "SR",      "species")
    add_rows(d_main, "IR",      "interaction")
    add_rows(d_alt,  "SR",      "species")       # repeated SR — more replicates
    add_rows(d_alt,  "EgoIR",   "egoIR")
    add_rows(d_alt,  "ConnInt", "connInt")
    add_rows(d_comp, "SpComp",  "spComp")
    add_rows(d_comp, "IntComp", "intComp")
    add_rows(d_comp, "EgoComp", "egoComp")

    long = pd.concat(records, ignore_index=True)
    long["nTiles"] = long["avgPopulationsPerTile"].map(ntiles_map)

    # Add total (primary+secondary) in absolute counts for decomposition
    long["totalSpeciesAbs"]      = long["primarySpeciesExt"]     + long["secondarySpeciesExt"]
    long["totalInteractionAbs"]  = long["primaryInteractionExt"] + long["secondaryInteractionExt"]
    long["fracSecondarySpecies"] = np.where(
        long["totalSpeciesAbs"] > 0,
        long["secondarySpeciesExt"] / long["totalSpeciesAbs"], np.nan
    )
    long["fracSecondaryInteraction"] = np.where(
        long["totalInteractionAbs"] > 0,
        long["secondaryInteractionExt"] / long["totalInteractionAbs"], np.nan
    )

    ntiles_sorted = sorted(ntiles_map.values())
    return long, d_main, ntiles_sorted, ntiles_map


# ---------------------------------------------------------------------------
# Aggregation helpers
# ---------------------------------------------------------------------------
def agg(df, groupby, cols):
    """Return mean and SE for cols grouped by groupby."""
    g = df.groupby(groupby)[cols]
    m = g.mean().reset_index()
    s = g.sem().reset_index()
    return m, s


def agg_median_iqr(df, groupby, cols):
    g = df.groupby(groupby)[cols]
    med = g.median().reset_index()
    q25 = g.quantile(0.25).reset_index()
    q75 = g.quantile(0.75).reset_index()
    return med, q25, q75


# ---------------------------------------------------------------------------
# Figure 1: Strategy comparison — 7 rows (tile sizes) × 2 cols (sp / int)
# 8 strategies, mean ± SE, full protection fraction range
# ---------------------------------------------------------------------------
def fig_strategy_comparison(long, ntiles_sorted):
    strats_order = ["Random", "SR", "IR", "EgoIR", "ConnInt", "SpComp", "IntComp", "EgoComp"]
    panels = [
        ("speciesExtinctions",     "% species extinct"),
        ("interactionExtinctions", "% interactions extinct"),
    ]
    nrows, ncols = len(ntiles_sorted), 2
    fig, axes = plt.subplots(nrows, ncols, figsize=(13, 3.2 * nrows), sharey=False)

    mean_df, se_df = agg(long, ["strategy", "nTiles", "fractionProtected"],
                          ["speciesExtinctions", "interactionExtinctions"])

    for ri, nt in enumerate(ntiles_sorted):
        for ci, (col, ylabel) in enumerate(panels):
            ax = axes[ri, ci]
            sub_m = mean_df[mean_df["nTiles"] == nt]
            sub_s = se_df[se_df["nTiles"] == nt]

            for strat in strats_order:
                sm = sub_m[sub_m["strategy"] == strat].sort_values("fractionProtected")
                ss = sub_s[sub_s["strategy"] == strat].sort_values("fractionProtected")
                if sm.empty:
                    continue
                st = STRAT_STYLE[strat]
                fp = sm["fractionProtected"].values
                ym = sm[col].values
                ye = ss[col].values
                ax.plot(fp, ym, color=st["color"], ls=st["ls"], lw=st["lw"], label=st["label"])
                ax.fill_between(fp, ym - ye, ym + ye, color=st["color"], alpha=ALPHA_BAND)

            ax.set_xlim(0, 1)
            ax.set_ylim(bottom=0)
            ax.xaxis.set_major_locator(mticker.MultipleLocator(0.25))
            if ri == nrows - 1:
                ax.set_xlabel("Fraction protected")
            if ri == 0:
                ax.set_title(ylabel, fontweight="bold")
            if ci == 0:
                ax.set_ylabel(f"nTiles={nt}", fontsize=8)
            despine(ax)

    handles = [Line2D([0],[0], color=STRAT_STYLE[s]["color"],
                      ls=STRAT_STYLE[s]["ls"], lw=STRAT_STYLE[s]["lw"],
                      label=STRAT_STYLE[s]["label"])
               for s in strats_order]
    fig.legend(handles=handles, loc="upper center", ncol=4,
               bbox_to_anchor=(0.5, 1.01), frameon=False, fontsize=8)
    fig.suptitle(
        "Figure 1: Extinction vs fraction protected, all strategies\n"
        "(mean ± SE across replicates; each row is a tile size)",
        fontsize=11, y=1.03)
    fig.tight_layout()
    return fig


# ---------------------------------------------------------------------------
# Figure 2: Gap heatmaps — each interaction strategy minus SR
# Rows = tile sizes, cols = strategy; panels = species / interaction
# ---------------------------------------------------------------------------
def fig_gap_heatmaps(long, ntiles_sorted):
    """
    For each interaction-based strategy X, compute mean(X) - mean(SR)
    at each (nTiles, fractionProtected) point.  Positive = X does worse.
    """
    interaction_strats = ["IR", "EgoIR", "ConnInt", "SpComp", "IntComp", "EgoComp"]
    outcomes = [
        ("speciesExtinctions",     "species extinct (%-pts)"),
        ("interactionExtinctions", "interactions extinct (%-pts)"),
    ]

    mean_df, _ = agg(long, ["strategy", "nTiles", "fractionProtected"],
                      ["speciesExtinctions", "interactionExtinctions"])

    fp_vals    = sorted(mean_df["fractionProtected"].unique())
    ntiles_arr = np.array(ntiles_sorted)

    nrows = len(outcomes)
    ncols = len(interaction_strats)
    fig, axes = plt.subplots(nrows, ncols, figsize=(3.2 * ncols, 4.5 * nrows))

    sr_mean = mean_df[mean_df["strategy"] == "SR"].set_index(["nTiles", "fractionProtected"])

    for ri, (col, col_label) in enumerate(outcomes):
        # shared vabs across all strategy panels in this row
        all_gaps = []
        for strat in interaction_strats:
            xm = mean_df[mean_df["strategy"] == strat].set_index(["nTiles", "fractionProtected"])
            gap = xm[col] - sr_mean[col]
            all_gaps.extend(gap.dropna().values)
        vabs = np.nanpercentile(np.abs(all_gaps), 95)

        for ci, strat in enumerate(interaction_strats):
            ax = axes[ri, ci]
            xm = mean_df[mean_df["strategy"] == strat].set_index(["nTiles", "fractionProtected"])
            gap = (xm[col] - sr_mean[col]).reset_index()

            mat = np.full((len(ntiles_sorted), len(fp_vals)), np.nan)
            for i, nt in enumerate(ntiles_sorted):
                for j, fp in enumerate(fp_vals):
                    row = gap[(gap["nTiles"] == nt) & (gap["fractionProtected"] == fp)]
                    if not row.empty:
                        mat[i, j] = row[col].values[0]

            im = ax.imshow(mat, aspect="auto", origin="upper",
                           cmap="RdBu_r", vmin=-vabs, vmax=vabs,
                           extent=[fp_vals[0], fp_vals[-1],
                                   len(ntiles_sorted) - 0.5, -0.5])
            if ri == 0:
                ax.set_title(STRAT_STYLE[strat]["label"], fontsize=8, fontweight="bold")
            if ci == 0:
                ax.set_ylabel(f"{col_label}\nnTiles →", fontsize=8)
                ax.set_yticks(range(len(ntiles_sorted)))
                ax.set_yticklabels([str(nt) for nt in ntiles_sorted], fontsize=7)
            else:
                ax.set_yticks([])
            if ri == nrows - 1:
                ax.set_xlabel("Fraction protected", fontsize=8)
            ax.xaxis.set_major_locator(mticker.MultipleLocator(0.5))

            cb = fig.colorbar(im, ax=ax, shrink=0.7, pad=0.02)
            cb.ax.tick_params(labelsize=6)
            if ci == ncols - 1:
                cb.set_label("%-pts vs SR\n(red=worse, blue=better)", fontsize=6)

    fig.suptitle(
        "Figure 2: Performance gap vs Species Richness strategy\n"
        "(blue = strategy does better than SR; red = worse)",
        fontsize=11)
    fig.tight_layout()
    return fig


# ---------------------------------------------------------------------------
# Figure 3: Primary vs secondary decomposition
# Left panel: absolute counts by strategy; Right panel: fraction secondary
# Faceted by tile size (select a few), x = fractionProtected
# ---------------------------------------------------------------------------
def fig_primary_secondary(long, ntiles_sorted):
    # Focus on SR vs IR vs EgoIR — the three that have primary/secondary data
    focal_strats = ["Random", "SR", "IR", "EgoIR", "ConnInt", "SpComp", "IntComp", "EgoComp"]
    # Use a subset of tile sizes for readability: small / medium / large
    focal_tiles = [ntiles_sorted[0], ntiles_sorted[2], ntiles_sorted[4], ntiles_sorted[-1]]

    sub = long[long["nTiles"].isin(focal_tiles) & long["strategy"].isin(focal_strats)].copy()
    mean_df, se_df = agg(sub,
                          ["strategy", "nTiles", "fractionProtected"],
                          ["fracSecondarySpecies", "fracSecondaryInteraction",
                           "speciesExtinctions", "interactionExtinctions"])

    panels = [
        ("fracSecondarySpecies",      "Fraction secondary\n(species)"),
        ("fracSecondaryInteraction",  "Fraction secondary\n(interactions)"),
    ]

    nrows = len(focal_tiles)
    ncols = len(panels)
    fig, axes = plt.subplots(nrows, ncols, figsize=(11, 3.5 * nrows), sharey=False)

    for ri, nt in enumerate(focal_tiles):
        for ci, (col, ylabel) in enumerate(panels):
            ax = axes[ri, ci]
            sm = mean_df[mean_df["nTiles"] == nt]
            ss = se_df[se_df["nTiles"] == nt]

            for strat in focal_strats:
                sms = sm[sm["strategy"] == strat].sort_values("fractionProtected")
                sss = ss[ss["strategy"] == strat].sort_values("fractionProtected")
                if sms.empty or sms[col].isna().all():
                    continue
                st = STRAT_STYLE[strat]
                fp = sms["fractionProtected"].values
                ym = sms[col].values
                ye = sss[col].values
                ax.plot(fp, ym, color=st["color"], ls=st["ls"], lw=st["lw"])
                ax.fill_between(fp, np.clip(ym - ye, 0, 1),
                                np.clip(ym + ye, 0, 1),
                                color=st["color"], alpha=ALPHA_BAND)

            ax.set_xlim(0, 1)
            ax.set_ylim(0, 1)
            ax.yaxis.set_major_locator(mticker.MultipleLocator(0.25))
            ax.xaxis.set_major_locator(mticker.MultipleLocator(0.25))
            if ri == nrows - 1:
                ax.set_xlabel("Fraction protected")
            if ri == 0:
                ax.set_title(ylabel, fontweight="bold")
            if ci == 0:
                ax.set_ylabel(f"nTiles={nt}", fontsize=8)
            despine(ax)

    handles = [Line2D([0],[0], color=STRAT_STYLE[s]["color"],
                      ls=STRAT_STYLE[s]["ls"], lw=STRAT_STYLE[s]["lw"],
                      label=STRAT_STYLE[s]["label"])
               for s in focal_strats if not all(long[long["strategy"]==s]["fracSecondarySpecies"].isna())]
    fig.legend(handles=handles, loc="upper center", ncol=4,
               bbox_to_anchor=(0.5, 1.01), frameon=False, fontsize=8)
    fig.suptitle(
        "Figure 3: Fraction of extinctions due to trophic cascades (secondary)\n"
        "(selected tile sizes; 0 = all primary, 1 = all cascade-driven)",
        fontsize=11, y=1.03)
    fig.tight_layout()
    return fig


# ---------------------------------------------------------------------------
# Figure 4: SR–IR tile selection correlation vs tile size
# Uses spIntCorrelation (Pearson of tile scores) and tileRankCorrelation
# and tileOverlapJaccard from tile_size_sweep.csv directly
# ---------------------------------------------------------------------------
def fig_tile_correlation(d_main, ntiles_sorted):
    ntiles_map = (
        d_main[["avgPopulationsPerTile", "nTiles"]]
        .drop_duplicates()
        .set_index("avgPopulationsPerTile")["nTiles"]
        .to_dict()
    )

    grp = d_main.groupby("nTiles")[
        ["spIntCorrelation", "tileRankCorrelation", "tileOverlapJaccard"]
    ].agg(["mean", "sem"])
    grp.columns = ["_".join(c) for c in grp.columns]

    metrics = {
        "spIntCorrelation":    ("Pearson score corr (SR vs IR tile scores)", "#4CAF50"),
        "tileRankCorrelation": ("Spearman rank corr (tile rankings)",         "#FF9800"),
        "tileOverlapJaccard":  ("Jaccard (selected tile overlap)",             "#9C27B0"),
    }

    fig, ax = plt.subplots(figsize=(8, 5))
    nt_arr = np.array(ntiles_sorted)
    for col, (label, color) in metrics.items():
        ym = [grp.loc[nt, f"{col}_mean"] if nt in grp.index else np.nan for nt in ntiles_sorted]
        ye = [grp.loc[nt, f"{col}_sem"]  if nt in grp.index else np.nan for nt in ntiles_sorted]
        ym, ye = np.array(ym), np.array(ye)
        ax.plot(nt_arr, ym, color=color, lw=2, marker="o", ms=6, label=label)
        ax.fill_between(nt_arr, ym - ye, ym + ye, color=color, alpha=0.15)

    ax.set_xscale("log")
    ax.set_xlabel("nTiles (log scale)", fontsize=10)
    ax.set_ylabel("Similarity / correlation (0–1)", fontsize=10)
    ax.set_ylim(0, 1)
    ax.set_xticks(nt_arr)
    ax.get_xaxis().set_major_formatter(mticker.ScalarFormatter())
    ax.axhline(1.0, color="k", lw=0.5, ls=":")
    ax.legend(fontsize=9, frameon=False)
    despine(ax)

    fig.suptitle(
        "Figure 4: SR–IR tile selection convergence vs tile size\n"
        "(mean ± SE across all protection fractions; higher = more similar tile choices)",
        fontsize=11)
    fig.tight_layout()
    return fig


# ---------------------------------------------------------------------------
# Figure 5: Complementarity gain — comp strategy vs its local counterpart
# SpComp vs SR, IntComp vs IR, EgoComp vs EgoIR
# Rows = tile sizes (subset), cols = species / interaction extinction
# ---------------------------------------------------------------------------
def fig_complementarity_gain(long, ntiles_sorted):
    pairs = [
        ("SpComp",  "SR",    "SpComp vs SR"),
        ("IntComp", "IR",    "IntComp vs IR"),
        ("EgoComp", "EgoIR", "EgoComp vs EgoIR"),
    ]
    focal_tiles = [ntiles_sorted[0], ntiles_sorted[2], ntiles_sorted[4], ntiles_sorted[-1]]
    outcomes = [
        ("speciesExtinctions",     "species extinct\ngap (%-pts)"),
        ("interactionExtinctions", "interactions extinct\ngap (%-pts)"),
    ]

    mean_df, _ = agg(long, ["strategy", "nTiles", "fractionProtected"],
                      ["speciesExtinctions", "interactionExtinctions"])

    pair_colors = {"SpComp vs SR": "#1565C0", "IntComp vs IR": "#B71C1C", "EgoComp vs EgoIR": "#E65100"}
    pair_ls     = {"SpComp vs SR": "-",       "IntComp vs IR": "-",       "EgoComp vs EgoIR": "--"}

    nrows = len(focal_tiles)
    ncols = len(outcomes)
    fig, axes = plt.subplots(nrows, ncols, figsize=(11, 3.5 * nrows), sharey=False)

    for ri, nt in enumerate(focal_tiles):
        for ci, (col, ylabel) in enumerate(outcomes):
            ax = axes[ri, ci]
            sub = mean_df[mean_df["nTiles"] == nt].set_index(["strategy", "fractionProtected"])[col]

            for comp_strat, local_strat, label in pairs:
                if (comp_strat, nt) not in [(s, n) for s, n, _ in
                     [(r["strategy"], r["nTiles"], None) for _, r in mean_df.iterrows()]]:
                    pass  # will just get empty
                fp_vals = sorted(mean_df["fractionProtected"].unique())
                gaps = []
                for fp in fp_vals:
                    try:
                        g = sub.loc[(comp_strat, fp)] - sub.loc[(local_strat, fp)]
                    except KeyError:
                        g = np.nan
                    gaps.append(g)

                color = pair_colors[label]
                ls    = pair_ls[label]
                ax.plot(fp_vals, gaps, color=color, ls=ls, lw=2, label=label)

            ax.axhline(0, color="k", lw=0.7, ls=":")
            ax.set_xlim(0, 1)
            ax.xaxis.set_major_locator(mticker.MultipleLocator(0.25))
            if ri == nrows - 1:
                ax.set_xlabel("Fraction protected")
            if ri == 0:
                ax.set_title(ylabel, fontweight="bold")
            if ci == 0:
                ax.set_ylabel(f"nTiles={nt}", fontsize=8)
            despine(ax)

    handles = [Line2D([0],[0], color=pair_colors[l], ls=pair_ls[l], lw=2, label=l)
               for _, _, l in pairs]
    handles.append(Line2D([0],[0], color="k", ls=":", lw=0.7, label="No gain (0)"))
    fig.legend(handles=handles, loc="upper center", ncol=2,
               bbox_to_anchor=(0.5, 1.01), frameon=False, fontsize=9)
    fig.suptitle(
        "Figure 5: Complementarity gain — comp strategy minus its local counterpart\n"
        "(negative = complementarity reduces extinctions; 0 = no gain)",
        fontsize=11, y=1.03)
    fig.tight_layout()
    return fig


# ---------------------------------------------------------------------------
# Figure 6: Beta diversity of protected set (pre-extinction)
# All strategies × tile sizes — species beta and interaction beta
# ---------------------------------------------------------------------------
def fig_beta_diversity(long, ntiles_sorted):
    strats_order = ["Random", "SR", "IR", "EgoIR", "ConnInt", "SpComp", "IntComp", "EgoComp"]
    outcomes = [
        ("speciesBetaPre",     "Species beta (Harrison, protected set)"),
        ("interactionBetaPre", "Interaction beta (Harrison, protected set)"),
    ]

    cmap = mpl.colormaps["plasma"]
    tile_colors = {nt: cmap(v) for nt, v in
                   zip(ntiles_sorted, np.linspace(0.05, 0.92, len(ntiles_sorted)))}

    mean_df, se_df = agg(long, ["strategy", "nTiles", "fractionProtected"],
                          ["speciesBetaPre", "interactionBetaPre"])

    fig, axes = plt.subplots(1, 2, figsize=(14, 6))

    for ax, (col, title) in zip(axes, outcomes):
        for nt in ntiles_sorted:
            color = tile_colors[nt]
            for strat, ls_key in [("SR", "-"), ("IR", "--"), ("EgoIR", ":"), ("SpComp", "-.")]:
                sm = mean_df[(mean_df["nTiles"] == nt) & (mean_df["strategy"] == strat)].sort_values("fractionProtected")
                se = se_df  [(se_df  ["nTiles"] == nt) & (se_df  ["strategy"] == strat)].sort_values("fractionProtected")
                if sm.empty:
                    continue
                fp = sm["fractionProtected"].values
                ym = sm[col].values
                ye = se[col].values
                label = f"nTiles={nt}" if strat == "SR" else "_nolegend_"
                ax.plot(fp, ym, color=color, ls=ls_key, lw=1.5, label=label)
                ax.fill_between(fp, np.clip(ym - ye, 0, 1), np.clip(ym + ye, 0, 1),
                                color=color, alpha=0.10)

        ax.set_xlim(0, 1)
        ax.set_ylim(0, 1)
        ax.set_xlabel("Fraction protected")
        ax.set_ylabel("Harrison beta-2 diversity")
        ax.set_title(title)
        despine(ax)

    handles_nt = [Line2D([0],[0], color=tile_colors[nt], lw=2, label=f"nTiles={nt}")
                  for nt in ntiles_sorted]
    handles_st = [
        Line2D([0],[0], color="k", ls="-",  lw=1.5, label="SR"),
        Line2D([0],[0], color="k", ls="--", lw=1.5, label="IR"),
        Line2D([0],[0], color="k", ls=":",  lw=1.5, label="EgoIR"),
        Line2D([0],[0], color="k", ls="-.", lw=1.5, label="SpComp"),
    ]
    fig.legend(handles=handles_nt + handles_st,
               loc="lower center", ncol=6, fontsize=8,
               frameon=False, bbox_to_anchor=(0.5, -0.04))
    fig.suptitle(
        "Figure 6: Beta diversity of the protected tile set before extinctions\n"
        "(colour = tile size; line style = strategy; higher = more spatial turnover in set)",
        fontsize=11)
    fig.tight_layout()
    return fig


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------
def main():
    print("Loading data ...")
    long, d_main, ntiles_sorted, ntiles_map = load_data()
    print(f"  Long-form rows: {len(long):,}")
    print(f"  Strategies: {sorted(long['strategy'].unique())}")
    print(f"  nTiles: {ntiles_sorted}")

    with PdfPages(OUT_PDF) as pdf:

        print("Figure 1: Strategy comparison ...")
        fig = fig_strategy_comparison(long, ntiles_sorted)
        pdf.savefig(fig, bbox_inches="tight"); plt.close(fig)

        print("Figure 2: Gap heatmaps ...")
        fig = fig_gap_heatmaps(long, ntiles_sorted)
        pdf.savefig(fig, bbox_inches="tight"); plt.close(fig)

        print("Figure 3: Primary vs secondary decomposition ...")
        fig = fig_primary_secondary(long, ntiles_sorted)
        pdf.savefig(fig, bbox_inches="tight"); plt.close(fig)

        print("Figure 4: Tile selection correlation ...")
        fig = fig_tile_correlation(d_main, ntiles_sorted)
        pdf.savefig(fig, bbox_inches="tight"); plt.close(fig)

        print("Figure 5: Complementarity gain ...")
        fig = fig_complementarity_gain(long, ntiles_sorted)
        pdf.savefig(fig, bbox_inches="tight"); plt.close(fig)

        print("Figure 6: Beta diversity ...")
        fig = fig_beta_diversity(long, ntiles_sorted)
        pdf.savefig(fig, bbox_inches="tight"); plt.close(fig)

        d = pdf.infodict()
        d["Title"]   = "Conservation strategy analysis — tile size sweep"
        d["Subject"] = "Ecological network conservation"

    print(f"\nSaved: {OUT_PDF}")


if __name__ == "__main__":
    main()
