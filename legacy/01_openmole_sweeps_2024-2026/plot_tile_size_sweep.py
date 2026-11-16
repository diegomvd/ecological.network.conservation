"""
plot_tile_size_sweep.py
-----------------------
Analysis and plotting script for the tile-size sweep conservation simulation dataset.
Produces a multi-page PDF (tile_size_sweep_plots.pdf).

Figures:
  1  – Extinction curves: 7 rows (nTiles) × 2 cols (species | interaction),
       3 strategies, LOWESS-smoothed means ± 1 SE bands, independent y-axes
  2  – Imshow: IR − SR gap (species and interaction), diverging palette
  3  – Imshow: 4 panels — SR/IR × species/interaction extinction absolute levels
  4  – SR–IR tile selection convergence vs nTiles
  5  – Landscape structure vs tile size
  6  – Imshow: fraction of secondary (cascade) extinctions for SR and IR strategies
  7  – Beta diversity of protected set pre-extinction

Usage:
    python plot_tile_size_sweep.py

Dependencies: pandas, numpy, matplotlib, statsmodels (for LOWESS)
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
DATA_PATH = "/home/diego/git/ecological.network.conservation/tile_size_sweep.csv"
OUT_PDF   = "/home/diego/git/ecological.network.conservation/tile_size_sweep_plots.pdf"

# ---------------------------------------------------------------------------
# Global style
# ---------------------------------------------------------------------------
mpl.rcParams.update({
    "font.size":        10,
    "axes.labelsize":   10,
    "xtick.labelsize":   8,
    "ytick.labelsize":   8,
    "legend.fontsize":   8,
    "axes.titlesize":   10,
    "figure.dpi":       130,
    "axes.spines.top":  False,
    "axes.spines.right":False,
})

ALPHA_BAND = 0.18

STRAT_COLOR = {"species": "steelblue", "interaction": "darkorange", "random": "dimgray"}
STRAT_LABEL = {"species": "SR strategy", "interaction": "IR strategy", "random": "Random"}


# ---------------------------------------------------------------------------
# Load & aggregate
# ---------------------------------------------------------------------------
def load_and_aggregate(path):
    df = pd.read_csv(path)

    ntiles_map = (
        df[["avgPopulationsPerTile", "nTiles"]]
        .drop_duplicates()
        .sort_values("nTiles")
        .set_index("avgPopulationsPerTile")["nTiles"]
        .to_dict()
    )

    grp = df.groupby(["avgPopulationsPerTile", "fractionProtected"])
    agg_mean = grp.mean(numeric_only=True).reset_index()
    agg_se   = (grp.std(numeric_only=True) / np.sqrt(grp.size().reset_index(name="n")["n"])).reset_index()

    agg_mean["nTiles"] = agg_mean["avgPopulationsPerTile"].map(ntiles_map)
    agg_se  ["nTiles"] = agg_se  ["avgPopulationsPerTile"].map(ntiles_map)

    return df, agg_mean, agg_se, ntiles_map


def despine(ax):
    sns.despine(ax=ax)


def get_nearest_fp(df_sub, fp_target):
    idx = (df_sub["fractionProtected"] - fp_target).abs().idxmin()
    return df_sub.loc[[idx]]


# ---------------------------------------------------------------------------
# Figure 1: 7 rows × 2 cols; seaborn lineplot with 95% CI; independent y per panel
# ---------------------------------------------------------------------------
def fig_extinction_grid(df, ntiles_sorted, ntiles_to_app):
    nrows = len(ntiles_sorted)
    ncols = 2
    fig, axes = plt.subplots(nrows, ncols, figsize=(12, 3.0 * nrows), sharey=False)

    panels = [
        ("speciesExtinctions",    "% species extinct"),
        ("interactionExtinctions","% interactions extinct"),
    ]

    # Melt to long form once: columns strategy and value
    strat_map = {"species": "species", "interaction": "interaction", "random": "random"}
    records = []
    for prefix, _ in panels:
        for strat in strat_map:
            col = f"{prefix}_{strat}"
            tmp = df[["nTiles", "avgPopulationsPerTile", "fractionProtected", col]].copy()
            tmp = tmp.rename(columns={col: "value"})
            tmp["panel"]    = prefix
            tmp["strategy"] = strat
            records.append(tmp)
    long_df = pd.concat(records, ignore_index=True)

    for row_idx, nt in enumerate(ntiles_sorted):
        app  = ntiles_to_app[nt]
        sub  = long_df[long_df["nTiles"] == nt]

        for col_idx, (prefix, ylabel) in enumerate(panels):
            ax      = axes[row_idx, col_idx]
            sub_p   = sub[sub["panel"] == prefix]

            # ylim: max raw mean across strategies for this nTiles+panel
            mean_max = (
                sub_p.groupby(["fractionProtected", "strategy"])["value"]
                .mean().reset_index()["value"].max()
            )
            step     = 10.0 if mean_max > 20.0 else 5.0
            ylim_top = np.ceil((mean_max * 1.1) / step) * step

            for strat in ("random", "species", "interaction"):
                sns.lineplot(
                    data=sub_p[sub_p["strategy"] == strat],
                    x="fractionProtected", y="value",
                    ax=ax,
                    color=STRAT_COLOR[strat],
                    label=STRAT_LABEL[strat],
                    lw=2.0,
                    errorbar=("ci", 95),   # 95% bootstrap CI shading
                    err_kws={"alpha": ALPHA_BAND},
                    legend=False,
                )

            ax.set_xlim(0, 1)
            ax.set_ylim(0, ylim_top)
            ax.xaxis.set_major_locator(mticker.MultipleLocator(0.25))
            ax.set_xlabel("Fraction protected" if row_idx == nrows - 1 else "", fontsize=9)

            if row_idx == 0:
                ax.set_title(ylabel, fontsize=10, fontweight="bold")
            if col_idx == 0:
                ax.set_ylabel(f"nTiles={nt}\n(avg {app} pop/tile)", fontsize=8)
            else:
                ax.set_ylabel(ylabel, fontsize=8)
            despine(ax)

    handles = [
        Line2D([0], [0], color=STRAT_COLOR[s], lw=2, label=STRAT_LABEL[s])
        for s in ("random", "species", "interaction")
    ]
    fig.legend(handles=handles, loc="upper center", ncol=3,
               bbox_to_anchor=(0.5, 1.01), frameon=False, fontsize=9)
    fig.suptitle(
        "Figure 1: Extinction vs fraction protected by tile size\n"
        "(seaborn lineplot, mean ± 95% CI across 100 seeds; independent y-axes)",
        fontsize=12, y=1.03)
    fig.tight_layout()
    return fig


# ---------------------------------------------------------------------------
# Helper: 2-D matrix (nTiles × fp) for imshow
# ---------------------------------------------------------------------------
def build_matrix(agg_mean, ntiles_sorted, fp_sorted, col):
    mat = np.full((len(ntiles_sorted), len(fp_sorted)), np.nan)
    for i, nt in enumerate(ntiles_sorted):
        sub = agg_mean[agg_mean["nTiles"] == nt].set_index("fractionProtected")
        for j, fp in enumerate(fp_sorted):
            if fp in sub.index:
                mat[i, j] = sub.loc[fp, col]
    return mat


def imshow_panel(ax, fig, mat, fp_sorted, ntiles_sorted, title,
                 cmap, vmin, vmax, cbar_label):
    im = ax.imshow(mat, aspect="auto", origin="upper",
                   cmap=cmap, vmin=vmin, vmax=vmax,
                   extent=[fp_sorted[0] - (fp_sorted[1]-fp_sorted[0])/2,
                            fp_sorted[-1] + (fp_sorted[1]-fp_sorted[0])/2,
                            len(ntiles_sorted) - 0.5, -0.5])
    cb = fig.colorbar(im, ax=ax, shrink=0.85, pad=0.02)
    cb.set_label(cbar_label, fontsize=8)
    ax.set_yticks(range(len(ntiles_sorted)))
    ax.set_yticklabels([f"nTiles={nt}" for nt in ntiles_sorted], fontsize=8)
    ax.set_xlabel("Fraction protected", fontsize=9)
    ax.set_title(title, fontsize=9)
    ax.xaxis.set_major_locator(mticker.MultipleLocator(0.25))
    return im


# ---------------------------------------------------------------------------
# Figure 2: IR − SR gap imshow (species and interaction), diverging
# ---------------------------------------------------------------------------
def fig_gap_imshow(agg_mean, ntiles_sorted, fp_sorted):
    fig, axes = plt.subplots(1, 2, figsize=(14, 5))

    panels = [
        ("speciesExtinctions_interaction",    "speciesExtinctions_species",
         "IR − SR gap: species extinct (%-pts)"),
        ("interactionExtinctions_interaction","interactionExtinctions_species",
         "IR − SR gap: interactions extinct (%-pts)"),
    ]

    for ax, (col_ir, col_sr, title) in zip(axes, panels):
        mat = build_matrix(agg_mean, ntiles_sorted, fp_sorted, col_ir) - \
              build_matrix(agg_mean, ntiles_sorted, fp_sorted, col_sr)
        vabs = np.nanmax(np.abs(mat))
        imshow_panel(ax, fig, mat, fp_sorted, ntiles_sorted, title,
                     "RdBu_r", -vabs, vabs,
                     "%-pt difference (IR − SR)\nred = IR worse; blue = IR better")

    fig.suptitle(
        "Figure 2: IR − SR performance gap\n"
        "(red = IR worse than SR; blue = IR better)",
        fontsize=12)
    fig.tight_layout()
    return fig


# ---------------------------------------------------------------------------
# Figure 3: 4-panel imshow — SR/IR × species/interaction absolute extinction
# ---------------------------------------------------------------------------
def fig_absolute_imshow(agg_mean, ntiles_sorted, fp_sorted):
    fig, axes = plt.subplots(2, 2, figsize=(14, 9))

    panels = [
        # (col, title, row, col_idx)
        ("speciesExtinctions_species",         "SR strategy — % species extinct",       0, 0),
        ("speciesExtinctions_interaction",     "IR strategy — % species extinct",       0, 1),
        ("interactionExtinctions_species",     "SR strategy — % interactions extinct",  1, 0),
        ("interactionExtinctions_interaction", "IR strategy — % interactions extinct",  1, 1),
    ]

    # Shared vmax within each row so SR and IR are on the same colour scale
    sp_max  = max(
        np.nanmax(build_matrix(agg_mean, ntiles_sorted, fp_sorted, "speciesExtinctions_species")),
        np.nanmax(build_matrix(agg_mean, ntiles_sorted, fp_sorted, "speciesExtinctions_interaction"))
    )
    int_max = max(
        np.nanmax(build_matrix(agg_mean, ntiles_sorted, fp_sorted, "interactionExtinctions_species")),
        np.nanmax(build_matrix(agg_mean, ntiles_sorted, fp_sorted, "interactionExtinctions_interaction"))
    )
    vmaxes = {0: sp_max, 1: int_max}

    for col, title, ri, ci in panels:
        ax  = axes[ri, ci]
        mat = build_matrix(agg_mean, ntiles_sorted, fp_sorted, col)
        imshow_panel(ax, fig, mat, fp_sorted, ntiles_sorted, title,
                     "YlOrRd", 0, vmaxes[ri], "% extinct (mean over seeds)")

    axes[0, 0].set_ylabel("Species extinctions\n" + axes[0,0].get_ylabel(), fontsize=9)
    axes[1, 0].set_ylabel("Interaction extinctions\n" + axes[1,0].get_ylabel(), fontsize=9)

    fig.suptitle(
        "Figure 3: Absolute extinction levels — SR vs IR strategy × species vs interactions\n"
        "(rows share colour scale)",
        fontsize=12)
    fig.tight_layout()
    return fig


# ---------------------------------------------------------------------------
# Figure 4: SR–IR tile selection convergence vs nTiles
# ---------------------------------------------------------------------------
def fig_correlations(agg_mean, ntiles_sorted):
    sub = agg_mean[
        (agg_mean["fractionProtected"] >= 0.1) &
        (agg_mean["fractionProtected"] <= 0.9)
    ]
    grp = sub.groupby("nTiles")[
        ["tileOverlapJaccard", "tileRankCorrelation", "spIntCorrelation"]
    ].mean()

    nt_arr = np.array(ntiles_sorted)
    metric_styles = {
        "tileOverlapJaccard":   ("Jaccard tile overlap",        "royalblue"),
        "tileRankCorrelation":  ("Rank correlation (Spearman)", "darkorange"),
        "spIntCorrelation":     ("Score correlation (Pearson)", "forestgreen"),
    }

    fig, ax = plt.subplots(figsize=(8, 5))
    for col, (label, color) in metric_styles.items():
        ym = [grp.loc[nt, col] if nt in grp.index else np.nan for nt in ntiles_sorted]
        ax.plot(nt_arr, ym, color=color, lw=2, marker="o", ms=6, label=label)

    ax.set_xscale("log")
    ax.set_xlabel("nTiles (log scale)", fontsize=10)
    ax.set_ylabel("Similarity / correlation (0–1)", fontsize=10)
    ax.set_ylim(0, 1)
    ax.set_xticks(ntiles_sorted)
    ax.get_xaxis().set_major_formatter(mticker.ScalarFormatter())
    ax.legend(fontsize=9, frameon=False)
    despine(ax)

    fig.suptitle(
        "Figure 4: SR–IR tile selection convergence vs tile size\n"
        "(averaged over fractionProtected 0.1–0.9)",
        fontsize=12)
    fig.tight_layout()
    return fig


# ---------------------------------------------------------------------------
# Figure 5: Landscape structure vs tile size
# ---------------------------------------------------------------------------
def fig_landscape_structure(agg_mean, ntiles_sorted):
    grp = agg_mean.groupby("nTiles")[
        ["landscapeAlphaSR", "landscapeAlphaIR",
         "landscapeBetaSR",  "landscapeBetaIR",
         "landscapeAlphaSRstd", "landscapeAlphaIRstd",
         "spIntCorrelation"]
    ].mean()

    nt_arr = np.array(ntiles_sorted)
    fig, axes = plt.subplots(2, 2, figsize=(13, 9))
    ax_tl, ax_tr, ax_bl, ax_br = axes.flatten()

    def _vals(col):
        return [grp.loc[nt, col] if nt in grp.index else np.nan for nt in ntiles_sorted]

    ax_tl.plot(nt_arr, _vals("landscapeAlphaSR"), color="steelblue",  lw=2, marker="o", ms=6, label="Alpha SR")
    ax_tl.plot(nt_arr, _vals("landscapeAlphaIR"), color="darkorange", lw=2, marker="s", ms=6, label="Alpha IR")
    ax_tl.set_xscale("log"); ax_tl.set_xlabel("nTiles (log scale)", fontsize=10)
    ax_tl.set_ylabel("Mean per-tile richness", fontsize=10)
    ax_tl.set_title("Mean alpha richness per tile", fontsize=10)
    ax_tl.set_xticks(ntiles_sorted); ax_tl.get_xaxis().set_major_formatter(mticker.ScalarFormatter())
    ax_tl.legend(fontsize=8, frameon=False); despine(ax_tl)

    ax_tr.plot(nt_arr, _vals("landscapeBetaSR"), color="steelblue",  lw=2, marker="o", ms=6, label="Beta SR")
    ax_tr.plot(nt_arr, _vals("landscapeBetaIR"), color="darkorange", lw=2, marker="s", ms=6, label="Beta IR")
    ax_tr.set_xscale("log"); ax_tr.set_xlabel("nTiles (log scale)", fontsize=10)
    ax_tr.set_ylabel("Harrison beta-2 diversity", fontsize=10)
    ax_tr.set_title("Landscape beta diversity", fontsize=10)
    ax_tr.set_xticks(ntiles_sorted); ax_tr.get_xaxis().set_major_formatter(mticker.ScalarFormatter())
    ax_tr.legend(fontsize=8, frameon=False); despine(ax_tr)

    cv_sr = [grp.loc[nt, "landscapeAlphaSRstd"] / grp.loc[nt, "landscapeAlphaSR"]
             if nt in grp.index and grp.loc[nt, "landscapeAlphaSR"] > 0 else np.nan
             for nt in ntiles_sorted]
    cv_ir = [grp.loc[nt, "landscapeAlphaIRstd"] / grp.loc[nt, "landscapeAlphaIR"]
             if nt in grp.index and grp.loc[nt, "landscapeAlphaIR"] > 0 else np.nan
             for nt in ntiles_sorted]
    ax_bl.plot(nt_arr, cv_sr, color="steelblue",  lw=2, marker="o", ms=6, label="CV alpha SR")
    ax_bl.plot(nt_arr, cv_ir, color="darkorange", lw=2, marker="s", ms=6, label="CV alpha IR")
    ax_bl.set_xscale("log"); ax_bl.set_xlabel("nTiles (log scale)", fontsize=10)
    ax_bl.set_ylabel("CV (std / mean)", fontsize=10)
    ax_bl.set_title("Alpha richness heterogeneity across tiles", fontsize=10)
    ax_bl.set_xticks(ntiles_sorted); ax_bl.get_xaxis().set_major_formatter(mticker.ScalarFormatter())
    ax_bl.legend(fontsize=8, frameon=False); despine(ax_bl)

    ax_br.plot(nt_arr, _vals("spIntCorrelation"), color="forestgreen", lw=2, marker="D", ms=6,
               label="Pearson corr (SR vs IR scores)")
    ax_br.set_xscale("log"); ax_br.set_xlabel("nTiles (log scale)", fontsize=10)
    ax_br.set_ylabel("Pearson correlation", fontsize=10)
    ax_br.set_title("SR vs IR tile-score correlation", fontsize=10)
    ax_br.set_ylim(0, 1)
    ax_br.set_xticks(ntiles_sorted); ax_br.get_xaxis().set_major_formatter(mticker.ScalarFormatter())
    ax_br.legend(fontsize=8, frameon=False); despine(ax_br)

    fig.suptitle("Figure 5: Landscape structure vs tile size", fontsize=12)
    fig.tight_layout()
    return fig


# ---------------------------------------------------------------------------
# Figure 6: Imshow — fraction of secondary extinctions over total (species)
#           Two panels: SR strategy and IR strategy
# ---------------------------------------------------------------------------
def fig_secondary_fraction_imshow(agg_mean, ntiles_sorted, fp_sorted):
    fig, axes = plt.subplots(1, 2, figsize=(14, 5))

    panels = [
        ("species",     "SR strategy\nSecondary / total species extinctions"),
        ("interaction", "IR strategy\nSecondary / total species extinctions"),
    ]

    vmin_frac = 0.25   # data minimum is ~0.27, so floor at 0.25

    for ax, (strat, title) in zip(axes, panels):
        pri_col = f"primarySpeciesExtinctions_{strat}"
        sec_col = f"secondarySpeciesExtinctions_{strat}"

        mat_pri = build_matrix(agg_mean, ntiles_sorted, fp_sorted, pri_col)
        mat_sec = build_matrix(agg_mean, ntiles_sorted, fp_sorted, sec_col)
        total   = mat_pri + mat_sec
        # Fraction of total that is secondary; NaN where total==0
        frac_sec = np.where(total > 0, mat_sec / total, np.nan)

        imshow_panel(ax, fig, frac_sec, fp_sorted, ntiles_sorted, title,
                     "viridis", vmin_frac, 1.0,
                     "Secondary / (primary + secondary)")

    fig.suptitle(
        "Figure 6: Fraction of cascade (secondary) extinctions out of total species lost\n"
        "(higher = more cascading; primary extinctions dominate when low)",
        fontsize=12)
    fig.tight_layout()
    return fig


# ---------------------------------------------------------------------------
# Figure 7: Beta diversity of protected set pre-extinction
# ---------------------------------------------------------------------------
def fig_beta_diversity(agg_mean, agg_se, ntiles_sorted):
    cmap = mpl.colormaps["plasma"]
    tile_colors = {nt: cmap(v) for nt, v in
                   zip(ntiles_sorted, np.linspace(0.05, 0.95, len(ntiles_sorted)))}

    fig, axes = plt.subplots(1, 2, figsize=(14, 6))
    panels = [
        ("speciesBetaDiversity_pre",     "Species beta diversity of protected set", axes[0]),
        ("interactionBetaDiversity_pre", "Interaction beta diversity of protected set", axes[1]),
    ]

    for base_col, panel_title, ax in panels:
        for nt in ntiles_sorted:
            color = tile_colors[nt]
            sub_m = agg_mean[agg_mean["nTiles"] == nt].sort_values("fractionProtected")
            sub_e = agg_se  [agg_se  ["nTiles"] == nt].sort_values("fractionProtected")
            fp_common = sorted(
                set(sub_m["fractionProtected"].values) &
                set(sub_e["fractionProtected"].values)
            )
            sub_m = sub_m[sub_m["fractionProtected"].isin(fp_common)]
            sub_e = sub_e[sub_e["fractionProtected"].isin(fp_common)]
            fp = sub_m["fractionProtected"].values

            for strat, ls in [("species", "-"), ("interaction", "--")]:
                col = f"{base_col}_{strat}"
                if col not in sub_m.columns:
                    continue
                ym  = sub_m[col].values
                se  = sub_e[col].values
                label = f"nTiles={nt}" if strat == "species" else "_nolegend_"
                ax.plot(fp, ym, color=color, ls=ls, lw=1.6, label=label)
                ax.fill_between(fp, ym - se, ym + se, color=color, alpha=ALPHA_BAND)

        ax.set_xlim(0, 1); ax.set_ylim(0, 1)
        ax.set_xlabel("Fraction protected", fontsize=10)
        ax.set_ylabel("Beta diversity (Harrison)", fontsize=10)
        ax.set_title(panel_title, fontsize=10)
        ax.xaxis.set_major_locator(mticker.MultipleLocator(0.25))
        despine(ax)

    handles_nt = [Line2D([0], [0], color=tile_colors[nt], lw=2, label=f"nTiles={nt}")
                  for nt in ntiles_sorted]
    handles_st = [Line2D([0], [0], color="black", ls="-",  lw=1.5, label="SR strategy"),
                  Line2D([0], [0], color="black", ls="--", lw=1.5, label="IR strategy")]
    fig.legend(handles=handles_nt + handles_st,
               loc="lower center", ncol=5, fontsize=8,
               frameon=False, bbox_to_anchor=(0.5, -0.04))

    fig.suptitle(
        "Figure 7: Beta diversity of protected tile set before extinctions\n"
        "(solid = SR strategy, dashed = IR strategy; colour = nTiles)",
        fontsize=12)
    fig.tight_layout()
    return fig


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------
def main():
    print(f"Loading data from {DATA_PATH} ...")
    df, agg_mean, agg_se, ntiles_map = load_and_aggregate(DATA_PATH)

    ntiles_sorted = sorted(ntiles_map.values())
    ntiles_to_app = {v: k for k, v in ntiles_map.items()}
    fp_sorted     = sorted(agg_mean["fractionProtected"].unique())
    df["nTiles"]  = df["avgPopulationsPerTile"].map(ntiles_map)

    print(f"nTiles: {ntiles_sorted}")
    print(f"fractionProtected: {len(fp_sorted)} values, {fp_sorted[0]:.3f}–{fp_sorted[-1]:.3f}")
    print("Building figures ...")

    with PdfPages(OUT_PDF) as pdf:

        fig1 = fig_extinction_grid(df, ntiles_sorted, ntiles_to_app)
        pdf.savefig(fig1, bbox_inches="tight"); plt.close(fig1)
        print("  Figure 1 done.")

        fig2 = fig_gap_imshow(agg_mean, ntiles_sorted, fp_sorted)
        pdf.savefig(fig2, bbox_inches="tight"); plt.close(fig2)
        print("  Figure 2 done.")

        fig3 = fig_absolute_imshow(agg_mean, ntiles_sorted, fp_sorted)
        pdf.savefig(fig3, bbox_inches="tight"); plt.close(fig3)
        print("  Figure 3 done.")

        fig4 = fig_correlations(agg_mean, ntiles_sorted)
        pdf.savefig(fig4, bbox_inches="tight"); plt.close(fig4)
        print("  Figure 4 done.")

        fig5 = fig_landscape_structure(agg_mean, ntiles_sorted)
        pdf.savefig(fig5, bbox_inches="tight"); plt.close(fig5)
        print("  Figure 5 done.")

        fig6 = fig_secondary_fraction_imshow(agg_mean, ntiles_sorted, fp_sorted)
        pdf.savefig(fig6, bbox_inches="tight"); plt.close(fig6)
        print("  Figure 6 done.")

        fig7 = fig_beta_diversity(agg_mean, agg_se, ntiles_sorted)
        pdf.savefig(fig7, bbox_inches="tight"); plt.close(fig7)
        print("  Figure 7 done.")

        d = pdf.infodict()
        d["Title"]   = "Tile-size sweep conservation simulation analysis"
        d["Author"]  = "plot_tile_size_sweep.py"
        d["Subject"] = "Ecological network conservation — tile granularity sweep"

    print(f"\nSaved: {OUT_PDF}")


if __name__ == "__main__":
    main()
