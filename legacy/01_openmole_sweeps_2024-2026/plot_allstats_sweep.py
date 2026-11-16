import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

df = pd.read_csv("protection_strategy_sweep_allstats_more_replicas.csv")

strategies = ["Random", "SpeciesRichness", "InteractionRichness", "InteractionComplementarity"]
colors = {
    "Random": "#888888",
    "SpeciesRichness": "#2196F3",
    "InteractionRichness": "#E91E63",
    "InteractionComplementarity": "#4CAF50",
}
labels = {
    "Random": "Random",
    "SpeciesRichness": "Species Richness",
    "InteractionRichness": "Interaction Richness",
    "InteractionComplementarity": "Interaction Complementarity",
}


def agg_metric(df, metric):
    """Aggregate a metric by strategy x fractionProtected: median + IQR."""
    grouped = df.groupby(["strategy", "fractionProtected"])[metric].agg(
        median="median",
        q25=lambda x: x.quantile(0.25),
        q75=lambda x: x.quantile(0.75),
    ).reset_index()
    return grouped


def plot_metric(ax, df, metric, ylabel, title):
    """Plot one metric vs fractionProtected for all strategies."""
    grouped = agg_metric(df, metric)
    for strat in strategies:
        sub = grouped[grouped["strategy"] == strat].sort_values("fractionProtected")
        if sub.empty:
            continue
        c = colors[strat]
        lab = labels[strat]
        fp = sub["fractionProtected"]
        ax.plot(fp, sub["median"], color=c, label=lab, linewidth=2)
        ax.fill_between(fp, sub["q25"], sub["q75"], color=c, alpha=0.15)
    ax.set_xlabel("Fraction Protected")
    ax.set_ylabel(ylabel)
    ax.set_title(title)
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)


# ── Figure 1: Extinctions (3 panels) ──────────────────────────────────────────
fig1, axes1 = plt.subplots(1, 3, figsize=(18, 5))

plot_metric(axes1[0], df, "interactionExtinctions",
            "Interaction Extinctions (%)", "Interaction Extinctions")
plot_metric(axes1[1], df, "speciesExtinctions",
            "Species Extinctions (%)", "Species Extinctions")

# Panel 3: species vs interaction extinctions (parametric curve)
grouped_ext = df.groupby(["strategy", "fractionProtected"]).agg(
    ie_median=("interactionExtinctions", "median"),
    ie_q25=("interactionExtinctions", lambda x: x.quantile(0.25)),
    ie_q75=("interactionExtinctions", lambda x: x.quantile(0.75)),
    se_median=("speciesExtinctions", "median"),
    se_q25=("speciesExtinctions", lambda x: x.quantile(0.25)),
    se_q75=("speciesExtinctions", lambda x: x.quantile(0.75)),
).reset_index()

ax = axes1[2]
for strat in strategies:
    sub = grouped_ext[grouped_ext["strategy"] == strat].sort_values("ie_median")
    c = colors[strat]
    lab = labels[strat]
    ax.plot(sub["ie_median"], sub["se_median"], "o-", color=c, label=lab,
            markersize=4, linewidth=1.5)
    ax.fill_betweenx(sub["se_median"], sub["ie_q25"], sub["ie_q75"],
                      color=c, alpha=0.1)
    ax.fill_between(sub["ie_median"], sub["se_q25"], sub["se_q75"],
                     color=c, alpha=0.1)
ax.set_xlabel("Interaction Extinctions (%)")
ax.set_ylabel("Species Extinctions (%)")
ax.set_title("Species vs. Interaction Extinctions")
ax.spines["top"].set_visible(False)
ax.spines["right"].set_visible(False)

axes1[0].legend()
fig1.tight_layout()
fig1.savefig("allstats_extinctions.png", dpi=200, bbox_inches="tight")
plt.close(fig1)
print("Saved allstats_extinctions.png")


# ── Figure 2: Network topology metrics (2x3 grid) ────────────────────────────
fig2, axes2 = plt.subplots(2, 3, figsize=(18, 10))

plot_metric(axes2[0, 0], df, "spConnectance",
            "Connectance", "Species Web Connectance")
plot_metric(axes2[0, 1], df, "spWeakComponents",
            "Weak Components", "Species Web Weak Components")
plot_metric(axes2[0, 2], df, "spMeanInDegree",
            "Mean In-Degree", "Species Web Mean In-Degree")

plot_metric(axes2[1, 0], df, "popConnectance",
            "Connectance", "Population Web Connectance")
plot_metric(axes2[1, 1], df, "popWeakComponents",
            "Weak Components", "Population Web Weak Components")
plot_metric(axes2[1, 2], df, "popMeanInDegree",
            "Mean In-Degree", "Population Web Mean In-Degree")

axes2[0, 0].legend()
fig2.tight_layout()
fig2.savefig("allstats_network.png", dpi=200, bbox_inches="tight")
plt.close(fig2)
print("Saved allstats_network.png")


# ── Figure 3: Landscape structure + Beta-diversity (2x2) ─────────────────────
fig3, axes3 = plt.subplots(2, 2, figsize=(12, 10))

plot_metric(axes3[0, 0], df, "largestPatchFraction",
            "Largest Patch Fraction", "Largest Connected Patch")
plot_metric(axes3[0, 1], df, "patchFragmentation",
            "Fragmentation", "Patch Fragmentation")

plot_metric(axes3[1, 0], df, "speciesBetaDiversity",
            "Normalized β", "Species Beta-Diversity (protected)")
plot_metric(axes3[1, 1], df, "interactionBetaDiversity",
            "Normalized β", "Interaction Beta-Diversity (protected)")

axes3[0, 0].legend()
fig3.tight_layout()
fig3.savefig("allstats_landscape_beta.png", dpi=200, bbox_inches="tight")
plt.close(fig3)
print("Saved allstats_landscape_beta.png")


# ── Figure 4: Gamma-normalized + degree percentiles (2x2) ────────────────────
fig4, axes4 = plt.subplots(2, 2, figsize=(12, 10))

plot_metric(axes4[0, 0], df, "speciesGammaNorm",
            "γ protected / γ total", "Species Gamma (normalized)")
plot_metric(axes4[0, 1], df, "interactionGammaNorm",
            "γ protected / γ total", "Interaction Gamma (normalized)")

# Degree distribution ribbon: P10–P90 + P25–P75 + median
for idx, strat in enumerate(strategies):
    ax = axes4[1, 0] if idx < 2 else axes4[1, 1]

grouped_deg = df.groupby(["strategy", "fractionProtected"]).agg(
    p10=("spDegreeP10", "median"),
    p25=("spDegreeP25", "median"),
    p50=("spDegreeP50", "median"),
    p75=("spDegreeP75", "median"),
    p90=("spDegreeP90", "median"),
).reset_index()

# Left: Random + SpeciesRichness; Right: InteractionRichness + Complementarity
pairs = [
    (axes4[1, 0], ["Random", "SpeciesRichness"]),
    (axes4[1, 1], ["InteractionRichness", "InteractionComplementarity"]),
]
for ax, strats in pairs:
    for strat in strats:
        sub = grouped_deg[grouped_deg["strategy"] == strat].sort_values("fractionProtected")
        c = colors[strat]
        lab = labels[strat]
        fp = sub["fractionProtected"]
        ax.fill_between(fp, sub["p10"], sub["p90"], color=c, alpha=0.08)
        ax.fill_between(fp, sub["p25"], sub["p75"], color=c, alpha=0.15)
        ax.plot(fp, sub["p50"], color=c, label=lab, linewidth=2)
    ax.set_xlabel("Fraction Protected")
    ax.set_ylabel("Total Degree")
    ax.set_title("Species Degree Distribution (P10–P90)")
    ax.legend()
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)

fig4.tight_layout()
fig4.savefig("allstats_gamma_degree.png", dpi=200, bbox_inches="tight")
plt.close(fig4)
print("Saved allstats_gamma_degree.png")
