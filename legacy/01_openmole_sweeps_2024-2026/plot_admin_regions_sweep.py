import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

df = pd.read_csv("protection_strategy_sweep_admin_tiles.csv")

strategies = ["Random", "SpeciesRichness", "InteractionRichness"]
colors = {"Random": "#888888", "SpeciesRichness": "#2196F3", "InteractionRichness": "#E91E63"}
labels = {"Random": "Random", "SpeciesRichness": "Species Richness", "InteractionRichness": "Interaction Richness"}

admin_values = sorted(df["nAdminRegions"].unique())
n_rows = len(admin_values)

fig, axes = plt.subplots(n_rows, 3, figsize=(16, 5 * n_rows))

for row_idx, admin_val in enumerate(admin_values):
    df_admin = df[df["nAdminRegions"] == admin_val]

    grouped = df_admin.groupby(["strategy", "fractionProtected"]).agg(
        ie_median=("interactionExtinctions", "median"),
        ie_q25=("interactionExtinctions", lambda x: x.quantile(0.25)),
        ie_q75=("interactionExtinctions", lambda x: x.quantile(0.75)),
        se_median=("speciesExtinctions", "median"),
        se_q25=("speciesExtinctions", lambda x: x.quantile(0.25)),
        se_q75=("speciesExtinctions", lambda x: x.quantile(0.75)),
    ).reset_index()

    ax0 = axes[row_idx, 0]
    ax1 = axes[row_idx, 1]
    ax2 = axes[row_idx, 2]

    for strat in strategies:
        sub = grouped[grouped["strategy"] == strat].sort_values("fractionProtected")
        c = colors[strat]
        lab = labels[strat]
        fp = sub["fractionProtected"]

        # Plot 1: Interaction extinctions vs fraction protected
        ax0.plot(fp, sub["ie_median"], color=c, label=lab, linewidth=2)
        ax0.fill_between(fp, sub["ie_q25"], sub["ie_q75"], color=c, alpha=0.15)

        # Plot 2: Species extinctions vs fraction protected
        ax1.plot(fp, sub["se_median"], color=c, label=lab, linewidth=2)
        ax1.fill_between(fp, sub["se_q25"], sub["se_q75"], color=c, alpha=0.15)

        # Plot 3: Species extinctions vs interaction extinctions
        sub2 = grouped[grouped["strategy"] == strat].sort_values("ie_median")
        ax2.plot(sub2["ie_median"], sub2["se_median"], "o-", color=c, label=lab, markersize=4, linewidth=1.5)
        ax2.fill_betweenx(sub2["se_median"], sub2["ie_q25"], sub2["ie_q75"], color=c, alpha=0.1)
        ax2.fill_between(sub2["ie_median"], sub2["se_q25"], sub2["se_q75"], color=c, alpha=0.1)

    ax0.set_ylabel(f"nAdminRegions={admin_val}\nInteraction Extinctions (%)")
    ax0.set_xlabel("Fraction Protected")
    ax1.set_ylabel("Species Extinctions (%)")
    ax1.set_xlabel("Fraction Protected")
    ax2.set_xlabel("Interaction Extinctions (%)")
    ax2.set_ylabel("Species Extinctions (%)")

    if row_idx == 0:
        ax0.set_title("Interaction Extinctions vs. Protection")
        ax1.set_title("Species Extinctions vs. Protection")
        ax2.set_title("Species vs. Interaction Extinctions")

    ax0.legend()
    ax1.legend()
    ax2.legend()

    for ax in [ax0, ax1, ax2]:
        ax.spines["top"].set_visible(False)
        ax.spines["right"].set_visible(False)

plt.tight_layout()
plt.savefig("admin_regions_sweep_plots.png", dpi=200, bbox_inches="tight")
plt.close()
print("Saved to admin_regions_sweep_plots.png")
