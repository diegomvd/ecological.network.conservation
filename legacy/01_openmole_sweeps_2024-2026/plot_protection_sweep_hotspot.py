import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

df = pd.read_csv("protection_strategy_sweep_hotspot_0.csv")

# Compute median and IQR per strategy x fractionProtected
grouped = df.groupby(["strategy", "fractionProtected"]).agg(
    ie_median=("interactionExtinctions", "median"),
    ie_q25=("interactionExtinctions", lambda x: x.quantile(0.25)),
    ie_q75=("interactionExtinctions", lambda x: x.quantile(0.75)),
    se_median=("speciesExtinctions", "median"),
    se_q25=("speciesExtinctions", lambda x: x.quantile(0.25)),
    se_q75=("speciesExtinctions", lambda x: x.quantile(0.75)),
).reset_index()

strategies = ["Random", "SpeciesRichness", "InteractionRichness"]
colors = {"Random": "#888888", "SpeciesRichness": "#2196F3", "InteractionRichness": "#E91E63"}
labels = {"Random": "Random", "SpeciesRichness": "Species Richness", "InteractionRichness": "Interaction Richness"}

fig, axes = plt.subplots(1, 3, figsize=(16, 5))
fig.suptitle("Protection Sweep — Hotspot Landscape", fontsize=14, y=1.02)

for strat in strategies:
    sub = grouped[grouped["strategy"] == strat].sort_values("fractionProtected")
    c = colors[strat]
    lab = labels[strat]
    fp = sub["fractionProtected"]

    # Plot 1: Interaction extinctions vs fraction protected
    ax = axes[0]
    ax.plot(fp, sub["ie_median"], color=c, label=lab, linewidth=2)
    ax.fill_between(fp, sub["ie_q25"], sub["ie_q75"], color=c, alpha=0.15)

    # Plot 2: Species extinctions vs fraction protected
    ax = axes[1]
    ax.plot(fp, sub["se_median"], color=c, label=lab, linewidth=2)
    ax.fill_between(fp, sub["se_q25"], sub["se_q75"], color=c, alpha=0.15)

axes[0].set_xlabel("Fraction Protected")
axes[0].set_ylabel("Interaction Extinctions (%)")
axes[0].set_title("Interaction Extinctions vs. Protection")
axes[0].legend()

axes[1].set_xlabel("Fraction Protected")
axes[1].set_ylabel("Species Extinctions (%)")
axes[1].set_title("Species Extinctions vs. Protection")
axes[1].legend()

# Plot 3: Species extinctions vs interaction extinctions (scatter with medians)
ax = axes[2]
for strat in strategies:
    sub = grouped[grouped["strategy"] == strat].sort_values("ie_median")
    c = colors[strat]
    lab = labels[strat]
    ax.plot(sub["ie_median"], sub["se_median"], "o-", color=c, label=lab, markersize=4, linewidth=1.5)
    ax.fill_betweenx(sub["se_median"], sub["ie_q25"], sub["ie_q75"], color=c, alpha=0.1)
    ax.fill_between(sub["ie_median"], sub["se_q25"], sub["se_q75"], color=c, alpha=0.1)

ax.set_xlabel("Interaction Extinctions (%)")
ax.set_ylabel("Species Extinctions (%)")
ax.set_title("Species vs. Interaction Extinctions")
ax.legend()

for ax in axes:
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)

plt.tight_layout()
plt.savefig("protection_sweep_hotspot_plots.png", dpi=200, bbox_inches="tight")
plt.close()
print("Saved to protection_sweep_hotspot_plots.png")
