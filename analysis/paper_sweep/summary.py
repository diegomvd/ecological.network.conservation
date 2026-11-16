#!/usr/bin/env python3
"""Summaries of paper_sweep.csv (SR vs IR, emergent assembly). Outcomes are % of post-stabilization baseline;
contrasts IR-SR in percentage points (negative = IR loses less)."""
import sys, numpy as np, pandas as pd
from paths import SWEEP, CELLSTATS
CSV = sys.argv[1] if len(sys.argv) > 1 else SWEEP
d = pd.read_csv(CSV)
pd.set_option("display.width", 220); pd.set_option("display.max_columns", 40)
def se(x): return x.std(ddof=1) / np.sqrt(len(x))

print("== assembled communities (one per C, seed) ==")
a = d.drop_duplicates(["c_meta", "seed"])
print(a.groupby("c_meta")[["assembled_S", "assembled_L_per_S", "assembled_connectance", "assembled_frac_basal",
      "assembled_frac_top", "assembled_diet_breadth", "assembled_pops", "SR_baseline_sp", "SR_baseline_spint"]].mean().round(3))

print("\n== levels (SR / IR), mean over all ==")
for k in ["sp_ext_total", "spint_ext_total", "sp_ext_secondary", "spint_ext_secondary", "nCascades"]:
    print(f"  {k:22s} SR {d['SR_'+k].mean():7.2f}  IR {d['IR_'+k].mean():7.2f}")

def tab(by, cols=("dSp_IR_SR", "dSpInt_IR_SR")):
    g = d.groupby(by)
    out = pd.concat({c: g[c].mean() for c in cols} | {c + "_se": g[c].apply(se) for c in cols}, axis=1)
    return out[[x for c in cols for x in (c, c + "_se")]].round(2)

print("\n== by tile size x C (over frac, pExt) ==");  print(tab(["tile_size", "c_meta"]).unstack("c_meta"))
print("\n== by tile size x pExt ==");                 print(tab(["tile_size", "p_ext"]).unstack("p_ext"))
d["frac_bin"] = pd.cut(d.frac, [0, .12, .2, .28, .41], labels=["0.05-0.12", "0.12-0.2", "0.2-0.28", "0.28-0.4"])
print("\n== by tile size x budget ==");               print(tab(["tile_size", "frac_bin"]).unstack("frac_bin"))

# cell-level classification: cell = (C, t, frac, pExt), 50 seeds
cell = d.groupby(["c_meta", "tile_size", "frac", "p_ext"]).agg(
    dSp=("dSp_IR_SR", "mean"), dSp_se=("dSp_IR_SR", se), dI=("dSpInt_IR_SR", "mean"), dI_se=("dSpInt_IR_SR", se)).reset_index()
def sig(m, s): return np.where(m < -2 * s, -1, np.where(m > 2 * s, 1, 0))
cell["sSp"], cell["sI"] = sig(cell.dSp, cell.dSp_se), sig(cell.dI, cell.dI_se)
lab = {(-1, -1): "IR both", (-1, 0): "IR sp", (0, -1): "IR ints", (0, 0): "tie", (1, -1): "tradeoff (IR ints, SR sp)",
       (-1, 1): "tradeoff (IR sp, SR ints)", (1, 0): "SR sp", (0, 1): "SR ints", (1, 1): "SR both"}
cell["cls"] = [lab[(a_, b_)] for a_, b_ in zip(cell.sSp, cell.sI)]
print(f"\n== cell classification (|mean|>2SE, 50 seeds), {len(cell)} cells ==")
print(pd.crosstab(cell.tile_size, cell.cls))
print(pd.crosstab([cell.c_meta], cell.cls))
cell.to_csv(CELLSTATS, index=False)

print("\n== protected-set overlap / ranking agreement by tile ==")
print(d.groupby("tile_size")[["pair_SR_IR_jaccard_protected", "pair_SR_IR_rankCorr_spearman", "SR_cover_species",
      "IR_cover_species", "SR_cover_ints", "IR_cover_ints"]].mean().round(3))
print("\n== primary vs secondary contributions to the contrast (pp) by tile ==")
for k in ["sp_ext_primary", "sp_ext_secondary", "spint_ext_primary", "spint_ext_secondary"]:
    d["d_" + k] = d["IR_" + k] - d["SR_" + k]
print(d.groupby("tile_size")[["d_sp_ext_primary", "d_sp_ext_secondary", "d_spint_ext_primary", "d_spint_ext_secondary"]].mean().round(2))
print("\n== species contrast by trophic group / body quartile (pp) by tile ==")
for k in ["sp_ext_basal", "sp_ext_intermediate", "sp_ext_top", "sp_ext_bodyQ1", "sp_ext_bodyQ4"]:
    d["d_" + k] = d["IR_" + k] - d["SR_" + k]
print(d.groupby("tile_size")[[c for c in d.columns if c.startswith("d_sp_ext_") and "ary" not in c]].mean().round(2))
