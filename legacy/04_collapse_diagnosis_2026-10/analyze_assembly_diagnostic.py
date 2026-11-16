#!/usr/bin/env python3
"""Summary table of assembly_diagnostic.csv: one row per (config, c_meta, hr), means over seeds."""
import csv, sys
from collections import defaultdict
import numpy as np

CSV = sys.argv[1] if len(sys.argv) > 1 else "assembly_diagnostic.csv"
rows = list(csv.DictReader(open(CSV)))
order = []
g = defaultdict(lambda: defaultdict(list))
for r in rows:
    k = (r["config"], r["c_meta"], r["hr"])
    if k not in order: order.append(k)
    for c, v in r.items():
        try: g[k][c].append(float(v))
        except ValueError: pass
cols = [("assembled", "asm", "{:.0f}"), ("assembled_basal", "bas", "{:.0f}"), ("assembled_inter", "int", "{:.0f}"),
        ("assembled_top", "top", "{:.0f}")]
print(f"{'config':18} {'C':>4} {'HR':>5} | {'placed':>6} {'asm':>5} {'loss%':>5} {'basal':>5} {'inter':>5} {'top':>4} | "
      f"{'S':>4} {'L/S':>5} {'1prey':>5} | {'pop/tile':>8} | {'SRprim':>6} {'SRsec':>6} {'sec%':>5} {'casc':>5} | {'IR-SR sp':>8} {'IR-SR int':>9}")
for k in sorted(order, key=lambda k: (k[1], k[2], order.index(k))):
    m = {c: np.mean(v) for c, v in g[k].items()}
    loss = 100 * (1 - m["assembled"] / m["placed"])
    sec = m["SR_sp_ext_secondary"]; prim = m["SR_sp_ext_primary"]
    share = 100 * sec / (sec + prim) if sec + prim > 0 else 0
    print(f"{k[0]:18} {k[1]:>4} {k[2]:>5} | {m['placed']:6.0f} {m['assembled']:5.0f} {loss:5.0f} {m['assembled_basal']:5.0f} "
          f"{m['assembled_inter']:5.0f} {m['assembled_top']:4.0f} | {m['stable_S']:4.0f} {m['stable_L_per_S']:5.2f} "
          f"{m['stable_frac_single_prey']:5.2f} | {m['pops_per_occupied_tile_finest']:8.2f} | {prim:6.1f} {sec:6.1f} {share:5.0f} "
          f"{m['SR_nCascades']:5.0f} | {m['IR_sp_ext_total']-m['SR_sp_ext_total']:+8.2f} {m['IR_spint_ext_total']-m['SR_spint_ext_total']:+9.2f}")

# biomassPyramid calibration table (only when the CSV has the new columns).
if rows and "energetic_pruning_pct" in rows[0]:
    print("\nCalibration (targets: prune <= 20-30%, >= 120 species with inter+top present, L/S 2-4, cascade present but not total)")
    print(f"{'config':24} {'C':>4} {'HR':>5} | {'placed':>6} {'filt%':>5} {'prune%':>6} {'asm':>5} | {'sp':>4} {'spB':>4} {'spI':>4} "
          f"{'spT':>4} {'small':>5} | {'L/S':>5} {'1prey':>5} | {'SRprim':>6} {'SRsec':>6} {'casc':>5} | {'asm_s':>5}")
    for k in sorted(order, key=lambda k: (k[1], k[2], order.index(k))):
        m = {c: np.mean(v) for c, v in g[k].items()}
        print(f"{k[0]:24} {k[1]:>4} {k[2]:>5} | {m['placed']:6.0f} {m['spatial_filtering_pct']:5.1f} "
              f"{m['energetic_pruning_pct']:6.1f} {m['assembled']:5.0f} | {m['assembled_sp']:4.0f} {m['assembled_sp_basal']:4.0f} "
              f"{m['assembled_sp_inter']:4.0f} {m['assembled_sp_top']:4.0f} {m['small_species']:5.0f} | "
              f"{m['stable_L_per_S']:5.2f} {m['stable_frac_single_prey']:5.2f} | {m['SR_sp_ext_primary']:6.1f} "
              f"{m['SR_sp_ext_secondary']:6.1f} {m['SR_nCascades']:5.0f} | {m['assembly_seconds']:5.1f}")
