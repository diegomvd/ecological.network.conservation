#!/usr/bin/env python3
"""
Sobol analysis of the merged Saltelli results (step 4 of the plan): compute first-order (S1)
and total-order (ST) indices — plus second-order (S2) if the design was built with them — for
each analyzed output, with bootstrap confidence intervals.

Uses the SAME SALib `problem` definition written by gen_design.py (loaded from the
.problem.json) so the analysis is guaranteed consistent with the sample.

Primary analyzed outputs (curated contrasts; see saltelli_sensitivity_plan.md):
    dSp_IR_SR, dSpInt_IR_SR, dRobust_IR_SR,        # naive IR vs SR  (primary)
    dSp_EIR_SR, dSpInt_EIR_SR, dRobust_EIR_SR      # EIR vs SR       (secondary)
Raw anchors are present in the CSV too and can be analyzed by passing --outputs.

IMPORTANT: SALib requires the results to be in the EXACT SALLIB SAMPLE ORDER. The merged CSV
is sorted by row_index (== design row order == sample order), so we sort by row_index here to
be safe before extracting the Y vector.

Usage:
    python analyze.py --results ../results_merged.csv \
                      --problem ../design.problem.json \
                      --out-dir ../sobol
"""
import argparse
import json
import os
import sys

import numpy as np

try:
    from SALib.analyze import sobol as sobol_analyze
except ImportError as e:
    print(f"[analyze] ERROR importing SALib: {e}", file=sys.stderr)
    sys.exit(2)


DEFAULT_OUTPUTS = [
    "dSp_IR_SR", "dSpInt_IR_SR", "dRobust_IR_SR",
    "dSp_EIR_SR", "dSpInt_EIR_SR", "dRobust_EIR_SR",
]


def load_csv(path):
    import csv
    with open(path, newline="") as f:
        r = csv.reader(f)
        header = next(r)
        rows = [row for row in r if row]
    return header, rows


def main():
    ap = argparse.ArgumentParser(description="Sobol analysis of merged Saltelli results.")
    ap.add_argument("--results", required=True, help="merged results CSV (from merge_results.py).")
    ap.add_argument("--problem", required=True, help="the .problem.json from gen_design.py.")
    ap.add_argument("--out-dir", default="sobol", help="output directory for per-output CSVs.")
    ap.add_argument("--outputs", nargs="*", default=DEFAULT_OUTPUTS,
                    help="which output columns to analyze (default: the 6 curated contrasts).")
    ap.add_argument("--resamples", type=int, default=1000,
                    help="bootstrap resamples for confidence intervals.")
    args = ap.parse_args()

    with open(args.problem) as f:
        meta = json.load(f)
    problem = meta["problem"]
    calc_second = meta.get("calc_second_order", True)
    n_rows = meta.get("n_rows")

    header, rows = load_csv(args.results)
    col = {name: i for i, name in enumerate(header)}
    if "row_index" not in col:
        print("[analyze] ERROR: results CSV has no row_index column.", file=sys.stderr)
        sys.exit(2)

    # Sort by row_index to guarantee SALib sample order.
    rows.sort(key=lambda r: int(r[col["row_index"]]))

    if n_rows is not None and len(rows) != n_rows:
        print(f"[analyze] ERROR: results have {len(rows)} rows but the design expects "
              f"{n_rows}. Run merge_results.py and fix missing rows first.", file=sys.stderr)
        sys.exit(1)

    os.makedirs(args.out_dir, exist_ok=True)
    names = problem["names"]

    summary_path = os.path.join(args.out_dir, "sobol_summary.csv")
    with open(summary_path, "w") as summ:
        summ.write("output,parameter,S1,S1_conf,ST,ST_conf\n")

        for out_name in args.outputs:
            if out_name not in col:
                print(f"[analyze] WARNING: output '{out_name}' not in results header; skipping.",
                      file=sys.stderr)
                continue
            Y = np.array([float(r[col[out_name]]) for r in rows], dtype=float)
            if not np.isfinite(Y).all():
                print(f"[analyze] WARNING: non-finite values in {out_name}; "
                      "replacing with column mean.", file=sys.stderr)
                Y = np.nan_to_num(Y, nan=np.nanmean(Y))

            Si = sobol_analyze.analyze(
                problem, Y,
                calc_second_order=calc_second,
                num_resamples=args.resamples,
                print_to_console=False,
            )

            # Per-output detailed CSV.
            out_csv = os.path.join(args.out_dir, f"sobol_{out_name}.csv")
            with open(out_csv, "w") as f:
                f.write("parameter,S1,S1_conf,ST,ST_conf\n")
                for i, p in enumerate(names):
                    f.write(f"{p},{Si['S1'][i]:.6f},{Si['S1_conf'][i]:.6f},"
                            f"{Si['ST'][i]:.6f},{Si['ST_conf'][i]:.6f}\n")
                    summ.write(f"{out_name},{p},{Si['S1'][i]:.6f},{Si['S1_conf'][i]:.6f},"
                               f"{Si['ST'][i]:.6f},{Si['ST_conf'][i]:.6f}\n")

            if calc_second and "S2" in Si:
                s2_csv = os.path.join(args.out_dir, f"sobol_{out_name}_S2.csv")
                with open(s2_csv, "w") as f:
                    f.write("param_i,param_j,S2,S2_conf\n")
                    for i in range(len(names)):
                        for j in range(i + 1, len(names)):
                            f.write(f"{names[i]},{names[j]},"
                                    f"{Si['S2'][i][j]:.6f},{Si['S2_conf'][i][j]:.6f}\n")

            # Rank inputs by ST for a quick console readout.
            order = np.argsort(-Si["ST"])
            ranked = ", ".join(f"{names[i]}(ST={Si['ST'][i]:.3f})" for i in order[:4])
            print(f"[analyze] {out_name}: top-4 by ST -> {ranked}", file=sys.stderr)

    print(f"[analyze] wrote per-output CSVs + summary -> {args.out_dir}/", file=sys.stderr)
    print(f"[analyze] summary: {summary_path}", file=sys.stderr)


if __name__ == "__main__":
    main()
