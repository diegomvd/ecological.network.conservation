#!/usr/bin/env python3
"""
Generate the Saltelli design matrix for the SR-vs-IR / EIR-vs-SR sensitivity analysis.

This is step (1) of the pipeline described in saltelli_sensitivity_plan.md: the design is
built OFFLINE with SALib and written to a CSV that the Scala `SaltelliRunner` consumes on
the cluster. Keeping the sampler in Python (rather than OpenMOLE) means the same `problem`
definition is reused verbatim by `analyze.py` for `sobol.analyze`, which is what makes the
indices trustworthy/auditable.

k = 8 continuous inputs. Ranges match saltelli_sensitivity_plan.md (2026-09-05 revision)
and saltelli_sensitivity.oms EXACTLY:

    connectanceMetaWeb                0.05 – 0.25
    epsilon                           0.5  – 5.0
    nPopulations                      5000 – 10000   (sampled continuous, rounded to Int by runner)
    medianHomeRange                   0.01 – 0.05
    extinctionProbabilityUnprotected  0.05 – 1.0
    fractionProtected                 0.05 – 0.45
    avgPopulationsPerTile             10   – 100      (sampled continuous, rounded to Int by runner)
    meanPatchesPerSpecies             1.0  – 10.0

Row count = N * (2k + 2)  with second-order indices (default), or N * (k + 2) without.
For N=1024, k=8:  second-order -> 18432 rows;  first+total only -> 10240 rows.

Usage:
    python gen_design.py --n 1024 --out ../design.csv
    python gen_design.py --n 1024 --no-second-order --out ../design.csv
"""
import argparse
import csv
import json
import sys

# SALib >= 1.4 layout; fall back to the legacy import path if needed.
try:
    from SALib.sample import sobol as sobol_sample
    _NEW_API = True
except ImportError:  # SALib < 1.4
    from SALib.sample import saltelli as sobol_sample
    _NEW_API = False


PROBLEM = {
    "num_vars": 8,
    "names": [
        "connectanceMetaWeb",
        "epsilon",
        "nPopulations",
        "medianHomeRange",
        "extinctionProbabilityUnprotected",
        "fractionProtected",
        "avgPopulationsPerTile",
        "meanPatchesPerSpecies",
    ],
    "bounds": [
        [0.05, 0.25],
        [0.5, 5.0],
        [5000.0, 10000.0],
        [0.01, 0.05],
        [0.05, 1.0],
        [0.05, 0.45],
        [10.0, 100.0],
        [1.0, 10.0],
    ],
}


def main():
    ap = argparse.ArgumentParser(description="Generate Saltelli design matrix (SALib).")
    ap.add_argument("--n", type=int, default=1024,
                    help="base sample size N (power of 2 recommended). Default 1024.")
    ap.add_argument("--out", default="design.csv", help="output design CSV path.")
    ap.add_argument("--no-second-order", action="store_true",
                    help="skip second-order indices: N*(k+2) rows instead of N*(2k+2).")
    ap.add_argument("--seed", type=int, default=12345,
                    help="RNG seed for the sampler (reproducibility).")
    ap.add_argument("--problem-out", default=None,
                    help="also write the SALib problem definition as JSON here "
                         "(analyze.py needs the SAME definition).")
    args = ap.parse_args()

    calc_second = not args.no_second_order

    # SALib's sobol/saltelli sampler. `calc_second_order` must match analyze.py.
    kwargs = dict(calc_second_order=calc_second)
    if _NEW_API:
        kwargs["seed"] = args.seed
    param_values = sobol_sample.sample(PROBLEM, args.n, **kwargs)

    n_rows = param_values.shape[0]
    expected = args.n * (2 * PROBLEM["num_vars"] + 2) if calc_second \
        else args.n * (PROBLEM["num_vars"] + 2)
    print(f"[gen_design] N={args.n}  k={PROBLEM['num_vars']}  "
          f"second_order={calc_second}  -> {n_rows} rows (expected {expected})",
          file=sys.stderr)

    with open(args.out, "w", newline="") as f:
        w = csv.writer(f)
        w.writerow(PROBLEM["names"])
        for row in param_values:
            w.writerow([repr(float(x)) for x in row])
    print(f"[gen_design] wrote {n_rows} design points -> {args.out}", file=sys.stderr)

    # Persist the problem + run config so analyze.py is guaranteed to use the same definition.
    meta = {
        "problem": PROBLEM,
        "N": args.n,
        "calc_second_order": calc_second,
        "n_rows": n_rows,
        "seed": args.seed,
        "salib_new_api": _NEW_API,
    }
    problem_out = args.problem_out or (args.out.rsplit(".", 1)[0] + ".problem.json")
    with open(problem_out, "w") as f:
        json.dump(meta, f, indent=2)
    print(f"[gen_design] wrote problem definition -> {problem_out}", file=sys.stderr)


if __name__ == "__main__":
    main()
