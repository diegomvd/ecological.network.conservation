#!/usr/bin/env python3
"""
Merge the per-task result shards into a single results CSV, and VERIFY completeness:
every design row must appear exactly once. This is the safety gate before Sobol analysis
(step 3(c) of the plan: "verify every design row appears exactly once").

Each SLURM array task writes results_<taskid>.csv with a `row_index` column referring back
to the 0-based row of the design matrix. This script:
  - concatenates all shards,
  - checks for missing / duplicate row_index values against the design length,
  - writes a single deduplicated, row_index-sorted CSV,
  - exits non-zero (unless --allow-incomplete) if anything is missing.

Usage:
    python merge_results.py --design ../design.csv \
                            --results-dir ../out \
                            --out ../results_merged.csv
"""
import argparse
import csv
import glob
import os
import sys


def count_design_rows(design_path):
    with open(design_path) as f:
        return sum(1 for _ in f) - 1  # minus header


def main():
    ap = argparse.ArgumentParser(description="Merge + verify Saltelli result shards.")
    ap.add_argument("--design", required=True, help="the design.csv used for the run.")
    ap.add_argument("--results-dir", required=True,
                    help="directory containing results_*.csv shards.")
    ap.add_argument("--glob", default="results_*.csv", help="shard filename glob.")
    ap.add_argument("--out", required=True, help="merged output CSV path.")
    ap.add_argument("--allow-incomplete", action="store_true",
                    help="write the merged file and exit 0 even if rows are missing.")
    args = ap.parse_args()

    n_design = count_design_rows(args.design)
    shards = sorted(glob.glob(os.path.join(args.results_dir, args.glob)))
    if not shards:
        print(f"[merge] ERROR: no shards match {args.glob} in {args.results_dir}",
              file=sys.stderr)
        sys.exit(2)

    header = None
    rows_by_index = {}
    duplicates = []
    for shard in shards:
        with open(shard, newline="") as f:
            r = csv.reader(f)
            try:
                h = next(r)
            except StopIteration:
                print(f"[merge] WARNING: empty shard {shard}", file=sys.stderr)
                continue
            if header is None:
                header = h
            elif h != header:
                print(f"[merge] ERROR: header mismatch in {shard}", file=sys.stderr)
                sys.exit(2)
            idx_col = header.index("row_index")
            for row in r:
                if not row:
                    continue
                ri = int(row[idx_col])
                if ri in rows_by_index:
                    duplicates.append(ri)
                rows_by_index[ri] = row  # last write wins (idempotent re-runs)

    present = set(rows_by_index)
    expected = set(range(n_design))
    missing = sorted(expected - present)
    extra = sorted(present - expected)

    print(f"[merge] design rows expected : {n_design}", file=sys.stderr)
    print(f"[merge] distinct rows present : {len(present)}", file=sys.stderr)
    print(f"[merge] duplicate row_index   : {len(set(duplicates))}", file=sys.stderr)
    if extra:
        print(f"[merge] WARNING: {len(extra)} out-of-range row_index (design mismatch?): "
              f"{extra[:10]}{'...' if len(extra) > 10 else ''}", file=sys.stderr)
    if missing:
        preview = missing[:20]
        print(f"[merge] MISSING {len(missing)} rows: {preview}"
              f"{'...' if len(missing) > 20 else ''}", file=sys.stderr)

    os.makedirs(os.path.dirname(os.path.abspath(args.out)) or ".", exist_ok=True)
    with open(args.out, "w", newline="") as f:
        w = csv.writer(f)
        w.writerow(header)
        for ri in sorted(rows_by_index):
            w.writerow(rows_by_index[ri])
    print(f"[merge] wrote {len(rows_by_index)} rows -> {args.out}", file=sys.stderr)

    if missing and not args.allow_incomplete:
        print("[merge] INCOMPLETE: re-run the missing chunks before analysis "
              "(or pass --allow-incomplete).", file=sys.stderr)
        # Emit the missing row indices to a file to make resubmission easy.
        miss_path = args.out + ".missing_rows.txt"
        with open(miss_path, "w") as mf:
            mf.write("\n".join(str(m) for m in missing) + "\n")
        print(f"[merge] missing row indices -> {miss_path}", file=sys.stderr)
        sys.exit(1)

    print("[merge] COMPLETE: every design row present exactly once.", file=sys.stderr)


if __name__ == "__main__":
    main()
