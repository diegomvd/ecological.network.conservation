# Saltelli / Sobol sensitivity run on Drago CSIC

Native SLURM execution of the SR-vs-IR / EIR-vs-SR sensitivity analysis, replacing the
abandoned OpenMOLE workflow (`saltelli_sensitivity.oms`). Same model, same 5-seed averaging,
same 17 output columns — just driven by a plain job array instead of OpenMOLE.

**Pipeline (4 stages):**

```
[laptop]  gen_design.py ──► design.csv  ──►  sbt assembly ──► econetcons-saltelli.jar
                                    │                              │
                                    └──────────► upload.sh ◄───────┘   (rsync to Drago)
                                                     │
[Drago]                                          submit.sh ──► SLURM array ──► out/results_*.csv
                                                                                   │
[laptop]  merge_results.py (verify complete) ◄── (rsync back) ◄────────────────────┘
              │
              └──► analyze.py ──► sobol/  (S1, ST, S2 + bootstrap CIs)
```

Everything cluster-specific lives in **`config.sh`** as placeholders — edit that one file
(username, host, paths, partition, Java module) and the rest just works.

---

## What's here

| File | Where it runs | Purpose |
|------|---------------|---------|
| `config.sh` | both | All placeholders (SSH, paths, SLURM, Java). **Edit this first.** |
| `scripts/gen_design.py` | laptop | Build the Saltelli design matrix with SALib → `design.csv` + `.problem.json`. |
| `upload.sh` | laptop | rsync the JAR, design, and scripts to Drago. |
| `run_array.sbatch` | Drago | One array task = one JVM processing a chunk of design rows → its own shard. |
| `submit.sh` | Drago | Computes array size from the design and submits with `config.sh` settings. |
| `scripts/merge_results.py` | laptop | Concatenate shards **and verify every design row is present exactly once**. |
| `scripts/analyze.py` | laptop | Sobol S1/ST/S2 with bootstrap CIs, using the saved `.problem.json`. |
| `requirements.txt` | laptop | `pip install -r requirements.txt` (SALib, numpy). Cluster needs no Python. |

The model JAR is built from the repo by `sbt assembly` (main class `econetcons.SaltelliRunner`)
and lands at `target/scala-3.3.3/econetcons-saltelli.jar`.

---

## Step-by-step

### 0. One-time: edit `config.sh`
Fill in every `<<< SET ME >>>`: `DRAGO_USER`, `DRAGO_LOGIN_HOST`, `REMOTE_BASE`,
`SLURM_PARTITION`, and `JAVA_MODULE` (find it on Drago with
`module avail 2>&1 | grep -i -E 'java|jdk'`; leave empty if `java` is already on PATH).
Recommended: set up an SSH key so `upload.sh` doesn't prompt for a password:
```bash
ssh-copy-id <DRAGO_USER>@<DRAGO_LOGIN_HOST>
```

### 1. Build the JAR (laptop, in repo root)
```bash
sbt assembly
# → target/scala-3.3.3/econetcons-saltelli.jar  (self-contained; only needs a JRE)
```

### 2. Generate the design matrix (laptop)
```bash
python3 -m venv .venv && source .venv/bin/activate      # optional
pip install -r drago/requirements.txt
python3 drago/scripts/gen_design.py --n 1024 --out drago/design.csv
#   N=1024, k=8, second-order  → 18,432 design points  (drago/design.csv)
#   also writes drago/design.problem.json  (analyze.py reuses it)
```
Cheaper pilot first (screen low-index inputs before the full run):
```bash
python3 drago/scripts/gen_design.py --n 64 --out drago/design_pilot.csv
```
First+total order only (no second-order → fewer rows, N*(k+2)=10,240 at N=1024):
```bash
python3 drago/scripts/gen_design.py --n 1024 --no-second-order --out drago/design.csv
```

### 3. Upload to Drago (laptop)
```bash
cd drago && ./upload.sh
```

### 4. Submit the array (on Drago)
```bash
ssh <DRAGO_USER>@<DRAGO_LOGIN_HOST>
cd <REMOTE_BASE>
./submit.sh --dry-run     # sanity: prints array size + the exact sbatch command
./submit.sh               # submit
squeue -u <DRAGO_USER>    # watch
```
Each task writes `out/results_<taskid>.csv`. Chunk size and array throttle are in `config.sh`
(`CHUNK_SIZE`, `SLURM_ARRAY_THROTTLE`).

### 5. Pull results back + verify completeness (laptop)
```bash
rsync -avP <DRAGO_USER>@<DRAGO_LOGIN_HOST>:<REMOTE_BASE>/out/ drago/out/
python3 drago/scripts/merge_results.py \
    --design drago/design.csv --results-dir drago/out --out drago/results_merged.csv
```
If it reports **INCOMPLETE**, it writes `results_merged.csv.missing_rows.txt`. Map those row
indices back to chunk ids (`chunk = row // CHUNK_SIZE`) and resubmit just those on Drago:
```bash
./submit.sh --array 3,7,12
```
Then re-pull and re-merge (merge is idempotent — re-runs overwrite by `row_index`).

### 6. Sobol analysis (laptop)
```bash
python3 drago/scripts/analyze.py \
    --results drago/results_merged.csv \
    --problem drago/design.problem.json \
    --out-dir drago/sobol
# → drago/sobol/sobol_summary.csv  + per-output S1/ST (and S2) tables
```
By default it analyzes the 6 curated contrasts. Add raw anchors with
`--outputs dSpInt_IR_SR SR_spint_ext_total IR_spint_ext_total ...`.

---

## Sizing / budget

- Measured ~12 s per model run × `N_SEEDS=5` ≈ **~60 s per design row**.
- Full design N=1024 (second-order) = 18,432 rows → ~307 CPU-hours total.
- With `CHUNK_SIZE=200` → 93 array tasks, ~3.3 h each. At `SLURM_ARRAY_THROTTLE=100`
  concurrent, wall time ≈ one chunk (~3–4 h) if the partition grants 100 slots; longer if
  throttled. Tune `CHUNK_SIZE` so `CHUNK_SIZE × 60 s` sits comfortably under `SLURM_TIME`.
- Memory: measured peak heap ~320 MB/run; `SLURM_MEM=4G` / `JAVA_XMX=4g` is generous.

## Design ↔ code invariants (must stay in sync)

- The 8 input columns and their ranges in `scripts/gen_design.py` **exactly** match
  `SaltelliRunner.InputCols` and the ranges in `saltelli_sensitivity_plan.md`.
- Fixed factors (in `SaltelliRunner`, not swept): `numberOfSpecies=200`, `bodySizeSigma=1.0`,
  `preyPerPredator=0.0`, `observationNoise=falsePositiveRate=0.0`.
- `calc_second_order` must be identical between `gen_design.py` and `analyze.py` — this is
  guaranteed by passing the saved `design.problem.json` to `analyze.py`.
- The runner echoes the 8 inputs into every output row alongside `row_index`, so the merged
  CSV is self-contained; `analyze.py` sorts by `row_index` to restore SALib sample order.
