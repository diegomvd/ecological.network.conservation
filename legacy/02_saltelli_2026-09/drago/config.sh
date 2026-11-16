#!/usr/bin/env bash
# ─────────────────────────────────────────────────────────────────────────────
# Drago CSIC — Saltelli sensitivity run configuration.
#
# EDIT the PLACEHOLDER values below to match your Drago account/allocation, then
# `source` this file (the helper scripts source it automatically). Everything the
# submit/run scripts need is here, so nothing else has hard-coded cluster details.
#
# Placeholders you MUST set are marked  <<< SET ME >>>.
# ─────────────────────────────────────────────────────────────────────────────

# ── SSH / login (used only by upload.sh from your laptop) ────────────────────
DRAGO_USER="<<< SET ME: your Drago username >>>"
DRAGO_LOGIN_HOST="<<< SET ME: e.g. drago.csic.es or the login node hostname >>>"
# Password is NEVER stored here. Use SSH keys (recommended) or you'll be prompted.
# To set up a key once:  ssh-copy-id ${DRAGO_USER}@${DRAGO_LOGIN_HOST}

# ── Remote paths (on Drago) ──────────────────────────────────────────────────
# Project root on the cluster where the JAR, design and scripts live.
REMOTE_BASE="<<< SET ME: e.g. /home/${DRAGO_USER}/saltelli or a /scratch path >>>"
# Where per-task result shards are written (should be on fast/scratch storage).
REMOTE_OUT="${REMOTE_BASE}/out"
# Where SLURM stdout/stderr logs go.
REMOTE_LOGS="${REMOTE_BASE}/logs"

# ── SLURM allocation ─────────────────────────────────────────────────────────
SLURM_PARTITION="<<< SET ME: partition name, e.g. generic / long / batch >>>"
SLURM_ACCOUNT=""                 # optional: set if your site requires -A <account>
SLURM_QOS=""                     # optional: set if your site requires --qos
# Per-array-task resources. One task = one JVM processing a CHUNK of design rows,
# single-threaded model, so 1 core is right; bump memory if you see OOM.
SLURM_CPUS_PER_TASK=1
SLURM_MEM="4G"                   # measured peak heap ~320 MB; 4G is generous headroom
SLURM_TIME="24:00:00"            # per-task wall limit (see CHUNK_SIZE below)
# Cap on how many array tasks run at once (respect the partition/QOS limits).
# Drago array size / concurrency limits: <<< CHECK with `scontrol show config | grep -i MaxArray` >>>
SLURM_ARRAY_THROTTLE=100         # the "%N" in --array=0-M%N

# ── Java on the cluster ──────────────────────────────────────────────────────
# If Drago uses environment modules, set the module name to load a JDK >= 17.
# Find it with:  module avail 2>&1 | grep -i -E 'java|jdk|openjdk'
JAVA_MODULE="<<< SET ME or leave empty: e.g. Java/21.0.2 or openjdk/17 >>>"
# If java is already on PATH without a module, leave JAVA_MODULE="" .
JAVA_XMX="4g"                    # JVM heap; keep <= SLURM_MEM

# ── Run parameters ───────────────────────────────────────────────────────────
JAR_NAME="econetcons-saltelli.jar"
DESIGN_NAME="design.csv"         # produced by scripts/gen_design.py
N_SEEDS=5                        # seed-averaging per design point (plan: 5)

# Rows per array task. Total array size = ceil(n_design_rows / CHUNK_SIZE).
# Sizing guide (measured ~12 s/row-run × N_SEEDS seeds ≈ ~60 s per design row):
#   CHUNK_SIZE=200  ->  ~200*60s ≈ 3.3 h per task  (well under a 24h SLURM_TIME)
# With N=1024, k=8, second-order design = 18432 rows -> 18432/200 ≈ 93 tasks.
# Tune so (CHUNK_SIZE * ~60s) comfortably fits SLURM_TIME with margin.
CHUNK_SIZE=200
