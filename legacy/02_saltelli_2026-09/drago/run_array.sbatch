#!/usr/bin/env bash
#SBATCH --job-name=saltelli
#SBATCH --output=logs/saltelli_%A_%a.out
#SBATCH --error=logs/saltelli_%A_%a.err
# NOTE: partition / account / qos / array / resources are injected by submit.sh
#       from drago/config.sh via sbatch CLI flags, so this file stays portable.
#       You CAN hard-code them here instead if you prefer a self-contained script.
# ─────────────────────────────────────────────────────────────────────────────
# One array task = one JVM that processes design rows
#     [ SLURM_ARRAY_TASK_ID * CHUNK_SIZE , (SLURM_ARRAY_TASK_ID+1) * CHUNK_SIZE )
# and writes its OWN shard  out/results_<taskid>.csv  (no shared-file contention).
# Re-running a task simply overwrites its shard, so the array is idempotent.
# ─────────────────────────────────────────────────────────────────────────────
set -euo pipefail

# These are exported into the job env by submit.sh (--export), with safe defaults here.
: "${REMOTE_BASE:?REMOTE_BASE not set}"
: "${REMOTE_OUT:=${REMOTE_BASE}/out}"
: "${JAR_NAME:=econetcons-saltelli.jar}"
: "${DESIGN_NAME:=design.csv}"
: "${N_SEEDS:=5}"
: "${CHUNK_SIZE:=200}"
: "${JAVA_XMX:=4g}"
: "${JAVA_MODULE:=}"

cd "${REMOTE_BASE}"
mkdir -p "${REMOTE_OUT}"

# Load a JDK via environment modules if configured.
if [[ -n "${JAVA_MODULE}" ]]; then
  # shellcheck disable=SC1091
  source /etc/profile.d/modules.sh 2>/dev/null || true
  module load "${JAVA_MODULE}"
fi
echo "[task ${SLURM_ARRAY_TASK_ID}] java: $(command -v java)"
java -version

TASK_ID="${SLURM_ARRAY_TASK_ID}"
START=$(( TASK_ID * CHUNK_SIZE ))
END=$(( START + CHUNK_SIZE ))          # runner clamps END to the design length
OUT_FILE="${REMOTE_OUT}/results_${TASK_ID}.csv"

echo "[task ${TASK_ID}] rows [${START}, ${END})  seeds=${N_SEEDS}  -> ${OUT_FILE}"
echo "[task ${TASK_ID}] host=$(hostname)  start=$(date -Is)"

java -Xmx"${JAVA_XMX}" \
     -cp "${REMOTE_BASE}/${JAR_NAME}" \
     econetcons.SaltelliRunner \
     --design "${REMOTE_BASE}/${DESIGN_NAME}" \
     --out    "${OUT_FILE}" \
     --start  "${START}" \
     --end    "${END}" \
     --seeds  "${N_SEEDS}"

echo "[task ${TASK_ID}] done=$(date -Is)"
