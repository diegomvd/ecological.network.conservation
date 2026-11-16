#!/usr/bin/env bash
# ─────────────────────────────────────────────────────────────────────────────
# Submit the Saltelli SLURM job array on Drago.
# RUN THIS ON THE CLUSTER (after upload.sh has staged the files), from drago/ :
#     ./submit.sh
#
# It computes the array size from the design length and CHUNK_SIZE, then submits
# with the partition/account/qos/resources from config.sh. A dry run prints the
# sbatch command without submitting:
#     ./submit.sh --dry-run
# Resubmit only specific chunks (e.g. after a merge reports missing rows):
#     ./submit.sh --array 3,7,12
# ─────────────────────────────────────────────────────────────────────────────
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck disable=SC1091
source "${HERE}/config.sh"

DRY_RUN=0
ARRAY_OVERRIDE=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    --dry-run) DRY_RUN=1; shift ;;
    --array)   ARRAY_OVERRIDE="$2"; shift 2 ;;
    *) echo "unknown arg: $1"; exit 2 ;;
  esac
done

DESIGN_PATH="${REMOTE_BASE}/${DESIGN_NAME}"
[[ -f "${DESIGN_PATH}" ]] || { echo "ERROR: design not found at ${DESIGN_PATH} (run upload.sh / gen_design.py first)"; exit 1; }
[[ -f "${REMOTE_BASE}/${JAR_NAME}" ]] || { echo "ERROR: JAR not found at ${REMOTE_BASE}/${JAR_NAME}"; exit 1; }

mkdir -p "${REMOTE_OUT}" "${REMOTE_LOGS}"

# Design rows = lines minus header.
N_ROWS=$(( $(wc -l < "${DESIGN_PATH}") - 1 ))
N_TASKS=$(( (N_ROWS + CHUNK_SIZE - 1) / CHUNK_SIZE ))   # ceil
LAST_IDX=$(( N_TASKS - 1 ))

if [[ -n "${ARRAY_OVERRIDE}" ]]; then
  ARRAY_SPEC="${ARRAY_OVERRIDE}%${SLURM_ARRAY_THROTTLE}"
else
  ARRAY_SPEC="0-${LAST_IDX}%${SLURM_ARRAY_THROTTLE}"
fi

echo "design rows      : ${N_ROWS}"
echo "chunk size       : ${CHUNK_SIZE}"
echo "array tasks      : ${N_TASKS}  (indices 0..${LAST_IDX})"
echo "array spec       : ${ARRAY_SPEC}"

# Build the sbatch invocation. Optional flags only added when set.
SBATCH_ARGS=(
  --partition="${SLURM_PARTITION}"
  --array="${ARRAY_SPEC}"
  --cpus-per-task="${SLURM_CPUS_PER_TASK}"
  --mem="${SLURM_MEM}"
  --time="${SLURM_TIME}"
  --chdir="${REMOTE_BASE}"
  --export=ALL,REMOTE_BASE="${REMOTE_BASE}",REMOTE_OUT="${REMOTE_OUT}",JAR_NAME="${JAR_NAME}",DESIGN_NAME="${DESIGN_NAME}",N_SEEDS="${N_SEEDS}",CHUNK_SIZE="${CHUNK_SIZE}",JAVA_XMX="${JAVA_XMX}",JAVA_MODULE="${JAVA_MODULE}"
)
[[ -n "${SLURM_ACCOUNT}" ]] && SBATCH_ARGS+=( --account="${SLURM_ACCOUNT}" )
[[ -n "${SLURM_QOS}"     ]] && SBATCH_ARGS+=( --qos="${SLURM_QOS}" )

SBATCH_ARGS+=( "${HERE}/run_array.sbatch" )

if [[ "${DRY_RUN}" -eq 1 ]]; then
  echo; echo "DRY RUN — would submit:"; echo "  sbatch ${SBATCH_ARGS[*]}"
  exit 0
fi

echo; echo "submitting..."
sbatch "${SBATCH_ARGS[@]}"
echo "monitor with:  squeue -u ${DRAGO_USER}   |   tail -f logs/saltelli_*_*.out"
