#!/usr/bin/env bash
# ─────────────────────────────────────────────────────────────────────────────
# Stage everything to Drago. RUN THIS ON YOUR LAPTOP, from drago/ :
#     ./upload.sh
#
# Ships: the fat JAR, the design CSV (+ problem.json), the sbatch/submit/config
# scripts, and the Python analysis scripts. Uses rsync over SSH. You'll be
# prompted for a password unless you've set up an SSH key (recommended once with
# `ssh-copy-id`). No credentials are stored anywhere.
# ─────────────────────────────────────────────────────────────────────────────
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO="$(cd "${HERE}/.." && pwd)"
# shellcheck disable=SC1091
source "${HERE}/config.sh"

JAR_SRC="${REPO}/target/scala-3.3.3/${JAR_NAME}"
DESIGN_SRC="${HERE}/${DESIGN_NAME}"

[[ -f "${JAR_SRC}" ]]    || { echo "ERROR: ${JAR_SRC} missing — run 'sbt assembly' first."; exit 1; }
[[ -f "${DESIGN_SRC}" ]] || { echo "ERROR: ${DESIGN_SRC} missing — run scripts/gen_design.py first."; exit 1; }

REMOTE="${DRAGO_USER}@${DRAGO_LOGIN_HOST}"

echo "creating remote dirs under ${REMOTE_BASE} ..."
ssh "${REMOTE}" "mkdir -p '${REMOTE_BASE}' '${REMOTE_OUT}' '${REMOTE_LOGS}' '${REMOTE_BASE}/scripts'"

echo "uploading JAR ($(du -h "${JAR_SRC}" | cut -f1)) ..."
rsync -avP "${JAR_SRC}" "${REMOTE}:${REMOTE_BASE}/${JAR_NAME}"

echo "uploading design + problem definition ..."
rsync -avP "${DESIGN_SRC}" "${REMOTE}:${REMOTE_BASE}/${DESIGN_NAME}"
PROBLEM_SRC="${DESIGN_SRC%.csv}.problem.json"
[[ -f "${PROBLEM_SRC}" ]] && rsync -avP "${PROBLEM_SRC}" "${REMOTE}:${REMOTE_BASE}/"

echo "uploading run scripts ..."
rsync -avP "${HERE}/run_array.sbatch" "${HERE}/submit.sh" "${HERE}/config.sh" \
      "${REMOTE}:${REMOTE_BASE}/"
rsync -avP "${HERE}/scripts/" "${REMOTE}:${REMOTE_BASE}/scripts/"
ssh "${REMOTE}" "chmod +x '${REMOTE_BASE}/submit.sh'"

echo
echo "done. next, ON DRAGO:"
echo "  ssh ${REMOTE}"
echo "  cd ${REMOTE_BASE} && ./submit.sh --dry-run   # sanity check"
echo "  ./submit.sh                                  # submit the array"
