#!/bin/zsh
# run_simulations_full.sh — Parallel India 2014 full-audit (mirrors run_simulations_parallel.sh)
#
# Runs the full audit (Naive scheme with r_majority = W) one replicate per job,
# in a parallel pool. Uses the SAME seed formula as run_simulations_parallel.sh
# so that for each rep, the simulated population and ballot-draw stream match
# the Naive method in run_simulations_parallel.sh bit-for-bit. This makes the
# full audit a true paired comparison against the adaptive methods.
#
# n_false is forced to 0 (full audit only meaningful when no seats are falsely
# reported -- otherwise the null is true).
#
# Output: $RESULTS_DIR/results_india_multi_full_rep<k>.rds
#         $RESULTS_DIR/log_full_rep<k>.txt
#
# Usage:  zsh run_simulations_full.sh
#         (override knobs via environment, e.g.  R=30 MAX_JOBS=24 zsh ...)

set -e
cd "$(dirname "$0")"

# --- Tunable knobs (override via environment) ---
RESULTS_DIR="${RESULTS_DIR:-results_R}"
R="${R:-100}"            # replicates
MAX_JOBS="${MAX_JOBS:-18}"
BASE_SEED="${BASE_SEED:-1}"

mkdir -p "$RESULTS_DIR"

n_false=0  # full audit only meaningful for n_false = 0
total=$R
echo "Launching $total full-audit jobs (R=$R reps, n_false=$n_false)"
echo "Results dir: $RESULTS_DIR    MAX_JOBS=$MAX_JOBS"
echo

launched=0
for rep in $(seq 1 "$R"); do
  # Block until a worker slot frees up.
  while (( $(jobs -rp | wc -l) >= MAX_JOBS )); do sleep 0.5; done

  # Seed formula MUST match run_simulations_parallel.sh exactly so that, at
  # the same (n_false=0, rep), Full audit and Naive draw the same ballots.
  seed=$(( BASE_SEED + 100000 * (n_false + 1) + rep ))
  launched=$(( launched + 1 ))
  echo "[${launched}/${total}] launching: rep=${rep}  seed=${seed}"

  Rscript run_india_audit_multi_full.R \
    1 "$RESULTS_DIR" "$seed" "$rep" \
    > "${RESULTS_DIR}/log_full_rep${rep}.txt" 2>&1 &
done

echo
echo "All ${total} jobs dispatched. Waiting for completion..."
wait
echo "All done. RDS files in ${RESULTS_DIR}/"
