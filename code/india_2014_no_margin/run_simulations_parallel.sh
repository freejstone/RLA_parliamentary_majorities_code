#!/bin/zsh
# run_simulations_parallel.sh — Parallel India 2014 no-margin audit (higher R)
#
# Runs one replicate per job (R=1 inside each Rscript call) so that many reps
# can run concurrently. A polling job pool keeps at most MAX_JOBS workers busy:
# whenever a job finishes the next one is launched immediately.
#
# Output goes to a SEPARATE folder (RESULTS_DIR) so existing results/ is left
# untouched. Each job writes its own RDS file:
#   results_R/results_india_multi_nfalse<n>_rep<k>.rds
# and its own log:
#   results_R/log_nfalse<n>_rep<k>.txt
#
# The plot scripts (plot_results.R / plot_history.R / plot_workload.R) call
# list.files(results_dir, pattern = "\\.rds$") and rbind everything they find,
# so they will pick all per-rep files up automatically — just point them at
# results_R/.
#
# Usage:  zsh run_simulations_parallel.sh
#         (override knobs via environment, e.g.  R=30 MAX_JOBS=24 zsh ...)

set -e
cd "$(dirname "$0")"

# --- Tunable knobs (override via environment) ---
RESULTS_DIR="${RESULTS_DIR:-results_R}"
N_FALSE_VALS=(${N_FALSE_VALS:-0 3 5})
R="${R:-10}"             # replicates per n_false
MAX_JOBS="${MAX_JOBS:-18}"
BASE_SEED="${BASE_SEED:-1}"

mkdir -p "$RESULTS_DIR"

total=$(( ${#N_FALSE_VALS[@]} * R ))
echo "Launching $total jobs (${#N_FALSE_VALS[@]} n_false vals x R=$R reps)"
echo "Results dir: $RESULTS_DIR    MAX_JOBS=$MAX_JOBS"
echo

launched=0
for n_false in "${N_FALSE_VALS[@]}"; do
  for rep in $(seq 1 "$R"); do
    # Block until a worker slot frees up.
    while (( $(jobs -rp | wc -l) >= MAX_JOBS )); do sleep 0.5; done

    # Distinct seed per (n_false, rep). The 1000-step gap on n_false ensures
    # no overlap with the per-method seed offsets (max 5000) used inside the
    # R script.
    seed=$(( BASE_SEED + 100000 * (n_false + 1) + rep ))
    launched=$(( launched + 1 ))
    echo "[${launched}/${total}] launching: n_false=${n_false}  rep=${rep}  seed=${seed}"

    Rscript run_india_audit_multi.R \
      "$n_false" 1 "$RESULTS_DIR" "$seed" "$rep" \
      > "${RESULTS_DIR}/log_nfalse${n_false}_rep${rep}.txt" 2>&1 &
  done
done

echo
echo "All ${total} jobs dispatched. Waiting for completion..."
wait
echo "All done. RDS files in ${RESULTS_DIR}/"
