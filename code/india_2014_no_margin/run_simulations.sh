#!/bin/zsh
# run_simulations.sh — No-margin variant of India 2014 multi-candidate audit
#
# Bayesian / Greedy Bayesian use per-seat winner-tilted Dirichlet prior
# (every assertion mean = 0.51 by construction);
# ALPHA's eta0 stays at 0.51 for every method;
# Top-r Naive ranks by the TRUE (oracle) assertion means.
# eps is dropped (no reported proportions to corrupt).
#
# Usage:  zsh run_simulations.sh

set -e
cd "$(dirname "$0")"

RESULTS_DIR="results"
mkdir -p "$RESULTS_DIR"

# --- Tunable knobs ---
N_FALSE_VALS=(0 3 5)
R=3
MAX_JOBS=3

# --- Launch all 3 configurations in parallel ---
seed=1
for n_false in "${N_FALSE_VALS[@]}"; do
  while (( $(jobs -rp | wc -l) >= MAX_JOBS )); do sleep 0.5; done
  echo "  Launching: n_false=$n_false  R=$R  seed=$seed"
  Rscript run_india_audit_multi.R "$n_false" "$R" "$RESULTS_DIR" "$seed" \
    > "${RESULTS_DIR}/log_nfalse${n_false}.txt" 2>&1 &
  seed=$((seed + 1))
done

echo "All 3 jobs launched. Waiting for completion..."
wait
echo "All done."
