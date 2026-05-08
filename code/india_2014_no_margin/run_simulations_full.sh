#!/bin/zsh
# run_simulations_full.sh — Full-audit variant of India 2014 multi-candidate audit
#
# Runs run_india_audit_multi_full.R, which uses the Naive scheme with
# r_majority = W (every reported BJP seat must individually certify).
# n_false is hardcoded to 0 inside the R script.
#
# Usage:  zsh run_simulations_full.sh

set -e
cd "$(dirname "$0")"

RESULTS_DIR="results"
mkdir -p "$RESULTS_DIR"

R=3
seed=1

echo "  Launching: full audit  R=$R  seed=$seed"
Rscript run_india_audit_multi_full.R "$R" "$RESULTS_DIR" "$seed" \
  > "${RESULTS_DIR}/log_full.txt" 2>&1

echo "Done."
