#!/bin/zsh
# run_simulations.sh — No-margin variant of simulation_1
#
# Bayesian / Greedy Bayesian use prior 0.51 (no reported margin);
# ALPHA's eta0 stays at 0.51 for every method;
# Top-r Naive ranks by the TRUE (oracle) margins.
# eps is dropped (no reported margins to corrupt).
#
# Parameters varied:
#   W        : number of reported winning seats
#   p_alice  : Alice's true winning share in correctly reported seats
#   n_false  : number of seats in W that Alice did NOT truly win (p = 0.48)
#
# Fixed:
#   S = 100 total seats, N = 5000 ballots/seat
#
# Usage:  zsh run_simulations.sh

set -e
cd "$(dirname "$0")"

RESULTS_DIR="results"
mkdir -p "$RESULTS_DIR"

# --- Tunable knobs ---
W_VALS=(51 52 60 80)
S=100
N=5000
R=10
MAX_JOBS=18

P_ALICE_VALS=(0.52 0.55 0.60)
N_FALSE_VALS=(0 3 5)

# --- Run simulations (parallel, batched) ---
seed=1
for W in "${W_VALS[@]}"; do
  for p_alice in "${P_ALICE_VALS[@]}"; do
    for n_false in "${N_FALSE_VALS[@]}"; do
      while (( $(jobs -rp | wc -l) >= MAX_JOBS )); do sleep 0.5; done
      echo "  Launching: W=$W  p_alice=$p_alice  n_false=$n_false  seed=$seed"
      Rscript sim_1.R "$W" "$S" "$N" "$p_alice" "$n_false" "$R" "$RESULTS_DIR" "$seed" &
      seed=$((seed + 1))
    done
  done
done
wait
echo "All simulations finished."

# --- Generate figures ---
echo "============================================"
echo "  Generating figures …"
echo "============================================"
Rscript plot_results.R "$RESULTS_DIR" "comparison.pdf"
Rscript plot_workload.R "$RESULTS_DIR" "workload.pdf"
Rscript plot_history.R "$RESULTS_DIR" "history.pdf"

echo ""
echo "Done!  See comparison.pdf, workload.pdf, history.pdf"
