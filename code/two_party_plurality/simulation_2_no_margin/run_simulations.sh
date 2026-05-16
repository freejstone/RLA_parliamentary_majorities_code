#!/bin/zsh
# run_simulations.sh — No-margin variant of simulation_2 (heterogeneous margins)
#
# Bayesian / Greedy Bayesian use prior 0.51 (no reported margin);
# ALPHA's eta0 stays at 0.51 for every method;
# Top-r Naive ranks by the TRUE (oracle) margins.
# eps is dropped (no reported margins to corrupt).
#
# Parameters varied:
#   W        : number of reported winning seats
#   p_mean   : mean of the Beta distribution for truly-won seats
#   p_spread : concentration parameter (kappa); higher = less heterogeneity
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
R=100
MAX_JOBS=16

P_MEAN_VALS=(0.52 0.55 0.60)
P_SPREAD_VALS=(10 30 100)       # kappa: 10=very heterogeneous, 100=nearly homogeneous
N_FALSE_VALS=(0 3 5)

# --- Run simulations (parallel, batched) ---
seed=1
for W in "${W_VALS[@]}"; do
  for p_mean in "${P_MEAN_VALS[@]}"; do
    for p_spread in "${P_SPREAD_VALS[@]}"; do
      for n_false in "${N_FALSE_VALS[@]}"; do
        while (( $(jobs -rp | wc -l) >= MAX_JOBS )); do sleep 0.5; done
        echo "  Launching: W=$W  p_mean=$p_mean  kappa=$p_spread  n_false=$n_false  seed=$seed"
        Rscript sim_1.R "$W" "$S" "$N" "$p_mean" "$p_spread" "$n_false" "$R" "$RESULTS_DIR" "$seed" &
        seed=$((seed + 1))
      done
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

echo ""
echo "Done!  See comparison.pdf"
