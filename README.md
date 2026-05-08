# Reproducibility code for the parliament-election audit paper

This repository contains the code, data, and scripts needed to reproduce the
simulation results in the paper. Everything is in `R`; running times for the
larger simulations are non-trivial (see "Runtime expectations" below).

## Contents

```
code/
├── functions/                        Core implementation (sourced by every script)
│   ├── functions.R                   ALPHA betting process, sampling schemes,
│   │                                 parliament-level e-process aggregator
│   └── helpers.R                     Input-validation helpers
│
├── data/
│   └── india_2014/                   Indian 2014 General Election ECI data
│       ├── eci-candidate-wise.csv
│       └── eci-constituency-wise.csv
│
├── india_2014_no_margin/             Section 5.2 — semi-simulated Indian audit
│   ├── run_india_audit_multi.R       Parliamentary audit (varies n_false)
│   ├── run_india_audit_multi_full.R  All-seats baseline (n_false = 0 only)
│   ├── run_simulations.sh            Driver: 3 parliamentary configs in parallel
│   └── run_simulations_full.sh       Driver: All-seats run
│
└── two_party_plurality/
    ├── simulation_1_no_margin/       Scenario 1 — homogeneous two-candidate
    │   ├── sim_1.R                   Per-config simulator
    │   ├── run_simulations.sh        Driver: sweeps W, p_alice, n_false
    │   └── plot_*.R                  Figures (results, history, workload)
    │
    └── simulation_2_no_margin/       Scenario 2 — heterogeneous two-candidate
        ├── sim_1.R
        ├── run_simulations.sh
        └── plot_*.R
```

## Software requirements

- **R ≥ 4.2** (tested on 4.4).
- R packages: `dplyr`, `readr`, `ggplot2`. The `qpdf` package is optional (used
  by the plot scripts to combine multi-page PDFs; the scripts fall back to the
  Ghostscript `gs` command if `qpdf` is unavailable).
  ```r
  install.packages(c("dplyr", "readr", "ggplot2", "qpdf"))
  ```
- **`zsh`** (the simulation driver scripts are zsh shell scripts; on macOS this
  is the default). Linux users with `bash` can replace `zsh run_simulations.sh`
  by `bash run_simulations.sh`; the scripts use `set -e` and basic process
  management only.

## Data

The Indian-election scenario uses the publicly available 2014 Lok Sabha
election results published by the Election Commission of India (ECI). Both
data files (`eci-candidate-wise.csv` and `eci-constituency-wise.csv`) are
included under `code/data/india_2014/`. The synthetic two-candidate and
multi-candidate scenarios generate their own ballot populations from the
configurations in their respective `sim_1.R` scripts; no external data is
needed.

## Reproducing the paper

All commands below assume the working directory is the relevant simulation
folder (`cd` into it first). Output `.rds` files and per-config logs are
written to the local `results/` subfolder created on the first run.

### Scenario 1 — homogeneous two-candidate (Section 5.1.1)

```sh
cd code/two_party_plurality/simulation_1_no_margin
zsh run_simulations.sh
Rscript plot_results.R
Rscript plot_history.R
Rscript plot_workload.R
```

`run_simulations.sh` sweeps the configuration grid declared at the top of the
script: `W ∈ {51, 52, 60, 80}`, `p_alice ∈ {0.52, 0.55, 0.60}`, `n_false ∈
{0, 3, 5}`, `R = 10` replicates per cell — 36 configurations in total.
Configurations run in parallel up to `MAX_JOBS = 18` at a time. Edit those
constants to change the grid size.

### Scenario 2 — heterogeneous two-candidate (Section 5.1.2)

```sh
cd code/two_party_plurality/simulation_2_no_margin
zsh run_simulations.sh
Rscript plot_results.R
Rscript plot_workload.R
```

Adds a second-tier sweep over the Beta concentration `kappa` (controlling
heterogeneity of true winning shares across seats) on top of Scenario 1's
grid: `W ∈ {51, 52, 60, 80}`, `p_mean ∈ {0.52, 0.55, 0.60}`, `kappa ∈ {10,
30, 100}`, `n_false ∈ {0, 3, 5}`, `R = 18` — 108 configurations, again with
`MAX_JOBS = 18`.

### Semi-simulated Indian election (Section 5.2)

The Indian scenario is split into two scripts because the *All-seats* baseline
takes very different running time from the parliamentary audit.

**Parliamentary audit (varies `n_false`):**
```sh
cd code/india_2014_no_margin
zsh run_simulations.sh
```
Runs all parliamentary methods (Non-adaptive, Greedy `(a=0)`, Greedy `(a=3)`,
Filtered, Greedy Filtered `(a=0)`, Greedy Filtered `(a=3)`, Reported top-r
seats) at `n_false ∈ {0, 3, 5}` with `R = 3` replicates each. Three separate
log files are produced: `results/log_nfalse{0,3,5}.txt`, plus the corresponding
`.rds` files.

**All-seats baseline (`r = |W|`, `n_false = 0` only):**
```sh
cd code/india_2014_no_margin
zsh run_simulations_full.sh
```
Runs only the All-seats baseline, with `R = 3` replicates. Output:
`results/log_full.txt` and `results/results_india_multi_full.rds`.

The two driver scripts are independent and can be launched concurrently if
hardware allows.

## Output

Every `sim_*.R` and `run_india_audit_multi*.R` script writes one `.rds` per
configuration containing a data frame with one row per `(method, replicate)`,
with columns `t_eval` (total ballots drawn), `t_round` (audit rounds used),
`certified` (logical), and metadata for the configuration. The `plot_*.R`
scripts read every `.rds` in `results/` and produce the figures/tables in
the paper.

## Runtime expectations

Approximate wall-clock times on a single modern desktop (single-threaded R):

| Scenario | Walltime | Bottleneck |
|---|---:|---|
| Scenario 1 | ~1–2 h | Greedy Filtered ($a = 3$) |
| Scenario 2 | ~3–6 h | Greedy Filtered ($a = 3$) |
| India parliamentary, all $n_{\mathrm{false}}$ | ~12–24 h per config | Filtered / Greedy Filtered |
| India All-seats | ~24–48 h | Sampling all 282 BJP seats to individual certification |

