# Source code and data for "Risk-Limiting Audits for Parliamentary Majorities"

This repository contains the code, data and scripts needed to reproduce the
simulation results in the following paper:

Freestone J, Leung D, Vukcevic D (2026).
*Risk-Limiting Audits for Parliamentary Majorities*.
Accepted for the
[Eleventh International Joint Conference on Electronic Voting][evoteid]
(E-Vote-ID 2026), to be held in Oct 2026 in Estonia.

[evoteid]: https://e-vote-id.org/

The simulations were implemented in R.
Running times for the larger simulations are non-trivial
(see [Runtime expectations](#runtime-expectations) below).

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
├── india_2014_no_margin/             Section 4.2 — Indian audit
│   ├── run_india_audit_multi.R       Parliamentary audit (varies n_false)
│   ├── run_india_audit_multi_full.R  All-seats baseline (n_false = 0 only)
│   ├── run_simulations_parallel.sh   Driver: one job per replicate, R = 100
│   │                                 by default (used for the paper)
│   ├── run_simulations_full.sh       Driver: All-seats baseline, parallel,
│   │                                 R = 100 by default
│   └── plot_india_results.R          Results box plots (Figure 3)
│
└── two_party_plurality/
    ├── plot_paper_figure.R           Combined 2x3 figure for sim 1 + sim 2
    │                                 (Figure 2 — used in the paper)
    │
    ├── simulation_1_no_margin/       Scenario 1 — homogeneous two-candidate
    │   ├── sim_1.R                   Per-config simulator
    │   ├── run_simulations.sh        Driver: sweeps W, p_alice, n_false
    │   └── plot_*.R                  Per-scenario figures
    │                                 (results, history, workload)
    │
    └── simulation_2_no_margin/       Scenario 2 — heterogeneous two-candidate
        ├── sim_1.R
        ├── run_simulations.sh
        └── plot_*.R
```

## Software requirements

- **R ≥ 4.2** (tested on 4.4).
- R packages: `dplyr`, `readr`, `ggplot2`. The `qpdf` package is optional (used
  by some plot scripts to combine multi-page PDFs; the scripts fall back to the
  Ghostscript `gs` command if `qpdf` is unavailable).
  ```r
  install.packages(c("dplyr", "readr", "ggplot2", "qpdf"))
  ```
- **`zsh`** (the simulation driver scripts are zsh shell scripts; on macOS this
  is the default). Linux users with `bash` can replace `zsh run_simulations.sh`
  by `bash run_simulations.sh`; the scripts use `set -e` and basic process
  management only.

## Data

The Indian election scenario uses the publicly available 2014 Lok Sabha
election results published by the Election Commission of India (ECI). Both
data files (`eci-candidate-wise.csv` and `eci-constituency-wise.csv`) are
included under `code/data/india_2014/`. The synthetic two-candidate scenarios
generate their own ballot populations from the configurations in their
respective `sim_1.R` scripts; no external data are needed.

## Reproducing the paper

All commands below assume the working directory is the relevant simulation
folder (`cd` into it first). Output `.rds` files and per-config / per-rep logs
are written to the local `results/` (or `results_R/`) subfolder created on
the first run.

### Scenario 1 — homogeneous two-candidate (Section 4.1, Scenario 1)

```sh
cd code/two_party_plurality/simulation_1_no_margin
zsh run_simulations.sh
Rscript plot_results.R
Rscript plot_history.R
Rscript plot_workload.R
```

`run_simulations.sh` sweeps the configuration grid declared at the top of the
script: `W ∈ {51, 52, 60, 80}`, `p_alice ∈ {0.52, 0.55, 0.60}`,
`n_false ∈ {0, 3, 5}`, `R = 100` replicates per cell — 36 configurations
in total. Configurations run in parallel up to `MAX_JOBS = 18` at a time.
Edit those constants to change the grid size. The paper figure uses only
the `W = 60` slice; the other `W` values are available for sensitivity
checks.

### Scenario 2 — heterogeneous two-candidate (Section 4.1, Scenario 2)

```sh
cd code/two_party_plurality/simulation_2_no_margin
zsh run_simulations.sh
Rscript plot_results.R
Rscript plot_workload.R
```

Adds a sweep over the Beta concentration `kappa` (controlling heterogeneity
of true winning shares across seats) on top of Scenario 1's grid:
`W ∈ {51, 52, 60, 80}`, `p_mean ∈ {0.52, 0.55, 0.60}`,
`kappa ∈ {10, 30, 100}`, `n_false ∈ {0, 3, 5}`, `R = 100` — 108
configurations, with `MAX_JOBS = 18`. As above, the paper figure uses only
the `W = 60`, `kappa = 30` slice.

### Combined Scenario 1 + Scenario 2 figure (Figure 2)

After both Scenario 1 and Scenario 2 have run (so their `results/` folders
are populated), generate the bespoke 2x3 grid used in the paper:

```sh
cd code/two_party_plurality
Rscript plot_paper_figure.R
```

The script reads the `W = 60` slice from each scenario, with
`kappa = 30` for Scenario 2, and writes `paper_figure.pdf` in the same folder.

### Indian election (Section 4.2)

The Indian scenario is split across three driver scripts. For the paper we
use the two *parallel* drivers (`run_simulations_parallel.sh` and
`run_simulations_full.sh`), which dispatch one replicate per job and write a
separate `.rds` per replicate. This is the configuration that produced the
`R = 100` results in the paper. The older `run_simulations.sh` is retained
for compatibility with the earlier `R = 3` workflow.

**Parliamentary audit (varies `n_false`, parallel):**
```sh
cd code/india_2014_no_margin
zsh run_simulations_parallel.sh                  # R = 100  by default
# or override knobs:
# R=30 MAX_JOBS=24 zsh run_simulations_parallel.sh
```
Runs all parliamentary methods (Non-adaptive, Greedy `(a = 0)`,
Greedy `(a = 3)`, Filtered, Greedy Filtered `(a = 0)`,
Greedy Filtered `(a = 3)`, Reported top-r seats) at `n_false ∈ {0, 3, 5}`.
Each replicate is one Rscript invocation, so `n_false × R` jobs in total
(default `3 × 100 = 300`), capped to `MAX_JOBS = 18` concurrent workers.
Output goes to `results_R/`:
- `results_R/results_india_multi_nfalse{0,3,5}_rep{1..R}.rds`
- `results_R/log_nfalse{0,3,5}_rep{1..R}.txt`

**All-seats baseline (`r = |W|`, `n_false = 0` only):**
```sh
cd code/india_2014_no_margin
zsh run_simulations_full.sh                      # R = 100  by default
```
Runs only the All-seats baseline, `R = 100` replicates, parallel. The seed
formula matches `run_simulations_parallel.sh` so that for each rep the
sampled ballot stream is identical to the Non-adaptive method — a true paired
comparison. Output:
- `results_R/results_india_multi_full_rep{1..R}.rds`
- `results_R/log_full_rep{1..R}.txt`

**Figure (box plots, log scale, Figure 3):**
```sh
Rscript plot_india_results.R                     # reads results_R/ by default
# or:
# Rscript plot_india_results.R results_R comparison.pdf 100
```
Produces a 1x3 facet (one panel per `n_false`) of per-replicate box plots
on a log y-axis. *All seats* is shown only for `n_false = 0` (it tests a
true null otherwise).

The three driver scripts above are independent and can be launched
concurrently if hardware allows.

## Output

There are two output conventions:

- **Per-config** (the two-candidate Scenario 1 and Scenario 2 drivers,
  `run_simulations.sh`): each configuration runs all `R` replicates in a single
  Rscript call and writes one `.rds` per configuration, containing a data frame
  with `R` rows per method (one row per replicate). Files are written to
  `results/`.
- **Per-replicate** (the Indian drivers `run_simulations_parallel.sh` and
  `run_simulations_full.sh`, which run one replicate per job): one `.rds` per
  `(config, replicate)`, containing a data frame with one row per method for
  that replicate. Files are written to `results_R/`.

Each row carries the columns `t_eval` (total ballots drawn),
`t_round` (audit rounds used), `certified` (logical), and configuration
metadata. The `plot_*.R` scripts call `list.files(results_dir, pattern = "\\.rds$")`
and `rbind` everything they find, so they handle both conventions uniformly —
just point them at the relevant `results/` or `results_R/` folder.

## Runtime expectations

Approximate wall-clock times on a single modern desktop (using the parallel
drivers; per-job runtime in parentheses).

| Scenario | Walltime | Bottleneck |
|---|---:|---|
| Scenario 1 (R=100, MAX_JOBS=18)         | ~5 h | Greedy Filtered (`a = 3`) |
| Scenario 2 (R=100, MAX_JOBS=18)         | ~16 h | Greedy Filtered (`a = 3`) |
| India parliamentary (R=100, MAX_JOBS=18) | ~2 weeks (LONG) | Greedy Filtered / Filtered |
| India All-seats (R=100, MAX_JOBS=18)     | ~2 weeks (LONG) | Sampling all 282 BJP seats to individual certification |
