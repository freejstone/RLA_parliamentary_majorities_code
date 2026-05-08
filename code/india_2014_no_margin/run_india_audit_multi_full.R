## ============================================================
## India 2014 multi-candidate audit — FULL audit only
## ============================================================
## Runs the Naive scheme with r_majority = W (every reported BJP seat
## must individually certify), versus run_india_audit_multi.R which
## tests the parliamentary majority r_majority = 272.
##
## Hardcoded n_false = 0, since the full audit's null (all |W| seats
## truly won) is true whenever n_false > 0 and would require a full
## recount with probability >= 1 - alpha.
##
## SEED-COMPATIBLE WITH run_india_audit_multi.R:
##   The Naive method in run_india_audit_multi.R uses seed_offset = 100L.
##   We use the same seed_offset here. Because lambda_naive issues the
##   same predictable lambda = (1, ..., 1) every round in both scripts,
##   the ballot-draw stream is identical for the same CLI seed --
##   the Full audit just continues sampling past the point where
##   the parliament-level Naive run stops.
##
## Usage: Rscript run_india_audit_multi_full.R <R> <results_dir> <seed>
## ============================================================

source("../functions/functions.R")
source("../functions/helpers.R")

library(dplyr)
library(readr)

build_plurality_assorter <- function(L) {
  J <- L - 1L
  mat <- matrix(0.5, nrow = J, ncol = L)
  mat[, 1] <- 1.0
  for (j in seq_len(J)) {
    mat[j, j + 1L] <- 0.0
  }
  rownames(mat) <- paste0("A_vs_C", 2:L)
  colnames(mat) <- paste0("C", 1:L)
  mat
}

simulate_multi_audit <- function(seats,
                                 r_majority,
                                 alpha = 0.05,
                                 keep_history = FALSE,
                                 lambda_fun = function(t_round, seats) rep(1.0, length(seats)),
                                 mu0 = 0.5,
                                 u = 1.0,
                                 eta_mode = "trunc_shrinkage",
                                 eta0 = 0.51,
                                 d = 200,
                                 c = NULL,
                                 max_t_eval = Inf,
                                 verbose = FALSE) {
  stopif_pos(alpha, "alpha")
  log_thresh <- log(1 / alpha)

  W <- length(seats)
  total_N <- sum(vapply(seats, function(s) s$N, integer(1)))

  hist_logMr   <- if (keep_history) numeric(0) else NULL
  hist_t_round <- if (keep_history) integer(0) else NULL
  hist_t_eval  <- if (keep_history) integer(0) else NULL

  t_eval <- 0L
  certified <- FALSE
  final_t_round <- 0L
  last_verbose_time <- proc.time()[3]

  repeat {
    final_t_round <- final_t_round + 1L
    t_round <- final_t_round

    lambdas <- lambda_fun(t_round, seats)
    lambdas <- as.integer(lambdas >= 0.5)

    rems <- vapply(seats, function(s) s$rem, integer(1))
    lambdas[rems <= 0L] <- 0L

    seats_to_sample <- which(lambdas == 1L)
    if (length(seats_to_sample) == 0L) break

    for (idx in seats_to_sample) {
      out <- draw_from_seat_general(seats[[idx]])
      seats[[idx]] <- out$seat
      if (any(is.na(out$x_vec))) next

      eta0_i <- if (length(eta0) > 1L) eta0[idx] else eta0
      seats[[idx]] <- seat_update_ALPHA_general(
        seats[[idx]], x_vec = out$x_vec,
        mu0 = mu0, u = u,
        lambda = 1.0,
        eta_mode = eta_mode,
        eta0 = eta0_i, d = d, c = c
      )

      t_eval <- t_eval + 1L
      logMr <- log_Mr_from_seats(seats, r_majority = r_majority)

      if (keep_history) {
        hist_logMr[length(hist_logMr) + 1L]     <- logMr
        hist_t_round[length(hist_t_round) + 1L] <- t_round
        hist_t_eval[length(hist_t_eval) + 1L]   <- t_eval
      }

      if (logMr >= log_thresh) {
        certified <- TRUE
        break
      }
      if (t_eval >= max_t_eval) break
    }

    if (certified || t_eval >= max_t_eval) break

    if (verbose) {
      now <- proc.time()[3]
      if ((now - last_verbose_time) > 30) {
        total_drawn <- sum(vapply(seats, function(s) s$k, integer(1)))
        n_cert <- sum(vapply(seats, function(s) all(s$logM == Inf), logical(1)))
        cat(sprintf("\r    round %d | %d/%d ballots (%.1f%%) | %d/%d seats certified",
                    t_round, total_drawn, total_N,
                    100 * total_drawn / total_N, n_cert, W))
        flush.console()
        last_verbose_time <- now
      }
    }
  }
  if (verbose) cat("\r")

  logMr <- log_Mr_from_seats(seats, r_majority = r_majority)

  list(
    stop = certified,
    t_round = final_t_round,
    t_eval = t_eval,
    logMr = logMr,
    Mr = exp(logMr),
    per_seat_draws = vapply(seats, function(s) s$k, integer(1)),
    history = if (keep_history) list(
      logMr = hist_logMr,
      t_round = hist_t_round, t_eval = hist_t_eval
    ) else NULL
  )
}

replicate_multi_audits <- function(R,
                                   seat_type_probs,
                                   N_vec,
                                   assorter_matrix,
                                   r_majority,
                                   alpha = 0.05,
                                   keep_history = FALSE,
                                   lambda_fun = function(t_round, seats) rep(1.0, length(seats)),
                                   mu0 = 0.5,
                                   u = 1.0,
                                   eta_mode = "trunc_shrinkage",
                                   eta0 = 0.51,
                                   d = 200,
                                   c = NULL,
                                   max_t_eval = Inf,
                                   verbose = FALSE) {
  stopif_pos(R, "R")
  R <- as.integer(R)
  W <- length(seat_type_probs)

  out <- vector("list", R)
  t0_all <- proc.time()[3]
  for (i in seq_len(R)) {
    seats <- lapply(seq_len(W), function(j) {
      new_seat_general(
        id = j,
        N = N_vec[j],
        type_probs = seat_type_probs[[j]],
        assorter_matrix = assorter_matrix
      )
    })
    out[[i]] <- simulate_multi_audit(
      seats = seats,
      r_majority = r_majority,
      alpha = alpha,
      keep_history = keep_history,
      lambda_fun = lambda_fun,
      mu0 = mu0, u = u,
      eta_mode = eta_mode,
      eta0 = eta0, d = d, c = c,
      max_t_eval = max_t_eval,
      verbose = verbose
    )
    if (verbose) {
      elapsed <- proc.time()[3] - t0_all
      cat(sprintf("\r  rep %d/%d  t_eval=%-8d cert=%-5s  (%.0fs elapsed)",
                  i, R, out[[i]]$t_eval, out[[i]]$stop, elapsed))
      flush.console()
    }
  }
  if (verbose) cat("\n")
  out
}

## ============================================================
## Load India 2014 data — same as run_india_audit_multi.R
## ============================================================
cand <- read_csv("../../data/india_2014/eci-candidate-wise.csv",
                 show_col_types = FALSE)

seat_info <- cand %>%
  group_by(State, Constituency, `Constituency-code`) %>%
  arrange(desc(Votes)) %>%
  summarise(
    winner_party = first(Party),
    total_votes  = sum(Votes),
    n_candidates = n(),
    vote_shares  = list(Votes / sum(Votes)),
    .groups = "drop"
  )

bjp_seats <- seat_info %>%
  filter(grepl("Bharatiya Janata", winner_party)) %>%
  mutate(mu2 = vapply(vote_shares,
                      function(v) v[1] / (v[1] + v[2]), numeric(1))) %>%
  arrange(mu2)

W <- nrow(bjp_seats)
N_vec <- as.integer(bjp_seats$total_votes)
L_vec <- bjp_seats$n_candidates
L_max <- max(L_vec)

cat(sprintf("BJP seats: W=%d\n", W))
cat(sprintf("Candidates per seat (L): min=%d  median=%d  max=%d\n",
            min(L_vec), as.integer(median(L_vec)), L_max))
cat(sprintf("N (total votes per seat): min=%d  median=%d  max=%d\n",
            min(N_vec), as.integer(median(N_vec)), max(N_vec)))

## ============================================================
## Parliament parameters
## ============================================================
S <- 543L
r_majority <- W   # FULL AUDIT: every reported BJP seat must certify

cat(sprintf("|S|=%d  |W|=%d  r=%d (FULL audit)  k=%d\n",
            S, W, r_majority, W - r_majority + 1L))

## ============================================================
## Build shared assorter matrix and padded type_probs
## ============================================================
ASSORTER_MATRIX <- build_plurality_assorter(L_max)
cat(sprintf("Assorter matrix: %d assertions x %d ballot types (L_max)\n",
            nrow(ASSORTER_MATRIX), ncol(ASSORTER_MATRIX)))

pad_probs <- function(v, L_target) {
  c(v, rep(0, L_target - length(v)))
}

actual_type_probs <- lapply(seq_len(W), function(j) {
  pad_probs(bjp_seats$vote_shares[[j]], L_max)
})

## ============================================================
## Command-line arguments
## Usage: Rscript run_india_audit_multi_full.R <R> <results_dir> <seed>
## ============================================================
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3L) {
  stop("Usage: Rscript run_india_audit_multi_full.R <R> <results_dir> <seed>")
}
R           <- as.integer(args[1])
results_dir <- args[2]
seed        <- as.integer(args[3])

n_false <- 0L  # full audit only meaningful when no seats are falsely reported

cat(sprintf("Config: n_false=%d (hardcoded, full audit)  R=%d  seed=%d\n",
            n_false, R, seed))

## ============================================================
## Audit parameters (match run_india_audit_multi.R)
## ============================================================
alpha      <- 0.05
mu0        <- 0.5
u          <- 1.0
eta_mode   <- "trunc_shrinkage"
eta0       <- 0.51
d          <- 200
c_tuning   <- NULL

## ============================================================
## Helper: collect results
## ============================================================
collect <- function(res_list, method_name, r_used, nf) {
  df <- data.frame(
    method    = method_name,
    eps       = 0,
    n_false   = nf,
    W = W, S = S,
    r         = r_used,
    t_eval    = vapply(res_list, `[[`, numeric(1), "t_eval"),
    t_round   = vapply(res_list, `[[`, numeric(1), "t_round"),
    certified = vapply(res_list, `[[`, logical(1), "stop"),
    stringsAsFactors = FALSE
  )
  df
}

## ============================================================
## Build true population for n_false = 0 (no swap)
## ============================================================
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)

cat(sprintf("\n========== Full audit, n_false = %d ==========\n", n_false))

seat_type_probs_true <- actual_type_probs

total_ballots <- sum(N_vec)

run_method <- function(lambda_fun, r_maj, seed_offset,
                       eta0_arg = eta0, d_arg = d) {
  set.seed(seed + seed_offset)
  replicate_multi_audits(
    R = R, seat_type_probs = seat_type_probs_true, N_vec = N_vec,
    assorter_matrix = ASSORTER_MATRIX,
    r_majority = r_maj, alpha = alpha, keep_history = FALSE,
    lambda_fun = lambda_fun,
    mu0 = mu0, u = u, eta_mode = eta_mode,
    eta0 = eta0_arg, d = d_arg, c = c_tuning,
    max_t_eval = total_ballots,
    verbose = TRUE
  )
}

## -- Full audit (Naive scheme with r_majority = W) --
## seed_offset = 100L matches the Naive method in run_india_audit_multi.R,
## so the ballot-draw stream is identical for the same CLI seed.
t0 <- proc.time()
cat("[Full audit] "); flush.console()
res <- run_method(lambda_naive, r_majority, 100L)
cat(sprintf("median t_eval = %.0f  (%.1fs)\n",
            median(vapply(res, `[[`, numeric(1), "t_eval")),
            (proc.time() - t0)[3])); flush.console()
results <- collect(res, "Full audit", r_majority, n_false)

outfile <- file.path(results_dir, "results_india_multi_full.rds")
saveRDS(results, outfile)
cat(sprintf("\nSaved %s (%d rows)\n", outfile, nrow(results)))

## Quick summary
cat("\n===== Summary =====\n")
for (m in unique(results$method)) {
  rows <- results[results$method == m, ]
  med <- median(rows$t_eval)
  cert_rate <- mean(rows$certified)
  cat(sprintf("%-20s  median_t_eval = %8.0f  cert = %.0f%%\n",
              m, med, cert_rate * 100))
}
cat("\n===== Done =====\n")
