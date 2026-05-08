## ============================================================
## India 2014 multi-candidate audit, no-margin variant
## ============================================================
## Identical to ../india_2014/run_india_audit_multi.R except:
##   * Bayesian / Greedy Bayesian use the WINNER-TILTED Dirichlet prior
##     with PER-SEAT L_c (= bjp_seats$n_candidates[j]):
##       p_winner = 1 - (L_c - 1) * 0.98 / L_c
##       p_loser  = 0.98 / L_c                  for each of L_c-1 real opponents
##       0                                      for padded slots beyond L_c
##     By construction every real assertion's prior mean equals 0.51.
##     Padded slots have zero prior mass mirroring zero true ballots.
##   * ALPHA's eta0 stays at the SHANGRLA default 0.51 (scalar)
##   * Top-r Naive ranks by the TRUE (oracle) assertion means
##   * Misclassification (eps) is dropped; eps positional argument removed
##
## Usage: Rscript run_india_audit_multi.R <n_false> <R> <results_dir> <seed>
## ============================================================

source("../functions/functions.R")
source("../functions/helpers.R")

library(dplyr)
library(readr)

## ============================================================
## Helpers (same as multi_candidate_plurality/simulation_1_no_margin)
## ============================================================
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

# Per-seat L_c-aware winner-tilted prior.
# Real candidates 1..L_c get the L_c-formula; padded slots L_c+1..L_max get 0.
make_no_margin_prior_padded <- function(L_c, L_max) {
  pw <- 1 - (L_c - 1) * 0.98 / L_c
  pl <- 0.98 / L_c
  v <- c(pw, rep(pl, L_c - 1), rep(0, L_max - L_c))
  v
}

## ============================================================
## Main simulator
## ============================================================
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
## Load India 2014 data — one row per (seat, candidate) with Votes
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
r_majority <- 272L
cat(sprintf("|S|=%d  |W|=%d  r=%d  k=%d\n",
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
## Per-seat L_c-aware winner-tilted no-margin prior
## ============================================================
seat_priors <- lapply(seq_len(W), function(j) {
  make_no_margin_prior_padded(L_vec[j], L_max)
})

## Sanity: report prior for the median-L_c seat.
median_L <- as.integer(median(L_vec))
sample_idx <- which(L_vec == median_L)[1]
sample_prior <- seat_priors[[sample_idx]]
cat(sprintf("No-margin prior for L_c=%d (seat %s):\n",
            L_vec[sample_idx], bjp_seats$Constituency[sample_idx]))
cat(sprintf("  p_winner=%.4f  p_loser=%.4f  (%d real losers + %d padded zeros)\n",
            sample_prior[1], sample_prior[2], L_vec[sample_idx] - 1, L_max - L_vec[sample_idx]))
sample_assertion_means <- as.numeric(ASSORTER_MATRIX %*% sample_prior)
cat(sprintf("  prior assertion means: real range=[%.4f, %.4f]; padded all=%.4f\n",
            min(sample_assertion_means[seq_len(L_vec[sample_idx] - 1L)]),
            max(sample_assertion_means[seq_len(L_vec[sample_idx] - 1L)]),
            sample_assertion_means[L_vec[sample_idx]]))

## ============================================================
## Command-line arguments (eps removed)
## Usage: Rscript run_india_audit_multi.R <n_false> <R> <results_dir> <seed>
## ============================================================
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 4L) {
  stop("Usage: Rscript run_india_audit_multi.R <n_false> <R> <results_dir> <seed>")
}
n_false     <- as.integer(args[1])
R           <- as.integer(args[2])
results_dir <- args[3]
seed        <- as.integer(args[4])

cat(sprintf("Config: n_false=%d  R=%d  seed=%d\n", n_false, R, seed))

## ============================================================
## Audit parameters
## ============================================================
alpha      <- 0.05
mu0        <- 0.5
u          <- 1.0
eta_mode   <- "trunc_shrinkage"
eta0       <- 0.51
d          <- 200
c_tuning   <- NULL
conc_val   <- 200L
tau_val    <- 0.1

## ============================================================
## True (oracle) assertion means for Top-r Naive ranking
## ============================================================
true_assertion_means <- vapply(actual_type_probs, function(p) {
  min(as.numeric(ASSORTER_MATRIX %*% p))
}, numeric(1))
cat(sprintf("True assertion means: range=[%.4f, %.4f]\n",
            min(true_assertion_means), max(true_assertion_means)))

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
## Build true population for this n_false configuration
## ============================================================
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)

cat(sprintf("\n========== n_false = %d ==========\n", n_false))

seat_type_probs_true <- actual_type_probs

if (n_false > 0L) {
  false_idx <- seq_len(n_false)
  cat(sprintf("False seats (tightest): %s\n",
              paste(sprintf("%s (mu2=%.4f)",
                            bjp_seats$Constituency[false_idx],
                            bjp_seats$mu2[false_idx]),
                    collapse = ", ")))
  for (j in false_idx) {
    p <- seat_type_probs_true[[j]]
    p[c(1L, 2L)] <- p[c(2L, 1L)]
    seat_type_probs_true[[j]] <- p
  }
}

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

results <- NULL

## -- Method 1: Naive --
t0 <- proc.time()
cat("[Naive] "); flush.console()
res <- run_method(lambda_naive, r_majority, 100L)
cat(sprintf("median t_eval = %.0f  (%.1fs)\n",
            median(vapply(res, `[[`, numeric(1), "t_eval")),
            (proc.time() - t0)[3])); flush.console()
results <- collect(res, "Naive", r_majority, n_false)

## -- Method 2: Greedy (a=0) --
t0 <- proc.time()
cat("[Greedy] "); flush.console()
res <- run_method(make_lambda_greedy(r_majority, alpha, a = 0L),
                  r_majority, 1000L)
cat(sprintf("median t_eval = %.0f  (%.1fs)\n",
            median(vapply(res, `[[`, numeric(1), "t_eval")),
            (proc.time() - t0)[3])); flush.console()
results <- rbind(results, collect(res, "Greedy", r_majority, n_false))

## -- Method 3: Greedy (a = 3) --
a_greedy3 <- 3L
t0 <- proc.time()
cat(sprintf("[Greedy a=%d] ", a_greedy3)); flush.console()
res <- run_method(make_lambda_greedy(r_majority, alpha, a = a_greedy3),
                  r_majority, 2000L)
cat(sprintf("median t_eval = %.0f  (%.1fs)\n",
            median(vapply(res, `[[`, numeric(1), "t_eval")),
            (proc.time() - t0)[3])); flush.console()
results <- rbind(results, collect(res, "Greedy (a=3)", r_majority, n_false))

## -- Method 4: Bayesian (no margin) --
t0 <- proc.time()
cat("[Bayesian] "); flush.console()
res <- run_method(
  make_lambda_bayesian(r_majority, alpha,
                       seat_proportions = seat_priors,
                       assorter_values = ASSORTER_MATRIX,
                       conc = conc_val, tau = tau_val),
  r_majority, 3000L,
  eta0_arg = eta0, d_arg = d)
cat(sprintf("median t_eval = %.0f  (%.1fs)\n",
            median(vapply(res, `[[`, numeric(1), "t_eval")),
            (proc.time() - t0)[3])); flush.console()
results <- rbind(results, collect(res, "Bayesian", r_majority, n_false))

## -- Method 5: Greedy Bayesian (no margin) --
t0 <- proc.time()
cat("[Greedy Bayesian] "); flush.console()
res <- run_method(
  make_lambda_greedy_bayesian(r_majority, alpha, a = 0L,
                              seat_proportions = seat_priors,
                              assorter_values = ASSORTER_MATRIX,
                              conc = conc_val, tau = tau_val),
  r_majority, 4000L,
  eta0_arg = eta0, d_arg = d)
cat(sprintf("median t_eval = %.0f  (%.1fs)\n",
            median(vapply(res, `[[`, numeric(1), "t_eval")),
            (proc.time() - t0)[3])); flush.console()
results <- rbind(results, collect(res, "Greedy Bayesian", r_majority, n_false))

## -- Method 5b: Greedy Bayesian (a=3) (no margin) --
t0 <- proc.time()
cat(sprintf("[Greedy Bayesian a=%d] ", a_greedy3)); flush.console()
res <- run_method(
  make_lambda_greedy_bayesian(r_majority, alpha, a = a_greedy3,
                              seat_proportions = seat_priors,
                              assorter_values = ASSORTER_MATRIX,
                              conc = conc_val, tau = tau_val),
  r_majority, 4500L,
  eta0_arg = eta0, d_arg = d)
cat(sprintf("median t_eval = %.0f  (%.1fs)\n",
            median(vapply(res, `[[`, numeric(1), "t_eval")),
            (proc.time() - t0)[3])); flush.console()
results <- rbind(results, collect(res, "Greedy Bayesian (a=3)", r_majority, n_false))

## -- Method 6: Top-r Naive — ranks by TRUE (oracle) assertion means --
top_r_idx <- order(true_assertion_means, decreasing = TRUE)[seq_len(r_majority)]
t0 <- proc.time()
cat("[Top-r Naive] "); flush.console()
res <- run_method(make_lambda_top_r_fallback(top_r_idx),
                  r_majority, 5000L,
                  eta0_arg = eta0, d_arg = d)
cat(sprintf("median t_eval = %.0f  (%.1fs)\n",
            median(vapply(res, `[[`, numeric(1), "t_eval")),
            (proc.time() - t0)[3])); flush.console()
results <- rbind(results, collect(res, "Top-r Naive", r_majority, n_false))

outfile <- file.path(results_dir,
                     sprintf("results_india_multi_nfalse%d.rds", n_false))
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
