source("../../functions/functions.R")
source("../../functions/helpers.R")
## ============================================================
## Two-candidate plurality, heterogeneous margins, no-margin variant
## ============================================================

## Usage: Rscript sim_1.R W S N p_mean p_spread n_false R output_dir [seed]


simulate_party_audit <- function(seats,
                                 r_majority,
                                 alpha = 0.05,
                                 keep_history = FALSE,
                                 lambda_fun = function(t_round, seats) rep(1.0, length(seats)),
                                 mu0 = 0.5,
                                 u = 1.0,
                                 eta_mode = "trunc_shrinkage",
                                 eta0 = 0.51,
                                 d = 200,
                                 c = NULL) {
  stopif_pos(alpha, "alpha")

  log_thresh <- log(1 / alpha)

  hist_logMr   <- if (keep_history) numeric(0) else NULL
  hist_seat    <- if (keep_history) integer(0) else NULL
  hist_t_round <- if (keep_history) integer(0) else NULL
  hist_t_eval  <- if (keep_history) integer(0) else NULL

  t_eval <- 0L
  certified <- FALSE
  final_t_round <- 0L

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
      out <- draw_from_seat(seats[[idx]])
      seats[[idx]] <- out$seat
      if (is.na(out$x)) next

      eta0_i <- if (length(eta0) > 1L) eta0[idx] else eta0
      seats[[idx]] <- seat_update_ALPHA(
        seats[[idx]], x = out$x,
        mu0 = mu0, u = u,
        lambda = 1.0,
        eta_mode = eta_mode,
        eta0 = eta0_i, d = d, c = c
      )

      t_eval <- t_eval + 1L

      logMr <- log_Mr_from_seats(seats, r_majority = r_majority)

      if (keep_history) {
        hist_logMr[t_eval]   <- logMr
        hist_seat[t_eval]    <- idx
        hist_t_round[t_eval] <- t_round
        hist_t_eval[t_eval]  <- t_eval
      }

      if (logMr >= log_thresh) {
        certified <- TRUE
        break
      }
    }

    if (certified) break
  }

  logMr <- log_Mr_from_seats(seats, r_majority = r_majority)

  list(
    stop = certified,
    t_round = final_t_round,
    t_eval = t_eval,
    logMr = logMr,
    Mr = exp(logMr),
    per_seat_draws = vapply(seats, function(s) s$k, integer(1)),
    history = if (keep_history) list(
      logMr = hist_logMr, seat = hist_seat,
      t_round = hist_t_round, t_eval = hist_t_eval
    ) else NULL
  )
}

replicate_audits <- function(R,
                             seat_specs,
                             r_majority,
                             alpha = 0.05,
                             keep_history = FALSE,
                             lambda_fun = function(t_round, seats) rep(1.0, length(seats)),
                             mu0 = 0.5,
                             u = 1.0,
                             eta_mode = "trunc_shrinkage",
                             eta0 = 0.51,
                             d = 200,
                             c = NULL) {
  stopif_pos(R, "R")
  R <- as.integer(R)

  out <- vector("list", R)
  for (i in seq_len(R)) {
    seats <- lapply(seq_len(nrow(seat_specs)), function(j) {
      new_seat(
        id = j,
        N = seat_specs$N[j],
        p_alice = seat_specs$p_alice[j],
        p_other = seat_specs$p_other[j]
      )
    })
    out[[i]] <- simulate_party_audit(
      seats = seats,
      r_majority = r_majority,
      alpha = alpha,
      keep_history = keep_history,
      lambda_fun = lambda_fun,
      mu0 = mu0, u = u,
      eta_mode = eta_mode,
      eta0 = eta0, d = d, c = c
    )
  }
  out
}


args <- commandArgs(trailingOnly = TRUE)

if (length(args) >= 8) {
  W           <- as.integer(args[1])
  S           <- as.integer(args[2])
  N_ballots   <- as.integer(args[3])
  p_mean      <- as.numeric(args[4])
  p_spread    <- as.numeric(args[5])
  n_false     <- as.integer(args[6])
  R           <- as.integer(args[7])
  output_dir  <- args[8]
  seed        <- if (length(args) >= 9) as.integer(args[9]) else 42L
} else {
  W <- 60L; S <- 100L; N_ballots <- 5000L
  p_mean <- 0.55; p_spread <- 30; n_false <- 0L; R <- 1L
  output_dir <- "results"
  seed <- 42L
}

r_majority <- floor(S / 2) + 1L
p_alice_false <- 0.48

set.seed(seed)
n_true <- W - n_false
a_beta <- p_mean * p_spread
b_beta <- (1 - p_mean) * p_spread
p_target_seats <- rbeta(n_true, a_beta, b_beta)
p_target_seats <- pmin(pmax(p_target_seats, 0.51), 0.999)

cat(sprintf("Heterogeneous target margins: mean=%.2f  kappa=%.0f  range=[%.3f, %.3f]\n",
            p_mean, p_spread, min(p_target_seats), max(p_target_seats)))

seat_specs <- data.frame(
  N       = rep(N_ballots, W),
  p_alice = c(p_target_seats,
              if (n_false > 0L) rep(p_alice_false, n_false) else numeric(0)),
  p_other = rep(0, W)
)

alpha      <- 0.05
mu0        <- 0.5
u          <- 1.0
eta_mode   <- "trunc_shrinkage"
eta0       <- 0.51
d          <- 200
c          <- NULL

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

## ============================================================
## True (oracle) margins for Top-r Naive's ranking
## No-margin Bayesian prior: 0.51 for every seat
## ============================================================
temp_seats <- lapply(seq_len(W), function(j) {
  new_seat(id = j, N = seat_specs$N[j],
           p_alice = seat_specs$p_alice[j],
           p_other = seat_specs$p_other[j])
})
true_margins <- vapply(temp_seats, function(s) s$counts["A"] / s$N, numeric(1))

no_margin_prior_vec <- rep(0.51, W)
conc_val <- 200L

cat(sprintf("True margins: range=[%.3f, %.3f]\n",
            min(true_margins), max(true_margins)))
cat(sprintf("No-margin prior: %.3f for every seat\n", 0.51))

## -- helper to collect results into a data.frame row per rep --
## eps hardcoded to 0 so that existing plot_*.R scripts (which facet on eps)
## work without modification.
collect <- function(res_list, method_name, r_used) {
  df <- data.frame(
    method    = method_name,
    p_mean    = p_mean,
    p_spread  = p_spread,
    eps       = 0,
    n_false   = n_false,
    W = W, S = S, N = N_ballots,
    r         = r_used,
    t_eval    = vapply(res_list, `[[`, numeric(1), "t_eval"),
    t_round   = vapply(res_list, `[[`, numeric(1), "t_round"),
    certified = vapply(res_list, `[[`, logical(1), "stop"),
    stringsAsFactors = FALSE
  )
  df$history <- lapply(res_list, `[[`, "history")
  df
}

## -- Method 1: Non-adaptive scheme (label "Naive") --
set.seed(seed + 1000L)
cat(sprintf("[Naive]  p_mean=%.2f  n_false=%d  R=%d  seed=%d\n", p_mean, n_false, R, seed))
res_naive <- replicate_audits(
  R = R, seat_specs = seat_specs, r_majority = r_majority,
  alpha = alpha, keep_history = TRUE, lambda_fun = lambda_naive,
  mu0 = mu0, u = u, eta_mode = eta_mode, eta0 = eta0, d = d, c = c
)

## -- Method 2: Greedy scheme (a = 0) --
set.seed(seed + 1000L)
cat(sprintf("[Greedy] p_mean=%.2f  n_false=%d  R=%d  seed=%d\n", p_mean, n_false, R, seed))
res_greedy <- replicate_audits(
  R = R, seat_specs = seat_specs, r_majority = r_majority,
  alpha = alpha, keep_history = TRUE,
  lambda_fun = make_lambda_greedy(r_majority, alpha, a = 0L),
  mu0 = mu0, u = u, eta_mode = eta_mode, eta0 = eta0, d = d, c = c
)

## -- Method 3: Greedy scheme with a = 3 --
set.seed(seed + 1000L)
a_greedy3 <- 3L
cat(sprintf("[Greedy a=%d] p_mean=%.2f  n_false=%d  R=%d  seed=%d\n", a_greedy3, p_mean, n_false, R, seed))
res_greedy3 <- replicate_audits(
  R = R, seat_specs = seat_specs, r_majority = r_majority,
  alpha = alpha, keep_history = TRUE,
  lambda_fun = make_lambda_greedy(r_majority, alpha, a = a_greedy3),
  mu0 = mu0, u = u, eta_mode = eta_mode, eta0 = eta0, d = d, c = c
)

results <- rbind(
  collect(res_naive,   "Naive",            r_majority),
  collect(res_greedy,  "Greedy",           r_majority),
  collect(res_greedy3, "Greedy (a=3)", r_majority)
)

## -- Method 4: Filtered scheme (label "Bayesian") — prior mean 0.51, eta0 = 0.51 --
set.seed(seed + 1000L)
cat(sprintf("[Bayesian] p_mean=%.2f  n_false=%d  R=%d  seed=%d\n", p_mean, n_false, R, seed))
res_bayesian <- replicate_audits(
  R = R, seat_specs = seat_specs, r_majority = r_majority,
  alpha = alpha, keep_history = TRUE,
  lambda_fun = make_lambda_bayesian(r_majority, alpha,
                                    seat_margins = no_margin_prior_vec,
                                    conc = conc_val),
  mu0 = mu0, u = u, eta_mode = eta_mode, eta0 = eta0, d = d, c = c
)
results <- rbind(results, collect(res_bayesian, "Bayesian", r_majority))

## -- Method 5: Greedy Filtered scheme (label "Greedy Bayesian") — prior mean 0.51, eta0 = 0.51 --
set.seed(seed + 1000L)
cat(sprintf("[Greedy Bayesian] p_mean=%.2f  n_false=%d  R=%d  seed=%d\n", p_mean, n_false, R, seed))
res_greedy_bayes <- replicate_audits(
  R = R, seat_specs = seat_specs, r_majority = r_majority,
  alpha = alpha, keep_history = TRUE,
  lambda_fun = make_lambda_greedy_bayesian(r_majority, alpha, a = 0L,
                                           seat_margins = no_margin_prior_vec,
                                           conc = conc_val),
  mu0 = mu0, u = u, eta_mode = eta_mode, eta0 = eta0, d = d, c = c
)
results <- rbind(results, collect(res_greedy_bayes, "Greedy Bayesian", r_majority))

## -- Method 5b: Greedy Filtered scheme (a=3) (label "Greedy Bayesian (a=3)") --
set.seed(seed + 1000L)
cat(sprintf("[Greedy Bayesian a=%d] p_mean=%.2f  n_false=%d  R=%d  seed=%d\n",
            a_greedy3, p_mean, n_false, R, seed))
res_greedy_bayes3 <- replicate_audits(
  R = R, seat_specs = seat_specs, r_majority = r_majority,
  alpha = alpha, keep_history = TRUE,
  lambda_fun = make_lambda_greedy_bayesian(r_majority, alpha, a = a_greedy3,
                                           seat_margins = no_margin_prior_vec,
                                           conc = conc_val),
  mu0 = mu0, u = u, eta_mode = eta_mode, eta0 = eta0, d = d, c = c
)
results <- rbind(results, collect(res_greedy_bayes3, "Greedy Bayesian (a=3)", r_majority))

## -- Method 6: "Reported top-r seats" baseline (label "Top-r Naive") — ranks by TRUE (oracle) margins --
top_r_idx <- order(true_margins, decreasing = TRUE)[seq_len(r_majority)]
set.seed(seed + 1000L)
cat(sprintf("[Top-r Naive] p_mean=%.2f  n_false=%d  R=%d  seed=%d\n", p_mean, n_false, R, seed))
res_top_r <- replicate_audits(
  R = R, seat_specs = seat_specs, r_majority = r_majority,
  alpha = alpha, keep_history = TRUE,
  lambda_fun = make_lambda_top_r_fallback(top_r_idx),
  mu0 = mu0, u = u, eta_mode = eta_mode, eta0 = eta0, d = d, c = c
)
results <- rbind(results, collect(res_top_r, "Top-r Naive", r_majority))

## -- Method 7: "All seats" baseline (label "Full audit", r = W) — only when n_false = 0 --
if (n_false == 0L) {
  set.seed(seed + 1000L)
  cat(sprintf("[Full]   p_mean=%.2f  n_false=%d  R=%d  seed=%d\n", p_mean, n_false, R, seed))
  res_full <- replicate_audits(
    R = R, seat_specs = seat_specs, r_majority = W,
    alpha = alpha, keep_history = TRUE, lambda_fun = lambda_naive,
    mu0 = mu0, u = u, eta_mode = eta_mode, eta0 = eta0, d = d, c = c
  )
  results <- rbind(results, collect(res_full, "Full audit", W))
}

## -- Save --
outfile <- file.path(output_dir,
                     sprintf("results_W%d_pmean%.2f_kappa%.0f_nfalse%d.rds",
                             W, p_mean, p_spread, n_false))
saveRDS(results, outfile)
cat(sprintf("Saved %s (%d rows)\n", outfile, nrow(results)))
