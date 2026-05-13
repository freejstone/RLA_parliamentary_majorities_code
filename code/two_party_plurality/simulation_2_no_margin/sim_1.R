source("../../functions/functions.R")
source("../../functions/helpers.R")
# Jack: Checked
## ============================================================
## Main simulator
## ============================================================

simulate_party_audit <- function(seats,
                                 r_majority,
                                 alpha = 0.05,
                                 keep_history = FALSE,
                                 # lambda policy: function(t_round, seats) -> numeric vector of length |W|.
                                 # Each element must be 0 or 1 (deterministic).
                                 lambda_fun = function(t_round, seats) rep(1.0, length(seats)),
                                 # ALPHA tuning
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

    # Compute lambdas deterministically: 1 = sample, 0 = skip
    lambdas <- lambda_fun(t_round, seats)
    lambdas <- as.integer(lambdas >= 0.5)  # force to 0/1

    # Seats with no remaining ballots cannot be sampled
    rems <- vapply(seats, function(s) s$rem, integer(1))
    lambdas[rems <= 0L] <- 0L

    # Identify which seats to sample this round
    seats_to_sample <- which(lambdas == 1L)
    if (length(seats_to_sample) == 0L) break

    # Draw one ballot from each seat with lambda=1, one-by-one,
    # evaluating the parliament statistic after each draw.
    for (idx in seats_to_sample) {
      out <- draw_from_seat(seats[[idx]])
      seats[[idx]] <- out$seat
      if (is.na(out$x)) next

      # Update this seat's test statistic (lambda = 1)
      eta0_i <- if (length(eta0) > 1L) eta0[idx] else eta0
      seats[[idx]] <- seat_update_ALPHA(
        seats[[idx]], x = out$x,
        mu0 = mu0, u = u,
        lambda = 1.0,
        eta_mode = eta_mode,
        eta0 = eta0_i, d = d, c = c
      )

      t_eval <- t_eval + 1L

      # Evaluate parliament-level statistic after each ballot
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

## ============================================================
## Convenience: replicate simulations
## ============================================================

replicate_audits <- function(R,
                             seat_specs,   # data.frame with columns: N, p_alice, p_other
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


## ============================================================
## Parameterized simulation — noisy reported margins
## ============================================================
## Same as simulation_2 but with noisy reported margins:
##   - eps varies: probability each ballot is misreported
##   - kappa (p_spread) varies: concentration of the Beta distribution
##   - Bayesian methods receive noisy reported margins
##
## Usage: Rscript sim_1.R W S N p_mean p_spread eps n_false R output_dir [seed]
##   p_mean   : mean of the Beta distribution for truly-won seats
##   p_spread : concentration parameter (kappa); higher = less heterogeneity
##   eps      : probability that each ballot is incorrectly reported
##              (each A ballot flipped to B, and vice versa, independently)

args <- commandArgs(trailingOnly = TRUE)

if (length(args) >= 9) {
  W           <- as.integer(args[1])
  S           <- as.integer(args[2])
  N_ballots   <- as.integer(args[3])
  p_mean      <- as.numeric(args[4])
  p_spread    <- as.numeric(args[5])  # kappa (concentration)
  eps         <- as.numeric(args[6])
  n_false     <- as.integer(args[7])
  R           <- as.integer(args[8])
  output_dir  <- args[9]
  seed        <- if (length(args) >= 10) as.integer(args[10]) else 42L
} else {
  W <- 60L; S <- 100L; N_ballots <- 5000L
  p_mean <- 0.55; p_spread <- 30; eps <- 0.05; n_false <- 0L; R <- 1L
  output_dir <- "results"
  seed <- 42L
}

r_majority <- floor(S / 2) + 1L
p_alice_false <- 0.48  # true share for incorrectly-reported seats

# Draw heterogeneous target p_alice for truly-won seats from Beta(a, b)
# with mean = p_mean, concentration = p_spread (kappa)
set.seed(seed)
n_true <- W - n_false
a_beta <- p_mean * p_spread
b_beta <- (1 - p_mean) * p_spread
p_target_seats <- rbeta(n_true, a_beta, b_beta)
# Clamp to (0.51, 0.999) so every truly-won seat actually has Alice winning
p_target_seats <- pmin(pmax(p_target_seats, 0.51), 0.999)

cat(sprintf("Heterogeneous target margins: mean=%.2f  kappa=%.0f  range=[%.3f, %.3f]\n",
            p_mean, p_spread, min(p_target_seats), max(p_target_seats)))

# Build seat specs (target proportions; actualized via largest-remainder in new_seat)
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
## Generate noisy reported margins for Bayesian methods
## ============================================================
## Create a temporary set of seats to get the actualized integer ballot
## counts (from the target proportions via largest-remainder rounding),
## then simulate ballot-level misreporting on the actualized counts.

temp_seats <- lapply(seq_len(W), function(j) {
  new_seat(id = j, N = seat_specs$N[j],
           p_alice = seat_specs$p_alice[j],
           p_other = seat_specs$p_other[j])
})

## Actualized margins (from rounded counts)
actual_margins <- vapply(temp_seats, function(s) s$counts["A"] / s$N, numeric(1))

set.seed(seed + 500L)
if (eps > 0) {
  reported_margins <- vapply(temp_seats, function(s) {
    A_true <- s$counts["A"]
    B_true <- s$counts["B"]
    # Each A ballot has prob eps of being reported as B, and vice versa
    A_reported <- rbinom(1, A_true, 1 - eps) + rbinom(1, B_true, eps)
    A_reported / s$N
  }, numeric(1))
} else {
  reported_margins <- actual_margins
}

# Clamp: all seats in W are *reported* as won, so reported margins must exceed 0.5
reported_margins <- pmax(reported_margins, 0.501)

cat(sprintf("Noisy margins (eps=%.2f): range=[%.3f, %.3f]  (actual range=[%.3f, %.3f])\n",
            eps, min(reported_margins), max(reported_margins),
            min(actual_margins), max(actual_margins)))

# Concentration for the Bayesian prior (same as lazy-init default in functions.R)
conc_val <- 200L

## -- helper to collect results into a data.frame row per rep --
collect <- function(res_list, method_name, r_used) {
  df <- data.frame(
    method    = method_name,
    p_mean    = p_mean,
    p_spread  = p_spread,
    eps       = eps,
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

## -- Method 1: Naive (r = r_majority) --
set.seed(seed + 1000L)
cat(sprintf("[Naive]  p_mean=%.2f  eps=%.2f  n_false=%d  R=%d  seed=%d\n", p_mean, eps, n_false, R, seed))
res_naive <- replicate_audits(
  R = R, seat_specs = seat_specs, r_majority = r_majority,
  alpha = alpha, keep_history = TRUE, lambda_fun = lambda_naive,
  mu0 = mu0, u = u, eta_mode = eta_mode, eta0 = eta0, d = d, c = c
)

## -- Method 2: Greedy (r = r_majority) --
set.seed(seed + 1000L)
cat(sprintf("[Greedy] p_mean=%.2f  eps=%.2f  n_false=%d  R=%d  seed=%d\n", p_mean, eps, n_false, R, seed))
res_greedy <- replicate_audits(
  R = R, seat_specs = seat_specs, r_majority = r_majority,
  alpha = alpha, keep_history = TRUE,
  lambda_fun = make_lambda_greedy(r_majority, alpha, a = 0L),
  mu0 = mu0, u = u, eta_mode = eta_mode, eta0 = eta0, d = d, c = c
)

## -- Method 3: Greedy with a = ceiling(0.05 * W) --
set.seed(seed + 1000L)
a_greedy5 <- ceiling(0.05 * W)
cat(sprintf("[Greedy a=%d] p_mean=%.2f  eps=%.2f  n_false=%d  R=%d  seed=%d\n", a_greedy5, p_mean, eps, n_false, R, seed))
res_greedy5 <- replicate_audits(
  R = R, seat_specs = seat_specs, r_majority = r_majority,
  alpha = alpha, keep_history = TRUE,
  lambda_fun = make_lambda_greedy(r_majority, alpha, a = a_greedy5),
  mu0 = mu0, u = u, eta_mode = eta_mode, eta0 = eta0, d = d, c = c
)

results <- rbind(
  collect(res_naive,   "Naive",            r_majority),
  collect(res_greedy,  "Greedy",           r_majority),
  collect(res_greedy5, "Greedy (a=5%)", r_majority)
)

## -- Method 4: Bayesian adaptive (r = r_majority) — uses reported margins --
set.seed(seed + 1000L)
cat(sprintf("[Bayesian] p_mean=%.2f  eps=%.2f  n_false=%d  R=%d  seed=%d\n", p_mean, eps, n_false, R, seed))
res_bayesian <- replicate_audits(
  R = R, seat_specs = seat_specs, r_majority = r_majority,
  alpha = alpha, keep_history = TRUE,
  lambda_fun = make_lambda_bayesian(r_majority, alpha,
                                    seat_margins = reported_margins,
                                    conc = conc_val),
  mu0 = mu0, u = u, eta_mode = "trunc_shrinkage", eta0 = reported_margins, d = conc_val, c = c
)
results <- rbind(results, collect(res_bayesian, "Bayesian", r_majority))

## -- Method 5: Greedy Bayesian (r = r_majority) — uses reported margins --
set.seed(seed + 1000L)
cat(sprintf("[Greedy Bayesian] p_mean=%.2f  eps=%.2f  n_false=%d  R=%d  seed=%d\n", p_mean, eps, n_false, R, seed))
res_greedy_bayes <- replicate_audits(
  R = R, seat_specs = seat_specs, r_majority = r_majority,
  alpha = alpha, keep_history = TRUE,
  lambda_fun = make_lambda_greedy_bayesian(r_majority, alpha, a = 0L,
                                           seat_margins = reported_margins,
                                           conc = conc_val),
  mu0 = mu0, u = u, eta_mode = "trunc_shrinkage", eta0 = reported_margins, d = conc_val, c = c
)
results <- rbind(results, collect(res_greedy_bayes, "Greedy Bayesian", r_majority))

## -- Method 6: Top-r Naive with fallback (top-r first, then remaining) --
top_r_idx <- order(reported_margins, decreasing = TRUE)[seq_len(r_majority)]
set.seed(seed + 1000L)
cat(sprintf("[Top-r Naive] p_mean=%.2f  eps=%.2f  n_false=%d  R=%d  seed=%d\n", p_mean, eps, n_false, R, seed))
res_top_r <- replicate_audits(
  R = R, seat_specs = seat_specs, r_majority = r_majority,
  alpha = alpha, keep_history = TRUE,
  lambda_fun = make_lambda_top_r_fallback(top_r_idx),
  mu0 = mu0, u = u, eta_mode = eta_mode, eta0 = reported_margins, d = conc_val, c = c
)
results <- rbind(results, collect(res_top_r, "Top-r Naive", r_majority))

## -- Method 7: Full audit (r = W) — only when n_false = 0 --
if (n_false == 0L) {
  set.seed(seed + 1000L)
  cat(sprintf("[Full]   p_mesan=%.2f  eps=%.2f  n_false=%d  R=%d  seed=%d\n", p_mean, eps, n_false, R, seed))
  res_full <- replicate_audits(
    R = R, seat_specs = seat_specs, r_majority = W,
    alpha = alpha, keep_history = TRUE, lambda_fun = lambda_naive,
    mu0 = mu0, u = u, eta_mode = eta_mode, eta0 = eta0, d = d, c = c
  )
  results <- rbind(results, collect(res_full, "Full audit", W))
}

## -- Save --
outfile <- file.path(output_dir,
                     sprintf("results_W%d_pmean%.2f_kappa%.0f_eps%.2f_nfalse%d.rds",
                             W, p_mean, p_spread, eps, n_false))
saveRDS(results, outfile)
cat(sprintf("Saved %s (%d rows)\n", outfile, nrow(results)))
