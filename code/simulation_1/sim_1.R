source("../functions/functions.R")
source("../functions/helpers.R")

## ============================================================
## Main simulator
## ============================================================

simulate_party_audit <- function(seats,
                                 r_majority,
                                 alpha = 0.05,
                                 max_draws = 1e6,
                                 keep_history = FALSE,
                                 # lambda policy: MUST be predictable from past (we call it before drawing)
                                 lambda_fun = function(seat, t, seats) 1.0,
                                 # ALPHA tuning
                                 mu0 = 0.5,
                                 u = 1.0,
                                 eta_mode = "trunc_shrinkage",
                                 eta0 = 0.6,
                                 d = 100,
                                 c = NULL) {
  stopif_pos(alpha, "alpha")
  stopif_pos(max_draws, "max_draws")
  
  log_thresh <- log(1 / alpha)
  
  hist_logMr <- if (keep_history) numeric(0) else NULL
  hist_seat  <- if (keep_history) integer(0) else NULL
  hist_lambda <- if (keep_history) numeric(0) else NULL
  
  for (t in seq_len(as.integer(max_draws))) {
    
    # Choose lambdas based on current information (pre-draw)
    lambdas <- vapply(seats, function(s) lambda_fun(s, t, seats), numeric(1))
    lambdas <- pmax(0, pmin(1, lambdas))
    
    # Use rem * lambda as sampling weight (lambda=0 => not sampled)
    rems <- vapply(seats, function(s) s$rem, integer(1))
    weights <- as.numeric(rems) * lambdas
    
    ddraw <- draw_one_global(seats, weights = weights)
    seats <- ddraw$seats
    if (is.na(ddraw$seat_idx)) break
    
    idx <- ddraw$seat_idx
    lam <- lambdas[idx]
    
    # Update only the touched seat using the pre-chosen lambda
    seats[[idx]] <- seat_update_ALPHA(
      seats[[idx]], x = ddraw$x,
      mu0 = mu0, u = u,
      lambda = lam,
      eta_mode = eta_mode,
      eta0 = eta0, d = d, c = c
    )
    
    # Parliament statistic
    logMr <- log_Mr_from_seats(seats, r_majority = r_majority)
    
    if (keep_history) {
      hist_logMr[t] <- logMr
      hist_seat[t] <- idx
      hist_lambda[t] <- lam
    }
    
    if (logMr >= log_thresh) {
      return(list(
        stop = TRUE,
        draws = t,
        logMr = logMr,
        Mr = exp(logMr),
        per_seat_draws = vapply(seats, function(s) s$k, integer(1)),
        history = if (keep_history) list(logMr = hist_logMr, seat = hist_seat, lambda = hist_lambda) else NULL
      ))
    }
  }
  
  # If no stop
  logMr <- log_Mr_from_seats(seats, r_majority = r_majority)
  list(
    stop = FALSE,
    draws = sum(vapply(seats, function(s) s$k, integer(1))),
    logMr = logMr,
    Mr = exp(logMr),
    per_seat_draws = vapply(seats, function(s) s$k, integer(1)),
    history = if (keep_history) list(logMr = hist_logMr, seat = hist_seat, lambda = hist_lambda) else NULL
  )
}

## ============================================================
## Convenience: replicate simulations
## ============================================================

replicate_audits <- function(R,
                             seat_specs,   # data.frame with columns: N, p_alice, p_other
                             r_majority,
                             alpha = 0.05,
                             max_draws = 1e6,
                             lambda_fun = function(seat, t, seats) 1.0,
                             mu0 = 0.5,
                             u = 1.0,
                             eta_mode = "trunc_shrinkage",
                             eta0 = 0.6,
                             d = 100,
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
      max_draws = max_draws,
      keep_history = FALSE,
      lambda_fun = lambda_fun,
      mu0 = mu0, u = u,
      eta_mode = eta_mode,
      eta0 = eta0, d = d, c = c
    )
  }
  out
}


## ============================================================
## Example simulation (ALPHA-based seat processes)
## ============================================================

set.seed(1)

# Example: 60 reported-winning seats out of 100 total
W <- 60L
S <- 100L
r_majority <- floor(S / 2) + 1L

# Each seat has N ballots and a true Alice share p_alice
seat_specs <- data.frame(
  N = rep(5000L, W),
  p_alice = c(rep(0.55, 35), rep(0.55, 15), rep(0.55, 10)),
  p_other = rep(0, W)
)

# Simulation parameters
alpha <- 0.05
mu0   <- 0.5
u     <- 1.0
eta_mode <- "trunc_shrinkage"
eta0  <- 0.6
d     <- 100
c     <- NULL
max_draws <- 2e5

# Run a batch of independent audits
R <- 10
res_list <- replicate_audits(
  R = R,
  seat_specs = seat_specs,
  r_majority = r_majority,
  alpha = alpha,
  max_draws = max_draws,
  lambda_fun = function(seat, t, seats) 1.0,  # lambda_{c,t} = 1
  mu0 = mu0,
  u = u,
  eta_mode = eta_mode,
  eta0 = eta0,
  d = d,
  c = c
)

# Summarize results
draws <- vapply(res_list, `[[`, numeric(1), "draws")
stops <- vapply(res_list, `[[`, logical(1), "stop")

cat("\nALPHA-based party-level audit summary\n")
cat("---------------------------------------\n")
cat("alpha =", alpha, "\n")
cat("r_majority =", r_majority, "out of", S, "total seats\n")
cat("Repetitions:", R, "\n")
cat("Stopped in", sum(stops), "out of", R, "runs\n")
cat("Average ballots sampled:", mean(draws), "\n")
cat("Median ballots sampled :", median(draws), "\n")

hist(draws, breaks = 20, main = "Distribution of total ballots drawn until stop",
     xlab = "Ballots drawn", col = "gray")