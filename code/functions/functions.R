## ============================================================
## Seat generator + within-seat sampling (without replacement)
## Two-candidate plurality assorter values: X in {1, 0, 1/2}
## ============================================================

#Jack: Checked 
new_seat <- function(id, N, p_alice, p_other = 0, enforce_winner = TRUE) {
  stopif_pos(N, "N")
  if (N != as.integer(N)) stop("'N' must be an integer.")
  stopif_prob(p_alice, "p_alice")
  stopif_prob(p_other, "p_other")
  if (p_alice + p_other > 1) stop("Need p_alice + p_other <= 1.")
  p_bob <- 1 - p_alice - p_other
  
  probs <- c(A = p_alice, B = p_bob, O = p_other)
  
  # Base integer counts via floors
  raw <- N * probs
  counts <- floor(raw)
  names(counts) <- names(probs)
  
  # Allocate remaining ballots to the largest fractional parts (Hamilton / largest remainder method)
  rem <- N - sum(counts)
  if (rem > 0) {
    frac <- raw - counts
    ord <- order(frac, decreasing = TRUE)
    for (i in seq_len(rem)) {
      counts[ord[i]] <- counts[ord[i]] + 1L
    }
  }
  
  # Enforce winner if requested and feasible
  if (enforce_winner) {
    # If p_alice > p_bob, require A > B; if p_bob > p_alice, require B > A.
    # If equal, no enforcement.
    if (p_alice > p_bob) {
      if (counts["A"] <= counts["B"]) {
        # Try to shift 1 ballot from B->A, else from O->A, else cannot enforce
        if (counts["B"] > 0) {
          counts["B"] <- counts["B"] - 1L
          counts["A"] <- counts["A"] + 1L
        } else if (counts["O"] > 0) {
          counts["O"] <- counts["O"] - 1L
          counts["A"] <- counts["A"] + 1L
        } else {
          stop("Cannot enforce A>B: no ballots available to reallocate.")
        }
      }
    } else if (p_bob > p_alice) {
      if (counts["B"] <= counts["A"]) {
        if (counts["A"] > 0) {
          counts["A"] <- counts["A"] - 1L
          counts["B"] <- counts["B"] + 1L
        } else if (counts["O"] > 0) {
          counts["O"] <- counts["O"] - 1L
          counts["B"] <- counts["B"] + 1L
        } else {
          stop("Cannot enforce B>A: no ballots available to reallocate.")
        }
      }
    }
  }
  
  # Final sanity checks
  if (any(counts < 0)) stop("Internal error: negative counts.")
  if (sum(counts) != N) stop("Internal error: counts do not sum to N.")
  
  counts <- as.integer(counts)
  names(counts) <- c("A","B","O")
  
  list(
    id = id,
    N = as.integer(N),
    rem = as.integer(N),
    counts = counts,
    
    k = 0L,
    sum_x = 0.0,
    
    logT = 0.0,
    logM = 0.0,

    obs_counts = c(A = 0L, B = 0L, O = 0L)
  )
}

# Jack: Checked
draw_from_seat <- function(seat) {
  if (seat$rem <= 0L) return(list(seat = seat, x = NA_real_))
  
  # Force a 3-vector in the correct order, even if seat$counts got reordered/dropped
  cats <- c("A", "B", "O")
  if (is.null(names(seat$counts))) stop("seat$counts must be a named vector with names A,B,O.")
  if (!all(cats %in% names(seat$counts))) {
    stop("seat$counts must contain names A,B,O; got: ", paste(names(seat$counts), collapse = ","))
  }
  
  cnt <- as.numeric(seat$counts[cats])
  tot <- sum(cnt)
  if (tot <= 0) stop("seat$counts sums to 0 but seat$rem > 0. Inconsistent state.")
  probs <- cnt / tot  # length 3 guaranteed
  
  cat_idx <- sample.int(3L, size = 1L, prob = probs)
  
  if (cat_idx == 1L) {
    x <- 1.0
    seat$counts["A"] <- seat$counts["A"] - 1L
    seat$obs_counts["A"] <- seat$obs_counts["A"] + 1L
  } else if (cat_idx == 2L) {
    x <- 0.0
    seat$counts["B"] <- seat$counts["B"] - 1L
    seat$obs_counts["B"] <- seat$obs_counts["B"] + 1L
  } else {
    x <- 0.5
    seat$counts["O"] <- seat$counts["O"] - 1L
    seat$obs_counts["O"] <- seat$obs_counts["O"] + 1L
  }
  
  seat$rem <- seat$rem - 1L
  list(seat = seat, x = x)
}


## ============================================================
## General seat generator (arbitrary ballot types + assertions)
## ============================================================
#
# For multi-candidate systems (IRV, Borda, etc.) each seat has
# L ballot types and J SHANGRLA assertions.  The assorter_matrix
# is J x L; entry [j,l] is the assorter value for assertion j
# on a ballot of type l.
#
# The seat tracks J independent e-processes (one per assertion).
# The seat-level process is the minimum of the per-assertion processes,
# i.e. E_{s,t} = min_j E_{s,j,t} (see paper, Section "A test process for
# certifying a single seat").

# Jack: Checked
new_seat_general <- function(id, N, type_probs, assorter_matrix) {
  stopif_pos(N, "N")
  if (N != as.integer(N)) stop("'N' must be an integer.")
  L <- length(type_probs)
  if (L < 2L) stop("Need at least 2 ballot types.")
  if (abs(sum(type_probs) - 1) > 1e-8) stop("type_probs must sum to 1.")
  if (any(type_probs < 0)) stop("type_probs must be non-negative.")

  # assorter_matrix: J x L  (single assertion = 1-row matrix or vector)
  if (is.null(dim(assorter_matrix))) {
    assorter_matrix <- matrix(assorter_matrix, nrow = 1)
  }
  J <- nrow(assorter_matrix)
  if (ncol(assorter_matrix) != L) stop("assorter_matrix must have L columns.")

  # Integer counts via Hamilton / largest-remainder method
  raw    <- N * type_probs
  counts <- floor(raw)
  rem_alloc <- N - sum(counts)
  if (rem_alloc > 0L) {
    frac <- raw - counts
    ord  <- order(frac, decreasing = TRUE)
    for (i in seq_len(rem_alloc)) {
      counts[ord[i]] <- counts[ord[i]] + 1L
    }
  }
  counts <- as.integer(counts)
  if (!is.null(names(type_probs))) names(counts) <- names(type_probs)

  list(
    id     = id,
    N      = as.integer(N),
    rem    = as.integer(N),
    counts = counts,
    J      = J,
    L      = L,

    k      = 0L,
    sum_x  = rep(0.0, J),

    logT   = rep(0.0, J),
    logM   = rep(0.0, J),

    obs_counts      = rep(0L, L),
    assorter_matrix = assorter_matrix
  )
}


## Draw one ballot from a general seat.
## Returns list(seat, x_vec) where x_vec has J assorter values.
# Jack: Checked
draw_from_seat_general <- function(seat) {
  if (seat$rem <= 0L) return(list(seat = seat, x_vec = rep(NA_real_, seat$J)))

  L   <- seat$L
  cnt <- as.numeric(seat$counts)
  tot <- sum(cnt)
  if (tot <= 0) stop("counts sum to 0 but rem > 0. Inconsistent state.")

  type_idx <- sample.int(L, size = 1L, prob = cnt / tot)

  seat$counts[type_idx]     <- seat$counts[type_idx] - 1L
  seat$obs_counts[type_idx] <- seat$obs_counts[type_idx] + 1L
  seat$rem <- seat$rem - 1L

  x_vec <- seat$assorter_matrix[, type_idx]  # length J
  list(seat = seat, x_vec = x_vec)
}


## ============================================================
## ALPHA update for multi-assertion seats
## ============================================================
#
# Updates J independent ALPHA e-processes in one seat, given
# a J-vector of assorter values from a single ballot draw.
# ALPHA/SHANGRLA also has a shrinkage parameter f but that is set to f = 0 by default and has no effect.
#Jack: Checked. Though note that this function is called ASSUMING a seat is sampled.
# Vectorized over J assertions for performance.
seat_update_ALPHA_general <- function(seat, x_vec,
                                       mu0 = 0.5,
                                       u = 1.0,
                                       lambda = 1.0,
                                       eta_mode = c("trunc_shrinkage", "shrinkage", "fixed"),
                                       eta0 = 0.51,
                                       d = 200,
                                       c = NULL,
                                       tiny = 1e-12) {
  eta_mode <- match.arg(eta_mode)
  J <- seat$J
  N <- seat$N
  k <- seat$k
  if (k >= N) return(seat)

  denomN <- N - k
  if (denomN <= 0) return(seat)

  # Identify finite observations; skip already-certified assertions
  fin <- is.finite(x_vec)
  if (!any(fin)) { seat$k <- k + 1L; return(seat) }
  if (any(x_vec[fin] < 0 | x_vec[fin] > u)) stop("Need x in [0,u].")

  already_inf <- !is.finite(seat$logT)
  active <- fin & !already_inf

  if (any(active)) {
    mu_ub <- (N * mu0 - seat$sum_x) / denomN

    # Blown up: null is impossible
    blown <- active & (mu_ub < 0)
    if (any(blown)) {
      seat$logT[blown] <- Inf
      if (lambda > 0) seat$logM[blown] <- Inf
      active[blown] <- FALSE
    }

    if (any(active)) {
      mu_next_all <- pmin(mu_ub, u)

      # At boundary: no-bet (mult = 1)
      at_boundary <- active & ((u - mu_next_all) <= tiny)
      active[at_boundary] <- FALSE
    }

    if (any(active)) {
      idx <- which(active)
      x_a <- x_vec[idx]
      mu_next_a <- mu_next_all[idx]
      sum_x_a <- seat$sum_x[idx]

      # Vectorized eta computation
      if (eta_mode == "trunc_shrinkage") {
        denom_d <- d + k
        eta_raw <- (d * eta0 + sum_x_a) / denom_d
        c_val <- if (is.null(c)) max(0, (eta0 - 0.5) / 2 - .Machine$double.eps) else c
        eps_eta <- c_val / sqrt(denom_d)
        eta_a <- pmin(pmax(eta_raw, mu_next_a + eps_eta), u - tiny)
      } else if (eta_mode == "shrinkage") {
        denom_d <- d + k
        eta_raw <- (d * eta0 + sum_x_a) / denom_d
        eta_a <- pmin(pmax(eta_raw, tiny), u - tiny)
      } else {
        eta_a <- pmin(u - tiny, pmax(mu_next_a + tiny, eta0))
      }

      # ALPHA multiplier (vectorized, with NaN guards)
      term1 <- ifelse(x_a == 0, 0, x_a * (eta_a / mu_next_a))
      term2 <- ifelse(x_a == u, 0, (u - x_a) * ((u - eta_a) / (u - mu_next_a)))
      mult_T <- (1 / u) * (term1 + term2)

      if (any(is.nan(mult_T))) stop("ALPHA multiplier is NaN.")

      # Update logT
      inf_T <- is.infinite(mult_T) & mult_T > 0
      fin_T <- !inf_T
      if (any(mult_T[fin_T] <= 0)) stop("ALPHA multiplier became nonpositive.")
      seat$logT[idx[inf_T]] <- Inf
      seat$logT[idx[fin_T]] <- seat$logT[idx[fin_T]] + log(mult_T[fin_T])

      # Update logM
      if (lambda > 0) {
        mult_M <- ifelse(inf_T, Inf, (1 - lambda) + lambda * mult_T)
        inf_M <- is.infinite(mult_M) & mult_M > 0
        fin_M <- !inf_M
        if (any(mult_M[fin_M] <= 0)) stop("Merged multiplier became nonpositive.")
        seat$logM[idx[inf_M]] <- Inf
        seat$logM[idx[fin_M]] <- seat$logM[idx[fin_M]] + log(mult_M[fin_M])
      }
    }
  }

  # Update sum_x for all finite observations
  seat$sum_x[fin] <- seat$sum_x[fin] + x_vec[fin]
  seat$k <- k + 1L
  seat
}


## ============================================================
## ALPHA components
## ============================================================

#Jack: Checked
eta_trunc_shrinkage <- function(k, sum_x, mu_next,
                                u = 1, eta0 = 0.51, d = 200, c = NULL, tiny = 1e-12,
                                truncate = TRUE) {
  # k = number of processed draws so far; next index is j = k+1
  stopif_pos(d, "d")
  if (!is.numeric(eta0) || length(eta0) != 1L || !is.finite(eta0)) stop("'eta0' must be finite scalar.")

  if (is.null(c)) {
    # SHANGRLA default: (eta - t)/2 - eps, with t = mu0 = 0.5
    c <- max(0, (eta0 - 0.5) / 2 - .Machine$double.eps)
  }
  if (!is.numeric(c) || length(c) != 1L || !is.finite(c) || c < 0) stop("'c' must be finite scalar >= 0.")
  
  denom <- d + k
  eta_raw <- (d * eta0 + sum_x) / denom
  eps <- c / sqrt(denom)
  
  if (truncate) {
    eta <- max(eta_raw, mu_next + eps)
  } else {
    eta <- max(eta_raw, tiny)
  }
  eta <- min(eta, u - tiny)
  eta
}

#Jack: Checked. Though note that this function is called ASSUMING a seaet is sampled. 
seat_update_ALPHA <- function(seat, x,
                              mu0 = 0.5,
                              u = 1.0,
                              lambda = 1.0,
                              eta_mode = c("trunc_shrinkage", "shrinkage", "fixed"),
                              eta0 = 0.51,
                              d = 200,
                              c = NULL,
                              tiny = 1e-12) {
  eta_mode <- match.arg(eta_mode)
  stopif_prob(lambda, "lambda")
  
  # If no observation, do nothing
  if (!is.finite(x)) return(seat)
  if (x < 0 || x > u) stop("Need x in [0,u].")
  
  N <- seat$N
  k <- seat$k
  if (k >= N) return(seat)  # already fully processed
  
  # Remaining draws
  denomN <- N - k
  if (denomN <= 0) return(seat)
  
  # Upper bound on the next conditional mean under the composite null E[X] <= mu0:
  # For any population with mean <= mu0 and values in [0,u], the remaining average must satisfy
  #   (sum_x + (N-k)*mu_rem) / N <= mu0  => mu_rem <= (N*mu0 - sum_x)/(N-k)
  mu_ub <- (N * mu0 - seat$sum_x) / denomN
  
  # If mu_ub < 0, even the *maximum* possible remaining mean (>=0) cannot satisfy the inequality.
  # Hence the composite null is impossible given the past, so we can safely "explode".
  if (mu_ub < 0) {
    seat$logT <- Inf
    if (lambda > 0) seat$logM <- Inf
    seat$sum_x <- seat$sum_x + x
    seat$k <- k + 1L
    return(seat)
  }
  
  # For ALPHA update we need a feasible mu in [0,u].
  # If mu_ub > u, the point null mean=mu0 is impossible, but the composite null mean<=mu0 may still hold.
  # Use the least-favourable feasible conditional mean capped at u.
  mu_next <- min(mu_ub, u)
  
  # Upper-boundary handling: if mu_next is ~u, the null is essentially true, so no-bet (mult=1).
  # Lower boundary is handled by eta_trunc_shrinkage's mu_next + c/sqrt(d+k) floor, which
  # keeps eta/mu_next finite. This preserves the large-multiplier gains available when
  # mu_next approaches 0 (e.g. marginal true-winner seats about to certify).
  if ((u - mu_next) <= tiny) {
    mult_T <- 1.0
    # merged update: (1-lambda)+lambda*1 = 1
    mult_M <- 1.0

    seat$logT <- seat$logT + log(mult_T)
    seat$logM <- seat$logM + log(mult_M)

    seat$sum_x <- seat$sum_x + x
    seat$k <- k + 1L
    return(seat)
  }
  
  # Choose eta in (mu_next, u)
  eta <- switch(
    eta_mode,
    fixed = min(u - tiny, max(mu_next + tiny, eta0)),
    trunc_shrinkage = eta_trunc_shrinkage(
      k = k, sum_x = seat$sum_x, mu_next = mu_next, u = u,
      eta0 = eta0, d = d, c = c, tiny = tiny, truncate = TRUE
    ),
    shrinkage = eta_trunc_shrinkage(
      k = k, sum_x = seat$sum_x, mu_next = mu_next, u = u,
      eta0 = eta0, d = d, c = c, tiny = tiny, truncate = FALSE
    )
  )
  
  # ALPHA multiplier for the raw martingale T.
  # Guard against 0*Inf = NaN when x=0 and mu_next~0, or x=u and mu_next~u.
  term1 <- if (x == 0) 0 else x * (eta / mu_next)
  term2 <- if (x == u) 0 else (u - x) * ((u - eta) / (u - mu_next))
  mult_T <- (1 / u) * (term1 + term2)
  
  # Update logT
  if (is.nan(mult_T)) stop("ALPHA multiplier is NaN; check inputs.")
  if (is.infinite(mult_T) && mult_T > 0) {
    seat$logT <- Inf
  } else {
    if (mult_T <= 0) stop("ALPHA multiplier became nonpositive.")
    seat$logT <- seat$logT + log(mult_T)
  }

  # Update logM (merged/weighted seat process used for parliament)
  # Standard “bet fraction” merge: mult_M = (1-lambda) + lambda * mult_T
  if (lambda <= 0) {
    mult_M <- 1.0
  } else if (is.infinite(mult_T)) {
    mult_M <- Inf
  } else {
    mult_M <- (1 - lambda) + lambda * mult_T
  }
  
  if (is.nan(mult_M)) stop("Merged multiplier is NaN; check lambda/mult_T.")
  if (is.infinite(mult_M) && mult_M > 0) {
    seat$logM <- Inf
  } else {
    if (mult_M <= 0) stop("Merged multiplier became nonpositive; check lambda/mult_T.")
    seat$logM <- seat$logM + log(mult_M)
  }
  
  # Bookkeeping
  seat$sum_x <- seat$sum_x + x
  seat$k <- k + 1L
  seat
}


## ============================================================
## Parliament-level statistic: product of k smallest seat processes
## ============================================================

# Jack: Checked
log_Mr_from_seats <- function(seats, r_majority) {
  W <- length(seats)
  if (r_majority > W) stop(sprintf("r_majority (%d) cannot exceed number of seats W (%d).", r_majority, W))
  k <- W - r_majority + 1L
  if (k <= 0L) stop("Need W >= r_majority.")
  
  # IMPORTANT: use logM (seat-level process E_{s,t}), not logT
  # For multi-assertion seats, the seat-level process is the minimum over the
  # seat's assertions: logM = min_j logM_j (see paper, Section "A test process
  # for certifying a single seat").
  logs <- vapply(seats, function(s) min(s$logM), numeric(1))
  sum(sort(logs, decreasing = FALSE)[seq_len(k)])
}

## ============================================================
## Lambda policies
## ============================================================

# Non-adaptive scheme (paper, Section "Adaptive sampling schemes: Non-adaptive";
# labelled "Naive" internally): sample every seat, i.e. D_{s,t} = 1 for all s, t.
# Jack: Checked
lambda_naive <- function(t_round, seats) rep(1.0, length(seats))

# "Reported top-r seats" baseline (paper, Section "Results"; labelled
# "Top-r Naive" internally): audit only the r seats ranked highest by margin.
# With fallback: once all r are exhausted without certifying, switch to the
# remaining seats.
#Jack: Checked
make_lambda_top_r_fallback <- function(top_r_indices) {
  function(t_round, seats) {
    lambdas <- rep(0.0, length(seats))
    top_r_active <- vapply(seats[top_r_indices], function(s) s$rem > 0L, logical(1))
    if (any(top_r_active)) {
      lambdas[top_r_indices] <- 1.0
    } else {
      lambdas[-top_r_indices] <- 1.0
    }
    lambdas
  }
}

# Greedy scheme (paper, Section "Adaptive sampling schemes: Greedy"):
# sample the d_t seats with the weakest seat-level processes, where d_t is
# derived from delta_t with the hedging parameter a (a = 0 is most aggressive).
# Returns a closure that captures r_majority, alpha, and a.
# Jack: Checked
make_lambda_greedy <- function(r_majority, alpha, a = 0L) {
  log_thresh <- log(1 / alpha)
  function(t_round, seats) {
    W <- length(seats)
    k <- W - r_majority + 1L  # = |W| - r + 1

    logMs <- vapply(seats, function(s) min(s$logM), numeric(1))
    sorted_logMs <- sort(logMs, decreasing = FALSE)

    # Compute delta_t:
    # delta_t = max{ i in {1,...,r} : prod_{c=i}^{W-r+i} M_{(c)} < 1/alpha }
    # Convention: delta_t = 0 if the set is empty (already certified)
    delta_t <- 0L
    for (i in seq_len(r_majority)) {
      log_prod <- sum(sorted_logMs[i:(k + i - 1L)])
      if (log_prod < log_thresh) {
        delta_t <- i
      } else {
        break
      }
    }

    if (delta_t == 0L) {
      return(rep(0.0, W))
    }

    # d_t: number of weakest seats to sample
    if (delta_t >= r_majority) {
      d_t <- W
    } else {
      d_t <- min(delta_t + a, W)
    }

    # If all d_t weakest seats are exhausted, expand d_t until at least
    # one selected seat has remaining ballots (fallback to stronger seats)
    rems <- vapply(seats, function(s) s$rem, integer(1))
    ord  <- order(logMs)  # seat indices sorted weakest-first
    while (d_t < W && all(rems[ord[seq_len(d_t)]] <= 0L)) {
      d_t <- d_t + 1L
    }

    # lambda = 1 if seat is among the d_t weakest, else 0
    cutoff <- sorted_logMs[d_t]
    lambdas <- ifelse(logMs <= cutoff, 1.0, 0.0)

    # Break ties: if more than d_t seats selected, trim excess
    n_selected <- sum(lambdas)
    if (n_selected > d_t) {
      tied <- which(logMs == cutoff)
      n_excess <- as.integer(n_selected - d_t)
      lambdas[tail(tied, n_excess)] <- 0.0
    }

    lambdas
  }
}

## ============================================================
## Helper: expected one-step log-gain per seat (unused; kept for reference)
## ============================================================

# Jack: Unused
.compute_gains <- function(seats, beta_alpha, beta_beta, W) {
  gains <- rep(-Inf, W)
  mu0_val <- 0.5
  u_val   <- 1.0
  tiny    <- 1e-12
  eta0_val <- 0.51
  d_val    <- 200
  c_val    <- max(0, (eta0_val - 0.5) / 2 - .Machine$double.eps)

  for (c_idx in seq_len(W)) {
    s <- seats[[c_idx]]
    if (s$rem <= 0L) next

    a_c <- beta_alpha[c_idx]
    b_c <- beta_beta[c_idx]
    p_alice_hat <- a_c / (a_c + b_c)

    N_c <- s$N
    k_c <- s$k
    denom_N <- N_c - k_c
    if (denom_N <= 0L) next

    mu_ub <- (N_c * mu0_val - s$sum_x) / denom_N
    if (mu_ub < 0) {
      gains[c_idx] <- Inf
      next
    }
    mu_next <- min(mu_ub, u_val)
    if (mu_next <= tiny || (u_val - mu_next) <= tiny) next

    denom_d  <- d_val + k_c
    eta_raw  <- (d_val * eta0_val + s$sum_x) / denom_d
    eps      <- c_val / sqrt(denom_d)
    eta      <- max(eta_raw, mu_next + eps)
    eta      <- min(eta, u_val - tiny)

    ratio_A <- eta / mu_next
    ratio_B <- (u_val - eta) / (u_val - mu_next)
    mult_1 <- (1 / u_val) * (1.0 * ratio_A + (u_val - 1.0) * ratio_B)
    mult_0 <- (1 / u_val) * (0.0 * ratio_A + (u_val - 0.0) * ratio_B)

    log_mult_1 <- if (mult_1 > 0) log(mult_1) else -Inf
    log_mult_0 <- if (mult_0 > 0) log(mult_0) else -Inf

    gains[c_idx] <- p_alice_hat * log_mult_1 + (1 - p_alice_hat) * log_mult_0
  }
  gains
}

## ============================================================
## Filtered scheme (paper, Section "Adaptive sampling schemes: Filtered";
## labelled "Bayesian" internally)
## ============================================================
#
# Restricts sampling to the |W| - r + 1 weakest seats (the "active set") and,
# among those, samples only seats whose posterior probability
#   pi_s = min_j P(theta_{s,j} > 1/2 + eps | data)   (eps = 0)
# exceeds the threshold tau (default 0.01). This filters out seats that appear
# to be falsely reported. If no active-set seat passes the filter in a round,
# the filter is dropped for that round (revert to Greedy on the active set).
#
# Two-candidate (Beta) path:
#   Each seat gets a Beta prior on its true Alice share p_s.
#   Mean = reference margin; concentration = conc.
#   Posterior update: alpha += sum_x, beta += (k - sum_x).
#   pi_s = P(p_s > 1/2 | data) via the Beta CDF (exact).
#
# General (Dirichlet) path:
#   Each seat gets a Dirichlet prior on ballot-type proportions.
#   Prior:  alpha_{s,0,l} = p_hat_{s,l} * conc.
#   Update: alpha_{s,k,l} = alpha_{s,0,l} + n_{s,l}.
#   pi_s = min_j P(theta_{s,j} > 1/2 | data) via a normal approximation to each
#     assertion mean theta_{s,j} = sum_l a_{j,l} p_{s,l}.
#
# The Dirichlet path is activated when seat_proportions and
# assorter_values are provided.  Otherwise uses Beta (backward compat).

# Jack: Checked
make_lambda_bayesian <- function(r_majority, alpha,
                                  seat_margins = NULL,
                                  seat_proportions = NULL,
                                  assorter_values = NULL,
                                  conc = NULL,
                                  tau = 0.01) {
  log_thresh <- log(1 / alpha)
  use_dirichlet <- !is.null(seat_proportions) && !is.null(assorter_values)

  # --- Shared state ---
  W         <- NULL
  k_tracked <- NULL

  # --- Beta state (two-candidate backward compat) ---
  init_alpha_b <- NULL
  init_beta_b  <- NULL
  beta_alpha_b <- NULL
  beta_beta_b  <- NULL

  # --- Dirichlet state (general) ---
  L          <- NULL
  a_vals     <- NULL
  init_dir   <- NULL   # list of W vectors (prior alphas)
  dir_alpha  <- NULL   # list of W vectors (posterior alphas)

  init_beta_state <- function(margins) {
    W            <<- length(margins)
    init_alpha_b <<- margins * conc
    init_beta_b  <<- (1 - margins) * conc
    beta_alpha_b <<- init_alpha_b
    beta_beta_b  <<- init_beta_b
    k_tracked    <<- rep(0L, W)
  }

  init_dirichlet_state <- function(proportions, a_values) {
    L         <<- length(a_values)
    a_vals    <<- a_values
    W         <<- length(proportions)
    init_dir  <<- lapply(proportions, function(p) p * conc)
    dir_alpha <<- lapply(init_dir, function(a) a)
    k_tracked <<- rep(0L, W)
  }

  # --- Up-front initialisation ---
  if (use_dirichlet) {
    if (is.null(conc)) stop("conc must be specified when seat_proportions is provided")
    if (is.matrix(seat_proportions)) {
      props_list <- lapply(seq_len(nrow(seat_proportions)), function(i) seat_proportions[i, ])
    } else {
      props_list <- seat_proportions
    }
    init_dirichlet_state(props_list, assorter_values)
  } else if (!is.null(seat_margins)) {
    if (is.null(conc)) stop("conc must be specified when seat_margins is provided")
    init_beta_state(seat_margins)
  }

  function(t_round, seats) {
    W_now <- length(seats)

    # --- Lazy init (Beta path only): derive margins from true ballot counts ---
    if (is.null(W)) {
      if (is.null(conc)) conc <<- 200L
      margins <- vapply(seats, function(s) s$counts["A"] / s$N, numeric(1))
      init_beta_state(margins)
    }

    # --- Detect new replication (seats reset to k=0) and reset posteriors ---
    if (t_round == 1L && any(k_tracked > 0L)) {
      if (use_dirichlet) {
        dir_alpha <<- lapply(init_dir, function(a) a)
      } else {
        beta_alpha_b <<- init_alpha_b
        beta_beta_b  <<- init_beta_b
      }
      k_tracked <<- rep(0L, W)
    }

    # --- Update posteriors from new observations ---
    if (use_dirichlet) {
      for (c_idx in seq_len(W_now)) {
        s <- seats[[c_idx]]
        if (s$k > k_tracked[c_idx]) {
          dir_alpha[[c_idx]] <<- init_dir[[c_idx]] + as.numeric(s$obs_counts)
          k_tracked[c_idx]   <<- s$k
        }
      }
    } else {
      for (c_idx in seq_len(W_now)) {
        s <- seats[[c_idx]]
        if (s$k > k_tracked[c_idx]) {
          beta_alpha_b[c_idx] <<- init_alpha_b[c_idx] + s$sum_x
          beta_beta_b[c_idx]  <<- init_beta_b[c_idx] + (s$k - s$sum_x)
          k_tracked[c_idx]    <<- s$k
        }
      }
    }

    # --- P(theta > 0.5 | data) for each seat ---
    if (use_dirichlet) {
      post_prob <- vapply(seq_len(W_now), function(c_idx) {
        a <- dir_alpha[[c_idx]]
        S_a <- sum(a)
        p_hat <- a / S_a
        if (is.matrix(a_vals)) {
          # Per-assertion normal approximation, take min P(assertion_j > 0.5)
          probs_j <- vapply(seq_len(nrow(a_vals)), function(j) {
            av <- a_vals[j, ]
            mu_j <- sum(av * p_hat)
            var_j <- (sum(av^2 * p_hat) - mu_j^2) / (S_a + 1)
            if (var_j <= 0) return(as.numeric(mu_j > 0.5))
            pnorm(0.5, mean = mu_j, sd = sqrt(var_j), lower.tail = FALSE)
          }, numeric(1))
          min(probs_j)
        } else {
          mu_j <- sum(p_hat * a_vals)
          var_j <- (sum(a_vals^2 * p_hat) - mu_j^2) / (S_a + 1)
          if (var_j <= 0) return(as.numeric(mu_j > 0.5))
          pnorm(0.5, mean = mu_j, sd = sqrt(var_j), lower.tail = FALSE)
        }
      }, numeric(1))
    } else {
      post_prob <- pbeta(0.5, beta_alpha_b, beta_beta_b, lower.tail = FALSE)
    }

    # --- Selection rule ---
    logMs <- vapply(seats, function(s) min(s$logM), numeric(1))
    k_vec <- W_now - r_majority + 1L
    ord <- order(logMs)

    search_k <- k_vec
    rems_now <- vapply(seats, function(s) s$rem, integer(1))
    while (search_k < W_now && all(rems_now[ord[seq_len(search_k)]] <= 0L)) {
      search_k <- search_k + 1L
    }

    lambdas <- rep(0.0, W_now)
    for (i in seq_len(search_k)) {
      c_idx <- ord[i]
      if (rems_now[c_idx] > 0L && post_prob[c_idx] > tau) {
        lambdas[c_idx] <- 1.0
      }
    }

    if (sum(lambdas) == 0) {
      for (i in seq_len(search_k)) {
        c_idx <- ord[i]
        if (rems_now[c_idx] > 0L) lambdas[c_idx] <- 1.0
      }
    }

    lambdas
  }
}

## ============================================================
## Greedy Filtered scheme (paper, Section "Adaptive sampling schemes:
## Greedy Filtered"; labelled "Greedy Bayesian" internally)
## ============================================================
#
# Combines the Greedy scheme's d_t active-set window with the Filtered
# scheme's posterior filter (pi_s > tau).  Steps:
#   1. Compute delta_t, d_t exactly as make_lambda_greedy (with 'a').
#   2. Expand d_t if all selected seats are exhausted.
#   3. Among the d_t weakest seats, sample those with pi_s > tau, where
#      pi_s = min_j P(theta_{s,j} > 1/2 | data)
#      (normal approximation for Dirichlet, exact pbeta for Beta).
#
# Supports both Beta (two-candidate) and Dirichlet (general) paths.

# Jack: Checked
make_lambda_greedy_bayesian <- function(r_majority, alpha, a = 0L,
                                        seat_margins = NULL,
                                        seat_proportions = NULL,
                                        assorter_values = NULL,
                                        conc = NULL,
                                        tau = 0.01) {
  log_thresh <- log(1 / alpha)
  use_dirichlet <- !is.null(seat_proportions) && !is.null(assorter_values)

  # --- Shared state ---
  W         <- NULL
  k_tracked <- NULL

  # --- Beta state ---
  init_alpha_b <- NULL
  init_beta_b  <- NULL
  beta_alpha_b <- NULL
  beta_beta_b  <- NULL

  # --- Dirichlet state ---
  L          <- NULL
  a_vals     <- NULL
  init_dir   <- NULL
  dir_alpha  <- NULL

  init_beta_state <- function(margins) {
    W            <<- length(margins)
    init_alpha_b <<- margins * conc
    init_beta_b  <<- (1 - margins) * conc
    beta_alpha_b <<- init_alpha_b
    beta_beta_b  <<- init_beta_b
    k_tracked    <<- rep(0L, W)
  }

  init_dirichlet_state <- function(proportions, a_values) {
    L         <<- length(a_values)
    a_vals    <<- a_values
    W         <<- length(proportions)
    init_dir  <<- lapply(proportions, function(p) p * conc)
    dir_alpha <<- lapply(init_dir, function(a) a)
    k_tracked <<- rep(0L, W)
  }

  if (use_dirichlet) {
    if (is.null(conc)) stop("conc must be specified when seat_proportions is provided")
    if (is.matrix(seat_proportions)) {
      props_list <- lapply(seq_len(nrow(seat_proportions)), function(i) seat_proportions[i, ])
    } else {
      props_list <- seat_proportions
    }
    init_dirichlet_state(props_list, assorter_values)
  } else if (!is.null(seat_margins)) {
    if (is.null(conc)) stop("conc must be specified when seat_margins is provided")
    init_beta_state(seat_margins)
  }

  function(t_round, seats) {
    W_now <- length(seats)

    # --- Lazy init ---
    if (is.null(W)) {
      if (is.null(conc)) conc <<- 100L
      margins <- vapply(seats, function(s) s$counts["A"] / s$N, numeric(1))
      init_beta_state(margins)
    }

    # --- Detect new replication and reset posteriors ---
    if (t_round == 1L && any(k_tracked > 0L)) {
      if (use_dirichlet) {
        dir_alpha <<- lapply(init_dir, function(a) a)
      } else {
        beta_alpha_b <<- init_alpha_b
        beta_beta_b  <<- init_beta_b
      }
      k_tracked <<- rep(0L, W)
    }

    # --- Update posteriors ---
    if (use_dirichlet) {
      for (c_idx in seq_len(W_now)) {
        s <- seats[[c_idx]]
        if (s$k > k_tracked[c_idx]) {
          dir_alpha[[c_idx]] <<- init_dir[[c_idx]] + as.numeric(s$obs_counts)
          k_tracked[c_idx]   <<- s$k
        }
      }
    } else {
      for (c_idx in seq_len(W_now)) {
        s <- seats[[c_idx]]
        if (s$k > k_tracked[c_idx]) {
          beta_alpha_b[c_idx] <<- init_alpha_b[c_idx] + s$sum_x
          beta_beta_b[c_idx]  <<- init_beta_b[c_idx] + (s$k - s$sum_x)
          k_tracked[c_idx]    <<- s$k
        }
      }
    }

    # --- P(theta > 0.5 | data) for each seat ---
    if (use_dirichlet) {
      post_prob <- vapply(seq_len(W_now), function(c_idx) {
        a <- dir_alpha[[c_idx]]
        S_a <- sum(a)
        p_hat <- a / S_a
        if (is.matrix(a_vals)) {
          probs_j <- vapply(seq_len(nrow(a_vals)), function(j) {
            av <- a_vals[j, ]
            mu_j <- sum(av * p_hat)
            var_j <- (sum(av^2 * p_hat) - mu_j^2) / (S_a + 1)
            if (var_j <= 0) return(as.numeric(mu_j > 0.5))
            pnorm(0.5, mean = mu_j, sd = sqrt(var_j), lower.tail = FALSE)
          }, numeric(1))
          min(probs_j)
        } else {
          mu_j <- sum(p_hat * a_vals)
          var_j <- (sum(a_vals^2 * p_hat) - mu_j^2) / (S_a + 1)
          if (var_j <= 0) return(as.numeric(mu_j > 0.5))
          pnorm(0.5, mean = mu_j, sd = sqrt(var_j), lower.tail = FALSE)
        }
      }, numeric(1))
    } else {
      post_prob <- pbeta(0.5, beta_alpha_b, beta_beta_b, lower.tail = FALSE)
    }

    # --- Greedy d_t computation (identical to make_lambda_greedy) ---
    k_vec <- W_now - r_majority + 1L
    logMs <- vapply(seats, function(s) min(s$logM), numeric(1))
    sorted_logMs <- sort(logMs, decreasing = FALSE)

    delta_t <- 0L
    for (i in seq_len(r_majority)) {
      log_prod <- sum(sorted_logMs[i:(k_vec + i - 1L)])
      if (log_prod < log_thresh) {
        delta_t <- i
      } else {
        break
      }
    }

    if (delta_t == 0L) {
      return(rep(0.0, W_now))
    }

    if (delta_t >= r_majority) {
      d_t <- W_now
    } else {
      d_t <- min(delta_t + a, W_now)
    }

    rems <- vapply(seats, function(s) s$rem, integer(1))
    ord  <- order(logMs)
    while (d_t < W_now && all(rems[ord[seq_len(d_t)]] <= 0L)) {
      d_t <- d_t + 1L
    }

    # --- Within d_t weakest, sample seats with P(theta>0.5) > tau ---
    lambdas <- rep(0.0, W_now)

    search_d <- d_t
    repeat {
      for (i in seq_len(search_d)) {
        c_idx <- ord[i]
        if (rems[c_idx] > 0L && post_prob[c_idx] > tau) {
          lambdas[c_idx] <- 1.0
        }
      }
      if (sum(lambdas) > 0 || search_d >= k_vec) break
      search_d <- search_d + 1L
    }

    if (sum(lambdas) == 0) {
      for (i in seq_len(k_vec)) {
        c_idx <- ord[i]
        if (rems[c_idx] > 0L) lambdas[c_idx] <- 1.0
      }
    }

    lambdas
  }
}


## ============================================================
## Global pooled sampling (one ballot globally per step)
## ============================================================

# Jack: Unused
draw_one_global <- function(seats, weights = NULL) {
  rems <- vapply(seats, function(s) s$rem, integer(1))
  
  if (is.null(weights)) {
    w <- as.numeric(rems)
  } else {
    if (!is.numeric(weights) || length(weights) != length(seats)) stop("'weights' must match length(seats).")
    w <- as.numeric(weights)
  }
  
  w[rems <= 0L] <- 0
  total_w <- sum(w)
  if (total_w <= 0) return(list(seats = seats, seat_idx = NA_integer_, x = NA_real_))
  
  seat_idx <- sample.int(length(seats), size = 1L, prob = w)
  out <- draw_from_seat(seats[[seat_idx]])
  seats[[seat_idx]] <- out$seat
  list(seats = seats, seat_idx = seat_idx, x = out$x)
}