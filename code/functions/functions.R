## ============================================================
## Seat generator + within-seat sampling (without replacement)
## Two-candidate plurality assorter values: X in {1, 0, 1/2}
## ============================================================

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
    logM = 0.0
  )
}


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
  } else if (cat_idx == 2L) {
    x <- 0.0
    seat$counts["B"] <- seat$counts["B"] - 1L
  } else {
    x <- 0.5
    seat$counts["O"] <- seat$counts["O"] - 1L
  }
  
  seat$rem <- seat$rem - 1L
  list(seat = seat, x = x)
}

## ============================================================
## ALPHA components
## ============================================================

eta_trunc_shrinkage <- function(k, sum_x, mu_next,
                                u = 1, eta0, d = 100, c = NULL, tiny = 1e-12) {
  # k = number of processed draws so far; next index is j = k+1
  stopif_pos(d, "d")
  if (!is.numeric(eta0) || length(eta0) != 1L || !is.finite(eta0)) stop("'eta0' must be finite scalar.")
  
  if (is.null(c)) {
    # simple default (tune later as you planned)
    c <- max(0, (eta0 - 0.5) / 2)
  }
  if (!is.numeric(c) || length(c) != 1L || !is.finite(c) || c < 0) stop("'c' must be finite scalar >= 0.")
  
  denom <- d + k
  eta_raw <- (d * eta0 + sum_x) / denom
  eps <- c / sqrt(denom)
  
  eta <- max(eta_raw, mu_next + eps)
  eta <- min(eta, u - tiny)
  eta
}

seat_update_ALPHA <- function(seat, x,
                              mu0 = 0.5,
                              u = 1.0,
                              lambda = 1.0,
                              eta_mode = c("trunc_shrinkage", "fixed"),
                              eta0 = 0.6,
                              d = 100,
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
  # Jack: Need to check if this is sensible
  mu_next <- min(mu_ub, u)
  
  # Degenerate/boundary handling: if mu_next is ~0 or ~u, do a conservative "no-bet" update (mult=1).
  if (mu_next <= tiny || (u - mu_next) <= tiny) {
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
      eta0 = eta0, d = d, c = c, tiny = tiny
    )
  )
  
  # ALPHA multiplier for the raw martingale T
  # mult_T = (1/u) * ( x*(eta/mu) + (u-x)*((u-eta)/(u-mu)) )
  mult_T <- (1 / u) * (x * (eta / mu_next) + (u - x) * ((u - eta) / (u - mu_next)))
  
  # Update logT
  if (!is.finite(mult_T) && mult_T > 0) {
    seat$logT <- Inf
  } else {
    if (mult_T <= 0) stop("ALPHA multiplier became nonpositive; check inputs.")
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
  
  if (!is.finite(mult_M) && mult_M > 0) {
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

log_Mr_from_seats <- function(seats, r_majority) {
  W <- length(seats)
  k <- W - r_majority + 1L
  if (k <= 0L) stop("Need W >= r_majority.")
  
  # IMPORTANT: use logM (seat-level M_{c,t,lambda}), not logT
  logs <- vapply(seats, function(s) s$logM, numeric(1))
  sum(sort(logs, decreasing = FALSE)[seq_len(k)])
}

## ============================================================
## Global pooled sampling (one ballot globally per step)
## ============================================================

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