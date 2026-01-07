## ============================================================
## Helpers
## ============================================================

stopif_pos <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x <= 0) {
    stop(sprintf("'%s' must be a positive finite scalar.", name))
  }
}

stopif_prob <- function(p, name) {
  if (!is.numeric(p) || length(p) != 1L || !is.finite(p) || p < 0 || p > 1) {
    stop(sprintf("'%s' must be a finite scalar in [0,1].", name))
  }
}
