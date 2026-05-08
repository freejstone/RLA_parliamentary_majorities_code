## ============================================================
## plot_history.R — Plot parliament test statistic over time (t_eval)
## Homogeneous margins + noisy reports (simulation_4)
## Usage: Rscript plot_history.R [results_dir] [output.pdf]
## ============================================================

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2", repos = "https://cloud.r-project.org")
}
library(ggplot2)

args <- commandArgs(trailingOnly = TRUE)
results_dir <- if (length(args) >= 1) args[1] else "results"
output_pdf  <- if (length(args) >= 2) args[2] else "history.pdf"

rds_files <- list.files(results_dir, pattern = "\\.rds$", full.names = TRUE)
if (length(rds_files) == 0) stop("No .rds files found in ", results_dir)

df <- do.call(rbind, lapply(rds_files, readRDS))

## -- Keep only scenarios where the null is false --
r_majority <- floor(df$S[1] / 2) + 1L
df <- df[df$W - df$n_false >= r_majority, ]
if (nrow(df) == 0) stop("No scenarios with false null remain after filtering.")

alpha <- 0.05
log_thresh <- log(1 / alpha)

## -- Extract history and build summary ribbon across replications --
N_GRID <- 500L

combos <- unique(df[, c("method", "W", "p_alice", "eps", "n_false")])
summary_rows <- list()
row_id <- 0L

for (i in seq_len(nrow(combos))) {
  m  <- combos$method[i]
  w  <- combos$W[i]
  pa <- combos$p_alice[i]
  ep <- combos$eps[i]
  nf <- combos$n_false[i]

  sub <- df[df$method == m & df$W == w & df$p_alice == pa &
            df$eps == ep & df$n_false == nf, ]
  if (nrow(sub) == 0) next

  histories <- list()
  for (j in seq_len(nrow(sub))) {
    h <- sub$history[[j]]
    if (!is.null(h) && !is.null(h$logMr) && length(h$logMr) > 0) {
      histories[[length(histories) + 1L]] <- h
    }
  }
  if (length(histories) == 0) next

  max_t <- max(vapply(histories, function(h) max(h$t_eval), numeric(1)))
  grid  <- unique(round(seq(1, max_t, length.out = N_GRID)))

  mat <- matrix(NA_real_, nrow = length(grid), ncol = length(histories))
  for (j in seq_along(histories)) {
    h <- histories[[j]]
    sf <- approxfun(h$t_eval, h$logMr, method = "constant", rule = 2, f = 0)
    mat[, j] <- sf(grid)
  }

  row_id <- row_id + 1L
  summary_rows[[row_id]] <- data.frame(
    method  = m,
    W       = w,
    p_alice = pa,
    eps     = ep,
    n_false = nf,
    t_eval  = grid,
    median  = apply(mat, 1, median),
    q25     = apply(mat, 1, quantile, probs = 0.25),
    q75     = apply(mat, 1, quantile, probs = 0.75),
    stringsAsFactors = FALSE
  )
}

if (length(summary_rows) == 0) stop("No history data found. Was keep_history = TRUE?")
hdf <- do.call(rbind, summary_rows)

## -- Labels --
hdf$margin_label <- sprintf("p = %.2f", hdf$p_alice)
hdf$W_label      <- sprintf("W = %d", hdf$W)
hdf$W_label      <- factor(hdf$W_label,
                            levels = sprintf("W = %d", sort(unique(hdf$W))))
hdf$false_label  <- sprintf("%d incorrectly reported", hdf$n_false)
hdf$false_label  <- factor(hdf$false_label,
                            levels = sprintf("%d incorrectly reported",
                                             sort(unique(hdf$n_false))))

method_remap <- c(
  "Naive"                 = "Non-adaptive",
  "Bayesian"              = "Filtered",
  "Greedy Bayesian"       = "Greedy Filtered",
  "Greedy Bayesian (a=3)" = "Greedy Filtered (a=3)",
  "Top-r Naive"           = "Reported top-r seats",
  "Full audit"            = "All seats"
)
for (old in names(method_remap)) {
  hdf$method[hdf$method == old] <- method_remap[[old]]
}
method_order  <- c("All seats", "Reported top-r seats", "Non-adaptive", "Greedy", "Greedy (a=3)", "Filtered", "Greedy Filtered", "Greedy Filtered (a=3)")
method_colors <- c("All seats" = "#D55E00", "Reported top-r seats" = "#882255",
                    "Non-adaptive" = "#0072B2",
                    "Greedy" = "#009E73", "Greedy (a=3)" = "#CC79A7",
                    "Filtered" = "#E69F00", "Greedy Filtered" = "#56B4E9",
                    "Greedy Filtered (a=3)" = "#F0E442")
hdf$method <- factor(hdf$method, levels = method_order)

## -- One page per (W, n_false); panels = p_alice (stacked across rows) --
pages <- unique(hdf[, c("W_label", "false_label")])
pages <- pages[order(pages$W_label, pages$false_label), ]

n_margins <- length(unique(hdf$margin_label))
fig_width  <- 8
fig_height <- 2 + 2.5 * n_margins

pdf(output_pdf, width = fig_width, height = fig_height)

for (pg in seq_len(nrow(pages))) {
  wl <- pages$W_label[pg]
  fl <- pages$false_label[pg]

  pdata <- hdf[hdf$W_label == wl & hdf$false_label == fl, ]
  if (nrow(pdata) == 0) next

  p <- ggplot(pdata, aes(x = t_eval, colour = method, fill = method)) +
    geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.2, colour = NA) +
    geom_line(aes(y = median), linewidth = 0.6) +
    geom_hline(yintercept = log_thresh, linetype = "dashed", colour = "red",
               linewidth = 0.6) +
    annotate("text", x = -Inf, y = log_thresh, label = sprintf("log(1/alpha) = %.2f", log_thresh),
             hjust = -0.05, vjust = -0.5, colour = "red", size = 3) +
    facet_wrap(~ margin_label, scales = "free_x", ncol = 1) +
    scale_colour_manual(values = method_colors, drop = FALSE) +
    scale_fill_manual(values = method_colors, drop = FALSE) +
    labs(
      title    = sprintf("Parliament Test Statistic vs Ballots -- %s, %s", wl, fl),
      subtitle = "Median with 25th-75th percentile ribbon",
      x      = "Total ballots sampled (t_eval)",
      y      = expression(log(M[r/W])),
      colour = "Method",
      fill   = "Method"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position  = "bottom",
      strip.text       = element_text(face = "bold", size = 11),
      plot.title       = element_text(face = "bold", hjust = 0.5, size = 13),
      plot.subtitle    = element_text(hjust = 0.5, size = 10),
      panel.grid.minor = element_blank()
    )

  print(p)
}

dev.off()
cat(sprintf("Figure saved: %s\n", output_pdf))
