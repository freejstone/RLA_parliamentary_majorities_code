## ============================================================
## plot_workload.R — Plot cumulative ballots sampled vs round
## Noisy reported margins version (simulation_3)
## Usage: Rscript plot_workload.R [results_dir] [output.pdf]
## ============================================================

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2", repos = "https://cloud.r-project.org")
}
library(ggplot2)

args <- commandArgs(trailingOnly = TRUE)
results_dir <- if (length(args) >= 1) args[1] else "results"
output_pdf  <- if (length(args) >= 2) args[2] else "workload.pdf"

rds_files <- list.files(results_dir, pattern = "\\.rds$", full.names = TRUE)
if (length(rds_files) == 0) stop("No .rds files found in ", results_dir)

alpha <- 0.05

## -- Extract history: t_eval as a function of t_round --
## Process one file at a time to avoid loading all history into memory.
N_GRID <- 500L

summary_rows <- list()
row_id <- 0L

for (fi in seq_along(rds_files)) {
  df_file <- readRDS(rds_files[fi])

  ## Determine r_majority from first file
  if (fi == 1L) r_majority <- floor(df_file$S[1] / 2) + 1L

  ## Keep only scenarios where the null is false
  df_file <- df_file[df_file$W - df_file$n_false >= r_majority, ]
  if (nrow(df_file) == 0) next

  combos <- unique(df_file[, c("method", "W", "p_mean", "p_spread", "eps", "n_false")])

  for (i in seq_len(nrow(combos))) {
    m  <- combos$method[i]
    w  <- combos$W[i]
    pm <- combos$p_mean[i]
    ps <- combos$p_spread[i]
    ep <- combos$eps[i]
    nf <- combos$n_false[i]

    sub <- df_file[df_file$method == m & df_file$W == w & df_file$p_mean == pm &
              df_file$p_spread == ps & df_file$eps == ep & df_file$n_false == nf, ]
    if (nrow(sub) == 0) next

    histories <- list()
    for (j in seq_len(nrow(sub))) {
      h <- sub$history[[j]]
      if (!is.null(h) && !is.null(h$t_round) && length(h$t_round) > 0) {
        histories[[length(histories) + 1L]] <- h
      }
    }
    if (length(histories) == 0) next

    max_round <- max(vapply(histories, function(h) max(h$t_round), numeric(1)))
    grid <- unique(round(seq(1, max_round, length.out = N_GRID)))

    mat <- matrix(NA_real_, nrow = length(grid), ncol = length(histories))
    for (j in seq_along(histories)) {
      h <- histories[[j]]
      rnd_vals <- tapply(h$t_eval, h$t_round, max)
      rounds   <- as.integer(names(rnd_vals))
      evals    <- as.numeric(rnd_vals)
      sf <- approxfun(rounds, evals, method = "constant", rule = 2, f = 0)
      mat[, j] <- sf(grid)
    }

    row_id <- row_id + 1L
    summary_rows[[row_id]] <- data.frame(
      method   = m,
      W        = w,
      p_mean   = pm,
      p_spread = ps,
      eps      = ep,
      n_false  = nf,
      t_round  = grid,
      median   = apply(mat, 1, median),
      q25      = apply(mat, 1, quantile, probs = 0.25),
      q75      = apply(mat, 1, quantile, probs = 0.75),
      stringsAsFactors = FALSE
    )
  }
  rm(df_file); gc(verbose = FALSE)
}

if (length(summary_rows) == 0) stop("No history data found. Was keep_history = TRUE?")
hdf <- do.call(rbind, summary_rows)

## -- Labels --
hdf$mean_label   <- sprintf("mean p = %.2f", hdf$p_mean)
hdf$spread_label <- sprintf("kappa = %.0f", hdf$p_spread)
hdf$spread_label <- factor(hdf$spread_label,
                            levels = sprintf("kappa = %.0f", sort(unique(hdf$p_spread))))
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

## -- One PDF page per (kappa, W, n_false); panels = p_mean (stacked across rows) --
pages <- unique(hdf[, c("spread_label", "W_label", "false_label")])
pages <- pages[order(pages$spread_label, pages$W_label, pages$false_label), ]

n_margins <- length(unique(hdf$mean_label))
fig_width  <- 8
fig_height <- 2 + 2.5 * n_margins

pdf(output_pdf, width = fig_width, height = fig_height)

for (pg in seq_len(nrow(pages))) {
  sl <- pages$spread_label[pg]
  wl <- pages$W_label[pg]
  fl <- pages$false_label[pg]

  pdata <- hdf[hdf$spread_label == sl &
               hdf$W_label == wl & hdf$false_label == fl, ]
  if (nrow(pdata) == 0) next

  p <- ggplot(pdata, aes(x = t_round, colour = method, fill = method)) +
    geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.2, colour = NA) +
    geom_line(aes(y = median), linewidth = 0.6) +
    facet_wrap(~ mean_label, scales = "free", ncol = 1) +
    scale_colour_manual(values = method_colors, drop = FALSE) +
    scale_fill_manual(values = method_colors, drop = FALSE) +
    labs(
      title    = sprintf("Cumulative Ballots Sampled vs Round -- %s, %s, %s", sl, wl, fl),
      subtitle = "Median with 25th-75th percentile ribbon",
      x      = "Round (t_round)",
      y      = "Cumulative ballots sampled (t_eval)",
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
