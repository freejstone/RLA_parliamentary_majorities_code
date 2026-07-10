## ============================================================
## plot_results.R — Read saved RDS files and produce a PDF
## Noisy reported margins version (simulation_3)
## Usage: Rscript plot_results.R [results_dir] [output.pdf]
## ============================================================

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2", repos = "https://cloud.r-project.org")
}
library(ggplot2)

args <- commandArgs(trailingOnly = TRUE)
results_dir <- if (length(args) >= 1) args[1] else "results"
output_pdf  <- if (length(args) >= 2) args[2] else "comparison.pdf"

rds_files <- list.files(results_dir, pattern = "\\.rds$", full.names = TRUE)
if (length(rds_files) == 0) stop("No .rds files found in ", results_dir)

## -- Drop history column immediately to avoid blowing up memory --
df <- do.call(rbind, lapply(rds_files, function(f) {
  d <- readRDS(f); d$history <- NULL; d
}))

## -- Keep only scenarios where the null is false (certification is correct) --
## Null H^{r/W} is false when Alice truly won >= r seats, i.e. W - n_false >= r
r_majority <- floor(df$S[1] / 2) + 1L
df <- df[df$W - df$n_false >= r_majority, ]
if (nrow(df) == 0) stop("No scenarios with false null remain after filtering.")

## -- Labels --
## -- Summarise: mean ± SD per (method, W, p_mean, p_spread, eps, n_false) --
agg <- aggregate(t_eval ~ method + W + p_mean + p_spread + eps + n_false,
                 data = df, FUN = function(x) c(mean = mean(x), sd = sd(x)))
agg <- do.call(data.frame, agg)
names(agg)[names(agg) == "t_eval.mean"] <- "mean_eval"
names(agg)[names(agg) == "t_eval.sd"]   <- "sd_eval"

## -- Whiskers: floor the lower end at 1 so it stays on the log scale --
agg$ymin <- pmax(agg$mean_eval - agg$sd_eval, 1)
agg$ymax <- agg$mean_eval + agg$sd_eval

## -- Labels --
agg$false_label  <- sprintf("%d incorrect", agg$n_false)
agg$false_label  <- factor(agg$false_label,
                            levels = sprintf("%d incorrect", sort(unique(agg$n_false))))
agg$spread_label <- sprintf("kappa = %.0f", agg$p_spread)
agg$spread_label <- factor(agg$spread_label,
                            levels = sprintf("kappa = %.0f", sort(unique(agg$p_spread))))
agg$W_label      <- sprintf("W = %d", agg$W)
agg$W_label      <- factor(agg$W_label,
                            levels = sprintf("W = %d", sort(unique(agg$W))))

method_remap <- c(
  "Naive"                 = "Non-adaptive",
  "Bayesian"              = "Filtered",
  "Greedy Bayesian"       = "Greedy Filtered",
  "Greedy Bayesian (a=3)" = "Greedy Filtered (a=3)",
  "Top-r Naive"           = "Reported top-r seats",
  "Full audit"            = "All seats"
)
for (old in names(method_remap)) {
  agg$method[agg$method == old] <- method_remap[[old]]
}

method_order <- c("All seats", "Reported top-r seats", "Non-adaptive", "Greedy",
                   "Greedy (a=3)", "Filtered", "Greedy Filtered",
                   "Greedy Filtered (a=3)")
agg$method <- factor(agg$method, levels = method_order)

method_colors <- c("All seats" = "#D55E00", "Reported top-r seats" = "#882255",
                    "Non-adaptive" = "#0072B2",
                    "Greedy" = "#009E73", "Greedy (a=3)" = "#CC79A7",
                    "Filtered" = "#E69F00", "Greedy Filtered" = "#56B4E9",
                    "Greedy Filtered (a=3)" = "#F0E442")

## -- Ensure all facet combinations exist (empty panels kept) --
agg$mean_label <- sprintf("mean p = %.2f", agg$p_mean)
agg$mean_label <- factor(agg$mean_label,
                          levels = sprintf("mean p = %.2f", sort(unique(agg$p_mean))))

## -- One 3x3 grid per kappa: rows = W (51, 60, 80), cols = n_false (0, 3, 5) --
grid_W       <- c(51, 60, 80)
n_W_grid     <- length(grid_W)
n_false_grid <- length(unique(agg$n_false))

plots <- list()

for (k_val in sort(unique(agg$p_spread))) {
  dsub <- agg[agg$W %in% grid_W & agg$p_spread == k_val, ]
  if (nrow(dsub) == 0) next

  dsub$W_label <- factor(sprintf("W = %d", dsub$W),
                         levels = sprintf("W = %d", grid_W))

  ## facet_grid keeps every row/column combination, so filtered-out cells
  ## (e.g. W = 51 with n_false = 3, 5) appear as blank panels.
  p <- ggplot(dsub, aes(x = mean_label, y = mean_eval, colour = method)) +
    geom_pointrange(aes(ymin = ymin, ymax = ymax),
                    size = 0.35, linewidth = 0.5,
                    position = position_dodge(width = 0.6)) +
    facet_grid(W_label ~ false_label, scales = "free_y", drop = FALSE) +
    scale_y_log10(labels = function(x) format(x, big.mark = ",",
                                              scientific = FALSE, trim = TRUE)) +
    scale_colour_manual(values = method_colors, drop = FALSE) +
    labs(
      title    = sprintf("Heterogeneous Margins -- W = %s,  kappa = %.0f",
                          paste(grid_W, collapse = ", "), k_val),
      subtitle = sprintf("S = %d,  N = %d ballots/seat,  alpha = 0.05",
                          df$S[1], df$N[1]),
      x      = "Mean winning share",
      y      = "Total ballots sampled (mean +/- SD, log scale)",
      colour = "Method"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position  = "bottom",
      strip.text       = element_text(face = "bold", size = 10),
      plot.title       = element_text(face = "bold", hjust = 0.5, size = 14),
      plot.subtitle    = element_text(hjust = 0.5, size = 10),
      panel.grid.minor = element_blank(),
      panel.border     = element_rect(colour = "grey80", fill = NA, linewidth = 0.5)
    )
  plots[[as.character(k_val)]] <- list(plot = p,
                                       width  = 4 + 3 * n_false_grid,
                                       height = 3 + 3 * n_W_grid)
}

## -- Write each kappa's 3x3 grid as a separate temp PDF, then combine --
tmp_files <- character(0)
for (nm in names(plots)) {
  item <- plots[[nm]]
  tmp <- tempfile(fileext = ".pdf")
  pdf(tmp, width = item$width, height = item$height)
  print(item$plot)
  dev.off()
  tmp_files <- c(tmp_files, tmp)
}
if (requireNamespace("qpdf", quietly = TRUE)) {
  qpdf::pdf_combine(tmp_files, output_pdf)
} else {
  gs_cmd <- sprintf("gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=%s %s",
                     shQuote(output_pdf), paste(shQuote(tmp_files), collapse = " "))
  system(gs_cmd)
}
file.remove(tmp_files)
cat(sprintf("Figure saved: %s (%d pages, one 3x3 grid per kappa)\n",
            output_pdf, length(plots)))
