## ============================================================
## plot_results.R — Read saved RDS files and produce a PDF
## Two-candidate plurality, homogeneous margins, no-margin variant
## (simulation_1_no_margin)
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

df <- do.call(rbind, lapply(rds_files, readRDS))

## -- Keep only scenarios where the null is false (certification is correct) --
r_majority <- floor(df$S[1] / 2) + 1L
df <- df[df$W - df$n_false >= r_majority, ]
if (nrow(df) == 0) stop("No scenarios with false null remain after filtering.")

## -- Summarise: mean ± SD per (method, W, p_alice, eps, n_false) --
agg <- aggregate(t_eval ~ method + W + p_alice + eps + n_false,
                 data = df, FUN = function(x) c(mean = mean(x), sd = sd(x)))
agg <- do.call(data.frame, agg)
names(agg)[names(agg) == "t_eval.mean"] <- "mean_eval"
names(agg)[names(agg) == "t_eval.sd"]   <- "sd_eval"

## -- Whiskers: floor the lower end at 1 so it stays on the log scale --
agg$ymin <- pmax(agg$mean_eval - 2 * agg$sd_eval, 1)
agg$ymax <- agg$mean_eval + 2 * agg$sd_eval

## -- Labels --
agg$false_label <- sprintf("%d incorrectly reported", agg$n_false)
agg$false_label <- factor(agg$false_label,
                           levels = sprintf("%d incorrectly reported",
                                            sort(unique(agg$n_false))))
agg$W_label     <- sprintf("W = %d", agg$W)
agg$W_label     <- factor(agg$W_label,
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
agg$margin_label <- sprintf("p = %.2f", agg$p_alice)
agg$margin_label <- factor(agg$margin_label,
                            levels = sprintf("p = %.2f", sort(unique(agg$p_alice))))

## -- Single 3x3 grid: rows = W (51, 60, 80), columns = n_false (0, 3, 5) --
grid_W <- c(51, 60, 80)
dsub <- agg[agg$W %in% grid_W, ]
if (nrow(dsub) == 0) stop("No scenarios remain for W in {51, 60, 80}.")

dsub$W_label <- factor(sprintf("W = %d", dsub$W),
                       levels = sprintf("W = %d", grid_W))

n_W_grid     <- length(grid_W)
n_false_grid <- length(unique(agg$n_false))

## Lower y-axis limit (just under 1000, matching the main-text figure). Rows
## whose natural minimum sits below this are clamped to it; over-long lower
## whiskers then run off the bottom of the panel (clipped) instead of dragging
## the log axis down to 1.
y_lower_floor <- 900

## facet_grid keeps every row/column combination, so filtered-out cells
## (e.g. W = 51 with n_false = 3, 5) appear as blank panels.
p <- ggplot(dsub, aes(x = margin_label, y = mean_eval, colour = method)) +
  geom_pointrange(aes(ymin = ymin, ymax = ymax),
                  size = 0.35, linewidth = 0.5,
                  position = position_dodge(width = 0.6)) +
  facet_grid(W_label ~ false_label, scales = "free_y", drop = FALSE) +
  scale_y_log10(
    limits = function(l) c(max(l[1], y_lower_floor), l[2]),
    oob    = scales::oob_keep,
    labels = function(x) format(x, big.mark = ",",
                                scientific = FALSE, trim = TRUE)) +
  scale_colour_manual(values = method_colors, drop = FALSE) +
  labs(
    title    = sprintf("Homogeneous Margins -- W = %s",
                        paste(grid_W, collapse = ", ")),
    subtitle = sprintf("S = %d total seats,  N = %d ballots/seat,  alpha = 0.05",
                        df$S[1], df$N[1]),
    x      = "True winning share",
    y      = "Total ballots sampled (mean +/- 2 SD, log scale)",
    colour = "Method"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "bottom",
    strip.text       = element_text(face = "bold", size = 11),
    plot.title       = element_text(face = "bold", hjust = 0.5, size = 14),
    plot.subtitle    = element_text(hjust = 0.5, size = 10),
    panel.grid.minor = element_blank(),
    panel.border     = element_rect(colour = "grey80", fill = NA, linewidth = 0.5)
  )

## -- Write the single-page 3x3 grid --
pdf(output_pdf, width = 4 + 3 * n_false_grid, height = 3 + 3 * n_W_grid)
print(p)
dev.off()
cat(sprintf("Figure saved: %s (1 page, 3x3 grid)\n", output_pdf))
