## ============================================================
## plot_results.R — Read saved RDS files and produce a PDF
## Homogeneous margins + noisy reports (simulation_4)
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

## -- Group W=51,52 onto one page; separate pages for the rest --
slim_W  <- c(51, 52)
other_W <- sort(setdiff(unique(agg$W), slim_W))

plots <- list()

## --- Combined page for W = 51, 52: panels stacked across rows by W ---
dsub <- agg[agg$W %in% slim_W, ]
if (nrow(dsub) > 0) {
  n_W_slim <- length(unique(dsub$W))
  p <- ggplot(dsub, aes(x = margin_label, y = mean_eval, colour = method)) +
    geom_pointrange(aes(ymin = mean_eval - sd_eval, ymax = mean_eval + sd_eval),
                    size = 0.35, linewidth = 0.5,
                    position = position_dodge(width = 0.6)) +
    facet_grid(W_label ~ ., scales = "free_y") +
    scale_colour_manual(values = method_colors, drop = FALSE) +
    labs(
      title    = sprintf("Homogeneous Margins -- W = %s",
                          paste(slim_W, collapse = ", ")),
      subtitle = sprintf("S = %d total seats,  N = %d ballots/seat,  alpha = 0.05",
                          df$S[1], df$N[1]),
      x      = "True winning share",
      y      = "Total ballots sampled (mean +/- SD)",
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
  plots[["slim"]] <- list(plot = p,
                           width  = 6.5,
                           height = 3 + 3 * n_W_slim)
}

## --- Separate pages for other W values: panels stacked across rows by n_false ---
for (w_val in other_W) {
  dsub <- agg[agg$W == w_val, ]
  if (nrow(dsub) == 0) next
  n_false_w <- length(unique(dsub$n_false))
  p <- ggplot(dsub, aes(x = margin_label, y = mean_eval, colour = method)) +
    geom_pointrange(aes(ymin = mean_eval - sd_eval, ymax = mean_eval + sd_eval),
                    size = 0.35, linewidth = 0.5,
                    position = position_dodge(width = 0.6)) +
    facet_grid(false_label ~ ., scales = "free_y") +
    scale_colour_manual(values = method_colors, drop = FALSE) +
    labs(
      title    = sprintf("Homogeneous Margins -- W = %d", w_val),
      subtitle = sprintf("S = %d total seats,  N = %d ballots/seat,  alpha = 0.05",
                          df$S[1], df$N[1]),
      x      = "True winning share",
      y      = "Total ballots sampled (mean +/- SD)",
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
  plots[[as.character(w_val)]] <- list(plot = p,
                                        width  = 6.5,
                                        height = 3 + 3 * n_false_w)
}

## -- Write each page as a separate temp PDF, then combine --
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
cat(sprintf("Figure saved: %s (%d pages)\n", output_pdf, length(plots)))
