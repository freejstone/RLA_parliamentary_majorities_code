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

## -- Labels --
agg$eps_label    <- sprintf("eps = %.2f", agg$eps)
agg$eps_label    <- factor(agg$eps_label,
                            levels = sprintf("eps = %.2f", sort(unique(agg$eps))))
agg$false_label  <- sprintf("%d incorrect", agg$n_false)
agg$false_label  <- factor(agg$false_label,
                            levels = sprintf("%d incorrect", sort(unique(agg$n_false))))
agg$spread_label <- sprintf("kappa = %.0f", agg$p_spread)
agg$spread_label <- factor(agg$spread_label,
                            levels = sprintf("kappa = %.0f", sort(unique(agg$p_spread))))
agg$W_label      <- sprintf("W = %d", agg$W)
agg$W_label      <- factor(agg$W_label,
                            levels = sprintf("W = %d", sort(unique(agg$W))))

method_order <- c("Full audit", "Top-r Naive", "Naive", "Greedy",
                   "Greedy (a=5%)", "Bayesian", "Greedy Bayesian")
agg$method <- factor(agg$method, levels = method_order)

method_colors <- c("Full audit" = "#D55E00", "Top-r Naive" = "#882255",
                    "Naive" = "#0072B2",
                    "Greedy" = "#009E73", "Greedy (a=5%)" = "#CC79A7",
                    "Bayesian" = "#E69F00", "Greedy Bayesian" = "#56B4E9")

## -- Ensure all facet combinations exist (empty panels kept) --
agg$mean_label <- sprintf("mean p = %.2f", agg$p_mean)
agg$mean_label <- factor(agg$mean_label,
                          levels = sprintf("mean p = %.2f", sort(unique(agg$p_mean))))

## -- Group W=51,52 onto one page; separate pages for the rest --
slim_W  <- c(51, 52)
other_W <- sort(setdiff(unique(agg$W), slim_W))

plots <- list()

for (k_val in sort(unique(agg$p_spread))) {
  ## --- Combined page for W = 51, 52: rows = W, cols = eps ---
  dsub <- agg[agg$W %in% slim_W & agg$p_spread == k_val, ]
  if (nrow(dsub) > 0) {
    n_eps_slim <- length(unique(dsub$eps))
    n_W_slim   <- length(unique(dsub$W))
    p <- ggplot(dsub, aes(x = mean_label, y = mean_eval, colour = method)) +
      geom_pointrange(aes(ymin = mean_eval - sd_eval, ymax = mean_eval + sd_eval),
                      size = 0.35, linewidth = 0.5,
                      position = position_dodge(width = 0.6)) +
      facet_grid(W_label ~ eps_label, scales = "free_y") +
      scale_colour_manual(values = method_colors, drop = FALSE) +
      labs(
        title    = sprintf("Heterogeneous Margins -- W = %s,  kappa = %.0f",
                            paste(slim_W, collapse = ", "), k_val),
        subtitle = sprintf("S = %d,  N = %d ballots/seat,  alpha = 0.05",
                            df$S[1], df$N[1]),
        x      = "Mean winning share",
        y      = "Total ballots sampled (mean +/- SD)",
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
    plots[[paste("slim", k_val)]] <- list(plot = p,
                                           width  = 3 + 3.5 * n_eps_slim,
                                           height = 3 + 3 * n_W_slim)
  }

  ## --- Separate pages for other W values ---
  for (w_val in other_W) {
    dsub <- agg[agg$W == w_val & agg$p_spread == k_val, ]
    if (nrow(dsub) == 0) next
    n_eps_w   <- length(unique(dsub$eps))
    n_false_w <- length(unique(dsub$n_false))
    p <- ggplot(dsub, aes(x = mean_label, y = mean_eval, colour = method)) +
      geom_pointrange(aes(ymin = mean_eval - sd_eval, ymax = mean_eval + sd_eval),
                      size = 0.35, linewidth = 0.5,
                      position = position_dodge(width = 0.6)) +
      facet_grid(false_label ~ eps_label, scales = "free_y") +
      scale_colour_manual(values = method_colors, drop = FALSE) +
      labs(
        title    = sprintf("Heterogeneous Margins -- W = %d,  kappa = %.0f", w_val, k_val),
        subtitle = sprintf("S = %d,  N = %d ballots/seat,  alpha = 0.05",
                            df$S[1], df$N[1]),
        x      = "Mean winning share",
        y      = "Total ballots sampled (mean +/- SD)",
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
    plots[[paste(w_val, k_val)]] <- list(plot = p,
                                          width  = 3 + 3.5 * n_eps_w,
                                          height = 3 + 3 * n_false_w)
  }
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
