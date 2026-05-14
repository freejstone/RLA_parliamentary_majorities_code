## ============================================================
## plot_india_results.R --- boxplot of India 2014 audit results
##
## Reads per-replicate rds files saved by run_india_audit_multi.R
## (one per replicate per n_false config, plus optional Full audit
## reps), and writes a boxplot PDF (one panel per n_false).
## Log y-scale to keep the All seats and Greedy (a=3) extremes visible.
##
## Usage:
##   Rscript plot_india_results.R                            # defaults
##   Rscript plot_india_results.R results_R                  # data dir
##   Rscript plot_india_results.R results_R comparison.pdf   # data dir + out
##   Rscript plot_india_results.R results_R out.pdf 10       # + R cap
## ============================================================

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2", repos = "https://cloud.r-project.org")
}
library(ggplot2)

## -- arguments --------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
results_dir <- if (length(args) >= 1) args[1] else "results_R"
output_pdf  <- if (length(args) >= 2) args[2] else "comparison.pdf"
R_max       <- if (length(args) >= 3) as.integer(args[3]) else Inf

stopifnot(dir.exists(results_dir))

## -- load replicate files ---------------------------------------------------
load_rds_by_pattern <- function(pattern) {
  files <- list.files(results_dir, pattern = pattern, full.names = TRUE)
  if (!length(files)) return(NULL)
  rep_num <- as.integer(sub(".*_rep([0-9]+)\\.rds$", "\\1", files))
  files <- files[order(rep_num)]
  if (is.finite(R_max)) files <- head(files, R_max)
  do.call(rbind, lapply(files, readRDS))
}

n_false_vals <- c(0, 3, 5)
nf_dat <- lapply(n_false_vals, function(nf)
  load_rds_by_pattern(
    sprintf("^results_india_multi_nfalse%d_rep[0-9]+\\.rds$", nf)))
nf_dat <- nf_dat[!sapply(nf_dat, is.null)]
df <- do.call(rbind, nf_dat)
if (is.null(df) || !nrow(df)) stop("No India n_false rds files found in ", results_dir)

full_df <- load_rds_by_pattern("^results_india_multi_full_rep[0-9]+\\.rds$")
if (!is.null(full_df)) df <- rbind(df, full_df)

## -- method name remapping (R name -> paper name) --------------------------
method_remap <- c(
  "Naive"                  = "Non-adaptive",
  "Greedy"                 = "Greedy",
  "Greedy (a=3)"           = "Greedy (a=3)",
  "Bayesian"               = "Filtered",
  "Greedy Bayesian"        = "Greedy Filtered",
  "Greedy Bayesian (a=3)"  = "Greedy Filtered (a=3)",
  "Top-r Naive"            = "Reported top-r seats",
  "Full audit"             = "All seats"
)
for (old in names(method_remap)) {
  df$method[df$method == old] <- method_remap[[old]]
}

cat("Replicates per (method, n_false):\n")
counts <- aggregate(t_eval ~ method + n_false, data = df, FUN = length)
names(counts)[3] <- "n_rep"
print(reshape(counts, idvar = "method", timevar = "n_false", direction = "wide"),
      row.names = FALSE)
R_used <- max(counts$n_rep)

## -- factor ordering / colour palette (matches simulation plots) -----------
method_order <- c("All seats", "Reported top-r seats", "Non-adaptive",
                  "Greedy", "Greedy (a=3)", "Filtered",
                  "Greedy Filtered", "Greedy Filtered (a=3)")
df$method <- factor(df$method, levels = method_order)

method_colors <- c("All seats" = "#D55E00", "Reported top-r seats" = "#882255",
                   "Non-adaptive" = "#0072B2",
                   "Greedy" = "#009E73", "Greedy (a=3)" = "#CC79A7",
                   "Filtered" = "#E69F00", "Greedy Filtered" = "#56B4E9",
                   "Greedy Filtered (a=3)" = "#F0E442")

## All seats only certifies for n_false = 0 (true null otherwise) — drop the rest
df <- df[!(df$method == "All seats" & df$n_false != 0), ]

## -- plot -------------------------------------------------------------------
df$nfalse_label <- factor(sprintf("%d incorrectly reported", df$n_false),
                          levels = sprintf("%d incorrectly reported",
                                           sort(unique(df$n_false))))

p <- ggplot(df, aes(x = method, y = t_eval, fill = method)) +
  geom_boxplot(outlier.size = 1.2, outlier.alpha = 0.7, linewidth = 0.4) +
  facet_wrap(~ nfalse_label, nrow = 1) +
  scale_fill_manual(values = method_colors, drop = FALSE) +
  scale_y_log10(labels = function(x)
    formatC(x, format = "d", big.mark = ",")) +
  labs(
    x      = NULL,
    y      = "Total ballots sampled (log scale)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "none",
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 9),
    strip.text       = element_text(face = "bold", size = 11),
    plot.title       = element_text(face = "bold", hjust = 0.5, size = 14),
    plot.subtitle    = element_text(hjust = 0.5, size = 10),
    panel.grid.minor = element_blank(),
    panel.border     = element_rect(colour = "grey80", fill = NA, linewidth = 0.5)
  )

pdf(output_pdf, width = 11, height = 6)
print(p)
dev.off()
cat(sprintf("Figure saved: %s\n", output_pdf))
