## ============================================================
## plot_paper_figure.R -- Bespoke 2x3 grid for the paper
##
## Row 1: simulation_1_no_margin page 2 (W = 60, homogeneous margins)
## Row 2: simulation_2_no_margin page 5 (W = 60, kappa = 30, heterogeneous)
## Cols : n_false in {0, 3, 5}
##
## Each panel: total ballots sampled (mean +/- SD across reps) vs. true
## winning share, coloured by method. The y-axis is free per row so the
## two regimes (homogeneous / heterogeneous) can have appropriate scales.
##
## Usage: Rscript plot_paper_figure.R [output.pdf]
## ============================================================

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2", repos = "https://cloud.r-project.org")
}
library(ggplot2)

## -- Resolve paths relative to the script's directory, so this works
##    regardless of the working directory used to invoke Rscript. --
get_script_dir <- function() {
  argv <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", argv, value = TRUE)
  if (length(file_arg) > 0L) {
    dirname(normalizePath(sub("^--file=", "", file_arg[1])))
  } else {
    getwd()
  }
}
script_dir <- get_script_dir()

args <- commandArgs(trailingOnly = TRUE)
output_pdf <- if (length(args) >= 1L) args[1] else file.path(script_dir, "paper_figure.pdf")

W_TARGET     <- 60L
KAPPA_TARGET <- 30

## ============================================================
## Load and filter
##
## Result files are large (history columns can be huge); load only the
## files we actually need by filtering on filename pattern, and strip
## the history column immediately on read.
## ============================================================
load_files <- function(dir, pattern) {
  files <- list.files(dir, pattern = pattern, full.names = TRUE)
  if (length(files) == 0L) stop("No matching .rds files in ", dir)
  do.call(rbind, lapply(files, function(f) {
    d <- readRDS(f); d$history <- NULL; d
  }))
}

## sim_1: results_W60_palice<p>_nfalse<n>.rds
sim1_pat <- sprintf("^results_W%d_palice[0-9.]+_nfalse[0-9]+\\.rds$", W_TARGET)
sim1 <- load_files(file.path(script_dir, "simulation_1_no_margin/results"), sim1_pat)

## sim_2: results_W60_pmean<p>_kappa30_nfalse<n>.rds
sim2_pat <- sprintf("^results_W%d_pmean[0-9.]+_kappa%d_nfalse[0-9]+\\.rds$",
                    W_TARGET, KAPPA_TARGET)
sim2 <- load_files(file.path(script_dir, "simulation_2_no_margin/results"), sim2_pat)

r_majority <- floor(sim1$S[1] / 2) + 1L

## Only scenarios where the null is false (W - n_false >= r_majority).
sim1 <- sim1[sim1$W - sim1$n_false >= r_majority, ]
sim2 <- sim2[sim2$W - sim2$n_false >= r_majority, ]

if (nrow(sim1) == 0L) stop("No simulation_1 rows after filtering.")
if (nrow(sim2) == 0L) stop("No simulation_2 rows after filtering.")

## Harmonise the x-axis variable.
sim1$p     <- sim1$p_alice
sim2$p     <- sim2$p_mean
sim1$panel <- sprintf("Homogeneous (W = %d)", W_TARGET)
sim2$panel <- sprintf("Heterogeneous (W = %d, kappa = %d)", W_TARGET, KAPPA_TARGET)

keep <- c("method", "p", "n_false", "t_eval", "panel")
df   <- rbind(sim1[, keep], sim2[, keep])

## ============================================================
## Aggregate: mean +/- 2 SD per cell
## ============================================================
agg <- aggregate(t_eval ~ method + p + n_false + panel, data = df,
                 FUN = function(x) c(mean = mean(x), sd = sd(x)))
agg <- do.call(data.frame, agg)
names(agg)[names(agg) == "t_eval.mean"] <- "mean_eval"
names(agg)[names(agg) == "t_eval.sd"]   <- "sd_eval"

## Floor the lower whisker at 1 so it stays on the log scale even when
## (mean - 2*SD) would go non-positive.
agg$ymin <- pmax(agg$mean_eval - 2 * agg$sd_eval, 1)
agg$ymax <- agg$mean_eval + 2 * agg$sd_eval

## ============================================================
## Labels
## ============================================================
agg$margin_label <- sprintf("p = %.2f", agg$p)
agg$margin_label <- factor(agg$margin_label,
                            levels = sprintf("p = %.2f", sort(unique(agg$p))))

agg$false_label <- sprintf("%d incorrectly reported", agg$n_false)
agg$false_label <- factor(agg$false_label,
                           levels = sprintf("%d incorrectly reported",
                                            sort(unique(agg$n_false))))

agg$panel <- factor(
  agg$panel,
  levels = c(sprintf("Homogeneous (W = %d)", W_TARGET),
             sprintf("Heterogeneous (W = %d, kappa = %d)", W_TARGET, KAPPA_TARGET))
)

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

method_order  <- c("All seats", "Reported top-r seats", "Non-adaptive",
                   "Greedy", "Greedy (a=3)", "Filtered", "Greedy Filtered",
                   "Greedy Filtered (a=3)")
method_colors <- c("All seats" = "#D55E00", "Reported top-r seats" = "#882255",
                    "Non-adaptive" = "#0072B2",
                    "Greedy" = "#009E73", "Greedy (a=3)" = "#CC79A7",
                    "Filtered" = "#E69F00", "Greedy Filtered" = "#56B4E9",
                    "Greedy Filtered (a=3)" = "#F0E442")
agg$method <- factor(agg$method, levels = method_order)

## ============================================================
## Plot: pointrange (mean +/- 2 SD) on a log-scaled y-axis
## ============================================================
p <- ggplot(agg, aes(x = margin_label, y = mean_eval, colour = method)) +
  geom_pointrange(aes(ymin = ymin, ymax = ymax),
                  size = 0.3, linewidth = 0.45,
                  position = position_dodge(width = 0.6)) +
  facet_grid(panel ~ false_label, scales = "free_y") +
  scale_y_log10(labels = function(x) format(x, big.mark = ",",
                                            scientific = FALSE, trim = TRUE)) +
  scale_colour_manual(values = method_colors, drop = FALSE) +
  labs(
    x      = "True winning share",
    y      = "Total ballots sampled (mean +/- 2 SD, log scale)",
    colour = "Method"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position  = "bottom",
    legend.box       = "horizontal",
    strip.text       = element_text(face = "bold", size = 9),
    axis.text.x      = element_text(size = 8),
    axis.text.y      = element_text(size = 8),
    panel.grid.minor = element_blank(),
    panel.border     = element_rect(colour = "grey80", fill = NA, linewidth = 0.4)
  ) +
  guides(colour = guide_legend(nrow = 2, byrow = FALSE))

pdf(output_pdf, width = 9, height = 6)
print(p)
dev.off()
cat(sprintf("Figure saved: %s\n", output_pdf))
