## summarise_results.R
##
## Reads the per-replicate India audit results saved by
## run_india_audit_multi.R (one .rds file per replicate per n_false
## configuration), computes mean t_eval per (method x n_false) and
## prints both a readable summary and LaTeX-ready rows matching
## the table in main.tex.
##
## Usage:
##   Rscript summarise_results.R                  # defaults: results_R/, R = all
##   Rscript summarise_results.R results_R 10     # results_R/, R = 10 reps

suppressPackageStartupMessages({
  ## base only; no external deps
})

## -- arguments --------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
res_dir <- if (length(args) >= 1) args[1] else "results_R"
R_max   <- if (length(args) >= 2) as.integer(args[2]) else Inf

stopifnot(dir.exists(res_dir))

## -- method name mapping (R name -> paper name) -----------------------------
method_map <- c(
  "Naive"                  = "Non-adaptive",
  "Greedy"                 = "Greedy ($a = 0$)",
  "Greedy (a=3)"           = "Greedy ($a = 3$)",
  "Bayesian"               = "Filtered",
  "Greedy Bayesian"        = "Greedy Filtered ($a = 0$)",
  "Greedy Bayesian (a=3)"  = "Greedy Filtered ($a = 3$)",
  "Top-r Naive"            = "Reported top-$r$ seats"
)

## fixed row order for the table
row_order <- c(
  "Non-adaptive",
  "Greedy ($a = 0$)",
  "Greedy ($a = 3$)",
  "Filtered",
  "Greedy Filtered ($a = 0$)",
  "Greedy Filtered ($a = 3$)",
  "Reported top-$r$ seats"
)

## which methods are "parliamentary-majority schemes" (bold competes here)
maj_methods <- c(
  "Non-adaptive",
  "Greedy ($a = 0$)",
  "Greedy ($a = 3$)",
  "Filtered",
  "Greedy Filtered ($a = 0$)",
  "Greedy Filtered ($a = 3$)"
)

## -- load per-n_false replicates --------------------------------------------
load_nfalse <- function(nf) {
  files <- list.files(
    res_dir,
    pattern = sprintf("^results_india_multi_nfalse%d_rep[0-9]+\\.rds$", nf),
    full.names = TRUE
  )
  if (!length(files)) return(NULL)
  ## sort by rep number then take first R_max
  rep_num <- as.integer(sub(
    sprintf(".*nfalse%d_rep([0-9]+)\\.rds$", nf), "\\1", files
  ))
  files <- files[order(rep_num)]
  if (is.finite(R_max)) files <- head(files, R_max)
  do.call(rbind, lapply(files, readRDS))
}

n_false_vals <- c(0, 3, 5)
all_dat <- lapply(n_false_vals, load_nfalse)
names(all_dat) <- as.character(n_false_vals)
R_used <- sapply(all_dat, function(d) if (is.null(d)) 0 else
                 length(unique(d$t_eval)))   # approx reps via unique rows
## actual rep count = nrow / number of methods
n_methods_in_data <- length(unique(all_dat[[1]]$method))
R_used <- sapply(all_dat, function(d)
  if (is.null(d)) 0 else nrow(d) %/% length(unique(d$method)))

cat(sprintf("Loaded from: %s\n", res_dir))
for (i in seq_along(n_false_vals))
  cat(sprintf("  n_false = %d : R = %d replicates\n",
              n_false_vals[i], R_used[i]))

## -- aggregate (mean t_eval) ------------------------------------------------
agg_one <- function(d) {
  if (is.null(d)) return(NULL)
  a <- aggregate(t_eval ~ method, data = d,
                 FUN = function(x) c(mean = mean(x), sd = sd(x)))
  a <- do.call(data.frame, a)
  names(a)[names(a) == "t_eval.mean"] <- "mean_eval"
  names(a)[names(a) == "t_eval.sd"]   <- "sd_eval"
  a$sd_eval[is.na(a$sd_eval)] <- 0  # n=1 edge case
  a$method_paper <- method_map[a$method]
  a
}
agg <- lapply(all_dat, agg_one)

## helpers: get mean / sd for (n_false, method) in thousands
get_val_k <- function(nf, method_paper) {
  a <- agg[[as.character(nf)]]
  if (is.null(a)) return(NA_real_)
  x <- a$mean_eval[a$method_paper == method_paper]
  if (!length(x)) NA_real_ else x / 1e3
}
get_sd_k <- function(nf, method_paper) {
  a <- agg[[as.character(nf)]]
  if (is.null(a)) return(NA_real_)
  x <- a$sd_eval[a$method_paper == method_paper]
  if (!length(x)) NA_real_ else x / 1e3
}

## -- All seats (full audit) -------------------------------------------------
full_files <- list.files(
  res_dir,
  pattern = "^results_india_multi_full_rep[0-9]+\\.rds$",
  full.names = TRUE
)
if (length(full_files)) {
  rep_num <- as.integer(sub(".*full_rep([0-9]+)\\.rds$", "\\1", full_files))
  full_files <- full_files[order(rep_num)]
  if (is.finite(R_max)) full_files <- head(full_files, R_max)
  full_dat <- do.call(rbind, lapply(full_files, readRDS))
  full_mean_k <- mean(full_dat$t_eval) / 1e3
  full_sd_k   <- if (nrow(full_dat) > 1) sd(full_dat$t_eval) / 1e3 else 0
  cat(sprintf("\nAll seats (full audit), n_false = 0 : R = %d replicates\n",
              length(full_files)))
} else {
  full_mean_k <- NA_real_
  full_sd_k   <- NA_real_
}

## -- readable summary -------------------------------------------------------
cat("\n--- Mean +/- SD t_eval (thousands) ---\n")
print_tbl <- function() {
  hdr <- sprintf("%-30s  %18s  %18s  %18s", "Method",
                 "n_false=0", "n_false=3", "n_false=5")
  cat(hdr, "\n")
  cat(strrep("-", nchar(hdr)), "\n")
  fmt_int <- function(x) {
    if (is.na(x)) NA_character_ else formatC(round(x), format="d", big.mark=",")
  }
  fmt_pair <- function(mean_k, sd_k) {
    if (is.na(mean_k)) return("---")
    sprintf("%s +/- %s", fmt_int(mean_k), fmt_int(sd_k))
  }
  for (m in row_order) {
    means <- sapply(n_false_vals, get_val_k, method_paper = m)
    sds   <- sapply(n_false_vals, get_sd_k,  method_paper = m)
    cat(sprintf("%-30s  %18s  %18s  %18s\n", m,
                fmt_pair(means[1], sds[1]),
                fmt_pair(means[2], sds[2]),
                fmt_pair(means[3], sds[3])))
  }
  cat(sprintf("%-30s  %18s  %18s  %18s\n", "All seats",
              fmt_pair(full_mean_k, full_sd_k), "---", "---"))
}
print_tbl()

## -- LaTeX-ready rows -------------------------------------------------------
## Locate, per n_false, the minimum within the parliamentary-majority schemes
maj_min_by_nf <- sapply(n_false_vals, function(nf) {
  vs <- sapply(maj_methods, get_val_k, nf = nf)
  if (all(is.na(vs))) NA_real_ else min(vs, na.rm = TRUE)
})
names(maj_min_by_nf) <- as.character(n_false_vals)

## LaTeX-friendly integer with thousands sep that escapes the comma
fmt_tex_int <- function(x) {
  if (is.na(x)) return(NA_character_)
  s <- formatC(round(x), format = "d", big.mark = "{,}")
  ## formatC already uses "," — we replace with TeX-safe "{,}"
  gsub(",", "{,}", formatC(round(x), format = "d", big.mark = ","), fixed = TRUE)
}

## LaTeX cell: mean +/- sd, optionally with the mean bolded
fmt_tex_cell <- function(mean_k, sd_k, is_min) {
  if (is.na(mean_k)) return("---")
  m_str <- fmt_tex_int(mean_k)
  s_str <- fmt_tex_int(sd_k)
  if (isTRUE(is_min)) m_str <- sprintf("\\mathbf{%s}", m_str)
  sprintf("$%s \\pm %s$", m_str, s_str)
}

cat("\n--- LaTeX rows (paste into main.tex) ---\n")
for (m in row_order) {
  cells <- sapply(n_false_vals, function(nf) {
    v  <- get_val_k(nf, m)
    sd <- get_sd_k(nf, m)
    target <- maj_min_by_nf[[as.character(nf)]]
    is_min <- (m %in% maj_methods) &&
              !is.na(v) && isTRUE(all.equal(v, target))
    fmt_tex_cell(v, sd, is_min)
  })
  if (m == "Reported top-$r$ seats") cat("\\midrule\n")
  cat(sprintf("%-26s & %-30s & %-30s & %-30s \\\\\n", m,
              cells[1], cells[2], cells[3]))
}
cat(sprintf("%-26s & %-30s & %-30s & %-30s \\\\\n", "All seats",
            fmt_tex_cell(full_mean_k, full_sd_k, FALSE), "---", "---"))
