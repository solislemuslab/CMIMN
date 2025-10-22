## =============================
## CMIMN stability across normalizations
## Original data + provided bootstraps (.csv or .rds)
## =============================

rm(list = ls())
set.seed(123)

# ---- libs ----
library(SpiecEasi)
library(CMIMN)
library(compositions)
library(igraph)
library(GMPR)

# ---- helpers ----
f1_from_graphs <- function(g_true, g_pred) {
  edgelab <- function(g) {
    e <- get.edgelist(g)
    if (is.null(e) || nrow(e) == 0) return(character(0))
    apply(cbind(pmin(e[,1], e[,2]), pmax(e[,1], e[,2])), 1, paste, collapse="--")
  }
  tset <- edgelab(g_true); pset <- edgelab(g_pred)
  tp <- length(intersect(tset, pset))
  fp <- length(setdiff(pset, tset))
  fn <- length(setdiff(tset, pset))
  precision <- if ((tp + fp) == 0) 0 else tp / (tp + fp)
  recall    <- if ((tp + fn) == 0) 0 else tp / (tp + fn)
  if ((precision + recall) == 0) return(0)
  2 * precision * recall / (precision + recall)
}

to_mat <- function(X) {
  M <- as.matrix(X)
  storage.mode(M) <- "double"
  if (is.null(rownames(M))) rownames(M) <- paste0("S", seq_len(nrow(M)))
  if (is.null(colnames(M))) colnames(M) <- paste0("V", seq_len(ncol(M)))
  M
}

# GMPR wrapper (auto-orients, returns samples in rows)
norm_gmpr <- function(x) {
  X <- as.matrix(x)  # rows = samples, cols = taxa
  sf <- try(GMPR::GMPR(as.data.frame(X)), silent = TRUE)
  if (inherits(sf, "try-error") || is.null(sf) || length(sf) != nrow(X)) {
    sf <- GMPR::GMPR(as.data.frame(t(X)))
  }
  sf[!is.finite(sf) | sf <= 0] <- 1
  sweep(X, 1, sf, "/")
}

totalsum <- function(x, scale = 1) {
  X <- as.matrix(x)
  rs <- rowSums(X); rs[rs == 0] <- 1
  Y <- sweep(X, 1, rs, "/")
  if (!is.null(scale) && is.finite(scale) && scale != 1) Y <- Y * scale
  Y
}

# ---- paths ----
orig_path <- "data/phylum/da_phylum_count_scabpit.csv"
boot_dir  <- "Result/phylum/bootstrap"
out_csv   <- "Result/F1_score_total_phylum.csv"
dir.create("Result", showWarnings = FALSE, recursive = TRUE)

# ---- load original (samples in rows, taxa in cols) ----
dat <- read.csv(orig_path, check.names = FALSE)
# If first column is sample IDs, uncomment:
# dat <- dat[, -1, drop = FALSE]
dat = dat [,-ncol(dat)]
for (j in seq_len(ncol(dat))) {
  if (!is.numeric(dat[[j]])) dat[[j]] <- suppressWarnings(as.numeric(dat[[j]]))
}
X0 <- to_mat(dat)

# ---- baseline CMIMN (4 normalizations) ----
result_log  <- conditional_MI(X0,                 q1 = 0.70, q2 = 0.95, quantitative = TRUE)
result_clr  <- conditional_MI(compositions::clr(X0 + 1e-6), q1 = 0.70, q2 = 0.95, quantitative = FALSE)
result_gmpr <- conditional_MI(norm_gmpr(X0),      q1 = 0.70, q2 = 0.95, quantitative = FALSE)
result_tss  <- conditional_MI(totalsum(X0, 1),    q1 = 0.70, q2 = 0.95, quantitative = FALSE)

cat("Internal log: Order0 =", result_log$sum_order0,  " Order1 =", result_log$sum_order1,  "\n")
cat("CLR:          Order0 =", result_clr$sum_order0,  " Order1 =", result_clr$sum_order1,  "\n")
cat("GMPR:         Order0 =", result_gmpr$sum_order0, " Order1 =", result_gmpr$sum_order1, "\n")
cat("TSS:          Order0 =", result_tss$sum_order0,  " Order1 =", result_tss$sum_order1,  "\n")

g_true_log  <- graph_from_adjacency_matrix(result_log$G_order1,  mode = "undirected", diag = FALSE)
g_true_clr  <- graph_from_adjacency_matrix(result_clr$G_order1,  mode = "undirected", diag = FALSE)
g_true_gmpr <- graph_from_adjacency_matrix(result_gmpr$G_order1, mode = "undirected", diag = FALSE)
g_true_tss  <- graph_from_adjacency_matrix(result_tss$G_order1,  mode = "undirected", diag = FALSE)

# ---- bootstraps (.csv or .rds) ----
boot_files <- sort(list.files(boot_dir, pattern = "\\.(csv|rds)$", full.names = TRUE, ignore.case = TRUE))
if (length(boot_files) == 0L) stop("No bootstrap files (.csv or .rds) found in: ", boot_dir)

load_boot <- function(path) {
  if (grepl("\\.rds$", path, ignore.case = TRUE)) {
    obj <- readRDS(path)
    # obj can be a data.frame/matrix or a list containing it; try common keys:
    if (is.list(obj) && !is.data.frame(obj) && !is.matrix(obj)) {
      for (k in c("X","data","counts","mat","Y")) {
        if (!is.null(obj[[k]])) { obj <- obj[[k]]; break }
      }
    }
    if (is.matrix(obj)) obj <- as.data.frame(obj, check.names = FALSE)
    if (!is.data.frame(obj)) stop("RDS does not contain a data.frame/matrix: ", path)
    df <- obj
  } else {
    df <- read.csv(path, check.names = FALSE)
  }
  # If there is a leading sample ID column, drop it (uncomment if needed):
  # if (!is.numeric(df[[1]])) df <- df[, -1, drop = FALSE]
  for (j in seq_len(ncol(df))) {
    if (!is.numeric(df[[j]])) df[[j]] <- suppressWarnings(as.numeric(df[[j]]))
  }
  to_mat(df)
}

F1_score_total <- NULL

for (b in seq_along(boot_files)) {
  Xb <- load_boot(boot_files[b])
  
  # (Optional) enforce same columns & order — uncomment if you want a hard check
  # stopifnot(identical(colnames(Xb), colnames(X0)))
  
  res_log_b  <- conditional_MI(Xb,                 q1 = 0.70, q2 = 0.95, quantitative = TRUE)
  res_clr_b  <- conditional_MI(compositions::clr(Xb + 1e-6), q1 = 0.70, q2 = 0.95, quantitative = FALSE)
  res_gmpr_b <- conditional_MI(norm_gmpr(Xb),      q1 = 0.70, q2 = 0.95, quantitative = FALSE)
  res_tss_b  <- conditional_MI(totalsum(Xb, 1),    q1 = 0.70, q2 = 0.95, quantitative = FALSE)
  
  g_log_b  <- graph_from_adjacency_matrix(res_log_b$G_order1,  mode = "undirected", diag = FALSE)
  g_clr_b  <- graph_from_adjacency_matrix(res_clr_b$G_order1,  mode = "undirected", diag = FALSE)
  g_gmpr_b <- graph_from_adjacency_matrix(res_gmpr_b$G_order1, mode = "undirected", diag = FALSE)
  g_tss_b  <- graph_from_adjacency_matrix(res_tss_b$G_order1,  mode = "undirected", diag = FALSE)
  
  F1_score <- c(
    b,
    f1_from_graphs(g_true_log,  g_log_b),
    f1_from_graphs(g_true_clr,  g_clr_b),
    f1_from_graphs(g_true_gmpr, g_gmpr_b),
    f1_from_graphs(g_true_tss,  g_tss_b),
    f1_from_graphs(g_log_b,  g_clr_b),
    f1_from_graphs(g_log_b,  g_gmpr_b),
    f1_from_graphs(g_log_b,  g_tss_b),
    f1_from_graphs(g_clr_b,  g_gmpr_b),
    f1_from_graphs(g_clr_b,  g_tss_b),
    f1_from_graphs(g_gmpr_b, g_tss_b)
  )
  F1_score_total <- rbind(F1_score_total, F1_score)
}

colnames(F1_score_total) <- c(
  "b",
  "F1_log_vs_logB",
  "F1_clr_vs_clrB",
  "F1_gmpr_vs_gmprB",
  "F1_tss_vs_tssB",
  "F1_logB_vs_clrB",
  "F1_logB_vs_gmprB",
  "F1_logB_vs_tssB",
  "F1_clrB_vs_gmprB",
  "F1_clrB_vs_tssB",
  "F1_gmprB_vs_tssB"
)

print(head(F1_score_total))
write.csv(F1_score_total, out_csv, row.names = FALSE)
cat("\nSaved F1 panel to:", out_csv, "\n")
