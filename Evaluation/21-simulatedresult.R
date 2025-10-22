rm(list = ls())

suppressPackageStartupMessages({
  library(SpiecEasi)   # make_graph, graph2prec, prec2cov, cov2cor, synth_comm_from_counts
  library(Matrix)
})

# ---- Your methods (as in your current pipeline) ----
library(SPRING)
source("fun_BN.R")     # provides edgereduce(...)
source("OIPC_1.R")
Synth_SP <- function(data, ADJ, numberseed){
  n <- nrow(data)
  Cor1 <- Cor_AdjGraph(ADJ)
  X1_count <- SPRING::synthData_from_ecdf(data, Sigma = Cor1, n = n, seed = numberseed)
  return(X1_count)
}

#' Convert Adjacency Matrix to Correlation Matrix
Cor_AdjGraph <- function(ADJ){
  class(ADJ) <- "graph"
  Prec_ADJ  <- SpiecEasi::graph2prec(ADJ)
  Cor_ADJ <- cov2cor(SpiecEasi::prec2cov(Prec_ADJ))
  return(Cor_ADJ)
}
# ======================
# 0) Prepare AMGut-based counts and simulate
# ======================
# You already have amgut1.filt in your env; if not, load it (SpiecEasi has data)
if (!exists("amgut1.filt")) data("amgut1.filt", package = "SpiecEasi")

depths <- rowSums(amgut1.filt)
amgut1.filt.n  <- t(apply(amgut1.filt, 1, norm_to_total))
amgut1.filt.cs <- round(amgut1.filt.n * min(depths))

d <- ncol(amgut1.filt.cs)
n <- nrow(amgut1.filt.cs)

# Graph generator (cluster)
set.seed(10010)
# e here is avg degree; use a modest sparse graph
e <- d
#graph <- SpiecEasi::make_graph('cluster', ncol(amgut1.filt), 175)
graph <- SpiecEasi::make_graph('band', ncol(amgut1.filt), 175)
#Prec  <- graph2prec(graph)         # TRUE underlying precision (sparse)
#Cor   <- cov2cor(prec2cov(Prec))   # Implied correlation (dense)

# Simulate ZINB counts with AMGut margins and desired correlation
#X <- synth_comm_from_counts(amgut1.filt.cs, mar = 2, distr = "zinegbin", Sigma = Cor, n = n)

X<- Synth_SP (amgut1.filt.cs, graph, 100)
colnames(X) <- paste0("OTU", seq_len(ncol(X)))
print(X)

# ======================
# 1) Define ground truth
# ======================
# Option A (recommended): precision-graph truth (non-zero off-diagonal of Prec)
A_true <- as.matrix(graph)

# ======================
# 2) Helper functions
# ======================

edge_metrics <- function(A_est, A_true) {
  ut <- upper.tri(A_true)
  tp <- sum(A_est[ut] == 1 & A_true[ut] == 1)
  fp <- sum(A_est[ut] == 1 & A_true[ut] == 0)
  fn <- sum(A_est[ut] == 0 & A_true[ut] == 1)
  precision <- ifelse(tp + fp == 0, NA, tp/(tp+fp))
  recall    <- ifelse(tp + fn == 0, NA, tp/(tp+fn))
  f1        <- ifelse(is.na(precision) | is.na(recall) | (precision+recall)==0,
                      NA, 2*precision*recall/(precision+recall))
  data.frame(tp=tp, fp=fp, fn=fn,sum(A_est),sum(A_true), precision=precision, recall=recall, F1=f1)
}

# SparCC threshold to match truth sparsity (top-K abs correlations)
sparcc_topK <- function(counts, K_pairs) {
  sc <- sparcc(counts)
  C <- sc$Cor; diag(C) <- 0
  ut <- which(upper.tri(C), arr.ind = TRUE)
  vals <- abs(C[upper.tri(C)])
  ord  <- order(vals, decreasing = TRUE)
  keep <- ut[ord[seq_len(min(K_pairs, length(ord)))], , drop = FALSE]
  p <- ncol(counts)
  A <- matrix(0, p, p)
  for (i in seq_len(nrow(keep))) {
    A[keep[i,1], keep[i,2]] <- 1
    A[keep[i,2], keep[i,1]] <- 1
  }
  diag(A) <- 0
  A
}

# ======================
# 3) Run all methods
# ======================
message("Running SPIEC-EASI (glasso) ...")
se_gl <- spiec.easi(X, method = "glasso", sel.criterion = "stars",
                    nlambda = 30, lambda.min.ratio = 1e-2,
                    pulsar.params = list(rep.num = 20))
A_se_gl <- as.matrix(getRefit(se_gl)); diag(A_se_gl) <- 0

message("Running SPRING ...")

SP_original2 <- function(data){
  output <- list()
  start <- Sys.time()
  SP_graph <- SPRING(data, Rmethod = "original", quantitative = TRUE, ncores = 5,
                     lambdaseq = "data-specific", nlambda = 8, rep.num = 20)
  print(Sys.time() - start)
  opt.K <- SP_graph$output$stars$opt.index
  adj.K <- as.matrix(SP_graph$fit$est$path[[opt.K]])
  output$SP_graph <- SP_graph
  output$adj_SP <- adj.K
  return(output)
}

sp_out <- SP_original2(X)    # your wrapper; assumes [[2]] is adjacency
A_spring <- sp_out[[2]]
sum(A_spring)

message("Running SparCC (top-K to match truth) ...")
K_pairs <- sum(A_true) / 2L
A_sparcc <- sparcc_topK(X, K_pairs = K_pairs)

message("Running CMIMN / BN ...")
data_BN <- log(X + 1)
n_gene  <- ncol(data_BN)
G       <- matrix(1, n_gene, n_gene); diag(G) <- 0
Gval    <- G
q1 <- 0.95; q2 <- 0.97
res  <- edgereduce(G, Gval, t(data_BN), q1, q2)
A_bn <- res$G_order1

# ======================
# 4) Metrics + Save
# ======================
dir.create("Result", showWarnings = FALSE)
write.csv(A_true,    "Result/simulate_SE/adj_TRUE.csv", row.names = FALSE)
write.csv(A_se_gl,   "Result/simulate_SE/adj_SE_glasso.csv", row.names = FALSE)
write.csv(A_spring,  "Result/simulate_SE/adj_SPRING.csv", row.names = FALSE)
write.csv(A_sparcc,  "Result/simulate_SE/adj_SparCC_topK.csv", row.names = FALSE)
write.csv(A_bn,      "Result/simulate_SE/adj_CMIMN_BN.csv", row.names = FALSE)

metrics <- rbind(
  cbind(method = "SE_glasso",    edge_metrics(A_se_gl,  A_true)),
  cbind(method = "SPRING",       edge_metrics(A_spring, A_true)),
  cbind(method = "SparCC_topK",  edge_metrics(A_sparcc, A_true)),
  cbind(method = "CMIMN_BN",     edge_metrics(A_bn,     A_true))
)
write.csv(metrics, "Result/simulate_SE/metrics.csv", row.names = FALSE)
print(metrics)

# q1 <- 0.95; q2 <- 0.97
# res  <- edgereduce(G, Gval, t(data_BN), q1, q2)
# A_bn <- res$G_order1
# edge_metrics(A_bn,A_true)

# results <- data.frame(
#   q1 = numeric(), q2 = numeric(),
#   tp = integer(), fp = integer(), fn = integer(),
#   precision = numeric(), recall = numeric(), F1 = numeric(),
#   stringsAsFactors = FALSE
# )
# k=0
# q_seq <- seq(0.80, 1.00, by = 0.001)
# for (q1 in q_seq) {
#   for (q2 in q_seq) {
#     k <- k + 1
#     # run edgereduce; guard against occasional failures
#     res <- tryCatch(edgereduce(G, Gval, t(data_BN), q1, q2), error = function(e) NULL)
#     if (is.null(res)) next
#     A_bn <- symbin(res$G_order1)
#     m <- edge_metrics(A_bn, A_true)
#     results[nrow(results) + 1, ] <- c(q1, q2, m$tp, m$fp, m$fn, m$precision, m$recall, m$F1)
#   }
# }
#results[order(results[[ncol(results)]], decreasing = TRUE), ]




