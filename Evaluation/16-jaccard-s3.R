rm(list=ls())

# -------------------- Packages --------------------
library(igraph)

# -------------------- Paths & Settings --------------------
path_root <- "/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code"
path_out  <- file.path(path_root, "Result")

list_names <- c("phylum","class","order")
#methods    <- c("sparcc","SE_glasso","SP","BN")
methods    <- c("sparcc","SE_glasso","SP","BN","BN2")
n_R        <- 50

# -------------------- Helpers --------------------
to_mat <- function(X) {
  M <- as.matrix(X)
  storage.mode(M) <- "double"
  if (is.null(rownames(M))) rownames(M) <- paste0("S", seq_len(nrow(M)))
  if (is.null(colnames(M))) colnames(M) <- paste0("V", seq_len(ncol(M)))
  M
}

as_adj_binary <- function(M) {
  M <- as.matrix(M)
  M[is.na(M)] <- 0
  diag(M) <- 0
  M <- (M + t(M)) > 0
  storage.mode(M) <- "numeric"
  M * 1
}

# Jaccard similarity for undirected edge sets
jaccard_edges <- function(g1, g2) {
  e1 <- get.edgelist(g1)
  e2 <- get.edgelist(g2)
  if (nrow(e1) == 0 && nrow(e2) == 0) return(1)
  if (nrow(e1) == 0 || nrow(e2) == 0) return(0)
  e1s <- apply(cbind(pmin(e1[,1], e1[,2]), pmax(e1[,1], e1[,2])), 1, paste, collapse="--")
  e2s <- apply(cbind(pmin(e2[,1], e2[,2]), pmax(e2[,1], e2[,2])), 1, paste, collapse="--")
  inter <- length(intersect(e1s, e2s))
  uni   <- length(union(e1s, e2s))
  if (uni == 0) return(1)
  inter / uni
}

# compute F1 from graphs
f1_from_graphs <- function(g_true, g_pred) {
  edgelab <- function(g) {
    e <- get.edgelist(g)
    if (nrow(e) == 0) return(character(0))
    apply(cbind(pmin(e[,1], e[,2]), pmax(e[,1], e[,2])), 1, paste, collapse="--")
  }
  tset <- edgelab(g_true)
  pset <- edgelab(g_pred)
  
  tp <- length(intersect(tset, pset))
  fp <- length(setdiff(pset, tset))
  fn <- length(setdiff(tset, pset))
  
  precision <- if ((tp + fp) == 0) 0 else tp / (tp + fp)
  recall    <- if ((tp + fn) == 0) 0 else tp / (tp + fn)
  if ((precision + recall) == 0) return(0)
  2 * precision * recall / (precision + recall)
}

# -------------------- Step 3: Score networks --------------------
message("== Step 3: Scoring networks (F1 + Jaccard) ==")
results <- list()

for (name in list_names) {
  message(" Scoring level: ", name)
  true_dir <- file.path(path_out, name, "total")
  net_dir  <- file.path(path_out, name, "networks")
  
  for (m in methods) {
    true_file <- file.path(true_dir, sprintf("%s_%s_total.csv", m, name))
    if (!file.exists(true_file)) {
      warning("True file missing: ", true_file, " (skip)")
      next
    }
    True <- read.csv(true_file, row.names = 1, check.names = FALSE)
    True <- as_adj_binary(True)
    
    nodes <- colnames(True)
    
    for (b in seq_len(n_R)) {
      pred_file <- file.path(net_dir, sprintf("%s_boot_%02d.csv", m, b))
      if (!file.exists(pred_file)) next
      
      Pred <- read.csv(pred_file, row.names = 1, check.names = FALSE)
      Pred <- to_mat(Pred)
      
      # align node sets
      #Pred <- Pred[nodes, nodes, drop = FALSE]
      Pred[is.na(Pred)] <- 0
      Pred <- as_adj_binary(Pred)
      
      g_true <- graph_from_adjacency_matrix(True, mode="undirected", diag=FALSE)
      g_pred <- graph_from_adjacency_matrix(Pred, mode="undirected", diag=FALSE)
      
      f1  <- f1_from_graphs(g_true, g_pred)
      jac <- jaccard_edges(g_true, g_pred)
      
      results[[length(results)+1]] <- data.frame(
        level     = name,
        method    = m,
        bootstrap = b,
        F1        = f1,
        Jaccard   = jac,
        stringsAsFactors = FALSE
      )
    }
  }
}

# bind and save
scores_df <- do.call(rbind, results)
out_file  <- file.path(path_out, "scores_F1_Jaccard.csv")
write.csv(scores_df, out_file, row.names = FALSE)

message(" Scores written to: ", out_file)
