rm(list=ls())

# -------------------- Packages --------------------
library(SpiecEasi)
library(SPRING)
library(Matrix)
library(igraph)
library(CMIMN)
source('fun_BN.R')   # uses your cmi() and edgereduce()
source('OIPC_1.R')
# -------------------- Paths & Settings --------------------
path_root <- "/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code"
path_out  <- file.path(path_root, "Result")
#list_names <- c("phylum","class","order")
list_names <- c("phylum","class")
#list_names <- c("order")
#methods    <- c("sparcc","SE_glasso","SP","BN")
methods    <- c("BN2")
n_R        <- 50

# -------------------- Helpers --------------------
dir_create <- function(...) {
  p <- file.path(...)
  if (!dir.exists(p)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
  p
}

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

# -------- BN helper (correct call to your edgereduce) --------
bn_adjacency <- function(X, q1 = 0.8, q2 = 0.9) {
  X <- to_mat(X)
  Xlog <- log(X + 1)
  Xt   <- t(Xlog)        # your BN expects variables in rows
  
  p <- nrow(Xt)
  # Start with a fully connected undirected graph (no self-loops)
  G    <- matrix(1, p, p); diag(G) <- 0
  Gval <- matrix(0, p, p)
  
  # If your edgereduce() relies on a global 'data', we can prime it safely
  old_exists <- exists("data", envir = .GlobalEnv, inherits = FALSE)
  if (old_exists) old_data <- get("data", envir = .GlobalEnv, inherits = FALSE)
  assign("data", Xt, envir = .GlobalEnv)
  on.exit({
    if (old_exists) {
      assign("data", old_data, envir = .GlobalEnv)
    } else if (exists("data", envir = .GlobalEnv, inherits = FALSE)) {
      rm("data", envir = .GlobalEnv)
    }
  }, add = TRUE)
  
  # >>> Correct argument order <<<
  res <- edgereduce(G, Gval, Xt, q1, q2)
  
  as_adj_binary(res$G_order1)
}

bn_adjacency2 <- function(X, q1 = 0.8, q2 = 0.9) {
  Xt <- t(X) 
  clr_data <- clr(Xt + 1e-6)
  res <- conditional_MI(clr_data, q1 = 0.8, q2 = 0.9, quantitative = FALSE)
  as_adj_binary(res$G_order1)
}

# unified runner for all methods
pnet <- function(selected_data, m) {
  X <- to_mat(selected_data)
  stopifnot(m %in% c("sparcc","SE_glasso","SP","BN","BN2"))
  
  if (m == "sparcc") {
    sp <- sparcc(X)
    A  <- (abs(sp$Cor) >= 0.4) * 1
    diag(A) <- 0
    return(as_adj_binary(A))
  }
  if (m == "SE_glasso") {
    se <- suppressMessages(SE_original(X, 'glasso'))
    return(as_adj_binary(se[[2]]))
  }
  if (m == "SP") {
    spmb <- SP_original(X)
    return(as_adj_binary(spmb[[2]]))
  }
  if (m == "BN") {
    return(bn_adjacency(X))
  }
  if (m == "BN2") {
    return(bn_adjacency2(X))
  }
}

# -------------------- Step 2: Run methods on saved bootstraps --------------------
message("== Step 2: Running networks on saved bootstrap datasets ==")

for (name in list_names) {
  message(" Processing level: ", name)
  boot_dir <- file.path(path_out, name, "bootstrap")
  if (!dir.exists(boot_dir)) {
    stop("Bootstrap folder not found: ", boot_dir,
         "\nRun Step 1 (data generation) before this step.")
  }
  net_dir <- dir_create(path_out, name, "networks")
  
  for (b in seq_len(n_R)) {
    Xb_file <- file.path(boot_dir, sprintf("boot_%02d.rds", b))
    if (!file.exists(Xb_file)) {
      message("  (skip) missing bootstrap file: ", Xb_file)
      next
    }
    Xb <- readRDS(Xb_file)
    Xb <- to_mat(Xb)
    
    for (m in methods) {
      out_csv <- file.path(net_dir, sprintf("%s_boot_%02d.csv", m, b))
      if (file.exists(out_csv)) next  # already computed
      
      A <- tryCatch(
        pnet(Xb, m),
        error = function(e) {
          message(sprintf("  ERROR at level=%s method=%s boot=%d: %s", name, m, b, e$message))
          return(NULL)
        }
      )
      if (!is.null(A)) write.csv(A, out_csv)
    }
  }
}

message("Done. Networks saved under Result/{level}/networks/")
