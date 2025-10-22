rm(list=ls())
library(SpiecEasi)
library(SPRING)
library(Matrix)
source('fun_BN.R')
source('OIPC_1.R')

ensure_dir <- function(p) if (!dir.exists(p)) dir.create(p, recursive = TRUE, showWarnings = FALSE)

time_it <- function(expr) {
  gc()                                # reduce noise
  t <- system.time(force(expr))
  as.numeric(t["elapsed"])            # seconds (numeric)
}

path_data <- "/Users/rosa/Desktop/ALLWork_big/Madison/Project/BayesianNetwork/R_BN_code/data/"
levels <- c("phylum","class","order")

ensure_dir("Result")
master_bench <- NULL

for (lev in levels) {
  message(">>> Level: ", lev)
  out_dir <- file.path("Result", lev); ensure_dir(out_dir)
  
  dat <- read.csv(file.path(path_data, lev, paste0("da_", lev, "_count_scabpit.csv")), check.names = FALSE)
  data_otu <- dat[, -ncol(dat), drop = FALSE]
  n_samples <- nrow(data_otu)
  n_taxa    <- ncol(data_otu)
  
  # --- SparCC ---
  sparcc_graph <- NULL
  t_sparcc <- tryCatch({
    time_it({
      sparcc.ot <- sparcc(data_otu)
      S <- abs(sparcc.ot$Cor) >= 0.4
      diag(S) <- 0
      sparcc_graph <<- S
    })
  }, error = function(e){ warning("SparCC failed: ", e$message); NA_real_ })
  
  # --- SpiecEasi (glasso via your wrapper) ---
  SE_glasso <- NULL
  t_se <- tryCatch({
    time_it({
      se_glasso <- SE_original(as.matrix(data_otu), 'glasso')
      SE_glasso <<- se_glasso[[2]]
    })
  }, error = function(e){ warning("SE_glasso failed: ", e$message); NA_real_ })
  
  # --- SPRING (MB) ---
  SP <- NULL
  t_sp <- tryCatch({
    time_it({
      sp_mb <- SP_original(data_otu)
      SP <<- sp_mb[[2]]
    })
  }, error = function(e){ warning("SPRING MB failed: ", e$message); NA_real_ })
  
  # --- BN (edgereduce pipeline) ---
  BN <- NULL
  t_bn <- tryCatch({
    time_it({
      data_BN <- log(data_otu + 1)
      n_gene <- nrow(t(data_BN))
      G <- matrix(1, n_gene, n_gene); diag(G) <- 0
      Gval <- G
      q1 <- 0.8; q2 <- 0.9
      data_tr <- t(data_BN)
      result <- edgereduce(G, Gval, data_tr, q1, q2)
      BN <<- result$G_order1
    })
  }, error = function(e){ warning("BN/edgereduce failed: ", e$message); NA_real_ })
  
  # write outputs
  if (!is.null(sparcc_graph)) write.csv(sparcc_graph, file = file.path(out_dir, paste0("sparcc_", lev, "_total.csv")), row.names = TRUE)
  if (!is.null(SE_glasso))     write.csv(SE_glasso,     file = file.path(out_dir, paste0("SE_glasso_", lev, "_total.csv")), row.names = TRUE)
  if (!is.null(SP))            write.csv(SP,            file = file.path(out_dir, paste0("SP_", lev, "_total.csv")), row.names = TRUE)
  if (!is.null(BN))            write.csv(BN,            file = file.path(out_dir, paste0("BN_", lev, "_total.csv")), row.names = TRUE)
  
  # benchmarking table (mem_mb kept as NA)
  bench_df <- data.frame(
    level     = lev,
    n_samples = n_samples,
    n_taxa    = n_taxa,
    algorithm = c("SparCC","SpiecEasi_glasso","SPRING_MB","BN_edgereduce"),
    time_sec  = c(t_sparcc, t_se, t_sp, t_bn),
    mem_mb    = NA_real_,
    stringsAsFactors = FALSE
  )
  
  write.csv(bench_df, file = file.path(out_dir, paste0("benchmark_", lev, "_total.csv")), row.names = FALSE)
  master_bench <- rbind(master_bench, bench_df)
}

write.csv(master_bench, file = file.path("Result","benchmark_master.csv"), row.names = FALSE)

# ---- Save machine/session info (for manuscript Methods) ----
hw <- capture.output({
  cat("=== Sys.info ===\n"); print(Sys.info())
  cat("\n=== sessionInfo ===\n"); print(sessionInfo())
})
writeLines(hw, con = file.path("Result","hardware_and_session_info.txt"))
