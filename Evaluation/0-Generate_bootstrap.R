rm(list=ls())

# -------------------- Config ----------------------
set.seed(20250826)  # reproducible bootstraps

path_root <- "/Users/rosa/Desktop/ALLWork_big/Madison/Project/BayesianNetwork/R_BN_code"
path_data <- file.path(path_root, "data")
path_out  <- file.path(path_root, "Result")

list_names <- c("phylum","class","order")
n_R        <- 50   # number of bootstrap datasets

# -------------------- Helpers ---------------------
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

# -------------------- Phase 1: Generate bootstraps --------------------
message("== Phase 1: Generating bootstrap datasets ==")
for (name in list_names) {
  message(" Bootstrapping: ", name)
  infile <- file.path(path_data, name, paste0("da_", name, "_count_scabpit.csv"))
  dat <- read.csv(infile, check.names = FALSE)
  X   <- dat[, -ncol(dat), drop = FALSE]   # drop last column if metadata/label
  boot_dir <- dir_create(path_out, name, "bootstrap")
  n <- nrow(X)
  for (b in seq_len(n_R)) {
    idx <- sample.int(n, size = n, replace = TRUE)     # bootstrap rows
    Xb  <- to_mat(X[idx, , drop = FALSE])
    saveRDS(Xb, file.path(boot_dir, sprintf("boot_%02d.rds", b)))
  }
}

