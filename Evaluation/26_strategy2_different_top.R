#############################################################
# ONE FILE SUMMARY
# Columns:
#   p, level, method,
#   n_df1_method, n_df2_method,
#   n_df1_allMethods, n_df2_allMethods,
#   n_union_allMethods_df1_df2
#############################################################

rm(list = ls())
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
})

# ------------------ CONFIG ------------------
base_path <- "/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code"
folders   <- c("phylum", "class", "order")
methods   <- c("sparcc","SE_glasso","SP","BN")
nets      <- c("df1","df2")
p_values  <- c(10, 20, 30)

# centrality weights for Strategy 2 composite
w <- c(degree=.05, evcent=.15, page_rank=.15, closeness=.20, betweenness=.50)

compare_dir <- function(folder) file.path(base_path, "Result", folder, "compare")
out_dir    <- file.path(base_path, "Result", "toppthereshold")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ------------------ HELPERS ------------------
get_name_otu <- function(folder) {
  f <- switch(folder,
              phylum = "data/phylum/da_phylum_clr_scabpit.csv",
              class  = "data/class/da_class_clr_scabpit.csv",
              order  = "data/order/da_order_clr_scabpit.csv")
  d <- read.csv(file.path(base_path, f), check.names = FALSE)
  colnames(d)[-ncol(d)]
}

read_compare <- function(folder, method, net) {
  fp <- file.path(compare_dir(folder), paste0(method, net, ".csv"))
  if (!file.exists(fp)) return(NULL)
  DF <- read.csv(fp, row.names = 1, check.names = FALSE)
  if (nrow(DF) == 0) return(NULL)
  DF[] <- lapply(DF, function(x) { x[is.na(x)] <- 0; x })
  as.data.frame(DF, check.names = FALSE)
}

score_method <- function(DF, method) {
  s <- w["degree"]*DF$degree + w["evcent"]*DF$evcent + w["page_rank"]*DF$page_rank +
    w["closeness"]*DF$closeness + w["betweenness"]*DF$betweenness
  if (method == "SE_glasso") s <- 2*s
  as.numeric(s)
}

select_top_p <- function(scores, p) {
  n <- length(scores)
  if (n == 0) return(integer(0))
  k <- max(1, round(n * p / 100))
  o <- order(scores, decreasing = TRUE)
  o[seq_len(min(k, length(o)))]
}

# Safe intersection across a list of character vectors
safe_intersect_all <- function(sets_list) {
  keep <- sets_list[ lengths(sets_list) > 0 ]
  if (length(keep) == 0) return(character(0))
  Reduce(intersect, keep)
}

# ------------------ MAIN ------------------
rows_out <- list()

for (p in p_values) {
  for (folder in folders) {
    name_otu <- get_name_otu(folder)
    
    # Build per-method sets for df1 and df2
    sets_df1 <- setNames(vector("list", length(methods)), methods)
    sets_df2 <- setNames(vector("list", length(methods)), methods)
    
    for (m in methods) {
      # df1
      DF1 <- read_compare(folder, m, "df1")
      if (is.null(DF1)) {
        sets_df1[[m]] <- character(0)
      } else {
        s1 <- score_method(DF1, m)
        idx1 <- select_top_p(s1, p)
        sets_df1[[m]] <- unique(name_otu[idx1])
      }
      
      # df2
      DF2 <- read_compare(folder, m, "df2")
      if (is.null(DF2)) {
        sets_df2[[m]] <- character(0)
      } else {
        s2 <- score_method(DF2, m)
        idx2 <- select_top_p(s2, p)
        sets_df2[[m]] <- unique(name_otu[idx2])
      }
    }
    
    # Confirmed-by-all (intersection across methods) per network
    all_df1 <- safe_intersect_all(sets_df1)
    all_df2 <- safe_intersect_all(sets_df2)
    union_all_df1_df2 <- union(all_df1, all_df2)
    
    n_df1_all <- length(all_df1)
    n_df2_all <- length(all_df2)
    n_union_all <- length(union_all_df1_df2)
    
    # Emit one row per method with method-specific counts + the shared "confirmed-by-all" counts
    for (m in methods) {
      n_df1_method <- length(sets_df1[[m]])
      n_df2_method <- length(sets_df2[[m]])
      
      rows_out[[length(rows_out)+1]] <- data.frame(
        p = p,
        level = folder,
        method = m,
        n_df1_method = n_df1_method,
        n_df2_method = n_df2_method,
        n_df1_allMethods = n_df1_all,
        n_df2_allMethods = n_df2_all,
        n_union_allMethods_df1_df2 = n_union_all,
        stringsAsFactors = FALSE
      )
    }
  }
}

summary_df <- bind_rows(rows_out)

out_file <- file.path(out_dir, "summary_topP_by_level_method_strategy2.csv")
write_csv(summary_df, out_file)

cat("Wrote:\n  ", out_file, "\n", sep = "")
