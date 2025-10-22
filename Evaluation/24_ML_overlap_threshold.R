rm(list=ls())
library(readxl)
library(purrr)

folders <- c("phylum", "class","order")

for (folder in folders) {
  path_result <- paste0("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/ML/",folder,"/")
  path_out    <- paste0("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/Result/",folder,"/ML_FS/")
  dir.create(path_out, showWarnings = FALSE, recursive = TRUE)
  
  file_names <- list.files(path_result, pattern = "feature_selection")
  
  read_and_filter_file <- function(file) {
    df <- read_excel(file)
    # your current threshold (Count > 4); change if you want 3/5/6
    df[df$Count > 5, c(1, ncol(df))]
  }
  
  filtered_files <- map(file_names, ~ read_and_filter_file(paste0(path_result, .x)))
  
  # Create normalization-specific vectors (as you do now)
  otu_list <- lapply(filtered_files, function(df) as.character(df[[1]]))
  names(otu_list)[1:4] <- c("clr","count","log","rowSum")
  
  # === Pairwise overlap matrix (what you already compute)
  ML_SF_scabpit <- crossprod(table(stack(otu_list)))
  write.csv(ML_SF_scabpit, file = paste0(path_out,"ML_SF_scabpit.csv"), row.names = TRUE)
  
  # === Save your four per-normalization sets (same as now)
  write.csv(otu_list$clr,    file = paste0(path_out,"1.csv"), row.names = TRUE)
  write.csv(otu_list$count,  file = paste0(path_out,"2.csv"), row.names = TRUE)
  write.csv(otu_list$log,    file = paste0(path_out,"3.csv"), row.names = TRUE)
  write.csv(otu_list$rowSum, file = paste0(path_out,"4.csv"), row.names = TRUE)
  
  # === 4-way intersection (ALL normalizations)
  # Method A: direct intersection
  all4 <- Reduce(intersect, otu_list)
  
  # Method B: via frequency (equivalent)
  # freq <- table(unlist(otu_list, use.names = FALSE))
  # all4 <- names(freq[freq == 4])
  
  # Also compute union (for a percentage)
  all_union <- Reduce(union, otu_list)
  
  n_all4      <- length(all4)            # <- your ONE overlap value (count)
  n_union     <- length(all_union)
  prop_all4_u <- if (n_union > 0) n_all4 / n_union else NA_real_
  
  # (Optional) average pairwise overlap count (no Jaccard):
  pair_counts <- ML_SF_scabpit[lower.tri(ML_SF_scabpit)]
  mean_pairwise_count <- if (length(pair_counts)) mean(pair_counts) else NA_real_
  
  # Write the 4-way intersection names (handy for supplement)
  write.csv(data.frame(otu = all4), file = paste0(path_out,"ALL4.csv"), row.names = FALSE)
  
  # Write a compact summary so you have a single number to cite
  summary_row <- data.frame(
    folder = folder,
    n_clr    = length(otu_list$clr),
    n_count  = length(otu_list$count),
    n_log    = length(otu_list$log),
    n_rowSum = length(otu_list$rowSum),
    n_union  = n_union,
    n_all4   = n_all4,                # <- overlap across ALL 4 normalizations (count)
    prop_all4_of_union = prop_all4_u, # <- percentage of the union
    mean_pairwise_overlap_count = mean_pairwise_count
  )
  write.csv(summary_row, file = paste0(path_out,"ML_overlap_summary.csv"), row.names = FALSE)
  
  # If you want a SINGLE file that accumulates across folders, append like this:
  master_path <- "/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/Result/toppthereshold/ML_overlap_summary_all_levels>5.csv"
  if (!file.exists(master_path)) {
    write.csv(summary_row, master_path, row.names = FALSE)
  } else {
    write.table(summary_row, master_path, sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE)
  }
}
