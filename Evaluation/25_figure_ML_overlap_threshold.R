#############################################################
# Per-normalization counts + ALL4 intersection
# across ML thresholds Count > {2,3,4,5} in one figure
#############################################################

rm(list = ls())
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(purrr)
  library(stringr)
})

# ---------- Paths / config ----------
base_path   <- "/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code"
levels_vec  <- c("Phylum","Class","Order")
ml_dir_for  <- function(lvl) file.path(base_path, "ML", lvl)  # contains feature_selection* files
out_dir     <- file.path(base_path, "Result", "toppthereshold")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Thresholds to display on X-axis
count_thresholds <- c(2,3,4,5)

# We will label normalizations like before
norm_labels <- c("CLR","Count","Log","RowSum","ALL4 (∩)")

# ---------- Helper to read & filter one Excel file ----------
read_and_filter_file <- function(filepath, thr){
  df <- read_excel(filepath)
  # Expect a "Count" column; keep rows where Count > thr
  if (!"Count" %in% names(df)) stop("Missing 'Count' column in: ", filepath)
  df <- df[df$Count > thr, , drop = FALSE]
  # Return the first column (OTU names) and the last column (score) for compatibility
  df[, c(1, ncol(df)), drop = FALSE]
}

# ---------- Build summary across thresholds ----------
rows <- list()

for (lvl in levels_vec) {
  in_dir <- ml_dir_for(lvl)
  files  <- list.files(in_dir, pattern = "feature_selection", full.names = TRUE)
  if (length(files) < 4) {
    warning("Expected ≥4 'feature_selection' files in ", in_dir, " but found ", length(files))
  }
  # make the order stable
  files <- sort(files)
  
  for (thr in count_thresholds) {
    # Read and filter all four files for this threshold
    filtered_files <- map(files, ~ read_and_filter_file(.x, thr))
    
    # Create lists of OTU names (first column) for each normalization
    otu_list <- lapply(filtered_files, function(df) as.character(df[[1]]))
    # Name the first 4 entries as your normalizations
    names(otu_list)[seq_len(min(4, length(otu_list)))] <- c("clr","count","log","rowSum")[seq_len(min(4, length(otu_list)))]
    
    # Extract per-normalization sets
    s1 <- unique(otu_list$clr     %||% character(0))
    s2 <- unique(otu_list$count   %||% character(0))
    s3 <- unique(otu_list$log     %||% character(0))
    s4 <- unique(otu_list$rowSum  %||% character(0))
    
    # ALL4 intersection
    all4 <- Reduce(intersect, list(s1, s2, s3, s4))
    
    # Append rows (counts)
    rows[[length(rows)+1]] <- data.frame(level=lvl, threshold=paste0("> ", thr), normalization="CLR",        n=length(s1))
    rows[[length(rows)+1]] <- data.frame(level=lvl, threshold=paste0("> ", thr), normalization="Count",      n=length(s2))
    rows[[length(rows)+1]] <- data.frame(level=lvl, threshold=paste0("> ", thr), normalization="Log",        n=length(s3))
    rows[[length(rows)+1]] <- data.frame(level=lvl, threshold=paste0("> ", thr), normalization="RowSum",     n=length(s4))
    rows[[length(rows)+1]] <- data.frame(level=lvl, threshold=paste0("> ", thr), normalization="ALL4 (∩)",   n=length(all4))
  }
}

summary_df <- bind_rows(rows) %>%
  mutate(
    normalization = factor(normalization, levels = norm_labels),
    level = factor(level, levels = levels_vec),
    threshold = factor(threshold, levels = paste0("> ", count_thresholds))
  )

# Write tidy summary
summary_csv <- file.path(out_dir, "ML_counts_vs_thresholds_with_ALL4.csv")
write_csv(summary_df, summary_csv)

# ---------- Figure: grouped bars by threshold (x), bars = normalization, facet by level ----------
dodge_w <- 0.75

p <- ggplot(summary_df, aes(x = threshold, y = n, fill = normalization)) +
  geom_col(position = position_dodge(width = dodge_w), width = 0.65) +
  geom_text(aes(label = n),
            position = position_dodge(width = dodge_w),
            vjust = -0.35, size = 3.4) +
  facet_grid(level ~ .) +
  labs(title = "",
       x = "ML selection threshold (TOTAL > th)",
       y = "Number of taxa",
       fill = "Normalization") +
  theme_bw() +
  expand_limits(y = max(summary_df$n, na.rm = TRUE) * 1.12)

ggsave(file.path(out_dir, "per_norm_plus_ALL4_counts_by_threshold.png"),
       p, width = 9, height = 9, dpi = 300)

cat("Done.\nSummary CSV:\n  ", summary_csv,
    "\nFigure:\n  ", file.path(out_dir, "per_norm_plus_ALL4_counts_by_threshold.png"), "\n", sep = "")
