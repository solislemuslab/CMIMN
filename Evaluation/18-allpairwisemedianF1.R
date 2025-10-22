rm(list=ls())

library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)

# ---- load ----
path_root <- "/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code"
scores_file <- file.path(path_root, "Result", "scores_F1_Jaccard.csv")
df <- read_csv(scores_file, show_col_types = FALSE)

# (optional) map method codes to paper labels used elsewhere
method_map <- c(
  "BN"        = "CMIMN",
  "SE_glasso" = "SE_glasso",
  "sparcc"    = "SPARCC",
  "SP"        = "SPRING"
)
df <- df %>%
  mutate(method = recode(method, !!!method_map))

# ---- helper: one paired wilcoxon between two methods within one level ----
paired_wilcox_two <- function(dat_level, mA, mB) {
  wide <- dat_level %>%
    filter(method %in% c(mA, mB)) %>%
    select(bootstrap, method, F1) %>%
    pivot_wider(names_from = method, values_from = F1)
  
  # keep rows with both values present
  wide <- wide %>% filter(!is.na(.data[[mA]]), !is.na(.data[[mB]]))
  n_pairs <- nrow(wide)
  
  if (n_pairs < 3) {
    return(tibble(
      method_A = mA, method_B = mB, n_pairs = n_pairs,
      A_median = NA_real_, B_median = NA_real_, median_diff = NA_real_,
      wilcox_p = NA_real_, cliffs_delta = NA_real_
    ))
  }
  
  A <- wide[[mA]]
  B <- wide[[mB]]
  
  # paired Wilcoxon (two-sided)
  pval <- tryCatch(wilcox.test(A, B, paired = TRUE, exact = FALSE)$p.value,
                   error = function(e) NA_real_)
  
  # simple paired Cliff's delta: proportion(A>B) - proportion(A<B)
  d <- A - B
  cd <- (sum(d > 0) - sum(d < 0)) / length(d)
  
  tibble(
    method_A = mA,
    method_B = mB,
    n_pairs  = n_pairs,
    A_median = median(A, na.rm = TRUE),
    B_median = median(B, na.rm = TRUE),
    median_diff = median(A - B, na.rm = TRUE),  # median of paired diffs
    wilcox_p = pval,
    cliffs_delta = cd
  )
}

# ---- run all pairwise comparisons per level ----
out <- df %>%
  group_by(level) %>%
  group_modify(function(d, key) {
    meths <- sort(unique(d$method))
    pairs <- t(combn(meths, 2))
    res <- map_dfr(seq_len(nrow(pairs)), function(i) {
      paired_wilcox_two(d, pairs[i,1], pairs[i,2])
    })
    # adjust p-values within level (BH)
    res %>%
      mutate(p_adj_BH = p.adjust(wilcox_p, method = "BH")) %>%
      arrange(p_adj_BH, wilcox_p)
  }) %>%
  ungroup() %>%
  select(level, method_A, method_B, n_pairs,
         A_median, B_median, median_diff, wilcox_p, p_adj_BH, cliffs_delta)

# save & print
out_file <- file.path(path_root, "Result", "F1_pairwise_wilcoxon_all_methods.csv")
readr::write_csv(out, out_file)
print(out)
