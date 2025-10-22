rm(list=ls())

library(readr)
library(dplyr)
library(ggplot2)

# ---------- paths ----------
path_root <- "/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code"
pairwise_file <- file.path(path_root, "Result", "F1_pairwise_wilcoxon_all_methods.csv")

# ---------- load ----------
df <- read_csv(pairwise_file, show_col_types = FALSE)

# ---------- fixed orders ----------
level_levels  <- c("phylum","class","order")
method_levels <- c("CMIMN","SE_glasso","SPARCC","SPRING")

df <- df %>%
  mutate(
    level    = factor(level, levels = level_levels),
    method_A = factor(method_A, levels = method_levels),
    method_B = factor(method_B, levels = method_levels),
    sig = case_when(
      p_adj_BH <= 0.001 ~ "***",
      p_adj_BH <= 0.01  ~ "**",
      p_adj_BH <= 0.05  ~ "*",
      TRUE              ~ ""
    )
  )

# ---------- mirror (ensure both A→B and B→A exist) ----------
sym_df <- bind_rows(
  df %>% select(level, method_A, method_B, median_diff, sig),
  df %>% transmute(level, method_A = method_B, method_B = method_A,
                   median_diff = -median_diff, sig = sig)
)

# ---------- build full 4x4 grid ----------
grid <- expand.grid(
  level    = factor(level_levels, levels = level_levels),
  method_A = factor(method_levels, levels = method_levels),
  method_B = factor(method_levels, levels = method_levels),
  KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
)

plot_df <- grid %>%
  left_join(sym_df, by = c("level","method_A","method_B")) %>%
  mutate(
    # remove diagonal
    median_diff = ifelse(as.character(method_A) == as.character(method_B), NA_real_, median_diff),
    sig         = ifelse(as.character(method_A) == as.character(method_B), "", sig),
    label = case_when(
      !is.na(median_diff) & sig != "" & median_diff > 0 ~ paste0(sig, "↑"),
      !is.na(median_diff) & sig != "" & median_diff < 0 ~ paste0(sig, "↓"),
      TRUE ~ ""
    ),
    dir_legend = case_when(
      !is.na(median_diff) & sig != "" & median_diff > 0 ~ "A better (BH ≤ 0.05)",
      !is.na(median_diff) & sig != "" & median_diff < 0 ~ "B better (BH ≤ 0.05)",
      TRUE                                              ~ "Not significant (BH > 0.05)"
    ),
    dir_legend = factor(dir_legend,
                        levels = c("A better (BH ≤ 0.05)",
                                   "B better (BH ≤ 0.05)",
                                   "Not significant (BH > 0.05)"))
  )

label_colors <- c(
  "A better (BH ≤ 0.05)"        = "firebrick3",
  "B better (BH ≤ 0.05)"        = "royalblue3",
  "Not significant (BH > 0.05)" = "grey30"
)

# ---------- plot ----------
p <- ggplot(plot_df, aes(x = method_A, y = method_B, fill = median_diff)) +
  geom_tile(color = "white") +
  geom_text(aes(label = label, color = dir_legend),
            size = 5, fontface = "bold", na.rm = TRUE) +
  scale_fill_gradient2(
    low = "royalblue2", mid = "white", high = "firebrick2", midpoint = 0,
    na.value = "grey95",
    name = "Median difference (F1)\nMethod A − Method B"
  ) +
  scale_color_manual(
    values = label_colors,
    name = "Direction key\nStars indicate significance",
    labels = c(
      "A better (BH ≤ 0.05)"        = "Arrow ↑  → A significantly higher",
      "B better (BH ≤ 0.05)"        = "Arrow ↓  → B significantly higher"
      #"Not significant (BH > 0.05)" = "No arrow/stars → no significant difference\n\nStars: * p≤0.05, ** p≤0.01, *** p≤0.001"
    )
  ) +
  facet_wrap(~ level, nrow = 1) +
  labs(
    title = "",
    x = "Method A", y = "Method B"
  ) +
  theme_bw(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_rect(fill = "grey90"),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p)
ggsave(file.path(path_root, "Result", "F1_pairwise_heatmap_4x4_no_diagonal.png"),
       p, width = 12, height = 4.8, dpi = 300)
