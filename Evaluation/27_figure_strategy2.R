# --- One figure: df1/df2 ALL-methods + UNION + a single "Per method (each)" bar first ---

rm(list = ls()); suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tidyr); library(ggplot2)
})

csv_path <- "/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/Result/toppthereshold/summary_topP_by_level_method_strategy2.csv"
out_png  <- "/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/Result/toppthereshold/Strategy2_numbertaxa.png"
dir.create(dirname(out_png), showWarnings = FALSE, recursive = TRUE)

df0 <- read_csv(csv_path, show_col_types = FALSE) %>%
  mutate(
    p     = factor(p, levels = c(10, 20, 30), labels = c("> 10", "> 20", "> 30")),
    level = factor(level, levels = c("phylum", "class", "order"),
                   labels = c("Phylum", "Class", "Order"))
  )

# (1) ONE "Per method (each)" bar per (level, p)
per_method <- df0 %>%
  group_by(p, level) %>%
  summarise(`Per method (each)` = unique(n_df1_method)[1], .groups = "drop") %>%
  pivot_longer(-c(p, level), names_to = "group", values_to = "n")

# (2) Bars for ALL-methods (intersection) and UNION
all_union <- df0 %>%
  select(p, level, n_df1_allMethods, n_df2_allMethods, n_union_allMethods_df1_df2) %>%
  distinct() %>%
  transmute(
    p, level,
    `Healthy (df1) — ALL methods` = n_df1_allMethods,
    `Disease (df2) — ALL methods` = n_df2_allMethods,
    `Union (df1 ∪ df2)`           = n_union_allMethods_df1_df2
  ) %>%
  pivot_longer(-c(p, level), names_to = "group", values_to = "n")

# Combine + relabel legend items & set order
label_map <- c(
  "Per method (each)"                 = "Selected taxa in each network",
  "Healthy (df1) — ALL methods"       = "Healthy — ALL methods",
  "Disease (df2) — ALL methods"       = "Disease — ALL methods",
  "Union (df1 ∪ df2)"                 = "Union (Healthy ∪ Disease)"
)

plot_df <- bind_rows(per_method, all_union) %>%
  mutate(group = recode(group, !!!label_map)) %>%
  mutate(group = factor(group, levels = c(
    "Selected taxa in each network",
    "Healthy — ALL methods",
    "Disease — ALL methods",
    "Union (Healthy ∪ Disease)"
  )))

# colors (consistent with your prior figures)
col_per   <- "#999999"  # neutral gray for the single "Per method" bar
col_df1   <- "#F8766D"  # red
col_df2   <- "#7CAE00"  # green
col_union <- "#C77CFF"  # purple

dodge_w <- 0.80
p_fig <- ggplot(plot_df, aes(x = p, y = n, fill = group)) +
  geom_col(position = position_dodge(width = dodge_w), width = 0.65) +
  geom_text(aes(label = n),
            position = position_dodge(width = dodge_w),
            vjust = -0.35, size = 3.4, na.rm = TRUE) +
  facet_grid(level ~ .) +
  scale_fill_manual(values = c(
    "Selected taxa in each network" = col_per,
    "Healthy — ALL methods"         = col_df1,
    "Disease — ALL methods"         = col_df2,
    "Union (Healthy ∪ Disease)"     = col_union
  )) +
  labs(
    x = "Strategy 2 selection threshold (top p%)",
    y = "Number of taxa",
    fill = "",
    title = ""
  ) +
  theme_bw() +
  expand_limits(y = max(plot_df$n, na.rm = TRUE) * 1.15)

ggsave(out_png, p_fig, width = 8, height = 9, dpi = 300)
message("Saved: ", out_png)
