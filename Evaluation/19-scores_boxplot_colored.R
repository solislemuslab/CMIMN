rm(list=ls())

library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(forcats)

# ---------- paths ----------
path_root   <- "/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code"
scores_file <- file.path(path_root, "Result", "scores_F1_Jaccard.csv")

# ---------- load & tidy ----------
df <- read_csv(scores_file, show_col_types = FALSE)

# map method names to paper labels & enforce order
df <- df %>%
  mutate(
    # method = recode(method,
    #                 "BN"        = "CMIMN",
    #                 "SE_glasso" = "SE_glasso",
    #                 "sparcc"    = "SPARCC",
    #                 "SP"        = "SPRING"
    # ),
    method = recode(method,
                    "BN"        = "CMIMN_log",
                    "BN2"        = "CMIMN_clr",
                    "SE_glasso" = "SE_glasso",
                    "sparcc"    = "SPARCC",
                    "SP"        = "SPRING"
    ),
    level  = factor(level, levels = c("phylum","class","order")),
    #method = factor(method, levels = c("CMIMN","SE_glasso","SPARCC","SPRING"))
    method = factor(method, levels = c("CMIMN_log","CMIMN_clr","SE_glasso","SPARCC","SPRING"))
  ) %>%
  pivot_longer(c(F1, Jaccard), names_to = "Metric", values_to = "Value")

# Okabe–Ito palette (colorblind-friendly, high contrast)
# pal_okabe <- c(
#   "CMIMN"     = "#E69F00",
#   "SE_glasso" = "#009E73",
#   "SPARCC"    = "#56B4E9",
#   "SPRING"    = "#D55E00"
# )
pal_okabe <- c(
  "CMIMN_log"     = "#E69F00",
  "CMIMN_clr"     = "red",
  "SE_glasso" = "#009E73",
  "SPARCC"    = "#56B4E9",
  "SPRING"    = "#D55E00"
)

set.seed(42)
p <- ggplot(df, aes(x = method, y = Value, fill = method, color = method)) +
  geom_boxplot(width = 0.7, outlier.shape = NA, alpha = 0.85, linewidth = 0.5) +
  geom_jitter(position = position_jitter(width = 0.15, height = 0, seed = 42),
              size = 1.2, alpha = 0.6) +
  facet_grid(Metric ~ level, scales = "free_y") +
  scale_fill_manual(values = pal_okabe, guide = "none") +
  scale_color_manual(values = pal_okabe, guide = "none") +
  labs(x = "Method", y = "Score", title = "") +
  theme_bw(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "grey90"),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.grid.minor = element_blank()
  )

print(p)
ggsave(file.path(path_root, "Result", "scores_boxplot_colored.png"),
       p, width = 12, height = 7, dpi = 300)
