rm(list=ls())
# Load libraries
library(ggplot2)
library(readr)
library(reshape2)
F1_score_total  = read.csv("Result/F1_score_total.csv")
F1_score_total = F1_score_total [,2:ncol(F1_score_total)]
tab <- as.data.frame(F1_score_total)
numeric_cols <- setdiff(names(tab), "b")

summ <- data.frame(
  metric = numeric_cols,
  median = sapply(tab[numeric_cols], function(z) median(as.numeric(z), na.rm=TRUE)),
  q25    = sapply(tab[numeric_cols], function(z) quantile(as.numeric(z), 0.25, na.rm=TRUE)),
  q75    = sapply(tab[numeric_cols], function(z) quantile(as.numeric(z), 0.75, na.rm=TRUE))
)
print(summ, row.names = FALSE)


# Read your results
F1 <- F1_score_total

# Keep only the columns for baseline vs. bootstrap stability
F1_sub <- F1[, c("F1_log_vs_logB",
                 "F1_clr_vs_clrB",
                 "F1_gmpr_vs_gmprB",
                 "F1_tss_vs_tssB")]

# Rename columns for clarity
colnames(F1_sub) <- c("Internal log", "CLR", "GMPR", "TSS")

# tell melt there are no id columns
F1_long <- reshape2::melt(F1_sub, id.vars = NULL,
                          variable.name = "Normalization",
                          value.name = "F1")

# Basic boxplot
p <- ggplot(F1_long, aes(x = Normalization, y = F1, fill = Normalization)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 1.5, alpha = 0.8) +
  scale_fill_brewer(palette = "Set2") +
  theme_bw(base_size = 13) +
  labs(title = "Stability of CMIMN under Different Normalizations",
       x = "Normalization Method",
       y = "F1 Score (Baseline vs Bootstrap)") +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(size = 12, angle = 20, hjust = 1)
  )

# Print plot
print(p)

# Optionally save to file
ggsave("Result/CMIMN_normalization_F1_boxplot.png", p, width = 6.5, height = 4.5, dpi = 300)

