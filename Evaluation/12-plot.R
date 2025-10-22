rm(list= ls())
# Load the ggplot2 library
library(ggplot2)

# Create a data frame with your data
data <- data.frame(
  level = rep(c("Phylum", "Class", "Order"), each = 16),
  Normalization_method = rep(rep(c("clr", "original", "log", "TSS"), each = 4), times = 3),
  Constructed_method = rep(c("BN", "SPARCC", "SE_glasso", "SPRING"), each = 1, times = 12),
  value = c(
    0.5, 0.375, 0.375, 0.5, 0.15, 0.4, 0.3, 0.2, 0.473684211, 0.526315789, 0.368421053, 0.342105263,
    0.25, 0.25, 0.25, 0.375, 0.25, 0.45, 0.45, 0.2, 0.605263158, 0.684210526, 0.605263158, 0.368421053,
    0.25, 0.125, 0.25, 0.25, 0.2, 0.4, 0.4, 0.15, 0.578947368, 0.631578947, 0.526315789, 0.289473684,
    0.5, 0.375, 0.375, 0.375, 0.5, 0.55, 0.55, 0.35, 0.657894737, 0.684210526, 0.552631579, 0.368421053
  )
)

v = c(
  0.5, 0.375, 0.375, 0.5, 0.15, 0.4, 0.3, 0.2, 0.473684211, 0.526315789, 0.368421053, 0.342105263,
  0.25, 0.25, 0.25, 0.375, 0.25, 0.45, 0.45, 0.2, 0.605263158, 0.684210526, 0.605263158, 0.368421053,
  0.25, 0.125, 0.25, 0.25, 0.2, 0.4, 0.4, 0.15, 0.578947368, 0.631578947, 0.526315789, 0.289473684,
  0.5, 0.375, 0.375, 0.375, 0.5, 0.55, 0.55, 0.35, 0.657894737, 0.684210526, 0.552631579, 0.368421053
)

# Reorder the levels
data$level <- factor(data$level, levels = c("Phylum", "Class", "Order"))

# Create the plot using ggplot2
plot <- ggplot(data, aes(x = Normalization_method , y = value, fill = Constructed_method)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, shape = 21) +
  facet_grid(. ~ level, scales = "free", space = "free_x") +
  labs(
    x = "Normalization Methods",
    y = "Agreement between ML and Network-based methods"
  ) +
  scale_fill_manual(values = c("BN" = "black", "SPARCC" = "blue", "SE_glasso" = "darkgray", "SPRING" = "purple")) +  # Assign colors to methods
  theme_minimal() +
  guides(fill = guide_legend(title = NULL)) +
  ylim(0, 0.8) +  # Set the y-axis range
  geom_vline(xintercept = c(0.5,1.5,2.5,3.5, 4.5, 5.5,6.5,7.5,8.5,9.5), color = "gray", linetype = "dashed")+  # Add vertical lines
theme(panel.grid.major.x = element_blank())+
  geom_segment(data = data[data$Normalization_method == "clr" & data$level == "Phylum", ], aes(x = 1, xend = 4, y = mean(v[1:16]), yend = mean(v[1:16])), color = "blue", linetype = "dashed", size = 1) +
  geom_segment(data = data[data$Normalization_method == "clr" & data$level == "Class", ], aes(x = 1, xend = 4, y = mean(v[17:32]), yend = mean(v[17:32])), color = "blue", linetype = "dashed", size = 1) +
  geom_segment(data = data[data$Normalization_method == "clr" & data$level == "Order", ], aes(x = 1, xend = 4, y = mean(v[33:48]), yend = mean(v[33:48])), color = "blue", linetype = "dashed", size = 1) 
 print(plot)
# Save the plot with white background
ggsave("overlapbetweenMLandnetwork.png", plot = plot, width = 10, height = 6, dpi = 300, bg = "white")


###################################3Net2
rm(list = ls())
# Load the ggplot2 library
library(ggplot2)

# Create a data frame with your new data
data <- data.frame(
  level = rep(c("Phylum", "Class", "Order"), each = 16),
  Normalization_method= rep(rep(c("clr", "original", "log", "TSS"), each = 4), times = 3),
  Constructed_method = rep(c("BN", "SPARCC", "SE_glasso", "SPRING"), each = 1, times = 12),
  value = c(
    0.375, 0.625, 0.5, 0.5, 0.2, 0.25, 0.4, 0.2, 0.394736842, 0.5, 0.394736842, 0.315789474,
    0.25, 0.375, 0.25, 0.25, 0.35, 0.25, 0.35, 0.15, 0.473684211, 0.631578947, 0.578947368, 0.368421053,
    0.25, 0.5, 0.375, 0.375, 0.3, 0.3, 0.4, 0.2, 0.394736842, 0.473684211, 0.473684211, 0.315789474,
    0.75, 0.625, 0.5, 0.625, 0.55, 0.5, 0.45, 0.4, 0.605263158, 0.605263158, 0.578947368, 0.368421053
  )
)
v= c(
  0.375, 0.625, 0.5, 0.5, 0.2, 0.25, 0.4, 0.2, 0.394736842, 0.5, 0.394736842, 0.315789474,
  0.25, 0.375, 0.25, 0.25, 0.35, 0.25, 0.35, 0.15, 0.473684211, 0.631578947, 0.578947368, 0.368421053,
  0.25, 0.5, 0.375, 0.375, 0.3, 0.3, 0.4, 0.2, 0.394736842, 0.473684211, 0.473684211, 0.315789474,
  0.75, 0.625, 0.5, 0.625, 0.55, 0.5, 0.45, 0.4, 0.605263158, 0.605263158, 0.578947368, 0.368421053
)

# Reorder the levels
data$level <- factor(data$level, levels = c("Phylum", "Class", "Order"))
# Create the plot using ggplot2
# plot_new <- ggplot(data, aes(x = method, y = value, fill = normalization)) +
#   geom_point(position = position_dodge(width = 0.8), size = 3, shape = 21) +
#   facet_grid(. ~ level, scales = "free", space = "free_x") +
#   labs(
#     x = "Normalization Methods",
#     y = "Values"
#   ) +
# 
# 
# scale_fill_manual(values = c("BN" = "black", "SPARCC" = "blue", "SE_glasso" = "green", "SPRING" = "purple")) +  # Assign colors to methods
#   theme_minimal() +
#   theme(legend.position = "top") +
#   guides(fill = guide_legend(title = NULL)) +
#   ylim(0, 0.8)  # Set the y-axis range

plot_new <- ggplot(data, aes(x = Normalization_method , y = value, fill = Constructed_method)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, shape = 21) +
  facet_grid(. ~ level, scales = "free", space = "free_x") +
  labs(
    x = "Normalization Methods",
    y = "Agreement between ML and Network-based methods"
  ) +
  scale_fill_manual(values = c("BN" = "black", "SPARCC" = "blue", "SE_glasso" = "darkgray", "SPRING" = "purple")) +  # Assign colors to methods
  theme_minimal() +
  guides(fill = guide_legend(title = NULL)) +
  ylim(0, 0.8) +  # Set the y-axis range
  geom_vline(xintercept = c(0.5,1.5,2.5,3.5, 4.5, 5.5,6.5,7.5,8.5,9.5), color = "gray", linetype = "dashed")+  # Add vertical lines
  theme(panel.grid.major.x = element_blank())+
  geom_segment(data = data[data$Normalization_method == "clr" & data$level == "Phylum", ], aes(x = 1, xend = 4, y = mean(v[1:16]), yend = mean(v[1:16])), color = "blue", linetype = "dashed", size = 1) +
  geom_segment(data = data[data$Normalization_method == "clr" & data$level == "Class", ], aes(x = 1, xend = 4, y = mean(v[17:32]), yend = mean(v[17:32])), color = "blue", linetype = "dashed", size = 1) +
  geom_segment(data = data[data$Normalization_method == "clr" & data$level == "Order", ], aes(x = 1, xend = 4, y = mean(v[33:48]), yend = mean(v[33:48])), color = "blue", linetype = "dashed", size = 1) 
print(plot_new)

# Save the plot with white background
ggsave("overlapbetweenMLandnetwork2.png", plot = plot_new, width = 10, height = 6, dpi = 300, bg = "white")




############################################################BOXPLOT for simulation
library(ggplot2)

# Load the data from the CSV file
data <- read.csv("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/Result/FScore.csv", row.names = 1)

# Extract the level and algorithm information from the first two rows
level <- as.character(data[1, ])
algorithm <- as.character(data[2, ])

# Remove the first three rows to keep only the values
data_values <- data[3:nrow(data), ]

# Transpose the data so that it's in the correct format for melting
data_transposed <- t(data_values)

data_df <- data.frame(Level = rep(level, each = ncol(data_transposed)),
                      Algorithm = rep(algorithm, each = ncol(data_transposed)),
                      Value = as.numeric(unlist(data_values)))

# Reorder the levels
data_df$Level <- factor(data_df$Level, levels = c("phylum", "class", "order"))

# Define a color palette for the three levels
my_colors <- c("skyblue1", "green", "gray")

# Create the ggplot object
plot <- ggplot(data_df, aes(x = Algorithm, y = Value, fill = Level)) +
  geom_boxplot() +
  labs(x = "Algorithm", y = "F-score") +
  scale_fill_manual(values = my_colors) +
  theme_minimal() +
  ylim(0.4, 1.02)  # Set the y-axis range

plot <- plot + scale_x_discrete(labels = c("BN" = "OIPCQ", "SE_glasso" = "SE_glasso", "SP" = "SPRING", "sparcc" = "SPARCC"))

print(plot)
# Save the plot with a white background
ggsave("robusstness.png", plot = plot, width = 10, height = 6, dpi = 300, bg = "white")