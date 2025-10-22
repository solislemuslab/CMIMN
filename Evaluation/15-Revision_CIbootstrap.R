library(dplyr)
library(tidyr)
library(readr)
library(stringr)

path <- "Result/FScore.csv"

raw <- read_csv(path, col_names = FALSE, show_col_types = FALSE)
if (nrow(raw) < 4) stop("Expected at least 4 rows: index, levels, algorithms, data.")

# Row 2 = level, Row 3 = algorithm
level_row <- raw %>% slice(2) %>% unlist(use.names = FALSE) %>% as.character() %>% str_trim()
algo_row  <- raw %>% slice(3) %>% unlist(use.names = FALSE) %>% as.character() %>% str_trim()

# Keep only columns whose headers are valid (start with a letter)
keep_idx <- str_detect(level_row, "^[A-Za-z]") & str_detect(algo_row, "^[A-Za-z]")

# Data starts row 4
dat <- raw %>% slice(-(1:3))
dat <- dat[, keep_idx, drop = FALSE]

# Composite colnames
names(dat) <- paste(level_row[keep_idx], algo_row[keep_idx], sep = "|")

# Convert to numeric
dat <- dat %>% mutate(across(everything(), ~ suppressWarnings(as.numeric(.x))))

# Long format
long <- dat %>%
  mutate(replicate = row_number()) %>%
  pivot_longer(-replicate, names_to = c("level","algorithm"), names_sep = "\\|",
               values_to = "F1") %>%
  filter(!is.na(F1))

# Summary with mean ± SD and CI
ci_tbl <- long %>%
  group_by(level, algorithm) %>%
  summarise(
    mean_F1  = mean(F1, na.rm = TRUE),
    sd_F1    = sd(F1, na.rm = TRUE),
    mean_sd  = sprintf("%.3f ± %.3f", mean_F1, sd_F1),
    ci_lower = quantile(F1, 0.025, na.rm = TRUE, type = 7),
    ci_upper = quantile(F1, 0.975, na.rm = TRUE, type = 7),
    ci_interval = sprintf("[%.3f, %.3f]", ci_lower, ci_upper),
    .groups = "drop"
  ) %>%
  select(level, algorithm, mean_sd, ci_interval)

# Save
if (!dir.exists("Result")) dir.create("Result", recursive = TRUE, showWarnings = FALSE)
write_csv(ci_tbl, "Result/F1_meanSD_CI_by_level_algorithm.csv")
