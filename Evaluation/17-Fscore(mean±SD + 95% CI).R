library(dplyr); library(tidyr); library(readr); library(stringr)

path <- "Result/scores_f1_jaccard.csv"
raw <- read_csv(path, col_names = FALSE, show_col_types = FALSE)
stopifnot(nrow(raw) >= 4)

level_row <- raw %>% slice(2) %>% unlist(use.names = FALSE) %>% as.character() %>% str_trim()
algo_row  <- raw %>% slice(3) %>% unlist(use.names = FALSE) %>% as.character() %>% str_trim()

keep_idx <- str_detect(level_row, "^[A-Za-z]") & str_detect(algo_row, "^[A-Za-z]")
dat <- raw %>% slice(-(1:3))
dat <- dat[, keep_idx, drop = FALSE]
names(dat) <- paste(level_row[keep_idx], algo_row[keep_idx], sep = "|")

dat <- dat %>% mutate(across(everything(), ~ suppressWarnings(as.numeric(.x))))

long <- dat %>%
  mutate(replicate = row_number()) %>%
  pivot_longer(-replicate, names_to = c("level","algorithm"),
               names_sep = "\\|", values_to = "F1") %>%
  filter(!is.na(F1))

ci_tbl <- long %>%
  group_by(level, algorithm) %>%
  summarise(
    mean_F1  = mean(F1),
    sd_F1    = sd(F1),
    mean_sd  = sprintf("%.3f ± %.3f", mean_F1, sd_F1),
    ci_lower = quantile(F1, 0.025, type = 7),
    ci_upper = quantile(F1, 0.975, type = 7),
    ci_interval = sprintf("[%.3f, %.3f]", ci_lower, ci_upper),
    .groups = "drop"
  ) %>%
  select(level, algorithm, mean_sd, ci_interval)

write_csv(ci_tbl, "Result/F1_meanSD_CI_by_level_algorithm.csv")
