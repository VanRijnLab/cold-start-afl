## ----------------------------------------------------------------------------
##
## Collect raw experiment data from CSV files, remove identifying information,
## and turn into usable data frames for the rest of the analysis.
##
## Author: Maarten van der Velde
##
## Last updated: 2019-10-25
##
## ----------------------------------------------------------------------------

library(dplyr)
library(purrr)
library(readr)
library(fst)



# Read raw data -----------------------------------------------------------

dir_raw <- file.path("data", "raw")
dir_processed <- file.path("data", "processed")


# Session 1 (one learning block)

files_1 <- list.files(
  path = file.path(dir_raw, "s1"),
  pattern = ".csv",
  recursive = TRUE,
  full.names = TRUE
)

raw_1 <- map_df(files_1, read_csv, col_types = cols(
  .default = col_character(),
  value = col_logical(),
  trial_index = col_double(),
  time_elapsed = col_double(),
  participant_number = col_double(),
  rt = col_double(),
  presentation_start_time = col_double(),
  id = col_double(),
  text = col_logical(),
  correct = col_logical(),
  study = col_logical(),
  backspace_used = col_logical(),
  backspaced_first_letter = col_logical())
)

s1 <- raw_1 %>%
  select(-value, -participant_number, -browser_info, -view_history, -responses, -keypresses) %>%
  mutate(
    text = "",
    correct = as.logical(correct),
    study = as.logical(study),
    backspace_used = as.logical(backspace_used),
    backspaced_first_letter = as.logical(backspaced_first_letter)
  ) %>%
  rename(
    start_time = presentation_start_time,
    fact_id = id
  ) %>%
  mutate_at(vars(subject, fact_id), as.factor)

s1_rl <- filter(s1, trial_type == "html-keyboard-word-response" & grepl("0.0-8", internal_node_id)) %>%
  mutate(threshold = -0.8) %>%
  select(-trial_type, -time_elapsed, -internal_node_id) %>%
  droplevels()


write_fst(s1_rl, path = file.path(dir_processed, "s1", "s1_rl.fst"))


# Session 2 (two learning blocks and two tests)

files_2 <- list.files(
  path = file.path(dir_raw, "s2"),
  pattern = ".csv",
  recursive = TRUE,
  full.names = TRUE
)

raw_2 <- map_df(files_2, read_csv, col_types = cols(
  .default = col_character(),
  value = col_logical(),
  trial_index = col_double(),
  time_elapsed = col_double(),
  participant_number = col_double(),
  participant_sona = col_double(),
  rt = col_double(),
  presentation_start_time = col_double(),
  id = col_double(),
  text = col_logical(),
  correct = col_logical(),
  study = col_logical(),
  backspace_used = col_logical(),
  backspaced_first_letter = col_logical(),
  tetris_highscore = col_double(),
  tetris_games_played = col_double())
)

s2 <- raw_2 %>%
  select(-value, -participant_number, -participant_sona, -responses, -keypresses, -browser_info, -view_history, -tetris_highscore, -tetris_games_played, -url) %>%
  mutate(
    text = "",
    correct = as.logical(correct),
    study = as.logical(study),
    backspace_used = as.logical(backspace_used),
    backspaced_first_letter = as.logical(backspaced_first_letter)
  ) %>%
  rename(
    start_time = presentation_start_time,
    fact_id = id
  ) %>%
  mutate_at(vars(subject, fact_id, condition, condition_order), as.factor)

s2_rl1 <- filter(s2, grepl("0.0-9", internal_node_id)) %>%
  mutate(block = as.factor(1),
         threshold = -0.8) %>%
  select(-trial_type, -time_elapsed, -internal_node_id) %>%
  droplevels()

s2_facts1 <- s2_rl1 %>%
  distinct(subject, condition, fact_id) %>%
  mutate(studied = TRUE)

s2_test1 <- filter(s2, grepl("0.0-16", internal_node_id)) %>%
  mutate(block = as.factor(1), condition = as.factor(ifelse(condition_order == "DF", "default", "fact"))) %>%
  select(subject, block, condition, fact_id, correct) %>%
  droplevels() %>%
  left_join(s2_facts1, by = c("subject", "condition", "fact_id")) %>%
  mutate(studied = ifelse(is.na(studied), FALSE, studied))

s2_rl2 <- filter(s2, grepl("0.0-21", internal_node_id)) %>%
  mutate(block = as.factor(2),
         threshold = -0.8) %>%
  select(-trial_type, -time_elapsed, -internal_node_id) %>%
  droplevels()

s2_facts2 <- s2_rl2 %>%
  distinct(subject, condition, fact_id) %>%
  mutate(studied = TRUE)

s2_test2 <- filter(s2, grepl("0.0-27", internal_node_id)) %>%
  mutate(block = as.factor(2), condition = as.factor(ifelse(condition_order == "DF", "fact", "default"))) %>%
  select(subject, block, condition, fact_id, correct) %>%
  droplevels() %>%
  left_join(s2_facts2, by = c("subject", "condition", "fact_id")) %>%
  mutate(studied = ifelse(is.na(studied), FALSE, studied))

s2_tetris <- filter(raw_2, !is.na(tetris_highscore)) %>% 
  select(subject, tetris_highscore) %>%
  group_by(subject) %>%
  mutate(block = as.factor(c(1, 2)))

write_fst(s2_rl1, path = file.path(dir_processed, "s2", "s2_rl1.fst"))
write_fst(s2_test1, path = file.path(dir_processed, "s2", "s2_test1.fst"))
write_fst(s2_rl2, path = file.path(dir_processed, "s2", "s2_rl2.fst"))
write_fst(s2_test2, path = file.path(dir_processed, "s2", "s2_test2.fst"))
write_fst(s2_tetris, path = file.path(dir_processed, "s2", "s2_tetris.fst"))
