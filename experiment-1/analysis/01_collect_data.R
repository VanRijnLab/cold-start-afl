## ----------------------------------------------------------------------------
##
## Collect raw experiment data from CSV files, remove identifying information,
## and turn into usable data frames for the rest of the analysis.
##
## Author: Maarten van der Velde
##
## Last updated: 2019-07-11
##
## ----------------------------------------------------------------------------

library(dplyr)
library(jsonlite)
library(purrr)
library(readr)
library(fst)



# Lab data ----------------------------------------------------------------

lab_dir_raw <- file.path("data", "raw", "lab")
lab_dir_processed <- file.path("data", "processed", "lab")

# Session 1

lab_files_1 <- list.files(
  path = file.path(lab_dir_raw, "session1"),
  pattern = ".csv",
  recursive = TRUE,
  full.names = TRUE
)

lab_raw_1 <- map_df(lab_files_1, read_csv)

# Ensure that we only keep data from verified participants
lab_ids_1 <- read_csv(file.path(lab_dir_raw, "session1_lab_participants.csv")) %>%
  pull(id_code)

lab_1 <- filter(lab_raw_1, participant_number %in% lab_ids_1) %>%
  select(-value, -participant_number, -browser_info, -view_history) %>%
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
  )

lab_1_typing <- filter(lab_1, trial_type == "html-keyboard-word-response" & grepl("0.0-3", internal_node_id)) %>%
  select(-trial_type, -time_elapsed, -internal_node_id, -responses, -fact_id, -text, -study)

lab_1_rl <- filter(lab_1, trial_type == "html-keyboard-word-response" & grepl("0.0-7", internal_node_id)) %>%
  mutate(threshold = -0.8) %>%
  select(-trial_type, -time_elapsed, -internal_node_id, -responses)


write.fst(lab_1_typing, path = file.path(lab_dir_processed, "session1", "session1_typing_lab.fst"))
write.fst(lab_1_rl, path = file.path(lab_dir_processed, "session1", "session1_rl_lab.fst"))


# Session 2

lab_files_2 <- list.files(
  path = file.path(lab_dir_raw, "session2"),
  pattern = ".csv",
  recursive = TRUE,
  full.names = TRUE
)

lab_raw_2 <- map_df(lab_files_2, read_csv)

# Ensure that we only keep data from verified participants
lab_ids_2 <- read_csv(file.path(lab_dir_raw, "session2_lab_participants.csv")) %>%
  pull(id_code)


# One participant mistyped their ID in the form, so include their data via their anonymous subject code instead
lab_2 <- filter(lab_raw_2, participant_sona %in% lab_ids_2 | subject == "ypk474e140dxr6e") %>%
  select(-value, -participant_number, -participant_sona, -browser_info, -view_history) %>%
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
  )

# Fix incorrect condition labelling bug that occurred in the first few days of data collection
lab_2 <- filter(lab_2, !is.na(start_time)) %>%
  mutate(timestamp = as.POSIXct(start_time / 1000, origin = "1970-01-01")) %>%
  group_by(subject) %>%
  mutate(condition = ifelse(any(timestamp < as.POSIXct("2019-03-26 14:00", origin = "1970-01-01")), "fact", condition)) %>% # up to this point participants always got the "fact" condition, regardless of the actual label
  mutate(condition = ifelse(is.na(condition), "fact", condition)) %>% # indexing was broken for a short while longer, which also caused these participants to always receive the "fact" condition
  ungroup() %>%
  mutate(condition = ifelse(condition == "both", "fact-and-student", condition)) # rename the "both" condition


lab_2_typing <- filter(lab_2, trial_type == "html-keyboard-word-response" & grepl("0.0-3", internal_node_id)) %>%
  select(-trial_type, -time_elapsed, -internal_node_id, -url, -tetris_highscore, -tetris_games_played, -responses, -fact_id, -text, -study, -condition, -timestamp)

lab_2_rl_1 <- filter(lab_2, trial_type == "html-keyboard-word-response" & grepl("0.0-7", internal_node_id)) %>%
  mutate(threshold = -0.8) %>%
  select(-trial_type, -time_elapsed, -internal_node_id, -url, -tetris_highscore, -tetris_games_played, -responses, -timestamp)

lab_2_rl_2 <- filter(lab_2, trial_type == "html-keyboard-word-response" & grepl("0.0-19", internal_node_id)) %>%
  mutate(threshold = -0.8) %>%
  select(-trial_type, -time_elapsed, -internal_node_id, -url, -tetris_highscore, -tetris_games_played, -responses, -timestamp)

lab_2_test_1 <- filter(lab_2, trial_type == "html-keyboard-word-response" & grepl("0.0-14", internal_node_id)) %>%
  select(-trial_type, -time_elapsed, -internal_node_id, -url, -tetris_highscore, -tetris_games_played, -responses, -study, -timestamp)

lab_2_test_2 <- filter(lab_2, trial_type == "html-keyboard-word-response" & grepl("0.0-25", internal_node_id)) %>%
  select(-trial_type, -time_elapsed, -internal_node_id, -url, -tetris_highscore, -tetris_games_played, -responses, -study, -timestamp)


write.fst(lab_2_typing, path = file.path(lab_dir_processed, "session2", "session2_typing_lab.fst"))
write.fst(lab_2_rl_1, path = file.path(lab_dir_processed, "session2", "session2_rl1_lab.fst"))
write.fst(lab_2_rl_2, path = file.path(lab_dir_processed, "session2", "session2_rl2_lab.fst"))
write.fst(lab_2_test_1, path = file.path(lab_dir_processed, "session2", "session2_test1_lab.fst"))
write.fst(lab_2_test_2, path = file.path(lab_dir_processed, "session2", "session2_test2_lab.fst"))





# MTurk data --------------------------------------------------------------

mturk_dir_raw <- file.path("data", "raw", "mturk")
mturk_dir_processed <- file.path("data", "processed", "mturk")

# Session 1

mturk_files_1 <- list.files(
  path = file.path(mturk_dir_raw, "session1"),
  pattern = ".csv",
  recursive = TRUE,
  full.names = TRUE
)

mturk_raw_1 <- map_df(mturk_files_1, read_csv)

# Ensure that we only keep data from verified participants (check against downloaded batch results from MTurk)
mturk_batch_records_1 <- list.files(
  path = mturk_dir_raw,
  pattern = "session1_batch\\d+.csv",
  recursive = TRUE,
  full.names = TRUE
)

whitelist_1 <- map_df(mturk_batch_records_1, read_csv) %>%
  filter(AssignmentStatus == "Approved") %>%
  pull(Answer.surveycode)

mturk_raw_1 <- filter(mturk_raw_1, subject %in% whitelist_1)

# Clicking away during the session results in a half-complete trial, which we can recognise by the missing presentation_start_time.
# Participants who clicked away did not complete their session, so they have to be removed from the data.
incomplete_sessions_1 <- mturk_raw_1 %>%
  group_by(subject) %>%
  filter(
    trial_type == "html-keyboard-word-response" &
      grepl("0.0-7", internal_node_id) &
      is.na(presentation_start_time)
  ) %>%
  ungroup() %>%
  distinct(subject)

mturk_1 <- mturk_raw_1 %>%
  anti_join(incomplete_sessions_1, by = "subject") %>%
  select(-value, -browser_info, -view_history) %>%
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
  )

# Extract questionnaire responses
mturk_1_questionnaire <- mturk_1 %>%
  filter(trial_type == "survey-multi-choice") %>%
  cbind(., map_df(.$responses, fromJSON)) %>%
  select(subject,
         rt,
         degree = Q0,
         occupation = Q1)

# Extract typing test
mturk_1_typing <- filter(mturk_1, trial_type == "html-keyboard-word-response" & grepl("0.0-3", internal_node_id)) %>%
  select(-trial_type, -time_elapsed, -internal_node_id, -url, -responses, -fact_id, -text, -study)

# Extract RL trials
mturk_1_rl <- filter(mturk_1, trial_type == "html-keyboard-word-response" & grepl("0.0-7", internal_node_id)) %>%
  mutate(threshold = -0.8) %>%
  select(-trial_type, -time_elapsed, -internal_node_id, -url, -responses)

write.fst(mturk_1_questionnaire, path = file.path(mturk_dir_processed, "session1", "session1_questionnaire_mturk.fst"))
write.fst(mturk_1_typing, path = file.path(mturk_dir_processed, "session1", "session1_typing_mturk.fst"))
write.fst(mturk_1_rl, path = file.path(mturk_dir_processed, "session1", "session1_rl_mturk.fst"))


# Session 2

mturk_files_2 <- list.files(
  path = file.path(mturk_dir_raw, "session2"),
  pattern = ".csv",
  recursive = TRUE,
  full.names = TRUE
)

mturk_raw_2 <- map_df(mturk_files_2, read_csv)

# Ensure that we only keep data from verified participants
mturk_batch_records_2 <- list.files(
  path = mturk_dir_raw,
  pattern = "session2_batch\\d+.csv",
  recursive = TRUE,
  full.names = TRUE
)

whitelist_2 <- map_df(mturk_batch_records_2, read_csv) %>%
  filter(AssignmentStatus == "Approved") %>%
  pull(Answer.surveycode)

mturk_raw_2 <- filter(mturk_raw_2, subject %in% whitelist_2)

# Clicking away during the session results in a half-complete trial, which we can recognise by the missing presentation_start_time.
# Participants who clicked away did not complete their session, so they have to be removed from the data.
incomplete_sessions_2 <- mturk_raw_2 %>%
  group_by(subject) %>%
  filter(trial_type == "html-keyboard-word-response" &
           is.na(presentation_start_time)) %>%
  ungroup() %>%
  distinct(subject)

mturk_2 <- mturk_raw_2 %>%
  anti_join(incomplete_sessions_2, by = "subject") %>%
  select(-value, -browser_info, -view_history) %>%
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
  )

# Condition was not correctly labelled in the first 15 cases because of a bug (should always be "fact")
# Relabel using anonymous IDs
fact_condition <- c(
  "r544o7sk62p2deg",
  "8smvmwyahy2opol",
  "dr4fs1rh551c235",
  "04nx9fqg7gu56jx",
  "2jctml9q0pr1r35",
  "0dee0s463utgaf9",
  "5y5y98jxwylh86n",
  "cjxkbq89spjt6d4",
  "px1jrckj6cyko83",
  "gvtrn6cznz5vt1w",
  "y3ayj1qtcpk4ztj",
  "sgsgsx5k56h9xpz",
  "msqpdpq0g2yhj33",
  "2xy4vn380awyx6g",
  "7ux8umq9rhnzufv"
)


mturk_2 <- mturk_2 %>%
  mutate(condition = ifelse(subject %in% fact_condition, "fact", condition)) %>%
  mutate(condition = ifelse(condition == "both", "fact-and-student", condition)) # rename "both" condition

mturk_2_questionnaire <- mturk_2 %>%
  filter(trial_type == "survey-multi-choice") %>%
  cbind(., map_df(.$responses, fromJSON)) %>%
  select(subject,
         rt,
         degree = Q0,
         occupation = Q1)

mturk_2_typing <- filter(mturk_2, trial_type == "html-keyboard-word-response" & grepl("0.0-3", internal_node_id)) %>%
  select(-trial_type, -time_elapsed, -internal_node_id, -url, -tetris_highscore, -tetris_games_played, -responses, -fact_id, -text, -study, -condition)


mturk_2_rl_1 <- filter(mturk_2, trial_type == "html-keyboard-word-response" & grepl("0.0-7", internal_node_id)) %>%
  mutate(threshold = -0.8) %>%
  select(-trial_type, -time_elapsed, -internal_node_id, -url, -tetris_highscore, -tetris_games_played, -responses)

mturk_2_rl_2 <- filter(mturk_2, trial_type == "html-keyboard-word-response" & grepl("0.0-19", internal_node_id)) %>%
  mutate(threshold = -0.8) %>%
  select(-trial_type, -time_elapsed, -internal_node_id, -url, -tetris_highscore, -tetris_games_played, -responses)

mturk_2_test_1 <- filter(mturk_2, trial_type == "html-keyboard-word-response" & grepl("0.0-14", internal_node_id)) %>%
  select(-trial_type, -time_elapsed, -internal_node_id, -url, -tetris_highscore, -tetris_games_played, -responses, -study)

mturk_2_test_2 <- filter(mturk_2, trial_type == "html-keyboard-word-response" & grepl("0.0-25", internal_node_id)) %>%
  select(-trial_type, -time_elapsed, -internal_node_id, -url, -tetris_highscore, -tetris_games_played, -responses, -study)

write.fst(mturk_2_questionnaire, path = file.path(mturk_dir_processed, "session2", "session2_questionnaire_mturk.fst"))
write.fst(mturk_2_typing, path = file.path(mturk_dir_processed, "session2", "session2_typing_mturk.fst"))
write.fst(mturk_2_rl_1, path = file.path(mturk_dir_processed, "session2", "session2_rl1_mturk.fst"))
write.fst(mturk_2_rl_2, path = file.path(mturk_dir_processed, "session2", "session2_rl2_mturk.fst"))
write.fst(mturk_2_test_1, path = file.path(mturk_dir_processed, "session2", "session2_test1_mturk.fst"))
write.fst(mturk_2_test_2, path = file.path(mturk_dir_processed, "session2", "session2_test2_mturk.fst"))
