## ----------------------------------------------------------------------------
##
## Calculate the model estimates (rate of forgetting, activation, expected RT). 
##
## Author: Maarten van der Velde
##
## Last updated: 2019-07-11
##
## ----------------------------------------------------------------------------

library(dplyr)
library(parallel)
library(purrr)
library(fst)

source("analysis/slimstampen_model_funs.R")


# Set up multi-core processing (note: this may not work on Windows; see ?makeCluster for workarounds)
no_cores <- max(1, detectCores() - 1)
cl <- makeCluster(no_cores, type = "FORK")



# Lab data ----------------------------------------------------------------

# Session 1

lab_data_path_1 <- file.path("data", "processed", "lab", "session1")

lab_rl <- read.fst(file.path(lab_data_path_1, "session1_rl_lab.fst"))

responses <- lab_rl %>%
  group_by(subject) %>%
  mutate(start_time = (start_time - min(start_time))) %>%
  ungroup() %>%
  select(subject,
         fact_id,
         text,
         answer,
         start_time,
         rt,
         correct,
         threshold)


# Calculate model parameter values at time of repetition
responses_with_model_params <- responses %>%
  split(responses$subject) %>%
  parLapply(cl, ., function(x) {
    x %>%
      rowwise() %>%
      mutate(activation = calculate_activation(start_time, fact_id, 0.3, x[x$start_time < start_time,]),
             alpha = calculate_alpha(start_time, fact_id, 0.3, x[x$start_time < start_time,])) %>%
      mutate(estimated_rt = estimate_reaction_time_from_activation(activation, get_reading_time(text))) %>%
      ungroup()
  }) %>%
  bind_rows() %>%
  group_by(subject, fact_id) %>%
  arrange(start_time) %>%
  mutate(repetition = 1:n()) %>%
  ungroup() %>%
  arrange(subject, start_time)

# Calculate final alpha values directly after last observation
lab_rl_final_alpha <- responses %>%
  split(list(responses$subject, responses$fact_id)) %>%
  parLapply(cl, ., function(x) {
    x %>%
      summarise(subject = subject[1],
                fact_id = fact_id[1],
                reps = n(),
                final_alpha = calculate_alpha(max(start_time) + 1, fact_id, 0.3, x))
  }) %>%
  bind_rows() %>%
  na.omit()


write.fst(responses_with_model_params, path = file.path(lab_data_path_1, "session1_rl_with_alpha_lab.fst"))
write.fst(lab_rl_final_alpha, path = file.path(lab_data_path_1, "session1_final_alpha_lab.fst"))



# MTurk data --------------------------------------------------------------

# Session 1

mturk_data_path_1 <- file.path("data", "processed", "mturk", "session1")

mturk_rl <- read.fst(file.path(mturk_data_path_1, "session1_rl_mturk.fst"))

responses <- mturk_rl %>%
  group_by(subject) %>%
  mutate(start_time = (start_time - min(start_time))) %>%
  ungroup() %>%
  select(subject,
         fact_id,
         text,
         answer,
         start_time,
         rt,
         correct,
         threshold)


# Calculate model parameter values at time of repetition
responses_with_model_params <- responses %>%
  split(responses$subject) %>%
  parLapply(cl, ., function(x) {
    x %>%
      rowwise() %>%
      mutate(activation = calculate_activation(start_time, fact_id, 0.3, x[x$start_time < start_time,]),
             alpha = calculate_alpha(start_time, fact_id, 0.3, x[x$start_time < start_time,])) %>%
      mutate(estimated_rt = estimate_reaction_time_from_activation(activation, get_reading_time(text))) %>%
      ungroup()
  }) %>%
  bind_rows() %>%
  group_by(subject, fact_id) %>%
  arrange(start_time) %>%
  mutate(repetition = 1:n()) %>%
  ungroup() %>%
  arrange(subject, start_time)

# Calculate final alpha values directly after last observation
mturk_rl_final_alpha <- responses %>%
  split(list(responses$subject, responses$fact_id)) %>%
  parLapply(cl, ., function(x) {
    x %>%
      summarise(subject = subject[1],
                fact_id = fact_id[1],
                reps = n(),
                final_alpha = calculate_alpha(max(start_time) + 1, fact_id, 0.3, x))
  }) %>%
  bind_rows() %>%
  na.omit()


write.fst(responses_with_model_params, path = file.path(mturk_data_path_1, "session1_rl_with_alpha_mturk.fst"))
write.fst(mturk_rl_final_alpha, path = file.path(mturk_data_path_1, "session1_final_alpha_mturk.fst"))


#  -----------------------------------------------------------------------

stopCluster(cl)
