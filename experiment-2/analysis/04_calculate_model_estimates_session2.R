## ----------------------------------------------------------------------------
##
## Calculate the model estimates (rate of forgetting, activation, expected RT)
## for data collected in Session 2.
##
## Author: Maarten van der Velde
##
## Last updated: 2019-10-25
##
## ----------------------------------------------------------------------------

library(dplyr)
library(furrr)
library(fst)

source("analysis/slimstampen_model_funs.R")

dir_s1 <- file.path("data", "processed", "s1")
dir_s2 <- file.path("data", "processed", "s2")

fact_predictions <- read_fst(file.path(dir_s1, "fact_predictions.fst")) %>%
  select(fact_id, 
         prediction = mu)


# Block 1 -----------------------------------------------------------------

s2_rl1 <- read_fst(file.path(dir_s2, "s2_rl1.fst"))

s2_rl1_responses <- s2_rl1 %>%
  group_by(subject) %>%
  mutate(start_time = (start_time - min(start_time))) %>%
  ungroup() %>%
  select(subject,
         condition,
         fact_id,
         text,
         answer,
         start_time,
         rt,
         correct,
         threshold) %>%
  left_join(fact_predictions, by = "fact_id") %>%
  mutate(prediction = ifelse(condition == "default", 0.3, prediction))

s2_rl1_by_subj_and_fact <- split(s2_rl1_responses, list(s2_rl1_responses$subject, s2_rl1_responses$fact_id), drop = TRUE)

# Calculate model parameter values at time of repetition
s2_rl1_calc <- future_map_dfr(s2_rl1_by_subj_and_fact, function (x) {
  x %>%
    rowwise() %>%
    mutate(activation = calculate_activation(start_time, fact_id, prediction, x[x$start_time < start_time,]),
           alpha = calculate_alpha(start_time, fact_id, prediction, x[x$start_time < start_time,])) %>%
    mutate(estimated_rt = estimate_reaction_time_from_activation(activation, get_reading_time(text))) %>%
    ungroup()
}) %>%
  group_by(subject, fact_id) %>%
  arrange(start_time) %>%
  mutate(repetition = 1:n()) %>%
  ungroup() %>%
  arrange(subject, start_time)


# Calculate final alpha values directly after last observation
s2_rl1_final_alpha <- future_map_dfr(s2_rl1_by_subj_and_fact, function (x) {
  x %>%
    summarise(subject = subject[1],
              fact_id = fact_id[1],
              reps = n(),
              final_alpha = calculate_alpha(max(start_time) + 1, fact_id, prediction[1], x))
})

write_fst(s2_rl1_calc, path = file.path(dir_s2, "s2_rl1_calc.fst"))
write_fst(s2_rl1_final_alpha, path = file.path(dir_s2, "s2_rl1_final_alpha.fst"))


# Block 2 -----------------------------------------------------------------


s2_rl2 <- read_fst(file.path(dir_s2, "s2_rl2.fst"))

s2_rl2_responses <- s2_rl2 %>%
  group_by(subject) %>%
  mutate(start_time = (start_time - min(start_time))) %>%
  ungroup() %>%
  select(subject,
         condition,
         fact_id,
         text,
         answer,
         start_time,
         rt,
         correct,
         threshold) %>%
  left_join(fact_predictions, by = "fact_id") %>%
  mutate(prediction = ifelse(condition == "default", 0.3, prediction))

s2_rl2_by_subj_and_fact <- split(s2_rl2_responses, list(s2_rl2_responses$subject, s2_rl2_responses$fact_id), drop = TRUE)

# Calculate model parameter values at time of repetition
s2_rl2_calc <- future_map_dfr(s2_rl2_by_subj_and_fact, function (x) {
  x %>%
    rowwise() %>%
    mutate(activation = calculate_activation(start_time, fact_id, prediction, x[x$start_time < start_time,]),
           alpha = calculate_alpha(start_time, fact_id, prediction, x[x$start_time < start_time,])) %>%
    mutate(estimated_rt = estimate_reaction_time_from_activation(activation, get_reading_time(text))) %>%
    ungroup()
}) %>%
  group_by(subject, fact_id) %>%
  arrange(start_time) %>%
  mutate(repetition = 1:n()) %>%
  ungroup() %>%
  arrange(subject, start_time)


# Calculate final alpha values directly after last observation
s2_rl2_final_alpha <- future_map_dfr(s2_rl2_by_subj_and_fact, function (x) {
  x %>%
    summarise(subject = subject[1],
              fact_id = fact_id[1],
              reps = n(),
              final_alpha = calculate_alpha(max(start_time) + 1, fact_id, prediction[1], x))
})

write_fst(s2_rl2_calc, path = file.path(dir_s2, "s2_rl2_calc.fst"))
write_fst(s2_rl2_final_alpha, path = file.path(dir_s2, "s2_rl2_final_alpha.fst"))
