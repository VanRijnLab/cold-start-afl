## ----------------------------------------------------------------------------
##
## Calculate the model estimates (rate of forgetting, activation, expected RT)
## for data collected in Session 1.
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

s1_rl <- read_fst(file.path(dir_s1, "s1_rl.fst"))

s1_responses <- s1_rl %>%
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


s1_by_subj_and_fact <- split(s1_responses, list(s1_responses$subject, s1_responses$fact_id), drop = TRUE)

# Calculate model parameter values at time of repetition
s1_calc <- future_map_dfr(s1_by_subj_and_fact, function (x) {
  x %>%
    rowwise() %>%
    mutate(activation = calculate_activation(start_time, fact_id, 0.3, x[x$start_time < start_time,]),
           alpha = calculate_alpha(start_time, fact_id, 0.3, x[x$start_time < start_time,])) %>%
    mutate(estimated_rt = estimate_reaction_time_from_activation(activation, get_reading_time(text))) %>%
    ungroup()
}) %>%
  group_by(subject, fact_id) %>%
  arrange(start_time) %>%
  mutate(repetition = 1:n()) %>%
  ungroup() %>%
  arrange(subject, start_time)


# Calculate final alpha values directly after last observation
s1_final_alpha <- future_map_dfr(s1_by_subj_and_fact, function (x) {
  x %>%
    summarise(subject = subject[1],
              fact_id = fact_id[1],
              reps = n(),
              final_alpha = calculate_alpha(max(start_time) + 1, fact_id, 0.3, x))
})

write_fst(s1_calc, path = file.path(dir_s1, "s1_rl_calc.fst"))
write_fst(s1_final_alpha, path = file.path(dir_s1, "s1_final_alpha.fst"))