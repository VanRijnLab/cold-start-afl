## ----------------------------------------------------------------------------
##
## Generate learner-level rate of forgetting predictions to be used in the
## second block of Session 2, using the observed final rates of forgetting of
## these learners in the first learning block.
## Also generate learner&fact predictions using opinion pooling, and store all
## predictions in one place.
##
## Author: Maarten van der Velde
##
## Last updated: 2019-07-12
##
## ----------------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(ggpubr)
library(parallel)
library(readr)
library(tidyr)
library(fst)

source("analysis/slimstampen_model_funs.R")
source("analysis/bayes_funs.R")

mu_0 <- 0.3 # mean (b)
kappa_0 <- 1 # number of observations/precision (c)
a_0 <- 3 # shape of Gamma (g)
b_0 <- 0.2 # rate of Gamma (h)

prior <- tibble(mu_n = mu_0,
                kappa_n = kappa_0,
                a_n = a_0,
                b_n = b_0)


# Set up multi-core processing
no_cores <- max(1, detectCores() - 1)
cl <- makeCluster(no_cores, type = "FORK")



# LAB DATA ----------------------------------------------------------------

lab_2_rl_1 <- read.fst(file.path("data", "processed", "lab", "session2", "session2_rl1_lab.fst"))
lab_2_rl_2 <- read.fst(file.path("data", "processed", "lab", "session2", "session2_rl2_lab.fst"))

# The first block has the default value of alpha, so run the model in the regular way

responses_lab_1 <- lab_2_rl_1 %>%
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
responses_lab_1_with_model_params <- responses_lab_1 %>%
  split(responses_lab_1$subject) %>%
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
final_alpha_lab_1 <- responses_lab_1 %>%
  split(list(responses_lab_1$subject, responses_lab_1$fact_id)) %>%
  parLapply(cl, ., function(x) {
    x %>%
      summarise(subject = subject[1],
                fact_id = fact_id[1],
                reps = n(),
                final_alpha = calculate_alpha(max(start_time) + 1, fact_id, 0.3, x))
  }) %>%
  bind_rows() %>%
  na.omit()


# Run Bayesian model to predict learner alpha for block 2
learner_alpha_lab <- final_alpha_lab_1 %>%
  group_by(fact_id) %>%
  filter(reps >= 3) %>%
  ungroup() %>%
  select(subject, final_alpha) %>%
  nest_legacy(-subject) %>%
  mutate(alpha = map(data, ~ run_bayes_model(.$final_alpha))) %>%
  unnest_legacy(alpha) %>%
  select(-data) %>%
  select(subject,
         mu_l = mu_n,
         kappa_l = kappa_n,
         a_l = a_n,
         b_l = b_n)



# Fact alpha predictions for set 1, based on the data from Experiment 1, are stored in the stimulus file
fact_alpha_lab <- read_csv("stimuli/set1_with_alpha_lab.csv") %>%
  select(fact_id = id,
         mu_f = mu,
         mu_domain,
         kappa_f = kappa,
         a_f = a,
         b_f = b)


# Now we can make predictions for block 2
predictions_lab_2 <- crossing(learner_alpha_lab, fact_alpha_lab) %>%
  rowwise() %>%
  mutate(fact_pred = mu_f,
         learner_pred = mu_l,
         domain_pred = mu_domain,
         default_pred = 0.3,
         fact_and_learner_pred = get_mode(
           calculate_logarithmic_pool(
             calculate_t_distr(mu_f, kappa_f, a_f, b_f),
             calculate_t_distr(mu_l, kappa_l, a_l, b_l)
             )
           )
         ) %>%
  ungroup() %>%
  select(subject,
         fact_id,
         fact_pred,
         learner_pred,
         fact_and_learner_pred,
         domain_pred,
         default_pred)


# Recalculate the model parameters for block 2 using the predicted values as initial values
responses_lab_2 <- lab_2_rl_2 %>%
  group_by(subject) %>%
  mutate(start_time = (start_time - min(start_time))) %>%
  rowwise() %>%
  mutate(prediction = case_when(
    condition == "fact" ~ predictions_lab_2[predictions_lab_2$subject == subject & predictions_lab_2$fact_id == fact_id,]$fact_pred,
    condition == "student" ~ predictions_lab_2[predictions_lab_2$subject == subject & predictions_lab_2$fact_id == fact_id,]$learner_pred,
    condition == "fact-and-student" ~ predictions_lab_2[predictions_lab_2$subject == subject & predictions_lab_2$fact_id == fact_id,]$fact_and_learner_pred,
    condition == "domain" ~ predictions_lab_2[predictions_lab_2$subject == subject & predictions_lab_2$fact_id == fact_id,]$domain_pred,
    condition == "default" ~ predictions_lab_2[predictions_lab_2$subject == subject & predictions_lab_2$fact_id == fact_id,]$default_pred,
    TRUE ~ -999
  )) %>%
  ungroup() %>%
  select(subject,
         condition,
         fact_id,
         text,
         answer,
         start_time,
         rt,
         correct,
         threshold,
         prediction)


# Calculate model parameter values at time of repetition using the predictions made earlier
responses_lab_2_with_model_params <- responses_lab_2 %>%
  split(responses_lab_2$subject) %>%
  parLapply(cl, ., function(x) {
    x %>%
      rowwise() %>%
      mutate(activation = calculate_activation(start_time, fact_id, prediction, x[x$start_time < start_time,]),
             alpha = calculate_alpha(start_time, fact_id, prediction, x[x$start_time < start_time,])) %>%
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
final_alpha_lab_2 <- responses_lab_2 %>%
  split(list(responses_lab_2$subject, responses_lab_2$fact_id)) %>%
  parLapply(cl, ., function(x) {
      x %>%
      summarise(subject = subject[1],
                fact_id = fact_id[1],
                prediction = prediction[1],
                reps = n(),
                final_alpha = calculate_alpha(max(start_time) + 1, fact_id, prediction, x))
  }) %>%
  bind_rows() %>%
  na.omit()


# Include the condition in this dataframe
condition <- responses_lab_2_with_model_params %>%
  distinct(subject, condition)

final_alpha_lab_2 <- left_join(final_alpha_lab_2, condition, by = c("subject"))


# Save lab data
write.fst(responses_lab_1_with_model_params, file.path("data", "processed", "lab", "session2", "session2_rl1_with_alpha_lab.fst"))
write.fst(responses_lab_2_with_model_params, file.path("data", "processed", "lab", "session2", "session2_rl2_with_alpha_lab.fst"))
write.fst(final_alpha_lab_1, file.path("data", "processed", "lab", "session2", "session2_rl1_final_alpha_lab.fst"))
write.fst(final_alpha_lab_2, file.path("data", "processed", "lab", "session2", "session2_rl2_final_alpha_lab.fst"))
write.fst(predictions_lab_2, file.path("data", "processed", "lab", "session2", "session2_rl2_predictions_lab.fst"))





# MECHANICAL TURK DATA ----------------------------------------------------

mturk_2_rl_1 <- read.fst(file.path("data", "processed", "mturk", "session2", "session2_rl1_mturk.fst"))
mturk_2_rl_2 <- read.fst(file.path("data", "processed", "mturk", "session2", "session2_rl2_mturk.fst"))

# The first block has the default value of alpha, so run the model in the regular way

responses_mturk_1 <- mturk_2_rl_1 %>%
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
responses_mturk_1_with_model_params <- responses_mturk_1 %>%
  split(responses_mturk_1$subject) %>%
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
final_alpha_mturk_1 <- responses_mturk_1 %>%
  split(list(responses_mturk_1$subject, responses_mturk_1$fact_id)) %>%
  parLapply(cl, ., function(x) {
    x %>%
      summarise(subject = subject[1],
                fact_id = fact_id[1],
                reps = n(),
                final_alpha = calculate_alpha(max(start_time) + 1, fact_id, 0.3, x))
  }) %>%
  bind_rows() %>%
  na.omit()


# Run Bayesian model to predict learner alpha for block 2
learner_alpha_mturk <- final_alpha_mturk_1 %>%
  group_by(fact_id) %>%
  filter(reps >= 3) %>%
  ungroup() %>%
  select(subject, final_alpha) %>%
  nest_legacy(-subject) %>%
  mutate(alpha = map(data, ~ run_bayes_model(.$final_alpha))) %>%
  unnest_legacy(alpha) %>%
  select(-data) %>%
  select(subject,
         mu_l = mu_n,
         kappa_l = kappa_n,
         a_l = a_n,
         b_l = b_n)



# Fact alpha predictions for set 1, based on the data from Experiment 1, are stored in the stimulus file
fact_alpha_mturk <- read_csv("stimuli/set1_with_alpha_mturk.csv") %>%
  select(fact_id = id,
         mu_f = mu,
         mu_domain,
         kappa_f = kappa,
         a_f = a,
         b_f = b)


# Now we can make predictions for block 2
predictions_mturk_2 <- crossing(learner_alpha_mturk, fact_alpha_mturk) %>%
  rowwise() %>%
  mutate(fact_pred = mu_f,
         learner_pred = mu_l,
         domain_pred = mu_domain,
         default_pred = 0.3,
         fact_and_learner_pred = get_mode(
           calculate_logarithmic_pool(
             calculate_t_distr(mu_f, kappa_f, a_f, b_f),
             calculate_t_distr(mu_l, kappa_l, a_l, b_l)
           )
         )
  ) %>%
  ungroup() %>%
  select(subject,
         fact_id,
         fact_pred,
         learner_pred,
         fact_and_learner_pred,
         domain_pred,
         default_pred)


# Recalculate the model parameters for block 2 using the predicted values as initial values
responses_mturk_2 <- mturk_2_rl_2 %>%
  group_by(subject) %>%
  mutate(start_time = (start_time - min(start_time))) %>%
  rowwise() %>%
  mutate(prediction = case_when(
    condition == "fact" ~ predictions_mturk_2[predictions_mturk_2$subject == subject & predictions_mturk_2$fact_id == fact_id,]$fact_pred,
    condition == "student" ~ predictions_mturk_2[predictions_mturk_2$subject == subject & predictions_mturk_2$fact_id == fact_id,]$learner_pred,
    condition == "fact-and-student" ~ predictions_mturk_2[predictions_mturk_2$subject == subject & predictions_mturk_2$fact_id == fact_id,]$fact_and_learner_pred,
    condition == "domain" ~ predictions_mturk_2[predictions_mturk_2$subject == subject & predictions_mturk_2$fact_id == fact_id,]$domain_pred,
    condition == "default" ~ predictions_mturk_2[predictions_mturk_2$subject == subject & predictions_mturk_2$fact_id == fact_id,]$default_pred,
    TRUE ~ -999
  )) %>%
  ungroup() %>%
  select(subject,
         condition,
         fact_id,
         text,
         answer,
         start_time,
         rt,
         correct,
         threshold,
         prediction)


# Calculate model parameter values at time of repetition using the predictions made earlier
responses_mturk_2_with_model_params <- responses_mturk_2 %>%
  split(responses_mturk_2$subject) %>%
  parLapply(cl, ., function(x) {
    x %>%
      rowwise() %>%
      mutate(activation = calculate_activation(start_time, fact_id, prediction, x[x$start_time < start_time,]),
             alpha = calculate_alpha(start_time, fact_id, prediction, x[x$start_time < start_time,])) %>%
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
final_alpha_mturk_2 <- responses_mturk_2 %>%
  split(list(responses_mturk_2$subject, responses_mturk_2$fact_id)) %>%
  parLapply(cl, ., function(x) {
    x %>%
      summarise(subject = subject[1],
                fact_id = fact_id[1],
                prediction = prediction[1],
                reps = n(),
                final_alpha = calculate_alpha(max(start_time) + 1, fact_id, prediction, x))
    
  }) %>%
  bind_rows() %>%
  na.omit()


# Include the condition in this dataframe
condition <- responses_mturk_2_with_model_params %>%
  distinct(subject, condition)

final_alpha_mturk_2 <- left_join(final_alpha_mturk_2, condition, by = c("subject"))


# Save mturk data
write.fst(responses_mturk_1_with_model_params, file.path("data", "processed", "mturk", "session2", "session2_rl1_with_alpha_mturk.fst"))
write.fst(responses_mturk_2_with_model_params, file.path("data", "processed", "mturk", "session2", "session2_rl2_with_alpha_mturk.fst"))
write.fst(final_alpha_mturk_1, file.path("data", "processed", "mturk", "session2", "session2_rl1_final_alpha_mturk.fst"))
write.fst(final_alpha_mturk_2, file.path("data", "processed", "mturk", "session2", "session2_rl2_final_alpha_mturk.fst"))
write.fst(predictions_mturk_2, file.path("data", "processed", "mturk", "session2", "session2_rl2_predictions_mturk.fst"))


# End ---------------------------------------------------------------------

stopCluster(cl)