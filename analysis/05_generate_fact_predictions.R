## ----------------------------------------------------------------------------
##
## Generate fact-level rate of forgetting predictions to be used in Session 2,
## using the observed final rates of forgetting on these facts in Session 1.
## Also generate a domain-level prediction that is the mean of all fact-level
## predictions.
##
## Author: Maarten van der Velde
##
## Last updated: 2019-07-11
##
## ----------------------------------------------------------------------------

library(dplyr)
library(purrr)
library(readr)
library(tidyr)
library(fst)

source("analysis/bayes_funs.R")

# Model priors
mu_0 <- 0.3 # mean (b)
kappa_0 <- 1 # number of observations/precision (c)
a_0 <- 3 # shape of Gamma (g)
b_0 <- 0.2 # rate of Gamma (h)

set1 <- read_csv("stimuli/set1.csv", na = character())


# Lab ---------------------------------------------------------------------


lab_rl_final_alpha <- read.fst(file.path("data", "processed", "lab", "session1", "session1_final_alpha_lab.fst"))

alpha_lab <- lab_rl_final_alpha %>%
  filter(reps >= 3)


fact_lab <- alpha_lab %>%
  nest(-fact_id) %>%
  mutate(res = map(data, ~ run_bayes_model(.$final_alpha))) %>%
  select(-data) %>%
  unnest(res) %>%
  transmute(id = fact_id,
            mu = mu_n,
            mu_domain = mean(mu_n),
            kappa = kappa_n,
            a = a_n,
            b = b_n)


set1_lab <- left_join(set1, fact_lab, by = "id")

write_csv(set1_lab, path = "stimuli/set1_with_alpha_lab.csv")



# MTurk -------------------------------------------------------------------

mturk_rl_final_alpha <- read.fst(file.path("data", "processed", "mturk", "session1", "session1_final_alpha_mturk.fst"))

alpha_mturk <- mturk_rl_final_alpha %>%
  filter(reps >= 3)

fact_mturk <- alpha_mturk %>%
  nest(-fact_id) %>%
  mutate(res = map(data, ~ run_bayes_model(.$final_alpha))) %>%
  select(-data) %>%
  unnest(res) %>%
  transmute(id = fact_id,
            mu = mu_n,
            mu_domain = mean(mu_n),
            kappa = kappa_n,
            a = a_n,
            b = b_n)


set1_mturk <- left_join(set1, fact_mturk, by = "id")

write_csv(set1_mturk, path = "stimuli/set1_with_alpha_mturk.csv")