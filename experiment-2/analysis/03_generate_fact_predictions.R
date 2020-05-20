## ----------------------------------------------------------------------------
##
## Generate fact-level rate of forgetting predictions to be used in Session 2,
## using the observed final rates of forgetting on these facts in Session 1.
##
## Author: Maarten van der Velde
##
## Last updated: 2020-05-20
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

stimuli <- read_csv(file.path("stimuli", "setC.csv"),
                    col_types = cols(
                      id = col_double(),
                      image = col_character(),
                      text = col_logical(),
                      answer = col_character()
                      )
                    )


s1_final_alpha_full <- read_fst(file.path("data", "processed", "s1", "s1_final_alpha.fst"))


# Keep only final rate of forgetting estimates from facts that the particpiants saw at least 3 times
s1_final_alpha <- s1_final_alpha_full %>%
  filter(reps >= 3) %>%
  mutate(fact_id = as.numeric(as.character(fact_id)))

s1_pred <- s1_final_alpha %>%
  nest_legacy(-fact_id) %>%
  mutate(res = map(data, ~ run_bayes_model(.$final_alpha))) %>%
  select(-data) %>%
  unnest_legacy(res) %>%
  transmute(id = fact_id,
            mu = mu_n,
            mu_domain = mean(mu_n),
            kappa = kappa_n,
            a = a_n,
            b = b_n)

# Create stimulus file to use in Session 2
stimuli_alpha <- left_join(stimuli, s1_pred, by = "id")
write_csv(stimuli_alpha, path = "stimuli/setC_with_alpha.csv")


# Save predictions for analysis
fact_predictions <- s1_pred %>%
  rename(fact_id = id) %>%
  mutate(fact_id = as.factor(fact_id))

write_fst(fact_predictions, file.path("data", "processed", "s1", "fact_predictions.fst"))
