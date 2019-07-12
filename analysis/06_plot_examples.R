## ----------------------------------------------------------------------------
##
## Visualise rate of forgetting development for a single fact in session 1,
## plot examples of model predictions.
##
## Author: Maarten van der Velde
##
## Last updated: 2019-07-12
##
## ----------------------------------------------------------------------------


library(dplyr)
library(ggplot2)
library(ggforce)
library(patchwork)
library(purrr)
library(scales)
library(tidyr)
library(extraDistr)
library(extrafont)
library(fst)

# font_import() # Run once to populate the R font database with the fonts on the system

loadfonts()

theme_set(theme_light(base_size = 14) +
            theme(text = element_text(family = 'Merriweather Sans')))

set.seed(1)

source("analysis/bayes_funs.R")

# load("data/lab/experiment1/lab_data_1_with_alpha.rda")
lab_1_rl <- read.fst(file.path("data", "processed", "lab", "session1", "session1_rl_with_alpha_lab.fst"))

# Only look at one fact
buchanan <- lab_1_rl %>%
  group_by(subject) %>%
  mutate(time = (start_time - min(start_time)) / 60000) %>%
  filter(fact_id == 5) %>%
  filter(n() >= 3)  %>%
  ungroup() %>%
  # filter(subject %in% sample(subject, 20)) %>%
  identity()

buchanan_last <- buchanan %>%
  group_by(subject) %>%
  arrange(time) %>%
  slice(n()) %>%
  ungroup()



# Plot alpha development during session for all learners ------------------

alpha_plot <- ggplot(buchanan, aes(x = time, y = alpha)) +
  geom_line(alpha = 0.5, aes(colour = subject)) +
  geom_point(alpha = 0.5, size = 1, aes(colour = subject, fill = subject)) +
  geom_mark_ellipse(data = buchanan_last, expand = 0.02, aes(colour = subject, fill = subject)) +
  guides(colour = FALSE, fill = FALSE) +
  scale_x_continuous(breaks = seq(0, 10, 2), minor_breaks = NULL) +
  scale_y_continuous(breaks = seq(0.2, 0.8, 0.1), limits = c(0.199, 0.601), minor_breaks = NULL) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  labs(x = "Time (minutes)",
       y = "Rate of forgetting")

alpha_plot
ggsave("output/plots/alpha_plot.pdf", device = "pdf", width = 4, height = 3)
ggsave("output/plots/alpha_plot_narrow.pdf", device = "pdf", width = 3, height = 3)


# Highlight final estimate in plot ----------------------------------------

alpha_plot +
  geom_mark_ellipse(data = buchanan_last, alpha = 0.1, aes(fill = as.factor(fact_id), label = "Final estimate"))
ggsave("output/plots/alpha_plot_highlight.pdf", device = "pdf", width = 4, height = 3)


# Plot distribution of final alpha estimates ------------------------------

final_alpha_dist <- ggplot(buchanan_last, aes(x = as.factor(fact_id), y = alpha)) +
  geom_boxplot(outlier.shape = NA, width = 0.75) +
  geom_point(alpha = 0.5, size = 1, aes(colour = subject, fill = subject), position = position_jitter(width = 0.2, height = 0, seed = 1)) +
  geom_mark_ellipse(data = buchanan_last, expand = 0.06, aes(colour = subject, fill = subject), position = position_jitter(width = 0.2, height = 0, seed = 1)) +
  guides(colour = FALSE, fill = FALSE) +
  scale_x_discrete(labels = c("")) +
  scale_y_continuous(breaks = seq(0.2, 0.8, 0.1), limits = c(0.199, 0.601), minor_breaks = NULL) +
  scale_colour_viridis_d() +
  scale_fill_viridis_d() +
  labs(x = "", 
       y = "Rate of forgetting") +
  theme(axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank())

final_alpha_dist
ggsave("output/plots/final_alpha_dist.pdf", device = "pdf", width = 1.2, height = 2.2)



# Combine alpha development and final distribution in one plot ------------
final_alpha_dist_noY <- ggplot(buchanan_last, aes(x = as.factor(fact_id), y = alpha)) +
  geom_boxplot(outlier.shape = NA, width = 0.75) +
  geom_point(alpha = 0.5, size = 1, aes(colour = subject, fill = subject), position = position_jitter(width = 0.2, height = 0, seed = 1)) +
  geom_mark_ellipse(data = buchanan_last, expand = 0.08, aes(colour = subject, fill = subject), position = position_jitter(width = 0.2, height = 0, seed = 1)) +
  guides(colour = FALSE, fill = FALSE) +
  scale_x_discrete(labels = c("")) +
  scale_y_continuous(breaks = seq(0.2, 0.8, 0.1), limits = c(0.199, 0.601), minor_breaks = NULL) +
  scale_colour_viridis_d() +
  scale_fill_viridis_d() +
  labs(x = "", 
       y = "Rate of forgetting") +
  theme(axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())


alpha_plot + final_alpha_dist_noY + plot_layout(widths = c(0.8, 0.2))
ggsave("output/plots/alpha_plot_with_dist.pdf", device = "pdf", width = 3, height = 2.2)




# Plot posterior predictive distribution ----------------------------------

source("analysis/bayes_funs.R")

mu_0 <- 0.3 # mean (b)
kappa_0 <- 1 # number of observations/precision (c)
a_0 <- 3 # shape of Gamma (g)
b_0 <- 0.2 # rate of Gamma (h)

prior <- tibble(mu_n = mu_0,
                kappa_n = kappa_0,
                a_n = a_0,
                b_n = b_0)

buchanan_bayes <- bind_rows(prior, 
                            run_bayes_model_incremental(buchanan_last$alpha)) %>%
  mutate(obs = as.factor(0:(n()-1)))





post_pred <- buchanan_bayes %>%
  nest(-obs) %>%
  mutate(postpred = map(data, ~ calculate_t_distr(.$mu_n, .$kappa_n, .$a_n, .$b_n))) %>%
  unnest(postpred)

post_pred_first <- filter(post_pred, obs == 0)
post_pred_last <- filter(post_pred, as.numeric(obs) == nrow(buchanan_bayes))
post_pred_between <- filter(post_pred, obs != 0 & obs != nrow(buchanan_bayes))


post_pred_plot <- ggplot(post_pred_between, aes(x = x, y = y, colour = obs)) +
  geom_line(data = post_pred_first, colour = "darkgrey", lwd = 1.25, lty = 2) +
  geom_line(lwd = 0.75) +
  geom_line(data = post_pred_last, colour = "black", lwd = 2.0) +
  annotate("segment", x = tail(buchanan_bayes,1)$mu_n, xend = tail(buchanan_bayes,1)$mu_n, y = 3.75, yend = 3.25, arrow = arrow(angle = 20, length = unit(0.1, "inches"), type = "closed")) +
  annotate("text", x = tail(buchanan_bayes,1)$mu_n, y = 4, label = formatC(tail(buchanan_bayes,1)$mu_n, digits = 3)) +
  # geom_line(data = post_pred_last, colour = "white", lwd = 0.5) +
  # scale_colour_viridis_d(option = "E", direction = -1, begin = 0.25, end = 0.75) +
  scale_x_continuous(minor_breaks = NULL) +
  scale_y_continuous(minor_breaks = NULL, limits = c(0,4)) +
  scale_colour_viridis_d(option = "C", direction = -1, begin = 0.1, end = 0.9) +
  guides(colour = FALSE) +
  labs(x = "Rate of forgetting",
       y = "Density")

post_pred_plot
ggsave("output/plots/post_pred_plot.pdf", device = "pdf", width = 2.2, height = 2.2)




# Illustrate the conditions -----------------------------------------------

# Plot a few posterior predictive distributions in the same graph

lab_1_rl_final_alpha <- read.fst(file.path("data", "processed", "lab", "session1", "session1_final_alpha_lab.fst"))

alpha <- lab_1_rl_final_alpha %>%
  filter(reps >= 3)


fact_res <- alpha %>%
  nest(-fact_id) %>%
  mutate(res = map(data, ~ run_bayes_model(.$final_alpha))) %>%
  select(-data)

post_pred_facts <- fact_res %>%
  mutate(postpred = map(res, ~ calculate_t_distr(.$mu_n, .$kappa_n, .$a_n, .$b_n))) %>%
  unnest(postpred)

post_pred_select <- post_pred_facts %>%
  filter(fact_id %in% c(6, 21, 27))

predictions <- post_pred_select %>%
  group_by(fact_id) %>%
  filter(y == max(y))

ggplot(post_pred_select, aes(x = x, y = y, colour = as.factor(fact_id))) +
  geom_line(lwd = 0.75) +
  scale_x_continuous(minor_breaks = NULL) +
  scale_y_continuous(minor_breaks = NULL, limits = c(0,5)) +
  geom_segment(data = predictions, aes(x = x, xend = x, y = 4.525 + 1 * (y - mean(y)), yend = 3.9 + 1 * (y - mean(y)), colour = as.factor(fact_id)), arrow = arrow(angle = 20, length = unit(0.1, "inches"), type = "closed")) +
  scale_colour_viridis_d(option = "D", direction = -1, begin = 0.1, end = 0.9) +
  guides(colour = FALSE) +
  labs(x = "Rate of forgetting",
       y = "",
       title = "Fact") +
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())

ggsave("output/plots/post_pred_3_1.pdf", device = "pdf", width = 3, height = 3)


post_pred_select2 <- post_pred_facts %>%
  filter(fact_id %in% c(7, 24, 30))

predictions <- post_pred_select2 %>%
  group_by(fact_id) %>%
  filter(y == max(y))

ggplot(post_pred_select2, aes(x = x, y = y, colour = as.factor(fact_id))) +
  geom_line(lwd = 0.75) +
  scale_x_continuous(minor_breaks = NULL) +
  scale_y_continuous(minor_breaks = NULL, limits = c(0,5)) +
  geom_segment(data = predictions, aes(x = x, xend = x, y = 4.525 + 1 * (y - mean(y)), yend = 3.9 + 1 * (y - mean(y)), colour = as.factor(fact_id)), arrow = arrow(angle = 20, length = unit(0.1, "inches"), type = "closed")) +
  scale_colour_viridis_d(option = "C", direction = -1, begin = 0.1, end = 0.9) +
  guides(colour = FALSE) +
  labs(x = "Rate of forgetting",
       y = "",
       title = "Learner") +
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())



ggsave("output/plots/post_pred_3_2.pdf", device = "pdf", width = 3, height = 3)

