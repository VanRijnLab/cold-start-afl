## ----------------------------------------------------------------------------
##
## Plot the prediction matrix and map featured on the poster.
##
## Author: Maarten van der Velde
##
## Last updated: 2019-07-12
##
## ----------------------------------------------------------------------------


library(dplyr)
library(forcats)
library(ggplot2)
library(grid)
library(ggrepel)
library(ggforce)
library(wesanderson)
library(cowplot)
library(stringr)
library(tidyr)
library(extrafont)
library(fst)

# font_import() # Run once to populate the R font database with the fonts on the system

loadfonts()

theme_set(theme_light(base_size = 14) +
            theme(text = element_text(family = 'Merriweather Sans'),
                  strip.text = element_text(colour = "black")))



# Get the predictions and response data
predictions_lab_2 <- read.fst(file.path("data", "processed", "lab", "session2", "session2_rl2_predictions_lab.fst"))
responses_lab_2_with_model_params <- read.fst(file.path("data", "processed", "lab", "session2", "session2_rl2_with_alpha_lab.fst"))

# Get the fact IDs from the response data
city_ids <- responses_lab_2_with_model_params %>%
  distinct(fact_id, city = answer)

# Get lat/lng for the cities
load("stimuli/set1.rda")

city_coords <- set1 %>%
  transmute(city = tolower(city), lat = lat, lng = lng) %>%
  right_join(city_ids, by = "city")

# Combine with predictions
pred_lab <- left_join(predictions_lab_2, city_coords, by = "fact_id") 

pred_lab_single <- filter(pred_lab, subject == "04obeojhpmuk1oz")


# Plot prediction matrix, highlighting one participant's predictions

# Get each subject's condition from the response data
conditions <- responses_lab_2_with_model_params %>%
  distinct(subject, condition)


pred_lab_by_cond <- left_join(pred_lab, conditions, by = "subject") %>%
  gather(pred_type, pred_val, fact_pred:default_pred) %>%
  mutate(pred_type = case_when(
    pred_type == "fact_pred" ~ "fact",
    pred_type == "learner_pred" ~ "student",
    pred_type == "fact_and_learner_pred" ~ "fact-and-student",
    pred_type == "domain_pred" ~ "domain",
    pred_type == "default_pred" ~ "default",
    TRUE ~ "??"
  )) %>%
  filter(pred_type == condition)


pred_lab_by_cond <- pred_lab_by_cond %>%
  mutate(condition_pretty = str_replace(condition, "-and-", " & ") %>%
           str_replace("student", "learner") %>%
           str_to_title()) %>%
  mutate(condition_pretty = fct_relevel(condition_pretty, "Fact & Learner", after = Inf)) # Move F&L level to the end


pred_highlight <- filter(pred_lab_by_cond, subject == "mcysfrgyqpwutdx")
rof_pal <- wes_palette("Zissou1", 100, type = "continuous")

ggplot(pred_lab_by_cond, aes(x = subject, y = as.factor(fact_id))) +
  facet_grid(~ condition_pretty, scales = "free_x") +
  geom_tile(aes(fill = pred_val), colour = "white") +
  # geom_raster(data = filter(pred_lab_by_cond, subject == "mcysfrgyqpwutdx")) +
  geom_rect(data = pred_highlight, aes(ymin = 0.5, ymax = 30.5),
            xmin = 16.5, xmax = 17.5,
            fill = NA,
            colour = "black",
            size = 0.5) +
  labs(x = "Participants",
       y = "Facts",
       fill = "Predicted\nrate of forgetting") +
  scale_fill_gradientn(colours = rof_pal, limits = c(0.25, 0.45), values = scales::rescale(c(-0.5, -0.2, 0, 0.1, 0.5))) +
  guides(fill = FALSE) +
  theme(aspect.ratio = 1,
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(size = rel(1.1)),
        panel.border = element_blank(),
        panel.grid = element_blank())

ggsave("output/predictions_overview_no_legend.pdf", device = "pdf", width = 7, height = 2)


# Now plot the predictions for the highlighted participant on the USA map


ggplot(data = pred_highlight, aes(x = lng, y = lat)) + 
  # geom_polygon(data = map_data("state"), aes(x= long, y = lat, group = group), fill = "#b1c8cc", color = "#b1c8cc", size = 30) + # add border
  geom_polygon(data = map_data("state"), aes(x= long, y = lat, group = group), fill = "#8aa1a5", color = "#8aa1a5", size = 30) + # add border
  geom_polygon(data = map_data("state"), aes(x= long, y = lat, group = group), fill = "#f5f5f5", color = "#646464", size = 2) + 
  coord_map("albers", lat0 = 39, lat1 = 45) +
  geom_point(aes(fill = pred_val), size = 10, pch = 21) +
  geom_label_repel(aes(label = str_to_title(city)), nudge_y = -1, min.segment.length = 1.5) +
  scale_colour_gradientn(colours = rof_pal, limits = c(0.25, 0.45), values = scales::rescale(c(-0.5, -0.2, 0, 0.1, 0.5))) +
  scale_fill_gradientn(colours = rof_pal, limits = c(0.25, 0.45), values = scales::rescale(c(-0.5, -0.2, 0, 0.1, 0.5))) +
  guides(colour = FALSE, fill = FALSE) +
  theme_void()
  

ggsave("output/predictions_highlight.pdf", device = "pdf", width = 14, height = 8)

# Plot just the legend separately
p <- ggplot(data = pred_highlight, aes(x = lng, y = lat)) + 
  geom_polygon(data = map_data("state"), aes(x= long, y = lat, group = group), fill = "#b1c8cc", color = "#b1c8cc", size = 30) + # add border
  geom_polygon(data = map_data("state"), aes(x= long, y = lat, group = group), fill = "#f5f5f5", color = "#646464", size = 2) + 
  coord_map("albers", lat0 = 39, lat1 = 45) +
  geom_point(aes(fill = pred_val), size = 10, pch = 21) +
  geom_label_repel(aes(label = str_to_title(city)), nudge_y = -1, min.segment.length = 1.5) +
  scale_colour_gradientn(colours = rof_pal, limits = c(0.25, 0.45), values = scales::rescale(c(-0.5, -0.2, 0, 0.1, 0.5))) +
  scale_fill_gradientn(colours = rof_pal, limits = c(0.25, 0.45), values = scales::rescale(c(-0.5, -0.2, 0, 0.1, 0.5))) +
  labs(fill = "Predicted rate of forgetting") +
  guides(colour = FALSE,
         fill = guide_colourbar(title.position = "top", title.hjust = 0.5)) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.key.width = unit(1, "cm"))

rof_legend <- get_legend(p)

ggsave("output/predictions_legend.pdf", plot = rof_legend, device = "pdf", width = 2.15, height = 1)

