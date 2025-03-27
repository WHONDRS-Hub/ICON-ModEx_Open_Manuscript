# ==============================================================================
#
# FPI score for each variable over iterations 
#
# ==============================================================================
#
# Author: Brieanne Forbes
# 20 March 2025
#
# ==============================================================================

library(tidyverse)
library(gsheet)

current_path <- rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
setwd("./..")

# =============================== User inputs ==================================
short_names <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1zHmY46AIOhQ3W-gebzJwmCr4XR4HAZVc8hTZfni6JIk/edit?usp=sharing')

data <- read_csv('./fig-fpi/FPI_summary_table_ALL.csv') %>%
  rename(Feature = 1) %>%
  left_join(short_names, by = c('Feature' = 'Variable')) %>%
  rename(Variable = AM_short_var_names)

# =============================== make figure ==================================

# reorder to be same order as figure 5
data$Variable <- factor(data$Variable, levels = c("Elevation", "Water temperature",
                                                  "Forest cover", "Cropland", "Oxygen concentration",
                                                  "Oxygen saturation", "Ground water table", "Slope",
                                                  "Irrigated area", "MAAT", "Reach depth",
                                                  "Stream order", "Stream speed", "Stream speed range",
                                                  "MAP", "Runoff", "MAAT range", "MAP range", "Pasture extent",
                                                  "pH", "Stream gradient", "Population density", 
                                                  "Dam regulation", "Permafrost extent", "Glacier extent"))

fpi <- ggplot(data = data, aes(x = num_training_samples, y = Importance))+
  geom_errorbar(aes(ymin = Importance - `std(importance)`, ymax = Importance + `std(importance)`), width = 0.2, color = 'darkgrey')+
  geom_point()+
  facet_wrap(~ Variable)+
  theme_bw()+
  labs(x = 'Number of samples in training set', y = 'Feature Permutation Importance (FPI)')+
  theme(strip.text = element_text(size = 10))

ggsave(
  './fig-fpi/FPI_Per_Variable_Over_Iterations.pdf',
  fpi,
  device = 'pdf',
  width = 10,
  height = 10,
  units = 'in',
  dpi = 300
)
