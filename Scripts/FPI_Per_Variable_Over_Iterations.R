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
library(ggh4x)

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


# strip_colors <- c("Water Quality" = "black", 
#                   "Geomorphology" = "blue", 
#                   "Hydrology and Hydraulics" = "darkorange", 
#                   "Climate" = "purple", 
#                   "Land use" = "green")

# Ensure both columns exist in your dataset
if (!("Final categories for Stefan" %in% names(data))) {
  stop("Column 'Final categories for Stefan' not found in data.")
}

# Create a named vector of colors based on unique categories
category_colors <- c(
  "Water Quality" = "black", 
  "Geomorphology" = "blue", 
  "Hydrology and Hydraulics" = "darkorange", 
  "Climate" = "purple", 
  "Land use" = "darkgreen"
)  

# Get unique values of `Final categories for Stefan` corresponding to `Variable`
facet_levels <- unique(data$Variable)
strip_categories <- unique(data[, c("Variable", "Final categories for Stefan")])

# Ensure correct matching between facets and colors
strip_fill <- setNames(
  category_colors[strip_categories$`Final categories for Stefan`],  # Assign colors
  strip_categories$Variable  # Map to facets
)

# Debugging: Print strip_fill to ensure colors are mapped correctly
print(strip_fill)

# Plot
fpi <- ggplot(data = data, aes(x = num_training_samples, y = Importance)) +
  geom_errorbar(aes(ymin = Importance - `std(importance)`, ymax = Importance + `std(importance)`), 
                width = 0.2, color = 'darkgrey') +
  geom_point() +
  theme_bw() +
  labs(x = 'Number of samples in training set', y = 'Feature Permutation Importance (FPI)') +
  theme(strip.text = element_text(size = 10, color = 'white')) +
  ggh4x::facet_wrap2(
    facets = ~Variable, 
    strip = ggh4x::strip_themed(
      background_x = ggh4x::elem_list_rect(fill = strip_fill[facet_levels])  # Correct color mapping
    )
  )

ggsave(
  './fig-fpi/FPI_Per_Variable_Over_Iterations.pdf',
  fpi,
  device = 'pdf',
  width = 10,
  height = 10,
  units = 'in',
  dpi = 300
)
