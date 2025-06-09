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

# Reorder `Variable` to be in the desired order
data$Variable <- factor(data$Variable, levels = c("Elevation", "Water temperature",
                                                  "Forest cover", "Cropland", "Oxygen concentration",
                                                  "Oxygen saturation", "Ground water table", "Slope",
                                                  "Irrigated area", "MAAT", "Reach depth",
                                                  "Stream order", "Stream speed", "Stream speed range",
                                                  "MAP", "Runoff", "MAAT range", "MAP range", "Pasture extent",
                                                  "pH", "Stream gradient", "Population density", 
                                                  "Dam regulation", "Permafrost extent", "Glacier extent"))

# Ensure "Final categories for Stefan" exists
if (!("Final categories for Stefan" %in% names(data))) {
  stop("Column 'Final categories for Stefan' not found in data.")
}

# Define colors for categories
category_colors <- c(
  "Water Quality" = "black", 
  "Geomorphology" = "blue", 
  "Hydrology and Hydraulics" = "darkorange", 
  "Climate" = "purple", 
  "Land use" = "darkgreen"
)  

# Create mapping of facets to categories
strip_categories <- data[!duplicated(data$Variable), c("Variable", "Final categories for Stefan")]

# Map strip colors to variables
strip_fill <- setNames(
  category_colors[strip_categories$`Final categories for Stefan`],  
  strip_categories$Variable
)

# Print for debugging
print(strip_fill)

# Plot with white strip text color
fpi <- ggplot(data = data, aes(x = num_training_samples, y = Importance)) +
  geom_errorbar(aes(ymin = Importance - `std(importance)`, ymax = Importance + `std(importance)`), 
                width = 0.2, color = 'darkgrey') +
  geom_point() +
  theme_bw() +
  labs(x = 'Number of samples in training set', y = 'Feature Permutation Importance (FPI)') +
  theme(strip.text = element_text(size = 10, color = 'white')) +  # Set strip text color to white
  ggh4x::facet_wrap2(
    facets = ~Variable, 
    strip = ggh4x::strip_themed(
      background_x = ggh4x::elem_list_rect(fill = strip_fill[levels(data$Variable)])  
    )
  )

# Save the plot
ggsave(
  './fig-fpi/FPI_Per_Variable_Over_Iterations.pdf',
  fpi,
  device = 'pdf',
  width = 10,
  height = 10,
  units = 'in',
  dpi = 300
)


