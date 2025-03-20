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

current_path <- rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
setwd("./..")

# =============================== User inputs ==================================

data <- read_csv('./fig-fpi/FPI_summary_table_ALL.csv') %>%
  rename(Feature = 1)

# =============================== make figure ==================================

order <- data %>%
  filter(num_loop == 18) %>%
  arrange(desc(Importance)) %>%
  pull(Feature)

data$Feature <- factor(data$Feature, levels = order)

fpi <- ggplot(data = data, aes(x = num_training_samples, y = Importance))+
  geom_point()+
  geom_errorbar(aes(ymin = Importance - `std(importance)`, ymax = Importance + `std(importance)`), width = 0.2)+
  facet_wrap(~ Feature)+
  theme_bw()+
  labs(x = 'Number of samples in training set', y = 'Feature')

ggsave(
  './fig-fpi/FPI_Per_Variable_Over_Iterations.pdf',
  fpi,
  device = 'pdf',
  width = 10,
  height = 10,
  units = 'in',
  dpi = 300
)
