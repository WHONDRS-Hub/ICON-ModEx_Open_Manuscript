# ==============================================================================
#
# Create scatter w density plots for first and last iteration 
#
# Status: complete
#
#
# ==============================================================================
#
# Author: Stefan Gary, Brieanne Forbes 
# 21 Augut 2025
#
# ==============================================================================

library(tidyverse)
library(ggExtra)
library(segmented)
library(cowplot)
library(patchwork)

rm(list=ls(all=T))

current_path <- rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))

# ================================ User Inputs =================================

first <- read_csv("./intermediate_branch_data/Dec-2021a-log10/ICON-ModEx_Combined_Predicted_Observed_Respiration_Rates.csv", na = c(-9999, "N/A", "NA", NA))

last <- read_csv("./intermediate_branch_data/Nov-2023-log10-DO-update-correct/ICON-ModEx_Combined_Predicted_Observed_Respiration_Rates.csv", na = c(-9999, "N/A", "NA", NA))
  
# ================================ Logged  ==============================

## ================ Breakpoint of last iteration ========

lm_all_final_log <- lm(Log_Predicted_Normalized_Respiration_Rate ~ Log_Observed_Normalized_Respiration_Rate, last)
summary(lm_all_final_log)

#breakpoint
breakpoint_log <- segmented(lm_all_final_log)$psi[,2]
bp_uncertainty_log <- segmented(lm_all_final_log)$psi[,3]
segCI_log <- confint(lm_all_final_log,level = 0.95)

last_split_log <- last%>%
  mutate(ds_split = ifelse(Log_Observed_Normalized_Respiration_Rate > breakpoint_log, "Hot", ifelse(Log_Observed_Normalized_Respiration_Rate < breakpoint_log, "Cold", "Cold") ))


## ================== Create scatter ====================


fullscatter_log <- 
ggplot(data = last_split_log, aes(abs(Log_Observed_Normalized_Respiration_Rate ),
                               abs(Log_Predicted_Normalized_Respiration_Rate),
                            color =  ds_split))+ 
  geom_vline (xintercept = breakpoint_log, linetype = "dashed", color="grey90") + 
  geom_hline (yintercept = breakpoint_log, linetype = "dashed", color="grey90") + 
  geom_abline (linetype = "dotted", color="grey90", size = 1) + 
  geom_point(data = first, color = "cyan3", size = 3, pch = 19, alpha = 0.4) + 
  geom_point(data = last_split_log, color = "grey75", size = 3, pch = 21) +
  geom_smooth( data = last_split_log,
               aes(abs(Log_Observed_Normalized_Respiration_Rate ), 
                   abs(Log_Predicted_Normalized_Respiration_Rate),
                   color =  ds_split), method = "lm", alpha = 0.2) + 
  geom_smooth( data = last_split_log,
               aes(abs(Log_Observed_Normalized_Respiration_Rate ), 
                   abs(Log_Predicted_Normalized_Respiration_Rate)), method = "lm", alpha = 0.2,
               color =  "grey39") +
  geom_smooth( data = first,
               aes(abs(Log_Observed_Normalized_Respiration_Rate ), 
                   abs(Log_Predicted_Normalized_Respiration_Rate)), method = "lm", alpha = 0.2,
               color =  "cyan4",
               linetype = 'twodash') +
  xlab(expression(atop("Observed stream O"[2]*" consumption rates", "log10(mg O"[2]*" L"["sediment"]^{-1}*" hour"^{-1}*")"))) +
  ylab(expression(atop("Predicted stream O"[2]*" consumption rates", "log10(mg O"[2]*" L"["sediment"]^{-1}*" hour"^{-1}*")")))+
  scale_shape_manual(values = c(21, 16)) +
  scale_color_manual(values = c( "grey55", "grey55")) +
  lims(y = c(0,4), x = c(0,4)) + 
  theme_classic() +
  theme(text = element_text(size = 16, family = 'sans'), 
        legend.position = 'none')

# Create marginal density plot for x-axis
x_density_log <- ggplot() +
  geom_density(data = first, aes(abs(Log_Observed_Normalized_Respiration_Rate)), 
               fill = "cyan3", alpha = 0.6, color = "cyan3") +
  geom_density(data = last_split_log, aes(abs(Log_Observed_Normalized_Respiration_Rate)), 
               fill = "grey59", alpha = 0.75, color = "grey39") +
  xlim(0, 4) +  # Match your main plot limits
  theme_void()

# Create marginal density plot for y-axis
y_density_log <- ggplot() +
  geom_density(data = first, aes(abs(Log_Predicted_Normalized_Respiration_Rate)), 
               fill = "cyan3", alpha = 0.6, color = "cyan3") +
  geom_density(data = last_split_log, aes(abs(Log_Predicted_Normalized_Respiration_Rate)), 
               fill = "grey59", alpha = 0.75, color = "grey39") +
  xlim(0, 4) +  # Match your main plot limits
  coord_flip() +
  theme_void()

#adding density plots on axes
histograms_log <- x_density_log + plot_spacer() + fullscatter_log + y_density_log + 
  plot_layout(ncol = 2, nrow = 2, widths = c(4, 1), heights = c(1, 4))


ggsave(filename = "./First_Last_Iteration_Scatter_Log.pdf", plot = histograms_log, width = 6, height = 6, dpi = 300)

# ================================ Untransformed ==============================

## ================== Breakpoint of last iteration ============

lm_all_final <- lm(Predicted_Normalized_Respiration_Rate ~ Observed_Normalized_Respiration_Rate, last)
summary(lm_all_final)

#breakpoint
breakpoint <- segmented(lm_all_final)$psi[,2]
bp_uncertainty <- segmented(lm_all_final)$psi[,3]
segCI <- confint(lm_all_final,level = 0.95)

last_split <- last%>%
  mutate(ds_split = ifelse(Observed_Normalized_Respiration_Rate > breakpoint, "Hot", ifelse(Observed_Normalized_Respiration_Rate < breakpoint, "Cold", "Cold") ))



## ===================== Create scatter =======================


fullscatter <- 
  ggplot(data = last_split, aes(abs(Observed_Normalized_Respiration_Rate ),
                                abs(Predicted_Normalized_Respiration_Rate),
                                color =  ds_split))+ 
  geom_vline (xintercept = abs(breakpoint), linetype = "dashed", color="grey90") + 
  geom_hline (yintercept = abs(breakpoint), linetype = "dashed", color="grey90") + 
  geom_abline (linetype = "dotted", color="grey90", size = 1) + 
  geom_point(data = last_split, color = "grey75", size = 3, pch = 21) +
  geom_point(data = first, color = "cyan3", size = 3, pch = 19, alpha = 0.4) + 
  geom_smooth( data = last_split,
               aes(abs(Observed_Normalized_Respiration_Rate ), 
                   abs(Predicted_Normalized_Respiration_Rate),
                   color =  ds_split), method = "lm", alpha = 0.2) + 
  geom_smooth( data = last_split,
               aes(abs(Observed_Normalized_Respiration_Rate ), 
                   abs(Predicted_Normalized_Respiration_Rate)), method = "lm", alpha = 0.2,
               color =  "grey39") +
  geom_smooth( data = first,
               aes(abs(Observed_Normalized_Respiration_Rate ), 
                   abs(Predicted_Normalized_Respiration_Rate)), method = "lm", alpha = 0.2,
               color =  "cyan4",
               linetype = 'twodash') +
  xlab(expression(atop("Observed stream O"[2]*" consumption rates", "(mg O"[2]*" L"["sediment"]^{-1}*" hour"^{-1}*")"))) +
  ylab(expression(atop("Predicted stream O"[2]*" consumption rates", "(mg O"[2]*" L"["sediment"]^{-1}*" hour"^{-1}*")")))+
  scale_shape_manual(values = c(21, 16)) +
  scale_color_manual(values = c( "grey55", "grey55")) +
  lims(y = c(0,4000), x = c(0,4000)) +
  theme_classic() +
  theme(text = element_text(size = 16, family = 'sans'), 
        legend.position = 'none')

# Create marginal density plot for x-axis
x_density <- ggplot() +
  geom_density(data = first, aes(abs(Observed_Normalized_Respiration_Rate)), 
               fill = "cyan3", alpha = 0.6, color = "cyan3") +
  geom_density(data = last_split, aes(abs(Observed_Normalized_Respiration_Rate)), 
               fill = "grey59", alpha = 0.75, color = "grey39") +
  xlim(0, 4000) +  # Match your main plot limits
  theme_void()

# Create marginal density plot for y-axis
y_density <- ggplot() +
  geom_density(data = first, aes(abs(Predicted_Normalized_Respiration_Rate)), 
               fill = "cyan3", alpha = 0.6, color = "cyan3") +
  geom_density(data = last_split, aes(abs(Predicted_Normalized_Respiration_Rate)), 
               fill = "grey59", alpha = 0.75, color = "grey39") +
  xlim(0, 4000) +  # Match your main plot limits
  coord_flip() +
  theme_void()

#adding density plots on axes
histograms <- x_density + plot_spacer() + fullscatter + y_density + 
  plot_layout(ncol = 2, nrow = 2, widths = c(4, 1), heights = c(1, 4))


ggsave(filename = "./First_Last_Iteration_Scatter.pdf", plot = histograms, width = 6, height = 6, dpi = 300)
