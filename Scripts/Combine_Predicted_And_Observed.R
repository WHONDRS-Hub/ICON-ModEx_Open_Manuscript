# ==============================================================================
#
# Combine ICON-ModEx observed and predicted rates
#
# Status: Complete
#
# ==============================================================================
#
# Author: Brieanne Forbes
# 18 April 2024
#
# ==============================================================================

library(tidyverse)
library(crayon)

rm(list=ls(all=T))

# ================================= User inputs ================================

current_path <- rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
setwd("./../")

# File path to the dynamic learning rivers github repo
# https://github.com/parallelworks/dynamic-learning-rivers/tree/Nov-2023-log10
# Ensure you are on the correct branch. For this script, 
dynamic_learning_dir <- './../dynamic-learning-rivers/'

# ==========================  find and read data files =========================

# Data downloaded from https://data.ess-dive.lbl.gov/datasets/doi:10.15485/1923689 
observed_CM <- read_csv('./v3_CM_SSS_Data_Package/Sample_Data/v2_CM_SSS_Sediment_Normalized_Respiration_Rates.csv', 
                            skip = 2,
                            na = c('N/A', '-9999', NA, '')) %>%
  filter(!is.na(Sample_Name)) %>%
  select(-Field_Name, -Material, -IGSN) %>% 
  rename(Observed_Normalized_Respiration_Rate_mg_DO_per_H_per_L_sediment = Normalized_Respiration_Rate_mg_DO_per_H_per_L_sediment) %>%
  mutate(Parent_ID = str_extract(Sample_Name, '.{6}(?=_INC)'))

# Data downloaded from https://data.ess-dive.lbl.gov/datasets/doi:10.15485/1729719
# add deviations to S19S to match CM; pulled from "WHONDRS_S19S_Sediment_Normalized_Respiration_Methods.txt"
dev1 <- c("S19S_0006_SED_INC-D", "S19S_0011_SED_INC-M", "S19S_0013_SED_INC-D", "S19S_0013_SED_INC-M", "S19S_0013_SED_INC-U", "S19S_0014_SED_INC-D", "S19S_0014_SED_INC-M", "S19S_0016_SED_INC-M", "S19S_0020_SED_INC-M", "S19S_0021_SED_INC-D", "S19S_0024_SED_INC-D", "S19S_0024_SED_INC-M", "S19S_0024_SED_INC-U", "S19S_0039_SED_INC-D", "S19S_0039_SED_INC-U", "S19S_0040_SED_INC-U", "S19S_0044_SED_INC-D", "S19S_0044_SED_INC-M", "S19S_0044_SED_INC-U", "S19S_0047_SED_INC-M", "S19S_0047_SED_INC-U", "S19S_0054_SED_INC-D", "S19S_0054_SED_INC-M", "S19S_0054_SED_INC-U", "S19S_0055_SED_INC-D", "S19S_0059_SED_INC-D", "S19S_0061_SED_INC-D", "S19S_0061_SED_INC-M", "S19S_0066_SED_INC-M", "S19S_0066_SED_INC-U", "S19S_0067_SED_INC-D", "S19S_0067_SED_INC-M", "S19S_0067_SED_INC-U", "S19S_0087_SED_INC-D", "S19S_0089_SED_INC-D", "S19S_0089_SED_INC-M", "S19S_0097_SED_INC-U")
dev2 <- c("S19S_0006_SED_INC-D", "S19S_0006_SED_INC-M", "S19S_0006_SED_INC-U", "S19S_0007_SED_INC-D", "S19S_0007_SED_INC-U", "S19S_0008_SED_INC-D", "S19S_0008_SED_INC-M", "S19S_0008_SED_INC-U", "S19S_0009_SED_INC-D", "S19S_0009_SED_INC-M", "S19S_0009_SED_INC-U", "S19S_0010_SED_INC-D", "S19S_0010_SED_INC-M", "S19S_0010_SED_INC-U", "S19S_0036_SED_INC-M")

observed_S19S <- read_csv('./v8_WHONDRS_S19S_Sediment/WHONDRS_S19S_Sediment_Normalized_Respiration_Rates.csv') %>%
  rename(Sample_Name = Sample_ID) %>%
  mutate(Methods_Deviation1 = case_when(Sample_Name %in% dev1 ~ 'RATE_000',
                                       TRUE ~ NA),
         Methods_Deviation2 = case_when(Sample_Name %in% dev2 ~ 'INC_QA_002',
                                        TRUE ~ NA)) %>%
  unite(Methods_Deviation,
            c(Methods_Deviation1, Methods_Deviation2),
            sep = '; ',
            na.rm = T)%>% 
  rename(Observed_Normalized_Respiration_Rate_mg_DO_per_H_per_L_sediment = Normalized_Respiration_Rate_mg_DO_per_H_per_L_sediment) 

cat(red('MAKE SURE YOU ARE ON THE CORRECT NOV-2023 BRANCH\n'))

# Ensure you are on the correct branch. For this script, I am using the Nov-2023-log10-DO-update-correct branch
predicted_Nov2023 <- read_csv(str_c(dynamic_learning_dir, '/scripts/post_01_output_ml_predict_avg.csv')) %>%
  rename(Predicted_Normalized_Respiration_Rate_mg_DO_per_H_per_L_sediment_Nov2023 = Normalized_Respiration_Rate_mg_DO_per_H_per_L_sediment,
         Predicted_Latitude = Sample_Latitude,
         Predicted_Longitude = Sample_Longitude,
         Sample_Name = Sample_ID,
         mean.error_Nov2023 = mean.error,
         predict.error_Nov2023 = predict.error,
         pca.dist_Nov2023 = pca.dist,
         mean.error.scaled_Nov2023 = mean.error.scaled,
         pca.dist.scaled_Nov2023 = pca.dist.scaled,
         combined.metric_Nov2023 = combined.metric)

cat(red('SWITCH TO THE CORRECT JUL-2022 BRANCH\n'))

predicted_Jul2022 <- read_csv(str_c(dynamic_learning_dir, '/scripts/post_01_output_ml_predict_avg.csv')) %>%
  rename(Predicted_Normalized_Respiration_Rate_mg_DO_per_H_per_L_sediment_Jul2022 = Normalized_Respiration_Rate_mg_DO_per_H_per_L_sediment,
         Predicted_Latitude = Sample_Latitude,
         Predicted_Longitude = Sample_Longitude,
         Sample_Name = Sample_ID,
         mean.error_Jul2022 = mean.error,
         predict.error_Jul2022 = predict.error,
         pca.dist_Jul2022 = pca.dist,
         mean.error.scaled_Jul2022 = mean.error.scaled,
         pca.dist.scaled_Jul2022 = pca.dist.scaled,
         combined.metric_Jul2022 = combined.metric)

predicted <- predicted_Nov2023 %>%
  full_join(predicted_Jul2022)

# ========  find and read metadata files and combine with observed data ========

CM_metadata <- read_csv('./v3_CM_SSS_Data_Package/v3_CM_SSS_Field_Metadata.csv')

CM_coords <- CM_metadata %>%
  select(Parent_ID, Sample_Latitude, Sample_Longitude, Site_ID, Sample_Date) %>%
  rename(Observed_Latitude = Sample_Latitude,
         Observed_Longitude = Sample_Longitude,
         Observed_Sample_Date = Sample_Date)

S19S_metadata <- read_csv('./v8_WHONDRS_S19S_Sediment/WHONDRS_S19S_Sediment_Metadata/v4_WHONDRS_S19S_Metadata.csv') %>%
  filter(Study_Code != 'Study code')

# pivot the upstream, downstream, and midstream coordinates to each be a row
u <- S19S_metadata %>%
  select(Sample_ID,  US_Latitude_dec.deg, US_Longitude_dec.deg, Date) %>%
  rename(Observed_Latitude = US_Latitude_dec.deg, 
         Observed_Longitude = US_Longitude_dec.deg) %>%
  mutate(Sample_Name = paste0(Sample_ID, "_SED_INC-U"))

m <- S19S_metadata %>%
  select(Sample_ID,  MS_Latitude_dec.deg, MS_Longitude_dec.deg, Date) %>%
  rename(Observed_Latitude = MS_Latitude_dec.deg, 
         Observed_Longitude = MS_Longitude_dec.deg) %>%
  mutate(Sample_Name = paste0(Sample_ID, "_SED_INC-M"))

d <- S19S_metadata %>%
  select(Sample_ID,  DS_Latitude_dec.deg, DS_Longitude_dec.deg, Date) %>%
  rename(Observed_Latitude = DS_Latitude_dec.deg, 
         Observed_Longitude = DS_Longitude_dec.deg) %>%
  mutate(Sample_Name = paste0(Sample_ID, "_SED_INC-D"))

S19S_coords <- u %>%
  add_row(m) %>%
  add_row(d) %>%
  arrange(Sample_ID) %>%
  rename(Site_ID = Sample_ID,
         Observed_Sample_Date = Date)

observed_CM_combine <- observed_CM %>%
  full_join(CM_coords) %>%
  select(-Parent_ID) %>%
  mutate(Observed_Normalized_Respiration_Rate_mg_DO_per_H_per_L_sediment = as.numeric(Observed_Normalized_Respiration_Rate_mg_DO_per_H_per_L_sediment))

observed_S19S_combine <- observed_S19S %>%
  left_join(S19S_coords) %>%
  mutate(Observed_Latitude = as.numeric(Observed_Latitude),
         Observed_Longitude = as.numeric(Observed_Longitude),
         Observed_Sample_Date = ymd(Observed_Sample_Date))

full_observed <- observed_CM_combine %>%
  add_row(observed_S19S_combine) %>%
  mutate(Sample_Name = str_remove(Sample_Name, '_INC'),
         Sample_Name = str_remove(Sample_Name, '_SED'))

# ==================== filter and format predicted and observed ================

predicted_filter <- predicted %>%
  filter(str_detect(Sample_Name, '^10|^CM|^S19S|^SSS|^SP')) %>%
  mutate(Sample_Name = case_when(str_detect(Sample_Name, '^10') ~ str_c('MP', Sample_Name, sep = '-'),
                             TRUE ~ Sample_Name))

# get list of observed sites to remove duplicated predictions
observed_site_list <- full_observed %>%
  filter(str_detect(Site_ID, '^MP-|^SP-'),
         !is.na(Observed_Normalized_Respiration_Rate_mg_DO_per_H_per_L_sediment)) %>%
  select(Site_ID) %>%
  pull()

predicted_filter <- predicted_filter %>%
  filter(!Sample_Name %in% observed_site_list)

# ====================== fill in prediction for S19S_0085 ======================
# Prediction is missing from final output because it does not have a pH value and
# is excluded from the PCA, therefore there is no associated error
# To pull the prediction, we have to get the average of the 10 ML model predictions

cat(red('MAKE SURE YOU ARE ON THE CORRECT NOV-2023 BRANCH\n'))

prediction_files_Nov2023 <- list.files(str_c(dynamic_learning_dir, '/ml_models/'), 'sl_predictions.csv',full.names = T, recursive = T)

combine_Nov2023 <- tibble("Sample_ID" = as.character(),
                  "Sample_Longitude" = as.numeric(),
                  "Sample_Latitude" = as.numeric(),
                  "Normalized_Respiration_Rate_mg_DO_per_H_per_L_sediment" = as.numeric())

for (prediction_file_Nov2023 in prediction_files_Nov2023) {
  
  read_file_Nov2023 <- read_csv(prediction_file_Nov2023) %>%
    filter(str_detect(Sample_ID, 'S19S_0085')) %>%
    select(-contains('error'))
  
  combine_Nov2023 <- combine_Nov2023 %>%
    add_row(read_file_Nov2023)
  
}

#average the model predictions
summary_Nov2023 <- combine_Nov2023 %>%
  group_by(Sample_ID, Sample_Longitude, Sample_Latitude) %>%
  summarise(Predicted_Normalized_Respiration_Rate_mg_DO_per_H_per_L_sediment_Nov2023 = mean(Normalized_Respiration_Rate_mg_DO_per_H_per_L_sediment)) %>%
  rename(Sample_Name = Sample_ID,
         Predicted_Longitude = Sample_Longitude,
         Predicted_Latitude = Sample_Latitude) %>%
  add_column("mean.error_Nov2023" = NA,
             "predict.error_Nov2023" = NA,
             "pca.dist_Nov2023" = NA,
             "mean.error.scaled_Nov2023" = NA,
             "pca.dist.scaled_Nov2023" = NA,
             "combined.metric_Nov2023" = NA)


## repeat process for Jul2022 iteration
cat(red('SWITCH TO THE CORRECT JUL-2022 BRANCH\n'))

prediction_files_Jul2022 <- list.files(str_c(dynamic_learning_dir, '/ml_models/'), 'sl_predictions.csv',full.names = T, recursive = T)

combine_Jul2022 <- tibble("Sample_ID" = as.character(),
                          "Sample_Longitude" = as.numeric(),
                          "Sample_Latitude" = as.numeric(),
                          "Normalized_Respiration_Rate_mg_DO_per_H_per_L_sediment" = as.numeric())

for (prediction_file_Jul2022 in prediction_files_Jul2022) {
  
  read_file_Jul2022 <- read_csv(prediction_file_Jul2022) %>%
    filter(str_detect(Sample_ID, 'S19S_0085')) %>%
    select(-contains('error'))
  
  combine_Jul2022 <- combine_Jul2022 %>%
    add_row(read_file_Jul2022)
  
}

#average the model predictions
summary_Jul2022 <- combine_Jul2022 %>%
  group_by(Sample_ID, Sample_Longitude, Sample_Latitude) %>%
  summarise(Predicted_Normalized_Respiration_Rate_mg_DO_per_H_per_L_sediment_Jul2022 = mean(Normalized_Respiration_Rate_mg_DO_per_H_per_L_sediment)) %>%
  rename(Sample_Name = Sample_ID,
         Predicted_Longitude = Sample_Longitude,
         Predicted_Latitude = Sample_Latitude) %>%
  add_column("mean.error_Jul2022" = NA,
             "predict.error_Jul2022" = NA,
             "pca.dist_Jul2022" = NA,
             "mean.error.scaled_Jul2022" = NA,
             "pca.dist.scaled_Jul2022" = NA,
             "combined.metric_Jul2022" = NA)

summary <- summary_Nov2023 %>%
  full_join(summary_Jul2022)

#put results into predicted file
predicted_filter <- predicted_filter %>%
  add_row(summary)


# ====================== combine predicted and observed ========================

combine_predicted_observed <- predicted_filter %>%
  full_join(full_observed, by = 'Sample_Name') %>%
  mutate(Latitude = case_when(!is.na(Observed_Normalized_Respiration_Rate_mg_DO_per_H_per_L_sediment) ~ Observed_Latitude, 
                              TRUE ~ Predicted_Latitude),
         Longitude = case_when(!is.na(Observed_Normalized_Respiration_Rate_mg_DO_per_H_per_L_sediment) ~ Observed_Longitude, 
                              TRUE ~ Predicted_Longitude),
         Log_Observed_Normalized_Respiration_Rate = log10(Observed_Normalized_Respiration_Rate_mg_DO_per_H_per_L_sediment * -1),
         Log_Predicted_Normalized_Respiration_Rate_Nov2023 = log10(Predicted_Normalized_Respiration_Rate_mg_DO_per_H_per_L_sediment_Nov2023 * -1),
         Log_Predicted_Normalized_Respiration_Rate_Jul2022 = log10(Predicted_Normalized_Respiration_Rate_mg_DO_per_H_per_L_sediment_Jul2022 * -1),
         Site_ID = case_when(str_detect(Sample_Name, '^MP-|^SP-') ~ Sample_Name,
                             TRUE ~ Site_ID)) %>%
  select(-Observed_Latitude, -Observed_Longitude, -Predicted_Latitude, -Predicted_Longitude)%>%
  mutate(Sample_Name = case_when(is.na(Sample_Name) ~ 'N/A',
                                 TRUE ~ Sample_Name),
         Methods_Deviation = case_when(is.na(Methods_Deviation) | Methods_Deviation == '' ~ 'N/A',
                                 TRUE ~ Methods_Deviation),
         Site_ID = case_when(is.na(Site_ID) ~ 'N/A',
                                 TRUE ~ Site_ID),
         Observed_Sample_Date = as.character(str_c(' ', ymd(Observed_Sample_Date))),
         Observed_Sample_Date = case_when(is.na(Observed_Sample_Date) ~ '-9999',
                                          TRUE ~ Observed_Sample_Date),
         Raw_Error_Nov2023 = (Predicted_Normalized_Respiration_Rate_mg_DO_per_H_per_L_sediment_Nov2023 - Observed_Normalized_Respiration_Rate_mg_DO_per_H_per_L_sediment)/Predicted_Normalized_Respiration_Rate_mg_DO_per_H_per_L_sediment_Nov2023,
         Raw_Error_Jul2022 = (Predicted_Normalized_Respiration_Rate_mg_DO_per_H_per_L_sediment_Jul2022 - Observed_Normalized_Respiration_Rate_mg_DO_per_H_per_L_sediment)/Predicted_Normalized_Respiration_Rate_mg_DO_per_H_per_L_sediment_Jul2022,
         Log_mean.error_Nov2023 = log10(mean.error_Nov2023),
         Log_mean.error_Jul2022 = log10(mean.error_Jul2022)) %>%
  mutate_all(~replace_na(., -9999))



# ================================== write file ================================

write_csv(combine_predicted_observed, './ICON-ModEx_Combined_Predicted_Observed_Respiration_Rates.csv')

