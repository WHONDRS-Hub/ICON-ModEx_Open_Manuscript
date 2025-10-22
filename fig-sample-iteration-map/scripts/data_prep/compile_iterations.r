# Description: This script compiles predicted respiration rates and errors
#              across all sampling iterations into a single data frame for analysis and plotting.
#              This scripts also prepares the sampling points as points objects and 
#              reads in background data for mapping.



# /----------------------------------------------------------------------------#
#/  Import libraries
library(here); setwd(here()) # setwd to the location of the project
library(tidyverse)
library(sf)
library(dplyr)
library(zoo) # Load the zoo package for handling year-month format
library(ggnewscale)
library(colorspace)
library(RColorBrewer)
library(scales)
library(cowplot)
sf_use_s2(F)


# /----------------------------------------------------------------------------#
#/  List all folders in the directory;  a file for each iteration?       -------

# Set the directory path containing the prediction from individual iterations 
base_directory <- "../../fig-model-score-evolution/intermediate_branch_data"

# Find all files named ICON-ModEx_Combined_Predicted_Observed_Respiration_Rates.csv recursively
all_files <- list.files(
  path = base_directory, 
  pattern = "ICON-ModEx_Combined_Predicted_Observed_Respiration_Rates.csv", 
  recursive = TRUE, 
  full.names = TRUE)


# /----------------------------------------------------------------------------#
#/  Filter to only log10 files                                          --------

# Exclude certain directories
exclude_dirs <- c("pca_hp_100", "pca_hp_200", "pca_hp_300", 'pca_lp_100', 'pca_lp_200', 'pca_lp_300')

# Make list of files to concatenate
files_to_keep <- all_files[sapply(all_files, function(f) {
  # Check if the file is not in excluded directories and contains 'log10'
  !any(sapply(exclude_dirs, function(exclude_dir) {
    grepl(paste0("/", exclude_dir, "/"), f)
  })) && grepl("log10", f)
})]


# /----------------------------------------------------------------------------#
#/   Combine iteration files                                            --------

# Initialize an empty data frame to append the data
combined_df <- data.frame()

# Loop through the files and append their contents to the combined data frame
for (file in files_to_keep) {
  # Read the CSV file into a data frame
  temp_df <- read.csv(file)
  
  # Extract the subdirectory name
  path_parts <- strsplit(dirname(file), split = "/")[[1]]
  subdir_name <- path_parts[length(path_parts)]
  
  # Remove '-log10' from the subdirectory name
  subdir_name_clean <- sub("-log10", "", subdir_name)
  # print(subdir_name_clean)
  
  # Exclude the duplicated Nov 2023, in favor of keeping the DO update corrected
  if(subdir_name_clean %in% c('Nov-2023')) next
    
  
  if(subdir_name_clean %in% c('Dec-2021a'))  subdir_name_clean <- 'Sep-2019'
  
  # Convert the cleaned subdirectory name to a yearmon object
  date_format <- try(as.yearmon(subdir_name_clean, format = "%b-%Y"), silent = TRUE)
  print(date_format)
  
  # Check if the conversion to date was successful; otherwise, treat as NA
  if (inherits(date_format, "try-error")) {
    date_format <- NA
  }
  
  # Add a new column with the date
  temp_df$subdir_date <- subdir_name_clean
  temp_df$Date <- as.character(date_format)
  
  # Append the data to the combined data frame
  combined_df <- rbind(combined_df, temp_df)
  
}



# /----------------------------------------------------------------------------#
#/  Prep iteration number                                               --------

# Convert to year-month format
combined_df$Date_label <- combined_df$Date
combined_df$Date <- as.Date(as.yearmon(combined_df$Date), frac = 0)

# Sort the combined_df by the Date column
combined_df <- combined_df[order(combined_df$Date), ]

# Assign a numeric value for each unique date
combined_df$iteration_num <- as.numeric(factor(combined_df$Date))



# /----------------------------------------------------------------------------#
#/  Compute error between pred & obs                                    --------

combined_df <-
  combined_df %>% 
  mutate(Predicted_Normalized_Respiration_Rate = Predicted_Normalized_Respiration_Rate * -1, 
         Observed_Normalized_Respiration_Rate = Observed_Normalized_Respiration_Rate * -1) %>% 
  mutate(Normalized_Respiration_Rate_abserror = Predicted_Normalized_Respiration_Rate - Observed_Normalized_Respiration_Rate)


# Calculate difference
# combined_df['Normalized_Respiration_Rate_abserror'] <- abs(combined_df['Predicted_Normalized_Respiration_Rate'] - combined_df['Observed_Normalized_Respiration_Rate'])


# /----------------------------------------------------------------------------#
#/  Get first iteration of each point                                   --------

# Group by Sample_ID and filter rows with the lowest iteration_num in each group
first_iter_df <- combined_df %>%
  group_by(Sample_ID) %>%
  filter(iteration_num == min(iteration_num, na.rm=T)) %>%
  ungroup() %>% 
  rename(Normalized_Respiration_Rate_abserror_firstiter = Normalized_Respiration_Rate_abserror,
         Predicted_Normalized_Respiration_Rate_firstiter = Predicted_Normalized_Respiration_Rate)
         


# /----------------------------------------------------------------------------#
#/  Get last iteration of each point                                      ------

# Assuming combined_df has columns named Sample_ID and iteration_num

# Group by Sample_ID and filter rows with the lowest iteration_num in each group
last_iter_df <- combined_df %>%
  group_by(Sample_ID) %>%
  filter(iteration_num == max(iteration_num)) %>%
  ungroup() %>% 
  rename(Normalized_Respiration_Rate_abserror_lastiter = Normalized_Respiration_Rate_abserror,
         Predicted_Normalized_Respiration_Rate_lastiter = Predicted_Normalized_Respiration_Rate) %>% 
  dplyr::select(Sample_ID, Predicted_Normalized_Respiration_Rate_lastiter, Normalized_Respiration_Rate_abserror_lastiter)  



# /----------------------------------------------------------------------------#
### CALCULATE DIFFERENCE 
# Perform a left join using dplyr's left_join function
# Note:  The first-to-last change in rates will be the same as change in errors too. 

whndrs_diff_df <- 
  left_join(last_iter_df, first_iter_df, by = "Sample_ID") %>% 
  mutate(
    Predicted_Normalized_Respiration_Rate_lastminusfirst = Predicted_Normalized_Respiration_Rate_lastiter -  Predicted_Normalized_Respiration_Rate_firstiter,
    Normalized_Respiration_Rate_abserror_lastminusfirst  =  Normalized_Respiration_Rate_abserror_lastiter -  Normalized_Respiration_Rate_abserror_firstiter)  %>% 
  
  # Flip sign of the errors 
  # mutate(Normalized_Respiration_Rate_abserror_lastminusfirst = Normalized_Respiration_Rate_abserror_lastminusfirst * -1 ) %>%


  select(Sample_ID, 
         Sample_Longitude_obs, 
         Sample_Latitude_obs, 
         Predicted_Normalized_Respiration_Rate_lastiter,
         Predicted_Normalized_Respiration_Rate_firstiter,
         Predicted_Normalized_Respiration_Rate_lastminusfirst,
         
         Normalized_Respiration_Rate_abserror_lastiter,
         Normalized_Respiration_Rate_abserror_firstiter,
         Normalized_Respiration_Rate_abserror_lastminusfirst)



# /----------------------------------------------------------------------------#
#/   Get point locations of GloRICH predictions                           ------

# The `output_all_sites_*` files have both the WHONDRS data and the GLORICH data. 
# NOTE:  NOPE - not any more


# Get Glorich predictions for first iteration (Dec 2021a, b or later)
glorich_firstiter <- 
  read_csv('../../fig-model-score-evolution/intermediate_branch_data/Dec-2021a-log10/output_all_sites_avgpre_stdpre_merged_filtered.csv') %>%
  # read_csv('../../fig-model-score-evolution/intermediate_branch_data/Dec-2021b-log10/output_all_sites_avgpre_stdpre_merged_filtered.csv') %>%
  filter(!is.na(Sample_ID)) %>% 
  mutate(Sample_ID = as.numeric(Sample_ID)) %>% 
  select(Sample_ID, Normalized_Respiration_Rate_mg_DO_per_H_per_L_sediment_pre_avg)


names(glorich_firstiter) <- c('Sample_ID', 'Normalized_Respiration_Rate_mg_DO_per_H_per_L_sediment_pre_avg_first')




# Get Glorich predictions for Nov 2023
glorich_lastiter <- 
  read_csv('../../fig-model-score-evolution/intermediate_branch_data/Nov-2023-log10-DO-update-correct/output_all_sites_avgpre_stdpre_merged_filtered.csv') %>% 
  filter(!is.na(Sample_ID)) %>% 
  mutate(Sample_ID = as.numeric(Sample_ID)) %>% 
  select(Sample_ID, Sample_Latitude_pre_avg, Sample_Longitude_pre_avg, Normalized_Respiration_Rate_mg_DO_per_H_per_L_sediment_pre_avg)

names(glorich_lastiter) <- c('Sample_ID', 'Latitude', 'Longitude','Normalized_Respiration_Rate_mg_DO_per_H_per_L_sediment_pre_avg_last')



# Join across iterations
glorich_preds <- 
  full_join(glorich_lastiter, glorich_firstiter, by='Sample_ID') %>% 
  mutate(Normalized_Respiration_Rate_mg_DO_per_H_per_L_sediment_pre_avg_first = Normalized_Respiration_Rate_mg_DO_per_H_per_L_sediment_pre_avg_first * -1,
         Normalized_Respiration_Rate_mg_DO_per_H_per_L_sediment_pre_avg_last = Normalized_Respiration_Rate_mg_DO_per_H_per_L_sediment_pre_avg_last * -1)





# You can filter out the WHONDRS data by selecting all sites with ID's prefixed 
# with `SSS`, `CM_`, and `S19S` which correspond to the three major phases of the collection of respiration rate data.

glorich_preds <- glorich_preds %>%
  # read.csv('https://raw.githubusercontent.com/WHONDRS-Hub/ICON-ModEx_Open_Manuscript/refs/heads/main/fig-model-score-evolution/intermediate_branch_data/Nov-2023-log10-DO-update-correct/output_all_sites_avgpre_stdpre_merged_filtered.csv') %>%
  # mutate(Predicted_Normalized_Respiration_Rate_lastminusfirst = Predicted_Normalized_Respiration_Rate_mg_DO_per_H_per_L_sediment_Nov2023 - Predicted_Normalized_Respiration_Rate_mg_DO_per_H_per_L_sediment_Sep2019) %>% 
  mutate(Normalized_Respiration_Rate_lastminusfirst = Normalized_Respiration_Rate_mg_DO_per_H_per_L_sediment_pre_avg_last - Normalized_Respiration_Rate_mg_DO_per_H_per_L_sediment_pre_avg_first) %>% 
  mutate(sample_type = case_when(str_detect(Sample_ID, "SSS") ~ "WHONDRS",
                                 str_detect(Sample_ID, "CM_") ~ "WHONDRS",
                                 str_detect(Sample_ID, "S19S") ~ "WHONDRS")) %>% 
  mutate(sample_type = ifelse(is.na(sample_type), "GLORICH", sample_type)) %>% 
  
  dplyr::select(Sample_ID, sample_type, Latitude, Longitude, 
                Normalized_Respiration_Rate_mg_DO_per_H_per_L_sediment_pre_avg_last,
                Normalized_Respiration_Rate_mg_DO_per_H_per_L_sediment_pre_avg_first,
                Normalized_Respiration_Rate_lastminusfirst) 
  
  # Flip the rates
  # mutate(Normalized_Respiration_Rate_lastminusfirst = Normalized_Respiration_Rate_lastminusfirst * -1 )


# /----------------------------------------------------------------------------#
#/    Get CONUS state polygons                                            ------

library(rnaturalearth)

# Select USA polygon
conus <- ne_states(country = "united states of america")

# Keep only CONUS
conus <- conus %>% filter(!name %in% c('Alaska','Hawaii'))


# /----------------------------------------------------------------------------#
#/    Get map theme                                                       ------

# Load custom theme for figure
source('set_up/themes.r')


# /----------------------------------------------------------------------------#
#/   Convert to points

first_iter_df_points <- st_as_sf(
  first_iter_df,
  coords = c("Sample_Longitude_obs", "Sample_Latitude_obs"),
  crs = 4326) %>% 
  st_filter(., conus, .pred = st_intersects)


whndrs_diff_df_points <- st_as_sf(
  whndrs_diff_df,
  coords = c("Sample_Longitude_obs", "Sample_Latitude_obs"),
  crs = 4326) %>% 
  st_filter(., conus, .pred = st_intersects)


glorich_preds_points <- st_as_sf(
  glorich_preds,
  coords = c("Longitude", "Latitude"),
  crs = 4326) %>% 
  st_filter(., conus, .pred = st_intersects)



# /----------------------------------------------------------------------------#
#/  Get sampling dates

sample_dates <- 
  read.csv('https://raw.githubusercontent.com/parallelworks/dynamic-learning-rivers/refs/heads/main/input_data/ICON-ModEx_Data.csv') %>%
  mutate(Date = parse_date_time(Date, c('ymd', 'mdy'))) %>% 
  mutate(month = month(Date)) %>% 
  mutate(month_abb = month.abb[month]) %>% 
  filter(!is.na(Sample_Longitude) & !is.na(Sample_Longitude)) %>% 
  st_as_sf(., coords = c("Sample_Longitude", "Sample_Latitude"), crs = st_crs(conus)) %>% 
  st_filter(., conus, .pred = st_intersects) %>% 
  dplyr::select(Sample_Kit_ID, Sample_ID, Date, month, month_abb)



# /----------------------------------------------------------------------------#
#/    Color ramps for selected colors, positive and negative

library(colorspace)

sel_col <- rev(sequential_hcl(11, palette = "Purple-Yellow"))#[2:6]
pos_col <- rev(sequential_hcl(6, palette = 'Blues 3'))[2:6]
neg_col <- rev(sequential_hcl(6, palette = 'Reds 3'))[2:6]


# /----------------------------------------------------------------------------#
#/    Reproject to EPSG:6350: NAD83(2011) / Conus Albers                --------

conus <- st_transform(conus, crs = 6350)
whndrs_diff_df_points <- st_transform(whndrs_diff_df_points, crs = 6350)
# filtered_df_points <- st_transform(filtered_df_points, crs = 6350)
glorich_preds_points <- st_transform(glorich_preds_points, crs = 6350)
first_iter_df_points <- st_transform(first_iter_df_points, crs = 6350)
first_iter_df_points <- st_transform(first_iter_df_points, crs = 6350)
sample_dates <- st_transform(sample_dates, crs = 6350)


# /----------------------------------------------------------------------------#
#/    Add columns for coordinates               --------

whndrs_diff_df_points <- bind_cols(whndrs_diff_df_points, as.data.frame(st_coordinates(whndrs_diff_df_points)))

glorich_preds_points <- bind_cols(glorich_preds_points, as.data.frame(st_coordinates(glorich_preds_points)))


