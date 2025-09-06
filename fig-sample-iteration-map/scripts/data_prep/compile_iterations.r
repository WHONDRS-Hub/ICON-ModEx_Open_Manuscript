# Description: This script compiles predicted respiration rates and errors
#              across all sampling iterations into a single dataframe for analysis and plotting.
#              This scripts also prepares the sampling points as points objects and 
#              reads in geospatial polyonsmapping d

# Import libraries
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

# Set your base directory path here
base_directory <- "../ICON-ModEx_Open_Manuscript/fig-model-score-evolution/intermediate_branch_data"

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
    
  
  # Convert the cleaned subdirectory name to a yearmon object
  date_format <- try(as.yearmon(subdir_name_clean, format = "%b-%Y"), silent = TRUE)
  print(date_format)
  
  # Check if the conversion to date was successful; otherwise, treat as NA
  if (inherits(date_format, "try-error")) {
    date_format <- NA
  }
  
  # Add a new column with the date
  temp_df$Date <- date_format
  
  # Append the data to the combined data frame
  combined_df <- rbind(combined_df, temp_df)
}



# /----------------------------------------------------------------------------#
#/  Prep iteration number                                               --------

# Convert to year-month format
combined_df$Date <- as.Date(as.yearmon(combined_df$Date), frac = 0)

# Sort the combined_df by the Date column
combined_df <- combined_df[order(combined_df$Date), ]

# Assign a numeric value for each unique date
combined_df$iteration_num <- as.numeric(factor(combined_df$Date))


# /----------------------------------------------------------------------------#
#/  Compute error between pred & obs                                    --------

# Calculate difference
combined_df['Normalized_Respiration_Rate_abserror'] <- abs(combined_df['Predicted_Normalized_Respiration_Rate'] - combined_df['Observed_Normalized_Respiration_Rate'])


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



# Perform a left join using dplyr's left_join function
diff_df <- 
  left_join(first_iter_df, last_iter_df, by = "Sample_ID") %>% 
  mutate(Normalized_Respiration_Rate_abserror_lastminusfirst =  Normalized_Respiration_Rate_abserror_lastiter  -  Normalized_Respiration_Rate_abserror_firstiter,
         Predicted_Normalized_Respiration_Rate_lastminusfirst = Predicted_Normalized_Respiration_Rate_lastiter -  Predicted_Normalized_Respiration_Rate_firstiter)



# /----------------------------------------------------------------------------#
#/   Get point locations of GloRICH predictions                           ------

# The `output_all_sites_*` files have both the WHONDRS data and the GLORICH data. 
# You can filter out the WHONDRS data by selecting all sites with ID's prefixed with `SSS`, `CM_`, and `S19S` which correspond to the three major phases of the collection of respiration rate data.

glorich_preds <- 
  read_csv('https://github.com/WHONDRS-Hub/ICON-ModEx_Open_Manuscript/raw/refs/heads/origin/fig5_maps/fig-sample-iteration-map/data/ICON-ModEx_Combined_Predicted_Observed_Respiration_Rates.csv') %>%
  mutate(Predicted_Normalized_Respiration_Rate_lastminusfirst = Predicted_Normalized_Respiration_Rate_mg_DO_per_H_per_L_sediment_Nov2023 - Predicted_Normalized_Respiration_Rate_mg_DO_per_H_per_L_sediment_Sep2019) %>% 
  mutate(sample_type = case_when(str_detect(Sample_Name, "SSS") ~ "WHONDRS",
                                 str_detect(Sample_Name, "CM_") ~ "WHONDRS",
                                 str_detect(Sample_Name, "S19S") ~ "WHONDRS")) %>% 
  mutate(sample_type = ifelse(is.na(sample_type), "GLORICH", sample_type)) %>% 
  
  dplyr::select(Sample_Name, sample_type, Latitude, Longitude, 
         Predicted_Normalized_Respiration_Rate_mg_DO_per_H_per_L_sediment_Nov2023,
         Predicted_Normalized_Respiration_Rate_mg_DO_per_H_per_L_sediment_Sep2019,
         Predicted_Normalized_Respiration_Rate_lastminusfirst)


# /----------------------------------------------------------------------------#
#/    Get CONUS state polygons                                            ------

library(rnaturalearth)

conus <- ne_states(country = "united states of america")
conus <- conus %>% filter(!name %in% c('Alaska','Hawaii'))


# /----------------------------------------------------------------------------#
#/    Get map theme                                                       ------

# Load custom theme for figure
source('themes.r')


# /----------------------------------------------------------------------------#
#/   Convert to points

first_iter_df_points <- st_as_sf(
  first_iter_df,
  coords = c("Sample_Longitude_obs", "Sample_Latitude_obs"),
  crs = 4326) %>% 
  st_filter(., conus, .pred = st_intersects)


diff_df_points <- st_as_sf(
  diff_df,
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
  read.csv('https://github.com/parallelworks/dynamic-learning-rivers/blob/main/input_data/ICON-ModEx_Data.csv')
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
diff_df_points <- st_transform(diff_df_points, crs = 6350)
filtered_df_points <- st_transform(filtered_df_points, crs = 6350)
glorich_preds_points <- st_transform(glorich_preds_points, crs = 6350)
first_iter_df_points <- st_transform(first_iter_df_points, crs = 6350)
first_iter_df_points <- st_transform(first_iter_df_points, crs = 6350)
sample_dates <- st_transform(sample_dates, crs = 6350)


# /----------------------------------------------------------------------------#
#/    Add columns for coordinates               --------

diff_df_points <- bind_cols(diff_df_points, as.data.frame(st_coordinates(diff_df_points)))

glorich_preds_points <- bind_cols(glorich_preds_points, as.data.frame(st_coordinates(glorich_preds_points)))
