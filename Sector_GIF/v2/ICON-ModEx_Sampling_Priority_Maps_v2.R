# ==============================================================================
#
# Create maps of CONUS with sampling points over iterations for ICON-ModEx
#
# Status: In progress
#
# Note: 
# ==============================================================================
#
# Author: Brieanne Forbes 
# 28 Feb 2025
#
# ==============================================================================
library(tidyverse) 
library(sf) # tidy spatial
library(raster) # work with rasters, NOTE: masks dplyr::select
library(ggthemes) # theme_map()
library(ggnewscale) # set multiple color scales
library(ggspatial) # add north arrow and scale bar
library(elevatr) # pull elevation maps
library(terra)

rm(list=ls(all=T))

# Setting wd to folder
current_path <- rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
setwd('./../..')

# =================================== get iterations ===========================

iterations_all <- list.files('./fig-model-score-evolution/intermediate_branch_data', pattern = 'log')

iterations_good <- iterations_all[-14] #remove old nov 2023

iteration_dates <-  str_extract(iterations_good, "^[A-Za-z]+-\\d{4}")

dates <- parse_date_time(iteration_dates, orders = "b-Y")

# ============================ read in CONUS shp file ============================
common_crs = 4326

conus_shp <- list.files('./Maps/CONUS_Boundary/', '\\.shp$', full.names = T)

conus_boundary <- read_sf(conus_shp) %>%
  st_transform(common_crs)

# Order the iteration list chronologically 
sorted_iterations <- iterations_good[order(dates)]

for(iteration in sorted_iterations) {
  
  # =================================== find coords  ===============================
  
  sampling_coords <- read_csv(paste0('./fig-simplified-model-score-evolution/intermediate_branch_data/',iteration,'/ICON-ModEx_Combined_Predicted_Observed_Respiration_Rates.csv'),
                 na = c('N/A', NA, '-9999')) %>%
  filter(Sample_Latitude_obs  < 50,
         Sample_Longitude_obs  < 70) %>% # filtering to CONUS only
    rename(Longitude = Sample_Longitude_obs,
           Latitude = Sample_Latitude_obs) %>%
  dplyr::select(Sample_ID, Longitude , Latitude ) %>%
    add_column(Type = 'Sampled Site')
  
  
  priority_coords <- read_csv(paste0('./fig-simplified-model-score-evolution/intermediate_branch_data/',iteration,'/output_all_sites_avgpre_stdpre_merged_filtered.csv'),
                              na = c('N/A', NA, '-9999'))%>%
    filter(Sample_Latitude_pre_avg  < 50,
           Sample_Longitude_pre_avg  < 70) %>% # filtering to CONUS only 
    mutate(Priority = case_when( combined.metric_pre_avg >= quantile(combined.metric_pre_avg, 0.9, na.rm = TRUE) ~ "Divergent",
                                 combined.metric_pre_avg <= quantile(combined.metric_pre_avg, 0.1, na.rm = TRUE) ~ "Convergent")) %>%
    filter(!is.na(Priority))%>%  # filter to priority sites
    rename(Longitude = Sample_Longitude_pre_avg,
           Latitude = Sample_Latitude_pre_avg) %>%
  dplyr::select(Sample_ID, Latitude , Longitude, Priority ) %>%
    mutate(Type = case_when(Priority == "Divergent" ~ 'Priority Site - Divergent',
                            Priority == "Convergent" ~ 'Priority Site - Convergent'),
           Sample_ID = as.character(Sample_ID))
  
  combined_coords <- sampling_coords %>%
    bind_rows(priority_coords) %>%
    arrange(Type)
  
  # ============================ create map ============================
  
  coords_sf <- combined_coords %>%
    filter(Type != 'Priority Site - Convergent') %>% 
    st_as_sf(coords = c('Longitude','Latitude'), crs = common_crs)
  
  combined_map <- ggplot()+
    geom_sf(data = conus_boundary)+
    geom_sf(data = coords_sf, aes(color = Type, shape = Type, size = Type)) +
    theme_map() + 
    scale_color_manual(values = c("Priority Site - Divergent" = "purple3", "Priority Site - Convergent" = "dodgerblue1" , 'Sampled Site' = 'black')) + 
    scale_shape_manual(values = c("Priority Site - Divergent" = 18, "Priority Site - Convergent" = 16, 'Sampled Site' = 16)) + 
    scale_size_manual(values = c("Priority Site - Divergent" = 3, "Priority Site - Convergent" = 1.5, 'Sampled Site' = 3))+
    theme(legend.position = 'none')

  clean_iteration <- str_extract(iteration, "^[A-Za-z]+-\\d{4}")
  
  number <- match(iteration, sorted_iterations)

  ggsave(paste0('./Sector_GIF/v2/', number, '-', clean_iteration, '_Sites.png'),
         combined_map,
         width = 6.5,
         height = 5)

rm(sampling_coords)

}
