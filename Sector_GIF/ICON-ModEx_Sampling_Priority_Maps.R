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
setwd('./..')

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
  
  # =================================== sampling sites ===============================
  
  ## =================================== find coords ===============================
  
  sampling_coords <- read_csv(paste0('./fig-simplified-model-score-evolution/intermediate_branch_data/',iteration,'/ICON-ModEx_Combined_Predicted_Observed_Respiration_Rates.csv'),
                 na = c('N/A', NA, '-9999')) %>%
  filter(Sample_Latitude_obs  < 50,
         Sample_Longitude_pre_avg  < 70) %>% # filtering to CONUS only
  dplyr::select(Sample_ID, Sample_Longitude_obs , Sample_Latitude_obs )
  
  
  ## ============================ create map ============================
  
  sampling_coords_sf <- sampling_coords %>%
    st_as_sf(coords = c('Sample_Longitude_obs','Sample_Latitude_obs'), crs = common_crs)
  
  sampling_map <- ggplot()+
    geom_sf(data = conus_boundary)+
    geom_sf(data = sampling_coords_sf, show.legend = F) +
    theme_map() 

  clean_iteration <- str_extract(iteration, "^[A-Za-z]+-\\d{4}")
  
  number <- match(iteration, sorted_iterations)

  ggsave(paste0('./Sector_GIF/', number, '-', clean_iteration, '_Sampling_Sites.png'),
         sampling_map,
         width = 6.5,
         height = 5
  )
  
  # =================================== priority sites ===============================
  
  ## =================================== find coords ===============================

  priority_coords <- read_csv(paste0('./fig-simplified-model-score-evolution/intermediate_branch_data/',iteration,'/output_all_sites_avgpre_stdpre_merged_filtered.csv'),
                              na = c('N/A', NA, '-9999'))%>%
    filter(Sample_Latitude_pre_avg  < 50,
           Sample_Longitude_pre_avg  < 70) %>% # filtering to CONUS only 
    mutate(Priority = case_when( combined.metric_pre_avg >= quantile(combined.metric_pre_avg, 0.9, na.rm = TRUE) ~ "Divergent",
                                 combined.metric_pre_avg <= quantile(combined.metric_pre_avg, 0.1, na.rm = TRUE) ~ "Convergent")) %>%
    filter(!is.na(Priority)) # filter to priority sites


  ## ============================ create map ============================

  priority_coords_sf <- priority_coords %>%
    st_as_sf(coords = c('Sample_Longitude_pre_avg','Sample_Latitude_pre_avg'), crs = common_crs)

  priority_map <- ggplot()+
    geom_sf(data = conus_boundary)+
    geom_sf(data = priority_coords_sf, aes(color = Priority, shape = Priority, size = Priority)) +
    theme_map()+ 
    scale_color_manual(values = c("Divergent" = "purple3", "Convergent" = "dodgerblue1")) + 
    scale_shape_manual(values = c("Divergent" = 18, "Convergent" = 16)) + 
    scale_size_manual(values = c("Divergent" = 3, "Convergent" = 1.5))+
    theme(legend.background = element_blank(),
          legend.position = c(0.1, 0.1),
          legend.title = element_blank(),  # Remove legend title
          legend.text = element_text(size = 10))  # Increase legend key size
  
  ggsave(paste0('./Sector_GIF/', number, '-', clean_iteration, '_Priority_Sites.png'),
         priority_map,
         width = 6.5,
         height = 5
  )


}
