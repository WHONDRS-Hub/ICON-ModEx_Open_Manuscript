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

# =================================== find files ===============================

coords <- read_csv('./ICON-ModEx_Combined_Predicted_Observed_Respiration_Rates.csv',
                 na = c('N/A', NA, '-9999')) %>%
  filter(!is.na(Observed_Normalized_Respiration_Rate_mg_DO_per_H_per_L_sediment)) %>% # filtering to observed data
  filter(Latitude < 50) %>% # filtering to CONUS only 
  dplyr::select(Sample_Name, Observed_Sample_Date, Latitude, Longitude)

# ============================ read in CONUS shp file ============================
common_crs = 4326

conus_shp <- list.files('./Maps/CONUS_Boundary/', '\\.shp$', full.names = T)

conus_boundary <- read_sf(conus_shp) %>%
  st_transform(common_crs)

# ============================ pull elevation ============================
# elev_data <- get_elev_raster(locations = conus_boundary$geometry, z = 5, src = "aws")
# 
# elevation_crop <- mask(elev_data, conus_boundary)
# 
# elevation <- as.data.frame(elevation_crop, xy = T) %>%
#   as_tibble() %>%
#   rename("long" = x,
#          "lat" = y,
#          "elevation" = 3) %>% #column index -> name (changing resolution changes colname)
#   filter(!is.na(elevation))

# ============================ create map for each iteration ============================
iterations <- c("2021-12-01", "2022-08-01", "2022-09-01", "2022-10-01", 
               "2022-11-01", "2022-12-01", "2023-01-01", "2023-02-01", "2023-03-01", 
               "2023-04-01", "2023-05-01", "2023-06-01", "2023-07-01", "2023-08-01", 
               "2023-09-01", "2023-10-01", "2023-11-01")

for (i in iterations) {

filt_sf <- coords %>%
  filter(Observed_Sample_Date < i) %>%
  mutate(new = case_when(Observed_Sample_Date >= prev_date ~ TRUE,
                         TRUE ~ FALSE)) %>%
  st_as_sf(coords = c('Longitude','Latitude'), crs = common_crs)

month_year <- format(as.Date(i), "%B %Y")

map <- ggplot()+
  geom_sf(data = conus_boundary)+
  geom_sf(data = filt_sf, aes(color = new), show.legend = F) +
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black"))+
  theme_map() + 
  # ggspatial::annotation_scale(
  #   location = "bl",
  #   bar_cols = c("black", "white")) +
  # ggspatial::annotation_north_arrow(
  #   location = "bl", which_north = "true",
  #   pad_y = unit(0.25, "in"),
  #   style = ggspatial::north_arrow_nautical(
  #     fill = c("black", "white"),
  #     line_col = "grey20"))+
  annotate("text", x = -120, y = 51, label = month_year, size = 4, color = "black")

prev_date <- i



ggsave(paste0('./Sector_GIF/', month_year %>% str_replace(' ', "_"), '.png'),
       map,
       width = 6.5,
       height = 5
)

}
