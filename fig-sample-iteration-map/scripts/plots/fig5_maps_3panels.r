

# /----------------------------------------------------------------------------#
#/  Fig.5A -  Map predicted RR at WHONDRS and GLORICH sites          -----------


predicted_respiration_rate_map <-

  ggplot() +
  geom_sf(data=conus, size=0.2, color='grey50', fill='grey90') +

  # Plot GLORICH points  
  geom_point(data= subset(glorich_preds_points, sample_type=='GLORICH') %>% arrange(desc(Predicted_Normalized_Respiration_Rate_mg_DO_per_H_per_L_sediment_Nov2023)), 
             aes(x=X, y=Y, 
                 fill=abs(Predicted_Normalized_Respiration_Rate_mg_DO_per_H_per_L_sediment_Nov2023)),
             stroke=0.01, shape=21, size=1.25, colour = "transparent",
             position=position_jitter(h=0, w=0)) +

  # Plot WHNDRS points
  geom_point(data= subset(glorich_preds_points, sample_type!='GLORICH') %>% arrange(desc(Predicted_Normalized_Respiration_Rate_mg_DO_per_H_per_L_sediment_Nov2023)), 
             aes(x=X, y=Y, fill=abs(Predicted_Normalized_Respiration_Rate_mg_DO_per_H_per_L_sediment_Nov2023)), 
                 stroke=0.3, shape=21, size=1.75, colour = "black",
                 position=position_jitter(h=45000, w=45000)) +
  
  # coord_sf(expand=FALSE) +
  map_theme() +
  
  scale_fill_gradientn(colors= sel_col,
                       trans='log',
                       breaks=c(10^0, 10^1, 10^2, 10^3), # 10^5, 10^6
                       oob=squish,
                       labels=c(
                         expression(10^{0}),
                         expression(10^{1}),
                         expression(10^{2}),
                         expression(10^{3})),
                       limits=c(10^{0}, 10^{3})) +
  
  guides(fill = guide_colorbar(
    nbin=10, barheight = .6, barwidth=14, reverse=F, display='rectangles', 
    frame.colour=c('black'), frame.linewidth=0.4,
    ticks.colour='black',  direction='horizontal',
    title.position = "top", title.hjust=0.5,
    title = expression(paste("Predicted respiration rate (mg O"[2]" L of sediment "[-1]" hour"[-1]")"))))



# /----------------------------------------------------------------------------#
#/  5B - Map diff predRR with diverging color ramp                    ----------


# Color ramp
pos_col <- rev(sequential_hcl(5, palette = 'Greens 3'))[2:5]
neg_col <- rev(sequential_hcl(4, palette = 'Purples 3'))[2:4]




#  Make layer function for GLORICH points
glorich_pos_layer <- function(rng_start, rng_end, col_idx) {
  geom_point(data= subset(glorich_preds_points, sample_type=='GLORICH' & 
                            Predicted_Normalized_Respiration_Rate_lastminusfirst > 0 &
                            abs(Predicted_Normalized_Respiration_Rate_lastminusfirst) > rng_start &
                            abs(Predicted_Normalized_Respiration_Rate_lastminusfirst) <= rng_end),
             aes(x=X, y=Y), size=1.25,
             color = pos_col[col_idx] )
  }


#  Make layer function for GLORICH points
glorich_neg_layer <- function(rng_start, rng_end, col_idx) {
  geom_point(data= subset(glorich_preds_points, sample_type=='GLORICH' & 
                            Predicted_Normalized_Respiration_Rate_lastminusfirst < 0 &
                            abs(Predicted_Normalized_Respiration_Rate_lastminusfirst) > rng_start &
                            abs(Predicted_Normalized_Respiration_Rate_lastminusfirst) <= rng_end),
             aes(x=X, y=Y), size=1.25,
             color = neg_col[col_idx] )
  }


#  Make layer function for WHONDRS points
whondrs_pos_layer <- function(rng_start, rng_end, col_idx) {
  geom_point(data= subset(glorich_preds_points, sample_type != 'GLORICH' &
                            Predicted_Normalized_Respiration_Rate_lastminusfirst > 0 &
                            abs(Predicted_Normalized_Respiration_Rate_lastminusfirst) > rng_start &
                            abs(Predicted_Normalized_Respiration_Rate_lastminusfirst) <= rng_end),
             aes(x=X, y=Y), shape=21,  stroke=0.3, size=1.75, colour = "black",
             fill= pos_col[col_idx],
             position=position_jitter(h=45000, w=45000))
  }


#  Make layer function for WHONDRS points
whondrs_neg_layer <- function(rng_start, rng_end, col_idx) {
  geom_point(data= subset(glorich_preds_points, sample_type != 'GLORICH' &
                            Predicted_Normalized_Respiration_Rate_lastminusfirst < 0 &
                            abs(Predicted_Normalized_Respiration_Rate_lastminusfirst) > rng_start &
                            abs(Predicted_Normalized_Respiration_Rate_lastminusfirst) <= rng_end),
             aes(x=X, y=Y), shape=21,  stroke=0.3, size=1.75, colour = "black",
             fill= neg_col[col_idx],
             position=position_jitter(h=45000, w=45000))
  }



# /----------------------------------------------------------------------------#
#/  
site_predRR_map <- 
  
  ggplot() +
  geom_sf(data=conus, size=0.2, color='grey50', fill='grey90') +
  
  # Plot GLORICH points w/ positive values
  geom_point(data= subset(glorich_preds_points, sample_type=='GLORICH' & Predicted_Normalized_Respiration_Rate_lastminusfirst > 0) %>%
             arrange(desc(abs(Predicted_Normalized_Respiration_Rate_lastminusfirst))),
             aes(x=X, y=Y, fill=abs(Predicted_Normalized_Respiration_Rate_lastminusfirst)),
             stroke=0.01, shape=21, size=0, colour = "transparent",
             position=position_jitter(h=0, w=0)) +
  
  scale_fill_gradientn(colors= pos_col,
                       trans='log',
                       breaks=c(10^0, 10^1, 10^2, 10^3, 10^4, 10^5),
                       oob=squish,
                       labels=c(
                                expression(10^{0}),
                                expression(10^{1}),
                                expression(10^{2}),
                                expression(10^{3}),
                                expression(10^{4}),
                                expression(10^{5})),
                       limits=c(10^{0}, 10^{5})) +
  
  ###  GLORICH
  glorich_pos_layer(10^0, 10^1, 1) +
  glorich_neg_layer(10^0, 10^1, 1) +
  
  glorich_pos_layer(10^1, 10^2, 2) +
  glorich_neg_layer(10^1, 10^2, 2) +
  
  glorich_pos_layer(10^2, 10^3, 3) +
  glorich_neg_layer(10^2, 10^3, 3) +
  
  glorich_pos_layer(10^3, 10^4, 4) +
  glorich_neg_layer(10^3, 10^4, 4) +
  
  glorich_pos_layer(10^4, 10^5, 5) +
  glorich_neg_layer(10^4, 10^5, 5) +
  
  ###  WHONDRS
  whondrs_pos_layer(10^0, 10^1, 1) +
  whondrs_neg_layer(10^0, 10^1, 1) +
  
  whondrs_pos_layer(10^1, 10^2, 2) +
  whondrs_neg_layer(10^1, 10^2, 2) +
  
  whondrs_pos_layer(10^2, 10^3, 3) +
  whondrs_neg_layer(10^2, 10^3, 3) +
  
  whondrs_pos_layer(10^3, 10^4, 4) +
  whondrs_neg_layer(10^3, 10^4, 4) +
  
  whondrs_pos_layer(10^4, 10^5, 5) +
  whondrs_neg_layer(10^4, 10^5, 5) +
  
  
  map_theme() +
  
  guides(fill = guide_colorbar(
    nbin=5, barheight = .6, barwidth=7, reverse=F, raster=F, 
    frame.colour=c('black'), frame.linewidth=0.4,
    ticks.colour='black',  direction='horizontal',
    title.position = "top", title.hjust=0.5,
    title = expression(paste("Change in predicted respiration rate from first to last iteration (mg O"[2]," L of sediment "[-1]" hour"[-1]")"))))





# /----------------------------------------------------------------------------#
#/   Fig.5B -  negative bar plot                                       --------
# Note - needs a positive bar (and not negative) because the actual figure plotting ends by putting the negative values on top bc there are fewer of those points.
# Update- no longer;  now individual values are on top.


site_predRR_map_positive_bar <- 
  
  
  ggplot() +
  geom_sf(data=conus, size=0.2, color='grey50', fill='grey90') +  
  
  geom_sf(data= subset(diff_df_points, Predicted_Normalized_Respiration_Rate_lastminusfirst < 0 ), 
          aes(fill=Predicted_Normalized_Respiration_Rate_lastminusfirst * -1), 
          stroke=0.3, shape=21, size=1.75, color='black') + 
  
  scale_fill_gradientn(colors= neg_col,
                       trans='log',
                       breaks=c( 10^0, 10^1, 10^2, 10^3),
                       labels=c(expression(10^{0}),
                                expression(10^{1}),
                                expression(10^{2}),
                                expression(10^{3})),
                       limits=c(10^{0}, 10^{3})) +
  map_theme() +
  
  guides(fill = guide_colorbar(
    nbin=3, barheight = .6, barwidth=7, reverse=T, raster=F, 
    frame.colour=c('black'), frame.linewidth=0.4,
    ticks.colour='black',  direction='horizontal',
    title.position = "top", title.hjust=0.5,
    title = expression(paste("Change in predicted respiration rate from first to last iteration (mg O"[2]," L of sediment "[-1]" hour"[-1]")"))))



ggsave('../ICON-ModEx_Open_Manuscript/Maps/sample_iteration_map/site_predRR_positivebar_v05.pdf',
       site_predRR_map_positive_bar,
       width=180, height=110, dpi=500, units='mm')



# /----------------------------------------------------------------------------#
#/  Fig.5C -  Map diff with diverging color ramp                 ----------


# Color ramp
pos_col <- rev(sequential_hcl(5, palette = 'Blues 3'))[2:5]
neg_col <- rev(sequential_hcl(5, palette = 'Reds 3'))[2:5]



#  Make layer function for WHONDRS points
differror_pos_layer <- function(rng_start, rng_end, col_idx) {
  geom_point(data= subset(diff_df_points, #sample_type != 'GLORICH' &
                            Normalized_Respiration_Rate_abserror_lastminusfirst > 0 &
                            abs(Normalized_Respiration_Rate_abserror_lastminusfirst) > rng_start &
                            abs(Normalized_Respiration_Rate_abserror_lastminusfirst) <= rng_end),
             aes(x=X, y=Y), shape=21,  stroke=0.3, size=1.75, colour = "black",
             fill= neg_col[col_idx],
             position=position_jitter(h=45000, w=45000))
}


#  Make layer function for WHONDRS points
differror_neg_layer <- function(rng_start, rng_end, col_idx) {
  geom_point(data= subset(diff_df_points, #sample_type != 'GLORICH' &
                            Normalized_Respiration_Rate_abserror_lastminusfirst < 0 &
                            abs(Normalized_Respiration_Rate_abserror_lastminusfirst) > rng_start &
                            abs(Normalized_Respiration_Rate_abserror_lastminusfirst) <= rng_end),
             aes(x=X, y=Y), shape=21,  stroke=0.3, size=1.75, colour = "black",
             fill= pos_col[col_idx],
             position=position_jitter(h=45000, w=45000))
}



# /----------------------------------------------------------------------------#
#/  
site_error_map <- 
  
  ggplot() +
  geom_sf(data=conus, size=0.2, color='grey50', fill='grey90') +  
  
  # Negative error values means last < first, meaning decrease in error, and positive colors
  geom_point(data= subset(diff_df_points, Normalized_Respiration_Rate_abserror_lastminusfirst < 0 ),
             aes(x=X, y=Y, fill=Normalized_Respiration_Rate_abserror_lastminusfirst * -1),
             stroke=0.3, shape=21, size=0, color='transparent',
             position=position_jitter(h=45000, w=45000)) +
  
  
  scale_fill_gradientn(colors= pos_col,
                     trans='log',
                     breaks=c(10^0, 10^1, 10^2, 10^3),
                     oob=squish,
                     labels=c(
                              expression(10^{0}),
                              expression(10^{1}),
                              expression(10^{2}),
                              expression(10^{3})),
                     limits=c(10^{0}, 10^{3})) +
  
  ###  WHONDRS layers
  differror_pos_layer(10^0, 10^1, 1) +
  differror_neg_layer(10^0, 10^1, 1) +
  
  differror_pos_layer(10^1, 10^2, 2) +
  differror_neg_layer(10^1, 10^2, 2) +
  
  differror_pos_layer(10^2, 10^3, 3) +
  differror_neg_layer(10^2, 10^3, 3) +
  
  differror_pos_layer(10^3, 10^4, 4) +
  differror_neg_layer(10^3, 10^4, 4) +
  
  differror_pos_layer(10^4, 10^5, 5) +
  differror_neg_layer(10^4, 10^5, 5) +
  
  map_theme() +
  guides(fill = guide_colorbar(
    nbin=3, raster=F, barheight = .6, barwidth=7, reverse=F,
    frame.colour=c('black'), frame.linewidth=0.4,
    ticks.colour='black',  direction='horizontal',
    title.position = "top", title.hjust=0.5,
    title = expression(paste("Change in error from first to last iteration (mg O"[2]," L of sediment "[-1]" hour"[-1]")"))))





# /----------------------------------------------------------------------------#
#/  Fig.5C - Negative bar plot                                          --------

site_error_map_negative_bar <- 
  
  ggplot() +
  geom_sf(data=conus, size=0.2, color='grey50', fill='grey90') +  
  
  # Negative error values means last < first, meaning decrease in error, and positive colors
  geom_sf(data= subset(diff_df_points, Normalized_Respiration_Rate_abserror_lastminusfirst < 0 ), 
          aes(fill=Normalized_Respiration_Rate_abserror_lastminusfirst * -1), 
          stroke=0.3, shape=21, size=0, color='transparent') + 
  
  scale_fill_gradientn(colors= neg_col,
                       trans='log',
                       breaks=c(10^0, 10^1, 10^2, 10^3),
                       labels=c(expression(10^{0}),
                                expression(10^{1}),
                                expression(10^{2}),
                                expression(10^{3})),
                       limits=c(10^{0}, 10^{3})) +
  map_theme() +
  
  guides(fill = guide_colorbar(
    nbin=3, raster=F, barheight = .6, barwidth=7, reverse=T,
    frame.colour=c('black'), frame.linewidth=0.4,
    ticks.colour='black',  direction='horizontal',
    title.position = "top", title.hjust=0.5,
    title = expression(paste("Change in error from first to last iteration (mg O"[2]," L of sediment "[-1]" hour"[-1]")"))))



ggsave('../ICON-ModEx_Open_Manuscript/Maps/sample_iteration_map/site_error_map_negativebar_v05.pdf',
       site_error_map_negative_bar,
       width=180, height=110, dpi=500, units='mm')



# /----------------------------------------------------------------------------#
#/  Fig.5 - Combine on single plot                                     ---------

# arrange plots grob into layout 

fig <- plot_grid(predicted_respiration_rate_map, 
                 site_predRR_map,
                 site_error_map,
                 
                 ncol=1, nrow=3, 
                 rel_heights = c(1, 1, 1),
                 labels = c('A','B','C'),
                 align='hv')


# /----------------------------------------------------------------------------#
#/   Save figure to file                                               ---------

# Note: This initial figure is then manually modified to include the additional
#       colorbars from the other figures.

ggsave('../ICON-ModEx_Open_Manuscript/Maps/sample_iteration_map/iconmodex_fig5_3panels_v05.pdf',
       fig,
       width=90, height=230, dpi=600, units='mm')



