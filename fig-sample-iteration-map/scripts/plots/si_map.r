
# /----------------------------------------------------------------------------#
#/   SI: A-  Map month of sampling at each site                        ----------

month_map <-
  
  ggplot() +
  geom_sf(data=conus, size=0.2, color='grey50', fill='grey90') +
  geom_sf(data= sample_dates, aes(fill=month), stroke=0.2, shape=21, size=2.5, color='black') + 
  
  scale_fill_gradientn(colors = c(rainbow(11), "red"), breaks= seq(1,12), labels = month.abb) +

  map_theme() +
  
  guides(fill = guide_colorbar(
    nbin=12, raster=F, barheight = .6, barwidth=14, reverse=F,
    frame.colour=c('black'), frame.linewidth=0.4,
    ticks.colour='black',  direction='horizontal',
    title.position = "top", title.hjust=0.5,
    title = expression(paste("Sampling month"))))



# /----------------------------------------------------------------------------#
#/   SI: B-  Map iteration number of each site                        ----------

site_iteration_map <-
  
  ggplot() +
  # Color points
  geom_sf(data=conus, size=0.2, color='grey50', fill='grey90') +
  # Add circle outline
  geom_sf(data= filtered_df_points, aes(fill=iteration_num), stroke=0.4, shape=21, size=2.5, color='black') + 
  
  scale_fill_distiller(palette='YlGn', direction=1, breaks=c(1, 5, 10, 15, 19)) +

  map_theme() +
  
  guides(fill = guide_colorbar(
    nbin=8, raster=F, barheight = .6, barwidth=14, reverse=F,
    frame.colour=c('black'), frame.linewidth=0.4,
    ticks.colour='black',  direction='horizontal',
    title.position = "top", title.hjust=0.5,
    title = expression(paste("Iteration count of first sample"))))



# /----------------------------------------------------------------------------#
#/  SI: C - Map observed Sediment Respiration Rate                       -------

obs_respiration_rate_map <-
  
  ggplot() +
  geom_sf(data=conus, size=0.2, color='grey50', fill='grey90') +
  
  geom_sf(data= first_iter_df_points, aes(fill=abs(Normalized_Respiration_Rate_mg_DO_per_H_per_L_sediment_obs)), 
          stroke=0.4, shape=21, size=2.5) + 
  
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
                       # expression(10^{4})),
                       limits=c(10^{0}, 10^{3})) +
  
  guides(fill = guide_colorbar(
    nbin=8, raster=F, barheight = .6, barwidth=14, reverse=F,
    frame.colour=c('black'), frame.linewidth=0.4,
    ticks.colour='black',  direction='horizontal',
    title.position = "top", title.hjust=0.5,
    title = expression(paste("Observed respiration rate  (mg O"[2]," / L of sediment / hour)"))))


# /----------------------------------------------------------------------------#
#/   Combine on single plot                                             --------

# arrange plots grob into layout 
si_fig <- plot_grid(month_map, 
                    site_iteration_map, 
                    obs_respiration_rate_map,
                    
                    ncol=1, nrow=3, 
                    rel_heights = c(1, 1),
                    
                    labels = c('A','B','C'),
                    align='hv')


# /----------------------------------------------------------------------------#
#/    Save figure to file                                               --------

ggsave('../ICON-ModEx_Open_Manuscript/Maps/sample_iteration_map/iconmodex_si_maps_3panels_v04.pdf',
       si_fig,
       width=110, height=220, dpi=600, units='mm')





# ggsave('../ICON-ModEx_Open_Manuscript/Maps/sample_iteration_map/site_iteration_map_v03.pdf',
#        site_iteration_map,
#        width=180, height=110, dpi=500, units='mm')


# ggsave('../ICON-ModEx_Open_Manuscript/Maps/sample_iteration_map/obs_respiration_rate_map_v03.pdf',
#        obs_respiration_rate_map,
#        width=180, height=110, dpi=500, units='mm')