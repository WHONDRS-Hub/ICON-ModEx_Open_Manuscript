



# Join the 
combined_df_j <- 
  left_join(combined_df,
            diff_df[,c('Sample_ID', 'Predicted_Normalized_Respiration_Rate_lastminusfirst', 'Normalized_Respiration_Rate_abserror_lastminusfirst')],
            by='Sample_ID') %>% 
  arrange(Normalized_Respiration_Rate_abserror_lastminusfirst) %>% 
  mutate(Date_MY = as.yearmon(Date)) %>% 
  mutate(iteration_date = paste('#', iteration_num, ' - ', Date_MY)) %>% 
  mutate(iteration_date = fct_reorder(iteration_date, iteration_num))
  # mutate(name = factor(name, levels=c("north", "north-east", "east", "south-east", "south", "south-west", "west", "north-west"))) %>%
  







# Color ramp
green_cols <- rev(sequential_hcl(6, palette = 'Greens 3'))[2:6]
purp_cols <- rev(sequential_hcl(6, palette = 'Purples 3'))[2:6]

# Color ramp
blue_cols <- rev(sequential_hcl(6, palette = 'Blues 3'))[2:6]
red_cols <- rev(sequential_hcl(6, palette = 'Reds 3'))[2:6]






#-------------------------------------------------------------------------------
### PLOT OF RR CHANGE



rr_line_plot  <- 
  ggplot()+
  geom_line(data= subset(combined_df_j, Predicted_Normalized_Respiration_Rate_lastminusfirst < 0), 
            aes(x= iteration_num, y= abs(Predicted_Normalized_Respiration_Rate), color=abs(Predicted_Normalized_Respiration_Rate_lastminusfirst), group=Sample_ID), size=0.1) +
  
  # geom_point(data= first_iter_df, aes(x= iteration_num, y= abs(Predicted_Normalized_Respiration_Rate))) +
  
  scale_color_gradientn(colors= green_cols,
                        trans='log',
                        breaks=c(10^0, 10^1, 10^2, 10^3),
                        oob=squish,
                        labels=c(
                          expression(10^{0}),
                          expression(10^{1}),
                          expression(10^{2}),
                          expression(10^{3})),
                        limits=c(10^{0}, 10^{3})) +
  
  new_scale_color() +
  
  geom_line(data= subset(combined_df_j, Predicted_Normalized_Respiration_Rate_lastminusfirst > 0), 
            aes(x= iteration_num, y= abs(Predicted_Normalized_Respiration_Rate), color=abs(Predicted_Normalized_Respiration_Rate_lastminusfirst), group=Sample_ID), size=0.1) +
  
  scale_color_gradientn(colors= purp_cols,
                        trans='log',
                        breaks=c(10^0, 10^1, 10^2, 10^3),
                        oob=squish,
                        labels=c(
                          expression(10^{0}),
                          expression(10^{1}),
                          expression(10^{2}),
                          expression(10^{3})),
                        limits=c(10^{0}, 10^{3})) +
  
  scale_y_log10(labels = label_log(digits = 1)) +
  # coord_cartesian(ylim=c(10^1, 10^3.5)) +
  scale_x_continuous(breaks=seq(1,19), labels=seq(1,19), expand=c(0,0)) +
  line_plot_theme() +
  theme(legend.title = element_blank()) +
  xlab("Iteration number") +
  ylab( expression(paste("Predicted respiration rate"))) +  #  (mg O"[2],"/ L of sediment / hour)
  labs(caption = 'Green = increased predicted RR between first and last\n
              Purple = decrease predicted RR between first and last')



#-------------------------------------------------------------------------------
### PLOT OF ERROR CHANGE

error_line_plot <- 
  ggplot()+
  geom_line(data= subset(combined_df_j, Normalized_Respiration_Rate_abserror_lastminusfirst < -1), 
            aes(x= iteration_num, y= Normalized_Respiration_Rate_abserror, color=abs(Normalized_Respiration_Rate_abserror_lastminusfirst), group=Sample_ID), size=0.1) +

  scale_color_gradientn(colors= blue_cols,
                       trans='log',
                       breaks=c(10^0, 10^1, 10^2, 10^3, 10^4),
                       oob=squish,
                       labels=c(
                         expression(10^{0}),
                         expression(10^{1}),
                         expression(10^{2}),
                         expression(10^{3}),
                         expression(10^{4})),
                       limits=c(10^{0}, 10^{4})) +

  new_scale_color() +
  
  geom_line(data= subset(combined_df_j, Normalized_Respiration_Rate_abserror_lastminusfirst > 1), 
            aes(x= iteration_num, y= Normalized_Respiration_Rate_abserror, color=abs(Normalized_Respiration_Rate_abserror_lastminusfirst), group=Sample_ID), size=0.1) +
  
  scale_color_gradientn(colors= red_cols,
                        trans='log',
                        breaks=c(10^0, 10^1, 10^2, 10^3, 10^4),
                        oob=squish,
                        labels=c(
                          expression(10^{0}),
                          expression(10^{1}),
                          expression(10^{2}),
                          expression(10^{3}),
                          expression(10^{4})),
                        limits=c(10^{0}, 10^{4})) +
  
  scale_y_log10(labels = label_log(digits = 1)) +
  coord_cartesian(ylim=c(10^1, 10^3.5)) +
  scale_x_continuous(breaks=seq(1,19), labels=seq(1,19), expand=c(0,0)) +
  line_plot_theme() +
  theme(legend.title = element_blank()) +
  xlab("Iteration number") +
  ylab( expression(paste("Error of predicted RR"))) +  # (mg O"[2],"/ L of sediment / hour)
  labs(caption = 'Red = increased in error of predicted RR between first and last\n
              Purple = decrease in error of predicted RR between first and last') 





# /----------------------------------------------------------------------------#
#/  Fig.5 - Combine on single plot                                     ---------

# arrange plots grob into layout 
fig <- plot_grid(rr_line_plot,
                 error_line_plot, 
                 
                 ncol=1, nrow=2, 
                 rel_heights = c(1, 1),
                 labels = c('A','B'),
                 align='hv')


# /----------------------------------------------------------------------------#
#/   Save figure to file                                               ---------

ggsave('../ICON-ModEx_Open_Manuscript/Maps/sample_iteration_map/iconmodex_iteration_lineplot_v01.png',
       fig,
       width=190, height=170, dpi=600, units='mm')





##### 

combined_df_j_s <- combined_df_j %>% 
  filter(iteration_num %in% c(1,2,19))


iteration_scatter <-
  
  
  ggplot(combined_df_j_s) +
  geom_point(aes(x=abs(Observed_Normalized_Respiration_Rate), y=abs(Predicted_Normalized_Respiration_Rate), 
                 color=iteration_date), size=0.75) +

  scale_y_log10(labels = label_log(digits = 1), limits=c(10^0, 10^4)) +
  scale_x_log10(labels = label_log(digits = 1)) +
  labs(color='Iteration # and date') +
  coord_equal() +
  line_plot_theme() +
  # theme(legend.title = element_blank()) +
  xlab('Observed respiration rate') +
  ylab('Predicted respiration rate')



# /----------------------------------------------------------------------------#
#/   Save figure to file                                               ---------

ggsave('../ICON-ModEx_Open_Manuscript/Maps/sample_iteration_map/iteration_scatter.png',
       iteration_scatter,
       width=110, height=90, dpi=300, units='mm')



