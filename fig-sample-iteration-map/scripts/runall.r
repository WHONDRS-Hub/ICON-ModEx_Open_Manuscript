# Description: Script running the data preparation and plotting 
#              Figure 5 showing maps of sampling locations.


library(here); here()


# /-----------------------------------------------------------------------------
#/    Set up                                                    ----------------

# Get Natural Earth polygons; to plot on maps as background
source('set_up/get_natearth.r')

# Get ggplot custom themes
source('set_up/theme.r')



# /-----------------------------------------------------------------------------
#/    Data prep                                                 ----------------

# Compile RR predictions across all the iterations into a single df
source('data_prep/compile_iterations.r')


# Report key summary metrics; to report in ICON-Modex paper
source('data_prep/summary_for_paper.r')



# /-----------------------------------------------------------------------------
#/    Plots                                                     ----------------


# Make figure 5 for ICON-Modex paper; of 3 map panels
source('plots/fig_maps_3panels.r')

# Make lineplot of RR across iterations
source('plots/lineplot_iterations.r')


# Make supplemental maps
source('plots/si_map.r')

