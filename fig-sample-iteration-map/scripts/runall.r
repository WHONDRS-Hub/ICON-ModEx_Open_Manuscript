# Description: Overview script executing the data preparation and plotting 
#              Figure 5 showing maps of ICON-MODEX sampling locations across the United States.
# Contact: Etienne Fluet-Chhouinard
# Affiliation: Pacific Northwest National Laboratory, Richland, WA, USA
# Email: etienne.fluet@pnnl.gov


# /-----------------------------------------------------------------------------
#/    Set up                                                    ----------------

# Set working directory in this script/project location
library(here); here()

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

