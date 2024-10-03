20 December 2022
CM SSS Data Package

CM_Respiration_10-28-2022.R and SSS_Respiration_10-28-2022.R were used to calculate respiration rates from time series of dissolved oxygen measurements. This script does not need to be run to use the respiration rate data. It is included for informational purposes and to allow users to run or modify the script if desired.

The input file for the script is found in the Inputs folder.

The script completes the following steps:
1) Removes the first measurement if the dissolved oxygen was higher than 9.5 millgrams per liter (mg/L) of O2. This value is set 1 mg/L above the threshold of expected DO concentration for 100% saturation at experimental temperature and pressure.
2) Calculates respiration rates as the slope of the linear regression between dissolved oxygen concentration and incubation time
3) If the regression does not seem to have a linear relationship, points are removed from the end of the regression line if the initial Rsq is below the set threshold of 0.99 to keep linearity. Points continue to be removed until Rsq stops increasing or the total number of points reaches four. Note points are removed only if the Rsq goes up after removing the points. If it goes down, then the point is not removed.
