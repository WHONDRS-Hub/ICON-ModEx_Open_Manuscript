25 November 2020
WHONDRS S19S Sediment Data Package

Respiration_11-12-20.R was used to calculate respiration rates from time series dissolved oxygen measurements. This script does not need to be run to use the respiration rate data. It is included for informational purposes and to allow users to run or modify the script if desired.

The input file for the script is found in the Inputs folder.

The script completes the following steps:
1) Removes samples for which dissolved oxygen reached 5 mg/L O2 in less than 3 minutes
2) Calculates respiration rates as the slope of the linear regression between dissolved oxygen concentration and incubation time
3) Removes samples that had an Rsq< 0.25 from their regressions

