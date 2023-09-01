01 June 2023
WHONDRS S19S Sediment Data Package

Respiration_06-01-2023.R was used to calculate respiration rates from time series dissolved oxygen measurements. This script does not need to be run to use the respiration rate data. It is included for informational purposes and to allow users to run or modify the script if desired.

The input file for the script is found in the Inputs folder.

The script completes the following steps:
1) Calculates respiration rates as the slope of the linear regression between dissolved oxygen concentration and incubation time
2) Removes samples that had positive slopes from their regressions
3) Estimates a theoretical respiration rates for samples for which dissolved oxygen reached 5 mg/L O2 (or less) in less than 3 minutes. See methods code for further details. 

