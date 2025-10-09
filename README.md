# ICON-ModEx_Open_Manuscript

#### Machine learning model inputs, outputs, and scripts associated with “Artificial intelligence-guided iterations between observations and modeling significantly improve environmental predictions”
Brieanne Forbes, Michael Bruen, Etienne Fluet-Chouinard, Amy E. Goldman, Vanessa Garayburu-Caruso, Stefan F. Gary, Avni Malhotra, Sushant Mehan, Tod Rubin, Timothy D. Scheibe,  Nicholas D. Ward, Bre Rivera Waterman,  James C. Stegen

## Summary
NOTE: The manuscript associated with this data package is currently in review. The data may be revised based on reviewer feedback. Upon manuscript acceptance, this data package will be updated with the final dataset and additional metadata.
This data package is associated with the manuscript “Artificial intelligence-guided iterations between observations and modeling significantly improve environmental predictions” (Malhotra et al., in prep).  This effort was designed following ICON (integrated, coordinated, open, and networked) principles to facilitate a model-experiment (ModEx) iteration approach, leveraging crowdsourced sampling across the contiguous United States (CONUS). New machine learning models were created every month to guide sampling locations. Data from the resulting samples were used to test and rebuild the machine learning models for the next round of sampling guidance. Associated sediment and water geochemistry and in situ sensor data can be found at https://data.ess-dive.lbl.gov/datasets/doi:10.15485/1923689, https://data.ess-dive.lbl.gov/datasets/doi:10.15485/1729719, and https://data.ess-dive.lbl.gov/datasets/doi:10.15485/1603775. This data package is associated with two GitHub repositories found at https://github.com/parallelworks/dynamic-learning-rivers and https://github.com/WHONDRS-Hub/ICON-ModEx_Open_Manuscript. 

In addition to this readme, this data package also includes two file-level metadata (FLMD) files that describes each file and two data dictionaries (DD) that describe all column/row headers and variable definitions. 

This data package consists of two main folders (1) dynamic-learning-rivers and (2) ICON-ModEx_Open_Manuscript which contain snapshots of the associated GitHub repositories ([dynamic-learning-rivers](https://github.com/parallelworks/dynamic-learning-rivers) and this repo).  

The input data, output data, and machine learning models used to guide sampling locations are within **dynamic-learning-rivers**. The folder is organized into five top-level directories: (1) “input_data” holds the training data for the ML models; (2) “ml_models” holds machine learning (ML) models trained on the data in “input_data”; (3) “examples” contains files for direct experimentation with the machine learning model, including scripts for setting up “hindcast” run; (4) “scripts” contains data preprocessing and postprocessing scripts and intermediate results specific to this data set that bookend the ML workflow; and (5) “output_data” holds the overall results of the ML model on that branch. Each trained ML model resides on its own branch in the repository; this means that inputs and outputs can be different branch-to-branch. There is also one hidden directory “.github/workflows”. This hidden directory contains information for how to run the ML workflow as an end-to-end automated GitHub Action but it is not needed for reusing the ML models archived here. Please see the top-level README.md in the GitHub repository for more details on the automation.

The scripts and data used to create figures in the manuscript are within **ICON-ModEx_Open_Manuscript**. The folder is organized into four folders which contain the scripts, data, and pdf for each figure. Within the “fig-model-score-evolution” folder, there is a folder called “intermediate_branch_data” which contains some intermediate files pulled from dynamic-learning-rivers and reorganized to easily integrate into the workflows. NOTE: THIS FOLDER INCLUDES THE FILES AT THE POINT OF PAPER SUBMISSION. IT WILL BE UPDATED ONCE THE PAPER IS ACCEPTED WITH ANY REVISIONS AND WILL INCLUDE A DD/FLMD AT THAT POINT.

## Manuscript Reference
The manuscript associated with this repo and Data Package can be cited as follows:  
> [manuscript citation will be added here once published]
## Data Reference
In addition to this repo, the Data Package is published and publicly available on ESS-DIVE. If using these data, please cite the Data Package with the following citation and DOI:  
> [data package citation will be added here once published]
## Contact
James Stegen; james.stegen@pnnl.gov
