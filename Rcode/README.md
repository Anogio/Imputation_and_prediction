# Code repository

## Auxiliary
Contains various auxiliary functions used throughout the main code.

### dataloaders.R
Contains methods for importing and cleaning data. The main *loader* method returns a clean dataset split between response, categorical variables and numeric variables

### generate_missing.R
Contains methods to generate additional missing data in the dataset.

### MVN_imputation.R
Contains the methods used to impute missing data by normal conditional expectation, with separate functions for estimation and imputation. (see also package SIMVN (https://github.com/Anogio/SIMVN)

### prediction_methods.R
Contains a method to perform SAEM prediction and one to evaluate a binary unbalanced prediction.

### Simulation methods
Contains methods to generate datasets and run analyses multiple times for benchmarking

## SAEM_Wei_Jiang
Code copied from Wei's work. Contains the functions for the SAEM logisitc regression on incomplete data.

## Test_hemo_definitions
Side work aimed at evaluating various possible definitions of what a haemorrhagic shock is. 

## Thesis_main_experiments
The main code for the results presented in the report. Every file is a Rmarkdown with its html output and explanatory comments. The files are designated by the number of the figure in the report that presents their results.
