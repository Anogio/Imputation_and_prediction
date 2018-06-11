# Code repository

## Auxiliary functions
Contains various functions used throughout the main code.

### dataloaders.R
Contains methods for importing and cleaning data. The main *loader* method returns a clean dataset split between response, categorical variables and numeric variables

### generate_missing.R
Contains methods to generate additional missing data in the dataset. Right now, only the very simple *MCAR* method is implemented.

### imputation_methods.R
Contains methods for multiple imputation of a dataset with missing data.

### prediction_methods.R
Contains methods to perform prediction and aggregation on (multiply imputed) datasets.

## Examples

These files are the main body of this project. They use the code from the other folders to perform analyses on the datasets.

### Clean
Contains all of the finished analyses, in the form of RMarkdown files along with their HTML output.

### Draft
Contains all work-in-progress analyses

## SAEM_Wei_Jiang
Code copied for Wei's work. Contains the functions for the SAEM logisitc regression on incomplete data.

## Test_hemo_definitions
Side work aimed at evaluating various possible definitions of what a haemorrhagic shock is. 

