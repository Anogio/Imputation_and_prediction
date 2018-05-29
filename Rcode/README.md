# Code repository

## Auxiliary functions

### dataloaders.R
Contains methods for importing and cleaning data. the main *loader* method returns a clean dataset split between response, categorical variables and numeric variables

### generate_missing.R
Contains methods to generate additional missing data in the dataset. Right now, only the very simple *MCAR* method is implemented.

### imputation_methods.R
Contains methods for multiple imputation of a dataset with missing data. Each method should return a list of completed dataframes.

### prediction_methods.R
Contains a main method *multiple.prediction* which performs a train/test split, then a prediction on the test set for each of the dataset in the list it is given (with the same split every time).

## Examples

These files use the functions defined in hte auxiliary folder to perform a comparison of various imputation methods on different datasets

## example1_titanic.R
Uses the methods from the auxiliary files to perform a full analysis on the titanic data. More precisely, its final purpose is to compare the ROC curves for prediction on 
- the original dataset
- the dataset with added missing data and multiple imputation
- the dataset with added missing data and simple imputation

## SAEM_Wei_Jiang
Code for the SAEM logistic regression, as implemented by Wei Jiang. 
