# Code repository

## main.R
Uses the methods from the auxiliary files to perform a full analysis. For now, performs imputation on a single dataset and applies a chosen prediction method.

## dataloaders.R
Contains methods for importing and cleaning data. the main *loader* method returns a clean dataset split between response, categorical variables and numeric variables

## generate_missing.R
Contains methods to generate additional missing data in the dataset. Right now, only the very simple *MCAR* method is implemented.

## imputation_methods.R
Contains methods for multiple imputation of a dataset with missing data. Each method should return a list of completed dataframes.

## prediction_methods.R
Contains a main method *multiple.prediction* which performs a train/test split, then a prediction on the test set for each of the dataset in the list it is given (with the same split every time).
