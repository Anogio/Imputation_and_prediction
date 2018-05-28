## Imports
library(tidyverse)

# Custom imports
source('dataloaders.R')
source('generate_missing.R')
source('imputation_methods.R')
source('prediction_methods.R')

seed = 42
dataset = 'titanic'
prop_added_missing = 0.05
n_imputations = 5
prediction_method = 'rf'
train_size = 0.5

# Load and format the dataset
dat = loader(dataset)
X = cbind(dat$X_numeric, dat$X_category)
y=dat$y

# Add some missing values
X_miss = MCAR(X, prop_added_missing)

# Perform multiple imputation
X_imputed = mice_imp(X_miss, m=n_imputations)

# Fit and predict on each filled dataset using a train/test split
predictions = multiple_prediction(X_imputed, y, pred_method = prediction_method, train_size = train_size, seed=seed)

