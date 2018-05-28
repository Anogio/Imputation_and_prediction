## Imports
library(tidyverse)

# Custom imports
source('dataloaders.R')
source('generate_missing.R')
source('imputation_methods.R')
source('prediction_methods.R')

seed = 42

dataset = 'iris'

dat = loader(dataset)
X = cbind(dat$X_numeric, dat$X_category)
y=dat$y

X_miss = MCAR(X, 0.05)

X_imputed = mice_imp(X_miss, m=5)

predictions = multiple_prediction(X_imputed, y, pred_method = 'rf', train_size = 0.5, seed=seed)
