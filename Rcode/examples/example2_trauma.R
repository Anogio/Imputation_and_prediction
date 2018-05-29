## Imports
library(tidyverse)
library(verification)

this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

# Custom imports
source('dataloaders.R')
source('generate_missing.R')
source('imputation_methods.R')
source('prediction_methods.R')

seed = 42
dataset = 'trauma'
prop_added_missing = 0
n_imputations = 5
prediction_method = 'rf'
train_size = 0.5

# Load and format the dataset
dat = loader(dataset)
###X = cbind(dat$X_numeric, dat$X_category)
X = dat$X_numeric
y=dat$y

# Add some missing values
X_miss = MCAR(X, prop_added_missing)

