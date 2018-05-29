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
dataset = 'iris'
prop_added_missing = 0.1
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
X_imputed_nomiss = mice_imp(X, m=1)

# Fit and predict on each filled dataset using a train/test split
predictions = multiple_prediction(X_imputed, y, pred_method = prediction_method, train_size = train_size, seed=seed)
predictions_nomiss = multiple_prediction(X_imputed_nomiss, y, pred_method = prediction_method, train_size = train_size, seed=seed)

# Aggregate results
y_pred = matrix(NA, nrow=length(predictions$y_true), ncol=n_imputations)
for(i in 1:n_imputations){
  y_pred[,i] = predictions$y_pred[[i]][,2]
}
results = data.frame(row.names=1:nrow(y_pred))
results$estimator = apply(y_pred, 1, mean)
dev = apply(y_pred, 1, sd)
results$upper = results$estimator + dev*qnorm(0.95)
results$lower = results$estimator - dev*qnorm(0.95)
results$true = as.numeric(predictions$y_true == 'X1')
results$nomiss = predictions_nomiss$y_pred[[1]][,2]
results = results %>% arrange(nomiss)

ggplot(results) + aes(x=1:nrow(results)) + geom_point(aes(y=estimator, color='Estimation with added missing data')) +
  geom_line(aes(y=upper)) + geom_line(aes(y=lower)) +
  geom_point(aes(y=true)) + geom_point(aes(y=nomiss, color='Estimation with no added missing data')) +
  scale_color_manual(values=c('red','blue'))

roc.plot(results$true, results[,c(5,1)], legend=T)
