## Imports
library(tidyverse)
library(verification)

# Custom imports
aux.folder = '../auxiliary/'
source(paste(aux.folder,'dataloaders.R',sep=''), chdir = T)
source(paste(aux.folder,'generate_missing.R',sep=''), chdir = T)
source(paste(aux.folder,'imputation_methods.R',sep=''), chdir = T)
source(paste(aux.folder,'prediction_methods.R',sep=''), chdir = T)

seed = 42
dataset = 'trauma'
prop_added_missing = 0
n_imputations = 5
prediction_method = 'rf'
train_size = 0.5
max_rows = 1000

#######################
# Preparation

# Load and format the dataset
dat = loader(dataset, max_rows, seed)
###X = cbind(dat$X_numeric, dat$X_category)
X = dat$X_numeric
X = X %>% dplyr::select(Age, Glasgow.moteur.initial, FC.max, Hemocue.init, Remplissage.total.cristalloides, Remplissage.total.colloides,
                        SD.min, SD.SMUR)
y=dat$y



# Add some missing values
X_miss = MCAR(X, prop_added_missing)


############################
# SAEM prediction
prediction_SAEM = saem_prediction(X_miss, y, train_size=train_size, seed=seed, printevery=50)

####################
# Multiple imputation
X_imputed = mice_imp(X_miss, m=n_imputations)
X_fillmean = X_miss
for(i in 1:ncol(X_fillmean)){
  c = X_fillmean[,i]
  X_fillmean[is.na(c),i] = mean(c, na.rm = T)
}

# Fit and predict on each filled dataset using the same train/test split as with the SAEM (ie same seed)
predictions = multiple_prediction(X_imputed, y, pred_method = prediction_method, 
                                  train_size = train_size, seed=seed, spl=prediction_SAEM$spl)
prediction_meanfill = multiple_prediction(list(X_fillmean), y, pred_method = prediction_method, 
                                          train_size = train_size, seed=seed,spl=prediction_SAEM$spl)


##################
# Result aggregation
y_pred = matrix(NA, nrow=length(predictions$y_true), ncol=n_imputations)
for(i in 1:n_imputations){
  y_pred[,i] = predictions$y_pred[[i]][,2]
}
results = data.frame(row.names=1:nrow(y_pred))
results$MI_estimate = apply(y_pred, 1, mean)
results$SAEM_estimate = prediction_SAEM$y_pred
results$meanImp_estimate = prediction_meanfill$y_pred[[1]][,2]
results$true = as.numeric(predictions$y_true == 'X1')

roc.plot(results$true, results[,c(1,2,3)], legend=T, leg.text=c("MI + randomforest predition", 'SAEM prediction', "Mean imputation + randomforest prediction"))
