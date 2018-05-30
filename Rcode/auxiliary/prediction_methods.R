library(caret)
library(xgboost)

multiple_prediction <- function(X_MI, y, pred_method, train_size=0.5, seed=42, spl=NULL){
  print(paste('Predicting response using method ', pred_method, ' on ', train_size*100, '% of the data as training set...', sep=''))
  y_pred = list()
  trained_predictors = list()
  
  splitted = train_test_split(X_MI,y, train_size=train_size, spl=spl, seed=seed)
  y_train = splitted$y_train
  y_test = splitted$y_test
  
  for(i in 1:length(X_MI)){
    print(paste('Processing imputed dataset', i))
    dat_train = cbind(splitted$X_train[[i]] , y_train)
    dat_test = splitted$X_test[[i]]
    
    fitControl = trainControl(method = "none", classProbs = TRUE)
    fittedM = train(y_train~., data=dat_train,
                    method=pred_method, 
                    trControl=fitControl)
    
    y_pred[[i]] = predict(fittedM, dat_test, type='prob') 
    trained_predictors[[i]] = fittedM
  }
  print('Done.')
  return(list(y_pred=y_pred, y_true=y_test, spl=splitted$spl, trained_predictors=trained_predictors))
}

saem_prediction <- function(X, y, train_size=0.5, seed=42, spl=NULL, printevery=50){
  print(paste('Predicting response using SAEM logistic regression on ', train_size*100, '% of the data as training set...', sep=''))
  source("../SAEM_Wei_Jiang/saem_model_selection_fct2.R", chdir = T)
  
  y = as.numeric(y) - 1 # Convert back to vector of 0 and 1s for saem
  
  splitted = train_test_split(X,y, train_size=train_size, spl=spl, seed=seed)
  y_train = splitted$y_train
  y_test = splitted$y_test
  X_train = splitted$X_train
  X_test = splitted$X_test
  
  list.saem.subset=miss.saem(data.matrix(X_train),1:ncol(X_train),y_train,maxruns=1000,tol_em=1e-7,
                             print_iter=TRUE,var_obs_cal=TRUE, printevery=printevery)
  beta.saem.train = list.saem.subset$beta
  se.saem.train = list.saem.subset$std_obs
  mu.saem = list.saem.subset$mu
  sig2.saem = list.saem.subset$sig2
  
  X_test1 =  data.matrix(X_test)
  rindic = as.matrix(is.na(X_test1))
  for(i in 1:dim(X_test1)[1]){
    if(sum(rindic[i,])!=0){
      miss_col = which(rindic[i,]==TRUE)
      x2 = X_test1[i,-miss_col]
      mu1 = mu.saem[miss_col]
      mu2 = mu.saem[-miss_col]
      sigma11 = sig2.saem[miss_col,miss_col]
      sigma12 = sig2.saem[miss_col,-miss_col]
      sigma22 = sig2.saem[-miss_col,-miss_col]
      sigma21 = sig2.saem[-miss_col,miss_col]
      mu_cond = mu1+sigma12 %*% solve(sigma22)%*%(x2-mu2)
      X_test1[i,miss_col] =mu_cond
    }
  }
  tmp <- as.matrix(cbind.data.frame(rep(1,dim(X_test1)[1]),X_test1)) %*% as.matrix(beta.saem.train) 
  pr <- 1/(1+(1/exp(tmp)))
  
  return(list(y_pred=pr, y_true=y_test, spl=splitted$spl, trained_param=list.saem.subset))
}


xgboost_prediction <- function(X, y, train_size=0.5, seed=42, spl=NULL, nrounds=30){
  print(paste('Predicting response using XGBoost on ', train_size*100, '% of the data as training set...', sep=''))
  source("../SAEM_Wei_Jiang/saem_model_selection_fct2.R", chdir = T)

  catData = which(sapply(X, is.factor))
  for(c in catData){
    X[,c] = as.numeric(X[,c])
  }
  X = as.matrix(X)
  y = as.numeric(y) - 1 # Convert back to vector of 0 and 1s for saem
  
  splitted = train_test_split(X,y, train_size=train_size, spl=spl, seed=seed)
  y_train = splitted$y_train
  y_test = splitted$y_test
  X_train = splitted$X_train
  X_test = splitted$X_test
  
  train.xgb <- xgboost(data = X_train, label = y_train, nrounds=nrounds, nthread = 4, objective = "binary:logistic", print_every_n = 50)
  pred <- predict(train.xgb, X_test)
  
  return(list(y_pred=pred, y_true=y_test, spl=splitted$spl, trained.predictor=train.xgb))
}

####################
# Auxiliary functions

# Method to pool the estimator from a list of prediction results for a binary response
pool_MI_binary <- function(preds){
  n_imputations = length(preds$y_pred)
  y_pred = matrix(NA, nrow=length(preds$y_true), ncol=n_imputations)
  for(i in 1:n_imputations){
    y_pred[,i] = preds$y_pred[[i]][,2]
  }
  
  return(apply(y_pred, 1, mean))
}

train_test_split <-  function(X,y, train_size=0.5, spl=NULL, seed=42){
  set.seed(seed)
  if(is.null(spl)){
    inTraining = createDataPartition(y, p=train_size, list=FALSE)
  }
  else{
    print('Existing train/test split provided. Ignoring train_size parameter.')
    inTraining = spl
  }
  
  y_train = y[inTraining]
  y_test = y[-inTraining]
  
  if(inherits(X, "list")){
    X_train = list()
    X_test = list()
    for(i in 1:length(X)){
      X_train[[i]] = X[[i]][inTraining,]
      X_test[[i]] = X[[i]][-inTraining,]
    }
  }
  else{
    X_train = X[inTraining,]
    X_test = X[-inTraining,]
  }
  return(list(X_train=X_train, y_train=y_train, X_test=X_test, y_test=y_test, spl=inTraining))
}
