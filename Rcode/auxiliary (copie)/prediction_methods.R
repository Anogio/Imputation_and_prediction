library(caret)
library(xgboost)
library(MLmetrics)

source("../SAEM_Wei_Jiang/saem_model_selection_fct2.R", chdir = T)

saem_prediction <- function(X, y, train_size=0.5, seed=42, spl=NULL, printevery=50){
  print(paste('Predicting response using SAEM logistic regression on ', train_size*100, '% of the data as training set...', sep=''))

  y = as.numeric(y) - 1 # Convert back to vector of 0 and 1s for saem

  splitted = train_test_split_MI(X,y, train_size=train_size, spl=spl, seed=seed)
  y_train = splitted$y_train
  y_test = splitted$y_test
  X_train = splitted$X_train

  list.saem.subset=miss.saem(data.matrix(X_train),1:ncol(X_train),y_train,maxruns=1000,tol_em=1e-4,
                             print_iter=TRUE,var_obs_cal=TRUE, printevery=printevery)
  beta.saem.train = list.saem.subset$beta
  se.saem.train = list.saem.subset$std_obs
  mu.saem = list.saem.subset$mu
  sig2.saem = list.saem.subset$sig2

  X_1 =  data.matrix(X)
  rindic = as.matrix(is.na(X_1))
  for(i in 1:dim(X_1)[1]){
    if(sum(rindic[i,])!=0){
      miss_col = which(rindic[i,]==TRUE)
      x2 = X_1[i,-miss_col]
      mu1 = mu.saem[miss_col]
      mu2 = mu.saem[-miss_col]
      sigma11 = sig2.saem[miss_col,miss_col]
      sigma12 = sig2.saem[miss_col,-miss_col]
      sigma22 = sig2.saem[-miss_col,-miss_col]
      sigma21 = sig2.saem[-miss_col,miss_col]
      mu_cond = mu1+sigma12 %*% solve(sigma22)%*%(x2-mu2)
      X_1[i,miss_col] =mu_cond
    }
  }
  tmp <- as.matrix(cbind.data.frame(rep(1,dim(X_1)[1]),X_1)) %*% as.matrix(beta.saem.train)
  pr <- 1/(1+(1/exp(tmp)))

  return(list(y_pred=pr, y_true=y, spl=splitted$spl, trained_param=list.saem.subset))
}

####################
# Auxiliary functions

train_test_split_MI <-  function(X,y, train_size=0.5, spl=NULL, seed=42){
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

weighted_log_loss <- function(y_pred, y_true, y_train=NULL){
  if(is.null(y_train)){
    w0 = 1/2
    w1 = 1/2
  }
  else{
    w0 = mean((as.numeric(as.factor(y_train))-1)==1)
    w1 = 1-w0
  }
  y_true = as.numeric(as.factor(y_true)) - 1
  y_pred = as.numeric(y_pred)
  y_pred[y_pred==0] = 1e-10
  y_pred[y_pred==1] = 1-1e-10
  return(
    -2 * mean(y_true * log(y_pred) * w1 + (1-y_true) * log(1-y_pred) * w0)
  )
}

oversample <- function(X,y, ratio=2){
  X_pos = X[(as.numeric(as.factor(y))-1) == 1,]
  n_pos = nrow(X_pos)

  samples = base::sample(n_pos, n_pos*ratio, replace=T)
  return(X_pos[samples,])
}

metric_best_separation <- function(y_pred, y_true, positive_weighting=10){
  w0 = 1/(1+positive_weighting)
  w1 = positive_weighting/(1+positive_weighting)

  y_true = as.numeric(as.factor(y_true)) - 1
  y_pred = as.numeric(y_pred)

  best_loss = Inf

  for(thresh in c(min(y_pred-1),sort(unique(y_pred)), max(y_pred+1))){
    FP = sum(y_pred>=thresh & y_true==0)
    FN = sum(y_pred<thresh & y_true==1)
    loss.val = w1 * FN + w0 * FP

    if(loss.val<best_loss){
      best_loss = loss.val
      best_thresh = thresh
    }
  }
  return(list(val=best_loss/length(y_pred)*100, best.threshold=best_thresh))
}

metric_best_separation_fixedThr <- function(y_pred, y_true, positive_weighting=10, threshold=0.5){
  w0 = 1/(1+positive_weighting)
  w1 = positive_weighting/(1+positive_weighting)
  
  y_true = as.numeric(as.factor(y_true)) - 1
  y_pred = as.numeric(y_pred)
  
  best_loss = Inf
  
  thresh = threshold
  FP = sum(y_pred>=thresh & y_true==0)
  FN = sum(y_pred<thresh & y_true==1)
  loss.val = w1 * FN + w0 * FP
  
  if(loss.val<best_loss){
    best_loss = loss.val
    best_thresh = thresh
  }

  return(list(val=best_loss/length(y_pred)*100, best.threshold=best_thresh))
}
