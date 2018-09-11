library(caret)
library(xgboost)
library(MLmetrics)

source("../SAEM_Wei_Jiang/saem_model_selection_fct2.R", chdir = T)

saem_prediction <- function(X, y, seed, spl, printevery=50){
  # A function that trains a SAEM model on the training data and predicts the whole dataset
  # X,y: covariates and response
  # seed: random seed
  # spl: a vector of positions for the lines of the training data
  # printevery: a verbosity parameter to print iterations of SAEM
  set.seed(seed)
  
  y = as.numeric(y) - 1 # Convert back to vector of 0 and 1s for saem

  y_train = y[spl]
  X_train = X[spl,]

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

  return(list(y_pred=pr, y_true=y, spl=spl, trained_param=list.saem.subset))
}

metric_best_separation <- function(y_pred, y_true, positive_weighting){
  # A metric to evaluate traumabase predictions
  # Returns the lowest achievable loss by setting a threshold to predict a positive
  # positive_weighting: the cost of missing a positive case in the prediction (false negative)
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
