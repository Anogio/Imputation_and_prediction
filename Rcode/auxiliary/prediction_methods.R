library(caret)

multiple_prediction <- function(X_MI, y, pred_method, train_size=0.5, seed=42){
  print(paste('Predicting response using method ', pred_method, ' on ', train_size*100, '% of the data as training set...', sep=''))
  set.seed(seed)
  
  y_pred = list()
  inTraining = createDataPartition(y, p=train_size, list=FALSE)

  y_train = y[inTraining]
  y_test = y[-inTraining]
  for(i in 1:length(X_MI)){
    print(paste('Processing imputed dataset', i))
    dat_train = cbind(X_MI[[i]][inTraining, ] , y_train)
    dat_test = X_MI[[i]][-inTraining, ]
    
    fitControl <- trainControl(method = "none", classProbs = TRUE)
    fittedM = train(y_train~., data=dat_train,
                    method=pred_method, 
                    trControl=fitControl)
    
    y_pred[[i]] = predict(fittedM, dat_test, type='prob') 
  }
  print('Done.')
  return(list(y_pred=y_pred, y_true=y_test))
}

saem_prediction <- function(X, y, train_size=0.5, seed=42){
  print(paste('Predicting response using SAEM logistic regression on ', train_size*100, '% of the data as training set...', sep=''))
  source("../SAEM_Wei_Jiang/saem_model_selection_fct2.R", chdir = T)
  set.seed(seed)
  
  y = as.numeric(y) - 1 # Convert back to vector of 0 and 1s for saem
  
  inTraining = createDataPartition(y, p=train_size, list=FALSE)
  y_train = y[inTraining]
  y_test = y[-inTraining]
  X_train = X[inTraining,]
  X_test = X[-inTraining,]
  
  list.saem.subset=miss.saem(data.matrix(X_train),1:ncol(X_train),y_train,maxruns=100,tol_em=1e-7,print_iter=TRUE,var_obs_cal=TRUE)
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
  
  return(list(y_pred=pr, y_true=y_test))
}