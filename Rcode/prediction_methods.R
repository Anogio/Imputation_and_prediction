library(caret)

multiple_prediction <- function(X_MI, y, pred_method, train_size=0.5, seed=42){
  set.seed(seed)
  
  
  y_pred = list()
  inTraining = createDataPartition(y, p=train_size, list=FALSE)

  y_train = y[inTraining]
  y_test = y[-inTraining]
  for(i in range(1:length(X_MI))){
    dat_train = cbind(X_MI[[i]][inTraining, ] , y_train)
    dat_test = X_MI[[i]][-inTraining, ]
    
    fitControl <- trainControl(method = "none", classProbs = TRUE)
    fittedM = train(y_train~., data=dat_train,
                    method=pred_method, 
                    trControl=fitControl)
    
    y_pred[[i]] = predict(fittedM, dat_test, type='prob') 
  }
  
  return(y_pred=y_pred, y_true=y_test)
}