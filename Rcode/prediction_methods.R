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