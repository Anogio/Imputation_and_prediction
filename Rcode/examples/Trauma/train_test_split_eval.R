rocs = c()
eval.points = seq(0.01,0.99,0.02)

splitted = train_test_split(X_miss,y,0.7,seed=6)
spl_val = splitted$spl
X_eval = X[-spl_val,]
y_eval = y[-spl_val]

X_remain = X[spl_val,]
y_remain = y[spl_val]

for(rate in eval.points){
  print(rate)
  splitted = train_test_split(X_remain,y_remain,rate,seed=seed)
  spl = splitted$spl
  
  X_train = X_remain[spl,]
  y_train = y_remain[spl]
  
  X_test = X_eval
  y_test = y_eval
  
  m = colMeans(X_train, na.rm=T)
  for(i in 1:ncol(X_train)){
    X_train[is.na(X_train[i]),i] = m[i]
    X_test[is.na(X_test[i]),i] = m[i]
  }
    
  fitControl = trainControl(method = "none", classProbs = TRUE)
  fittedM = train(X_train, y_train,
                  method='rf', 
                  trControl=fitControl)
  y_pred = predict(fittedM, X_test, type='prob')[,2]
  
  rocs = c(rocs,roc.area(as.numeric(y_test)-1, y_pred)$A)
}

plot(eval.points*0.7, rocs)


###################
# Compare was to complete (split before or after)
scores.part = c()
scores.full = c()

ntry = 20
for(seed in 1:ntry){
  rate = 0.5
  imp.method = 'mice'
  splitted = train_test_split(X_miss,y,rate,seed=seed)
  spl = splitted$spl
  
  X_train = splitted$X_train
  X_test = splitted$X_test
  y_train = splitted$y_train
  y_test= splitted$y_test
  
  X_imput_full = impute(X_miss, m=1, method=imp.method)[[1]]
  X_imput_full_train = X_imput_full[spl,]
  X_imput_full_test = X_imput_full[-spl,]
  
  X_imput_part_train = impute(X_train, m=1, method=imp.method)[[1]]
  X_imput_part_test = impute(X_test, m=1, method=imp.method)[[1]]
  
  fitControl = trainControl(method = "none", classProbs = TRUE)
  fittedM.full = train(X_imput_full_train, y_train,
                  method='rf', 
                  trControl=fitControl)
  fittedM.part = train(X_imput_part_train, y_train,
                       method='rf', 
                       trControl=fitControl)
  
  y_pred.full = predict(fittedM.full, X_imput_full_test, type='prob')[,2]
  y_pred.part = predict(fittedM.part, X_imput_part_test, type='prob')[,2]
  
  
  scores.full = c(scores.full, roc.area(as.numeric(y_test)-1, y_pred.full)$A)
  scores.part = c(scores.part, roc.area(as.numeric(y_test)-1, y_pred.part)$A)
}

ggplot() + aes(x=1:ntry) + geom_point(aes(y=scores.full, color='full')) + geom_point(aes(y=scores.part, color='part'))
wilcox.test(scores.full, scores.part, alternative='g', paired=T, conf.int=T)
