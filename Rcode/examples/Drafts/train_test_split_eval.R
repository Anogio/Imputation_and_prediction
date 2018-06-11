## Main point of this file:
# First try to see how much data we need to get good performance. This is important because maybe
# if there is a lot more data than needed missing value will not be as much of an issue for the algo
# so we can tryto train it with the bare minimum data for good performance and see if that changes anything.

# Second, compare between filling the data before or after the split. This is similar to titanic_fulldata_analysis.R
# but here the idea is to do multiple runs with different splits and test whether the performance varies between the two
# methods in a significant way (whereas in that other file we test lots of methods on just one split)

rocs = c()
eval.points = seq(0.01,0.99,0.02)

splitted = train_test_split(X_miss,y,0.7,seed=6)
spl_val = splitted$spl
X_eval = X_miss[-spl_val,]
y_eval = y[-spl_val]

X_remain = X_miss[spl_val,]
y_remain = y[spl_val]

for(rate in eval.points){
  print(rate)
  splitted = train_test_split(X_remain,y_remain,rate,seed=seed)
  spl = splitted$spl

  X_train = X_remain[spl,]
  y_train = y_remain[spl]

  X_test = X_eval
  y_test = y_eval

  for(i in 1:ncol(X_train)){
    c = X_train[,i]
    if(!is.factor(c)){
      m = mean(c, na.rm=T)
      X_train[is.na(X_train[i]),i] = m
      X_test[is.na(X_test[i]),i] = m
    }
    else{
      tt = table(c)
      most.freq = names(tt[which.max(tt)])
      X_train[is.na(X_train[i]),i] = most.freq
      X_test[is.na(X_test[i]),i] = most.freq
    }
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
# Compare ways to complete (split before or after)
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

ggplot() + aes(x=1:ntry) + geom_line(aes(y=scores.full, color='full')) + geom_line(aes(y=scores.part, color='part'))
wilcox.test(scores.full, scores.part, alternative='g', paired=T, conf.int=T)
