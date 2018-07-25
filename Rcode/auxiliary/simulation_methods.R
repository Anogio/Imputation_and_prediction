library(norm)
library(parallel)
library(tidyverse)
library(pbapply)
library(mvtnorm)
library(caret)
library(sn)
#############################"
# X generators

X.basic.MVN = function(args){
  n. = args$n
  rho. = args$rho
  p. = args$p
  X = rmvnorm(n., sigma = (1-rho.)*diag(p.) + rho.)
  return(X)
}

# mvtnorm with two groups of variables
X.two.groups.MVN = function(args){
  n. = args$n
  p. = args$p
  rho. = args$rho
  unif.noise=args$unif.noise

  p2 = ceiling(p./3)
  p1 = p. - p2
  M1 = (1-rho.)*diag(p1) + rho.
  M2 = (1-rho.)*diag(p2) + rho.
  z12 = matrix(0, nrow=p1, ncol=p2)
  z21 = matrix(0, nrow=p2, ncol=p1)
  s = cbind(
    rbind(M1, z21),
    rbind(z12, M2)
  )
  X = rmvnorm(n., sigma=s)
  colnames(X) = paste('V', 1:p., sep='')
  return((X + matrix(runif(n.*p., -unif.noise/2, unif.noise/2), ncol=p.))*100)
}

X.random.mvn = function(args){
  n. = args$n
  p. = args$p
  A <- matrix(runif(p.^2)*2-1, ncol=p.)
  s <- t(A) %*% A
  return(
    rmvnorm(n., sigma=s)
  )
}

X.skew = function(args){
  n = args$n
  p = args$p
  rho = args$rho
  alpha = args$alpha
  A <- matrix(runif(p^2)*2-1, ncol=p)
  s <- (1-rho)*diag(p) + rho

  return(
    rmsn(n,Omega=s, alpha=alpha*(1+(1:p)/p), xi=rep(0,p))
  )
}

#############################
# y generators
y.regression = function(X, args){
  sigma_reg = args$sigma_reg
  beta = runif(ncol(X), 0.5, 1.5)
  #beta = beta/sum(beta)
  y = X %*% beta + rnorm(nrow(X), 0, sqrt(sigma_reg))
  return(list(X=X, y=y, beta=beta))
}

y.firstCol = function(X, args){
  return(list(X=X[,-1], y=X[,1]))
}
###########################
# Abalone Data
if(exists(aux.folder)){
  old = aux.folder
}else{
  old = './'
}
aux.folder = './'
source(paste(aux.folder,'dataloaders.R',sep=''), chdir = T)
aux.folder = old

X.load = function(args){
  dataset = args$dataset
  n.= args$n
  return(list(n=n., ds=dataset))
}

y.load = function(X, args){
  data_folder = '../../../Data/'
  dat_load = loader(X$ds, max_rows=X$n)
  return(
    list(X=as.matrix(dat_load$X_numeric), y=dat_load$y)
  )
}

X.abalone = function(args){args$dataset='abalone'; X.load(args)}
y.abalone = y.load

X.trauma = function(args){args$dataset='trauma'; X.load(args)}
y.trauma = function(X, args){
  data_folder = '../../../Data/'
  dat_load = loader(X$ds, max_rows=X$n)
  return(
    list(X=as.matrix(dat_load$X_numeric), y=as.numeric(dat_load$y)-1)
  )
}

#########################
# Split
train_test_split = function(X,y, train_prop){
  #intrain = base::sample(nrow(X), ceiling(train_prop*nrow(X)))
  intrain = createDataPartition(y, p=train_prop, list=FALSE)
  return(list(
    X.train = X[intrain,],
    X.test = X[-intrain,],
    y.train = y[intrain],
    y.test = y[-intrain],
    inTrain=intrain
  ))
}

#########################
# Prediction
pred.lin.train = function(X.train, y.train, weights=NULL){
  dat = data.frame(y=y.train, X=I(X.train))
  return(
    lm(y~X, data = dat, weights=weights)
  )
}

pred.lin.predict = function(model, X.test){
  return(predict(model,data.frame(X=I(X.test))))
}

reg.lin = list(train=pred.lin.train, predict=pred.lin.predict)

pred.logit.train = function(X.train, y.train, weights=NULL){
  dat_train = data.frame(y=y.train, X=I(X.train))
  return(
    glm(y ~ .,family=binomial(link='logit'),data=dat_train, weights=weights)
  )
}

pred.logit.predict = function(model, X.test){
  return(predict(model,data.frame(X=I(X.test)), type='response'))
}

reg.logit = list(train=pred.logit.train, predict=pred.logit.predict)

pred.rf.train = function(X.train, y.train){
  fitControl = trainControl(method = "none", classProbs = F)
  fittedM = train(X.train, y.train,
                  method='rf',
                  trControl=fitControl)
  return(fittedM)
}

pred.rf.predict = function(model, X.test){
  return(
    predict(model, X.test)
  )
}
reg.rf = list(train=pred.rf.train, predict=pred.rf.predict)

######
# Imputation
imp.mean.train = function(X){
  return(colMeans(X, na.rm = T))
}
imp.mean.estim = function(mu, X){
  p = ncol(X)
  for(i in 1:p){
    X[is.na(X[,i]),i] = mu[i]
  }
  return(X)
}

################################
# Evaluation
################################
# Evaluation
evaluate.one.run = function(X.gen, y.gen, miss.gen, splitter, imputer, regressor){
  X = X.gen()
  y.g = y.gen(X)
  y = y.g$y
  X = y.g$X

  X_f = X
  #X = miss.gen(X)

  grouped.imp.train = (imputer$train)(X)
  grouped.imp = (imputer$estim)(grouped.imp.train, X)

  spl = splitter(X,y)
  rm(X)
  rm(y)
  X.train = (miss.gen$train)(spl$X.train)
  X.test = (miss.gen$test)(spl$X.test)
  y.train = spl$y.train
  y.test = spl$y.test
  datasets = list()

  correct.imp.train = (imputer$train)(X.train)
  X.train.correct = (imputer$estim)(correct.imp.train, X.train)
  X.test.correct = (imputer$estim)(correct.imp.train, X.test)
  datasets$correct = list(train=X.train.correct, test=X.test.correct)

  separate.imp.train = (imputer$train)(X.test)
  X.train.separate = datasets$correct$train
  X.test.separate = (imputer$estim)(separate.imp.train, X.test)
  datasets$separate = list(train=X.train.separate, test=X.test.separate)

  withY.imp.train = (imputer$train)(cbind(X.train, y.train))
  X.train.withY = (imputer$estim)(withY.imp.train, cbind(X.train, y.train))[,1:ncol(X.train)]
  withY.imp.train2 = (imputer$train)(X.train.withY) # Refit with the imputed data
  X.test.withY = (imputer$estim)(withY.imp.train2, X.test)
  datasets$withY = list(train=X.train.withY, test=X.test.withY)

  mean.imp.train = imp.mean.train(X.train)
  X.train.mean = imp.mean.estim(mean.imp.train, X.train)
  X.test.mean = imp.mean.estim(mean.imp.train, X.test)
  datasets$mean = list(train=X.train.mean, test=X.test.mean)

  #X.train.grouped = (imputer$estim)(grouped.imp.train, X.train)
  #X.test.grouped = (imputer$estim)(grouped.imp.train, X.test)
  X.train.grouped = grouped.imp[spl$inTrain,]
  X.test.grouped = grouped.imp[-spl$inTrain,]
  datasets$grouped = list(train=X.train.separate, test=X.test.separate)

  datasets$fullData = list(train=X_f[spl$inTrain,], test=X_f[-spl$inTrain,])
  datasets$fullTrain = list(train=X_f[spl$inTrain,], test=X.test.correct)
  datasets$fullTest = list(train=X.train.correct, test=X_f[-spl$inTrain,])

  regressors.fit = lapply(datasets,
                          function(x) {(regressor$train)(x$train, y.train)})

  predictions = lapply(names(datasets), function(x){(regressor$predict)(regressors.fit[[x]], datasets[[x]]$test)})
  names(predictions) = names(datasets)
  predictions$trueBeta = as.numeric(X.test.correct %*% matrix(1:ncol(X.train), ncol=1))


  predictions_MI = matrix(NA, nrow=length(y.test), ncol=m)
  pre = prelim.norm(X.test)
  for(i in 1:m){
    X.test.MI = imp.norm(pre, correct.imp.train, X.test)
    predictions_MI[,i] = (regressor$predict)(regressors.fit$correct, X.test.MI)
  }
  predictions$MI_testOnly = rowMeans(predictions_MI)

  errors = lapply(predictions, function(x){mean((x-y.test)^2)})

  return(errors)
}

evaluate.S.run = function(S, X.gen, y.gen, miss.gen, splitter, imputer, regressor, evaluator=evaluate.one.run, do.parallel=T, no_cores=4){
  MSE.correct = c()
  MSE.grouped = c()
  MSE.separate = c()

  f= function(i){
    rngseed(seed+i)
    evaluator(X.gen, y.gen, miss.gen, splitter, imputer, regressor)
  }
  if(do.parallel){
    cl <- makeCluster(no_cores, type='FORK')
    clusterSetRNGStream(cl, seed)
    z = pblapply(cl=cl, X=1:S, FUN=f)
    stopCluster(cl)
  }
  else{
    z = pblapply(X=1:S, FUN=f)
  }

  zz <- lapply(z, `[`, names(z[[1]]))
  res = apply(do.call(rbind, zz), 2, as.list)

  #for(i in 1:S){
  #  r = evaluate.one.run(X.gen, y.gen, miss.gen, splitter, imputer, regressor, imp.MI)
  #  MSE.correct = c(MSE.correct, r$correct)
  #  MSE.grouped = c(MSE.grouped, r$grouped)
  #  MSE.separate = c(MSE.separate, r$separate)
  #}
  #return(list(correct=MSE.correct, grouped=MSE.grouped,separate=MSE.separate))
  return(lapply(res, unlist))
}

evaluate.S.run.general = function(S, args, evaluator=evaluate.one.run, do.parallel=T, no_cores=4){
  f= function(i){
    rngseed(seed+i)
    evaluator(args)
  }
  if(do.parallel){
    cl <- makeCluster(no_cores, type='FORK')
    clusterSetRNGStream(cl, seed)
    z = pblapply(cl=cl, X=1:S, FUN=f)
    stopCluster(cl)
  }
  else{
    z = pblapply(X=1:S, FUN=f)
  }

  zz <- lapply(z, `[`, names(z[[1]]))
  res = apply(do.call(rbind, zz), 2, as.list)

  return(lapply(res, unlist))
}

expand_args = function(argsList){
  args.df = expand.grid(argsList)
  return(
    split(args.df, seq(nrow(args.df)))
  )
}

evaluate.S.run.multiArg = function(S, argsList, evaluator=evaluate.one.run, do.parallel=T, no_cores=4){
  args.df = expand.grid(argsList)
  argsList = expand_args(argsList)

  f= function(args){
    rngseed(seed)
    evaluate.S.run.general(S, args, evaluator, do.parallel=F) %>% lapply(mean)
  }
  if(do.parallel){
    cl <- makeCluster(no_cores, type='FORK')
    clusterSetRNGStream(cl, seed)
    z = pblapply(cl=cl, X=argsList, FUN=f)
    stopCluster(cl)
  }
  else{
    z = pblapply(X=argsList, FUN=f)
  }

  zz <- lapply(z, `[`, names(z[[1]]))
  res = apply(do.call(rbind, zz), 2, as.list)

  return(
    cbind(lapply(res, unlist) %>% as.data.frame(), args.df)
  )
}
