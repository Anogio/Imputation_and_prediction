library(tidyverse)
library(denoiseR)
library(mvtnorm)
library(norm)
library(parallel)

######################################"
seed = ceiling(runif(1,1e5,1e6))
print(seed)
miss_prop = 0.3
train_prop = 0.4

# Generate dataset
p = 6
n = 1000

# MVN parameters
rho = 0.3

# LRsim parameters
k = ceiling(p/3)
SNR = 0.2

# Regression response noise
sigma_noise = 0.2

########
# Method 1.1: mvtnorm

X.basic.MVN = function(n.=n, p.=p, rho.=rho){
  X = rmvnorm(n., sigma = (1-rho.)*diag(p.) + rho.)
  return(X)
}

# Method 1.2: mvtnorm with two groups of variables
X.two.groups.MVN = function(n.=n, p.=p, rho.=rho){
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
  return(X)
}

# Method 2 : LRsim
X.LR = function(n.=n, p.=p, k.=k, SNR.=SNR){
  X = LRsim(n., p., k., SNR.)$X
  return(X)
}

#######################################
# Selecting the response

###############
# Method 1: choosing a column
y.first.col = function(X){
  y = X[,1]
  X = X[,-1]
  return(list(X=X, y=y))
}
#############
# Method 2: Regression model
y.regression = function(X, sigma_n=sigma_noise){
  beta = seq(p)
  y = X %*% beta + rnorm(n, 0, sigma_n)
  return(list(X=X, y=y))
}

##########
# Method 3: Square regression
y.regression.square = function(X, sigma_n=sigma_noise){
  beta = seq(p)
  X.2 = X
  X.2[,1] = X.2[,1]^2
  y = X.2 %*% beta + rnorm(n, 0, sigma_n)
  return(list(X=X, y=y))
}

#####################################
# Missing data

##########
# MCAR
MCAR <- function(X, miss_prop.=miss_prop){
  n = nrow(X)
  m = ncol(X)
  missing = matrix(runif(n*m), nrow=n, ncol=m) <= miss_prop.
  X[missing] = NA
  return(X)
}

MCAR.noEmptyLines <- function(X, miss_prop.=miss_prop){
  n = nrow(X)
  m = ncol(X)
  missing = matrix(runif(n*m), nrow=n, ncol=m) <= miss_prop.
  missing[rowSums(missing)==ncol(X), 1] = F
  X[missing] = NA
  return(X)
}

########
# MAR
MAR.threshold.1var = function(X, miss_prop.=miss_prop){
  q = quantile(X[,2], miss_prop.)
  X[,1][X[,2]<q] = NA
  X[,-1] = MCAR(X[,-1], miss_prop.)
  return(X)
}

MAR.threshold.allvar = function(X, miss_prop. = miss_prop){
  p = ncol(X)
  X_miss = X
  for(i in 1:(p-1)){
    q = quantile(X[,i+1], miss_prop.)
    X_miss[,i][X[,i+1]<q] = NA
  }
  q = quantile(X[,1], miss_prop.)
  X_miss[,p][X[,1]<q] = NA
  return(X_miss)
}


#######################################
# Imputation


######
# Mean imputation
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

imp.mean = list(train=imp.mean.train, estim=imp.mean.estim)
#########
# MVN fit
imp.mvnorm.train = function(X){
  # Must run *rngseed* at least once before using
  pre <- prelim.norm(as.matrix(X))
  thetahat <- em.norm(pre)
  return(thetahat)
}

imp.mvnorm.estim = function(thetahat.train, X){
  pre = prelim.norm(as.matrix(X))
  res = imp.norm(pre, thetahat.train, X)
  if(any(is.na(res))){
    stop('There are still NA values in the imputation. Have you initialized rngseed?')
  }
  return(res)
}
imp.mvnorm = list(train=imp.mvnorm.train, estim=imp.mvnorm.estim)

############
# Alternative MVN fit
to_matrix = function(x, horiz){
  if(!is.null(dim(x))){
    return(x)
  }
  else{
    if(!horiz){
      return(as.matrix(x))
    }
    else{
      return(t(as.matrix(x)))
    }
  }
}

estimate.1row = function(row, s, m){
  miss_col = is.na(row)
  nmiss = sum(miss_col)
  if(nmiss>0){
    mu.miss = m[miss_col]
    mu.obs = m[!miss_col]
    sigma.miss = s[miss_col,miss_col]
    sigma.miss.obs = to_matrix(s[miss_col,!miss_col], horiz=nmiss==1)
    sigma.obs = s[!miss_col,!miss_col]
    mu_cond = mu.miss + sigma.miss.obs %*% solve(sigma.obs) %*% (row[!miss_col] - mu.obs)
    #sigma_cond = sigma.miss - sigma.miss.obs %*% solve(sigma.obs) %*% t(sigma.miss.obs)

   # row[miss_col] = rmvnorm(1, mean=mu_cond, sigma=sigma_cond)
    row[miss_col] = mu_cond
  }
  return(row)
}

impute_X = function(thetahat, X){
  #print(dim(X))
  pre <- prelim.norm(as.matrix(X))
  #thetahat <- em.norm(pre)
  params = getparam.norm(pre,thetahat)
  sigma = params$sigma
  mu = params$mu
  X = t(apply(X, 1, partial(estimate.1row, s=sigma, m=mu)))
  #print(dim(X))
  #print('')
  return(X)
}

imp.mvnorm = list(train=imp.mvnorm.train, estim=impute_X)
######################################
# Train/validation split
train_test_split = function(X,y, train_prop.=train_prop){
  intrain = base::sample(n, ceiling(train_prop.*n))
  return(list(
    X.train = X[intrain,],
    X.test = X[-intrain,],
    y.train = y[intrain],
    y.test = y[-intrain],
    inTrain=intrain
  ))
}

####################################
# Prediction

pred.lin.train = function(X.train, y.train){
  dat = data.frame(y=y.train, X=I(X.train))
  return(
    lm(y~X, data = dat)
  )
}

pred.lin.predict = function(model, X.test){
  return(predict(model,data.frame(X=I(X.test))))
}

reg.lin = list(train=pred.lin.train, predict=pred.lin.predict)

################################
# Evaluation
evaluate.one.run = function(X.gen, y.gen, miss.gen, splitter, imputer, regressor){
  X = X.gen()
  y.g = y.gen(X)
  y = y.g$y
  X = y.g$X

  X_f = X
  X = miss.gen(X)

  grouped.imp.train = (imputer$train)(X)

  spl = splitter(X,y)
  rm(X)
  rm(y)
  X.train = spl$X.train
  X.test = spl$X.test
  y.train = spl$y.train
  y.test = spl$y.test

  separate.imp.train = (imputer$train)(X.test)
  correct.imp.train = (imputer$train)(X.train)
  mean.imp.train = imp.mean.train(X.train)

  X.train.correct = (imputer$estim)(correct.imp.train, X.train)
  X.test.correct = (imputer$estim)(correct.imp.train, X.test)
  impute.error.correct = sum(colSums(X.test.correct-X_f[-spl$inTrain,])^2)/sum(colSums(is.na(X.test)))

  X.train.grouped = (imputer$estim)(grouped.imp.train, X.train)
  X.test.grouped = (imputer$estim)(grouped.imp.train, X.test)

  X.train.separate = X.train.correct
  X.test.separate = (imputer$estim)(separate.imp.train, X.test)

  X.train.mean = imp.mean.estim(mean.imp.train, X.train)
  X.test.mean = imp.mean.estim(mean.imp.train, X.test)

  correct.reg.train = (regressor$train)(X.train.correct, y.train)
  grouped.reg.train = (regressor$train)(X.train.grouped, y.train)
  separate.reg.train = (regressor$train)(X.train.separate, y.train)
  mean.reg.train = (regressor$train)(X.train.mean, y.train)
  fullData.reg.train = (regressor$train)(X_f[spl$inTrain,], y.train)

  y.pred.correct = (regressor$predict)(correct.reg.train, X.test.correct)
  y.pred.grouped = (regressor$predict)(grouped.reg.train, X.test.grouped)
  y.pred.separate = (regressor$predict)(separate.reg.train, X.test.separate)
  y.pred.mean = (regressor$predict)(mean.reg.train, X.test.mean)
  y.pred.fullData = (regressor$predict)(fullData.reg.train, X_f[-spl$inTrain,])

  err.correct = mean((y.pred.correct-y.test)^2)
  err.grouped = mean((y.pred.grouped-y.test)^2)
  err.separate = mean((y.pred.separate-y.test)^2)
  err.mean = mean((y.pred.mean-y.test)^2)
  err.full = mean((y.pred.fullData-y.test)^2)

  print(err.mean)
  return(list(correct=err.correct, grouped=err.grouped, separate=err.separate, full=err.full, mean=err.mean,
              impute.error.correct=impute.error.correct))
}

evaluate.S.run = function(S, X.gen, y.gen, miss.gen, splitter, imputer, regressor){
  MSE.correct = c()
  MSE.grouped = c()
  MSE.separate = c()
  cl <- makeCluster(no_cores, type='FORK')
  clusterSetRNGStream(cl, seed)
  f= function(i){
    rngseed(seed+i)
    evaluate.one.run(X.gen, y.gen, miss.gen, splitter, imputer, regressor)
    }
  z = parLapply(cl, 1:S, f)
  stopCluster(cl)

  zz <- lapply(z, `[`, names(z[[1]]))
  res = apply(do.call(rbind, zz), 2, as.list)

  #for(i in 1:S){
  #  r = evaluate.one.run(X.gen, y.gen, miss.gen, splitter, imputer, regressor)
  #  MSE.correct = c(MSE.correct, r$correct)
  #  MSE.grouped = c(MSE.grouped, r$grouped)
  #  MSE.separate = c(MSE.separate, r$separate)
  #}
  #return(list(correct=MSE.correct, grouped=MSE.grouped,separate=MSE.separate))
  return(list(correct=unlist(res$correct), grouped=unlist(res$grouped),separate=unlist(res$separate),
              full=unlist(res$full), mean=unlist(res$mean),
              impError.correct=unlist(res$impute.error.correct)))
}

######################################################################################
# Execution
X.gen = function(){X.LR() + X.two.groups.MVN()}
y.gen = y.regression
miss.gen = MCAR.noEmptyLines
splitter = train_test_split
imputer = imp.mvnorm
regressor = reg.lin
nSim = 300

burnIn = 200

res = evaluate.S.run(nSim, X.gen, y.gen, miss.gen, splitter, imputer, regressor)

#res %>% lapply(cummean) %>% as.data.frame() %>% slice((burnIn+1):nSim) %>% mutate(s=(burnIn+1):nSim) %>% gather('method', 'error', -s) %>%
#  ggplot() + aes(x=s, y=error, color=method) + geom_line()

res %>% as.data.frame() %>% gather('method', 'error', -impError.correct) %>% ggplot() + aes(x=method, y=error, color=method) + geom_boxplot()

ggplot(res %>% as.data.frame()) + aes(x=impError.correct, y=correct) + geom_point()
cor(res$correct, res$impError.correct)


####################################
# Multiple run parameters
nList = c(500, 1000)
#nList = c(100,200)

rhoList= c(0.3, 0.8)
#rhoList = c(0.1, 0.2)
allRes = NULL

for(n in nList){
  cat('n=',n)
  for(rho in rhoList){
    cat('rho=',rho)
    #X.gen = partial(X.basic.MVN, n=n, rho=rho)
    X.gen = function(){X.two.groups.MVN(n.=n, rho.=rho)}
    r = as.data.frame(
      evaluate.S.run(nSim, X.gen, y.gen, miss.gen, splitter, imputer, regressor)
    )
    r$n = n
    r$rho=rho
    if(is.null(allRes)){
      allRes = r
    }
    else{
      allRes = rbind(allRes,r)
    }
  }
}

allRes %>% as.data.frame() %>%
  gather('method', 'error', -c(impError.correct, rho,n, full)) %>%
  mutate(method = factor(method, levels=levels(as.factor(method))[c(2,1,4,3)])) %>%
  ggplot() + aes(x=method, y=error, color=method) + geom_boxplot() + facet_grid(n~rho, scales='free_y')








