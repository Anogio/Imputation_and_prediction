library(norm)

train.MI_mvnorm = function(X_train){
  # Must run *rngseed* at least once before using
  pre <- prelim.norm(as.matrix(X_train))
  thetahat <- em.norm(pre)
  return(thetahat)
}

impute.MI_mvnorm = function(thetahat.train, X, m=5){
  estimations = list()
  pre = prelim.norm(as.matrix(X))
  for(i in 1:m){
    print(i)
    estimations[[i]] = imp.norm(pre, thetahat.train, X)
  }
  if(any(is.na(estimations[[1]]))){
    stop('There are still NA values in the imputation. Have you initialized rngseed?')
  }
  return(estimations)
}

train.MI_mvnorm_boostrap = function(X_train, m=5){
  # Must run *rngseed* at least once before using
  thetas = list()
  for(i in 1:m){
    BS_indices = sample(1:nrow(X_train), nrow(X_train), replace = T)
    X_BS = X_train[BS_indices,]
    pre <- prelim.norm(as.matrix(X_BS))
    thetas[[i]] <- em.norm(pre)
  }
  return(thetas)
}

impute.MI_mvnorm_bootstrap = function(thetahat.train, X){
  estimations = list()
  pre = prelim.norm(as.matrix(X))
  for(theta in thetahat.train){
    estimations[[i]] = imp.norm(pre, theta, X)
  }
  if(any(is.na(estimations[[1]]))){
    stop('There are still NA values in the imputation. Have you initialized rngseed?')
  }
  return(estimations)
}
rngseed(1204)

inTraining = createDataPartition(y, p=0.3, list=F)
X_train = X_m[inTraining,]
X_test = X_m[-inTraining,]

trained = train.MI_mvnorm(X_train)
train.predict = impute.MI_mvnorm(trained, X_train, m=1)[[1]]
test.predict = impute.MI_mvnorm(trained, X_test, m=1)[[1]]

# Compare running time of the two methods: the joint one is faster
t0 = as.numeric(Sys.time())
imp.joint.train = train.MI_mvnorm(X_m)
imp.joint.imp = impute.MI_mvnorm(imp.joint.train, X_m, m=30)
t1 = as.numeric(Sys.time())
imp.mice = mice(X_m, m=30, method='norm')
t2 = as.numeric(Sys.time())

print(t1 - t0)
print(t2 - t1)

imputError = function(X_imp, X_true){colMeans(abs(X_imp-X_true), na.rm=T)/colMeans(X_true, na.rm=T)*100}
# Compare imputation error of the two methods: it is exactly the same
imputError(complete(imp.mice),X)
imputError(imp.joint.imp[[1]], X)

# See if there is a difference between predicting everything at once and training on a separate dataset
# Seems not -> not useful to do it clean? Or maybe only shows on small data (parameters converge quickly)
imp.nosplit = complete(mice(X_m, m=1, method='norm'))[-inTraining,]
imp.joint.train = train.MI_mvnorm(X_m[inTraining,])
imp.joint.imp = impute.MI_mvnorm(imp.joint.train, X_m[-inTraining,], m=1)[[1]]
imputError(imp.joint.imp, X[-inTraining,])
imputError(imp.nosplit, X[-inTraining,])

# Checking variation between multiple imputations: seems identical
imp.nosplit = mice(X_m, m=2, method='norm')
imp.joint.train = train.MI_mvnorm(X_m)
imp.joint.imp = impute.MI_mvnorm(imp.joint.train, X_m, m=2)
imputError(complete(imp.nosplit,1), complete(imp.nosplit,2))
imputError(imp.joint.imp[[1]], imp.joint.imp[[2]])

