library(tidyverse)
library(mi)
library(mice)
library(Amelia)
library(cat)
library(missMDA)
library(missForest)
########################
# Methods to generate filled datasets
impute <- function(X, method, m=5, mipca_ncp=3, spl=NULL){
  if(method=='mice'){
    return(mice_imp(X,m))
  }
  else if(method=='amelia'){
    return(amelia_imp(X,m))
  }
  else if(method=='mi'){
    return(mi_imp(X,m))
  }
  else if(method=='cat'){
    return(cat_imp(X,m))
  }
  if(method=='mipca'){
    return(mipca_imp(X,m, mipca_ncp))
  }
  else if(method=='mean'){
    return(list(mean_imp_single(X, spl=spl)))
  }
  else if(method=='MF'){
    return(list(missforest_imp_single(X)))
  }
  else{
    stop('Unknown method provided')
  }
}

mice_imp <- function(X, m=5){
  print(paste('Performing', m, 'imputations with MICE...'))
  imp = mice(X, m=m, printFlag = F)
  res = list()
  for(i in 1:m){
    res[[i]] = mice::complete(imp,i)
  }
  print('Done.')
  return(res)
}

amelia_imp <- function(X, m=5){
  print(paste('Performing', m, 'imputations with Amelia...'))
  nom = sapply(X,is.factor)
  if(any(nom)){
    noms = which(nom)
  }
  else{
    noms = NULL
  }

  imp = amelia(X, m=m, noms=noms, p2s=1, parallel = 'multicore', ncpus = 4)$imputations
  print('Done.')
  return(imp)
}

mi_imp <- function(X, m=5){
  print(paste('Performing', m, 'imputations with mi...'))
  X_m = missing_data.frame(X)
  X_m = mi(X, verbose=F)
  X_m = mi::complete(X_m, m=m)
  print('Done')
  return(X_m)
}

cat_imp <- function(X, m=5){
  print(paste('Performing', m, 'imputations with cat...'))
  if(!all(sapply(X,is.factor))){
    stop("All variables must be categorical")
  }
  rngseed()

  X = as.matrix(as.data.frame(lapply(X,as.numeric)))
  X_p = prelim.cat(X)
  theta = em.cat(X_p)
  imp = list()
  for(i in 1:m){
    imp[[i]] = imp.cat(X_p, theta)
  }
  return(imp)
}

mipca_imp <- function(X, m=5, ncp=2){
  print(paste('Performing', m, 'imputations with MIPCA using', ncp, 'components...'))
  imp = MIPCA(X, ncp=ncp, nboot=m)
  print('Done.')
  return(imp$res.MI)
}

mean_imp_single <- function(X, categorical='most.freq', spl=NULL){
  print('Performing mean imputation...')
  catCols = which(sapply(X, is.factor))
  numCols = setdiff(1:ncol(X), catCols)
  if(is.null(spl)){
    spl=1:nrow(X)
  }

  for(i in numCols){
    c = X[,i]
    X[is.na(c),i] = mean(c[spl], na.rm = T)
  }
  for(i in catCols){
    if(categorical=='new.level'){
      levels(X[,i]) = c(levels(X[,i]), 'miss')
      X[is.na(X[,i]), i] = 'miss'
    }
    else if(categorical=='most.freq'){
      tt = table(X[spl,i])
      most.freq = names(tt[which.max(tt)])
      X[is.na(X[,i]), i] = most.freq
    }
  }
  print('Done.')
  return(X)
}

missforest_imp_single <- function(X){
  print('Performing missForest imputation...')
  res = missForest(X)$ximp
  print('Done.')
  return(res)
}
