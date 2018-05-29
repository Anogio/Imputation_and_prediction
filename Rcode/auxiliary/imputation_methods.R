library(tidyverse)
library(mi)
library(mice)
library(Amelia)
library(cat)

########################
# Methods to generate filled datasets

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
  
  imp = amelia(X, m=m, ncpus = 4, noms=noms)$imputations
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