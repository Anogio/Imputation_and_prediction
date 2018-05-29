library(tidyverse)
library(mice)

mice_imp <- function(X, m=5){
  print(paste('Performing', m, 'imputations with MICE...'))
  imp = mice(X, m=m, printFlag = F)
  res = list()
  for(i in 1:m){
    res[[i]] = complete(imp,i)
  }
  print('Done.')
  return(res)
}


