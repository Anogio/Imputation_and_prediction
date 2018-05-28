library(tidyverse)
library(mice)

mice_imp <- function(X, m=5){
  imp = mice(X, m=m)
  res = list()
  for(i in 1:m){
    res[[i]] = complete(imp,i)
  }
  return(res)
}


