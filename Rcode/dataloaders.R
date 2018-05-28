library(tidyverse)

summarise_xy <-  function(X,y){
  missing = sum(colSums(is.na(X)))
  missing_prop = missing/(nrow(X)*ncol(X))
  print(paste(missing, ' (', missing_prop*100, '%) missing observations.', sep=''))
  print(paste('Done (', nrow(X), ' rows, ', ncol(X), ' columns).', sep=''))
}

iris_loader <- function(datatypes='numeric'){
  print(paste('Loading iris data (selected data type: ', datatypes, ') ...', sep=''))
  data(iris)
  if(datatypes=='numeric' | datatypes=='all'){
    X=iris[,1:4]
    y=iris[,5]
  }
  else{
    stop('Iris only has numeric values.')
  }
  
  summarise_xy(X,y)
  return(list(X=X, y=y))
}

titanic_loader <- function(datatypes='all'){
  print(paste('Loading titanic data (selected data type: ', datatypes, ') ...', sep=''))
  
  tit = read.csv('../Data/titanic.csv')
  tit$Pclass = as.factor(tit$Pclass)
  if(datatypes=='all'){
    X=tit[,c(3, 5:8,10,12)]
    y=tit[,2]
  }
  else if(datatypes=='category'){
    X=tit[,c(3, 5, 12)]
    y=tit[,2]
  }
  else if(datatypes=='numeric'){
    X=tit[,c(6:8, 11)]
    y=tit[,2]
  }
  else{
    stop('Wrong data type specified: must be "all", "numeric" or "category".')
  }
  
  summarise_xy(X,y)
  return(list(X=X, y=y))
}

trauma_loader <- function(){
  # TODO : load trauma data using Wei's code
}

