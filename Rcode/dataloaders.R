library(tidyverse)

#############################
# Information on each dataset, used for automatic loading
data_folder = '../Data/'

files = list(
  iris = 'iris.csv',
  titanic = 'titanic.csv'
)
y_columns = list(
  iris = 5,
  titanic = 2
)
cat_columns = list(
  iris = c(),
  titanic = c(3, 5, 12)
)
num_columns = list(
  iris = 1:4,
  titanic = c(6:8, 10)
)

##################
# Helper functions

summarise_x <-  function(X_num, X_cat){
  # Prints diagnostic messages on the imported data
  missing_num = sum(colSums(is.na(X_num)))
  missing_prop_num = missing_num/(nrow(X_num)*ncol(X_num))
  missing_cat = sum(colSums(is.na(X_cat)))
  missing_prop_cat = missing_cat/(nrow(X_cat)*ncol(X_cat))
  
  print(paste(
    'Imported ', ncol(X_num), ' numerical variables (', missing_prop_num*100, '% missing entries), and ',
    ncol(X_cat), ' categorical variables (', missing_prop_cat*100, '% missing entries).'
  ))
  print(paste('Done (', nrow(X_num), ' rows).', sep=''))
}

###############################
# Main loader
loader <- function(dataset){
  print(paste('Loading ',  dataset, ' data...', sep=''))
  
  dat = read.csv(paste(data_folder,files[dataset], sep=''))
  ycol = y_columns[[dataset]]
  numcols = num_columns[[dataset]]
  catcols = cat_columns[[dataset]]
  y = as.factor(dat[, ycol])
  X_num = dat[, numcols]
  X_cat = dat[, catcols]
  
  if (ncol(X_cat)>0){
    for(i in 1:ncol(X_cat)){
      X_cat[,i] = as.factor(X_cat[,i])
    }
  }
  
  summarise_x(X_num, X_cat)
  return(list(X_numeric=X_num, X_category=X_cat, y=y))
}

###############
# Example
tit = loader('titanic')
ir = loader('iris')