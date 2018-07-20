

MCAR <- function(X, miss_prop){
  print(paste('Adding ', miss_prop*100, '% missing observations completely at random.', sep=''))
  n = nrow(X)
  m = ncol(X)
  missing = matrix(runif(n*m), nrow=n, ncol=m) <= miss_prop
  X[missing] = NA

  missing = sum(colSums(is.na(X)))
  prop_missing = missing/(n*m)
  print(paste('New proportion of missing data: ', prop_missing*100, '%.', sep=''))
  print('Done.')
  return(X)
}


# Add a proportion of missing data but never empty a ful line
MCAR.noEmptyLines <- function(X, miss_prop.){
  print(dim(X))
  print(miss_prop.)
  n = nrow(X)
  m = ncol(X)
  missing = matrix(runif(n*m), nrow=n, ncol=m) <= miss_prop.
  missing[rowSums(missing)==ncol(X), 1] = F
  X[missing] = NA
  return(X)
}

# Add a proportion of missing data on just the first n_var columns
MCAR.nVars <- function(X, miss_prop, n_vars=NULL){
  n = nrow(X)
  m = ncol(X)
  if(is.null(n_vars)){
    n_vars = ceiling(ncol(X)/2)
  }

  if(n_vars==0){
    return(X)
  }

  for(i in 1:n_vars){
    X[,i][runif(n)<miss_prop] = NA
  }
  return(X)
}
