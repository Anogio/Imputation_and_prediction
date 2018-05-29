

MCAR <- function(X, miss_prop=0.05){
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
