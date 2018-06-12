library(norm)
library(mvtnorm)

# Custom imports
aux.folder = '../auxiliary/'
source(paste(aux.folder,'dataloaders.R',sep=''), chdir = T)
source(paste(aux.folder,'generate_missing.R',sep=''), chdir = T)

data_folder = '../../Data/'

dataset = 'abalone'
max_rows = NULL
seed = 3

dat = loader(dataset, max_rows, seed)
y = dat$y
X = dat$X_numeric

X_m = MCAR(X, 0.2)

#############################################
rngseed(seed)

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
    sigma_cond = sigma.miss - sigma.miss.obs %*% solve(sigma.obs) %*% t(sigma.miss.obs)

    row[miss_col] = rmvnorm(1, mean=mu_cond, sigma=sigma_cond)
}
  return(row)
}

impute_X = function(X){
  pre <- prelim.norm(as.matrix(X))
  thetahat <- em.norm(pre)
  params = getparam.norm(pre,thetahat)
  sigma = params$sigma
  mu = params$mu
  for(i in 1:nrow(X)){
    X[i,] = estimate.1row(X[i,], sigma, mu)
  }
  return(X)
}

#####################################################

impute_MI_mvnorm = function(X, m=5){
  # Must run *rngseed* at least once before using
  estimations = list()

  for(i in 1:m){
    X_bootstrap = X[base::sample(1:nrow(X), nrow(X), replace=T),]
    pre <- prelim.norm(as.matrix(X))
    thetahat <- em.norm(pre)
    estimations[[i]] = imp.norm(pre, thetahat, X)
  }

  return(estimations)
}


##############################
imp_custom2 = impute_X(X_m)
imp_custom = impute_MI_mvnorm(X_m)[[1]]
imp_mice = complete(mice(X_m, m=2, method='norm'), action=2)
imp_mice_pmm = complete(mice(X_m, m=1, method='pmm'))
imp_mean = complete(mice(X_m, m=1, method='mean'))

errors = data.frame(
  custom2 = colSums((X-imp_custom2)^2),
  custom = colSums((X-imp_custom)^2),
  mice_norm = colSums((X-imp_mice)^2),
  mice_pmm = colSums((X-imp_mice_pmm)^2),
  #mean = colSums((X-imp_mean)^2),
  column = colnames(X)
)

errors %>% gather('Imputation.method', 'MSE', -column) %>%
  ggplot() + aes(x=column, y=MSE, fill=Imputation.method) + geom_bar(stat='identity', position='dodge')
