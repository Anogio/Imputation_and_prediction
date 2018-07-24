library(norm)

# MVN fit
imp.mvnorm.train = function(X){
  # Must run *rngseed* at least once before using
  pre <- prelim.norm(as.matrix(X))
  thetahat <- em.norm(pre)
  return(list(thetahat=thetahat, sigma=getparam.norm(pre,thetahat)$sigma))
}

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
    #sigma_cond = sigma.miss - sigma.miss.obs %*% solve(sigma.obs) %*% t(sigma.miss.obs)

    # row[miss_col] = rmvnorm(1, mean=mu_cond, sigma=sigma_cond)
    row[miss_col] = mu_cond
  }
  return(row)
}

imp.mvnorm.estim = function(thetahat, X){
  thetahat = thetahat$thetahat
  #print(dim(X))
  pre <- prelim.norm(as.matrix(X))
  #thetahat <- em.norm(pre)
  params = getparam.norm(pre,thetahat)
  sigma = params$sigma
  mu = params$mu
  X = t(apply(X, 1, partial(estimate.1row, s=sigma, m=mu)))
  #print(dim(X))
  #print('')
  return(X)
}

imp.mvnorm = list(train=imp.mvnorm.train, estim=imp.mvnorm.estim)
