
trauma = trauma_i

X = MCAR.noEmptyLines(trauma, miss_prop. = 0.2)
keepL = rowSums(is.na(X))!=ncol(X)
X = X[keepL,]
trauma = trauma[keepL,]
nMiss = colSums(is.na(trauma) != is.na(X))

X_mean = imp.mean.estim(imp.mean.train(X), X)
X_mvn = imp.mvnorm.estim(imp.mvnorm.train(X), X)

X_mice = mice::complete(mice(X, method='pmm', m=1))
X$Glasgow = as.factor(X$Glasgow)
X$Colloides = as.factor(X$Colloides)
X_amelia = amelia(X, m=1, ords=c('Glasgow','Colloides'))$imputations$imp1
#X_mice = mice::complete(mice(X, method=c('pmm', 'polr', 'pmm', 'pmm', 'pmm', 'polr', 'pmm', 'pmm'), m=1))
X_amelia$Glasgow = as.numeric(levels(X_amelia$Glasgow))[X_amelia$Glasgow]
X_amelia$Colloides = as.numeric(levels(X_amelia$Colloides))[X_amelia$Colloides]

err_mean = colSums((X_mean-trauma)^2, na.rm=T) / nMiss
err_mice = colSums((X_mice-trauma)^2, na.rm=T) / nMiss
err_mvn = colSums((X_mvn-trauma)^2, na.rm=T) / nMiss
err_amelia = colSums((X_amelia-trauma)^2, na.rm=T) / nMiss
errs=data.frame(amelia=err_amelia,mean=err_mean, MVN=err_mvn, imp_mice=err_mice) %>% mutate(col=colnames(X)) %>% gather('method', 'error',-col)

ggplot(errs) + aes(x=col, y=error, fill=method) + geom_bar(stat='identity', position='dodge') + scale_y_log10()



trauma = ...

# MICE will full data and well-specified methods
# -> results
