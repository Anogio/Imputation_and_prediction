library(ggplot2)
library(tidyr)
library(knitr)
library(MASS)
library(mvtnorm)
library(mice)
library(ROCR)
source("../../../Wei_logreg/saem_model_selection_fct2.R")

#-------------------Load dataset---------------------- 
traumdata <- read.csv(file="../../Data/data_trauma.csv",header = TRUE,na.strings = c("","NR","IMP","NA","NF"),encoding = "UTF-8")
SAMU <- traumdata[ ,c(10:14,225:228,234,33:35,15,18,229:233,244,19:30,41:42,49:54,48,58,61,44,46:47,55:57,60,62, 38)+1]
#write.csv(SAMU, "SAMU.csv")
summary(SAMU)
names(SAMU)
SAMU$BMI[3302]=SAMU$Poids[3302]/(SAMU$Taille[3302]^2)
SAMU=SAMU[-which(SAMU$ACR.1==1 | is.na(SAMU$ACR.1)) ,]
Choc.hemorragique= traumdata$Choc.hemorragique
Choc.hemorragique = Choc.hemorragique[-which(traumdata$ACR.1==1 | is.na(traumdata$ACR.1))]

Choc.hemorragique = Choc.hemorragique[-which(SAMU$Mecanisme=="Arme blanche" | SAMU$Mecanisme=="Arme à feu")]
SAMU=SAMU[-which(SAMU$Mecanisme=="Arme blanche" | SAMU$Mecanisme=="Arme à feu"),]

#-------------------Select continues variables---------------------- 
SAMU_CONT = SAMU[,c(1,3:5,34:41,43:44,48:50)]
indx <- sapply(SAMU_CONT, is.factor)
SAMU_CONT[indx] <- lapply(SAMU_CONT[indx], function(x) as.numeric(as.character(x)))
which((SAMU_CONT$BMI>=SAMU_CONT$Poids/(SAMU_CONT$Taille^2)-0.1)&(SAMU_CONT$BMI<=SAMU_CONT$Poids/(SAMU_CONT$Taille^2)+0.1)==FALSE)

SAMU_CONT$SD.min=SAMU_CONT$PAS.min-SAMU_CONT$PAD.min
SAMU_CONT$SD.SMUR=SAMU_CONT$PAS.SMUR-SAMU_CONT$PAD.SMUR
SAMU_NEW = SAMU_CONT[,-c(7:8,10:11,15)]

p <- ggplot(data=SAMU_NEW)
for (n in names(SAMU_NEW)){
  print(p + geom_density(aes_string(x=n),alpha=.2, fill="#FF6666"))
}

#------------------model selection-------------



set.seed(100) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 80% of data as sample from total 'n' rows of the data  
#sample <- sample.int(n = nrow(SAMU_NEW), size = floor(.8*nrow(SAMU_NEW)), replace = F)
sample =1:1277
SAMU.train  <- SAMU_NEW[-sample, ]
SAMU.test  <- SAMU_NEW[sample, ]

Choc.hemorragique.train=Choc.hemorragique[-sample]
Choc.hemorragique.test=Choc.hemorragique[sample]
N=dim(SAMU.train)[1]
p=dim(SAMU.train)[2]
combinations = function(n){
  comb = NULL
  for( i in 1:n) comb = rbind(cbind(1,comb),cbind(0,comb))
  return(comb)
}

subsets=combinations(p)
ll = ll1 = ll2 = matrix(-Inf,nrow=p,ncol=p)

subsets1 = subsets[rowSums(subsets)==1,]
for (j in 1:(nrow(subsets1))){
  cat('subset ',subsets1[j,],'\n')  
  pos_var=which(subsets1[j,]==1)
  list.saem.subset=miss.saem(data.matrix(SAMU.train),pos_var,Choc.hemorragique.train,maxruns=1000,tol_em=1e-7,nmcmc=2,print_iter=FALSE,ll_obs_cal=TRUE)
  ll[1,j] = list.saem.subset$ll
  ll1[1,j] = list.saem.subset$ll1
  ll2[1,j] = list.saem.subset$ll2
}

id = AIC = BIC = AIC1 = BIC1 = rep(0,p)
subsetsi=subsets1
SUBSETS = matrix(-Inf,nrow=p,ncol=p)
for(i in 2:p){
  nb.x = i-1
  nb.para = (nb.x + 1) + p + p*p 
  id[i-1] = d = which.max(ll[i-1,])
  pos_var=which(subsetsi[d,]==1)
  AIC[i-1] =  -2*ll[i-1,d]+ 2*nb.para
  AIC1[i-1] = -2*ll1[i-1,d]+ 2*(nb.x+1)
  BIC[i-1] = -2*ll[i-1,d]+ nb.para * log(N)
  BIC1[i-1] = -2*ll1[i-1,d]+ (nb.x+1) * log(N)
  SUBSETS[i-1,]=subsetsi[d,]
  if(i==2){subsetsi = subsets[(rowSums(subsets)==i) & (subsets[,pos_var]==i-1),]}
  if(i>2){subsetsi = subsets[(rowSums(subsets)==i) & (rowSums(subsets[,pos_var])==i-1),]}
  for (j in 1:(nrow(subsetsi))){
    cat('subset ',subsetsi[j,],'\n')  
    pos_var=which(subsetsi[j,]==1)
    list.saem.subset=miss.saem(data.matrix(SAMU.train),pos_var,Choc.hemorragique.train,maxruns=1000,tol_em=1e-7,nmcmc=2,print_iter=FALSE,ll_obs_cal=TRUE)
    ll[i,j] = list.saem.subset$ll
    ll1[i,j] = list.saem.subset$ll1
    ll2[i,j] = list.saem.subset$ll2
  }
}

list.saem.subset=miss.saem(data.matrix(SAMU.train),1:p,Choc.hemorragique.train,maxruns=1000,tol_em=1e-7,nmcmc=2,print_iter=FALSE,ll_obs_cal=TRUE)
ll[p,1] = list.saem.subset$ll
ll1[p,1] = list.saem.subset$ll1
ll2[p,1] = list.saem.subset$ll2
nb.x = p
nb.para = (nb.x + 1) + p + p*p 
AIC[p] =  -2*ll[p,1]+ 2*nb.para 
AIC1[p] = -2*ll1[p,1]+ 2*(nb.x+1)#11
BIC[p] = -2*ll[p,1]+ nb.para * log(N)
BIC1[p] = -2*ll1[p,1]+ (nb.x+1) * log(N)

par(mfrow=c(2,2))
plot(AIC)
plot(BIC)
plot(AIC1)
plot(BIC1)

names(SAMU.train)[SUBSETS[which.min(BIC),]==1]

#prediction
subset_choose = which(SUBSETS[which.min(BIC),]==1)
#"Glasgow.moteur.initial"   "FC.max" "Hemocue.init" 
# "Remplissage.total.cristalloides" "Remplissage.total.colloides"    "SD.min" 
list.saem.subset=miss.saem(data.matrix(SAMU.train),subset_choose,Choc.hemorragique.train,maxruns=1000,tol_em=1e-7,print_iter=TRUE,var_obs_cal=TRUE)
beta.saem.train = list.saem.subset$beta
se.saem.train = list.saem.subset$std_obs
mu.saem = list.saem.subset$mu
sig2.saem = list.saem.subset$sig2


#method 1 x_mis = argmax p(x_mis | x_obs)
Choc.hemorragique.test = Choc.hemorragique.test[-which(rowSums(is.na(SAMU.test))==dim(SAMU.test)[2])]
SAMU.test = SAMU.test[-which(rowSums(is.na(SAMU.test))==dim(SAMU.test)[2]),]

SAMU.test1 = data.matrix(SAMU.test)
rindic = as.matrix(is.na(SAMU.test1))
for(i in 1:dim(SAMU.test1)[1]){
  if(sum(rindic[i,])!=0){
    miss_col = which(rindic[i,]==TRUE)
    x2 = SAMU.test1[i,-miss_col]
    mu1 = mu.saem[miss_col]
    mu2 = mu.saem[-miss_col]
    sigma11 = sig2.saem[miss_col,miss_col]
    sigma12 = sig2.saem[miss_col,-miss_col]
    sigma22 = sig2.saem[-miss_col,-miss_col]
    sigma21 = sig2.saem[-miss_col,miss_col]
    mu_cond = mu1+sigma12 %*% solve(sigma22)%*%(x2-mu2)
    SAMU.test1[i,miss_col] =mu_cond
  }
}
tmp <- as.matrix(cbind.data.frame(rep(1,dim(SAMU.test1)[1]),SAMU.test1)) %*% as.matrix(beta.saem.train) 
pr <- 1/(1+(1/exp(tmp)))


seuil = 0.1
#pred.saem = round(pr)
pred.saem = (pr>seuil)*1
acc.saem = sum(pred.saem == Choc.hemorragique.test)/dim(SAMU.test1)[1]
table(Choc.hemorragique.test,pred.saem)

pre <- prediction(pr, Choc.hemorragique.test)
prf <- performance(pre, measure = "tpr", x.measure = "fpr")
plot(prf, colorize = FALSE,main = "ROC on validation set - SAEM ")
abline(a=0, b= 1, col="grey80")
auc <- performance(pre, measure = "auc")
auc <- auc@y.values[[1]]
print(paste("The AUC is", auc ) )
cost.perf = performance(pre, "cost", cost.fp = 1, cost.fn = 1)#false positives are twice as costly as false negatives
pred@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]


#method 2
pred.saem2 = pr2 =rep(0,dim(SAMU.test)[1])

rindic = as.matrix(is.na(SAMU.test))
mc.size = 100
SAMU.test = data.matrix(SAMU.test)
for(i in 1:dim(SAMU.test)[1]){
  x=SAMU.test[i,]
  if(sum(rindic[i,])==0){
    pr2[i]=log_reg(y=1,x=c(1,x),beta.saem.train,iflog=FALSE)
    pred.saem2[i] = (pr2[i]>0.1)*1
  }
  else{
    miss_col = which(rindic[i,]==TRUE)
    x2 = SAMU.test[i,-miss_col]
    mu1 = mu.saem[miss_col]
    mu2 = mu.saem[-miss_col]
    sigma11 = sig2.saem[miss_col,miss_col]
    sigma12 = sig2.saem[miss_col,-miss_col]
    sigma22 = sig2.saem[-miss_col,-miss_col]
    sigma21 = sig2.saem[-miss_col,miss_col]
    mu_cond = mu1+sigma12 %*% solve(sigma22)%*%(x2-mu2)
    sigma_cond = sigma11 - sigma12 %*% solve(sigma22) %*% sigma21
    x1_all=mvrnorm(n = mc.size, mu_cond, sigma_cond, tol = 1e-6, empirical = FALSE, EISPACK = FALSE)
    p1=0
    for(j in 1:mc.size){
      x[miss_col] =x1= x1_all[j,]
      p1 = p1 + log_reg(y=1,x=c(1,x),beta.saem.train,iflog=FALSE)
    }
    pr2[i] =p1/mc.size
    if(p1/mc.size>0.1){
      pred.saem2[i] =1
    }
  }
}
table(Choc.hemorragique.test,pred.saem2)

pre <- prediction(pr2, Choc.hemorragique.test)
prf <- performance(pre, measure = "tpr", x.measure = "fpr")
plot(prf, colorize = FALSE,main = "ROC on validation set - SAEM ")
abline(a=0, b= 1, col="grey80")
auc <- performance(pre, measure = "auc")
auc <- auc@y.values[[1]]
print(paste("The AUC is", auc ))
cost.perf = performance(pre, "cost", cost.fp = 1, cost.fn = 10)#false positives are twice as costly as false negatives
pred@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]



#-------------------multiple imputation---------------------- 
DATA.ch= cbind.data.frame(Choc.hemorragique.train,SAMU.train)
N=dim(SAMU.train)[1]
p=dim(SAMU.train)[2]
imp.ch <- mice(DATA.ch,seed=100,m=20,print=FALSE,method='rf')

SAMU.mean = complete(imp.ch,action=1)
for(i in 2:20){SAMU.mean = SAMU.mean + complete(imp.ch,action=i)}
SAMU.mean = SAMU.mean[,2:dim(SAMU.mean)[2]]/20

expr <- expression(
  fit.ch0 <- glm(Choc.hemorragique.train~1,family = binomial),
  f2 <- step(fit.ch0, scope=list(upper=~Age+Poids+Taille+BMI+Glasgow.initial+Glasgow.moteur.initial+
                                   SD.min+FC.max+SD.SMUR+FC.SMUR+Hemocue.init+SpO2.min+
                                   Remplissage.total.cristalloides+ Remplissage.total.colloides, lower=~1),direction="forward",k=log(N),trace=FALSE)) 
fit <- with(imp.ch, expr)
formulas <- lapply(fit$an, formula)
terms <- lapply(formulas, terms)
vars <- unlist(lapply(terms, labels))
table(vars)
# Age                          FC.max                 Glasgow.initial 
# 16                              20                               6 
# Glasgow.moteur.initial                    Hemocue.init     Remplissage.total.colloides 
# 14                              20                              20 
# Remplissage.total.cristalloides                          SD.min                         SD.SMUR 
# 20                              20                              20 
# SpO2.min 
# 2 
fit.without <- with(imp.ch, glm(Choc.hemorragique.train~FC.max+
                                  Hemocue.init+Remplissage.total.colloides+Remplissage.total.cristalloides+
                                  SD.min+SD.SMUR,family = binomial))
fit.with <- with(imp.ch, glm(Choc.hemorragique.train~Age+FC.max+
                               Hemocue.init+Remplissage.total.colloides+Remplissage.total.cristalloides+
                               SD.min+SD.SMUR,family = binomial))
pool.compare(fit.with, fit.without)$pvalue
#0.002742855
# larger model is statistically different from the smaller submodel


fit.without <-with(imp.ch, glm(Choc.hemorragique.train~Age+FC.max+
                                 Hemocue.init+Remplissage.total.colloides+Remplissage.total.cristalloides+
                                 SD.min+SD.SMUR,family = binomial))
fit.with <- with(imp.ch, glm(Choc.hemorragique.train~Age+FC.max+Glasgow.moteur.initial +
                               Hemocue.init+Remplissage.total.colloides+Remplissage.total.cristalloides+
                               SD.min+SD.SMUR,family = binomial))
pool.compare(fit.with, fit.without)$pvalue
#2.66083e-06

fit.without <-with(imp.ch, glm(Choc.hemorragique.train~Age+Glasgow.moteur.initial+FC.max +
                                 Hemocue.init+Remplissage.total.cristalloides+Remplissage.total.colloides+
                                 SD.min+SD.SMUR,family = binomial))
fit.with <- with(imp.ch, glm(Choc.hemorragique.train~Age+FC.max+Glasgow.moteur.initial +Glasgow.initial+
                               Hemocue.init+Remplissage.total.colloides+Remplissage.total.cristalloides+
                               SD.min+SD.SMUR,family = binomial))
pool.compare(fit.with, fit.without)$pvalue
#0.5777976
beta.mice.train=summary(pool(fit.without))[,1]
se.mice.train=summary(pool(fit.without))[,2]

#prediction
SAMU.test3=SAMU.test
for(i in 1:ncol(SAMU.test3)){
  SAMU.test3[is.na(SAMU.test3[,i]), i]<- mean(SAMU.mean[,i], na.rm = TRUE)
}
SAMU.test4 = cbind.data.frame(rep(1,dim(SAMU.test3)[1]),SAMU.test3)[,c(1,2,7,8,10,12,13,14,15)]
SAMU.test4 = data.matrix(SAMU.test4)
tmp <- SAMU.test4%*% as.matrix(beta.mice.train) 
pr <- 1/(1+(1/exp(tmp)))
pred.mice = (pr>0.1)*1
acc.mice = sum(pred.mice == Choc.hemorragique.test)/dim(SAMU.test4)[1]
table(Choc.hemorragique.test,pred.mice)


pre <- prediction(pr, Choc.hemorragique.test)
prf <- performance(pre, measure = "tpr", x.measure = "fpr")
plot(prf, colorize = FALSE, main = "ROC on validation set - mice ")
abline(a=0, b= 1, col="grey80")
auc <- performance(pre, measure = "auc")
auc <- auc@y.values[[1]]
print(paste("The AUC is", auc ) )





#------------------imputePCA-------------
#estim_ncpPCA(SAMU_CONT)
library(missMDA)
library(FactoMineR)
res.comp = imputePCA(SAMU.train) #iterative PCA
imp = cbind.data.frame(Choc.hemorragique.train,res.comp$completeObs)
#perform the PCA on the completed data set using the PCA function of the FactoMineR package
res.pca = PCA(imp,quali.sup = 1,graph = FALSE)
plot(res.pca,choix='ind',hab=1, lab ='quali',cex=0.5)
plot(res.pca,choix='var',cex=0.5)
#CHANGER BMI A BONNE VALEUR

regfull =  glm(Choc.hemorragique.train~., data=imp,family = binomial)
reg0 =  glm(Choc.hemorragique.train~1, data=imp,family = binomial)

#msAIC = step(reg0,scope=list(lower=formula(reg0),upper=formula(regfull)), direction="forward",k=2,trace=FALSE)
#Step:  AIC=3398.02
#Choc.hemorragique ~ PAS.min + FC.max + Hemocue.init + 
#Remplissage.total.colloides + Remplissage.total.cristalloides + Glasgow.initial + Age

msBIC = step(reg0,scope=list(lower=formula(reg0),upper=formula(regfull)), direction="forward",k=log(N))
# SD.min + Remplissage.total.colloides + Remplissage.total.cristalloides + FC.max + Hemocue.init + Glasgow.initial
beta.imppca = summary(msBIC)$coef[,1]
se.imppca = summary(msBIC)$coef[,2]


SAMU.test3=SAMU.test
for(i in 1:ncol(SAMU.test3)){
  SAMU.test3[is.na(SAMU.test3[,i]), i]<- mean(res.comp$completeObs[,i], na.rm = TRUE)
}
tmp <- predict(msBIC, newdata=as.data.frame(SAMU.test3))
pr <- 1/(1+(1/exp(tmp)))
pred.imppca =(pr>0.1)*1
acc.imppca = sum(pred.imppca == Choc.hemorragique.test)/dim(SAMU.test3)[1]
table(Choc.hemorragique.test,pred.imppca)


pre <- prediction(pr, Choc.hemorragique.test)
prf <- performance(pre, measure = "tpr", x.measure = "fpr")
plot(prf, colorize = FALSE, main = "ROC on validation set - imputePCA")
abline(a=0, b= 1, col="grey80")
auc <- performance(pre, measure = "auc")
auc <- auc@y.values[[1]]
print(paste("The AUC is", auc ) )
#"The AUC is 0.877650795803943"

#----------------------------Mean imp----------------------------
SAMU.train1=SAMU.train
for(i in 1:ncol(SAMU.train1)){
  SAMU.train1 [is.na(SAMU.train1[,i]), i] <- mean(SAMU.train1[,i], na.rm = TRUE)
}
SAMU.test2=SAMU.test
for(i in 1:ncol(SAMU.test2)){
  SAMU.test2[is.na(SAMU.test2[,i]), i]<- mean(SAMU.train1[,i], na.rm = TRUE)
}
DATA.mean= cbind.data.frame(Choc.hemorragique.train,SAMU.train1)
regfull =  glm(Choc.hemorragique.train~., data=DATA.mean,family = binomial)
reg0 =  glm(Choc.hemorragique.train~1, data=DATA.mean,family = binomial)
BIC.mean = step(reg0,scope=list(lower=formula(reg0),upper=formula(regfull)), direction="forward",k=log(N),trace=FALSE)
AIC.mean = step(reg0,scope=list(lower=formula(reg0),upper=formula(regfull)), direction="forward",k=2,trace=FALSE)

beta.mean = summary(BIC.mean)$coef[,1]
se.mean = summary(BIC.mean)$coef[,2]
#tmp <- predict(BIC.mean, newdata=SAMU.test,type='response')
tmp <- predict(BIC.mean, newdata=as.data.frame(SAMU.test2))
pr <- 1/(1+(1/exp(tmp)))
pred.mean = (pr>seuil)*1
#pred.mean = round(pr)
acc.mean = sum(pred.mean == Choc.hemorragique.test)/dim(SAMU.test2)[1]
#0.9149877
table(Choc.hemorragique.test,pred.mean)


pre <- prediction(pr, Choc.hemorragique.test)
prf <- performance(pre, measure = "tpr", x.measure = "fpr")
plot(prf, colorize = FALSE, main = "ROC on validation set - mean imputation")
abline(a=0, b= 1, col="grey80")
auc <- performance(pre, measure = "auc")
auc <- auc@y.values[[1]]
print(paste("The AUC is", auc ) )