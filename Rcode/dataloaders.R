library(tidyverse)

trauma_loader <- function(){
  # Load data into memory
  trauma <- read.csv('../Data/data_trauma.csv',na.strings = c("","NR","IMP","NA","NF"),encoding = "UTF-8")
  # Select relevant columns and compute BMI
  trauma <- trauma[ ,c(10:14,225:228,234,33:35,15,18,229:233,244,19:30,41:42,49:54,48,58,61,44,46:47,55:57,60,62, 38)] 
  trauma$BMI[3302] <- trauma$Poids[3302]/(trauma$Taille[3302]^2)
  
  trauma <- trauma[-which(trauma$ACR.1==1),]
  Choc.hemorragique <-  traumdata$Choc.hemorragique
  Choc.hemorragique <-  Choc.hemorragique[-which(traumdata$ACR.1==1)]
  
  Choc.hemorragique <-  Choc.hemorragique[-which(trauma$Mecanisme=="Arme blanche" | trauma$Mecanisme=="Arme ύ feu")]
  trauma <- trauma[-which(trauma$Mecanisme=="Arme blanche" | trauma$Mecanisme=="Arme ύ feu"),]
}