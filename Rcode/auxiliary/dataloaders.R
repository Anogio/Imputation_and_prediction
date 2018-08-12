library(tidyverse)
library(caret)

#############################
# Information on each dataset, used for automatic loading
#\\TODO Put all of this in a separate configuration file
data_folder = '../../Data/'

files = list(
  iris = 'iris.csv',
  titanic = 'titanic.csv',
  trauma = 'hemo_shock.csv',
  trauma_new = 'hemo_shock_new.csv',
  abalone = 'abalone.csv'
)
response_type_categorical = list(
  iris = T,
  titanic = T,
  trauma = T,
  trauma_new = T,
  abalone = F
)
y_columns = list(
  iris = 5,
  titanic = 2,
  trauma = 40,
  trauma_new = 19,
  abalone = 9
)
cat_columns = list(
  iris = c(),
  titanic = c(3, 5, 12),
  trauma = c(2, 6:21, 28:30, 33:35),
  trauma_new = c(2, 6, 13, 16),
  abalone = c(1)
)
num_columns = list(
  iris = 1:4,
  titanic = c(6:8, 10),
  trauma = c(1, 3:5, 22:27, 31, 32, 36:39),
  trauma_new = c(1, 3:5, 7:12, 14, 15, 17, 18),
  abalone = 2:8
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
loader <- function(dataset, max_rows=NULL, seed=NULL){
  print(paste('Loading ',  dataset, ' data...', sep=''))

  dat = read.csv(paste(data_folder,files[dataset], sep=''))

  ycol = y_columns[[dataset]]
  numcols = num_columns[[dataset]]
  catcols = cat_columns[[dataset]]

  if(response_type_categorical[[dataset]]){
    y = as.factor(dat[, ycol])
    levels(y) = make.names(levels(y))
  }
  else{
    y = as.numeric(dat[,ycol])
  }

  if(!is.null(max_rows) && nrow(dat)>max_rows){
    print(paste('Truncating dataset (from', nrow(dat), 'to approximately', max_rows ,'rows).'))
    if(!is.null(seed)){
      set.seed(seed)
    }
    keeprows = createDataPartition(y, p=max_rows/nrow(dat), list=F)
    dat = dat[keeprows,]
    y = y[keeprows]
  }

  X_num = data.frame(dat[, numcols])
  X_cat = data.frame(dat[, catcols])

  if (ncol(X_cat)>0){
    for(i in 1:ncol(X_cat)){
      X_cat[,i] = as.factor(X_cat[,i])
    }
  }
  if (ncol(X_num)>0){
    for(i in 1:ncol(X_num)){
      X_num[,i] = as.numeric(X_num[,i])
    }
  }

  summarise_x(X_num, X_cat)
  return(list(X_numeric=X_num, X_category=X_cat, y=y))
}

################
# Data clearners to generate CSV files for the loader

clean_trauma <- function(){
  traumdata <- read.csv(file=paste(data_folder,'data_trauma.csv', sep=''), header = TRUE,na.strings = c("","NR","IMP","NA","NF"),encoding = "UTF-8")

  hemo_columns =c("Age", "Sexe", "Poids", "Taille", "BMI",
                  "Ante.ASA.PS", "Ante.grossesse", "Traitement.anticoagulant",
                  "Mecanisme", "Discordance",
                  "TC.ejection.vehicule", "TC.passager.decede", "TC.vitesse.elevee",
                  "TC.incarceration",
                  #"TC.durée.incarceration", # Seems superfluous
                  "TC.non.evaluable",
                  "TC.ecrase.projete", "TC.chute", "TC.blast", "TA.ischemie", "TA.amputation",
                  "TA.frac.bassin", "Glasgow.initial", "Glasgow.moteur.initial",
                  "PAS.min", "PAD.min", "FC.max", "PAS.SMUR", "PAD.SMUR", "FC.SMUR",
                  "ACR.1", "Hemocue.init", "SpO2.min", "Mydriase", "Mannitol.SSH",
                  "Regression.mydriase.sous.osmotherapie", "Remplissage.total.cristalloides",
                  "Remplissage.total.colloides", "Catecholamines", "IOT.SMUR",
                  "Origine",'Temperature', 'Temperature.min')
  SAMU = traumdata[, hemo_columns]
  SAMU$BMI[3302]=SAMU$Poids[3302]/(SAMU$Taille[3302]^2)
  SAMU=SAMU[-which(SAMU$ACR.1==1 | is.na(SAMU$ACR.1)) ,]
  Choc.hemorragique= traumdata$Choc.hemorragique
  Choc.hemorragique = Choc.hemorragique[-which(traumdata$ACR.1==1 | is.na(traumdata$ACR.1))]
  Choc.hemorragique = Choc.hemorragique[-which(SAMU$Mecanisme=="Arme blanche" | SAMU$Mecanisme=="Arme à feu")]
  SAMU=SAMU[-which(SAMU$Mecanisme=="Arme blanche" | SAMU$Mecanisme=="Arme à feu"),]

  SAMU$Origine[is.na(SAMU$Origine)] = 'Secondaire'
  Choc.hemorragique = Choc.hemorragique[SAMU$Origine=='Primaire']
  SAMU = SAMU[SAMU$Origine=='Primaire',]

  SAMU$SD.min=SAMU$PAS.min-SAMU$PAD.min
  SAMU$SD.SMUR=SAMU$PAS.SMUR-SAMU$PAD.SMUR

  SAMU = SAMU %>% dplyr::select(-one_of(c("PAS.min", "PAD.min", "PAS.SMUR", "PAD.SMUR", 'ACR.1')))

  write.csv(cbind(SAMU, Choc.hemorragique), paste(data_folder,'hemo_shock.csv', sep=''), row.names = F)
  }
clean_trauma()

###############
correspondences = list(
  "PASh" = "PAS",
  "PASphmin" = "PAS.min",
  "PASsmur" = "PAS.SMUR",
  "PADh" = "PAD",
  "PADsmur" = "PAD.SMUR",
  "PADphmin" = "PAD.min",
  "causedc" = "Cause.du.DC",
  "mecanisme" = "Mecanisme",
  "NADdechoc" = "Dose.NAD.depart", #??? (also nadph)
  "Fcsmur" = "FC.SMUR",
  "Fcphmax" = "FC.max",
  "FCh" = "FC",
  "CGRdechoc" = "Nombre.CGR.dechoc",
  "chochemo" = "Choc.hemorragique",
  "lactateph" = "Lactates", #-> Multiple variables
  "blocdirect" = "Bloc.direct",
  "bilanAIS" = "Bilan.lesion.AIS",
  "FASThemot" = "FAST.hemothorax",
  "FASTperic" = "FAST.epanchement.pericardique",
  "FASThemop" = "FAST.hemoperitoine",
  "FASTpno" = "FAST.pneumothorax",# !reste FASTnormale?
  "hemocueph" = "Hemocue.init",
  "age" = "Age",
  "BMI" = "BMI",
  "catechomax" = "Catecholamines",
  "cristalph" = "Remplissage.total.cristalloides", # -> aussi cristal24
  "poids" = "Poids",
  "collph" = "Remplissage.total.colloides", #-> aussi coll24
  "taille"= "Taille",
  "spo2ph" = "SpO2.min", #-> ou spo2h?
  "GCSi" = "Glasgow.initial",
  "GCSmi" = "Glasgow.moteur.initial",
  "ACR" = "ACR.1",
  "mydriase" = "Mydriase",
  "origine" = "Origine",
  'sexe' = 'Sexe',
  'date.entree' = "Date.entree"
)

clean_trauma_new <- function(){
  traumdata <- read.csv(file=paste(data_folder,'data_trauma2.csv', sep=''), header = TRUE, encoding = "UTF-8")

  traumdata$date.entree[2076] = '8/21/2016'
  traumdata$date.sortie[2076] = '8/27/2016'
  traumdata$date.entree[5234] = '12/17/2015'
  traumdata$date.sortie[5234] = '12/17/2015'
  traumdata = traumdata[-c(1541, 1515),] # remove rows with aberrant, unfixable dates

  replace_colnames = colnames(traumdata)
  for(newName in names(correspondences)){
    oldName = correspondences[[newName]]
    #print(newName)
    #print(sum(replace_colnames==newName))
    replace_colnames[which(replace_colnames==newName)] = oldName
  }
  colnames(traumdata) = replace_colnames

  write.csv(traumdata, paste(data_folder,'data_trauma2_oldnames.csv', sep=''), row.names = F)

  hemo_columns =c("Age", "Sexe", "Poids", "Taille", "BMI",
                  "Mecanisme", "Glasgow.initial", "Glasgow.moteur.initial",
                  "PAS.min", "PAD.min", "FC.max", "PAS.SMUR", "PAD.SMUR", "FC.SMUR",
                  "ACR.1", "Hemocue.init", "SpO2.min", "Mydriase",
                  "Remplissage.total.cristalloides",
                  "Remplissage.total.colloides", "Catecholamines",
                  "Origine")
  SAMU = traumdata[, hemo_columns]
  SAMU$BMI =SAMU$Poids/(SAMU$Taille^2)
  SAMU=SAMU[-which(SAMU$ACR.1==1 | is.na(SAMU$ACR.1)) ,]
  Choc.hemorragique = traumdata$Choc.hemorragique
  Choc.hemorragique = Choc.hemorragique[-which(traumdata$ACR.1==1 | is.na(traumdata$ACR.1))]
  Choc.hemorragique = Choc.hemorragique[-which(SAMU$Mecanisme=="Stab wound" | SAMU$Mecanisme=="Gunshot wound")]
  SAMU=SAMU[-which(SAMU$Mecanisme=="Stab wound" | SAMU$Mecanisme=="Gunshot wound"),]

  SAMU$SD.min=SAMU$PAS.min-SAMU$PAD.min
  SAMU$SD.SMUR=SAMU$PAS.SMUR-SAMU$PAD.SMUR

  SAMU$Origine[is.na(SAMU$Origine)] = 'Secondaire'
  Choc.hemorragique = Choc.hemorragique[SAMU$Origine=='Primaire']
  SAMU = SAMU[SAMU$Origine=='Primaire',]

  SAMU = SAMU[!is.na(Choc.hemorragique),]
  Choc.hemorragique = Choc.hemorragique[!is.na(Choc.hemorragique)]

  SAMU = SAMU %>% dplyr::select(-one_of(c("PAS.min", "PAD.min", "PAS.SMUR", "PAD.SMUR", 'ACR.1', 'Origine')))

  write.csv(cbind(SAMU, Choc.hemorragique), paste(data_folder,'hemo_shock_new.csv', sep=''), row.names = F)
}

clean_trauma_new()
