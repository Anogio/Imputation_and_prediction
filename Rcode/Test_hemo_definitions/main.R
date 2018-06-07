library(tidyverse)
library(stringr)

data_folder = '../../Data/'
traumdata <- read.csv(file=paste(data_folder,'data_trauma.csv', sep=''),
                      header = TRUE,na.strings = c("","NR","IMP","NA","NF"),encoding = "UTF-8")

traumdata.keep.IMP <- read.csv(file=paste(data_folder,'data_trauma.csv', sep=''),
                      header = TRUE,na.strings = c("","NR","NA","NF"),encoding = "UTF-8")

# If the pressure cannot be taken it means that it is low!
traumdata$PAS[traumdata.keep.IMP$PAS=='IMP'] = 30
traumdata$PAS.min[traumdata.keep.IMP$PAS.min=='IMP'] = 30
traumdata$PAS.SMUR[traumdata.keep.IMP$PAS.SMUR=='IMP'] = 30
rm(traumdata.keep.IMP)

# Recode death codes
traumdata$Cause.du.DC<-as.character(traumdata$Cause.du.DC)
traumdata[which(traumdata[ ,"Cause.du.DC"]=="Choc hémorragique"|
                  traumdata[ ,"Cause.du.DC"]=="Choc hemorragique"|
                  traumdata[ ,"Cause.du.DC"]=="Exsanguination" ), "Cause.du.DC"]<-"Exsanguination"

############################
# Def 1 : PROPPR study

# ABC score computation
type.mecanisme.perforant = c('Arme à feu', 'Arme blanche')
mecanisme.perforant = traumdata$Mecanisme %in% type.mecanisme.perforant

lowest.PAS = pmin(traumdata$PAS.SMUR, traumdata$PAS.min, traumdata$PAS, na.rm=T) # Are values of 0 actual low pressures or no record??
low.PAS = !is.na(lowest.PAS) & (lowest.PAS <= 90) # NA values as considered as normal (same afterwards)
vasopressor = !is.na(traumdata$Dose.NAD.depart) & (traumdata$Dose.NAD.depart > 0)

highest.HR = pmax(traumdata$FC.SMUR, traumdata$FC.max, traumdata$FC, na.rm = T)
high.HR = !is.na(highest.HR) & (highest.HR >= 120)

positive.FAST = !is.na(traumdata$FAST.hemoperitoine) & (traumdata$FAST.hemoperitoine == 1)

ABC = mecanisme.perforant + (low.PAS | vasopressor) + high.HR + positive.FAST
high.ABC = ABC>=2

# Transfusion first hour
transfusion.firstH = !is.na(traumdata$Nombre.CGR.dechoc) & (traumdata$Nombre.CGR.dechoc >= 1)

crit1 = high.ABC & transfusion.firstH

#############################
# Def 2 : PROMMTT study [3]:

# Cannot be done with current data

############################
# Def 3 : NAD study

crit3 = traumdata$Choc.hemorragique == 1

############################
# Def 4 : Red flag study

high.lactates = !is.na(traumdata$Lactates) & (traumdata$Lactates >= 5)

surgery.performed = !is.na(traumdata$Bloc.direct) & (traumdata$Bloc.direct == 1)

hemo.shock.death = !is.na(traumdata$Cause.du.DC) & (traumdata$Cause.du.DC == "Exsanguination")

crit4 = transfusion.firstH | traumdata$Choc.hemorragique | high.lactates | surgery.performed | hemo.shock.death

############################
# Def 5 : Resuscitation Outcome Consortium criteria

high.HR2 = !is.na(highest.HR) & (highest.HR >= 108)
HR.PAS.abnormal = high.HR2 & low.PAS
very.low.PAS = !is.na(lowest.PAS) & (lowest.PAS <= 70)
physiological.crit = HR.PAS.abnormal | very.low.PAS


unstable.pelvis = (!is.na(traumdata$Bilan.lesion.AIS)) & str_detect(traumdata$Bilan.lesion.AIS,
                             "85610.3|856151.3|856161.3|856171.4|856162.4|856163.4|856164.5|856172.4|856173.5|856174.5")
# Count positive FAST regions
positive.FAST.hemothorax = (!is.na(traumdata$FAST.hemothorax) & traumdata$FAST.hemothorax==1)
positive.FAST.epanchement = (!is.na(traumdata$FAST.epanchement.pericardique) & traumdata$FAST.epanchement.pericardique==1)
positive.FAST.hemoperitoine = (!is.na(traumdata$FAST.hemoperitoine) & traumdata$FAST.hemoperitoine==1)
positive.FAST.pneumothorax = (!is.na(traumdata$FAST.pneumothorax) & traumdata$FAST.pneumothorax==1)
multiple.positive.FAST = (positive.FAST.hemothorax + positive.FAST.epanchement +
                          positive.FAST.hemoperitoine + positive.FAST.pneumothorax) >= 2

pragmatic.crit = mecanisme.perforant | unstable.pelvis | multiple.positive.FAST

crit5 = physiological.crit & pragmatic.crit

###############################
# Evaluation

criteria = data.frame(c1=crit1, c3=crit3, c4=crit4, c5=crit5)
as.data.frame(table(criteria, useNA = 'ifany'))
colSums(criteria)


traumdata = cbind(traumdata, criteria)
traumdata$shockdeath = hemo.shock.death

getMonth = function(datetime){
  t =  strptime(datetime, format = '%d/%m/%y')
  return(t$mon + 12*t$year)
}
traumdata$arrivalMonth = sapply(traumdata$Date.entree, getMonth)
traumdata %>%
  select(arrivalMonth,c1,c3,c4,c5, shockdeath) %>%
  group_by(arrivalMonth) %>%
  summarise_all(funs(sum)) %>%
  gather('Criterion', 'Number_positives', -c(arrivalMonth, shockdeath)) %>%
  ggplot() + aes(x=arrivalMonth/12+1900, y=Number_positives, color=Criterion) + geom_line() +
    geom_line(aes(y=shockdeath, color='Haemorrhagic shock deaths')) +
  xlim(c(2010,NA)) + xlab('Year (one point per month)') + ylab('Number of patients matching criterion')
