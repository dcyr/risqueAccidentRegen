
rm(list=ls())

# library(caret)
require(randomForest)
# library(plyr); library(dplyr)
# library(reshape2)








nameDirec<-"C:\\rf\\bon\\pourDC_nouveau\\"
nom<-"fakeData.csv"


#trainName<-"datSif_RF.csv"

setwd(nameDirec)

#train<-read.csv(trainName,header=T)
peup<-read.csv(nom,header=T)


#rfclaDensEsse <-  get(load("pred_CombiDensEsse.R"))
rfClasDens <-  get(load("pred_clasDens.RData"))
rfEssReg<-  get(load("pred_essReg.RData"))



ini<-Sys.time()



#On fait la prediction
prClasDens<-predict(rfClasDens, peup, type= "prob")
prEsse<-predict(rfEssReg, peup, type= "prob")

#On assigne une transition en focntion des probabilit?s et un nombre al?atoire (f uniforme)
probTra<-data.frame(matrix(ncol =6,nrow=nrow(peup) ))
probTra[,1]<-prClasDens[,1]
for(i in 2:4){probTra[,i]<-rowSums(prClasDens[,1:i])}
probTra[,5]<-runif(nrow(peup), 0,1)
probTra$X6<-ifelse(probTra$X1>=probTra$X5,"AB",
                   ifelse(probTra$X2>=probTra$X5,"C",
                          ifelse(probTra$X3>=probTra$X5,"D","E")))

# claDens pr?dite
peup$clasDens_Fin<-probTra$X6
#colnames(peup)[ncol(peup)]<-"clasDens_Fin"

#   *   *    *         *

probTra[,1]<-prEsse[,1]
for(i in 2:4){probTra[,i]<-rowSums(prEsse[,1:i])}
probTra[,5]<-runif(nrow(peup), 0,1)
probTra$X6<-ifelse(probTra$X1>=probTra$X5,"EPN",
                   ifelse(probTra$X2>=probTra$X5,"FEU",
                          ifelse(probTra$X3>=probTra$X5,"PIG","RES")))

# essReg_Fin pr?dite
peup$essReg_Fin<-probTra$X6
#colnames(peup)[ncol(peup)]<-"essReg_Fin"







summary(peup)


# Columns avec les seuils de Tadeusz et l'incertitude (?cart Type =1/30 du seuil)
# seuils en format equation (cheap equation)

peup$sEPN_PP<--(0.5429*peup$IQSp^3)+(25.79 * peup$IQSp^2)-(409.83*peup$IQSp)+2211.1
peup$sEPN_AR<-peup$sEPN_PP/1.6666
peup$sPIG_PP<- -(0.2148*peup$IQSp^3) +(9.8147*peup$IQSp^2) - (150.52*peup$IQSp)+ 798.07
peup$sPIG_AR<-peup$sPIG_PP/1.5


## incertitude autour des seuils (écart-type /30)
peup$sEPN_PP<-rnorm(nrow(peup),peup$sEPN_PP,(peup$sEPN_PP/30))
peup$sEPN_AR<-rnorm(nrow(peup),peup$sEPN_AR,(peup$sEPN_AR/30))

peup$sPIG_PP<-rnorm(nrow(peup),peup$sPIG_PP,(peup$sPIG_PP/30))
peup$sPIG_AR<-rnorm(nrow(peup),peup$sPIG_AR,(peup$sPIG_AR/30))
peup[,(ncol(peup)+1)]<-runif(nrow(peup),0,1) ## conversion résineux -> PG


##
table(peup$essReg_Fin)
table(peup$clasDens_Fin)
# Modification des classes de tdensit?en fonction des seuils pour EPN, PIG  (AR et PP)

peup[which(peup$essReg_Ini=="EPN" & peup$ageAvant<=peup$sEPN_AR),"clasDens_Fin"]<-"E"
peup[which(peup$essReg_Ini=="EPN" & peup$ageAvant<=peup$sEPN_AR),"essReg_Fin"]<-"E/vide"
peup[which(peup$essReg_Ini=="PIG" & peup$ageAvant<=peup$sPIG_AR),"clasDens_Fin"]<-"E"
peup[which(peup$essReg_Ini=="PIG" & peup$ageAvant<=peup$sPIG_AR),"essReg_Fin"]<-"E/vide"

peup[which(peup$essReg_Ini=="EPN" & 
             peup$ageAvant<=peup$sEPN_PP & peup$ageAvant>peup$sEPN_AR &
             peup$clasDens_Fin=="AB" & peup[,ncol(peup)]< 0.666),"clasDens_Fin"]<-"C"
peup[which(peup$essReg_Ini=="PIG" & 
             peup$ageAvant<=peup$sEPN_PP & peup$ageAvant>peup$sEPN_AR &
             peup$clasDens_Fin=="AB" & peup[,ncol(peup)]< 0.666),"clasDens_Fin"]<-"C"


peup[which(peup$essReg_Ini=="EPN" & 
             peup$ageAvant<=peup$sEPN_PP & peup$ageAvant>peup$sEPN_AR &
             peup$clasDens_Fin=="AB" & peup[,ncol(peup)]>= 0.666),"clasDens_Fin"]<-"D"
peup[which(peup$essReg_Ini=="PIG" & 
             peup$ageAvant<=peup$sEPN_PP & peup$ageAvant>peup$sEPN_AR &
             peup$clasDens_Fin=="AB" & peup[,ncol(peup)]>= 0.666),"clasDens_Fin"]<-"D"


peup[which(peup$essReg_Ini=="EPN" & 
             peup$ageAvant<=peup$sEPN_PP & peup$ageAvant>peup$sEPN_AR &
             peup$clasDens_Fin=="C"),"clasDens_Fin"]<-"D"
peup[which(peup$essReg_Ini=="PIG" & 
             peup$ageAvant<=peup$sEPN_PP & peup$ageAvant>peup$sEPN_AR &
             peup$clasDens_Fin=="C"),"clasDens_Fin"]<-"D"

### accidents ou pas dans la classe D (pourrait être E)
peup[which(peup$essReg_Ini=="EPN" & 
             peup$ageAvant<=peup$sEPN_PP & peup$ageAvant>peup$sEPN_AR &
             peup$clasDens_Fin=="D" & peup[,ncol(peup)]< 0.666),"clasDens_Fin"]<-"D"
peup[which(peup$essReg_Ini=="PIG" & 
             peup$ageAvant<=peup$sEPN_PP & peup$ageAvant>peup$sEPN_AR &
             peup$clasDens_Fin=="D" & peup[,ncol(peup)]< 0.666),"clasDens_Fin"]<-"D"

peup[which(peup$essReg_Ini=="EPN" & 
             peup$ageAvant<=peup$sEPN_PP & peup$ageAvant>peup$sEPN_AR &
             peup$clasDens_Fin=="D" & peup[,ncol(peup)]>= 0.666),"clasDens_Fin"]<-"E"
peup[which(peup$essReg_Ini=="PIG" & 
             peup$ageAvant<=peup$sEPN_PP & peup$ageAvant>peup$sEPN_AR &
             peup$clasDens_Fin=="D" & peup[,ncol(peup)]>= 0.666),"clasDens_Fin"]<-"E"

peup[which(peup$clasDens_Fin=="E"), "essReg_Fin"]<-"E/vide"


# On efface les 4 columns cr?es pour les seuils d'AR-?ge
nc<-ncol(peup)

peup[,((nc-4):(nc-1))]<-NULL


peup[which(peup$essReg_Fin=="RES"& peup[,ncol(peup)]>0.6666),"essReg_Fin"]<-"PIG"
peup[which(peup$essReg_Fin=="RES"& peup[,ncol(peup)]<=0.6666),"essReg_Fin"]<-"EPN"

## mise à jour des variables 'densité' et 'essence'
peup$clasDens_Fin<-factor(peup$clasDens_Fin)
peup$essReg_Fin<-factor(peup$essReg_Fin)

peup[,ncol(peup)]<-NULL

summary(peup)

Sys.time()-ini










#

