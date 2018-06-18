
rm(list=ls())

# library(caret)
require(randomForest)
# library(plyr); library(dplyr)
# library(reshape2)





rfclaTranEsse <-  get(load("pred_CombiDensEsse.R"))
# rfClaTran <-  get(load("pred_claTra.R"))
# rfEssReg<-  get(load("pred_essReg.R"))



nameDirec<-"C:\\rf\\bon\\pourDC_nouveau\\"
nom<-"fakeData.csv"

#trainName<-"datSif_RF.csv"

setwd(nameDirec)

#train<-read.csv(trainName,header=T)
peup<-read.csv(nom,header=T)

ini<-Sys.time()

probUnif<-runif(nrow(peup), 0,1)

prfCombi<-predict(rfclaTranEsse, peup, type= "prob")

##On assigne une transition en focnction des probabilités et d'un nombre aléatoire (f uniforme)

probTra<-data.frame(matrix(ncol =15,nrow=nrow(peup)))
probTra[,1]<-prfCombi[,1]
for(j in 2:13){probTra[,j]<-rowSums(prfCombi[,1:j])}
probTra[,14]<-probUnif
probTra$X15<-ifelse(probTra$X1>=probTra$X14,"AB_EPN",
                    ifelse(probTra$X2>=probTra$X14,"AB_FEUsab",
                           ifelse(probTra$X3>=probTra$X14,"AB_PIG",
                                  ifelse(probTra$X4>=probTra$X14,"AB_RES",
                                         ifelse(probTra$X5>=probTra$X14,"C_EPN",
                                                ifelse(probTra$X6>=probTra$X14,"C_FEUsab",
                                                       ifelse(probTra$X7>=probTra$X14,"C_PIG",
                                                              ifelse(probTra$X8>=probTra$X14,"C_RES",
                                                                     ifelse(probTra$X9>=probTra$X14,"D_EPN",
                                                                            ifelse(probTra$X10>=probTra$X14,"D_FEUsab",
                                                                                   ifelse(probTra$X11>=probTra$X14,"D_PIG",
                                                                                          ifelse(probTra$X12>=probTra$X14,"D_RES","E_E/vide"))))))))))))



#
peup$pred<-probTra$X15
peup$pred<-factor(peup$pred)

summary(peup)


#res$clasDensCombi<-"E"
peup$clasDens<-substr(peup[,"pred"], 1, 1)
peup[which(peup$clasDens=="A"), "clasDens"]<-"AB"

peup$clasEsse<-"E/vide"
peup[which(peup$clasDens=="AB"), "clasEsse"]<-substr(peup[which(peup$clasDens=="AB"),"pred"], 4, 9)
peup[which(peup$clasDens=="C"), "clasEsse"]<-substr(peup[which(peup$clasDens=="C"),"pred"], 3, 9)
peup[which(peup$clasDens=="D"), "clasEsse"]<-substr(peup[which(peup$clasDens=="D"),"pred"], 3, 9)









# Columns avec les seuils de Tadeusz et l'incertitude (écart Type =1/30 du seuil)
# seuils en format equation (cheap equation)

peup$sEPN_PP<--(0.5429*peup$IQSp^3)+(25.79 * peup$IQSp^2)-(409.83*peup$IQSp)+2211.1
peup$sEPN_AR<-peup$sEPN_PP/1.6666
peup$sPIG_PP<- -(0.2148*peup$IQSp^3) +(9.8147*peup$IQSp^2) - (150.52*peup$IQSp)+ 798.07
peup$sPIG_AR<-peup$sPIG_PP/1.5



peup$sEPN_PP<-rnorm(nrow(peup),peup$sEPN_PP,(peup$sEPN_PP/30))
peup$sEPN_AR<-rnorm(nrow(peup),peup$sEPN_AR,(peup$sEPN_AR/30))

peup$sPIG_PP<-rnorm(nrow(peup),peup$sPIG_PP,(peup$sPIG_PP/30))
peup$sPIG_AR<-rnorm(nrow(peup),peup$sPIG_AR,(peup$sPIG_AR/30))
peup[,(ncol(peup)+1)]<-runif(nrow(peup),0,1)



table(peup$clasEsse)
table(peup$clasDens)
# Modification des classes de tdensitéen fonction des seuils pour EPN, PIG  (AR et PP)

peup[which(peup$essReg_Ini=="EPN" & peup$ageAvant<=peup$sEPN_AR),"clasDens"]<-"E"
peup[which(peup$essReg_Ini=="EPN" & peup$ageAvant<=peup$sEPN_AR),"clasEsse"]<-"E/vide"
peup[which(peup$essReg_Ini=="PIG" & peup$ageAvant<=peup$sPIG_AR),"clasDens"]<-"E"
peup[which(peup$essReg_Ini=="PIG" & peup$ageAvant<=peup$sPIG_AR),"clasEsse"]<-"E/vide"

peup[which(peup$essReg_Ini=="EPN" & 
             peup$ageAvant<=peup$sEPN_PP & peup$ageAvant>peup$sEPN_AR &
             peup$clasDens=="AB" & peup[,ncol(peup)]< 0.666),"clasDens"]<-"C"
peup[which(peup$essReg_Ini=="PIG" & 
             peup$ageAvant<=peup$sEPN_PP & peup$ageAvant>peup$sEPN_AR &
             peup$clasDens=="AB" & peup[,ncol(peup)]< 0.666),"clasDens"]<-"C"


peup[which(peup$essReg_Ini=="EPN" & 
             peup$ageAvant<=peup$sEPN_PP & peup$ageAvant>peup$sEPN_AR &
             peup$clasDens=="AB" & peup[,ncol(peup)]>= 0.666),"clasDens"]<-"D"
peup[which(peup$essReg_Ini=="PIG" & 
             peup$ageAvant<=peup$sEPN_PP & peup$ageAvant>peup$sEPN_AR &
             peup$clasDens=="AB" & peup[,ncol(peup)]>= 0.666),"clasDens"]<-"D"


peup[which(peup$essReg_Ini=="EPN" & 
             peup$ageAvant<=peup$sEPN_PP & peup$ageAvant>peup$sEPN_AR &
             peup$clasDens=="C"),"clasDens"]<-"D"
peup[which(peup$essReg_Ini=="PIG" & 
             peup$ageAvant<=peup$sEPN_PP & peup$ageAvant>peup$sEPN_AR &
             peup$clasDens=="C"),"clasDens"]<-"D"


peup[which(peup$essReg_Ini=="EPN" & 
             peup$ageAvant<=peup$sEPN_PP & peup$ageAvant>peup$sEPN_AR &
             peup$clasDens=="D" & peup[,ncol(peup)]< 0.666),"clasDens"]<-"D"
peup[which(peup$essReg_Ini=="PIG" & 
             peup$ageAvant<=peup$sEPN_PP & peup$ageAvant>peup$sEPN_AR &
             peup$clasDens=="D" & peup[,ncol(peup)]< 0.666),"clasDens"]<-"D"

peup[which(peup$essReg_Ini=="EPN" & 
             peup$ageAvant<=peup$sEPN_PP & peup$ageAvant>peup$sEPN_AR &
             peup$clasDens=="D" & peup[,ncol(peup)]>= 0.666),"clasDens"]<-"E"
peup[which(peup$essReg_Ini=="PIG" & 
             peup$ageAvant<=peup$sEPN_PP & peup$ageAvant>peup$sEPN_AR &
             peup$clasDens=="D" & peup[,ncol(peup)]>= 0.666),"clasDens"]<-"E"

peup[which(peup$clasDens=="E"), "clasEsse"]<-"E/vide"


# On éfface les 4 columns crées pour les seuils d'AR-Âge
nc<-ncol(peup)

peup[,((nc-4):(nc-1))]<-NULL


peup[which(peup$clasEsse=="RES"& peup[,ncol(peup)]>0.6666),"clasEsse"]<-"PIG"
peup[which(peup$clasEsse=="RES"& peup[,ncol(peup)]<=0.6666),"clasEsse"]<-"EPN"


peup$clasDens<-factor(peup$clasDens)
peup$clasEsse<-factor(peup$clasEsse)

peup[,ncol(peup)]<-NULL

summary(peup)

Sys.time()-ini










#

