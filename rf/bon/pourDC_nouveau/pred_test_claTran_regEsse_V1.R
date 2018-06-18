
rm(list=ls())

# on importe les objet RF . Des autres analyses seront faits et les objets devraient être mchangés.
require(gridExtra)
require(randomForest)

rfClaTran <-  get(load("C:/rf/bon/res/pred_claTra.R"))
rfEsse <-  get(load("C:/rf/bon/res/pred_essReg.R"))

varImpPlot(rfClaTran)   #  Importance des variables pour prédire claTran

varImpPlot(rfEsse)   #  Importance des variables pour prédire essReg_Fin


# On introduit les valeurs de Tadeusz qui vont modifier les probabilités RF. 
# Age avant perturbation (ageAvant).

#   Si ageAvant<= SXXX[1]   Accident régénération
#   Si ageAvant<= SXXX[2] & ageAvant> SXXX[1] Perte productivité
#   si ageAvant>SXXX[2]    prediction RF

#  on introduit de l'incertitude avec une fonction normal avec moyen la valeur de Tadeusz 
#  et avec écart type la moyenne /30 . Plus en bas dans le script



# Seuils par essence independantes de l'IQS
#sEPN<-c(30,50)
#sPIG<-c(20,30)
# Avec de l'incertitude
# sEPN<-c(rnorm(1,30,1),rnorm(1,50,1.666))
# sPIG<-c(rnorm(1,20,0.666),rnorm(1,30,1))


# On a réçu le document avec les seuils par espèce et IQS. Pour simplifier le calcul,
# j'ai trouvé une fonction par essence (PIG et EPN) qui prédit le suil d'accident de régénération et
# de perte de productivité en fonction de l'IQS.
# On devrait vérifier tout ça avec Tadeusz






start_time <- Sys.time()

# On cosntruuit une dataframe imitation  des tesselles ou pixels pour predire claTran et essReg_Fin
#    Vecteurs pour initialser le peuplement1


# fois: nombre de pixels  (temps depende de fois mais aussi, emntre autres du nombre d'Arbes de l'objet RF)
#  Mon Ordi
#    1000    1.6 s
#   10000    3.2 s
#  100000   20.0 s
# 1000000  308.0 s

fois<-10000



ageAvant<-rnorm(fois, mean=c(50,130), sd=c(10,22))
#ageAvant<-rnorm(fois, mean=c(20,30), sd=c(1,2))
hist(ageAvant)
min(ageAvant)

clade<-c(rep("AB",36),rep("C",54),rep("D",40))
clasDens_Ini<-sample(clade, fois, replace=TRUE)


esse<-c(rep("PIG",21),rep("EPN",98),rep("FEUsab",10))
essReg_Ini<-sample(esse, fois, replace=TRUE)

rg<-c(rep("6a",99),rep("6c",20),rep("6d",210),rep("6e",3),rep("6f",46),rep("6g",7),rep("6h",5),
      rep("6i",1),rep("6j",5))
regEcol<-sample(rg, fois, replace=TRUE)

cs<-c(rep("Sables Mésiques",13), rep("Sables Hidriques",1), 
      rep("Argiles Mésiques",1), rep("Argiles Hidriques", 4),
      rep("Organic", 10), rep("RocSolMinces",4), rep("Tills",98 )) 
codSol<-sample(cs,fois, replace=TRUE)

alt<-rnorm(fois, 390, 70)
IQSp<-rnorm( fois,12.8,0.8)
IDR100p<-rnorm(fois, 0.41, 0.09)
degJour<-rnorm(fois, 1100, 70)
HLI<-rnorm(fois,0.62, 0.01)
prodPot<-rnorm(fois, mean=c(65,85), sd=c(7,1))
indArid<-rnorm(fois, mean=1.633, sd=0.1)
abonEric<-rnorm(fois, mean=47, sd=8)
preNeig<-rnorm(fois, mean=340, sd=20)


peup<-data.frame(ageAvant, clasDens_Ini, essReg_Ini, regEcol, 
                 codSol, alt, IQSp, IDR100p, degJour,HLI,prodPot,indArid,abonEric,preNeig)
#summary(peup)

# peup$clasDens_Ini<-factor(peup$clasDens_Ini)
# peup$essReg_Ini<-factor(peup$essReg_Ini)
# peup$regEcol<-factor(peup$regEcol)
# peup$codSol<-factor(peup$codSol)
# peup$aspCl<-factor(peup$aspCl)




#On fait la prediction

prClaAR<-predict(rfClaTran, peup, type= "prob")
prEsse<-predict(rfEsse, peup, type= "prob")

#On assigne une transition en focntion des probabilités et un nombre aléatoire (f uniforme)
probTra<-data.frame(matrix(ncol =6,nrow=fois ))
probTra[,1]<-prClaAR[,1]
for(i in 2:4){probTra[,i]<-rowSums(prClaAR[,1:i])}
probTra[,5]<-runif(fois, 0,1)
probTra$X6<-ifelse(probTra$X1>=probTra$X5,"1_STQ",
                   ifelse(probTra$X2>=probTra$X5,"2_AR",
                       ifelse(probTra$X3>=probTra$X5,"3_PP","4_GP")))

# claTran prédite
peup[,ncol(peup)+1]<-probTra$X6
colnames(peup)[ncol(peup)]<-"claTran"

#   *   *    *         *

probTra[,1]<-prEsse[,1]
for(i in 2:4){probTra[,i]<-rowSums(prEsse[,1:i])}
probTra[,5]<-runif(fois, 0,1)
probTra$X6<-ifelse(probTra$X1>=probTra$X5,"EPN",
                   ifelse(probTra$X2>=probTra$X5,"FEU",
                          ifelse(probTra$X3>=probTra$X5,"PIG","RES")))

# essReg_Fin prédite
peup[,ncol(peup)+1]<-probTra$X6
colnames(peup)[ncol(peup)]<-"essReg_Fin"

peup$claTran<-factor(peup$claTran)
peup$essReg_Fin<-factor(peup$essReg_Fin)

#summary(peup)

#
# Columns avec les seuils de Tadeusz et l'incertitude (écart Type =1/30 du seuil)
# seuils en format equation (cheap equation)

peup$sEPN_PP<--(0.5429*peup$IQSp^3)+(25.79 * peup$IQSp^2)-(409.83*peup$IQSp)+2211.1
peup$sEPN_AR<-peup$sEPN_PP/1.6666
peup$sPIG_PP<- -(0.2148*peup$IQSp^3) +(9.8147*peup$IQSp^2) - (150.52*peup$IQSp)+ 798.07
peup$sPIG_AR<-peup$sPIG_PP/1.5

#
# peup<-cbind(peup,rnorm(fois,sEPN[1],sEPN[1]/30),rnorm(fois,sEPN[2],sEPN[2]/30),
#                       rnorm(fois,sPIG[1],sPIG[1]/30),rnorm(fois,sPIG[2],sPIG[2]/30))

peup$sEPN_PP<-rnorm(fois,peup$sEPN_PP,(peup$sEPN_PP/30))
peup$sEPN_AR<-rnorm(fois,peup$sEPN_AR,(peup$sEPN_AR/30))

peup$sPIG_PP<-rnorm(fois,peup$sPIG_PP,(peup$sPIG_PP/30))
peup$sPIG_AR<-rnorm(fois,peup$sPIG_AR,(peup$sPIG_AR/30))

# der<-ncol(peup)-3
# names(peup)[der:(der+3)]<-c("sEPN_AR","sEPN_PP", "sPIG_AR","sPIG_PP")#


# Modification des classes de transitions en fonction des seuils pour EPN, PIG  (AR et PP)

peup[which(peup$essReg_Ini=="EPN" & peup$ageAvant<=peup$sEPN_AR),"claTran"]<-"2_AR"
peup[which(peup$essReg_Ini=="PIG" & peup$ageAvant<=peup$sPIG_AR),"claTran"]<-"2_AR"

peup[which(peup$essReg_Ini=="EPN" & 
             peup$ageAvant<=peup$sEPN_PP & peup$ageAvant>peup$sEPN_AR),"claTran"]<-"3_PP"
peup[which(peup$essReg_Ini=="PIG" & 
             peup$ageAvant<=peup$sPIG_PP & peup$ageAvant>peup$sPIG_AR),"claTran"]<-"3_PP"

# On éfface les 4columns crées pour le suil d'AR-Âge
peup[,(ncol(peup)-3):ncol(peup)]<-NULL





# On attribue la nouvelle classe de densité (clasDens_Fin)

peup$clasDens_Fin<-"E"
peup[which(peup$claTran=="1_STQ" & peup$clasDens_Ini=="AB"), "clasDens_Fin"]<-"AB"
peup[which(peup$claTran=="1_STQ" & peup$clasDens_Ini=="C"), "clasDens_Fin"]<-"C"
peup[which(peup$claTran=="1_STQ" & peup$clasDens_Ini=="D"), "clasDens_Fin"]<-"D"



# 668% des AB_PP deviennet C et 371%  D
peup[,(ncol(peup)+1)]<-runif(fois,0,1)

peup[which(peup$claTran=="3_PP" & peup$clasDens_Ini=="AB"), "clasDens_Fin"]<-"C" ###  On a du margin 
peup[which(peup$claTran=="3_PP" & peup$clasDens_Ini=="AB" & peup[,ncol(peup)]>0.6282), "clasDens_Fin"]<-"D" 

peup[which(peup$claTran=="4_GP" & peup$clasDens_Ini=="AB"), "clasDens_Fin"]<-"AB"

peup[which(peup$claTran=="3_PP" & peup$clasDens_Ini=="C"), "clasDens_Fin"]<-"D" 
peup[which(peup$claTran=="4_GP" & peup$clasDens_Ini=="C"), "clasDens_Fin"]<-"AB" 


#  80 % des D avec PP on les classe comme AR ????????
peup[which(peup$claTran=="3_PP" & peup$clasDens_Ini=="D"), "clasDens_Fin"]<-"D" 
peup[which(peup$claTran=="3_PP" & peup$clasDens_Ini=="D"), "claTran"]<-"1_STQ"
peup[which(peup$claTran=="3_PP" & peup$clasDens_Ini=="D" & peup[,ncol(peup)]>0.1), "clasDens_Fin"]<-"E" 
peup[which(peup$claTran=="3_PP" & peup$clasDens_Ini=="D" & peup[,ncol(peup)]>0.1), "claTran"]<-"2_AR" 

# 66% des D_GP deviennet C et 33% AB
peup[which(peup$claTran=="4_GP" & peup$clasDens_Ini=="D"), "clasDens_Fin"]<-"C"   ###  On a du margin 
peup[which(peup$claTran=="4_GP" & peup$clasDens_Ini=="D"& peup[,ncol(peup)]>0.6666), "clasDens_Fin"]<-"AB" 



# Si AR, il n'y a pas de regroupement d'essences

peup$essReg_Fin<-as.character(peup$essReg_Fin)
peup[which(peup$claTran=="2_AR"),"essReg_Fin"]<-"E/Vide"
peup[which(peup$essReg_Fin=="RES"& peup[,ncol(peup)]>0.6666),"essReg_Fin"]<-"PIG"
peup[which(peup$essReg_Fin=="RES"& peup[,ncol(peup)]<=0.6666),"essReg_Fin"]<-"EPN"

peup$essReg_Fin<-factor(peup$essReg_Fin)
summary(peup)



peup$clasDens_Fin<-factor(peup$clasDens_Fin)

peup[, ncol(peup)]<-NULL
summary(peup)




tab<-table(peup$clasDens_Ini,peup$claTran)
mosaicplot(tab,main="Transitions par classe de densité",xlab="ClasDens_Ini",ylab="claTran",
           color=c("seashell4", "goldenrod4", "gold3", "forestgreen"))
tab<-table(peup$clasDens_Ini,peup$clasDens_Fin)
mosaicplot(tab,main="Transitions par classe de densité",xlab="ClasDens_Ini",ylab="clasDens_Fin",
           color=c("seashell4", "goldenrod4", "gold3", "forestgreen"))

tab<-table(peup$clasDens_Ini,peup$essReg_Fin)
mosaicplot(tab, main="Essence par classe de densité",xlab="ClasDens_Ini",ylab="essReg_Fin",
           color=c("goldenrod4", "forestgreen","aquamarine3", "seashell4"))


tab<-table(peup$essReg_Ini,peup$claTran)
mosaicplot(tab,main="Transitions par classe d'essence",xlab="ClasDens_Ini",ylab="clasDens_Fin",
           color=c("seashell4", "goldenrod4", "gold3", "forestgreen"))

tab<-table(peup$essReg_Ini,peup$essReg_Fin)
mosaicplot(tab,main="Essence par classe d'essence",xlab="essReg_Ini",ylab="essReg_Fin",
           color=c("goldenrod4", "forestgreen","aquamarine3", "seashell4"))





Sys.time()-start_time 
