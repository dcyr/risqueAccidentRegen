rm(list=ls())

require(rgdal)
require(raster)






dem<- raster("C:\\rf\\bon\\pourDC_nouveau\\geoInfo\\studyarea_dem")
asp= raster("C:\\rf\\bon\\pourDC_nouveau\\geoInfo\\studyarea_asp")
slo= raster("C:\\rf\\bon\\pourDC_nouveau\\geoInfo\\studyarea_slo")


fois<- 10000   # grandeur de l'?chantillon



# sample of size "fois" 

peupl <- sampleRandom(dem, fois, xy = TRUE, sp=TRUE, na.rm = TRUE)
# plot(dem)
# points(peupl)
names(peupl)[3]<-"alt"


peupl$asp = extract(asp, peupl)
peupl$slo = extract(slo, peupl)
#peupl@coords

projSif<-CRS("+proj=longlat +datum=NAD83")
#proj4string(tes) = CRS("+proj=longlat +datum=NAD83")
peuplSif = spTransform(peupl, projSif)
#peuplSif@coords


peuplSif$lon<-peuplSif@coords[,1]
peuplSif$lat<-peuplSif@coords[,2]

#peuSif<-as.data.frame(peuplSif[,])
peuplSif@data<-peuplSif@data[,3:7]

#clacul heat load index Mccune 2002 
summary(peuplSif@data)



peuplSif$asp1<- 180-abs(peuplSif$asp-180)   # aspect folded to 0-180 degrees
peuplSif$HLI<-0.339+(0.808*cos(peuplSif$lat/360*2*pi)*cos(peuplSif$slo/360*2*pi))-
  (0.196*sin(peuplSif$lat/360*2*pi)*sin(peuplSif$slo/360*2*pi))-(0.482*sin(peuplSif$asp1/360*2*pi)*sin(peuplSif$slo/360*2*pi))

peuplSif$asp1<-NULL
#peuSif$latitude<-NULL

summary(peuplSif)



distVar <- readOGR(dsn = "C://rf//bon//pourDC_nouveau//geoInfo", layer = "studyAreaDistEcolVar")
summary(distVar)
crs(distVar)
crs(peuplSif)
distVar@data<-distVar@data[c(20:30,32)]
distVar = spTransform(distVar, crs(peuplSif))
crs(distVar)
crs(peuplSif)

#distVar = spTransform(distVar, CRS(proj4string(peuplSif)))



peuplSif@data<-cbind(peuplSif@data, over(peuplSif, distVar))

summary(peuplSif)
summary(distVar)

peup<-as.data.frame(peuplSif[,])
peup<-peup[,c(1,6,7,9,10,13,14,18)]

rm(peuplSif,dem, asp, slo)


peup[,4]<-as.numeric(as.character(peup[,4]))
peup[,5]<-as.numeric(as.character(peup[,5]))
peup[,6]<-as.numeric(as.character(peup[,6]))
peup[,8]<-as.numeric(as.character(peup[,8]))



summary(peup)
        
 #      *   -    *     -      *   
 #      *   -    *     -      *  
 #      *   -    *     -      *  
 #      *   -    *     -      *  
 
# var_f_t: 
#   ageAvant : ?ge avant perturbation
peup$ageAvant<-rnorm(fois, mean=c(50,130), sd=c(10,22))
hist(peup$ageAvant)
min(peup$ageAvant)

# var_f_perturbation:
#   clasDens_Ini	(AB, C, D)
#   essReg_Ini	(EPN, FEUsab, PIG)

clade<-c(rep("AB",36),rep("C",54),rep("D",40))
peup$clasDens_Ini<-sample(clade, fois, replace=TRUE)
peup$clasDens_Ini<-factor(peup$clasDens_Ini)

esse<-c(rep("PIG",21),rep("EPN",98),rep("FEUsab",10))
peup$essReg_Ini<-sample(esse, fois, replace=TRUE)
peup$essReg_Ini<-factor(peup$essReg_Ini)
 #var_f_tesselle or pixel:
#   codSol
#   IQSp							
#   IDR100p
   
cs<-c(rep("Sables M?siques",13), rep("Sables Hidriques",1), 
      rep("Argiles M?siques",1), rep("Argiles Hidriques", 4),
      rep("Organic", 10), rep("RocSolMinces",4), rep("Tills",98 )) 

peup$codSol<-sample(cs,fois, replace=TRUE)  
peup$codSol<-factor(peup$codSol)

#  Class de Sol Splawinski ---How to get codSol from depSur and drain_comp
#    ****************   INI ******************
#    ****************   INI******************


# vecRocSolMin<-c("R", "R1A", "R4GA", "R7T", "R8E", "8E")
# vecTil<-c("1A", "1AA","1AAM","1AAY","1AB", "1AD", "1AM", "1AY", "1BC","1BD", "1BF", "1BG",
#           "1BI", "1BN", "1BP", "1BT")
# vecSab<-c("2A", "2AE", "2AK", "2AT", "2BD", "2BE", "3AE", "3AN",
#           "4GS", "4P","9S")
# vecArg<-c("4GA", "4GAM", "4GAY")
# vecOrg<-c("7E", "7T", "7TM", "7TY")
# 
# vecMes<-c("00", "10", "11", "16", "20", "21", "23", "24", "30", "31")
# vecHid<-c("40", "41", "42", "43", "44", "50", "51", "52", "53",
#           "54", "60", "61", "63")
# 
# df$codSol<-"vid"
# df$depSur<-factor(df$depSur)
# df$drai_comp<-factor(df$drai_comp)
# 
# 
# 
# 
# 
# df[which(df$depSur %in% vecRocSolMin), "codSol"]<-"RocSolMinces"
# df[which(df$depSur %in% vecTil), "codSol"]<-"Tills"
# 
# df[which(df$depSur %in% vecSab & df$drai_comp %in% vecMes) , "codSol"]<-"Sables M?siques"
# df[which(df$depSur %in% vecSab & df$drai_comp %in% vecHid) , "codSol"]<-"Sables Hidriques"
# 
# df[which(df$depSur %in% vecArg & df$drai_comp %in% vecMes) , "codSol"]<-"Argiles M?siques"
# df[which(df$depSur %in% vecArg & df$drai_comp %in% vecHid) , "codSol"]<-"Argiles Hidriques"
# 
# 
# df[which(df$depSur %in% vecOrg), "codSol"]<-"Organic"
# 
# 
# 
# table(df$codSol)
# table(df$codSol,df$drai_comp)
# table(df$codSol,df$depSur)
# table(df$codSol,df$drai4)
# 
# 
# df$drai_comp<-NULL
# df$depSur<-NULL
# 
# df$codSol<-factor(df$codSol)

#    ****************   FIN ******************
#    ****************   FIN ******************

peup$IQSp<-rnorm( fois,12.8,0.8)
peup$IDR100p<-rnorm(fois, 0.41, 0.09)
## saison croissance en jours
peup$saiCro<-runif(fois,110,140)
 
   
#   
# var_f_pixel:   from "studyArea_dem, *_slo, *_asp  & formula"
#   alt 
#   HLI 
#   
#   
#   
#
#   
# var_f_district: from "studyAreaDistEcolVar.shp" 
#   prodPot
#   degJour 
#   indArid
#   abonEric
#   preTot
#   indArid 
#   saiCro
#   
  #      *   -    *     -      *  
  #      *   -    *     -      *  
  #      *   -    *     -      *  
  #      *   -    *     -      *  



peup<-peup[complete.cases(peup), ]

peup<-peup[,c(9:11,8,1:7,14,12,13)]

summary(peup)


write.csv(peup, file = "C:\\rf\\bon\\pourDC_nouveau\\fakeData.csv", row.names = F)