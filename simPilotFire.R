####################################################################################################
####################################################################################################
###### Fire simulation pilot
######
###### Prepares simulation inputs
###### Deploys fire simulations
######
###### Dominic Cyr, in collaboration with Tadeusz Splawinski and Sylvie Gauthier
####################################################################################################
####################################################################################################
rm(list=ls())
# setwd("~/Travail/SCF/regenFailureRiskAssessment")
####################################################################################################
####################################################################################################
wwd <- paste(getwd(), Sys.Date(), sep="/")
dir.create(wwd)
setwd(wwd)


####################################################################################################
####################################################################################################
######
######      initial landscape
######
require(raster)
####################################################################################################
####################################################################################################
fireZones <- raster("../data/fireZones.tif")
studyArea <- raster("../data/studyArea.tif")
fireZoneTable <- read.csv("../data/fireZoneTable.csv")
### initial tsf
# set dummy tsf values outside of study area
tsfTmp <- fireZones
tsfTmp[!is.na(fireZones)] <- 100 # dummy tsf
tsfInit <- raster("../data/tsfInit.tif") 
tsfInit <- merge(tsfInit, tsfTmp) # setting values for known tsf
tsfInit[is.na(fireZones)] <- NA

####################################################################
####################################################################
### empirical fire size distribution
require(MASS)
require(dplyr)
fireObs <- read.csv("../data/fireObs.csv", header=TRUE)


### logNormal fit
fireSizeFit <-  list()
for (i in unique(as.character(fireObs$zone))) {
    x <- filter(fireObs, zone == i)$areaTotal_ha
    fireSizeFit[[i]] <- fitdistr(x, "lognormal")
}

####################################################################################################
####################################################################################################
######
######      simulations
######
source("../scripts/simFireFnc.R")
####################################################################################################
####################################################################################################
nRep <- 1000
simDuration <- 50
yearInit <- 2015

require(doSNOW)
require(parallel)
clusterN <-  max(1, floor(0.98*detectCores()))  ### choose number of nodes to add to cluster.
# #######


##
t1 <- Sys.time()
for (s in "RCP85") {#levels(fireZoneTable$scenario)) {
    ### prepare scenario specific inputs
    
    fireRegime <- filter(fireZoneTable, scenario == s)
    fireRegime[, "simYear"] <- as.numeric(substr(fireRegime$period, 1,4)) - yearInit

    # homogenous fire size distribution for RCP85
    if (s == "RCP85") {
        for (z in names(fireSizeFit)) {
            fireSizeFit[[z]] <- fireSizeFit[["G4"]]
        }
        FSD <- fireSizeFit
        
    } else {
        FSD <- fireSizeFit
    }
    
    cl = makeCluster(clusterN, outfile = "") ## 
    registerDoSNOW(cl)
    
    foreach(i = 954:(nRep-1)) %dopar%  {
        require(stringr)
        print(paste("simulating replicate", i))
        output <- simFire(tsfInit, simDuration, yearInit,
                      fireZones,
                      fireRegime,
                      fireSizeFit = FSD)
        
        fName <- paste0(getwd(), "/simFire_", s, "_", str_pad(i, nchar(nRep-1), pad = "0"), ".RData")
        print("##############################################################")
        print("##############################################################")
        print(paste0("Simulation #", i, " completed (", s, ")")) 
        save(output, file = fName)
        print(paste0("Done! Outputs saved to file '", fName, "'"))
        print("##############################################################")
        
    }
    
    stopCluster(cl)
}

t2 <- Sys.time()
print(t2-t1)

#######