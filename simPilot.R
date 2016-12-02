####################################################################################################
####################################################################################################
###### Simulation pilot
######
###### Prepares simulation inputs
###### Deploys experimental design
######
###### Dominic Cyr, in collaboration with Tadeusz Splawinski and Sylvie Gauthier
####################################################################################################
####################################################################################################
rm(list=ls())
setwd("~/Travail/SCF/regenFailureRiskAssessment")
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
fireRegimeAttrib <- read.csv("../data/fireZones.csv")
# temporary initial tsf
tsfInit <- fireZones


####################################################################
####################################################################
### empirical fire size distribution
require(MASS)
require(dplyr)
fireObs <- read.csv("../data/fireObs.csv", header=TRUE)


# fires <- read.table("feux_Zones_Gauthier2015.txt")
# colnames(fires) <- c("year", "areaTotal_ha", "areaZone_ha", "zone")
# write.csv(fires, "fireObs.csv", row.names = F)


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
source("../scripts/simFnc.R")
####################################################################################################
####################################################################################################
nRep <- 999
simDuration <- 50

### temporary
initialLandscape <- fireZones
n <- sum(values(!is.na(fireZones)))
tsfInit[!is.na(fireZones)] <- 100
#tsfInit[!is.na(fireZones)] <- round(rexp(n, 1/50))
#corr <- 0
require(doSNOW)
require(parallel)
clusterN <-  max(1, floor(0.9*detectCores()))  ### choose number of nodes to add to cluster.
# #######
cl = makeCluster(clusterN, outfile = "") ## 
registerDoSNOW(cl)


##
t1 <- Sys.time()
foreach(i = 1:nrep) %dopar%  {
    require(stringr)
    print(paste("simulating replicate", i))
    output <- sim(tsfInit, simDuration,
                  fireZones,
                  fireRegimeAttrib,
                  fireSizeFit)
    
    fName <- paste0(getwd(), "/simOutput_", str_pad(i, nchar(nRep), pad = "0"), ".RData")
    print("##############################################################")
    print("##############################################################")
    print(paste0("Simulation #", i, " completed.")) 
    save(output, file = fName)
    print(paste0("Done! Outputs saved to file '", fName, "'"))
    print("##############################################################")
    
}
stopCluster(cl)
t2 <- Sys.time()
print(t2-t1)

#######