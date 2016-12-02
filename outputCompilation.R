####################################################################
####################################################################
####################################################################
rm(list=ls())
####################################################################
####################################################################
setwd("~/Travail/SCF/regenFailureRiskAssessment/")
outputFolder <- paste(getwd(), "outputs", sep="/")
wwd <- paste(getwd(), Sys.Date(), sep="/")
dir.create(wwd)
setwd(wwd)
rm(wwd)

####################################################################
####################################################################
######
require(raster)
studyAreas <- raster("../data/studyArea.tif")
fireZones <- raster("../data/fireZones.tif")
fireRegimeAttrib <- read.csv("../data/fireZones.csv")
fireZones[is.na(studyAreas)] <- NA 
##
convFactor <- prod(res(studyAreas))/10000### to convert to hectares
fireZoneArea <- zonal(!is.na(fireZones), fireZones, sum)
fireZoneArea <- data.frame(zone = as.character(fireRegimeAttrib[match(fireZoneArea[,1], fireRegimeAttrib$ID),"Zone_LN"]),
                           areaZone_ha = fireZoneArea[,2] * convFactor)

####################################################################
####################################################################
######
######      compiling simulation outputs
######
x <- list.files(outputFolder)
x <- x[grep(".RData", x)]
simInfo <- gsub(".RData", "", x)
simInfo <- strsplit(simInfo, "_")
replicates <- as.numeric(lapply(simInfo, function(x) x[2]))
###########################################
###########################################


require(doSNOW)
require(parallel)
require(foreach)
clusterN <-  max(1, floor(0.9*detectCores()))  ### choose number of nodes to add to cluster.
# #######
cl = makeCluster(clusterN, outfile = "") ##
registerDoSNOW(cl)

#fireCycle <- meanTSF <- year <- replicate <- areaBurned <- list()
outputCompiled <- foreach(r = seq_along(replicates), .combine = "rbind") %dopar% {#
    require(raster)
    require(reshape2)
    require(dplyr)
    output <- get(load(paste(outputFolder, x[r], sep="/")))

    ##
    tsfStack <- output[["tsf"]]

    ## compiling 'true' FC statistics
    tmp <- zonal(tsfStack, fireZones, "mean")
    zoneID  <- tmp[,1]
    meanTSF <-  round(t(tmp[,-1]))
    areaBurned <- t(zonal(tsfStack == 0, fireZones,  "sum")[,-1]) * convFactor
    ##
    year <- as.numeric(gsub("[^0-9]", "", rownames(meanTSF)))
    colnames(meanTSF) <-  colnames(areaBurned) <- fireRegimeAttrib[match(zoneID, fireRegimeAttrib$ID), "Zone_LN"]

    meanTSF <- data.frame(year, replicate = r, meanTSF)
    areaBurned <- data.frame(year, replicate = r, areaBurned)


    meanTSF <- melt(meanTSF, id.vars = c("year", "replicate"),
                    variable.name = "zone", value.name = "meanTSF")
    areaBurned <- melt(areaBurned, id.vars = c("year", "replicate"),
                       variable.name = "zone", value.name = "areaBurned_ha")

    out <- merge(meanTSF, areaBurned)
    out <- arrange(out, zone, year)

    return(out)
}

stopCluster(cl)

outputCompiled <- merge(outputCompiled, fireZoneArea)
summary(outputCompiled)
save(outputCompiled, file = "outputCompiled.RData")
