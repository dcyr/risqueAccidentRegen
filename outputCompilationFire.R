####################################################################
####################################################################
####################################################################
rm(list=ls())
####################################################################
####################################################################
setwd("~/Travail/SCF/regenFailureRiskAssessment/")
outputFolder <- paste(getwd(), "outputs/", sep="/")
wwd <- paste(getwd(), Sys.Date(), sep="/")
dir.create(wwd)
setwd(wwd)
rm(wwd)

####################################################################
####################################################################
######
require(raster)
studyArea <- raster("../data/studyArea.tif")
fireZones <- raster("../data/fireZones.tif")
fireRegimeAttrib <- read.csv("../data/fireZoneTable.csv")
fireZones[is.na(studyArea)] <- NA 
##
convFactor <- prod(res(studyArea))/10000### to convert to hectares
fireZoneArea <- zonal(!is.na(fireZones), fireZones, sum)
fireZoneArea <- data.frame(zone = as.character(fireRegimeAttrib[match(fireZoneArea[,1], fireRegimeAttrib$ID),"Zone_LN"]),
                           areaZone_ha = fireZoneArea[,2] * convFactor)
fireZoneArea <- rbind(fireZoneArea, data.frame(zone = "total", areaZone_ha = sum(fireZoneArea$areaZone_ha)))

####################################################################
####################################################################
######
######      compiling simulation outputs
######
x <- list.files(outputFolder)
index <- grep(".RData", x)
index <- intersect(index, grep("Fire", x))
x <- x[index]
simInfo <- gsub(".RData", "", x)
simInfo <- strsplit(simInfo, "_")
scenario <- as.character(lapply(simInfo, function(x) x[[2]]))
replicates <- as.numeric(lapply(simInfo, function(x) x[3]))
###########################################
###########################################

require(doSNOW)
require(parallel)
require(foreach)
# clusterN <- 2
clusterN <-  max(1, floor(0.9*detectCores()))  ### choose number of nodes to add to cluster.
#######
cl = makeCluster(clusterN, outfile = "") ##
registerDoSNOW(cl)
#######
outputCompiled <- foreach(i = seq_along(x), .combine = "rbind") %dopar% {#
    require(raster)
    require(reshape2)
    require(dplyr)
    s <- scenario[i]
    r <- replicates[i]
    output <- get(load(paste(outputFolder, x[i], sep="/")))
    
    ##
    tsfStack <- output[["tsf"]]

    ## compiling 'true' FC statistics
    tmp <- zonal(tsfStack, fireZones, "mean")
    tmpTotal <- zonal(tsfStack, studyArea, "mean")
    zoneID  <- tmp[,1]
    meanTSF <-  round(cbind(t(tmp[,-1]), tmpTotal[,-1])) 
    
    areaBurned <- t(zonal(tsfStack == 0, fireZones,  "sum")[,-1]) * convFactor
    areaBurned <- data.frame(areaBurned, total = apply(areaBurned, 1, "sum"))
    ##
    year <- as.numeric(gsub("[^0-9]", "", rownames(meanTSF)))
    colnames(meanTSF) <-  colnames(areaBurned) <- c(as.character(fireRegimeAttrib[match(zoneID, fireRegimeAttrib$ID), "Zone_LN"]),
                                                    "total")

    meanTSF <- data.frame(year, replicate = r, meanTSF)
    areaBurned <- data.frame(year, replicate = r, areaBurned)


    meanTSF <- melt(meanTSF, id.vars = c("year", "replicate"),
                    variable.name = "zone", value.name = "meanTSF")
    areaBurned <- melt(areaBurned, id.vars = c("year", "replicate"),
                       variable.name = "zone", value.name = "areaBurned_ha")

    out <- merge(meanTSF, areaBurned)
    out <- data.frame(scenario = s, out)
    out <- arrange(out, scenario, zone, year)
    print(paste(s, r))
    return(out)
}

stopCluster(cl)

outputCompiled <- merge(outputCompiled, fireZoneArea)
save(outputCompiled, file = "outputCompiledFire.RData")
