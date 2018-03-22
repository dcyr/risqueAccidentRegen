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
coverTypes <- raster("../data/coverTypes.tif")
coverTypesDf <- get(load("../data/coverTypesDf.RData"))
coverTypesTable <- distinct(coverTypesDf[,c("ID", "descrip")])
coverTypesTable <- coverTypesTable[which(coverTypesTable$descrip %in% c("EN", "PG")), ]
coverTypes[coverTypes %in% coverTypesTable$ID == F] <- NA
coverTypesID <- unique(values(coverTypes))
coverTypesID <- coverTypesID[!is.na(coverTypesID)]

fireZones <- raster("../data/fireZones.tif")
fireRegimeAttrib <- read.csv("../data/fireZoneTable.csv")
fireZones[is.na(studyArea)] <- NA 
##
convFactor <- prod(res(studyArea))/10000### to convert to hectares
coverTypeArea <-  zonal(!is.na(coverTypes), coverTypes, sum)
coverTypeArea <- data.frame(zone = as.character(coverTypesTable[match(coverTypesID, coverTypesTable$ID),"descrip"]),
                           coverTypeArea_ha = coverTypeArea[,2] * convFactor)


####################################################################
####################################################################
######
######      compiling simulation outputs
######
x <- list.files(outputFolder)
index <- grep(".RData", x)
index <- intersect(index, grep("Harvest", x))
x <- x[index]
simInfo <- gsub(".RData", "", x)
simInfo <- strsplit(simInfo, "_")
harvest <- as.character(lapply(simInfo, function(x) x[[2]]))
scenario <- as.character(lapply(simInfo, function(x) x[[3]]))
replicates <- as.numeric(lapply(simInfo, function(x) x[4]))
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
    h <- harvest[i]
    output <- get(load(paste(outputFolder, x[i], sep="/")))
    
    ## compiling harvested areas
    areaHarvested <- t(zonal(output==0, coverTypes,  "sum")[,-1]) * convFactor
    colnames(areaHarvested) <- coverTypesTable[match(coverTypesID, coverTypesTable$ID), "descrip"]
    
    ##
    year <- as.numeric(gsub("[^0-9]", "", rownames(areaHarvested)))
    
    areaHarvested <- data.frame(year, replicate = r, areaHarvested)


    areaHarvested <- melt(areaHarvested, id.vars = c("year", "replicate"),
                    variable.name = "coverType", value.name = "areaHarvested_ha")
    
    out <- data.frame(scenario = s, harvestTreatment = h, areaHarvested)
    out <- arrange(out, scenario, coverType, year)
    print(paste(s, r, h))
    return(out)
}

stopCluster(cl)

outputCompiled <- merge(outputCompiled, coverTypeArea)
save(outputCompiled, file = "outputCompiledHarvest.RData")
