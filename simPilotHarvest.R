####################################################################################################
####################################################################################################
###### Harvest simulation pilot
######
###### Prepares simulation inputs
###### Deploys experimental design
###### Each harvesting treatemts (rates) are currently simulated manually
###### (could easily be automated)
######
###### Dominic Cyr, in collaboration with Tadeusz Splawinski and Sylvie Gauthier
####################################################################################################
####################################################################################################
rm(list=ls())
setwd("~/Travail/SCF/regenFailureRiskAssessment")
## fire simulations folder
outputFolder <- paste(getwd(), "outputs/", sep="/")
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
require(dplyr)
####################################################################################################
####################################################################################################
coverTypes <- raster("../data/coverTypes.tif")
coverTypesLevels <- distinct(get(load("../data/coverTypesDf.RData"))[,c("ID", "descrip")])
studyArea <- raster("../data/studyArea.tif")

### harvesting parameters (to be updated)
harvestedCoverTypes <- c("EN", "PG")
maturityCoverTypes <- c(EN = 90, PG = 76)
harvestingRates <- list(EN = c(0.0051, 0.0083, 0.01, 0.011, 0.0125, 0.015),
                        PG = c(0.0051, 0.0083, 0.01, 0.011, 0.0125, 0.015))



ID <- coverTypesLevels[match(harvestedCoverTypes, coverTypesLevels$descrip), "ID"]


for (h in seq_along(harvestingRates[[1]])) { ### this loops through harvesting threatments 
    
    hr <- as.numeric(lapply(harvestingRates, function(x) x[[h]]))
    names(hr) <- names(harvestingRates)

    prescriptions <- data.frame(ID = ID,
                                coverType = harvestedCoverTypes,
                                maturity = maturityCoverTypes,
                                rate = hr)
    
    coverTypes[coverTypes %in% ID == F] <- NA
    
    
    ####################################################################################################
    ####################################################################################################
    ######
    ######      simulations
    source("../scripts/simHarvestFnc.R")
    ####################################################################################################
    ###################################################################################################
    ### fetching fire simulations
    x <- list.files(outputFolder)
    x <- x[grep(".RData", x)]
    simInfo <- gsub(".RData", "", x)
    simInfo <- strsplit(simInfo, "_")
    scenario <- as.character(lapply(simInfo, function(x) x[[2]]))
    replicates <- as.character(lapply(simInfo, function(x) x[3]))
    ####################################################################################################
    ####################################################################################################
    
    
    
    
    require(doSNOW)
    require(parallel)
    clusterN <-  max(1, floor(0.9*detectCores()))  ### choose number of nodes to add to cluster.
    # #######
    cl = makeCluster(clusterN, outfile = "") ## 
    registerDoSNOW(cl)
    
    
    ##
    
    foreach(i = seq_along(x)) %dopar%  {
    
        require(raster)
        print(paste("simulating replicate", replicates[i], "; scenario", scenario[i]))
        
        output <- get(load(paste(outputFolder, x[i], sep ="/")))
        timeSinceFire <- output[["tsf"]]
        rm(output)
        
        timeSinceFire[is.na(coverTypes)] <- NA
        
        outputHarvest <- simHarvest(coverTypes,
                                    timeSinceFire,
                                    prescriptions)
        
        fName <- paste0(getwd(), "/simHarvest_", prescriptions[1,4], "_", scenario[i], "_", replicates[i], ".RData")
        print("##############################################################")
        print("##############################################################")
        print(paste0("Simulation #", i, " completed.")) 
    
        save(outputHarvest, file = fName)
        print(paste0("Done! Outputs saved to file '", fName, "'"))
        print("##############################################################")
        
    }
    stopCluster(cl)

}
#######