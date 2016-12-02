####################################################################
####################################################################
###### Simulation function
######
###### Runs one simulation based of inputs provided upstream
###### Outputs TSF maps and/or individual fire map
######
###### Dominic Cyr
####################################################################
####################################################################

sim <- function(tsfInit, simDuration, fireZones, fireRegimeAttrib, fireSizeFit,
                outputTSF = TRUE, outputFire = FALSE) {
    zones <- fireRegimeAttrib$ID
    zoneNames <- as.character(fireRegimeAttrib$Zone_LN)
    fireCycle <- fireRegimeAttrib$Fire_Cycle
    ### reordering fire Size distrib parameters 
    fireSizeFit <- fireSizeFit[match(zoneNames, names(fireSizeFit))]
    
    
    require(raster)
    source("../scripts/fireSpreadFnc.R")

    #### converting number of pixels to hectares
    scaleFactor <- prod(res(initialLandscape))/10000

    ##############################################################
    ##############################################################
    ####  fire regime
    require(MASS)
    ##############################################################
    ##############################################################

    ### average fraction of the "burnable" area burned annually
    fAAB <- 1 / fireCycle
    
    
    fireSizeMean <- numeric()
    for (i in zoneNames) {
        if (i == fireRegimeAttrib[1,"Zone_LN"]) {
            estimates <- names(fireSizeFit[[i]]$estimate)
        }
        if (estimates[1] == "rate") {
            distribType <- "exp"
            fireSizeMean <- append(fireSizeMean, 1000)#append(fireSizeMean, 1/fireSizeFit[[i]]$estimate["rate"])
        }
        if (estimates[1] == "meanlog" & estimates[2] == "sdlog") {
            distribType <- "lognorm"
            fireSizeMean <- append(fireSizeMean,
                                   exp(fireSizeFit[[i]]$estimate["meanlog"] +
                                           0.5*fireSizeFit[[i]]$estimate["sdlog"]^2))
        }
    }

    ########
    ### Fire sequence (annually)
    nFiresMean <- fAAB * zonal(!is.na(fireZones), fireZones, fun='sum')[,"sum"] * scaleFactor / fireSizeMean
    ### Generate a yearly sequence of number of fires
    nFireSequence <- list()
    for(i in seq_along(nFiresMean)) {
        nFireSequence[[i]] <- rpois(lambda = nFiresMean[i], n = simDuration)
    }
    # put that in a matrix where each column corresponds to one fire zone
    nFireSequence <- do.call("cbind", nFireSequence)
    colnames(nFireSequence) <- zoneNames
    ##############################################################
    ##############################################################
    ##### aging and burning
    ##############################################################
    ##############################################################
    if (outputFire) {
        fires <- list()        
    }
    for (y in 1:simDuration) {
        t1 <- Sys.time()
        #### aging landscape
        if (y == 1) {
            tsf <- tsfInit
            timeSinceFire <- list()
        } else {
            tsf <- tsf + 1
        }
        
        nFires <- nFireSequence[y,]
        if (sum(nFires) > 0) { #### skip if no fire events during that year
            
            eligible <- tsf > 0
            ### generating fire sizes
            for (i in seq_along(nFires)) {
                if (i == 1 & exists("fSize")) {
                    rm(fSize)
                }
                n <- nFires[i]
                if (n>0) {

                    if (distribType == "exp") {
                       fTmp <- round(rexp(n, rate = fireSizeFit[[i]]$estimate))
                    }
                    if (distribType == "lognorm") {
                        fTmp <- round(rlnorm(n, meanlog = fireSizeFit[[i]]$estimate[1],
                                              sdlog = fireSizeFit[[i]]$estimate[2]))
                    }
                    fTmp <- data.frame(fireZone = i,
                                       fireSize = fTmp)
                    if (!exists("fSize")) {
                        fSize <- fTmp
                    } else {
                        fSize <- rbind(fSize, fTmp)
                    }
                }
            }
            
            if (sum(values(eligible>0), na.rm = T) > 0) { ## skip if there's nothing eligible to burn
                f <- stack(fireSpread(eligible = eligible,
                                      fireSize = fSize,
                                      fireZones = fireZones))
                if (nlayers(f) == 1) {
                    tsf[f > 0] <- 0
                } else {
                    tsf[calc(f, sum, na.rm = T) > 0] <- 0
                }
                if (outputFire) {
                    fires[[y]] <- f
                } else {
                    rm(f)
                }
            }
            eligible <- tsf > 0     
        
        }

        timeSinceFire[[y]] <- tsf
        t2 <- Sys.time()
        print(paste0("year ", y, " ; ", sum(nFires), " fire(s) ; total ", sum(fSize), " ha"))
        print(round(t2-t1, 2))
    }
    
    ## preparing outputs
    output <- list()
    if (outputFire){ ### these outputs record all individual fires (larger)
        output[["fires"]] <- fires
    }
    if (outputTSF){ ### these outputs record TSF maps
        output[["tsf"]] <- stack(timeSinceFire)
        names(output[["tsf"]]) <- paste0("Y", 1:simDuration)
    }
    return(output)
}

