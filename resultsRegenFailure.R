####################################################################################################
####################################################################################################
###### regenFailureCalculation
######
###### Dominic Cyr, in collaboration with Tadeusz Splawinski and Sylvie Gauthier
rm(list = ls())
setwd("~/Travail/SCF/regenFailureRiskAssessment")
####################################################################################################
####################################################################################################
wwd <- paste(getwd(), Sys.Date(), sep = "/")
dir.create(wwd)
setwd(wwd)
#################
require(rgdal)
require(raster)
require(rgeos)
require(dplyr)
require(doSNOW)
require(parallel)
require(foreach)
###################
coverTypes <- get(load("../data/coverTypesDf.RData"))
studyArea <- raster("../data/studyArea.tif")
tsfInit <- get(load("../data/tsfInitDf.RData"))
colnames(tsfInit)[3] <- "tsfInit"
##
fireZones <- raster("../data/fireZones.tif")
fireZones <- rasterToPoints(fireZones)
fireZoneNames <- distinct(read.csv("../data/fireZoneTable.csv")[,c("ID", "Zone_LN")])
##
df <- merge(coverTypes, tsfInit)
df <- merge(df, fireZones)
################################################################################
################################################################################
## computing total areas for each subzones
dfArea <- df %>%
    group_by(fireZones, descrip) %>%
    summarise(areaTotal_ha = n()*25)
dfArea <- as.data.frame(dfArea)
colnames(dfArea)[colnames(dfArea)=="descrip"] <- "cover"
colnames(dfArea)[colnames(dfArea)=="fireZones"] <- "ID"
## adding 
dfArea[,"Zone_LN"] <- fireZoneNames[match(dfArea[, "ID"], fireZoneNames$ID), "Zone_LN"]


nTreatments <- 3
prodClasses <- c("low","intermediate", "high")
#### (30, 10); (40, 20); (50, 30); (70, 50); (90, 70); (110, 90);, and (130, 110).
maturity <- list(EN = c(30, 50, 90),
                 PG = c(10, 30, 70),
                 R = rep(0, nTreatments),
                 F = rep(0, nTreatments))

### writing csv
write.csv(dfArea, file = "regenArea.csv", row.names = F)



################################################################################
################################################################################
outputDir <- "../outputs"
outputs <- list.files(outputDir)
filesFire <- outputs[intersect(grep("Fire", outputs), grep(".RData", outputs))]
filesHarvest <- outputs[intersect(grep("Harvest", outputs), grep(".RData", outputs))]


simInfo <- gsub(".RData", "", filesFire)
simInfo <- strsplit(simInfo, "_")
scenario <- as.character(lapply(simInfo, function(x) x[[2]]))
replicates <- as.character(lapply(simInfo, function(x) x[3]))


simInfo <- gsub(".RData", "", filesHarvest)
simInfo <- strsplit(simInfo, "_")
harvestRates <- unique(as.character(lapply(simInfo, function(x) x[[2]])))
rm(simInfo)


# ################################################################################
# ###############################################################################
# ## computing pre-fire conditions
# clusterN <-  max(1, floor(0.8*detectCores()))  ### choose number of nodes to add to cluster.
# for (hr in c("0.0051", "0.01", "0.015")) {
#     index <- grep(hr, filesHarvest)
#     filesHarvestSubset <- filesHarvest[index]
# 
#     cl = makeCluster(clusterN, outfile = "")
#     registerDoSNOW(cl)
# 
#     preFireConditions <- foreach(i = seq_along(filesFire), .combine = "rbind") %dopar%  {
#         require(reshape2)
#         require(data.table)
#         require(stringr)
#         require(foreach)
#         require(raster)
#         r <- replicates[i]
#         s <- scenario[i]
# 
#         fires <- get(load(paste(outputDir, filesFire[i], sep = "/")))
#         harvests <- get(load(paste0(outputDir, "/simHarvest_", hr, "_", s, "_", r, ".RData")))
# 
#         fires$tsf[is.na(studyArea)] <- NA
# 
# 
#         tsf <- crop(fires$tsf, c(range(df$x),
#                                  range(df$y)))
#         timesteps <- as.numeric(gsub("[^0-9]", "", names(tsf)))
#         names(tsf) <- paste0("F", timesteps)
# 
#         tsh <- crop(harvests, c(range(df$x),
#                                 range(df$y)))
# 
# 
# 
#         tsd <- list()
#         ### extracting minimum values (time since last disturbance)
#         for (l in 1:nlayers(tsf)) {
#             x <- stack(tsf[[l]], tsh[[l]])
#             tsd[[l]] <- min(x, na.rm = T)
# 
#         }
#         tsd <- stack(tsd)
#         names(tsd) <- paste0("D", timesteps)
# 
#         ### formating tsf
#         x <- rasterToPoints(tsf)
#         tsf <- merge(df, x, all = F)
# 
#         ### formating tsd
#         x <- rasterToPoints(tsd)
#         tsd <- merge(df, x, all = F)
# 
#         productive <- rep(1, nrow(tsd))
# 
#         x <- foreach(j = seq_along(timesteps), .combine = "rbind") %do% {
#             ts <- timesteps[j]
#             colIndex <- which(colnames(tsf) == paste0("F", ts))
#             ## tsf == 0focussing on fire pixels to identify regen failures
#             index <- which(tsf[,colIndex]==0)
#             cover <- tsf[index, "descrip"]
#             fireZone <- tsf[index, "fireZones"]
#             if (j == 1) {
#                 tsfPrefire <- tsdPrefire <- tsf[index, "tsfInit"]
# 
#             } else {
#                 tsfPrefire <- tsf[index, colIndex-1]
#                 tsdPrefire <- tsd[index, colIndex-1]
#             }
#             dfTmp <- data.frame(tsfPrefire, tsdPrefire, cover, fireZone)
#             if (nrow(dfTmp) > 0) {
#                 dfTmp["timestep"] <- ts
#                 dfTmp["simID"] <- r
#                 dfTmp["scenario"] <- s
#                 dfTmp["harvestRate"] <- as.numeric(hr)
#                 return(dfTmp)
#             }
# 
#         }
#         print(paste("scenario", s, "; replicate", r, "; harvest rate ", as.numeric(hr)))
#         return(x)
#     }
#     stopCluster(cl)
#     #################
#     save(preFireConditions, file = paste0("outputCompiledPreFireConditions_hr", hr, ".RData"))
#     #################
#     rm(preFireConditions)
# }

################################################################################
################################################################################
### Subsetting (using 'outputCompiledFinal')
outputCompiled <- get(load("../compiledOutputs/outputCompiledFinalEnsemble.RData"))
rcp85Rep <- filter(outputCompiled, scenario == "RCP85")[,"replicate"]
rcp85Rep <- unique(rcp85Rep[order(rcp85Rep)])

harvestRates <- c("0.0051", "0.010", "0.015")
dfSummary <- foreach(i = seq_along(harvestRates), .combine = "rbind") %do%  {
    preFireConditions <- get(load(paste0("../compiledOutputs/outputCompiledPreFireConditions_hr", harvestRates[i], ".RData")))
    preFireBaseline <- filter(preFireConditions, scenario =="baseline")
    preFireRCP85 <- preFireConditions %>%
        filter(scenario =="RCP85") %>%
        mutate(replicate = as.numeric(simID)) %>%
        filter(replicate %in% rcp85Rep)
    preFireConditions <- rbind(preFireBaseline, preFireRCP85[,colnames(preFireBaseline)])
    rm(preFireBaseline, preFireRCP85)

    #################
    tmp <- foreach(j = 1:nTreatments, .combine = "rbind") %do% {

        dfSummary <- preFireConditions %>%
            filter(cover %in% c("EN", "PG", "R", "F")) %>%
            select(scenario, harvestRate, simID, timestep, fireZone, cover, tsfPrefire, tsdPrefire)

        m <- rapply(maturity, function(x) x[j])
        dfSummary <- data.frame(dfSummary,
                                maturity = m[as.character(dfSummary$cover)],
                                productivity = prodClasses[j])

        dfSummary <- dfSummary%>%
            mutate(burnedImmature = tsdPrefire < maturity,
                   harvestTreatment = harvestRates[i])

        if (i == 1) {## just to compute once instead of a few times unnecessarily
            dfSummary2 <- dfSummary %>%
                mutate(burnedImmature = tsfPrefire < maturity,
                       harvestTreatment = 0) #%>%
            dfSummary <- rbind(dfSummary, dfSummary2)
            rm(dfSummary2)
        }
        dfSummary <- dfSummary %>%
            group_by(scenario, harvestTreatment, productivity, simID, timestep, fireZone, cover, burnedImmature) %>%
            summarise(area_ha = n()*25)



        dfSummary <- dfSummary %>%
            select(scenario, harvestTreatment, productivity, simID, timestep, fireZone, cover, burnedImmature, area_ha) %>%
            arrange(scenario, harvestTreatment, simID, timestep, fireZone, cover, burnedImmature, area_ha)

        dfSummary[, "Zone_LN"] <- fireZoneNames[match(dfSummary$fireZone, fireZoneNames$ID), "Zone_LN"]


        return(dfSummary)
    }
    rm(preFireConditions)
    return(tmp)
}
write.csv(dfSummary, file = "regenSummary.csv", row.names = F)
# 
# 
# 
# ################################################################################
# dfSummary <- read.csv("../compiledOutputs/regenSummary.csv")
# 
# ### renaming some levels for nicer plotting 
# 
# dfSummary$harvestTreatment <- paste0("HarvestRate: ", 100*dfSummary$harvestTreatment, " %/yr")
# harvestTreatment <- unique(dfSummary$harvestTreatment)
# 
# dfSummary$harvestTreatment <- factor(dfSummary$harvestTreatment,
#                                      levels = harvestTreatment)
# 
# 
# prodNames <- c(high = "Early maturity",
#                intermediate = "Intermediate maturity",
#                low = "Late maturity")
# 
# dfSummary$productivity <- prodNames[dfSummary$productivity]
# ################################################################################
# #### computing results by cover type zone
# dfAreaCover <- dfArea %>%
#     group_by(cover) %>%
#     summarize(areaCover_ha = sum(areaTotal_ha))
# 
# 
# dfCover <- dfSummary %>%
#     filter(burnedImmature == T) %>%
#     #filter(simID == "000") %>%
#     group_by(scenario, harvestTreatment, productivity, simID, cover, timestep, burnedImmature) %>%
#     summarize(area_ha = sum(area_ha)) %>%
#     merge(dfAreaCover) %>%
#     mutate(propBurned = area_ha/areaCover_ha) %>%
#     ungroup() %>%
#     arrange(timestep) %>%
#     group_by(scenario, harvestTreatment, productivity, simID, cover, burnedImmature) %>%
#     mutate(cumulpropBurned = cumsum(propBurned)) 
# 
# 
# 
# #####
# 
# #####
# dfCoverTotal <- dfCover %>%
#     group_by(scenario, harvestTreatment, productivity, simID, timestep, burnedImmature) %>%
#     summarize(area_ha = sum(area_ha),
#               areaCover_ha = sum(dfAreaCover$areaCover_ha)) %>%
#     mutate(propBurned = area_ha/areaCover_ha) %>%
#     ungroup() %>%
#     arrange(timestep) %>%
#     group_by(scenario, harvestTreatment, productivity, simID, burnedImmature) %>%
#     mutate(cumulpropBurned = cumsum(propBurned),
#            cover = "Global") 
# 
# 
# dfCover <- rbind(dfCover, dfCoverTotal)
# 
# 
# # computing mean regen failure rates per simulation
# dfCover <- dfCover %>%
#     group_by(scenario, harvestTreatment, productivity, cover, simID) %>%
#     summarise(meanRate = sum(propBurned)/50) %>%
#     merge(dfCover)
# 
# # 
# # coverLevels <- c(EN = "Épinette noire",
# #                  PG = "Pin gris",
# #                  R = "Résineux ind.", 
# #                  F = "Feuillus intolérants",
# #                  Global = "Global")
# coverLevels <- c(EN = "Black Spruce",
#                  PG = "Jack Pine",
#                  R = "Ind.Softwood", 
#                  F = "Intolerant hardwoods",
#                  Global = "Global")
# 
# 
# dfCover$cover <- factor(coverLevels[as.character(dfCover$cover)],
#               levels = coverLevels)
# 
# 
# ## pad dfCover at timestep == 50 for when there's been no fire
# dfCoverPad <- dfCover %>%
#     group_by(cover, harvestTreatment, productivity, scenario, simID) %>%
#     filter(timestep == timestep[which.max(timestep)]) %>%
#     ungroup() %>%
#     mutate(area_ha = ifelse(timestep<50, 0, area_ha),
#            propBurned = ifelse(timestep<50, 0, propBurned)) %>%
#     mutate(timestep = 50)
# 
# 
# ### adding/replacing timestep == 50 with padding df
# dfCover <- dfCover %>%
#     filter(timestep != 50) %>%
#     rbind(dfCoverPad)
# 
# ####
# write.csv(dfCover, file = "dfCover.csv", row.names = F)
# ####
# 
# dfCoverPercentiles <- dfCover %>%
#     group_by(scenario, harvestTreatment, productivity, timestep, cover) %>%
#     summarise(p.050 = quantile(cumulpropBurned, 0.05, na.rm = T),
#               p.100 = quantile(cumulpropBurned, 0.1, na.rm = T),
#               p.250 = quantile(cumulpropBurned, 0.25, na.rm = T),
#               p.500 = quantile(cumulpropBurned, 0.5, na.rm = T),
#               p.750 = quantile(cumulpropBurned, 0.75, na.rm = T),
#               p.900 = quantile(cumulpropBurned, 0.9, na.rm = T),
#               p.950 = quantile(cumulpropBurned, 0.95, na.rm = T),
#               cumulPropMean = mean(cumulpropBurned, na.rm = T),
#               medianRate = median(meanRate))
# 
# ### make percentiles monotenous 
# for (ts in 1:50) {
#     dfCoverPercentiles <- dfCoverPercentiles %>%
#         filter(timestep <= ts) %>%
#         group_by(scenario, harvestTreatment, productivity, cover) %>%
#         summarize(p.050 = max(p.050),
#                   p.100 = max(p.100),
#                   p.250 = max(p.250),
#                   p.500 = max(p.500),
#                   p.750 = max(p.750),
#                   p.900 = max(p.900),
#                   p.950 = max(p.950),
#                   cumulPropMean = max(cumulPropMean),
#                   medianRate = max(medianRate),
#                   timestep = ts) %>%
#         rbind(filter(dfCoverPercentiles, timestep != ts))
# }
# 
# 
# #### summary table by decade
# ## zero pad
# dfTmp <- filter(as.data.frame(dfCoverPercentiles), timestep == 1)
# dfTmp[,c(4:11,13)] <- 0
# 
# 
# dfCoverPercentilesDecade <- as.data.frame(dfCoverPercentiles) %>%
#     filter(timestep %in% c(10,20,30,40,50)) %>%
#     rbind(dfTmp) %>%
#     arrange(scenario, harvestTreatment, productivity, cover, timestep) %>%
#     group_by(scenario, harvestTreatment, productivity, cover) %>%
#     mutate(diff_p.050 = c(NA, diff(p.050)),
#            diff_p.100 = c(NA, diff(p.100)),
#            diff_p.250 = c(NA, diff(p.250)),
#            diff_p.500 = c(NA, diff(p.500)),
#            diff_p.750 = c(NA, diff(p.750)),
#            diff_p.900 = c(NA, diff(p.900)),
#            diff_p.950 = c(NA, diff(p.950)),
#            diff_mean = c(NA, diff(cumulPropMean))) %>%
#     filter(timestep > 0) %>%
#     mutate(timestep = timestep - 5)
# 
# dfCoverPercentilesDecade <- select(dfCoverPercentilesDecade,
#                                    scenario, harvestTreatment, productivity, timestep, cover,
#                                    diff_mean,
#                                    diff_p.050,
#                                    diff_p.100,
#                                    diff_p.250,
#                                    diff_p.500,
#                                    diff_p.750,
#                                    diff_p.900,
#                                    diff_p.950)
# write.csv(dfCoverPercentilesDecade, file = "dfCoverPercentilesDecade.csv", row.names = F)
# 
# 
# 
# dfCover <- merge(dfCover, dfCoverPercentiles)
# 
# 
# 
# 
# 
# 
# # sName <- c(baseline = "Scénario de référence",
# #            RCP85 = "Scénario RCP 8.5")
# sName <- c(baseline = "Baseline scenario",
#            RCP85 = "RCP 8.5 scenario")
# 
# 
# labelDF <- dfCoverPercentiles %>%
#     filter(timestep == 50) %>%
#     select(scenario, cover, harvestTreatment, productivity, timestep, medianRate)
# labelDF$productivity <-  factor(labelDF$productivity, levels =  c("Early maturity",
#                                                                   "Intermediate maturity",
#                                                                   "Late maturity"))
# 
# labelDF$xLab <- factor(gsub("HarvestRate: ", "", labelDF$harvestTreatment), levels = c("0 %/yr","0.51 %/yr","1 %/yr","1.5 %/yr"))
# 
# write.csv(labelDF, file = "dfFailureMedianRate.csv", row.names = F)
# prodClassesLabel <- c("early", "intermediate", "late")
# 
# ### summary histogram
# cols = c("Black Spruce" = "darkolivegreen",
#          "Jack Pine" = "gold2",
#          "Global" = "grey85")
# require(ggplot2)
# p <- ggplot(labelDF, aes(fill = cover, y = medianRate*100, x = xLab)) +
#     geom_col(position = position_dodge(width=0.9)) +
#     scale_fill_manual(name = "Cover type", values=cols) +
#     theme_dark() +
#     facet_grid(scenario~productivity) +
#     labs(y = "Median annual rate (%)",
#          x = "Harvesting rate",
#          caption = paste0("Sexual maturity (years) - Black Spruce: ", paste(prodClassesLabel, maturity[["EN"]], collapse = " ; "), "\n",
#                           "Jack Pine: ", paste(prodClassesLabel, maturity[["PG"]], collapse = " ; ")),
#          title = "Median annual proportion of the landscape where immature stands were burned")
# 
# png(filename = "immatureBurnsCover_medianRateSummary.png",
#     width = 8, height = 6, units = "in", res = 600, pointsize=8)
#     print(p + theme(legend.position="top", legend.direction="horizontal",
#                     axis.text.x = element_text(angle = 45, hjust = 1)))
# dev.off()
# 
# 
# 
# 
# p <- c(p.050 = "5%", p.250 = "25%", p.500 = "médiane", p.750 = "75%", p.950 = "95%")
# 
# require(ggplot2)
# for (s in c("baseline", "RCP85")) {
#     
#     ### selecting subsample and fetching treatment specific parameters
#     # sexual maturity
#     prodLabel <- "late" # c("Early maturity", "Intermediate maturity", "Late maturity")
#     prodNames <- unique(dfCover$productivity)[grep(prodLabel, tolower(unique(dfCover$productivity)))]
#     prodIndex <- grep(prodLabel, prodClassesLabel)
# 
#     
#     for (i in c("all", "Global")) {
#         if (i == "all") {
#             df <- dfCover %>%
#                 filter(scenario == s,
#                        productivity == prodNames) %>%
#                 mutate(ID = as.numeric(as.factor(paste(scenario, harvestTreatment, simID))))
#             labels <- labelDF %>%
#                 filter(scenario == s,
#                        productivity == prodNames) 
#             fName <- paste0("immatureBurnsCover_", s, "_", prodLabel, ".png")
#             figHeight <- 6
#         }
#         if (i == "Global") {
#             df <- dfCover %>%
#                 filter(scenario == s,
#                        cover == i) %>%
#                 mutate(ID = as.numeric(as.factor(paste(scenario, harvestTreatment, simID))))
#             labels <- labelDF %>%
#                 filter(scenario == s,
#                        cover == i)  
#             fName <- paste0("immatureBurnsGlobal_", s, ".png")
#             figHeight <- 6
#         }
# 
#         nRep <- length(unique(df$simID))
# 
#         #options(scipen=999)
#         m <- ggplot(df, aes(x=timestep + 2015, y = 100*cumulpropBurned, group = ID)) +
#             geom_line(colour = "black", alpha = 0.1) +#fill = "grey25"
#             geom_line(aes(y = 100*p.500, group = 1),
#                       colour = "lightblue",
#                       linetype = 1, size = 0.7, alpha = 1) + #fill = "grey25"
#             geom_line(aes(y = 100*p.050, group = 1),
#                       colour = "yellow",
#                       linetype = 3, size = 0.3, alpha = 1) +
#             geom_line(aes(y = 100*p.950, group = 1),
#                       colour = "yellow",
#                       linetype = 3, size = 0.3, alpha = 1) +
#             geom_line(aes(y = 100*p.250, group = 1),
#                       colour = "yellow",
#                       linetype = 4, size = 0.4, alpha = 1) +
#             geom_line(aes(y = 100*p.750, group = 1),
#                       colour = "yellow",
#                       linetype = 4, size = 0.4, alpha = 1) +
#             theme_dark() #+
#             # geom_smooth(span = 0.7, aes_string(y = names(p)[2], group = 1),
#             #             colour="lightblue", linetype = 1, size = 0.7, alpha = 1) +
#             # geom_smooth(span = 0.2, aes_string(y = names(p)[1], group = 1),
#             #             colour = "yellow",
#             #             linetype = 4, size = 0.5, alpha = 1) +
#             # geom_smooth(span = 0.2, aes_string(y = names(p)[3], group = 1),
#             #             colour = "yellow",
#             #             linetype = 4, size = 0.5, alpha = 1) 
#         
#         
#         yMax <- layer_scales(m)$y$range$range[2]
#         xMin <- layer_scales(m)$x$range$range[1]
#         ### arranging facets according to dataset
#         if (i == "Global") {
#             m <- m + facet_grid(productivity~harvestTreatment) +
#                 labs(caption = paste0("Sexual maturity thresholds (years) - Black Spruce: ", paste(prodClassesLabel, maturity[["EN"]], collapse = " ; "), "\n",
#                                       "Jack Pine: ", paste(prodClassesLabel, maturity[["PG"]], collapse = " ; ")))
#         }
#         if (i == "all") {
#             m <- m + facet_grid(cover~harvestTreatment) +
#                 labs(caption = paste0("Sexual maturity thresholds (years): Black Spruce ", maturity[["EN"]][prodIndex], " ; ",
#                                       "Jack Pine ", maturity[["PG"]][prodIndex]))
#         }
#         
#        
# 
#         
#         png(filename = fName,
#             width = 7.5, height = figHeight, units = "in", res = 600, pointsize=8)
#         
#         
#         print(m + theme_dark() +
#                   theme(legend.position="top", legend.direction="horizontal",
#                         axis.text.x = element_text(angle = 45, hjust = 1),
#                         strip.text.y = element_text(size = 8)) +
#                   #labs(title = paste0("Proportion cumulative du territoire productif où une forêt immature a brûlé\n",
#                   labs(title = paste0("Cumulative proportion of productive area where immature stands were burned\n",
#                                       sName[s]),
#                        subtitle = #paste0("En bleu sont illustrées les médianes et en jaune les percentiles ",
#                            paste0("Median scenarios are highlighted in blue and percentiles ",
#                                          # p[1], ", ", p[2], ", ", p[4], " et ", p[5],
#                                          p[1], ", ", p[2], ", ", p[4], " and ", p[5],
#                                          # ",\nsur un total de ", nRep, " réalisations."),
#                         #sur un total de ", nRep, " réalisations."),
#                        " in yellow\n(Total of ", nRep, " realizations)"),
#                        
#                        x = "",
#                        #y = "Proportion cumulée")  +
#                        y = "Cumulative area (%)")  +
#                   geom_text(aes(x = xMin, y = yMax, group = NULL,
#                                 #label = paste0("taux annuel médian: ", round(100*medianRate, 3), "%")),
#                                 label = paste0("median annual rate: ", round(100*medianRate, 3), "%")),
#                             data = labels,
#                             hjust = 0, size = 3, fontface = 1))
#     
#         dev.off()
#     }
# }
# 
# 
# #####################################################################################
# #####################################################################################
# ###########################################################
# #### computing results by fire zone
# dfAreaZone <- dfArea %>%
#     group_by(Zone_LN) %>%
#     summarize(areaZone_ha = sum(areaTotal_ha))
# 
# dfZone <- dfSummary %>%
#     filter(burnedImmature == T) %>%
#     group_by(scenario, harvestTreatment, productivity, simID, Zone_LN, timestep) %>%
#     summarize(area_ha = sum(area_ha)) %>%
#     merge(dfAreaZone) %>%
#     mutate(propBurned = area_ha/areaZone_ha) %>%
#     ungroup() %>%
#     arrange(timestep) %>%
#     group_by(scenario, harvestTreatment, productivity, simID, Zone_LN) %>%
#     mutate(cumulpropBurned = cumsum(propBurned)) 
# 
# 
# dfZoneTotal <- dfZone %>%
#     group_by(scenario, harvestTreatment, productivity, simID, timestep) %>%
#     summarize(area_ha = sum(area_ha),
#               areaZone_ha = sum(dfAreaZone$areaZone_ha)) %>%
#     mutate(propBurned = area_ha/areaZone_ha) %>%
#     ungroup() %>%
#     arrange(timestep) %>%
#     group_by(scenario, harvestTreatment, productivity, simID) %>%
#     mutate(cumulpropBurned = cumsum(propBurned),
#            Zone_LN = "Global") 
# 
# 
# 
# dfZone <- rbind(dfZone, dfZoneTotal)
# 
# # computing mean regen failure rates per simulation
# dfZone <- dfZone %>%
#     group_by(scenario, harvestTreatment, productivity, Zone_LN, simID) %>%
#     summarise(meanRate = sum(propBurned)/50) %>%
#     merge(dfZone)
# 
# ### pad dfCover at timestep == 50 for when there's been no fire
# dfZonePad<- dfZone %>%
#     group_by(Zone_LN, harvestTreatment, productivity, scenario, simID) %>%
#     filter(timestep == timestep[which.max(timestep)]) %>%
#     ungroup() %>%
#     mutate(area_ha = ifelse(timestep<50, 0, area_ha),
#            propBurned = ifelse(timestep<50, 0, propBurned)) %>%
#     mutate(timestep = 50)
# 
# ### adding/replacing timestep == 50 with padding df
# dfZone <- dfZone %>%
#     filter(timestep != 50) %>%
#     rbind(dfZonePad)
# 
# 
# 
# dfZonePercentiles <- dfZone %>%
#     group_by(scenario, harvestTreatment, productivity, timestep, Zone_LN) %>%
#     summarise(p.050 = quantile(cumulpropBurned, 0.05, na.rm = T),
#               p.100 = quantile(cumulpropBurned, 0.1, na.rm = T),
#               p.250 = quantile(cumulpropBurned, 0.25, na.rm = T),
#               p.500 = quantile(cumulpropBurned, 0.5, na.rm = T),
#               p.750 = quantile(cumulpropBurned, 0.75, na.rm = T),
#               p.900 = quantile(cumulpropBurned, 0.9, na.rm = T),
#               p.950 = quantile(cumulpropBurned, 0.95, na.rm = T),
#               cumulPropMean = mean(cumulpropBurned, na.rm = T),
#               medianRate = median(meanRate))
# 
# 
# ### make percentiles monotenous
# for (ts in 1:50) {
#     dfZonePercentiles <- dfZonePercentiles %>%
#         filter(timestep <= ts) %>%
#         group_by(scenario, harvestTreatment, productivity, Zone_LN) %>%
#         summarize(timestep = ts,
#                   p.050 = max(p.050),
#                   p.100 = max(p.100),
#                   p.250 = max(p.250),
#                   p.500 = max(p.500),
#                   p.750 = max(p.750),
#                   p.900 = max(p.900),
#                   p.950 = max(p.950),
#                   cumulPropMean = max(cumulPropMean),
#                   medianRate = max(medianRate)) %>%
#         rbind(filter(dfZonePercentiles, timestep != ts))
# }
# 
# # #### summary table by decade
# # ## zero pad
# # dfTmp <- filter(as.data.frame(dfZonePercentiles), timestep == 1)
# # head(dfTmp)
# # dfTmp[,c(4:11)] <- 0
# # 
# # 
# # dfZonePercentilesDecade <- as.data.frame(dfZonePercentiles) %>%
# #     filter(timestep %in% c(10,20,30,40,50)) %>%
# #     rbind(dfTmp) %>%
# #     arrange(scenario, harvestTreatment, Zone_LN, timestep) %>%
# #     group_by(scenario, harvestTreatment, Zone_LN) %>%
# #     mutate(diff_p.050 = c(NA, diff(p.050)),
# #            diff_p.100 = c(NA, diff(p.100)),
# #            diff_p.250 = c(NA, diff(p.250)),
# #            diff_p.500 = c(NA, diff(p.500)),
# #            diff_p.750 = c(NA, diff(p.750)),
# #            diff_p.900 = c(NA, diff(p.900)),
# #            diff_p.950 = c(NA, diff(p.950))) %>%
# #     filter(timestep > 0) %>%
# #     mutate(timestep = timestep - 5)
# # 
# # dfZonePercentilesDecade <- select(dfZonePercentilesDecade,
# #                                    scenario, harvestTreatment, timestep, Zone_LN,
# #                                    diff_p.050,
# #                                    diff_p.100,
# #                                    diff_p.250,
# #                                    diff_p.500,
# #                                    diff_p.750,
# #                                    diff_p.900,
# #                                    diff_p.950)
# # write.csv(dfZonePercentilesDecade, file = "dfZonePercentilesDecade.csv", row.names = F)
# 
# ####################
# 
# 
# 
# 
# 
# labelDF <- dfZonePercentiles %>%
#     filter(timestep == 50) %>%
#     select(Zone_LN, scenario, harvestTreatment, productivity, timestep, medianRate)
# 
#   
# dfZone <- merge(dfZone, dfZonePercentiles)
# 
# require(ggplot2)
# #options(scipen=999)
# 
# for (s in c("baseline", "RCP85")) {
#     ### selecting subsample and fetching treatment specific parameters
#     # sexual maturity
#     prodLabel <- "late" # c("Early maturity", "Intermediate maturity", "Late maturity")
#     prodNames <- unique(dfZone$productivity)[grep(prodLabel, tolower(unique(dfZone$productivity)))]
#     prodIndex <- grep(prodLabel, prodClassesLabel)
#    
#     df <- dfZone %>%
#         filter(scenario == s,
#                productivity ==prodNames) %>%
#         mutate(ID = as.numeric(as.factor(paste(scenario, harvestTreatment, simID))))
#     nRep <- length(unique(df$simID))
#     
#     labels <- filter(labelDF, scenario == s,
#                      productivity ==prodNames)
#     fName <- paste0("immatureBurnsFireZone_", s, "_", prodLabel, ".png")
#     
#     
#     m <- ggplot(df, aes(x = timestep + 2015, y = cumulpropBurned, group = ID)) +
#         #ylim(c(0,0.5)) +
#         geom_line(colour = "black", alpha = 0.1) +#fill = "grey25"
#         geom_line(aes_string(y = names(p)[3], group = 1),
#                   colour = "lightblue",
#                   linetype = 1, size = 0.7, alpha = 1) + #fill = "grey25"
#         geom_line(aes_string(y = names(p)[1], group = 1),
#                   colour = "yellow",
#                   linetype = 3, size = 0.3, alpha = 1) +
#         geom_line(aes_string(y = names(p)[5], group = 1),
#                   colour = "yellow",
#                   linetype = 3, size = 0.3, alpha = 1) +
#         geom_line(aes_string(y = names(p)[2], group = 1),
#                   colour = "yellow",
#                   linetype = 4, size = 0.4, alpha = 1) +
#         geom_line(aes_string(y = names(p)[4], group = 1),
#                   colour = "yellow",
#                   linetype = 4, size = 0.4, alpha = 1) +
#         #fill = "grey25"
#         #geom_line(colour = "black", alpha = 0.1) +#fill = "grey25"
#         facet_grid(Zone_LN~harvestTreatment) +
#         theme_dark() #+
#         # geom_smooth(span = 0.7, aes_string(y = names(p)[2], group = 1),
#         #             colour="lightblue", linetype = 1, size = 0.7, alpha = 1) #+
#         # geom_line(aes_string(y = names(p)[3], group = 1),
#         #             colour = "red",
#         #             linetype = 1, size = 0.25, alpha = 1) +
#         # geom_smooth(span = 0.5,
#         #             method = "lm",
#         #             aes_string(y = names(p)[3], group = 1),
#         #             colour = "red",
#         #             linetype = 4, size = 0.5, alpha = 1) +
#         # geom_smooth(span = 0.7, aes_string(y = names(p)[3], group = 1),
#         #             colour = "yellow",
#         #             linetype = 4, size = 0.5, alpha = 1) +
#         # geom_smooth(span = 0.7, aes_string(y = names(p)[1], group = 1),
#         #             colour = "yellow",
#         #             linetype = 4, size = 0.5, alpha = 1)
#     
#     yMax <- layer_scales(m)$y$range$range[2]
#     xMin <- layer_scales(m)$x$range$range[1]
#     
#     png(filename=fName,
#         width = 7.5, height = 12, units = "in", res = 600, pointsize=10)
#     options(scipen=999)
#     
#     print(m + theme_dark() +
#               
#               theme(legend.position="top", legend.direction="horizontal",
#                     axis.text.x = element_text(angle = 45, hjust = 1),
#                     strip.text.y = element_text(size = 8))+
#               labs(title = paste0("Cumulative proportion of productive area where immature stands were burned\n",
#                                   sName[s]),
#                    subtitle = #paste0("En bleu sont illustrées les médianes et en jaune les percentiles ",
#                        paste0("Median scenarios are highlighted in blue and percentiles ",
#                               # p[1], ", ", p[2], ", ", p[4], " et ", p[5],
#                               p[1], ", ", p[2], ", ", p[4], " and ", p[5],
#                               # ",\nsur un total de ", nRep, " réalisations."),
#                               #sur un total de ", nRep, " réalisations."),
#                               " in yellow\n(Total of ", nRep, " realizations)"),
#                    
#                    x = "",
#                    #y = "Proportion cumulée")  +
#                    y = "Cumulative area (%)")  +
#               geom_text(aes(x = xMin, y = yMax, group = NULL,
#                             #label = paste0("taux annuel médian: ", round(100*medianRate, 3), "%")),
#                             label = paste0("median annual rate: ", round(100*medianRate, 3), "%")),
#                         data = labels,
#                         hjust = 0, size = 3, fontface = 1))
# 
#  
#     
#     dev.off()
# }
# 
# 
# 
# ### Figure synthèse (toute les courbes)
# 
# df <- dfZone %>%
#     filter(Zone_LN == "Global") %>%
#     group_by(scenario, harvestTreatment, productivity, timestep) %>%
#     summarize(p.050 = unique(p.050),
#               p.250 = unique(p.250),
#               p.500 = unique(p.500),
#               p.750 = unique(p.750),
#               p.950 = unique(p.950))
# df$treatment <- c("0%", "0.51%", "1%", "1.5%")[match(df$harvestTreatment, unique(df$harvestTreatment))]
# 
# write.csv(df, file = "summaryMedian.csv", row.names = T)
# 
# 
# nRep <- nrow(distinct(dfZone[,c("scenario", "simID")]))
# m <- ggplot(df, aes(x = timestep + 2015, y = p.500*100,
#                     color = scenario, linetype = treatment)) +
#     facet_wrap( ~ productivity) +
#     geom_line(size = 0.75) +
#     scale_color_manual("", values = c(baseline = "darkseagreen3",
#                                       RCP85 ="darkred"),
#                        # labels=c(baseline = "scénario de référence",
#                        #          RCP85 = "scénario RPC 8.5")) +
#                        labels=c(baseline = "Baseline",
#                                 RCP85 = "RPC 8.5")) +
#     # scale_fill_manual("", values = c(baseline = "darkolivegreen3",
#     #                                   RCP85 ="red3"),
#     #                    labels=c(baseline = "scénario de référence",
#     #                             RCP85 = "scénario RPC 8.5")) +
#     #scale_linetype_manual("Taux de récolte\nannuel", values = c(1, 2, 3, 4)) 
#     scale_linetype_manual("Annual\nharvesting rate", values = c(1, 2, 3, 4)) 
#     
# 
# 
# png(filename= paste0("immatureBurnsSummary.png"),
#     width = 8, height = 4, units = "in", res = 600, pointsize=10)
# options(scipen=999)
# 
#     print(m + theme_dark() +
#               
#               theme(legend.position="top", legend.direction="horizontal",
#                     legend.title = element_text(size = rel(0.85)),
#                     title = element_text(size = rel(0.85))) +
#               labs(title = "Cumulative proportion of productive area where immature stands were burned",
#                    subtitle = paste0("Median estimates, computed from an ensemble of ", nRep,  " realizations."),
#                    caption = paste0("Sexual maturity thresholds (years) - Black Spruce: ", paste(prodClassesLabel, maturity[["EN"]], collapse = " ; "), "\n",
#                                     "Jack Pine: ", paste(prodClassesLabel, maturity[["PG"]], collapse = " ; ")),
#                    x = "",
#                    y = "Cumulative area (%)"))
#               # labs(title = "Figure 10: Proportion cumulative du territoire productif où une forêt immature a brûlé",
#               #      subtitle = paste0("Valeurs médianes, issues d'un ensemble de ", nRep,  " simulations."),
#               #      caption = paste0("Maturité épinette: ", maturity["EN"], " ans; ",
#               #                       "Maturité pin gris: ",maturity["PG"], " ans"),
#               #      x = "",
#               #      y = "Proportion cumulée"))
# 
# 
# dev.off()

