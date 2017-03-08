####################################################################################################
####################################################################################################
###### Preparation of initial landscape (for regeneration model)
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


########################################
########################################
coverTypes <- get(load("../data/coverTypesDf.RData"))
tsfInit <- get(load("../data/tsfInitDf.RData"))
colnames(tsfInit)[3] <- "tsfInit"
##
fireZones <- raster("../data/fireZones.tif")
fireZones <- rasterToPoints(fireZones)
##
df <- merge(coverTypes, tsfInit)
df <- merge(df, fireZones)

########################################
########################################
outputDir <- "../outputs"
outputs <- list.files(outputDir)
outputs <- outputs[grep(".RData", outputs)]
simInfo <- gsub(".RData", "", outputs)
simInfo <- strsplit(simInfo, "_")
scenario <- as.character(lapply(simInfo, function(x) x[[2]]))
replicates <- as.numeric(lapply(simInfo, function(x) x[3]))

clusterN <-  max(1, floor(0.95*detectCores()))  ### choose number of nodes to add to cluster.

cl = makeCluster(clusterN, outfile = "") ##
registerDoSNOW(cl)
outputCompiled <- foreach(i = seq_along(outputs), .combine = "rbind") %dopar%  { # .combine = "rbind") { %do% {
    require(reshape2)
    require(data.table)
    require(stringr)
    require(foreach)
    require(raster)
    output <- get(load(paste(outputDir, outputs[i], sep = "/")))
    simID <- gsub("[^0-9]", "", outputs[i])
    s <- scenario[i]

    tsf <- crop(output$tsf, c(range(df$x),
                              range(df$y)))

    x <- rasterToPoints(tsf)

    x <- merge(df, x, all = F)
    tsf <- x[,grep("Y", colnames(x))]

    timestep <- as.numeric(gsub("Y", "", colnames(tsf)))


    x <- foreach(j = seq_along(timestep), .combine = "rbind") %do% {
        ts <- timestep[j]
        # index <- which(tsf[,j] > ts - 1)
        # tsf[index,j] <- df[index,"tsfInit"] + timestep[j] - 1
        ## focussing on fire pixels to identify regen failures
        index <- which(tsf[,j]==0)
        cover <- df[index, "descrip"]
        fireZone <- df[index, "fireZones"]
        if (j == 1) {
            tsfPrefire <- df[index, "tsfInit"]
        } else {
            tsfPrefire <- tsf[index, j-1]
        }
        dfTmp <- data.frame(tsfPrefire, cover, fireZone)
        if (nrow(dfTmp) > 0) {
            dfTmp["timestep"] <- ts
            dfTmp["simID"] <- simID
            dfTmp["scenario"] <- s
            return(dfTmp)
        }

    }
    return(x)
}
stopCluster(cl)
save(outputCompiled, file = "outputCompiledFull.RData")




########################################
########################################
##### loading a few preformatted data frames
coverTypes <- get(load("../data/coverTypesTmp.RData"))
# renaming columns for uniformity
colnames(coverTypes)[which(colnames(coverTypes) == "descrip")] <- "cover"
coverTypes[coverTypes$cover %in% c("R", "Improductif", "INO", "Non_Forestiere", "EAU"), "cover"] <- "autres"
#
fireZones <- raster("../data/fireZones.tif")
fireZones <- rasterToPoints(fireZones)
# loading another table with names of fire zones
fireZoneNames <- read.csv("../data/fireZones.csv")
# renaming columns for uniformity
colnames(fireZones)[which(colnames(fireZones) == "fireZones")] <- "fireZone"


## merging to make things easy
df <- merge(coverTypes, fireZones, all = F)
## removing unproductive cover types
# df <- filter(df, cover %in% c("PG", "EN", "F"))
head(df)

## computing total areas for each subzones
dfArea <- df %>%
    group_by(fireZone, cover) %>%
    summarise(areaTotal_ha = n()*25)
dfArea <- as.data.frame(dfArea)
## adding 
dfArea[,"Zone_LN"] <- fireZoneNames[match(dfArea[, "fireZone"], fireZoneNames$ID), "Zone_LN"]

### writing csv
write.csv(dfArea, file = "dfArea.csv", row.names = F)

outputCompiled <- get(load("../compiledOutputs/outputCompiledFull.RData"))
maturity <- c(EN = 50, PG = 30, F = 0)
outputCompiled[, "maturity"] <- maturity[as.character(outputCompiled$cover)]
nRep <- length(unique(outputCompiled$simID))
# 
# ##
# tsTmp <- rep(unique(outputCompiled$timestep), nrow(dfArea))
# tsTmp <- tsTmp[order(tsTmp)]
# dfArea <- data.frame(dfArea[rep(1:nrow(dfArea), 50),],
#                      timestep = tsTmp)


dfSummary <- outputCompiled %>%
    filter(cover %in% c("EN", "PG", "F")) %>%
    mutate(burnedImmature = tsfPrefire < maturity) %>%
    group_by(simID, timestep, fireZone, cover, burnedImmature) %>%
    summarise(area_ha = n()*25) 

dfSummary <- dfSummary %>%
    select(simID, timestep, fireZone, cover, burnedImmature, area_ha) %>%
    arrange(simID, timestep, fireZone, cover, burnedImmature, area_ha)

dfSummary[, "Zone_LN"] <- fireZoneNames[match(dfSummary$fireZone, fireZoneNames$ID), "Zone_LN"]
write.csv(dfSummary, file = "dfSummary.csv", row.names = F)


########################################
########################################
#### computing results by cover type zone
dfAreaCover <- dfArea %>%
    group_by(cover) %>%
    summarize(areaCover_ha = sum(areaTotal_ha))

dfCover <- dfSummary %>%
    #filter(simID == "000") %>%
    group_by(simID, cover, timestep, burnedImmature) %>%
    summarize(area_ha = sum(area_ha)) %>%
    merge(dfAreaCover) %>%
    mutate(propBurned = area_ha/areaCover_ha) %>%
    ungroup() %>%
    arrange(timestep) %>%
    group_by(simID, cover, burnedImmature) %>%
    mutate(cumulpropBurned = cumsum(propBurned)) 


dfCoverTotal <- dfCover %>%
    group_by(simID, timestep, burnedImmature) %>%
    summarize(area_ha = sum(area_ha),
              areaCover_ha = sum(areaCover_ha)) %>%
    mutate(propBurned = area_ha/areaCover_ha) %>%
    ungroup() %>%
    arrange(timestep) %>%
    group_by(simID, burnedImmature) %>%
    mutate(cumulpropBurned = cumsum(propBurned),
           cover = "Global") 
    
    

dfCover <- rbind(dfCover, dfCoverTotal)

dfCover <- filter(dfCover, burnedImmature == T)

coverLevels <- c(EN = "Épinette noire",
                 PG = "Pin gris",
                 autres = "autres", 
                 F = "Feuillus intolérants",
                 Global = "Global")

dfCover$cover <- factor(coverLevels[as.character(dfCover$cover)],
                        levels = c("Épinette noire", "Pin gris", "Global"))

dfCoverPercentiles <- dfCover %>%
    group_by(timestep, cover) %>%
    summarise(p.025 = quantile(cumulpropBurned, 0.025, na.rm = T),
              p.050 = quantile(cumulpropBurned, 0.05, na.rm = T),
              p.100 = quantile(cumulpropBurned, 0.1, na.rm = T),
              p.500 = quantile(cumulpropBurned, 0.5, na.rm = T),
              p.900 = quantile(cumulpropBurned, 0.9, na.rm = T),
              p.950 = quantile(cumulpropBurned, 0.95, na.rm = T),
              p.975 = quantile(cumulpropBurned, 0.975, na.rm = T),
              cumulPropMean = mean(cumulpropBurned, na.rm = T))


dfCover <- merge(dfCover, dfCoverPercentiles)

labelDF <- dfCoverPercentiles %>%
    filter(timestep == 50) %>%
    select(cover, cumulPropMean)

p <- c(p.100 = "10%", p.900 = "90%")



require(ggplot2)
#options(scipen=999)
m <- ggplot(dfCover, aes(x=timestep, y = cumulpropBurned, group = simID)) +
    geom_line(colour = "black", alpha = 0.1) +#fill = "grey25"
    facet_wrap(~cover) +
    theme_dark() +
    geom_smooth(span = 0.3, aes(group = 1),
                colour="lightblue", linetype = 1, size = 0.7, alpha = 1) +
    geom_smooth(span = 0.3, aes_string(y = names(p)[1], group = 1),
                colour = "yellow",
                linetype = 3, size = 0.5, alpha = 1) +
    geom_smooth(span = 0.3, aes_string(y = names(p)[2], group = 1),
                colour = "yellow",
                linetype = 3, size = 0.5, alpha = 1) 

yMax <- layer_scales(m)$y$range$range[2]
xMin <- layer_scales(m)$x$range$range[1]

png(filename="immatureBurnsCoverPrelim.png",
    width = 10, height = 4, units = "in", res = 600, pointsize=10)
 

print(m + theme_dark() +
          theme(legend.position="top", legend.direction="horizontal",
                axis.text.x = element_text(angle = 45, hjust = 1),
                strip.text.y = element_text(size = 8))+
          labs(title ="Proportion cumulative du territoire productif où une forêt immature a brûlé",
               subtitle = paste0("En bleu sont illustrées les moyennes et en jaune les percentiles ", p[1], " et ", p[2],
                                 " sur un total de ", nRep, " réalisations."),
               caption = paste0("Maturité épinette: ", maturity["EN"], " ans; ",
                                "Maturité pin gris: ",maturity["PG"], " ans; ",
                                "Maturité feuillus: ",maturity["F"], " an(s)"),
               x = "Année",
               y = "Proportion cumulée")  +
          geom_text(aes(x = xMin, y = yMax, group = NULL,
                        label = paste0("taux annuel moyen: ", round(100*cumulPropMean/timestep, 3), "%")),
                    data = labelDF,
                    hjust = 0, size = 3, fontface = 1))
          # +
          # geom_text(aes(x, 0.85*y, label = target),
          #           data = labelDF, hjust = 1, size = 3, colour = "lightblue") +
          # geom_text(aes(x, 0.75*y, label = mean),
          #           data = labelDF, hjust = 1, size = 3, colour = "yellow"))



dev.off()


#######################


### plotting parameters
pWidth  <- 1400
pHeight <- 1200
pointsize <- 8
coverCols <- c(EN = "darkolivegreen", PG = "lightgoldenrod3", F = "darkolivegreen2", autres = "grey75")
coverTypes$cover <- factor(as.character(coverTypes$cover), levels = c("EN", "PG", "F", "autres"))

coverTypesSummary <- round(100*table(coverTypes$cover)/length(coverTypes$cover), 1)
coverTypesSummary <- paste0(coverLevels[names(coverTypesSummary)], " (", coverTypesSummary, "%)")

require(ggplot2)
################  coverTypes map (preliminary)
p <- ggplot(data = coverTypes, aes_string("x", "y", fill = "cover")) +
    theme_bw() +
    geom_raster() +
    coord_fixed() +
    scale_fill_manual(name = "Types de couvert",
                         values = coverCols,
                      labels = coverTypesSummary) +
    labs(x = "\nx (UTM 18)",
         y = "y (UTM 18)\n") +
    theme(legend.position="top", legend.direction="horizontal")


png(filename = "coverTypesPrelim.png",
   width = pWidth, height = pHeight, units = "px", res = 300, pointsize = pointsize,
   bg = "white")

    print(p + theme(plot.title = element_text(size = rel(0.6)),
                    axis.title.x = element_text(size = rel(0.5)),
                    axis.title.y = element_text(size = rel(0.5)),
                    axis.text.x = element_text(size = rel(0.5)),
                    axis.text.y =  element_text(size = rel(0.5), angle = 90, hjust = 0.5),
                    legend.title = element_text(size = rel(0.75)),
                    legend.text = element_text(size = rel(0.4))))# +
              # annotate("text", x = max(coverTypes$x), y = max(coverTypes$y),
              #          label = coverTypesSummary,
              #          hjust = 1, vjust = 0, size = 0.3*pointsize, fontface = 2))

dev.off()

#####################################################################################
#####################################################################################
###########################################################
#### computing results by fire zone
dfAreaZone <- dfArea %>%
    group_by(Zone_LN) %>%
    summarize(areaZone_ha = sum(areaTotal_ha))

dfZone <- dfSummary %>%
    #filter(simID == "000") %>%
    group_by(simID, Zone_LN, timestep, burnedImmature) %>%
    summarize(area_ha = sum(area_ha)) %>%
    merge(dfAreaZone) %>%
    mutate(propBurned = area_ha/areaZone_ha) %>%
    ungroup() %>%
    arrange(timestep) %>%
    group_by(simID, Zone_LN, burnedImmature) %>%
    mutate(cumulpropBurned = cumsum(propBurned)) 


dfZoneTotal <- dfZone %>%
    group_by(simID, timestep, burnedImmature) %>%
    summarize(area_ha = sum(area_ha),
              areaZone_ha = sum(areaZone_ha)) %>%
    mutate(propBurned = area_ha/areaZone_ha) %>%
    ungroup() %>%
    arrange(timestep) %>%
    group_by(simID, burnedImmature) %>%
    mutate(cumulpropBurned = cumsum(propBurned),
           Zone_LN = "Global") 



dfZone <- rbind(dfZone, dfZoneTotal)

dfZone <- filter(dfZone, burnedImmature == T)

# coverLevels <- c(EN = "Épinette noire",
#                  PG = "Pin gris",
#                  autres = "autres", 
#                  F = "Feuillus intolérants",
#                  Global = "Global")


dfZonePercentiles <- dfZone %>%
    group_by(timestep, Zone_LN) %>%
    summarise(p.025 = quantile(cumulpropBurned, 0.025, na.rm = T),
              p.050 = quantile(cumulpropBurned, 0.05, na.rm = T),
              p.100 = quantile(cumulpropBurned, 0.1, na.rm = T),
              p.500 = quantile(cumulpropBurned, 0.5, na.rm = T),
              p.900 = quantile(cumulpropBurned, 0.9, na.rm = T),
              p.950 = quantile(cumulpropBurned, 0.95, na.rm = T),
              p.975 = quantile(cumulpropBurned, 0.975, na.rm = T),
              cumulPropMean = mean(cumulpropBurned, na.rm = T))


dfZone <- merge(dfZone, dfZonePercentiles)

labelDF <- dfZonePercentiles %>%
    filter(timestep == 50) %>%
    select(Zone_LN, cumulPropMean)

p <- c(p.100 = "10%", p.900 = "90%")



require(ggplot2)
#options(scipen=999)
m <- ggplot(dfZone, aes(x=timestep, y = cumulpropBurned, group = simID)) +
    geom_line(colour = "black", alpha = 0.1) +#fill = "grey25"
    facet_wrap(~Zone_LN) +
    theme_dark() +
    geom_smooth(span = 0.6, aes(group = 1),
                colour="lightblue", linetype = 1, size = 0.7, alpha = 1) +
    geom_smooth(span = 0.6, aes_string(y = names(p)[1], group = 1),
                colour = "yellow",
                linetype = 3, size = 0.5, alpha = 1) +
    geom_smooth(span = 0.6, aes_string(y = names(p)[2], group = 1),
                colour = "yellow",
                linetype = 3, size = 0.5, alpha = 1) 

yMax <- layer_scales(m)$y$range$range[2]
xMin <- layer_scales(m)$x$range$range[1]

png(filename="immatureBurnsFireZonesPrelim.png",
    width = 10, height = 6, units = "in", res = 600, pointsize=10)
options(scipen=999)

print(m + theme_dark() +
          theme(legend.position="top", legend.direction="horizontal",
                axis.text.x = element_text(angle = 45, hjust = 1),
                strip.text.y = element_text(size = 8))+
          labs(title ="Proportion cumulative du territoire productif où une forêt immature a brûlé",
               subtitle = paste0("En bleu sont illustrées les moyennes et en jaune les percentiles ", p[1], " et ", p[2],
                                 " sur un total de ", nRep, " réalisations."),
               caption = paste0("Maturité épinette: ", maturity["EN"], " ans; ",
                                "Maturité pin gris: ",maturity["PG"], " ans; ",
                                "Maturité feuillus: ",maturity["F"], " an(s)"),
               x = "Année",
               y = "Proportion cumulée")  +
          geom_text(aes(x = xMin, y = yMax, group = NULL,
                        label = paste0("taux annuel moyen: ", round(100*cumulPropMean/timestep, 3), "%")),
                    data = labelDF,
                    hjust = 0, size = 3, fontface = 1))
# +
# geom_text(aes(x, 0.85*y, label = target),
#           data = labelDF, hjust = 1, size = 3, colour = "lightblue") +
# geom_text(aes(x, 0.75*y, label = mean),
#           data = labelDF, hjust = 1, size = 3, colour = "yellow"))



dev.off()



################  fire region map (preliminary)

r <- raster("../data/fireZones.tif")
rNA <- r
rNA[] <- NA
rNA[is.na(r)] <- 1
df <- rasterToPoints(r)
rNA <- rasterToPoints(rNA)
rNA[,3] <- NA
colnames(rNA)[3] <- colnames(df)[3]
df <- rbind(df, data.frame(rNA))


df <- data.frame(df, Zone_LN = fireZoneNames[match(df[, "fireZones"],
                                               fireZoneNames[complete.cases(fireZoneNames), "ID"]), "Zone_LN"])

zoneLevels <- levels(df$Zone_LN)[order(as.numeric(gsub("[^0-9]", "", levels(df$Zone_LN))))]
df$Zone_LN <- factor(df$Zone_LN, levels = zoneLevels)
levels(df$Zone_LN) <- paste0(zoneLevels, " (", fireZoneNames[match(zoneLevels, fireZoneNames$Zone_LN), "Fire_Cycle"], " ans)")




head(df$Zone_LN)
studyAreaP <- get(load("../data/studyAreaP.RData"))
studyAreaF <- fortify(studyAreaP)

fireCols <- c("burlywood4", "darkseagreen4", "lightgoldenrod3", "orangered4", "palegreen4",
              "orangered3", "lightsteelblue3", "darkgreen", "bisque3", "coral3")

p <- ggplot(data = df, aes_string("x", "y", fill = "Zone_LN")) +
    theme_bw() +
    geom_raster() +
    coord_fixed() +
    geom_polygon(aes(x = long, y = lat, group = group), data = studyAreaF,
                 colour = 'white', fill = NA, size = 1) +
    
    scale_fill_manual(name = "Zones de régime de feu",
                      #palette = "Set1",
                      values = fireCols,
                      na.value = "dodgerblue1") +
    # scale_fill_brewer(name = "Zones de régime de feu",
    #                    palette = "Set1",
    #                    #values = coverCols,
    #                   na.value = "dodgerblue1") +
    labs(x = "\nx (UTM 18)",
         y = "y (UTM 18)\n") +
    theme(legend.position="top", legend.direction="horizontal")


png(filename = "fireZonesPrelim.png",
    width = pWidth, height = pHeight, units = "px", res = 300, pointsize = pointsize,
    bg = "white")

options(scipen=999)
print(p + theme(plot.title = element_text(size = rel(0.6)),
                axis.title.x = element_text(size = rel(0.5)),
                axis.title.y = element_text(size = rel(0.5)),
                axis.text.x = element_text(size = rel(0.5)),
                axis.text.y =  element_text(size = rel(0.5), angle = 90, hjust = 0.5),
                legend.title = element_text(size = rel(0.75)),
                legend.text = element_text(size = rel(0.4))))# +
# annotate("text", x = max(coverTypes$x), y = max(coverTypes$y),
#          label = coverTypesSummary,
#          hjust = 1, vjust = 0, size = 0.3*pointsize, fontface = 2))

dev.off()