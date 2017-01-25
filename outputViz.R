####################################################################################################
####################################################################################################
###### 
###### Output visualization
######
###### Dominic Cyr
####################################################################################################
####################################################################################################
rm(list=ls())
setwd("~/Travail/SCF/regenFailureRiskAssessment")
####################################################################################################
####################################################################################################
wwd <- paste(getwd(), Sys.Date(), sep="/")
dir.create(wwd)
setwd(wwd)
####################################################################
####################################################################
######
require(raster)
studyArea <- raster("../data/studyArea.tif")
fireZones <- raster("../data/fireZones.tif")
fireRegimeAttrib <- read.csv("../data/fireZones.csv")
fireZones[is.na(studyArea)] <- NA 
##
convFactor <- prod(res(studyArea))/10000### to convert to hectares
fireZoneArea <- zonal(!is.na(fireZones), fireZones, sum)
fireZoneArea <- data.frame(zone = as.character(fireRegimeAttrib[match(fireZoneArea[,1], fireRegimeAttrib$ID),"Zone_LN"]),
                           areaZone_ha = fireZoneArea[,2] * convFactor)
fireZoneArea <- rbind(fireZoneArea, data.frame(zone = "total", areaZone_ha = sum(fireZoneArea$areaZone_ha)))


# #############################################################
# #############################################################
# outputCompiled <- get(load("../compiledOutputs/outputCompiled.RData"))
# 
# require(dplyr)
# outputSummary <- outputCompiled %>%
#     group_by(zone, replicate) %>%
#     summarize(meanTSF = round((mean(meanTSF))),
#               fireCycle = round((1/mean(areaBurned_ha/areaZone_ha))),
#               propAAB = mean(areaBurned_ha/areaZone_ha)) %>%
#     arrange(zone, replicate)
# 
# fcSummary <- outputSummary %>%
#     group_by(zone) %>%
#     summarize(realizedFC_median = median(fireCycle),
#               realizedFC_mean = 1/mean(propAAB))
# 
# 
# fireRegimeAttrib <- merge(fireRegimeAttrib, fcSummary, by.x = "Zone_LN", by.y = "zone")                     
# colnames(fireRegimeAttrib)[1] <- "zone"
# fireRegimeAttrib <- merge(fireRegimeAttrib, fireZoneArea)
# fireRegimeAttrib[, "firezoneProp"] <- fireRegimeAttrib$areaZone_ha/sum(fireRegimeAttrib$areaZone_ha)*2
# 
# 
# 
# require(ggplot2)
# options(scipen=999)
# m <- ggplot(outputSummary, aes(x=fireCycle)) +
#     geom_histogram() +#fill = "grey25"
#     facet_wrap(~zone) +#, scales = "free_x") +
#     
#     scale_x_log10(breaks = c(30, 62.5, 125, 250, 500, 1000, 2000, 4000, 16000)) +
#     geom_vline(data = fireRegimeAttrib,  aes(xintercept = Fire_Cycle), 
#                colour="lightblue", linetype = 3, size = 0.7, alpha = 1) +
#     geom_vline(data = fireRegimeAttrib,  aes(xintercept = realizedFC_mean), 
#                colour="yellow", linetype = 3, size = 0.5, alpha = 1)
# 
# 
# 
# yMax <- layer_scales(m)$y$range$range[2]
# xMax <- layer_scales(m)$x$range$range[2]
# 
# labelDF <-  data.frame(x = 10^xMax, y = yMax, zone = fireRegimeAttrib$zone,
#                        prop = paste0("Prop. terr. ", round(fireRegimeAttrib$firezoneProp*100), "%"),
#                        target = paste("Cible:", fireRegimeAttrib$Fire_Cycle, "ans"),
#                        mean = paste("Moy.:", round(fireRegimeAttrib$realizedFC_mean), "ans"))
# 
# png(filename="realizedFC.png",
#     width = 10, height = 5, units = "in", res = 600, pointsize=10)
# 
# print(m + theme_dark() +
#           theme(legend.position="top", legend.direction="horizontal",
#                 axis.text.x = element_text(angle = 45, hjust = 1),
#                 strip.text.y = element_text(size = 8))+
#           labs(title ="Distribution des cycles de feux réalisés",
#                subtitle = "Les lignes pointillées indiquent pour chacune des zones les valeurs ciblées (bleu) et les moyennes réalisées* (jaune).",
#                caption = paste("*Total de", length(unique(outputSummary$replicate)), "simulations d'une durée de 50 ans" ),
#                x = "Cycle des feux (années)",
#                y = "Fréquence") +
#           geom_text(aes(x, y, label = prop),
#                     data = labelDF, hjust = 1, size = 3, fontface = 1) +
#           geom_text(aes(x, 0.85*y, label = target),
#                     data = labelDF, hjust = 1, size = 3, colour = "lightblue") +
#           geom_text(aes(x, 0.75*y, label = mean),
#                     data = labelDF, hjust = 1, size = 3, colour = "yellow"))
#           
#     
# 
# dev.off()





####################################################################################################
####################################################################################################
######
###### Visualizing one realisation
################################
require(rgdal)
require(rgeos)
require(raster)
require(maptools)
require(ggplot2)

fireZones <- raster("../data/fireZones.tif")
fireZonesP <- get(load("../data/fireZonesP.RData"))
studyAreaP <- get(load("../data/studyAreaP.RData"))

### cropping 20 km to remove 'border effect'
extentFig <- extent(fireZones)
extentFig[c(1,3)] <- extentFig[c(1,3)]+20000
extentFig[c(2,4)] <- extentFig[c(2,4)]-20000

fireZones <- crop(fireZones, extentFig)
fireZonesP <- crop(fireZonesP, extentFig)
studyAreaP <- crop(studyAreaP, extentFig)


fireZonesF <- fortify(fireZonesP)
studyAreaF <- fortify(studyAreaP)



################################

output <- get(load("../outputs/simOutput_003.RData"))
#output <- get(load("simOutput_002.RData"))

require(doSNOW)
require(parallel)
require(foreach)
clusterN <-  max(1, floor(0.75*detectCores()))  ### choose number of nodes to add to cluster.
# #######
cl = makeCluster(clusterN, outfile = "") ##
registerDoSNOW(cl)

fTitle <- foreach(l = 1:nlayers(output$tsf), .combine = c) %dopar%  {
    options(scipen=999)
    require(raster)
    require(stringr)
    require(ggplot2)
    ### data to plot
    r <- output$tsf[[l]]
    r <- crop(r, extentFig)

    rNA <- r
    rNA[] <- NA
    rNA[is.na(r)] <- 1
    df <- rasterToPoints(r)
    rNA <- rasterToPoints(rNA)
    rNA[,3] <- NA
    colnames(rNA)[3] <- colnames(df)[3]
    df <- rbind(df, data.frame(rNA))

    ### plotting parameters
    pWidth  <- 1400
    pHeight <- 1200
    pointsize <- 8
    maxVal <- max(values(output$tsf[[1]]), na.rm = T) + nlayers(output$tsf)
    colValues <- c(0, 10, 25, 50, maxVal)
    #cols = c("red", "orange", "gold2", "seagreen4", "darkgreen")
    cols = c("red", "orange", "gold2", "forestgreen", "darkgreen")

    ### plotting
    p <- ggplot(data = df, aes_string("x", "y", fill = colnames(df)[3])) +
        theme_bw() +
        #theme(legend.position="top", legend.direction="horizontal") +
        geom_raster() +
        coord_fixed() +
        scale_fill_gradientn(name = "Temps depuis le dernier feu (années)", #limits = c(0,1),
                             colours = cols,
                             values = colValues/maxVal, limits = c(0,maxVal),
                             na.value = "dodgerblue1") +
        #coord_fixed(ratio = figRatio) +
        geom_polygon(aes(x = long, y = lat, group = group), data = fireZonesF,
                     colour = 'black', fill = NA, alpha = 0.5, size = 0.5) +
        geom_polygon(aes(x = long, y = lat, group = group), data = studyAreaF,
                     colour = 'white', fill = NA, size = 1) +
        labs(x = "\nx (UTM 18)",
              y = "y (UTM 18)\n") +
        theme(legend.position="top", legend.direction="horizontal") +
        annotate("text", x = max(df$x), y = max(df$y)+2500,
                 label = paste("année", l),
                 hjust = 1, vjust = 0, size = 0.3*pointsize, fontface = 2)

    fTitle <- paste0("tsfTest_" , str_pad(l, nchar(nlayers(output$tsf)), pad = "0"),".png")

    png(filename = fTitle,
        width = pWidth, height = pHeight, units = "px", res = 300, pointsize = pointsize,
        bg = "white")

        print(p + theme(plot.title = element_text(size = rel(0.6)),
                        axis.title.x = element_text(size = rel(0.5)),
                        axis.title.y = element_text(size = rel(0.5)),
                        axis.text.x = element_text(size = rel(0.5)),
                        axis.text.y =  element_text(size = rel(0.5), angle = 90, hjust = 0.5),
                        legend.title = element_text(size = rel(0.75)),
                        legend.text = element_text(size = rel(0.5))))

    dev.off()
    return(fTitle)
}

stopCluster(cl)
require(animation)
oopt = ani.options(ani.dev="png", ani.type="png", interval = 0.3, autobrowse = FALSE)
### (Windows users may want to add):  ani.options(convert = 'c:/program files/imagemagick/convert.exe')
im.convert(c(fTitle, rep(fTitle[length(fTitle)], 10)), output = "tsfExample.gif",
           extra.opts = "", clean = F)
####################################################################################################
###################################################################################################



