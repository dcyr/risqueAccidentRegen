####################################################################################################
####################################################################################################
####################################################################################################
###### Summary of initial tsf by fire zones and cover types
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
# #######

# ########################################
# #########  initial tsf, a little long...
# #########  uncomment only if needed
# ### loading shapefiles
# forestInventory <- readOGR(dsn = "../gis", layer = "forestInventory")
# 
# ### loading study area
# studyArea <- raster("../data/studyArea.tif")
# 
# # defining projection (Quebec Lambert NAD93), and reprojecting for study area
# proj4string(forestInventory) <- CRS("+init=epsg:32198")
# forestInventory <- spTransform(forestInventory, CRSobj = crs(studyArea))
# 
# 
# tsfInit <- rasterize(forestInventory, studyArea, field = "Stand_Age")
# tsfInit[tsfInit == 999] <- NA
# 
# tsfInitDf <- rasterToPoints(tsfInit)
# colnames(tsfInitDf)[3] <- "tsf"
# 
# ### saving rasters raster (tif and DF formats)
# writeRaster(tsfInit, file = "tsfInit.tif", overwrite = T)
# save(tsfInitDf, file = "tsfInitDf.RData")



########################################
########################################
coverTypes <- get(load("../data/coverTypesDf.RData"))
tsfInit <- get(load("../data/tsfInitDf.RData"))
########################################

fireZones <- raster("../data/fireZones.tif")
fireZones <- rasterToPoints(fireZones)
fireZoneNames <- read.csv("../data/fireZoneTable.csv")

df <- merge(coverTypes, tsfInit)
df <- merge(df, fireZones)
df[,"Zone_LN"] <- fireZoneNames[match(df$fireZones, fireZoneNames$ID) , "Zone_LN"]

# df <- df %>%
#    filter(descrip %in% c("EN", "PG"))
df[, "cover"] <- NA
index <- which(df$descrip %in% c( "Improductif", "INO", "Non_Forestiere", "EAU"))
df[index, "cover"] <- "autres"
index <- which(df$descrip %in% c("EN", "F", "PG", "R"))
df[index, "cover"] <- as.character(df[index, "descrip"])


require(ggplot2)
### plotting parameters
pWidth  <- 1400
pHeight <- 1200
pointsize <- 8
maxVal <- max(df$tsf, na.rm = T)
colValues <- c(0, 10, 25, 50, maxVal)

cols = c("red", "orange", "gold2", "forestgreen", "darkgreen")

### plotting initial tsf
p <- ggplot(data = df, aes_string("x", "y", fill = "tsf")) +
    theme_bw() +
    geom_raster() +
    coord_fixed() +
    scale_fill_gradientn(name = c("Temps depuis le dernier feu initial\n(années)"), #limits = c(0,1),
                         colours = cols,
                         values = colValues/maxVal, limits = c(0,maxVal),
                         na.value = "dodgerblue1") +
    labs(x = "\nx (UTM 18)",
         y = "y (UTM 18)\n") +
    theme(legend.position="top", legend.direction="horizontal")
   
    
png(filename = "tsfInit.png",
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



#### summarizing by area and fire zones
require(dplyr)
tsfInitSummary <- df %>%
    group_by(cover, Zone_LN, tsf) %>%
    summarise(area_ha = n()*25) %>%
    arrange(tsf, Zone_LN, cover)

write.csv(tsfInitSummary, file = "tsfInitSummary.csv", row.names = F)

tsfInitGlobal <- tsfInitSummary %>%
    group_by(tsf)  %>%
    summarize(area_ha = sum(area_ha)) %>%
    mutate(cover = "Global",
           Zone_LN = "Global")




tsfInitSummary <- rbind(tsfInitSummary, tsfInitGlobal)
coverLevels <- c(EN = "Épinette", PG = "Pin gris", F = "Feuillu", R = "Résineux", autres = "autres", Global = "Global")

tsfInitSummary$cover <- factor(coverLevels[tsfInitSummary$cover], levels = coverLevels)


#figure by fireZone
png(filename="tsfDistribFireZones.png",
    width = 10, height = 5, units = "in", res = 600, pointsize=10)


options(scipen=999)
ggplot(data = tsfInitSummary, aes(x = tsf, weight = area_ha)) +
    geom_histogram(breaks = seq(from = 0, to = 150, by = 10)) +
    theme_dark() +
    facet_wrap(~Zone_LN) +
    labs(title ="Structure d'âge",
         subtitle = "Temps depuis le dernier feu - Préliminaire",
                        x = "Temps depuis le dernier feu (années)",
                        y = "Superficie totale (ha)")

dev.off()


#figure by cover
png(filename="tsfDistribCover.png",
    width = 10, height = 5, units = "in", res = 600, pointsize=10)


options(scipen=999)
ggplot(data = tsfInitSummary, aes(x = tsf, weight = area_ha)) +
    geom_histogram(breaks = seq(from = 0, to = 150, by = 10)) +
    theme_dark() +
    facet_wrap(~cover) +
    labs(title ="Structure d'âge",
         subtitle = "Temps depuis le dernier feu - Préliminaire",
         x = "Temps depuis le dernier feu (années)",
         y = "Superficie totale (ha)")

dev.off()
