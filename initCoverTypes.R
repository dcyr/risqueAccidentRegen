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
#source("../scripts/gdal_polygonizeR.R")
#################


# ########################################
# #########  initial cover types, a little long...
# #########  uncomment only if needed
# 
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
# coverTypes <- rasterize(forestInventory, studyArea, field = "Code_Poly")
# coverTypes[is.na(studyArea)] <- NA
# # attaching levels description
# coverTypes <- ratify(coverTypes)
# rat <- levels(coverTypes)[[1]]
# rat[,"descrip"] <- levels(forestInventory$Code_Poly)[rat$ID]
# levels(coverTypes) <- rat
# 
# 
# coverTypesDf <- rasterToPoints(coverTypes)
# colnames(coverTypesDf)[3] <- "ID"
# coverTypesDf <- data.frame(coverTypesDf,
#                            descrip = rat[match(coverTypesDf[, "ID"], rat$ID), "descrip"])
# 
# # saving rasters raster (tif and DF formats)
# writeRaster(coverTypes, file = "coverTypes.tif", overwrite = T)
# save(coverTypesDf, file = "coverTypesDf.RData")



########################################
########################################
coverTypesDf <- get(load("../data/coverTypesDf.RData"))
########################################

coverLevels <- c(EN = "Épinette noire", PG = "Pin gris", R = "Résineux ind.", F = "Feuillus intolérants", autres = "autres")
coverTypesDf[,"cover"] <- NA
index <- which(coverTypesDf$descrip %in% c("EAU", "Improductif", "INO", "Non_Forestiere"))
coverTypesDf[index, "cover"] <- "autres"
index <- which(coverTypesDf$descrip %in% c("EN", "PG", "F", "R"))
coverTypesDf[index, "cover"] <- as.character(coverTypesDf[index, "descrip"])
coverTypesDf$cover <- factor(coverTypesDf$cover, levels = names(coverLevels))


require(ggplot2)
### plotting parameters
pWidth  <- 1400
pHeight <- 1200
pointsize <- 8

cols = c(EN = "darkolivegreen", PG = "gold2", F = "darkolivegreen1", R = "forestgreen", autres = "grey")

### plotting initial tsf
p <- ggplot(data = coverTypesDf, aes_string("x", "y", fill = "cover")) +
    theme_bw() +
    geom_raster() +
    coord_fixed() +
    scale_fill_manual(name = "Types de couvert",
                      values = cols,
                      labels = paste0(coverLevels, " (", round(100 * table(coverTypesDf$cover)/nrow(coverTypesDf), 1), "%)"),
                      na.value = "dodgerblue1") +
    labs(x = "\nx (UTM 18)",
         y = "y (UTM 18)\n") +
    theme(legend.position="top", legend.direction="horizontal")


png(filename = "coverInit.png",
    width = pWidth, height = pHeight, units = "px", res = 300, pointsize = pointsize,
    bg = "white")

print(p + theme(plot.title = element_text(size = rel(0.6)),
                axis.title.x = element_text(size = rel(0.5)),
                axis.title.y = element_text(size = rel(0.5)),
                axis.text.x = element_text(size = rel(0.5)),
                axis.text.y =  element_text(size = rel(0.5), angle = 90, hjust = 0.5),
                legend.title = element_text(size = rel(0.6)),
                legend.text = element_text(size = rel(0.35))))

dev.off()
