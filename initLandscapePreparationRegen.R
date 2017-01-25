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

### loading shapefiles
forestInventory <- readOGR(dsn = "../gis", layer = "forestInventory")


### loading study area
studyArea <- raster("../data/studyArea.tif")

# defining projection (Quebec Lambert NAD93), and reprojecting for study area
proj4string(forestInventory) <- CRS("+init=epsg:32198")
forestInventory <- spTransform(forestInventory, CRSobj = crs(studyArea))


coverType <- rasterize(forestInventory, studyArea, field = "Code_Poly")
# attaching levels description
coverType <- ratify(coverType)
rat <- levels(coverType)[[1]]
rat[,"descrip"] <- levels(forestInventory$Code_Poly)[rat$ID]
levels(coverType) <- rat

