####################################################################################################
####################################################################################################
###### Preparation of initial landscape
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
source("../scripts/gdal_polygonizeR.R")
#################

### loading shapefiles
fireZones <- readOGR(dsn = "../gis", layer = "fireZones_Gauthier2015")
studyArea <- readOGR(dsn = "../gis", layer = "studyArea")
lakes <- readOGR(dsn = "../gis", layer = "canadlake_p")

### projecting into UTM 18 (seems appropriate for that study area)
fireZones <- spTransform(fireZones, CRSobj = CRS("+init=epsg:26918"))
studyArea <- spTransform(studyArea, CRSobj = CRS("+init=epsg:26918"))
lakes <- spTransform(lakes, CRSobj = CRS("+init=epsg:26918"))


### defining the simulation area (study area extent + 10 km buffer for fires)
spatialExtent <- extent(studyArea)

# creating buffer, after rounding extent to the nearest hundred
spatialExtent[c(1, 3)] <- round(spatialExtent[c(1, 3)], -3) - 50000
spatialExtent[c(2, 4)] <- round(spatialExtent[c(2, 4)], -3) + 50000
# creating raster from spatialExtent
spatialExtent <- raster(spatialExtent, resolution = 500, crs = projection(studyArea))

# cropping fireZones and lakes
fireZones <- fireZonesP <- crop(fireZones, spatialExtent)
save(fireZonesP, file = "fireZonesP.RData")
lakes <- crop(lakes, spatialExtent)


# # ### plotting just to check, looks fine
# plot(fireZones, border = "black", lwd = 3)
# plot(lakes, add = T,  col = "blue", border = NA)
# plot(studyArea, add = T, col = NA, border = "red", lwd = 3)

# 'rasterizing' fireZones
fireZones <- rasterize(fireZones, spatialExtent)
lakes <- rasterize(lakes, spatialExtent)
studyArea <- rasterize(studyArea, spatialExtent)

# removing clumps (patches) from the study area
# has to be done manually at this moment
rc <- clump(studyArea)
studyArea[rc==2] <- NA
rc <- clump(is.na(studyArea))
studyArea[rc %in% c(3,4)] <- 1
# saving as polygon for more plotting options
studyAreaP <- gdal_polygonizeR(studyArea)
save(studyAreaP, file = "studyAreaP.RData")




# removing lakes from firezones
fireZones[lakes>0] <- NA
studyArea[lakes>0] <- NA
fireZoneTable <- as.data.frame(levels(fireZones))
#fireZones[] <- fireZoneTable[match(values(fireZones), fireZoneTable$ID ),"Fire_Cycle"] 
meanFireSize <- c(G1 = 13087,
                  G2 = 7096,
                  G3 = 6936,
                  G4 = 5886,
                  G5 = 5379,
                  G6 = 5306,
                  G7 = 2142,
                  G8 = 1703,
                  G9 = 1613,
                  G10 = 2305)
fireZoneTable[,"meanFireSize_ha"] <- meanFireSize[as.character(fireZoneTable$Zone_LN)]
### saving fireZones
writeRaster(fireZones, file = "fireZones.tif", overwrite = T)
writeRaster(studyArea, file = "studyArea.tif", overwrite = T)
### creating a table with fire cycles
write.csv(fireZoneTable, file = "fireZones.csv", row.names = F)

