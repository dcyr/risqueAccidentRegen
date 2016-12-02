rm(list = ls())
setwd("~/Travail/SCF/regenFailureRiskAssessment/")
wwd <- paste(getwd(), Sys.Date(), sep = "/")
dir.create(wwd)
setwd(wwd)
rm(wwd)

require(raster)
require(ggmap)
require(rgdal)


studyArea <- raster("../data/studyArea.tif")
convFactor <- prod(res(studyArea))/10000
studyAreaP <- get(load("../data/studyAreaP.RData"))
fireZonesP <- readOGR(dsn = "../gis", layer = "fireZones_Gauthier2015")
#fireZonesP <- get(load("../data/fireZonesP.RData"))
studyAreaTotal <- rasterize(studyAreaP, studyArea)

studyAreaTotal[!is.na(studyAreaTotal)] <- 1
studyAreaTotal <- sum(values(studyAreaTotal), na.rm = T) *convFactor
flammableProp <- sum(values(studyArea), na.rm = T) *convFactor /studyAreaTotal

## converting to lat long, and fortifying for ggplot
studyAreaP <- spTransform(studyAreaP, CRS("+init=epsg:4326"))
studyAreaF <- fortify(studyAreaP)

fireZonesP <- spTransform(fireZonesP, CRS("+init=epsg:4326"))

############################################################
############################################################
#########
#########   Study area map
#########
############################################################
############################################################
#fond <- get_map(location = "Lac St-Jean", zoom = 6, source = "google")
fond <- get_map(location = bbox(studyAreaP), zoom = 7,  source = "google", maptype = "hybrid")#, maptype = "roadmap"

xlim=c(attr(fond, "bb")$ll.lon, attr(fond, "bb")$ur.lon)
ylim=c(attr(fond, "bb")$ll.lat, attr(fond, "bb")$ur.lat)

ext <-  extent(c(xlim, ylim))
ext[c(1,3)] <- ext[c(1,3)] + 0.1
ext[c(2,4)] <- ext[c(2,4)] - 0.1
fireZonesC <- crop(fireZonesP, ext)
fireZonesF <- fortify(fireZonesC)

map <- ggmap(fond) +
    # geom_polygon(aes(x = long, y = lat, group = group), data = fireZonesF,
    #              colour = 'red', fill = 'red', alpha = .05, size = .2) +
    geom_polygon(aes(x = long, y = lat, group = group), data = studyAreaF,
                 colour = 'white', fill = 'white', alpha = .05, size = .3) +
    labs(x = "longitude",
         y = "latitude")



xBreaks <- seq(from = -78, to = -72, by = 2)
yBreaks <- seq(from = 49, to = 52, by = 1)

xRange <- as.numeric(attr(fond, "bb")[c(2,4)])
yRange <- as.numeric(attr(fond, "bb")[c(1,3)])

png(filename = "studyAreaLargeScale.png",
    width = 1024, height = 1024, units = "px", res = 300, pointsize = 12,
    bg = "white")

    print(map + theme_bw() +
              coord_map(projection = "mercator", 
                        xlim=c(attr(fond, "bb")$ll.lon, attr(fond, "bb")$ur.lon),
                        ylim=c(attr(fond, "bb")$ll.lat, attr(fond, "bb")$ur.lat)) +
              
              theme(plot.title = element_text(size = rel(1.2)),
                    axis.title = element_text(size = rel(0.5)),
                    axis.text = element_text(size = rel(0.4)),
                    #axis.title.x = element_blank(),
                    #axis.title.y = element_blank(),
                    panel.grid.major = element_line(size = 0.25))
          )

dev.off()

