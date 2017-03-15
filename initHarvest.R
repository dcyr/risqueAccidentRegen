####################################################################################################
####################################################################################################
###### Calculation of harvesting rate
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
require(raster)
require(dplyr)

### fetching study area and cover types
studyArea <- raster("../data/studyArea.tif")
coverTypes <- raster("../data/coverTypes.tif")
coverTypesID <- distinct(get(load("../data/coverTypesDf.RData"))[c("ID", "descrip")])

### identifying "productive" cover types
coverTypesProd <- coverTypesID[which(coverTypesID$descrip %in% c("EN", "PG", "F", "R")), "ID"]


# ### estimating harvesting rate from Guindon et al 
# r <- raster("../data/Guindon/harvest_2002_2011.tif")
# harvestProp <- projectRaster(r, studyArea)/100
# harvestProp[is.na(studyArea)] <- NA
# harvestProp[!(coverTypes %in% coverTypesProd)] <- NA
# harvestProp[harvestProp>1] <- 1
# 
# plot(harvestProp)
# harvestRate <- mean(values(harvestProp), na.rm = T)/10



### estimating harvesting rate from forest inventory
### loading shapefiles
require(rgdal)
forestInventory <- readOGR(dsn = "../gis", layer = "forestInventory")
# defining projection (Quebec Lambert NAD93), and reprojecting for study area
proj4string(forestInventory) <- CRS("+init=epsg:32198")
forestInventory <- spTransform(forestInventory, CRSobj = crs(studyArea))

# extracting levels 
an_origineLevels <- levels(forestInventory$AN_ORIGINE)
origineLevels <- levels(forestInventory$ORIGINE)
# rasterizing polygons
origine <- rasterize(forestInventory, studyArea, field = "ORIGINE")
an_origine <- rasterize(forestInventory, studyArea, field = "AN_ORIGINE")

### creating another raster
originYear <- an_origine
originYear[] <- as.numeric(an_origineLevels)[values(originYear)]


summary(forestInventory$ORIGINE)
harvestLevels <- which(origineLevels %in% c("CBA", "CPR", "CT", "P", "PRR", "REA"))
harvest <- origine %in% harvestLevels
harvest[is.na(studyArea)] <- NA
writeRaster(harvest, file = "harvest.tif", overwrite = T)

year <- 2000
harvestProp <- harvest & originYear>=year
harvestProp[is.na(studyArea)] <- NA
harvestProp[!(coverTypes %in% coverTypesProd)] <- NA
plot(harvestProp)
harvestRate <- mean(values(harvestProp), na.rm = T)/(2015-year)

### 0.005102012

df <- studyArea
df[df==1] <- 3
df[(coverTypes %in% coverTypesProd)] <- 1
df[harvestProp == 1] <- 2



df <- as.data.frame(rasterToPoints(df))
df[,"value"] <- factor(c("prod", "recolte", "np")[df[,"studyArea"]], levels = c("prod", "recolte", "np"))
cols <-  c(prod = "darkolivegreen", recolte = "green", np = "grey")
coverLevels <- c(prod = "Productif", recolte = "Récolte 2000-2013", np = "Non productif")

require(ggplot2)
### plotting parameters
pWidth  <- 1400
pHeight <- 1200
pointsize <- 8
### plotting initial tsf
p <- ggplot(data = df, aes(x = x, y = y, fill = value)) +
    theme_bw() +
    geom_raster() +
    coord_fixed() +
    scale_fill_manual(name = "",
                      values = cols,
                      labels = paste0(coverLevels, " (", round(100 * table(df$value)/nrow(df), 1), "%)"),
                      na.value = "dodgerblue1") +
    labs(x = "\nx (UTM 18)",
         y = "y (UTM 18)\n") +
    theme(legend.position="top", legend.direction="horizontal") 



yMax <- layer_scales(p)$y$range$range[2]
xMax <- layer_scales(p)$x$range$range[2]

png(filename = "harvestInit.png",
    width = pWidth, height = pHeight, units = "px", res = 300, pointsize = pointsize,
    bg = "white")

print(p + geom_text(aes(xMax, yMax, label = paste0("Taux de récolte annuel (2000 - 2013): ", round(100*harvestRate, 2), "%")),
                   hjust = 1, vjust = 0, size = 2, fontface = 1) +
          theme(plot.title = element_text(size = rel(0.6)),
                axis.title.x = element_text(size = rel(0.5)),
                axis.title.y = element_text(size = rel(0.5)),
                axis.text.x = element_text(size = rel(0.5)),
                axis.text.y =  element_text(size = rel(0.5), angle = 90, hjust = 0.5),
                legend.title = element_text(size = rel(0.75)),
                legend.text = element_text(size = rel(0.5))))

dev.off()


