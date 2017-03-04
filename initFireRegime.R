####################################################################################################
####################################################################################################
###### Preparation of fire regime parameters
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
source("../scripts/gdal_polygonizeR.R")
#################


####################################################################################################
####################################################################################################
######   computing future burn rates from Boulanger et al 2014
####################################################################################################
areaZone7_ha <- 29902318.75 # burnable area in Zone 7 (Yan Boulanger, personnal communication)
require(foreign)
######
sourceDataDir <- "../data/Boulanger2014"
f <- list.files(sourceDataDir)
# selecting files containting AAB
f <- f[grep("FIRE_HA", f)]
simInfo <- strsplit(f, "_")
s <- as.character(lapply(simInfo, function(x) x[3]))
s <- gsub("\\.", "", s)

fireData <- list()
for(i in seq_along(f)) {
    x <- paste(sourceDataDir, f[i], sep = "/")
    fireData[[i]] <- data.frame(scenario = s[i], read.dbf(x))
    
}
burnRates <- do.call("rbind", fireData)
### tidying up and selecting data from zone 7 (James Bay East)
burnRates <- burnRates %>%
    filter(ZONES == 7) %>%
    group_by(scenario, ZONES, YEAR) %>%
    summarize(burnRate = mean(FIRE_HA_pr)/areaZone7_ha)
    
#################
#   smoothing and plotting burn rates
colScenarios <- c(baseline = "black",
                  RCP26 = "dodgerblue2",
                  RCP45 = "goldenrod1",
                  RCP85 = "red3")

require(ggplot2)
p <- ggplot(data = burnRates, aes(x = YEAR, y = burnRate * 100, color = scenario)) +
    theme_dark() +
    # geom_smooth(aes(y = burnRate * 100,
    #                 x = Year,color = scenario, group = ID, linetype = treatment), span = 0.5, se = F) +
    geom_line(size = 1) +
    scale_colour_manual(values = colScenarios) +
    #ylim(c(0, ceiling(max(burnRates$burnRate)*100))) +
    labs(x = "année",
         y = "Taux de brûlage (%)\n",
         title = "Taux de brûlage projetés dans le territoire à l'étude",
         subtitle = 'Adapté de Boulanger et al (2014) pour la region "Baie James (est)"') +
    theme(legend.position="top", legend.direction="horizontal")


png(filename = "burnRates_Boulanger2014.png",
    width = 8, height = 5, units = "in", res = 600, pointsize=10,
    #width = 1200, height = 1000, units = "px", res = 300, pointsize = 8,
    bg = "white")

print(p + theme(axis.title.x = element_blank(),
                axis.text.x = element_text(angle = 45, hjust = 1),
                legend.key = element_blank()))

dev.off()



fireCyclesProj <- burnRates %>%
    mutate(period = cut(YEAR,
                        c(1980, 2010, 2040, 2070, 2100),
                        labels = c("1981-2010", "2011-2040", "2041-2070", "2071-2100"))) %>%
    group_by(scenario, ZONES, period) %>%
    summarise(fireCycle = round(mean(1/burnRate)))




####################################################################################################
####################################################################################################
###### preparing  and plotting spatial information for fire zones
####################################################################################################
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

###############
### plotting fire zones
r <- raster("../data/fireZones.tif")
rNA <- r
rNA[] <- NA
rNA[is.na(r)] <- 1
df <- rasterToPoints(r)
rNA <- rasterToPoints(rNA)
rNA[,3] <- NA
colnames(rNA)[3] <- colnames(df)[3]
df <- rbind(df, data.frame(rNA))

fireZoneNames <- read.csv("../data/fireZoneTable.csv")
fireZoneNames <- fireZoneNames %>%
    filter(scenario == "baseline",
           period == "2011-2040")


df <- data.frame(df, Zone_LN = fireZoneNames[match(df[, "fireZones"],
                                                   fireZoneNames[complete.cases(fireZoneNames), "ID"]), "Zone_LN"])

zoneLevels <- levels(df$Zone_LN)[order(as.numeric(gsub("[^0-9]", "", levels(df$Zone_LN))))]
df$Zone_LN <- factor(df$Zone_LN, levels = zoneLevels)
levels(df$Zone_LN) <- paste0(zoneLevels, " (", fireZoneNames[match(zoneLevels, fireZoneNames$Zone_LN), "fireCycle"], " ans)")

studyAreaP <- get(load("../data/studyAreaP.RData"))
studyAreaF <- fortify(studyAreaP)

###############
pWidth  <- 1400
pHeight <- 1200
pointsize <- 8
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
    labs(x = "\nx (UTM 18)",
         y = "y (UTM 18)\n") +
    theme(legend.position="top", legend.direction="horizontal")


png(filename = "fireZones.png",
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

####################################################################################################
####################################################################################################
###### creating final input table for fire regimes (baseline and projected)
####################################################################################################
fireZoneTable <- data.frame(levels(fireZones))
names(fireZoneTable)[2] <- "fireCycle"


fireCycles <- list()
for (p in c("2011-2040", "2041-2070")) {
    df <- data.frame(fireZoneTable, scenario = "baseline", period = p)
    
    fc85 <- fireCyclesProj %>%
        filter(scenario == "RCP85",
               period == p)
    fc85 <- fc85$fireCycle
    df85 <-  data.frame(fireZoneTable, scenario = "RCP85", period = p)
    df85$fireCycle <- fc85
    
    fireCycles[[p]] <- rbind(df, df85)
    
    
}

fireZoneTable <- do.call("rbind", fireCycles)
rownames(fireZoneTable) <- 1:nrow(fireZoneTable)

### saving fireZones
writeRaster(fireZones, file = "fireZones.tif", overwrite = T)
writeRaster(studyArea, file = "studyArea.tif", overwrite = T)
### creating a table with fire cycles
write.csv(fireZoneTable, file = "fireZoneTable.csv", row.names = F)


