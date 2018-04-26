####################################################################
####################################################################
####################################################################
rm(list=ls())
####################################################################
####################################################################
setwd("~/Travail/SCF/regenFailureRiskAssessment/")
wwd <- paste(getwd(), Sys.Date(), sep="/")
dir.create(wwd)
setwd(wwd)
rm(wwd)


require(dplyr)
require(raster)
require(ggplot2)

####################################################################
####################################################################
######
### fetching compiled results
outputCompiled <- get(load("../compiledOutputs/outputCompiledHarvest.RData"))
# save(outputCompiled, file = "outputCompiledHarvest.RData")
# write.csv(outputCompiled,, file = "outputCompiledHarvest.csv", row.names = F)
# colnames(outputCompiled)[7] <- "areaCoverTypeTotal_ha"
# head(outputCompiled)

# save(outputCompiled, file)
nSims <- nrow(distinct(outputCompiled, scenario, replicate))

# ### fetching covertype raster
# studyArea <- raster("../data/studyArea.tif")
# coverTypes <- raster("../data/coverTypes.tif")
# coverTypesDf <- get(load("../data/coverTypesDf.RData"))
# coverTypesTable <- distinct(coverTypesDf[,c("ID", "descrip")])
# coverTypesTable <- coverTypesTable[which(coverTypesTable$descrip %in% c("EN", "PG")), ]
# coverTypes[coverTypes %in% coverTypesTable$ID == F] <- NA
# coverTypesID <- unique(values(coverTypes))
# coverTypesID <- coverTypesID[!is.na(coverTypesID)]
# ##
# convFactor <- prod(res(studyArea))/10000### to convert to hectares
# coverTypeArea <-  zonal(!is.na(coverTypes), coverTypes, sum)
# coverTypeArea <- data.frame(coverType = as.character(coverTypesTable[match(coverTypesID, coverTypesTable$ID),"descrip"]),
#                             coverTypeArea_ha = coverTypeArea[,2] * convFactor)


### summarizing results, percentile & such
summaryHarvest <- outputCompiled %>%
    group_by(scenario, harvestTreatment, year, coverType, areaCoverTypeTotal_ha) %>%
    summarise(p01Harvest_ha = quantile(areaHarvested_ha, .01),
              p05Harvest_ha = quantile(areaHarvested_ha, .05),
              p25Harvest_ha = quantile(areaHarvested_ha, .25),
              p50Harvest_ha = quantile(areaHarvested_ha, .5),
              p75Harvest_ha = quantile(areaHarvested_ha, .75),
              p95Harvest_ha = quantile(areaHarvested_ha, .95),
              p99Harvest_ha = quantile(areaHarvested_ha, .99))

#summaryHarvest <- merge(summaryHarvest, coverTypeArea)

require("RColorBrewer")


for (p in c("p01", "p05", "p25", "p50", "p75", "p95")) {
    riskTol <- paste0(100-as.numeric(gsub("[^0-9]","", p)), "%")
    percentile <- as.numeric(gsub("[^0-9]","", p))
    varName <- paste0(p, "Harvest_ha")
    df <- summaryHarvest[,c("scenario", "coverType", "harvestTreatment", "year", "areaCoverTypeTotal_ha", varName)]
    ### reformatting harvest treatments for better readability
    df$harvestTreatment <- as.numeric(as.character(df$harvestTreatment))
    df$harvestTreatment <- paste(100 * df$harvestTreatment, "%")
    colnames(df)[which(colnames(df)==varName)] <- "value"
    
    
    m <- ggplot(df, aes(x = year + 2015, y = (value/areaCoverTypeTotal_ha)*100,
                        linetype = harvestTreatment, colour = harvestTreatment)) +
        facet_grid(coverType ~ scenario) +
        geom_line(size = 0.75) +
        # scale_color_manual("", values = c(EN = "darkseagreen3",
        #                                   PG ="darkred"),
        #                    # labels=c(baseline = "scénario de référence",
        #                    #          RCP85 = "scénario RPC 8.5")) +
        #                    labels=c(EN = "Épinette noire",
        #                             RCP85 = "Pin gris")) +
        # scale_fill_manual("", values = c(baseline = "darkolivegreen3",
        #                                   RCP85 ="red3"),
        #                    labels=c(baseline = "scénario de référence",
        #                             RCP85 = "scénario RPC 8.5")) +
        # scale_linetype_manual("Taux de récolte\nannuel ciblé", values = c(1, 2, 3, 4, 5, 6),
        #                       label = c("0.51%","1%", "1.5%")) 
        scale_colour_manual("Taux de récolte\nannuel ciblé",
                            values = brewer.pal(length(unique(df$harvestTreatment)), "Greens"))
                                                 #label = c("0.51%","1%", "1.5%")) 
    
    
    
    png(filename= paste0("harvestRealized_", p, ".png"),
        width = 8, height = 6, units = "in", res = 600, pointsize=10)
    options(scipen=999)
    
    print(m + theme_dark() +
              
              theme(legend.position="top", legend.direction="horizontal",
                    legend.title = element_text(size = rel(0.85)),
                    title = element_text(size = rel(0.85)),
                    #plot.subtitle = element_text(size = rel(1)),
                    plot.caption = element_text(size = rel(0.65))) +
              labs(title = "Analyse de risque de rupture d'approvisionnement",
                   #subtitle = paste0(percentile, "e percentile"),
                   subtitle = paste0(nSims, " simulations"),
                   caption = paste0("Âge de récolte - Épinette noire: 90 ans\n",
                                    "Pin gris: 76 ans\n",
                                    "Cycle des feux - baseline: 101 ans\n",
                                    "RCP 8.5 (2015-2040): 57 ans\n",
                                    "RCP 8.5 (2041-2065): 33 ans"),
                   x = "",
                   y = paste0("Superficies récoltées annuellement (%)\n", percentile, "e percentile")))
    # labs(title = "Figure 10: Proportion cumulative du territoire productif où une forêt immature a brûlé",
    #      subtitle = paste0("Valeurs médianes, issues d'un ensemble de ", nRep,  " simulations."),
    #      caption = paste0("Maturité épinette: ", maturity["EN"], " ans; ",
    #                       "Maturité pin gris: ",maturity["PG"], " ans"),
    #      x = "",
    #      y = "Proportion cumulée"))
    
    
    dev.off()
    
}



colorRampPalette()

