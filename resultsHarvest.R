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

####################################################################
####################################################################
######
require(dplyr)

outputCompiled <- get(load("../compiledOutputs/outputCompiledFinalEnsemble.RData"))
outputCompiled <- filter(outputCompiled, )
outputCompiled <- get(load("../compiledOutputs/outputCompiledHarvest.RData"))


######################
#### temporary shit
######################
outputCompiled <- outputCompiled[,-c(7,8)]


head(outputCompiled)

summaryHarvest <- outputCompiled %>%
    group_by(scenario, harvestTreatment, year, coverType) %>%
    summarise(p25Harvest = quantile(areaHarvested_ha, .25),
              p50Harvest = quantile(areaHarvested_ha, .5),
              p99Harvest = quantile(areaHarvested_ha, .99),
              p99Harvest = quantile(areaHarvested_ha, .999))

3075/604025
head(outputCompiled)


as.data.frame(distinct(outputCompiled, harvestTreatment, scenario, year, coverType, areaHarvested_ha))
distinct()

as.data.frame(summaryHarvest[1:100,])
