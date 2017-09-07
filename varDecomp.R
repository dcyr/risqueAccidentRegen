###### Dominic Cyr, in collaboration with Tadeusz Splawinski and Sylvie Gauthier
rm(list = ls())
setwd("~/Travail/SCF/regenFailureRiskAssessment")
####################################################################################################
####################################################################################################
wwd <- paste(getwd(), Sys.Date(), sep = "/")
dir.create(wwd)
setwd(wwd)

require(dplyr)


dfCover <- read.csv("../compiledOutputs/dfCover.csv")
## ordering levels
harvValues <- c("HarvestRate: 0 %/yr" = 0,
                "HarvestRate: 0.51 %/yr" = 0.0051,
                "HarvestRate: 1 %/yr" = .01,
                "HarvestRate: 1.5 %/yr" = 0.015)
dfCover$harvestTreatment <- harvValues[as.character(dfCover$harvestTreatment)]
dfCover$cover <- factor(dfCover$cover, levels = c("Black Spruce", "Jack Pine", "Global"))


dfCover <- dfCover %>%
    arrange(scenario, cover, simID, harvestTreatment, productivity,  timestep)


### padding data.frame for zero propBurned
idVars <- c("scenario",  "simID", "harvestTreatment", "productivity")#,"cover",  "meanRate", "areaCover_ha")
dfCoverPad <- dfCover %>%
    distinct(scenario, simID, harvestTreatment, productivity) %>%
    merge(expand.grid(cover = levels(dfCover$cover),
                      timestep = 1:50), all = T) %>%
    merge(dfCover[,c(idVars, "cover", "timestep", "propBurned")], all.x = T) %>%
    mutate(propBurned = ifelse(is.na(propBurned), 0, propBurned)) %>%
    group_by(scenario, simID, harvestTreatment, productivity, cover) %>%
    mutate(cumulPropBurned = cumsum(propBurned)) %>%
    ungroup()
   






df <- filter(dfCoverPad, timestep == 50, cover == "Jack Pine")

foo <- lm(cumulPropBurned ~ scenario + harvestTreatment + productivity, data = df)


head(df)


foo <- dfCoverPad %>%
    
  summary(foo) 



mutate(mtcars, displ_l = disp / 61.0237)
transmute(mtcars, displ_l = disp / 61.0237)


head(foo)

head(dfCover)


%>%
    mutate(propBurned = 0) %>%
    merge(dfCover[,c(idVars, "propBurned")]) 


dfCoverPad <- arrange(dfCoverPad, scenario, harvestTreatment,  simID, productivity, cover,timestep)
head(dfCoverPad)


?expand.grid(idVars[1] = levels(dfCover[,idVars[1]]),
            idVars[2] = unique(dfCover[,idVars[2]]),
            idVars[3] = levels(dfCover[,idVars[3]]),
            idVars[4] = levels(dfCover[,idVars[4]]),
            
                               c("Low", "High"), Factor2 = c("Low", "High")