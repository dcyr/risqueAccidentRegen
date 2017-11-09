###### Dominic Cyr, in collaboration with Tadeusz Splawinski and Sylvie Gauthier
rm(list = ls())
setwd("~/Travail/SCF/regenFailureRiskAssessment")
####################################################################################################
####################################################################################################
wwd <- paste(getwd(), Sys.Date(), sep = "/")
dir.create(wwd)
setwd(wwd)

require(dplyr)


#################################################################################
## fetching realized fire cycles
fireOutputCompiled <- get(load("../compiledOutputs/outputCompiled.RData"))
initYear <- 2015
#################################################################################
## summarizing fire regimes
fireSummary <- fireOutputCompiled %>%
    filter(zone == "total") %>%
    group_by(scenario, replicate) %>%
    summarize(meanTSF = round((mean(meanTSF))),
              fireCycle = round((1/mean(areaBurned_ha/areaZone_ha))),
              propAAB = mean(areaBurned_ha/areaZone_ha)) %>%
    arrange(scenario, replicate) %>%
    mutate(simID = replicate) %>%
    select(scenario, simID, propAAB)

## creating a more complete df with all years, and cumulative pAAB

firePAAB <- fireOutputCompiled %>%
    filter(zone == "total") %>%
    group_by(scenario, replicate) %>%
    arrange(year) %>%
    mutate(areaBurnedCumul_ha = cumsum(areaBurned_ha),
           propAAB = (areaBurnedCumul_ha/areaZone_ha)/year) %>%
    ungroup() %>%
    mutate(simID = replicate) %>%
    arrange(scenario, simID, year) %>%
    select(scenario, year, simID, propAAB)


#################################################################################
## fetching regen failure results
dfCover <- read.csv("../compiledOutputs/dfCover.csv")
#################################################################################
## Tidying things up

## ordering levels
harvValues <- c("HarvestRate: 0 %/yr" = 0,
                "HarvestRate: 0.51 %/yr" = 0.0051,
                "HarvestRate: 1 %/yr" = .01,
                "HarvestRate: 1.5 %/yr" = 0.015)
maturityThresholds <- list("Jack Pine" = c("Early maturity" = 10,
                                           "Intermediate maturity" = 30,
                                           "Early maturity" = 70),
                           "Black Spruce" = c("Early maturity" = 30,
                                              "Intermediate maturity" = 50,
                                              "Early maturity" = 90))

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



#################################################################################
## Performing variance partitionning for all years and species

require(doSNOW)
require(parallel)
clusterN <-  max(1, floor(0.9*detectCores()))  ### choose number of nodes to add to cluster.
# #######
cl = makeCluster(clusterN, outfile = "") ## 
registerDoSNOW(cl)
#################################################################################
### plot time step == 50
tsPlot <- 50
cutoff <- 0.0001
varPartition <- foreach(s = c("baseline", "RCP85"), .combine = "rbind") %do% {
    foreach(sp = c("Jack Pine", "Black Spruce"), .combine = "rbind") %do% {
        foreach(y = unique(dfCoverPad$timestep), .combine = "rbind") %dopar% {
            require(dplyr)
            require(vegan)
            require(venneuler)
            ## selecting fire year
            fireDf <- firePAAB %>%
                filter(year == y, scenario == s) %>%
                select(scenario, simID, propAAB)
            ## selecting corresponding regen failure
            df <- dfCoverPad %>%
                filter(timestep == y, cover == sp) %>%
                mutate(maturity = maturityThresholds[[sp]][productivity]) %>%
                merge(fireDf) %>%
                select(propAAB, maturity, harvestTreatment, cumulPropBurned)
            ## structuring data for varpart
            # response variable (can be a matrix, here it's only a vector)
            Y <- data.frame(cumulPropBurned = df$cumulPropBurned)
            X <- df[,c("propAAB", "maturity", "harvestTreatment")]
            ## performing variance partitionning

            part <- varpart(Y, ~propAAB, ~maturity, ~harvestTreatment, data = X)
            
            
            # if(y %in% tsPlot) {
            #     ## classic Venn
            #     png(filename = paste0("vennDiagClassic_", gsub(" ", "", sp), "_",
            #                           s, "_", y, ".png"),
            #         width = 5, height = 4, units = "in", res = 600, pointsize=8)
            #     
            #        
            #         plot(part, main = "foo",
            #              Xnames = c("Burn rate", "Age of maturity", "Harvesting level"),
            #              cutoff = cutoff)
            #     
            #     dev.off()
            #     ### Venn-Euler (proportional)
            #     totalFrac <- part$part$frac[1:3,3]
            #     indFrac <-     part$part$indfract[4:7, 3]
            #     indFrac <- ifelse(indFrac<cutoff,0,indFrac)
            #     residuals <-  part$part$indfract[8,3]
            #     v <- venneuler(c("Fire" = totalFrac[1],
            #                      "Maturity" = totalFrac[2],
            #                      "Harvesting" = totalFrac[3],
            #                      "Fire&Maturity"=  indFrac[1],
            #                      "Fire&Harvesting"=  indFrac[2],
            #                      "Maturity&Harvesting"=  indFrac[3],
            #                      "Fire&Maturity&Harvesting"=  indFrac[4]))
            #     v$labels <- paste0(v$labels, "\n(", round(totalFrac*100, 1), "%)")
            #     
            #     png(filename = paste0("vennDiagProp_", gsub(" ", "", sp), "_",
            #                           s, "_", y, ".png"),
            #         width = 5, height = 4, units = "in", res = 600, pointsize=8)
            #         
            #         plot(v, main = paste0("Variance partitioning\n",
            #                               sp, " (timestep ", y, ")"))
            #     dev.off()
            # }
            
            x <- part$part$fract
            varPartition <- data.frame(scenario = s,
                                       year = y,
                                       species = sp,
                                       x,
                                       component = rownames(x))
            print(paste(s, sp, y))
            return(varPartition)
            
        }
    }
}
stopCluster(cl)    
   

##########################################################
##########################################################
### illustrating variation partitioning final time step
##########################################################

### replacing components with proper names
compNames <- c("[a+d+f+g] = X1" = "Fire",
               "[b+d+e+g] = X2" = "Maturity",
               "[c+e+f+g] = X3" = "Harvesting")


varPart <- varPartition %>%
    filter(component %in% names(compNames),
           year == tsPlot) %>%
    mutate(Adj.R.square = ifelse(Adj.R.square<cutoff,0,Adj.R.square),
           component = compNames[as.character(component)]) %>%
    select(scenario, year, species, Adj.R.square, component)
## adding residuals
varPart <- varPart %>%
    group_by(scenario, year, species) %>%
    summarize(Adj.R.square = 1-sum(Adj.R.square)) %>%
    mutate(component = "Residuals") %>% 
    ungroup() %>%
    rbind(varPart)

require(ggplot2)
cols <- c("darkred", "darkgoldenrod3", "palegreen3", "grey25")#"aquamarine3")


p <- ggplot(aes(y=Adj.R.square, x=scenario, fill = component), data = varPart) +
    geom_bar(stat = 'identity') +
    geom_hline(yintercept = c(0,1), linetype = 1, colour = "grey25") +
    geom_col(position = position_stack(reverse = T)) +
   # coord_flip() +
    geom_text(aes(label=paste0(round(100*Adj.R.square,1), "%")),
              position = position_stack(reverse = T), angle = 0, vjust = 1.25, hjust = 0.5,
              check_overlap = T, size = rel(2.5), colour = "white") +
    # geom_text(aes(x = c(1,2,1,2), y = 1, label=paste0("residual\n12%")),
    #           angle = 0, vjust = 1.25, hjust = 0.5,
    #           check_overlap = T, size = rel(2.5), colour = "white") +
    facet_wrap(~species, nrow = 1) +
    scale_fill_manual('', values = cols,
                       guide = guide_legend(reverse=TRUE))



png(filename = "variationDecomp.png",
    width = 7, height = 5, units = "in", res = 300, pointsize=8)

print(p +
          theme_dark()+
          labs(title ="Variation partitioning of rate of potential regeneration failure",
               subtitle = "After 50 years of simulation (horizon 2065)",
               caption = paste("*Treatment factors were orthogonal, hence the absence of shared explained variation." ),
               x = "",
               y = "Adj.R.square*\n") +
          theme(#legend.position="top", legend.direction="horizontal",
               # axis.text.x = element_text(angle = 45, hjust = 1),
               plot.caption=element_text(size=rel(0.75)))
      )
    

dev.off()




##########################################################
##########################################################
### illustrating variation partitioning all along the simulations
##########################################################

### replacing components with proper names
compNames <- c("[a+d+f+g] = X1" = "Fire",
               "[b+d+e+g] = X2" = "Maturity",
               "[c+e+f+g] = X3" = "Harvesting")


varPart <- varPartition %>%
    filter(component %in% names(compNames)) %>%
    mutate(Adj.R.square = ifelse(Adj.R.square<cutoff,0,Adj.R.square),
           component = compNames[as.character(component)]) %>%
    select(scenario, year, species, Adj.R.square, component)
## adding residuals
varPart <- varPart %>%
    group_by(scenario, year, species) %>%
    summarize(Adj.R.square = 1-sum(Adj.R.square)) %>%
    mutate(component = "Residuals") %>% 
    ungroup() %>%
    rbind(varPart)



p <- ggplot(data = varPart,
            aes(x = year+2015, y = Adj.R.square,
                group = component, fill = component, color = component)) +
    #geom_line() +
    geom_area(position = 'stack', show.legend = FALSE) +
    geom_col(position = position_stack(reverse = T)) +
    facet_grid(species~scenario) +
    scale_colour_manual(values = cols,
                        guide=FALSE) +
    scale_fill_manual(values = cols,
                      guide = guide_legend(reverse=TRUE,
                                           #override.aes = list(colour="black"),
                                           title = ""))


## adding labels

varPartLabels <- varPart %>%
    filter(year == 50) %>%
    arrange(component) %>%
    group_by(scenario, year, species) %>%
    mutate(cumul.R.square = cumsum(Adj.R.square))

png(filename = "variationDecompEntireSims.png",
    width = 9, height = 6, units = "in", res = 300, pointsize=8)

print(p +
          theme_dark()+
          labs(title ="Variation partitioning of rate of potential regeneration failure",
               subtitle = "Over 50 years of simulation (horizon 2015-2065)",
               caption = paste("*Treatment factors were orthogonal, hence the absence of shared explained variation." ),
               x = "",
               y = "Adj.R.square*\n") +
          xlim(c(2015, 2069)) +
          geom_text(data = varPartLabels,
                    aes(x = 2066, y = cumul.R.square,
                        label = paste(round(100*Adj.R.square, 1), "%")),
                        angle = 0, vjust = 1.25, hjust = 0,
                        check_overlap = F, size = rel(2.5), colour = "white") +
          theme(#legend.position="top", legend.direction="horizontal",
              # axis.text.x = element_text(angle = 45, hjust = 1),
              plot.caption=element_text(size=rel(0.75)))
)


dev.off()

