---
title: "Analyzing risk of regeneration failure in the managed boreal forest of North-western Quebec"
author: "Repository maintained by Dominic Cyr; Project conducted in collaboration with Sylvie Gauthier and Tadeusz Splawinski"
output: 
  html_document: 
    keep_md: yes
---

[Dominic Cyr][5]

Updated on Jun 23 2018


-------


### General description

The present repository contains the information necessary to reproduce the simulation experiment conducted by Splawinski _et al_. (under review). That experiment aims at assessing the relative risk of regeneration failure following successive disturbances in a variety of contexts.

The following provides a general description of the experiment and points to more detailed content that describes the simulation steps and data processing pipeline in order to reproduce the experiment conducted by Splawinski _et al_. (under review). We also invite you to take a look at [this graphic illustration of the simulation and data processing pipeline][6], which will help you navigate through the various scripts and input files needed to reproduce the experiment.


-----------


### Study area

The study area is located east of [Lake Mistassini][7], Quebec, Canada, just south of the northern limit of managed forest in Central Quebec. The total area is 1.2 Mha in size, excluding major water bodies. 


![](figures/studyAreaLargeScale.png)
------------


## Fire model

The fire model is based on the simulation experiment described in [Cyr et al. (2016)][1]. The core of the model is a [cellular automaton][2] of stochastic propagation of fires to neighbouring cells until a predefined fire size is reached. 

Targeted fire regimes are predefined by the user, i.e. the length of the fire cycle (or average area burned annually) and fire size distribution must be provided. At this stage, fire spread probability is homogeneous across all eligible (flammable) cells, although the current model structure could allow for differential fire spread probabilities. Only ignition probabilities currently vary among zones subjected to different fire regimes.

-----------

### Example of a 50-year simulation

![](figures/tsfExample.gif)

-----------

### Fire simulation ensemble

To verify the fire model, we conducted 1000 simulations under a baseline scenario (aligned with recent past fire activity; [Gauthier et al 2015][4]) and we computed the following summary statistics.

![](figures/realizedFC_baseline.png)

Targeted fire cycle was simulated with good precision when considering the entire study area (101-year global fire cycle).

However, targeted fire cycles were not achieved for the smaller fire zones contained within the study area. Because our implementation of fire simulated its contagious nature, adjacent fire zones affected each other's fire regimes. This is particularly noticeable for smaller fire zones that should have been subjected to longer fire cycles, but that are located near fire zones with much higher fire activity (ex. zones G7 and G8). This is normal considering that fire zone boundaries are not impermeable to fire spread.

Moreover, the smaller the fire zone the more dispersed the realized fire cycles are going to be. Similarly, the longer the fire cycle is relative to the length of the simulation (50 years, in the current experiment), the more dispersed the realized fire cycles are going to be. This is coherent with empirical observations and results from past simulation experiments.

-----------

## Harvesting

Harvesting is simulated as a stratified-random draw across all eligible stands ([script here][3]). Currently, the only eligibility condition is stand age, and harvesting is stratified among cover types.


-----------


## Experimental design

The simulation ensemble was produced using a full factorial design that included two climate scenarios and four harvesting treatments, which were each simulated a thousand times for a total of 8000 independent realizations. For each of these realizations, the occurrences of potential regeneration failure events were verified against three sets of stand maturity thresholds.

-----------


### Main results

[Under review. Will be updated when published]

-----------

## Additional information

In the root folder you will find the _R_ scripts that must be executed by the user, as opposed to those located in the [./scripts][2] folder. The latter are sourced by the former and do not necessitate direct interactions by the user unless he/she wishes to modify them. Some input files that are required for the landscape initiation steps (ex. forest inventories) are currently not publicly available. However, the raster files created during these steps are available and allow to reproduce all downstream steps.

Don't hesitate to contact me should you have requests, questions, comments or suggestions for improvement or additional functionalities.

[Dominic Cyr][5]

[1]: http://www.mdpi.com/1999-4907/7/7/131/html
[2]: https://github.com/dcyr/risqueAccidentRegen/blob/master/scripts/fireSpreadFnc.R
[3]: https://github.com/dcyr/risqueAccidentRegen/blob/master/scripts/simHarvestFnc.R
[4]: http://www.nrcresearchpress.com/doi/10.1139/cjfr-2014-0125#.WEHWNnUrLRY
[5]: http://dominiccyr.ca
[6]: https://github.com/dcyr/risqueAccidentRegen/blob/master/pipeline.md
[7]: https://goo.gl/maps/j8LpDsizptm
