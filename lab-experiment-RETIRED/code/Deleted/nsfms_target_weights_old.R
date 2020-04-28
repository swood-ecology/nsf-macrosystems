# Set-up targets with edited calculations

# last edit 3/9/2020 by AP 
# error in calculation of target fresh weight for water holding capacity
# multiplied fraction at 100% by target fraction instead of correctly multiplying g H2O g dry soil-1 @ 100% by target fraction

library(tidyverse)

path <- "C:/Users/alexa/Dropbox (Yale_FES)/Macrosystems Biol Bradford Wieder Wood 2019-2024/Microcosm experiments/"
setwd(path)

microcosmDesign <- read.csv("metadata/microcosm_exp_design.csv")
SoilGWC <- read.csv("calculated-data/nsfms_soil_GWC_calc.csv")
SoilWHC <- read.csv("calculated-data/nsfms_soil_WHC_calc.csv")
LitterWHC <- read.csv("calculated-data/nsfms_litter_WHC_calc_aggregate.csv")



# Add prefixes to dataframe names
# Soil Gravimetric Soil Moisture
names(SoilGWC)[3:5] <- paste("soilGWC.", names(SoilGWC[,3:5]), sep = "")

# Soil water holding capacity
names(SoilWHC)[3:6] <- paste("soilWHC.", names(SoilWHC[,3:6]), sep = "")


# split into two components with the experimental with litter and soil matrix and soil standards
# these have been called experiment a and experimenet b in the microcosm design raw and code file



setwd("calculated-data")
bind_rows(
  filter(microcosmDesign, microcosmExp == "a") %>%
    left_join(., SoilGWC, by = "plotID") %>%
    left_join(., SoilWHC, by = "plotID") %>%
    add_column(., moistureMasspergDryLitter = LitterWHC[1, "moistureMasspergDryLitter"]) %>%
    add_column(., soilDryMass = 0.25) %>%
    add_column(., litDryMass = 1) %>%
    mutate(
      soilFreshMass = soilDryMass/(1-soilGWC.moistureFraction),
      
      moistureSoilMassAtTargetWHC = soilWHC.moistureMasspergDrySoil*(moistTRT/100), # g H2O g soil-1 @ target WHC
      targetSoilFreshMass = (soilDryMass*moistureSoilMassAtTargetWHC)+soilDryMass, # mass of fresh weight @ target WHC
      
      moistureLitterMassAtTargetWHC = moistureMasspergDryLitter*(moistTRT/100),
      targetLitFreshMass = (litDryMass*moistureLitterMassAtTargetWHC)+litDryMass,
      
      totalDryMass = litDryMass + soilDryMass,
      totalFreshMass = soilFreshMass + litDryMass,
      targetFreshMass = targetSoilFreshMass + targetLitFreshMass,
      initialWaterAddition = targetFreshMass- totalFreshMass
    ) ,
  filter(microcosmDesign, microcosmExp == "b") %>% # soil standards
    left_join(., SoilGWC, by = "plotID") %>%
    left_join(., SoilWHC, by = "plotID") %>%
    add_column(., moistureMasspergDryLitter = LitterWHC[1, "moistureMasspergDryLitter"]) %>%
    add_column(., soilDryMass = 6) %>% # 6 grams dry mass soil
    add_column(., litDryMass = 0) %>% # no litter
    mutate(
      soilFreshMass = soilDryMass/(1-soilGWC.moistureFraction),
      
      moisturepergSoilMassAtTargetWHC = soilWHC.moistureMasspergDrySoil*(moistTRT/100), # g H2O g soil-1 @ target WHC
      targetSoilFreshMass = (soilDryMass*moisturepergSoilMassAtTargetWHC)+soilDryMass, # mass of fresh weight @ target WHC
      
      moisturepergLitterMassAtTargetWHC = moistureMasspergDryLitter*(moistTRT/100),
      targetLitFreshMass = (litDryMass*moisturepergLitterMassAtTargetWHC)+litDryMass,
      
      totalDryMass = litDryMass + soilDryMass,
      totalFreshMass = soilFreshMass + litDryMass,
      targetFreshMass = targetSoilFreshMass + targetLitFreshMass,
      initialWaterAddition = targetFreshMass- totalFreshMass
    )
) %>% 
  select(c(labID, soilFreshMass, litDryMass, totalFreshMass, targetFreshMass, initialWaterAddition))  %>%
write.csv("nsfms_set_up_ideal_targets_edit.csv")

moistureMassAtTargetWHC
# ACTUAL targets based on weighed masses of tube, fresh soil, and dry litter

# To use real raw data of tube mass and measured soil and litter fresh mass
setwd(path)
massTarget <- read.csv("calculated-data/nsfms_set_up_ideal_targets_edit.csv")
initialMass <- read.csv("raw-data/nsfms_microcosm_mass_setup.csv")


setwd("calculated-data")

# old now... because things happen!

# left_join(microcosmDesign, SoilGWC, by = "plotID") %>%
#   left_join(., SoilWHC, by = "plotID") %>%
#   left_join(., initialMass, by = "labID") %>%
#   add_column(., LitterWHC.moistureFraction = LitterWHC[1, "moistureFraction"]) %>%
# mutate(
#   actualSoilFreshMass = tube.fresh.soil.mass - tube.mass, 
#   actualLitFreshMass = tube.fresh.soil.and.litter.mass - tube.fresh.soil.mass, # since litter is oven dry - fresh and dry mass are equivalent
#   actualSoilDryMass = actualSoilFreshMass*(1-soilGWC.moistureFraction),
#   targetSoilFreshMass = actualSoilDryMass/(1-soilWHC.moistureFraction*(moistTRT/100)), # Fresh soil mass calculated by water holding capacity * % of that maximum for the treatment
#   targetLitFreshMass = actualLitFreshMass/(1-LitterWHC.moistureFraction*(moistTRT/100)),
#   targetFreshMass = targetSoilFreshMass + targetLitFreshMass,
#   targetTotalMass = targetFreshMass + tube.mass
# ) %>% select(c(labID, targetTotalMass)) %>%
#   write.csv("nsfms_set_up_actual_target.csv")
# 

# Edits for specific microcosms where either soil-litter was lost or something else
# on 2/25/2020, air dried mass of HARV-38 was then used to add to microcosm so initial GWC was modified



editGWC <- left_join(microcosmDesign, SoilGWC, by = "plotID")
editGWC[editGWC$labID == 388 | editGWC$labID == 389 | editGWC$labID == 390,"soilGWC.moistureFraction"] <- c(0.657423974,   0.614302885,  0.571689399)


left_join(editGWC, SoilWHC, by = "plotID") %>%
  left_join(., initialMass, by = "labID") %>%
  add_column(., moistureMasspergDryLitter = LitterWHC[1, "moistureMasspergDryLitter"])%>%
  mutate(
    actualSoilFreshMass = tube.fresh.soil.mass - tube.mass, 
    actualLitFreshMass = tube.fresh.soil.and.litter.mass - tube.fresh.soil.mass, # since litter is oven dry - fresh and dry mass are equivalent
    actualSoilDryMass = actualSoilFreshMass*(1-soilGWC.moistureFraction),
    
    moisturepergSoilMassAtTargetWHC = soilWHC.moistureMasspergDrySoil*(moistTRT/100), # g H2O g soil-1 @ target WHC
    targetSoilFreshMass = (actualSoilDryMass*moisturepergSoilMassAtTargetWHC)+actualSoilDryMass, # mass of fresh weight @ target WHC
    
    moisturepergLitterMassAtTargetWHC = moistureMasspergDryLitter*(moistTRT/100),
    targetLitFreshMass = (actualLitFreshMass*moisturepergLitterMassAtTargetWHC)+actualLitFreshMass,
  
    targetFreshMass = targetSoilFreshMass + targetLitFreshMass,
    targetTotalMass = targetFreshMass + tube.mass
    
    
  )  %>% select(c(labID, tube.mass, actualSoilDryMass, actualLitFreshMass, moisturepergSoilMassAtTargetWHC, moisturepergLitterMassAtTargetWHC, targetTotalMass)) %>%
  write.csv("nsfms_set_up_actual_mass_initial.csv")

# this should be updated each time - last updated 3/8/2020
# each updated time, when a loss of mass happens should be time stamped AS A NEW CSV
# to be able to make accurate calculations at each time point




