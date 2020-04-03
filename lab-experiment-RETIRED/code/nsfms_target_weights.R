# Set-up targets with edited calculations

# last edit 3/12/2020 by AP 
# error in calculation of target fresh weight for water holding capacity
# multiplied fraction at 100% by target fraction instead of correctly multiplying g H2O g dry soil-1 @ 100% by target fraction


library(tidyverse)

path <- "C:/Users/alexa/Dropbox (Yale_FES)/Macrosystems Biol Bradford Wieder Wood 2019-2024/microcosms/"
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



##### Idealized target weights with 0.25 g dry mass soil and 1.0 g dry mass litter ####

# split into two components with the experimental with litter and soil matrix and soil standards
# these have been called experiment a and experimenet b in the microcosm design raw and code file

setwd("calculated-data")
bind_rows(
  filter(microcosmDesign, microcosmExp == "a") %>%
    left_join(., SoilGWC, by = "plotID") %>%
    left_join(., SoilWHC, by = "plotID") %>%
    add_column(., litWHC.moistureFraction = LitterWHC[1, "moistureFraction"]) %>%
    add_column(., soilDryMass = 0.25) %>%
    add_column(., litDryMass = 1) %>%
    mutate(
      soilFreshMass = soilDryMass/(1-soilGWC.moistureFraction),
      
      totalDryMass = litDryMass + soilDryMass,
      totalFreshMass = soilFreshMass + litDryMass,

      moistureSoilPercentatTarget =  soilWHC.moistureFraction*(moistTRT/100),
      targetSoilFreshMass = soilDryMass*(1/(1-(moistureSoilPercentatTarget))),
      
      moistureLitterPercentatTarget =  litWHC.moistureFraction*(moistTRT/100),
      targetLitterFreshMass = litDryMass*(1/(1-(moistureLitterPercentatTarget))),
      
      targetFreshMass = targetSoilFreshMass + targetLitterFreshMass
      
    ) ,
  filter(microcosmDesign, microcosmExp == "b") %>% # soil standards
    left_join(., SoilGWC, by = "plotID") %>%
    left_join(., SoilWHC, by = "plotID") %>%
    add_column(., litWHC.moistureFraction = LitterWHC[1, "moistureFraction"]) %>%
    add_column(., soilDryMass = 6) %>% # 6 grams dry mass soil
    add_column(., litDryMass = 0) %>% # no litter
    mutate(
      soilFreshMass = soilDryMass/(1-soilGWC.moistureFraction),
      
      totalDryMass = litDryMass + soilDryMass,
      totalFreshMass = soilFreshMass + litDryMass,
      
      moistureSoilPercentatTarget =  soilWHC.moistureFraction*(moistTRT/100),
      targetSoilFreshMass = soilDryMass*(1/(1-(moistureSoilPercentatTarget))),
      
      moistureLitterPercentatTarget =  litWHC.moistureFraction*(moistTRT/100),
      targetLitterFreshMass = litDryMass*(1/(1-(moistureLitterPercentatTarget))),
      
      targetFreshMass = targetSoilFreshMass + targetLitterFreshMass
      
    )
) %>% 
  select(c(labID, soilFreshMass, litDryMass, totalFreshMass, targetFreshMass))  %>%
  write.csv("nsfms_set_up_ideal_targets.csv")



##### ACTUAL target weights based on weighed masses of tube, fresh soil, and dry litter #####

# To use real raw data of tube mass and measured soil and litter fresh mass
setwd(path)
massTarget <- read.csv("calculated-data/nsfms_set_up_ideal_targets.csv")
initialMass <- read.csv("raw-data/nsfms_microcosm_mass_setup.csv")


##### These are for changes in raw data initial masses ####

# edit for initial mass soil GWC, as soils were dried seperately on tins and recalculated their WHC

editGWC <- left_join(microcosmDesign, SoilGWC, by = "plotID")
editGWC[editGWC$labID == 388 | editGWC$labID == 389 | editGWC$labID == 390,"soilGWC.moistureFraction"] <- c(0.657423974,   0.614302885,  0.571689399)

# method to estimate how much mass was lost from labID 155
  # an unknown amount spilled on 3/5/2020 during flushing
  # it was then measured that it had 12.5361 g
  # 1. estimate how much water loss per day is experienced

wet_up_3 <- read.csv("raw-data/nsfms_wet_up_3.csv")
wet_up_4 <- read.csv("raw-data/nsfms_wet_up_4.csv")

# mass measurement on 3/6/2020; wetup 5
mass1_155 <- filter(wet_up_4, labID == 155) %>% select(total.mass)
# mass measurement on 3/12/2020 
mass2_155 <- 12.7226
daydiff <- 6 # difference in days between wetup 4 and wetup 5
changePerDay155 <- (mass2_155 - mass1_155)/daydiff # rate of mass loss per day

  # 2. use that to estimate how much fresh weight was there 

mass3_155 <- filter(wet_up_3, labID == 155) %>% select(total.mass)
daydiff2 <- 5 # difference in days before wetup 3 and flushing accident
massInitial_155 <- mass3_155+daydiff2*changePerDay155

massData <- read_csv("calculated-data/nsfms_set_up_actual_mass_initial_alldata.csv")


newDrymass155 <- filter(massData, labID == 155) %>% 
  mutate(    
    percentMoisture = 1-(actualSoilDryMass+actualLitFreshMass)/as.numeric(massInitial_155),
    freshMassLoss = as.numeric(massInitial_155) - 12.5361, # measured after mass loss took place
    moistureLoss = freshMassLoss*percentMoisture,
    dryMassLoss = freshMassLoss - moistureLoss,
    SoilDryMassLoss = dryMassLoss * actualSoilDryMass/(actualLitFreshMass+actualSoilDryMass),
    newSoilDryMass = actualSoilDryMass - SoilDryMassLoss,
    LitDryMassLoss = dryMassLoss * actualLitFreshMass/(actualLitFreshMass+actualSoilDryMass),
    newLitDryMass= actualLitFreshMass - LitDryMassLoss
  ) %>% select(labID, newSoilDryMass, newLitDryMass)
    

# within this, there is an edit to the actual dry soil and dry litter using the replace function, followed by the values caluclated above
# this was done 3/12/2020 for the wet_up_5 ++


left_join(editGWC, SoilWHC, by = "plotID") %>%
  left_join(., initialMass, by = "labID") %>%
  add_column(., litWHC.moistureFraction = LitterWHC[1, "moistureFraction"]) %>%
  mutate(
    
    actualSoilFreshMass = tube.fresh.soil.mass - tube.mass,
    actualLitFreshMass = tube.fresh.soil.and.litter.mass - tube.fresh.soil.mass, # since litter is oven dry - fresh and dry mass are equivalent
    
    actualSoilDryMass = actualSoilFreshMass*(1-soilGWC.moistureFraction),
    
    # edits to dry mass data !!!!
    actualLitFreshMass = replace(actualLitFreshMass, labID == 155, as.numeric(newDrymass155[,"newLitDryMass"])),
    actualSoilDryMass = replace(actualSoilDryMass, labID == 155, as.numeric(newDrymass155[,"newSoilDryMass"])),
    
    totalDryMass = actualLitFreshMass + actualSoilFreshMass,
    totalFreshMass = actualSoilFreshMass + actualLitFreshMass,
    
    moistureSoilPercentatTarget =  soilWHC.moistureFraction*(moistTRT/100),
    targetSoilFreshMass = actualSoilDryMass*(1/(1-(moistureSoilPercentatTarget))),
 
    moistureLitterPercentatTarget =  litWHC.moistureFraction*(moistTRT/100),
    targetLitterFreshMass = actualLitFreshMass*(1/(1-(moistureLitterPercentatTarget))),
    
    targetFreshMass = targetSoilFreshMass + targetLitterFreshMass,

    targetTotalMass = targetFreshMass + tube.mass,
    
  )  %>% select(c(labID, tube.mass, actualSoilDryMass, actualLitFreshMass, targetFreshMass, targetTotalMass
                  )) %>%
  write.csv("nsfms_set_up_actual_mass_20200312.csv")

# this should be updated each time - last updated 3/8/2020
# each updated time, when a loss of mass happens should be time stamped AS A NEW CSV
# to be able to make accurate calculations at each time point




