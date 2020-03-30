# This chunk of code does 3 things:
# 1. calculates ideal target masses for microcosms based on 0.25 g soil, 1g litter and target WHC
# 2. calculates the actual target masses based on what was actually weighed out fresh weight-wise
# 3. creates a new csv for new target masses based on any losses that happened through the course of the experiement

library(tidyverse)

setwd("C:/Users/alexa/Dropbox (Yale_FES)/Macrosystems Biol Bradford Wieder Wood 2019-2024/")

microcosmDesign <- read.csv("metadata/exp-1_design.csv")
SoilGWC <- read.csv("calculated-data/field-experiment/prelim/soilGWC_prelim-1_Fall-2019.csv")
SoilWHC <- read.csv("calculated-data/field-experiment/prelim/soilWHC_prelim-1_Fall-2019.csv")
LitterWHC <- read.csv("calculated-data/lab-experiment/experiment-1/litterWHC_exp-1_Spring-2020.csv")

# Add prefixes to dataframe names
# Soil Gravimetric Soil Moisture
names(SoilGWC)[3:5] <- paste("soilGWC.", names(SoilGWC[,3:5]), sep = "")

# Soil water holding capacity
names(SoilWHC)[3:6] <- paste("soilWHC.", names(SoilWHC[,3:6]), sep = "")


##### 1. Idealized target weights with 0.25 g dry mass soil and 1.0 g dry mass litter ####
# calculate ideal targets with 0.25 g dry soil and 1 g litter for microcosms
# 6g soil for soil standard microcosms

# split into two components with the experimental with litter and soil matrix and soil standards
# these have been called experiment a and experimenet b in the microcosm design raw and code file

setwd("calculated-data/lab-experiment/experiment-1/")
bind_rows(
  filter(microcosmDesign, microcosm.type == "a") %>%
    left_join(., SoilGWC, by = "unique.id") %>%
    left_join(., SoilWHC, by = "unique.id") %>%
    add_column(., litWHC.moistureFraction = LitterWHC[1, "moistureFraction"]) %>%
    add_column(., soilDryMass = 0.25) %>%
    add_column(., litDryMass = 1) %>%
    mutate(
      soilFreshMass = soilDryMass/(1-soilGWC.moistureFraction),
      
      totalDryMass = litDryMass + soilDryMass,
      totalFreshMass = soilFreshMass + litDryMass,
      
      moistureSoilPercentatTarget =  soilWHC.moistureFraction*(moist.trt/100),
      targetSoilFreshMass = soilDryMass*(1/(1-(moistureSoilPercentatTarget))),
      
      moistureLitterPercentatTarget =  litWHC.moistureFraction*(moist.trt/100),
      targetLitterFreshMass = litDryMass*(1/(1-(moistureLitterPercentatTarget))),
      
      targetFreshMass = targetSoilFreshMass + targetLitterFreshMass
      
    ) ,
  filter(microcosmDesign, microcosm.type == "b") %>% # soil standards
    left_join(., SoilGWC, by = "unique.id") %>%
    left_join(., SoilWHC, by = "unique.id") %>%
    add_column(., litWHC.moistureFraction = LitterWHC[1, "moistureFraction"]) %>%
    add_column(., soilDryMass = 6) %>% # 6 grams dry mass soil
    add_column(., litDryMass = 0) %>% # no litter
    mutate(
      soilFreshMass = soilDryMass/(1-soilGWC.moistureFraction),
      
      totalDryMass = litDryMass + soilDryMass,
      totalFreshMass = soilFreshMass + litDryMass,
      
      moistureSoilPercentatTarget =  soilWHC.moistureFraction*(moist.trt/100),
      targetSoilFreshMass = soilDryMass*(1/(1-(moistureSoilPercentatTarget))),
      
      moistureLitterPercentatTarget =  litWHC.moistureFraction*(moist.trt/100),
      targetLitterFreshMass = litDryMass*(1/(1-(moistureLitterPercentatTarget))),
      
      targetFreshMass = targetSoilFreshMass + targetLitterFreshMass
      
    )
) %>%  select(c(lab.id, soilDryMass, soilFreshMass, litDryMass, totalFreshMass, targetFreshMass))  %>%
  write.csv("idealMassTarget_exp-1_Spring-2020.csv")





##### 2. calculate actual target mass with the weighed out dry weights for litter and soil #####

# import weighed masses
setwd("C:/Users/alexa/Dropbox (Yale_FES)/Macrosystems Biol Bradford Wieder Wood 2019-2024/")
initialMass <- read_csv("raw-data/lab-experiment/experiment-1/initialMass-exp-1_Spring-2020.csv")

# edit for samples 388, 289, and 390 initial mass soil GWC, as soils were dried seperately on tins and recalculated their WHC

editGWC <- left_join(microcosmDesign, SoilGWC, by = "unique.id")
editGWC[editGWC$lab.id == 388 | editGWC$lab.id == 389 | editGWC$lab.id == 390,"soilGWC.moistureFraction"] <- c(0.657423974,   0.614302885,  0.571689399)


setwd("calculated-data/lab-experiment/experiment-1/")

left_join(editGWC, SoilWHC, by = "unique.id") %>%
  left_join(., initialMass, by = "lab.id") %>%
  add_column(., litWHC.moistureFraction = LitterWHC[1, "moistureFraction"]) %>%
  mutate(
    
    actualSoilFreshMass = tube.fresh.soil.mass - tube.mass,
    actualLitFreshMass = tube.fresh.soil.and.litter.mass - tube.fresh.soil.mass, # since litter is oven dry - fresh and dry mass are equivalent
    
    actualSoilDryMass = actualSoilFreshMass*(1-soilGWC.moistureFraction),
    
    totalDryMass = actualLitFreshMass + actualSoilFreshMass,
    totalFreshMass = actualSoilFreshMass + actualLitFreshMass,
    
    moistureSoilPercentatTarget =  soilWHC.moistureFraction*(moist.trt/100),
    targetSoilFreshMass = actualSoilDryMass*(1/(1-(moistureSoilPercentatTarget))),
    
    moistureLitterPercentatTarget =  litWHC.moistureFraction*(moist.trt/100),
    targetLitterFreshMass = actualLitFreshMass*(1/(1-(moistureLitterPercentatTarget))),
    
    targetFreshMass = targetSoilFreshMass + targetLitterFreshMass,
    
    targetTotalMass = targetFreshMass + tube.mass,
    
  )  %>% select(c(lab.id, tube.mass, actualSoilDryMass, actualLitFreshMass, targetFreshMass,  
                  targetTotalMass, moist.trt, soilWHC.moistureFraction, moistureSoilPercentatTarget, 
                  targetSoilFreshMass, targetLitterFreshMass, litWHC.moistureFraction, moistureLitterPercentatTarget)) -> massData
write.csv(massData, "actualMass_exp-1_Spring-2020_initial.csv")



##### These are for changes in raw data initial masses ####

# microcosm 155
setwd("C:/Users/alexa/Dropbox (Yale_FES)/Macrosystems Biol Bradford Wieder Wood 2019-2024/")
wet_up_3 <- read_csv("raw-data/lab-experiment/experiment-1/wetup_exp-1_day-3_20200228.csv")
wet_up_4 <- read_csv("raw-data/lab-experiment/experiment-1/wetup_exp-1_day-10_20200306.csv")

# mass measurement on 3/6/2020; wetup 5
mass1_155 <- filter(wet_up_4, lab.id == 155) %>% select(total.mass)
# mass measurement on 3/12/2020 
mass2_155 <- 12.7226
daydiff <- 6 # difference in days between wetup 4 and wetup 5
changePerDay155 <- (mass2_155 - mass1_155)/daydiff # rate of mass loss per day

# 2. use that to estimate how much fresh weight was there 

mass3_155 <- filter(wet_up_3, lab.id == 155) %>% select(total.mass)
daydiff2 <- 5 # difference in days before wetup 3 and flushing accident
massInitial_155 <- mass3_155+daydiff2*changePerDay155



newDrymass155 <- filter(massData, lab.id == 155) %>% 
  mutate(    
    percentMoisture = 1-(actualSoilDryMass+actualLitFreshMass)/as.numeric(massInitial_155),
    freshMassLoss = as.numeric(massInitial_155) - 12.5361, # measured after mass loss took place
    moistureLoss = freshMassLoss*percentMoisture,
    dryMassLoss = freshMassLoss - moistureLoss,
    SoilDryMassLoss = dryMassLoss * actualSoilDryMass/(actualLitFreshMass+actualSoilDryMass),
    newSoilDryMass = actualSoilDryMass - SoilDryMassLoss,
    LitDryMassLoss = dryMassLoss * actualLitFreshMass/(actualLitFreshMass+actualSoilDryMass),
    newLitDryMass= actualLitFreshMass - LitDryMassLoss
  ) %>% select(lab.id, newSoilDryMass, newLitDryMass)

# within this, there is an edit to the actual dry soil and dry litter using the replace function, followed by the values caluclated above
# this was done 3/12/2020 for the wet_up_5 ++

setwd("calculated-data/lab-experiment/experiment-1/")

left_join(editGWC, SoilWHC, by = "unique.id") %>%
  left_join(., initialMass, by = "lab.id") %>%
  add_column(., litWHC.moistureFraction = LitterWHC[1, "moistureFraction"]) %>%
  mutate(
    
    actualSoilFreshMass = tube.fresh.soil.mass - tube.mass,
    actualLitFreshMass = tube.fresh.soil.and.litter.mass - tube.fresh.soil.mass, # since litter is oven dry - fresh and dry mass are equivalent
    
    actualSoilDryMass = actualSoilFreshMass*(1-soilGWC.moistureFraction),
    
    # edits to dry mass data !!!!
    actualLitFreshMass = replace(actualLitFreshMass, lab.id == 155, as.numeric(newDrymass155[,"newLitDryMass"])),
    actualSoilDryMass = replace(actualSoilDryMass, lab.id == 155, as.numeric(newDrymass155[,"newSoilDryMass"])),
    
    totalDryMass = actualLitFreshMass + actualSoilFreshMass,
    totalFreshMass = actualSoilFreshMass + actualLitFreshMass,
    
    moistureSoilPercentatTarget =  soilWHC.moistureFraction*(moist.trt/100),
    targetSoilFreshMass = actualSoilDryMass*(1/(1-(moistureSoilPercentatTarget))),
    
    moistureLitterPercentatTarget =  litWHC.moistureFraction*(moist.trt/100),
    targetLitterFreshMass = actualLitFreshMass*(1/(1-(moistureLitterPercentatTarget))),
    
    targetFreshMass = targetSoilFreshMass + targetLitterFreshMass,
    
    targetTotalMass = targetFreshMass + tube.mass,
    
  )  %>% select(c(lab.id, tube.mass, actualSoilDryMass, actualLitFreshMass, targetFreshMass, targetTotalMass
  )) %>%
  write.csv("actualMass_exp-1_Spring-2020_20200312.csv")

# this should be updated each time - last updated 3/19/2020
# each updated time, when a loss of mass happens should be time stamped AS A NEW CSV
# to be able to make accurate calculations at each time point



