# extra code to make data file of initial masses with all data included for actual data


setwd("C:/Users/alexa/Dropbox (Yale_FES)/Macrosystems Biol Bradford Wieder Wood 2019-2024/")

source("code/calculations/calculate_target_idealmass_exp-1.R")

initialMass <- read_csv("raw-data/lab-experiment/experiment-1/initialMass-exp-1_Spring-2020.csv")


##### These are for changes in raw data initial masses ####

# edit for initial mass soil GWC, as soils were dried seperately on tins and recalculated their WHC

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
                  targetSoilFreshMass, targetLitterFreshMass, litWHC.moistureFraction, moistureLitterPercentatTarget)) %>%
  write.csv("actualMass_exp-1_Spring-2020_initial.csv")


setwd("C:/Users/alexa/Dropbox (Yale_FES)/Macrosystems Biol Bradford Wieder Wood 2019-2024/")

