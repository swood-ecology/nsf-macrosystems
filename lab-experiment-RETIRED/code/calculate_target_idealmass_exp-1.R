# Set-up ideal targets with edited calculations

# last edit 3/19/2020 by AP 

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



##### Idealized target weights with 0.25 g dry mass soil and 1.0 g dry mass litter ####

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

setwd("C:/Users/alexa/Dropbox (Yale_FES)/Macrosystems Biol Bradford Wieder Wood 2019-2024/")
