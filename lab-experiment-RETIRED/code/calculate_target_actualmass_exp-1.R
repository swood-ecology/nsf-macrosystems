# here are where all the adendums to any changes in microcosm masses happens
# the all data mass code is used to edit the edited microcosms

# change in 155 on 3/12/2020

setwd("C:/Users/alexa/Dropbox (Yale_FES)/Macrosystems Biol Bradford Wieder Wood 2019-2024/")

source("code/calculations/calculate_target_idealmass_exp-1.R")
source("code/calculations/calculate_target_alldata_mass_exp-1.R")




##### ACTUAL target weights based on weighed masses of tube, fresh soil, and dry litter #####

# To use real raw data of tube mass and measured soil and litter fresh mass
setwd("C:/Users/alexa/Dropbox (Yale_FES)/Macrosystems Biol Bradford Wieder Wood 2019-2024/")

initialMass <- read_csv("raw-data/lab-experiment/experiment-1/initialMass-exp-1_Spring-2020.csv")


##### These are for changes in raw data initial masses ####

# microcosm 155

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

massData <- read_csv("calculated-data/lab-experiment/experiment-1/actualMass_exp-1_Spring-2020_initial.csv")


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




