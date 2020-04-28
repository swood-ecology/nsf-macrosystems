# calculate how much actual % WHC compared to target WHC
# A Polussa - 3/11/2020

library(tidyverse)

path <- "C:/Users/alexa/Dropbox (Yale_FES)/Macrosystems Biol Bradford Wieder Wood 2019-2024/Microcosm experiments/"
setwd(path)

microcosmDesign <- read_csv("metadata/microcosm_exp_design.csv")
SoilGWC <- read_csv("calculated-data/nsfms_soil_GWC_calc.csv")
SoilWHC <- read_csv("calculated-data/nsfms_soil_WHC_calc.csv")
LitterWHC <- read_csv("calculated-data/nsfms_litter_WHC_calc_aggregate.csv")
initialMasses <- read_csv("calculated-data/nsfms_set_up_actual_mass_initial.csv")

wet_up_1 <- read_csv("raw-data/nsfms_wet_up_1.csv")
wet_up_2 <- read_csv("raw-data/nsfms_wet_up_2.csv")
wet_up_3 <- read_csv("raw-data/nsfms_wet_up_3.csv")

initialMasses %>%
  mutated(
    targetMoisture = targetTotalMass - tube.mass - actualSoilDryMass - actualLitFreshMass
  ) %>% select






