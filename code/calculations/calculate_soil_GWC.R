library(tidyverse)

# Read in raw data
setwd("C:/Users/alexa/Dropbox (Yale_FES)/Macrosystems Biol Bradford Wieder Wood 2019-2024/")

gravimetric_moisture <- read_csv("raw-data/field-experiment/prelim/soilGWC_prelim-1_Fall-2019.csv")

# Set directory to calculated data folder
setwd("calculated-data/field-experiment/prelim/")

# Calculate gravimetric moisture and export
gravimetric_moisture %>%
  mutate(
    freshSoil = tin.fresh.soil - tin.mass,
    ovenDriedSoil = tin.oven.dried.soil - tin.mass,
    moistureMass = freshSoil - ovenDriedSoil, # Calculated in grams
    moistureFraction = moistureMass / freshSoil, # No units, fraction
    moisturePercent = moistureFraction * 100
  ) %>%
  select(-tin.mass:-ovenDriedSoil) %>%
  write.csv("soilGWC_prelim-1_Fall-2019.csv")
