# Last updated March 5, 2020 by AP adapted from Stephen Wood's original code
# Cleaned up variable names

# Calculates Water holding capacity for soil


library(tidyverse)


# Read in raw data

path <- "C:/Users/alexa/Dropbox (Yale_FES)/Macrosystems Biol Bradford Wieder Wood 2019-2024/Microcosm experiments/"
setwd(path)


soil_WHC <- read_csv("raw-data/nsfms_soil_WHC_raw.csv")

# Set directory to calculated data folder
setwd("calculated-data")

# Calculate gravimetric moisture and export
soil_WHC %>%
  mutate(
    freshSoil = tin.fresh.soil - tin.mass,
    ovenDriedSoil = tin.oven.dried.soil - tin.mass,
    moistureMass = freshSoil - ovenDriedSoil, # Calculated in grams
    moistureFraction = moistureMass / freshSoil, # No units, fraction
    moisturePercent = moistureFraction * 100
  ) %>%
  select(-tin.mass:-ovenDriedSoil) %>%
  write.csv("nsfms_soil_WHC_calc.csv")
