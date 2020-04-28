library(tidyverse)

# Read in raw data
path <- "C:/Users/alexa/Dropbox (Yale_FES)/Macrosystems Biol Bradford Wieder Wood 2019-2024/Microcosm experiments/"
setwd(path)

gravimetric_moisture <- read_csv("raw-data/nsfms_soil_GWC_raw.csv")

# Set directory to calculated data folder
setwd("calculated-data")

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
  write.csv("nsfms_soil_GWC_calc.csv")
