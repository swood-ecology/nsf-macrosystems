library(tidyverse)

# Read in raw data
gravimetric_moisture <- read_csv("Box Sync/Work/The Nature Conservancy/NSF Macrosystems/raw-data/gravimetric_moisture.csv")

# Set directory to calculated data folder
setwd("~/Box Sync/Work/The Nature Conservancy/NSF Macrosystems/calculated-data")

# Calculate gravimetric moisture and export both non-aggregated and aggregated replicate data
gravimetric_moisture %>%
  mutate(
    freshSoil = tin_fresh_soil - tin_mass,
    ovenDriedSoil = tin_oven_dried_soil - tin_mass,
    moistureMass = freshSoil - ovenDriedSoil, # Calculated in grams
    moistureFraction = moistureMass / freshSoil, # No units, fraction
    moisturePercent = moistureFraction * 100
  ) %>%
  select(-tin_mass:-ovenDriedSoil) %>%
  write.csv("gravimetric_calcs.csv")

aggregate(. ~ site + plot,
  data =
    gravimetric_moisture %>%
      mutate(
        freshSoil = tin_fresh_soil - tin_mass,
        ovenDriedSoil = tin_oven_dried_soil - tin_mass,
        moistureMass = freshSoil - ovenDriedSoil, # Calculated in grams
        moistureFraction = moistureMass / freshSoil, # No units, fraction
        moisturePercent = moistureFraction * 100
      ) %>%
      select(-tin_mass:-ovenDriedSoil),
  FUN = mean
) %>%
  write.csv("gravimetric_calcs_aggregated.csv")
