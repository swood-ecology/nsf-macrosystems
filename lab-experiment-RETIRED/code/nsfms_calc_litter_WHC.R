# Last updated March 5, 2020 by AP adapted from Stephen Wood's original code
# Cleaned up variable names

# Calculates Water holding capacity for soil

library(tidyverse)

# Read in raw data


path <- "C:/Users/alexa/Dropbox (Yale_FES)/Macrosystems Biol Bradford Wieder Wood 2019-2024/Microcosm experiments/"
setwd(path)

litter_WHC <- read_csv("raw-data/nsfms_litter_WHC_raw.csv")

# Set directory to calculated data folder
setwd("calculated-data")

# # Calculate gravimetric moisture and export
# litter_WHC %>%
#   mutate(
#     freshlitter = tin.fresh.litter - tin.mass,
#     ovenDriedlitter = tin.oven.dried.litter - tin.mass,
#     moistureMass = freshlitter - ovenDriedlitter, # Calculated in grams
#     moistureFraction = moistureMass / freshlitter, # No units, fraction
#     moisturePercent = moistureFraction * 100
#   ) %>%
#   select(-tin.mass:-ovenDriedlitter) %>%
#   write.csv("nsfms_litter_WHC.csv")


aggregate(. ~ substrate,
          data =
            litter_WHC %>%
            mutate(
              freshlitter = tin.fresh.litter - tin.mass,
              ovenDriedlitter = tin.oven.dried.litter - tin.mass,
              moistureMass = freshlitter - ovenDriedlitter, # Calculated in grams
              moistureMasspergDryLitter = moistureMass / ovenDriedlitter, # g H2O g soil-1 at 100% WHC
              moistureFraction = moistureMass / freshlitter, # No units, fraction
              moisturePercent = moistureFraction * 100
            ) %>%
            select(-replicate:-ovenDriedlitter),
          FUN = mean
) %>%
  write.csv("nsfms_litter_WHC_calc_aggregate.csv")
