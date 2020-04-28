# Last updated March 16, 2020 by AP adapted from Stephen Wood's original code
# Cleaned up variable names

# Calculates Water holding capacity for soil

library(tidyverse)

# Read in raw data


setwd("C:/Users/alexa/Dropbox (Yale_FES)/Macrosystems Biol Bradford Wieder Wood 2019-2024/")


litter_WHC <- read_csv("raw-data/lab-experiment/experiment-1/litterWHC_exp-1_Spring-2020.csv")



# Set directory to calculated data folder
setwd("calculated-data/lab-experiment/experiment-1/")

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
  write.csv("litterWHC_exp-1_Spring-2020.csv")




# for non-aggregated data... 2 replicates

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
