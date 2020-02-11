library(tidyverse)

# Read in raw data
sir <- read_csv("~/Box Sync/Work/The Nature Conservancy/NSF Macrosystems/raw-data/substrate_induced_respiration.csv")

# Set directory to calculated data folder
setwd("~/Box Sync/Work/The Nature Conservancy/NSF Macrosystems/calculated-data")

# Calculate standard values for all places with standards
not_any_na <- function(x) all(!is.na(x))

sir[is.na(sir$std.value.1)==FALSE,] -> stds
stds %>% add_column(meanStandard='NA') -> stds

for(i in 1:nrow(stds)-1){
  bind_rows(
    stds[i,c("std.value.1","std.value.2","std.value.3","std.value.4")] %>%
              select_if(not_any_na) %>% gather(), 
            stds[(i+1),c("std.value.1","std.value.2","std.value.3","std.value.4")] %>%
              select_if(not_any_na) %>% gather()
    ) %>%
    summarize(mean = mean(value)) -> stds[(i+1),'meanStandard']
}
stds$meanStandard[1] <- stds$meanStandard[2]
stds$meanStandard <- as.numeric(stds$meanStandard)
stds <- stds %>% select(unique.id,meanStandard)
  
# Assign standard values at all places in between
right_join(stds,sir) %>% fill(meanStandard, .direction = "up") -> sir

# Calculate gravimetric moisture and export both non-aggregated and aggregated replicate data
sir %>%
  mutate(
    incubationTime = as.numeric((time.sampled - time.flushed)/3600),     # Hours
    dilutionFactor = ((5*times.sampled)/(57.15-actual.fresh.mass))+1,    
    measuredCO2 = irga.integral*(standard.co2/meanStandard),             # ppm
    concentrationCO2 = measuredCO2*dilutionFactor,                       # ppm
    volumeCO2 = concentrationCO2*((57.15-actual.fresh.mass)/1000),       # L
    molesCO2 = (volumeCO2/22.414)*273.15/293.15,                         # mol
    CO2C = molesCO2*12.011,                                              # g
    CO2CperHour = CO2C/incubationTime                                    # g h-1
  ) %>% select(-std.value.1:-std.value.4) %>%
  write.csv("sir_calcs.csv")

aggregate(CO2CperHour ~ unique.id,
          data =
            sir %>% mutate(
              incubationTime = as.numeric((time.sampled - time.flushed)/3600),
              dilutionFactor = ((5*times.sampled)/(57.15-actual.fresh.mass))+1,
              measuredCO2 = irga.integral*(standard.co2/meanStandard),
              concentrationCO2 = measuredCO2*dilutionFactor,
              volumeCO2 = concentrationCO2*((57.15-actual.fresh.mass)/1000),
              molesCO2 = (volumeCO2/22.414)*273.15/293.15,
              CO2C = molesCO2*12.011,
              CO2CperHour = CO2C/incubationTime 
            ) %>% select(-std.value.1:-std.value.4),
          FUN = mean
) %>%
  write.csv("sir_calcs_aggregated.csv")
