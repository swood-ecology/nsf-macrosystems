library(tidyverse)

# Read in raw data
setwd("C:/Users/alexa/Dropbox (Yale_FES)/Macrosystems Biol Bradford Wieder Wood 2019-2024/")
sir <- read_csv("raw-data/field-experiment/prelim/soilSIR_volume_SCBI_prelim-1_Fall-2019.csv")

# Set directory to calculated data folder
setwd("calculated-data/field-experiment/prelim/")

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

as.numeric(substr(sir$time.flushed, start= 11, stop = 15))
strptime(sir$time.flushed, "%m/%d/%Y %H:%M")

# Calculate gravimetric moisture and export both non-aggregated and aggregated replicate data
sir %>%
  mutate(
    incubationTime = as.numeric((strptime(time.sampled, "%m/%d/%Y %H:%M") - strptime(time.flushed, "%m/%d/%Y %H:%M"))),     # Hours
    dilutionFactor = ((5*times.sampled)/(57.15-actual.fresh.mass))+1,    
    measuredCO2 = irga.integral*(standard.co2/meanStandard),             # ppm
    concentrationCO2 = measuredCO2*dilutionFactor,                       # ppm
    volumeCO2 = concentrationCO2*((57.15-actual.fresh.mass)/1000),       # uL
    molesCO2 = (volumeCO2/22.414)*273.15/293.15,                         # umol
    CO2C = molesCO2*12.011,                                              # ug
    CO2CperHour = CO2C/incubationTime                                    # ug h-1
  ) %>% select(-std.value.1:-std.value.4) %>%
  write.csv("scbiSIR_prelim-1_Fall 2019.csv")

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
