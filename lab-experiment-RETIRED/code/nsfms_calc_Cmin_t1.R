# time point resolved c-mineralization of microcosms
# adapted from Stephen Wood's sir calcs code
# LAST WORKED ON 3/9/2020 FOR CMIN T1 BY AP



library(tidyverse)

# Read in raw data
path <- "C:/Users/alexa/Dropbox (Yale_FES)/Macrosystems Biol Bradford Wieder Wood 2019-2024/lab-experiment/"
setwd(path)


cminT1 <- read_csv("raw-data/nsfms_Cmin_t1.csv")
cminT1$irga.integral <- as.numeric(cminT1$irga.integral)

microcosmDesign <- read_csv("metadata/microcosm_exp_design.csv")
microcosmMasses <- read_csv("calculated-data/nsfms_set_up_actual_mass_initial.csv")


setwd("calculated-data")

# Calculate standard values for all places with standards
not_any_na <- function(x) all(!is.na(x))

cminT1[is.na(cminT1$std.value.1)==FALSE,] -> stds
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
stds <- stds %>% select(cminID, meanStandard)
  
# Assign standard values at all places in between
right_join(stds,cminT1) %>% fill(meanStandard, .direction = "up") -> cminT1


     
# 1. calculate co2 flux for all microcosms
cminT1 %>%
  mutate(
    incubationTime = as.numeric(strptime(time.sampled, "%m/%d/%Y %H:%M") - strptime(time.flushed, "%m/%d/%Y %H:%M")), # Hours
    dilutionFactor = ((5*times.sampled)/(57.15-soil.volume))+1,    
    measuredCO2 = irga.integral*(standard.co2/meanStandard),             # ppm
    concentrationCO2 = measuredCO2*dilutionFactor,                       # ppm
    volumeCO2 = concentrationCO2*((57.15-soil.volume)/1000),             # L
    molesCO2 = (volumeCO2/22.414)*273.15/293.15,                         # mol
    CO2C = molesCO2*12.011,                                              # g
    CO2CperHour = CO2C/incubationTime                                    # g h-1
  ) %>% select(c(labID, plotID, soil.volume, CO2CperHour)) -> cminT1_calc 

write.csv(cminT1_calc, "cminT1_raw_calc.csv")

# 2. calculate co2 flux for standards
left_join(microcosmDesign, cminT1_calc, by = "labID") %>% 
  left_join(., microcosmMasses, by = "labID") %>%
  filter(., microcosmExp == "b") %>% # calc standards
  mutate(.,
         standardCO2CperHourpergSoil = CO2CperHour / actualSoilDryMass #g CO2C hr-1 g dry soil-1
         ) %>% 
  select(plotID.x, microcosmExp, microcosmNum, Rep, labID, 
         moistTRT, actualSoilDryMass, standardCO2CperHourpergSoil) -> standards
  
# 2. correct co2 flux for amount of soil in experimental microcosms
#     then standardize by mass of litter

left_join(microcosmDesign, cminT1_calc, by = "labID") %>% 
  left_join(., microcosmMasses, by = "labID") %>%
  filter(., microcosmExp == "a") %>%
  left_join(., standards, by = c("plotID.x", "moistTRT")) %>%
  mutate(
    contributionCO2CfromSoil = standardCO2CperHourpergSoil*actualSoilDryMass.x, # gCO2 gSoil-1 h-1
    CO2CfromLitterCorrectedforSoil = CO2CperHour - contributionCO2CfromSoil, # gCO2 h-1
    CO2CpergLitter = CO2CfromLitterCorrectedforSoil / actualLitFreshMass # gCO2 gLitter-1 h-1
  ) %>% rename(plotID = plotID.x, microcosmNum = microcosmNum.x, Rep = Rep.x, labID = labID.x) %>%
  select(plotID, microcosmNum, Rep, labID, moistTRT, CO2CpergLitter) -> cminT1_corrected_calc  
  write.csv(cminT1_corrected_calc, "nsfms_Cmin_t1_calc.csv") 


# 3.  now make aggregated date for simplicity pre-stephen wood magic with stats

aggregate(. ~ plotID + moistTRT,
          data =
            cminT1_corrected_calc %>%
            select(-c("microcosmNum", "Rep", "labID")),
          FUN = mean
) -> cminT1_aggregate_calc
write.csv(cminT1_aggregate_calc, "nsfms_Cmin_t1_aggregate_calc.csv")






