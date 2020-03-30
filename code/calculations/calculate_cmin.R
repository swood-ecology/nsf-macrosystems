
# caclulate co2 per hour per g litter
library(tidyverse)

source("code/calculations/fun_cmin_calc.R")

# for a function called cmin_calc_fun


setwd("raw-data/lab-experiment/experiment-1/")

IRGA.files <- list.files(pattern = "IRGA_exp-1*")
time.list <- vector(mode = "list", length = length(IRGA.files))

for(i in 1:length(IRGA.files)){
  cmin <- read_csv(paste("raw-data/lab-experiment/experiment-1/", IRGA.files[i], sep = ""))
  names(time.list)[i] <- substr(IRGA.files[i], start = 12, stop = 26)
  setwd("calculated-data/lab-experiment/experiment-1/")
  time.list[[i]] <- cmin_calc_fun(cmin, substr(IRGA.files[i], start = 12, stop = 26))
}


# import mass data


mass_initial <- read_csv("actualMass_exp-1_Spring-2020_initial.csv")
mass_20200312 <- read_csv("actualMass_exp-1_Spring-2020_20200312.csv") # day 16

setwd("C:/Users/alexa/Dropbox (Yale_FES)/Macrosystems Biol Bradford Wieder Wood 2019-2024/metadata/")
microcosmDesign <- read_csv("exp-1_design.csv")

mass_initial <- left_join(microcosmDesign, mass_initial, by = c("lab.id", "moist.trt")) %>% select(-c("X1.x", "X1.y", "tube.mass", "targetFreshMass", "targetTotalMass"))
mass_20200312 <- left_join(microcosmDesign, mass_20200312, by = "lab.id") %>% select(-c("X1.x", "X1.y", "tube.mass", "targetFreshMass", "targetTotalMass"))


mapply(cbind, time.list, "date" = names(time.list), SIMPLIFY = F) %>%
  bind_rows(.) %>% 
  separate(col = date, into = c("day", "date"), sep = "_") %>% 
  mutate (day = substr(day, start = 5, stop = 6)
  ) %>% transform(., day = as.numeric(day), 
                  date = as.Date(date, format = "%Y%m%d")) -> cmin




bind_rows(filter(cmin, day < 16)%>%
            left_join(., mass_initial,  by = c("lab.id", "unique.id")),
  filter(cmin, day > 16)%>%
    left_join(., mass_20200312,  by = c("lab.id", "unique.id"))
) -> cflux

# for naming and using in loop
days <- factor(cflux$day)
dates <- factor(cflux$date)


setwd("C:/Users/alexa/Dropbox (Yale_FES)/Macrosystems Biol Bradford Wieder Wood 2019-2024/calculated-data/lab-experiment/experiment-1/")



cflux %>% group_by(day) %>%
  # calculate co2 flux for standards
  filter(., microcosm.type == "b") %>% # calc standards
  mutate(.,
         standardCO2CperHourpergSoil = CO2CperHour / actualSoilDryMass #g CO2C hr-1 g dry soil-1
  ) %>% 
  select(unique.id, microcosm.type, lab.id, 
         moist.trt, actualSoilDryMass, standardCO2CperHourpergSoil) -> standards

# correct co2 flux for amount of soil in experimental microcosms
#     then standardize by mass of litter

cflux %>% group_by(day) %>%
  filter(., microcosm.type == "a") %>%
  left_join(., standards, by = c("unique.id", "moist.trt", "day")) %>%
  mutate(
    
    contributionCO2CfromSoil = standardCO2CperHourpergSoil*actualSoilDryMass.x, # gCO2 gSoil-1 h-1
    CO2CfromLitterCorrectedforSoil = CO2CperHour - contributionCO2CfromSoil, # gCO2 h-1
    CO2CpergLitter = CO2CfromLitterCorrectedforSoil / actualLitFreshMass # gCO2 gLitter-1 h-1
    
  ) %>% rename(microcosm.type = microcosm.type.x, lab.id = lab.id.x) %>%
  select(unique.id, lab.id, microcosm.id, replicate, day, date,  moist.trt, CO2CpergLitter) -> cmin_corrected_calc


write.csv(cmin_corrected_calc, paste("cmin_calc_exp-1.csv", sep = "")) 

# now make aggregated date for simplicity pre-stephen wood magic with stats

aggregate(. ~ unique.id + moist.trt + day + date,
          data =
            cmin_corrected_calc %>%
            select(-c("replicate", "lab.id")),
          FUN = mean
) -> cmin_aggregate_calc
write.csv(cmin_aggregate_calc, "cmin_calc_aggregate_exp-1.csv")

# cumulative aggregated csv

# non cumulative aggregated csv
