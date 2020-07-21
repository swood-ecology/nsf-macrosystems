# combining aggregate carbon mineralization data
# last worked on March 26, 2020 by AP

library(tidyverse)
library(DescTools)

# non-aggregate c-min data
# Read in cmin data
setwd("C:/Users/alexa/Dropbox (Yale_FES)/Macrosystems Biol Bradford Wieder Wood 2019-2024/calculated-data/lab-experiment/experiment-1/")
cmin_cum <- read_csv("cmin_calc_exp-1.csv")

# data with t0, with respiration beginning at 0 (t0 get data for 0 -> day 1)
bind_rows(
  cmin_cum %>% 
    filter(day == 1) %>%
    mutate(
      day=replace(day, day == 1, 0),
      date= replace(date, day == 0, "2020-02-25"),
      CO2CpergLitter=replace(CO2CpergLitter, day == 0, 0)
    ),
  cmin_cum) %>% drop_na()-> cumulativeData # remove points with NA values


# Calculate integral for each microcosm @ specific moisture treatment

cumulativeData %>%
  group_by(unique.id, moist.trt, replicate) %>%
  summarize(cumulativeCO2Flux = AUC(day*24, CO2CpergLitter)) %>%
  mutate(cumulativeCO2Flux = cumulativeCO2Flux/1000) -> cumulativeDataCalc # ug to mg CO2-C

write_csv(cumulativeDataCalc, "cumulative_cmin_calc_exp-1.csv")


# aggregate c-min data 

# Read in cmin data
setwd("C:/Users/alexa/Dropbox (Yale_FES)/Macrosystems Biol Bradford Wieder Wood 2019-2024/calculated-data/lab-experiment/experiment-1/")
aggregate_cmin_cum <- read_csv("cmin_calc_aggregate_exp-1.csv")

# data with t0, with respiration beginning at 0 (t0 get data for 0 -> day 2)
bind_rows(
  aggregate_cmin_cum %>% 
    filter(day == 2) %>%
    mutate(
      day=replace(day, day == 2, 0),
      date= replace(date, day == 0, "2020-02-25"),
      CO2CpergLitter=replace(CO2CpergLitter, day == 0, 0)
    ),
  cmin_cum) %>% drop_na() -> aggregate_cumulativeData


# Calculate integral for each microcosm @ specific moisture treatment

aggregate_cumulativeData %>%
  group_by(unique.id, moist.trt) %>%
  summarize(cumulativeCO2Flux = AUC(day*24, CO2CpergLitter)) %>%
  mutate(cumulativeCO2Flux = cumulativeCO2Flux/1000) -> aggregate_cumulativeDataCalc # ug to mg CO2-C

write_csv(aggregate_cumulativeDataCalc, "cumulative_aggregate_cmin_calc_exp-1.csv")

