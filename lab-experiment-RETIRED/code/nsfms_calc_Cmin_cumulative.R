# combining aggregate carbon mineralization data

library(tidyverse)
library(DescTools)
library(ggplot2)
library(export)

# Read in raw data
path <- "C:/Users/alexa/Dropbox (Yale_FES)/Macrosystems Biol Bradford Wieder Wood 2019-2024/lab-experiment/"
setwd(path)

source("code/nsfms_calc_Cmin_t1.R")
setwd(path)
source("code/nsfms_calc_Cmin_t2.R")
setwd(path)
source("code/nsfms_calc_Cmin_t3.R")
setwd(path)
source("code/nsfms_calc_Cmin_t4.R")



# creating tidy dataset of CO2 flux gLitter-1 h-1 at each time point

day <- c(0, 2, 7, 10, 14)
# data with t0, with respiration beginning at 0 (t0 get data for 0 -> day 2)

setwd(path)
cminT0_aggregate_calc <- read_csv("calculated-data/nsfms_Cmin_t0_initial.csv")

bind_rows(
cminT0_aggregate_calc %>% add_column(day = day[1]),
cminT1_aggregate_calc %>% add_column(day = day[2]),
cminT2_aggregate_calc %>% add_column(day = day[3]),
cminT3_aggregate_calc %>% add_column(day = day[4]),
cminT4_aggregate_calc %>% add_column(day = day[5])
) -> cumulativeData


for_line <- cumulativeData  %>%
  group_by(moistTRT, day)%>%
  summarize(mean(CO2CpergLitter), sd(CO2CpergLitter))

?summarize()
# Calculate integral for each microcosm @ specific moisture treatment
# uses Purr to group by plotID and moistTRT
# Calculates integral, and spits out dataframe with columns as plotID&moistTRT combinations

cumulativeDataCalc <- cumulativeData %>%
  group_by(plotID, moistTRT) %>%
  summarize(cumulativeCO2Flux = AUC(day, CO2CpergLitter))

# Input some site data for plotting
setwd(path)

soilGWC <- read_csv("calculated-data/nsfms_soil_GWC_calc.csv")
siteData <- read_csv("metadata/nsfms_site_data.csv")


# Create factors for ploting
cumulativeDataCalc$moistTRT <- factor(cumulativeDataCalc$moistTRT, levels = c("35", "60", "100"))
left_join(cumulativeDataCalc, soilGWC, by = "plotID") %>%
  left_join(., siteData, by = "plotID") -> cumulativeCO2SiteData

# cumulative CO2 flux by moisture treatment 

ggplot(cumulativeCO2SiteData, aes(x = moistTRT, y = cumulativeCO2Flux, fill = moistTRT)) + 
  geom_boxplot(outlier.alpha = 0) + geom_point(position =position_jitterdodge(), alpha = .2) + facet_grid(.~Species)

# graph2ppt(file="Cumulative CO2 flux by Moisture.pptx", width=7, height=5) 

# cumulative CO2 flux by quadrat GWC
ggplot(cumulativeCO2SiteData, aes(x = moisturePercent, y = cumulativeCO2Flux, color = moistTRT)) + 
  geom_point() + geom_smooth(method='lm', formula= y~x) +facet_grid(Species~.)


# cumulative CO2 flux by quadrat slope
ggplot(cumulativeCO2SiteData, aes(x = Slope, y = cumulativeCO2Flux, color = moistTRT)) + 
  geom_point() + geom_smooth(method='lm', formula= y~x) +facet_grid(Species~.)

# cumulative CO2 flux by quadrat slope
ggplot(cumulativeCO2SiteData, aes(x = Aspect, y = cumulativeCO2Flux, color = moistTRT)) + 
  geom_point() + geom_smooth(method='lm', formula= y~x) +facet_grid(Species~.)

p <- ggplot(cumulativeCO2SiteData, aes(x = moisturePercent, y = cumulativeCO2Flux, color = moistTRT))
p + geom_box

# cumulative CO2 flux by species of tree canopy

ggplot(cumulativeCO2SiteData, aes(x = Species, y = cumulativeCO2Flux)) + 
  geom_boxplot(aes(color = moistTRT))



# 
p <- ggplot(for_line, aes(x = day, y = `mean(CO2CpergLitter)`, group = moistTRT, color = factor(moistTRT)))

p + geom_line() + geom_errorbar(aes(ymin = (`mean(CO2CpergLitter)`-`sd(CO2CpergLitter)`), 
                                    ymax = (`mean(CO2CpergLitter)`+`sd(CO2CpergLitter)`)))



# cumulative CO2 flux by Site
ggplot(cumulativeCO2SiteData, aes(x = Site, y = cumulativeCO2Flux, color = moistTRT)) + 
  geom_boxplot()



str(cumulativeCO2SiteData)
model_1 <- lm(cumulativeCO2Flux ~ moistTRT *Site * Species * moistureFraction, data = cumulativeCO2SiteData)
summary(model_1)
anova(model_1)

cumulativeData %>%
  group_by(plotID, moistTRT) %>%
  summarize(cumulativeCO2C = AUC(day, CO2CpergLitter)) 

