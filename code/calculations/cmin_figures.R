# some figure code 

library(plotrix)
library(ggplot2)
library(tidyverse)
library(export)
# Input some site data for plotting
setwd("C:/Users/alexa/Dropbox (Yale_FES)/Macrosystems Biol Bradford Wieder Wood 2019-2024/")


soilGWC <- read_csv("calculated-data/field-experiment/prelim/soilGWC_prelim-1_Fall-2019.csv")
siteData <- read_csv("metadata/sample_IDs.csv")
cumulativeDataCalc <- read_csv("calculated-data/lab-experiment/experiment-1/cumulative_cmin_calc_exp-1.csv")
aggregateData <-read_csv("calculated-data/lab-experiment/experiment-1/cmin_calc_aggregate_exp-1.csv")





# Create factors for ploting
cumulativeDataCalc$moist.trt <- factor(cumulativeDataCalc$moist.trt, levels = c("35", "60", "100"))
left_join(cumulativeDataCalc, soilGWC, by = "unique.id") %>%
  left_join(., siteData, by = "unique.id") -> cumulativeCO2SiteData

# cumulative CO2 flux by moisture treatment 

ggplot(cumulativeCO2SiteData, aes(x = moist.trt, y = cumulativeCO2Flux, fill = moist.trt)) + 
  geom_boxplot(outlier.alpha = 0) + geom_point(position =position_jitterdodge(), alpha = .2)
# graph2ppt(file="Cumulative CO2 flux by Moisture.pptx", width=7, height=5) 


ggplot(cumulativeCO2SiteData, aes(x = moist.trt, y = cumulativeCO2Flux, fill = moist.trt)) + 
  geom_boxplot(outlier.alpha = 0) + geom_point(position =position_jitterdodge(), alpha = .2) + facet_grid(.~species)




# cumulative CO2 flux by quadrat GWC
ggplot(cumulativeCO2SiteData, aes(x = moisturePercent, y = cumulativeCO2Flux, color = moist.trt)) + 
  geom_point() + geom_smooth(method='lm', formula= y~x) +facet_grid(species~.)


# cumulative CO2 flux by quadrat slope
ggplot(cumulativeCO2SiteData, aes(x = slope, y = cumulativeCO2Flux, color = moist.trt)) + 
  geom_point() + geom_smooth(method='lm', formula= y~x) +facet_grid(species~.)

# cumulative CO2 flux by quadrat slope
ggplot(cumulativeCO2SiteData, aes(x = aspect, y = cumulativeCO2Flux, color = moist.trt)) + 
  geom_point() + geom_smooth(method='lm', formula= y~x) +facet_grid(species~.)

p <- ggplot(cumulativeCO2SiteData, aes(x = moisturePercent, y = cumulativeCO2Flux, color = moist.trt))
p + geom_box

# cumulative CO2 flux by species of tree canopy

ggplot(cumulativeCO2SiteData, aes(x = species, y = cumulativeCO2Flux)) + 
  geom_boxplot(aes(color = moist.trt))




# cumulative CO2 flux by Site
ggplot(cumulativeCO2SiteData, aes(x = site, y = cumulativeCO2Flux, color = moist.trt)) + 
  geom_boxplot()



# time series data

aggregateData <-read_csv("calculated-data/lab-experiment/experiment-1/cmin_calc_aggregate_exp-1.csv")


# Create factors for ploting
aggregateData$moist.trt <- factor(aggregateData$moist.trt, levels = c("35", "60", "100"))
left_join(aggregateData, soilGWC, by = "unique.id") %>%
  left_join(., siteData, by = "unique.id") -> aggregateData

# add a new unit with all values of zero 
bind_rows(
  aggregateData %>% 
    filter(day == 2) %>%
    mutate(
      day=replace(day, day == 2, 0),
      date= replace(date, day == 0, "2020-02-25"),
      CO2CpergLitter=replace(CO2CpergLitter, day == 0, 0)
    ),
  aggregateData) -> aggregateData


aggregateData %>% 
  group_by(date, moist.trt) %>%
  add_tally() -> test

aggregateData %>% 
  group_by(date, moist.trt) %>%
  summarise(meanCO2 = mean(CO2CpergLitter),
            std.err = std.error(CO2CpergLitter),
            sd = sd(CO2CpergLitter)) -> plotData


p <- ggplot(plotData, aes(x = date, y = meanCO2))
p + geom_line(data = plotData, aes(x = date, y = meanCO2, color = moist.trt), size = .5, alpha = 1) + 
  geom_errorbar(data = plotData, aes(ymin=meanCO2-std.err, ymax=meanCO2+std.err),
                width=.5) + ylim(0, 100) + theme_classic()






j <- ggplot(aggregateData, aes(x = date, y = CO2CpergLitter, color = moist.trt))

ggplot(aggregateData, aes(x = date, y = CO2CpergLitter, color = moist.trt))  +
  geom_line(data = plotData, aes(x = date, y = meanCO2, color = moist.trt), size = .8, alpha = 1,
            position = position_dodge(width = 1)) + 
  geom_point(alpha = .6, position = position_dodge(width = 1), size = 1)  + 
  geom_point(data = plotData, aes(x = date, y = meanCO2, fill = moist.trt), size = 3, alpha = 1,
             shape = 0, position = position_dodge(width = 1)) +
  ylab("ug CO2-C g-1 litter hr-1") + 
  theme_classic()

# try to seperate time point data into species too 
aggregateData %>% 
  group_by(date, species, moist.trt) %>%
  summarise(meanCO2 = mean(CO2CpergLitter),
            std.err = std.error(CO2CpergLitter),
            sd = sd(CO2CpergLitter)) -> plotDataSpecies



ggplot(aggregateData, aes(x = date, y = CO2CpergLitter, color = moist.trt))  +
  geom_line(data = plotDataSpecies, aes(x = date, y = meanCO2, color = moist.trt), size = .8, alpha = 1,
            position = position_dodge(width = 1)) +
  geom_point(alpha = .6, position = position_dodge(width = 1), size = 1)  + 
 
  ylab("ug CO2-C g-1 litter hr-1") + 
  theme_classic() + facet_grid(.~species)


# geom_errorbar(data = plotData, aes(ymin=meanCO2-std.err, ymax=meanCO2+std.err),
#               width=.5, position = position_dodge(width = 1))

graph2ppt("timeseries.pptx", width = 7, height = 5)

?graph2ppt

  
p + geom_line(data = plotData, aes(x = date, y = meanCO2, color = moist.trt), size = .5, alpha = 1) + 
  geom_errorbar(data = plotData, aes(ymin=meanCO2-std.err, ymax=meanCO2+std.err),
                width=.5) + ylim(0, 100) + 
  ylab("ug CO2-C g-1 litter hr-1") + 
  theme_classic()







