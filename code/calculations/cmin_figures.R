# some figure code 

library(plotrix)
library(ggplot2)
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

aggregateData %>% 
  group_by(date, moist.trt) %>%
  summarise(meanCO2 = mean(CO2CpergLitter),
            std.err = std.error(CO2CpergLitter)) -> plotData


p <- ggplot(plotData, aes(x = date, y = meanCO2))

p + geom_line(aes(color = moist.trt), size = .2) + 
  geom_errorbar(aes(ymin=meanCO2-std.err, ymax=meanCO2+std.err), width=.1)








