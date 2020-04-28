# Visualization of c mineralization data
# time 1 
# A Polussa 

# Libraries
library(ggplot2)
library(lme4)
library(standardize)
library(export)
library(tidyverse)


path <- "C:/Users/alexa/Dropbox (Yale_FES)/Macrosystems Biol Bradford Wieder Wood 2019-2024/Microcosm experiments/"
setwd(path)

cminT1 <- read_csv("calculated-data/nsfms_Cmin_t1_aggregate_calc.csv")
siteData <- read_csv("metadata/nsfms_site_data.csv")
soilGWC <- read_csv("calculated-data/nsfms_soil_GWC_calc.csv")

cminT1$moistTRT <- factor(cminT1$moistTRT, levels = c("35", "60", "100"))

left_join(cminT1, siteData, by = "plotID") %>%
  select(-c(Lat, Long)) %>%
  left_join(., soilGWC, by = "plotID") %>%
  select(-c("moistureMass", "moistureFraction", "X1.y", "X1.x")) -> microcosmData

# Cmin by Quadrat Moisture
ggplot(microcosmData, aes(x = moisturePercent, y = CO2CpergLitter, color = moistTRT)) + geom_point(aes(shape = Species))

graph2ppt(file="Day 3 Microcosm CO2 flux by Quadrat soil Moisture.pptx", width=7, height=5) 

# microcosm effects plots

ggplot(cminT1, aes(x = moistTRT, y = CO2CpergLitter, fill = moistTRT)) + 
  geom_boxplot(outlier.alpha = 0) + geom_point(position =position_jitterdodge(), alpha = .2)
graph2ppt(file="Day 3 Microcosm CO2 flux by Moisture.pptx", width=7, height=5) 




#
cminT2 <- read_csv("calculated-data/nsfms_Cmin_t2_aggregate_calc.csv")
siteData <- read_csv("metadata/nsfms_site_data.csv")
soilGWC <- read_csv("calculated-data/nsfms_soil_GWC_calc.csv")

cminT2$moistTRT <- factor(cminT2$moistTRT, levels = c("35", "60", "100"))

left_join(cminT2, siteData, by = "plotID") %>%
  select(-c(Lat, Long)) %>%
  left_join(., soilGWC, by = "plotID") %>%
  select(-c("moistureMass", "moistureFraction", "X1.y", "X1.x")) -> microcosmData

# Cmin by Quadrat Moisture
ggplot(microcosmData, aes(x = moisturePercent, y = CO2CpergLitter, color = moistTRT)) + geom_point(aes(shape = Species))

graph2ppt(file="Day 7 Microcosm CO2 flux by Quadrat soil Moisture.pptx", width=7, height=5) 

# microcosm effects plots

ggplot(cminT2, aes(x = moistTRT, y = CO2CpergLitter, fill = moistTRT)) + 
  geom_boxplot(outlier.alpha = 0) + geom_point(position =position_jitterdodge(), alpha = .2)
graph2ppt(file="Day 7 Microcosm CO2 flux by Moisture.pptx", width=7, height=5) 


#
cminT3 <- read_csv("calculated-data/nsfms_Cmin_t3_aggregate_calc.csv")
siteData <- read_csv("metadata/nsfms_site_data.csv")
soilGWC <- read_csv("calculated-data/nsfms_soil_GWC_calc.csv")

cminT3$moistTRT <- factor(cminT3$moistTRT, levels = c("35", "60", "100"))

left_join(cminT3, siteData, by = "plotID") %>%
  select(-c(Lat, Long)) %>%
  left_join(., soilGWC, by = "plotID") %>%
  select(-c("moistureMass", "moistureFraction", "X1.y", "X1.x")) -> microcosmData

# Cmin by Quadrat Moisture
ggplot(microcosmData, aes(x = moisturePercent, y = CO2CpergLitter, color = moistTRT)) + geom_point(aes(shape = Species))

graph2ppt(file="Day 10 Microcosm CO2 flux by Quadrat soil Moisture.pptx", width=7, height=5) 

# microcosm effects plots

ggplot(cminT3, aes(x = moistTRT, y = CO2CpergLitter, fill = moistTRT)) + 
  geom_boxplot(outlier.alpha = 0) + geom_point(position =position_jitterdodge(), alpha = .2)
graph2ppt(file="Day 10 Microcosm CO2 flux by Moisture.pptx", width=7, height=5) 























# ANALYSIS of microcosm
# Does response variable need to be normally distributed
# risidually randomlly distributed




#Convert to numeric for ANOVA
# utemp.test$uMoisture <- as.numeric(as.character(utemp.test$uMoisture))
# utemp.test$uTemperature <- as.numeric(as.character(utemp.test$uTemperature))


#
utemp.model <- lm(uCmin ~ uTemperature*uMoisture*Moisture, data = utemp.test)

summary(utemp.model)
anova(utemp.model)

#
utemp.model1 <- lm(uCmin ~ uMoisture + uTemperature + Temperature + Moisture, data = utemp.test)


summary(utemp.model1)
anova(utemp.model1)
#
utemp.model2 <- lm(uCmin ~ uMoisture*uTemperature, data = utemp.test)


summary(utemp.model2)


#
utemp.model <- lmer(uCmin ~ uMoisture*uTemperature + (1|Temperature:Moisture), data = utemp.test)

summary(utemp.model)
anova(utemp.model)





## Test simulations

utemp.test <- sim.microcosm.data(overall_intercept = 50, 
                                 uLow_intercept = 0, uLow_coeff = .005, # cold microcosm parameters
                                 uhigh_intercept = 6, uhigh_coeff = .5, # warm microcosm parameters
                                 uMoistdry_intercept = 0, uMoistdry_coeff = 0, # dry microcosm parameters
                                 uMoistopt_intercept = 4, uMoistopt_coeff = 0,# optimal microcosm parameters
                                 uMoistwet_intercept = 2, uMoistwet_coeff = 0,# wet microcosm parameters
                                 uLitNinit = 0.9, rep = 1, sigma.residual = 1)

utemp.test


# Cmin by Quadrat Temperature
ggplot(utemp.test, aes(x = Temperature, y = uCmin, color = uMoisture, linetype = uTemperature)) + 
  geom_point(aes(shape = uTemperature)) + geom_smooth(method='lm', formula= y~x)

# Cmin by Quadrat Moisture
ggplot(utemp.test, aes(x = Moisture, y = uCmin, color = uMoisture, linetype = uTemperature)) + 
  geom_point(aes(shape = uTemperature))+ geom_smooth(method='lm', formula= y~x)

ggplot(utemp.test, aes(x = uMoisture, y = uCmin, fill = uTemperature)) + 
  geom_boxplot() + geom_point(position =position_jitterdodge())

ggplot(utemp.test, aes(x = uTemperature, y = uCmin, fill = uMoisture)) + 
  geom_boxplot() + geom_point(position =position_jitterdodge())

# modeling
#Convert to numeric for ANOVA
utemp.test$uMoisture <- as.numeric(as.character(utemp.test$uMoisture))
utemp.test$uTemperature <- as.numeric(as.character(utemp.test$uTemperature))


utemp.model <- lm(uCmin ~ uMoisture*uTemperature*Temperature*Moisture, data = utemp.test)


summary(utemp.model)
anova(utemp.model)

# Deteleted February 6, 2020
# made new document "field_data_exploration.R" that includes this and creates a dataframe called masterdata


# #  HARV
# HARV_data <- read.csv("/Users/alexa/Documents/Bradford Lab/Microcosm Work/Field Data/HARV_SecondVisit_Data.csv", header = T)
# 
# #  SCBI
# SCBI_data <- read.csv("/Users/alexa/Documents/Bradford Lab/Microcosm Work/Field Data/SCBI_SecondVisit_Data.csv", header = T)
# 
# # Calculate means 
# HARV_data$meanSoilT<-((HARV_data$SoilT1 +HARV_data$SoilT2 +HARV_data$SoilT3 )/3) 
# HARV_data$meanLitT <-((HARV_data$LitT1 +HARV_data$LitT2 +HARV_data$LitT3 )/3)
# HARV_data$meanSoilM <-((HARV_data$SoilM1  +HARV_data$SoilM2  +HARV_data$SoilM3 )/3)
# 
# SCBI_data$meanSoilT<-((SCBI_data$SoilT1 +SCBI_data$SoilT2 +SCBI_data$SoilT3 )/3) 
# SCBI_data$meanLitT <-((SCBI_data$LitT1 +SCBI_data$LitT2 +SCBI_data$LitT3 )/3)
# SCBI_data$meanSoilM <-((SCBI_data$SoilM1  +SCBI_data$SoilM2  +SCBI_data$SoilM3 )/3)
# 
# # Combine into one dataframe
# Sites <- c(rep(c("HARV", "SCBI"), each = 48), "SCBI") # 49 quadrats in SCBI
# 
# Field_data_1 <- rbind(HARV_data, SCBI_data)
# Field_data <- cbind(Sites, Field_data_1)
# 
# # Visualize HARV and SCBI Data for soil moisture, temperature, and litter temperature
# 
# # Litter Temp by species and Site
# ggplot(Field_data, aes(x = Species, y = meanLitT, fill = Species)) + geom_boxplot() + 
#   geom_point(position = position_jitterdodge(), aes(x = Species)) + facet_grid(.~Sites)
# graph2ppt(file="Litter Temp by species and Site.pptx", width=6, height=5) 
# # Soil Temp by species and Site
# ggplot(Field_data, aes(x = Species, y = meanSoilT, fill = Species)) +  geom_boxplot() + 
#   geom_point(position = position_jitterdodge(), aes(x = Species)) + facet_grid(.~Sites)
# # graph2ppt(file="Soil Temp by species and Site.pptx", width=6, height=5) 
# # Soil Moisture by species and Site
# ggplot(Field_data, aes(x = Species, y = meanSoilM, fill = Species)) +  geom_boxplot() + 
#   geom_point(position = position_jitterdodge(), aes(x = Species)) +facet_grid(.~Sites)
# # graph2ppt(file="Soil Moisture by species and Site.pptx", width=6, height=5) 
# # Soil GWC by species and Site
# ggplot(Field_data, aes(x = Species, y = SoilGWC, fill = Species)) +  geom_boxplot() + 
#   geom_point(position = position_jitterdodge(), aes(x = Species)) +facet_grid(.~Sites)
# # graph2ppt(file="Soil GWC by species and Site.pptx", width=6, height=5) 
# 
# ggplot(Field_data, aes(x = meanSoilM, y = SoilGWC, color = Species)) + geom_point(size = 3) +  geom_smooth(method='lm', formula= y~x) +
#   facet_grid(.~Sites)
# 
# 
# 
# 
# plot(SoilGWC ~ meanSoilM, data = SCBI_data)
# plot(SoilGWC ~ meanSoilM, data = HARV_data)


