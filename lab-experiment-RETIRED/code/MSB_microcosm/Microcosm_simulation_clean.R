# Data simulation 
# February 6, 2020
# A Polussa 

# Libraries
library(ggplot2)
library(lme4)
library(standardize)
library(export)


##### Data from field ####

masterdata
str(masterdata)

##### Simulate Field Data ##### 

# Doing 3 of 5 canopy types (Red Oak (SCBI and HARV), White Pine (HARV), Tulip Poplar(SCBI))
# Litter Carbon loss as a function of:
# Quadrat temperature, quadrat temperature^2, Initial litter Nitrogen, Quadrat moisture, Micorbial biomass (as measured by SIR), and available soil N: 
# Residual error from site, and at quadrat level. NOTE: how many levels of residual varaition should be input? Canopy?

  # CLitloss <- INTERCEPT + temperature_current*temp_effect +   I(temperature_current^2)*temp_effect_2 +
  # litterinitialN_current*litterN_effect +  moisture_current* moisture_effect + 
  # SIR_current*SIR_effect + Navail_current*soil_N_effect + Site_residual_varaition + Observation(quadrat)_residual


sim.field.data <- function(intercept = -17.1, litterN_effect = 19.3, temp_effect = 1.05, temp_effect_2 = -0.047,
                           moisture_effect = .141, soil_N_effect = 0.014, SIR_effect = 4.93, site.residual = 0.1, 
                           quadrat.residual = 0.2){
  
  
  ## Site
  site_names <- site.group.names <- c("HARV", "SCBI")
  
  ## Temperature by site
  ## Then will have a range within the site (which is about 3.5 from bradford 2017)
  ## Have soil probe temperature data but SCBI was taken in Jan, HARV in Dec... 
  
  temperature_effect <- c(8, 13)
  
  
  ## Litter moisture will have range of 12.8 - 81 from Bradford 2017 ranges from site 1 and 2
  ## LitMoist. percentage moisture (water mass divided by total fresh mass) of the harvested leaf litter.
  ## SIR will have range of 1.15 - 1.66 based on the 1st - 3rd quantile of data from bradford 2017 ( or 0.74 - 3.15 are max and min)
  ## SIR. ug C-CO2 g soil-1 h-1. 4 h yeast estimate of SIR microbial biomass
  ## N avail will have range of 7.8 to 28.6 based on 1st and 3rd quantil of data from bradford 2017 (or 0 to 124 are max and min)
  ## NH4init. mg/kg. Initial soil NH4 content at time zero (before lab incubation) - 2 July 2015. 10 g dw soil, 50 mL KCL
  ## NOxinit. mg/kg. Initial soil NO3 + NO2 extracted at time zero (before lab incubation) - 2 July 2015. 10 g dw soil, 50 mL KCL
  ## Navail is addition of NOx and NH4 
  
  ## Canopy Type
  # Made effect of tree association neutral for oak, negative for pine and positive for tulip polar
  canopy_names <- canopy.group.names <- c("Oak", "Pine", "Oak", "TulPop")
  
  mu <- intercept
  Site <- character()
  Canopy <- character()
  
  Quadrat <- character()
  Moisture <- numeric()
  Temperature <- numeric()
  SIR <- numeric()
  Navail <- numeric()
  Site_MeanTemp <- numeric()
  litterinitialN <- numeric()
  Unique_Quadrat <- character()
  LitCloss <- numeric()
  site_e <- numeric()
  e <- numeric()
  
  
  row.number <- 1
  
  for(a in 1:2){ # SITE -> TEMP
    site_current <- site_names[a]
    temperature_site_current <- temperature_effect[a]
    
    es <- site_current_residual_variation <- rnorm(1, 0, site.residual)
    
    for(b in 1:2){ # canopy association, if it is from HARV, it's either oak/Pine, if SCBI Oak/tulpop
      if(site_current == "HARV"){
        canopy_current <- canopy_names[b]}
      else{
        canopy_current <-canopy_names[b+2]}
      
      for(c in 1:16){ # MOIST, SIR, Navail from uniform distributions with real ranges
        # TEMP from uniform distribution shifted depending on SITE mean temp
        quadrat_current <- c
        moisture_current <- runif(1, 12.8, 81)
        temperature_current <- runif(1, temperature_site_current - 1.5, temperature_site_current + 1.5)
        SIR_current <- runif(1, 1.15, 1.66)
        Navail_current <- runif(1, 7.8, 28.6)
        
        
        # litterinitialN from NEON ranges for evergreen(Pine)/deciduous(Oak) chemical foliar data from HARV
        # # # Tulip Poplar arbitrarly shifted a bit higher than oak
        if(canopy_current == "Oak"){
          litterinitialN_current <- runif(1, 0.61, 1.1)
        } else if (canopy_current == "Pine"){
          litterinitialN_current <- runif(1, 0.31, 0.73)
        } else if (canopy_current == "TulPop"){
          litterinitialN_current <- runif(1, 1.0, 1.5)
        }
        
        quadrat_residual_variation <- rnorm(1, 0, quadrat.residual) ## Residual variation used here
        
        
        Site[row.number] <- site_current
        Site_MeanTemp[row.number] <- temperature_site_current
        Canopy[row.number] <- canopy_current
        litterinitialN[row.number] <- litterinitialN_current
        Moisture[row.number] <- moisture_current
        Temperature[row.number] <- temperature_current
        SIR[row.number] <- SIR_current
        Navail[row.number] <- Navail_current
        Quadrat[row.number] <- quadrat_current
        Unique_Quadrat[row.number] <- row.number
        site_e[row.number] <- site_current_residual_variation
        e[row.number] <- quadrat_residual_variation
        
        
        
        
        # Model to calcuate decomposition rates similar to bradford (2017) manuscript unstandardized MICROSITE MAIN EFFECTS
        LitCloss[row.number] <- mu + 
          temperature_current*temp_effect +
          I(temperature_current^2)*temp_effect_2 +
          litterinitialN_current*litterN_effect +
          moisture_current* moisture_effect +
          SIR_current*SIR_effect +
          Navail_current*soil_N_effect + site_current_residual_variation + quadrat_residual_variation
        
        
        row.number <- row.number + 1
        
      }
    }
  }
  
  # create data frame
  
  data.frame(Site = Site, 
             Canopy = Canopy,
             Unique_Quadrat = Unique_Quadrat,
             Quadrat = Quadrat,
             Site_MeanTemp = Site_MeanTemp,
             litterinitialN = litterinitialN,
             Moisture = Moisture,
             Temperature = Temperature,
             SIR = SIR,
             Navail = Navail, 
             site_e = site_e, 
             e = e,
             LitCloss = LitCloss
  )
  
  
  
}



test.data <- sim.field.data()
test.data

# Visualize simulated field data
# by Species
ggplot(test.data, aes(x = Canopy , y= LitCloss, fill = Canopy)) + geom_boxplot() + facet_grid(.~Site)
# by initial litter N
ggplot(test.data, aes(x = litterinitialN  , y= LitCloss, color = Canopy)) + geom_point() + facet_grid(.~Site)
# by Moisture
ggplot(test.data, aes(x = Moisture   , y= LitCloss, color = Canopy)) + geom_point() + facet_grid(.~Site)
# by Temperature
ggplot(test.data, aes(x = Temperature  , y= LitCloss, color = Canopy)) + geom_point(aes(shape = Site))
# by SIR
ggplot(test.data, aes(x = SIR  , y= LitCloss, color = Canopy)) + geom_point() + facet_grid(.~Site)


# Analayse with linear models

# test.data$log_LitCloss <- log(test.data$LitCloss)


test.data.model <- lm(LitCloss ~ Temperature + I(Temperature^2) + litterinitialN + Moisture + SIR + Navail, data = test.data) 
summary(test.data.model)


# # probably should not use site as random effect since there are only 2 and other variables capture ~everything~
# test.data.model1 <- lmer(LitCloss ~ Temperature + I(Temperature^2) + litterinitialN + Moisture + Navail + SIR + (1|Site), data = test.data) 
# summary(test.data.model1)
# 
# # correct to include site as a random variable? Only 2 sources, more like canopy, but that was coded as the source of initial N 
# test.data.model2 <- lmer(LitCloss ~ Temperature + I(Temperature^2) + litterinitialN + Moisture + Navail + SIR + (1|Canopy), data = test.data) 
# summary(test.data.model2)


# should make full interactions model, or is that overkill?



##### Microcosm Simulation #####

# Using only OAK CANOPY SOILS and OAK Litter 


# 3 moisture regimes: 25, 50, 100% WHC
# 2 temperature regimes: 12C, 22C

# Making dataset from hypothesis graphs for interaction with quadrat conditions (moisture and temperature)

# Have an intercept and slope for the mean effect of temperature and moisture microcosms

# assume there is no interaction between microcosm temp and moisture factors... Should test in simulation huh...

# Do not explicitly code for interactions, but could calculate them. 


# Overparameterized... 


# start simple alex, with crossed facotrs

# then add coefficents 


sim.microcosm.data <- function(overall_intercept = 50, 
                               uLow_intercept = -8, uLow_coeff = 0, # cold microcosm parameters
                               uhigh_intercept = 0, uhigh_coeff = 0, # warm microcosm parameters
                               uMoistdry_intercept = -5, uMoistdry_coeff = 0, # dry microcosm parameters
                               uMoistopt_intercept = 0, uMoistopt_coeff = 0,# optimal microcosm parameters
                               uMoistwet_intercept = -3, uMoistwet_coeff = 0,# wet microcosm parameters
                               
                               uLitNinit = 0.9, rep = 1, sigma.residual = 3){
  
  ## ucosm effect names
  
  uTemp_factor <- factor(c(12, 22), levels = c(12, 22))
  uMoist_factor <- factor(c(25, 50, 100), levels = c(25, 50, 100))
  
  
  # quadrat characteristics
  Site <- character()
  Canopy <- character()
  Unique_Quadrat <- numeric()
  Quadrat <- character()
  Site_MeanTemp <- numeric()
  litterinitialN <- numeric()
  Moisture <- numeric()
  Temperature <- numeric()
  SIR <- numeric()
  Navail <- numeric()
  LitCloss <- numeric()
  
  #made up variable to represent historical regime
  hist_current <- numeric()
  
  # microcosm characteristics
  Unique_Microcosm <- character()
  uLitN <- numeric()
  uMoisture <- character()
  uTemperature <- character()
  uCmin <- numeric()
  replicate <- numeric()
  
  
  LitCloss <- numeric()
  
  row.number <- 1
  
  #
  #
  # ONLY USING OAK DATA
  #
  #
  
  # oak.data <- test.data[test.data$Canopy == "Oak",]
  # to.run <- dim(oak.data)[1]
  
  oak.data <- test.data
  to.run <- dim(oak.data)[1]
  
  for(i in 1:to.run){
    
    # Pulling data from FIELD DATA SIMULATION
    # Cross factor for each quadrat
    
    current_quadrat_characteristics <- oak.data[i, c(
      "Site",
      "Canopy",
      "Unique_Quadrat",
      "Quadrat",
      "Site_MeanTemp",
      "litterinitialN",
      "Moisture",
      "Temperature",
      "SIR",
      "Navail",
      "LitCloss"
    )]
    
    for(j in 1:2){
      uTemp_current <- uTemp_factor[j]
      for(k in 1:3){
        uMoist_current <- uMoist_factor[k]
        
        # introduce some randomness for nitrogen content for litter samples 
        uLitN_current <- runif(1, uLitNinit-0.01, uLitNinit+0.01)
        
        # Historical variable, figure this out 2020112
        # me, 20200119, using simply temperature and moisture from the quadrats.
        
        quad.moist_current <- current_quadrat_characteristics[,"Moisture"]
        quad.temp_current <- current_quadrat_characteristics[,"Temperature"]
        
        for(l in 1:rep){
          rep_current <- l
          
          
          Site[row.number] <- current_quadrat_characteristics[,"Site"]
          Canopy[row.number] <- current_quadrat_characteristics[,"Canopy"]
          Unique_Quadrat[row.number] <- current_quadrat_characteristics[,"Unique_Quadrat"]
          Quadrat[row.number] <- current_quadrat_characteristics[,"Quadrat"]
          Site_MeanTemp[row.number] <- current_quadrat_characteristics[,"Site_MeanTemp"]
          Moisture[row.number] <- current_quadrat_characteristics[,"Moisture"]
          Temperature[row.number] <- current_quadrat_characteristics[,"Temperature"]
          SIR[row.number] <- current_quadrat_characteristics[,"SIR"]
          Navail[row.number] <- current_quadrat_characteristics[,"Navail"]
          LitCloss[row.number] <- current_quadrat_characteristics[,"LitCloss"]
          litterinitialN[row.number] <- current_quadrat_characteristics[,"litterinitialN"]
          
          
          Unique_Microcosm[row.number] <- row.number
          replicate[row.number] <- rep_current
          uMoisture[row.number] <- as.character(uMoist_current)
          uTemperature[row.number] <- as.character(uTemp_current)
          uLitN[row.number] <- uLitN_current
          
          
          e <- residual_variation <- rnorm(1, 0, sigma.residual) ## Residual variation used here
          
          # Model to calcuate decomposition rates for microcosm
          
          # intercept = 3, uTemp_coeff = 0.2, uMoist_coeff = 1, quad.moist_coeff = 0.04, 
          # quad.temp_coeff = 0.04, uTqTinter = .5, uTqMinter = .5,uMqMinter = .5, uMqTinter = .5, 
          # inoc.litter.inter = 0.1, uLitNinit = 0.9, rep = 3, sigma.residual = .1
          # 
          if(uMoist_current == 25 & uTemp_current == 12){ # cold and dry microcosm, no interactions
            uCmin[row.number] <- overall_intercept + uLow_intercept + uMoistdry_intercept + uLow_coeff*quad.temp_current + uMoistdry_coeff*quad.moist_current+ e
          } else if(uMoist_current == 50 & uTemp_current == 12){ # cold and optimal microcosm, no interactions
            uCmin[row.number] <- overall_intercept + uLow_intercept + uMoistopt_intercept + uLow_coeff*quad.temp_current + uMoistopt_coeff*quad.moist_current+ e
          } else if(uMoist_current == 100 & uTemp_current == 12){ # cold and wet microcosm, no interactions
            uCmin[row.number] <- overall_intercept + uLow_intercept + uMoistwet_intercept + uLow_coeff*quad.temp_current + uMoistwet_coeff*quad.moist_current+ e
          }
          
          else if(uMoist_current == 25 & uTemp_current == 22){ # warm and dry microcosm, no interactions
            uCmin[row.number] <- overall_intercept + uhigh_intercept + uMoistdry_intercept + uhigh_coeff*quad.temp_current + uMoistdry_coeff*quad.moist_current+ e
          }else if(uMoist_current == 50 & uTemp_current == 22){ # warm and optimal microcosm, no interactions
            uCmin[row.number] <- overall_intercept + uhigh_intercept + uMoistopt_intercept + uhigh_coeff*quad.temp_current + uMoistopt_coeff*quad.moist_current+ e
          }else if(uMoist_current == 100 & uTemp_current == 22){ # warm and wet microcosm, no interactions
            uCmin[row.number] <- overall_intercept + uhigh_intercept + uMoistwet_intercept + uhigh_coeff*quad.temp_current + uMoistwet_coeff*quad.moist_current+ e
          }
          
          else{
            uCmin[row.number] <- NA
          }
          
          
          row.number <- row.number + 1
        }
      }
    }
  }
  
  # create data frame
  
  data.frame(Site = Site, 
             Canopy = Canopy,
             Unique_Quadrat = Unique_Quadrat,
             Quadrat = Quadrat,
             Site_MeanTemp = Site_MeanTemp,
             litterinitialN = litterinitialN,
             Moisture = Moisture,
             Temperature = Temperature,
             SIR = SIR,
             Navail = Navail,
             LitCloss = LitCloss,
             Unique_Microcosm = Unique_Microcosm,
             replicate = replicate,
             uMoisture = uMoisture,
             uTemperature = uTemperature,
             uCmin = uCmin
  )
}


utemp.test <- sim.microcosm.data(sigma.residual = 0.5, uMoistdry_coeff = -0.05, uMoistopt_coeff = -0.05, uMoistwet_coeff = -0.05)

str(utemp.test)


utemp.test$uMoisture <- factor(utemp.test$uMoisture, levels = c("25", "50", "100"))
utemp.test$uTemperature <- factor(utemp.test$uTemperature, levels = c("12", "22"))


# Cmin by Quadrat Temperature
ggplot(utemp.test, aes(x = Temperature, y = uCmin, color = uMoisture, linetype = uTemperature)) + 
  geom_point(aes(shape = uTemperature)) + geom_smooth(method='lm', formula= y~x)
# graph2ppt(file="microcosm simulation by quadrat temp.pptx", width=7, height=5) 

# Cmin by Quadrat Moisture
ggplot(utemp.test, aes(x = Moisture, y = uCmin, color = uMoisture, linetype = uTemperature)) + 
  geom_point(aes(shape = uTemperature))+ geom_smooth(method='lm', formula= y~x)
# graph2ppt(file="microcosm simulation by quadrat moisture.pptx", width=7, height=5) 


# microcosm effects plots

ggplot(utemp.test, aes(x = uMoisture, y = uCmin, fill = uTemperature)) + 
  geom_boxplot(outlier.alpha = 0) + geom_point(position =position_jitterdodge(), alpha = .2)
# graph2ppt(file="microcosm simulation by uMoisture.pptx", width=7, height=5) 

ggplot(utemp.test, aes(x = uTemperature, y = uCmin, fill = uMoisture)) + 
  geom_boxplot(outlier.alpha = 0) + geom_point(position =position_jitterdodge(), alpha = .2)
# graph2ppt(file="microcosm simulation by uTemperature.pptx", width=7, height=5) 





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


