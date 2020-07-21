
# last worked on January 20, 2020

library(lme4)
library(ggplot2)



# Import real data from HARV
HARV_data <- read.csv("/Users/alexa/Documents/Bradford Lab/Microcosm Work/Field Data/HARV_SecondVisit_Data.csv", header = T)

# Import data from SCBI
SCBI_data <- read.csv("/Users/alexa/Documents/Bradford Lab/Microcosm Work/Field Data/SCBI_SecondVisit_Data.csv", header = T)

# calculate means 
HARV_data$meanSoilT<-((HARV_data$SoilT1 +HARV_data$SoilT2 +HARV_data$SoilT3 )/3) 
HARV_data$meanLitT <-((HARV_data$LitT1 +HARV_data$LitT2 +HARV_data$LitT3 )/3)
HARV_data$meanSoilM <-((HARV_data$SoilM1  +HARV_data$SoilM2  +HARV_data$SoilM3 )/3)

SCBI_data$meanSoilT<-((SCBI_data$SoilT1 +SCBI_data$SoilT2 +SCBI_data$SoilT3 )/3) 
SCBI_data$meanLitT <-((SCBI_data$LitT1 +SCBI_data$LitT2 +SCBI_data$LitT3 )/3)
SCBI_data$meanSoilM <-((SCBI_data$SoilM1  +SCBI_data$SoilM2  +SCBI_data$SoilM3 )/3)



summary(HARV_data$meanSoilT[HARV_data$Species == "RO"],na.rm = TRUE)
summary(HARV_data$meanSoilT[HARV_data$Species == "RM"],na.rm = TRUE)
summary(HARV_data$meanSoilT[HARV_data$Species == "WP"],na.rm = TRUE)

summary(HARV_data$meanLitT[HARV_data$Species == "RO"],na.rm = TRUE)
summary(HARV_data$meanLitT[HARV_data$Species == "RM"],na.rm = TRUE)
summary(HARV_data$meanLitT[HARV_data$Species == "WP"],na.rm = TRUE)

summary(HARV_data$meanSoilM[HARV_data$Species == "RO"],na.rm = TRUE)
summary(HARV_data$meanSoilM[HARV_data$Species == "RM"],na.rm = TRUE)
summary(HARV_data$meanSoilM[HARV_data$Species == "WP"],na.rm = TRUE)



plot(meanSoilT ~ Species, data = HARV_data)
plot(meanLitT ~ Species, data = HARV_data)
plot(meanSoilM ~ Species, data = HARV_data)
plot(SoilGWC ~ Species, data = HARV_data)



plot(meanSoilT ~ Species, data = SCBI_data)
plot(meanLitT ~ Species, data = SCBI_data)
plot(meanSoilM ~ Species, data = SCBI_data)
plot(SoilGWC ~ Species, data = SCBI_data)




cor(as.matrix(SCBI_data[,12:14]))



cor(as.matrix(HARV_data[-c(34, 36),12:14]))

# Correlations between Harvard real measurements 
# -0.5385 correlation between mean soil moisture and mean soil T

# > cor(as.matrix(HARV_data[-c(34, 36),12:14]))
#            meanSoilT   meanLitT  meanSoilM
# meanSoilT  1.0000000  0.5505060 -0.5385536
# meanLitT   0.5505060  1.0000000 -0.1517204
# meanSoilM -0.5385536 -0.1517204  1.0000000

# to simulate correlated data, with normal distribution, then the second variable which you want to simulate can be
# DATA = rnorm(n, CORRELATIONCOEFF*FIRSTDATA, sqrt(1-CORRELATIONCOEFF^2)) 

# but what is the cause is the question. is it temp affecting moisture or moisture affectin temp. 



# Grabbing just data from Oak Quadrats
SCBI_data$meanSoilM[SCBI_data$Species == "RO"]

Sites <- c(rep(c("HARV", "SCBI"), each = 48), "SCBI")


Field_data <- rbind(HARV_data, SCBI_data)
Field_data <- cbind(Sites, Field_data)

RO.data <- Field_data[Field_data$Species == "RO",]

plot(Field_data$meanSoilM)





summary(Field_data$meanSoilM[Field_data$Species == "WP"])
plot(Field_data$meanSoilM[Field_data$Species == "WP"] ~ Field_data$meanSoilT[Field_data$Species == "WP"])


range(RO.data$meanSoilT)
RO.data
# Variables

cor(as.matrix(RO.data[,13:15]))

hist(RO.data$meanSoilT)
hist(RO.data$meanSoilM)

mean(RO.data$meanSoilM)
lines(median(RO.data$meanSoilM))

abline(median(RO.data$meanSoilM))

ggplot(RO.data, aes(x = meanSoilT, y = meanSoilM)) + geom_point()
ggplot(RO.data, aes(x = meanSoilT, y = meanSoilM)) + geom_point()

# Litter Temp by species and Site
ggplot(Field_data, aes(x = Species, y = meanLitT, fill = Species)) + geom_boxplot() + facet_grid(.~Sites)
# Soil Temp by species and Site
ggplot(Field_data, aes(x = Species, y = meanSoilT, fill = Species)) + geom_boxplot() + facet_grid(.~Sites)

# Soil Moisture by species and Site
ggplot(Field_data, aes(x = Species, y = meanSoilM, fill = Species)) + geom_boxplot() + facet_grid(.~Sites)

# Soil Moisture by species
ggplot(Field_data, aes(x = Species, y = meanSoilM, fill = Species)) + geom_boxplot()


##### Model to simulate decomposition rates of field data 

# Sites
# Quadrats
# Canopy Type
# % N litter content
# Moisture
# Temperature





######



# There's the nameing part and then the filling in numbers part...


# HARV mean annual T is 8 C
# HARV mean annual P is 967 mm

# SCBI mean annual T is 13 C (+5)
# SCBI mean annual P is 1054 mm (+100mm)



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
        
        
        ### add variation for coefficients. !!!!
        
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

ggplot(test.data, aes(x = Canopy , y= LitCloss)) + geom_boxplot() + facet_grid(.~Site)
ggplot(test.data, aes(x = Canopy , y= LitCloss)) + geom_violin() + facet_grid(.~Site)



plot(test.data)

plot(test.data$Moisture ~ test.data$Unique_Quadrat, col = test.data$Canopy)
# here is maybe obvious that canopy and moisture should be more linked -- tulip poplar shows bimodal distribution (random from unif) of high and low moisture


plot.default(test.data$LitCloss    ~ test.data$Unique_Quadrat, col = test.data$Canopy, type = "p")
plot.default(test.data$LitCloss    ~ test.data$Temperature, col = test.data$Canopy, type = "p")

range(test.data$Temperature)
plot.default(test.data$LitCloss    ~ test.data$Moisture, col = test.data$Canopy, type = "p")
plot.default(test.data$LitCloss    ~ test.data$litterinitialN, col = test.data$Canopy, type = "p")
plot.default(test.data$LitCloss    ~ test.data$SIR, col = test.data$Canopy, type = "p")
plot.default(test.data$litterinitialN    ~ test.data$Unique_Quadrat, col = test.data$Canopy, type = "p")


plot.default(test.data$LitCloss ~ test.data$Site, col = test.data$Canopy, type = "p")

col(test.data)
# log transform dependent variable... 
test.data$log_litCloss <- log(test.data$LitCloss)

plot.default(test.data$log_litCloss    ~ test.data$Moisture, col = test.data$Canopy, type = "p")
plot.default(test.data$log_litCloss    ~ test.data$litterinitialN, col = test.data$Canopy, type = "p")
plot.default(test.data$log_litCloss    ~ test.data$SIR, col = test.data$Canopy, type = "p")
plot.default(test.data$log_litCloss    ~ test.data$Unique_Quadrat, col = test.data$Canopy, type = "p")


hist(test.data$log_litCloss)


## Analyse it ##
test.data.model <- lm(LitCloss ~ Temperature + I(Temperature^2) + litterinitialN + Moisture + Navail, data = test.data) 

test.data.model <- lmer(LitCloss ~ Temperature + I(Temperature^2) + litterinitialN + Moisture + Navail + (1|Site), data = test.data) 
summary(test.data.model)



# look at standardized coefficients as well. interesting. 
summary(standardize(test.data.model))
summary((test.data.model))

pamer.fnc(test.data.model)
plot(test.data.model)
r2.mixed(test.data.model)
sqrt(vif.mer(test.data.model))







##### Unstandardized coefficients for microsite interactions model to simulate data #####
# to do another day 20200119

sim.field.data <- function(intercept = -17.1, litterN_effect = 19.3, temp_effect = 1.05, temp_effect_2 = -0.047,
                           moisture_effect = .141, soil_N_effect = 0.014, SIR_effect = 4.93, site.residual = 0.1, quadrat.residual = 0.2){
  
  
  ## Site
  site_names <- site.group.names <- c("HARV", "SCBI")
  
  ## Temperature by site
  ## Then will have a range within the site (which is about 3.5 from bradford 2017)
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
  
  row.number <- 1
  
  for(a in 1:2){ # SITE -> TEMP
    site_current <- site_names[a]
    temperature_site_current <- temperature_effect[a]
    
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
        
        es <- site_residual_variation <- rnorm(1, 0, site.residual)
        e <- quadrat_residual_variation <- rnorm(1, 0, quadrat.residual) ## Residual variation used here
        
        # Model to calcuate decomposition rates similar to bradford (2017) manuscript unstandardized MICROSITE MAIN EFFECTS
        LitCloss[row.number] <- mu + 
          temperature_site_current*temp_effect +
          I(temperature_site_current^2)*temp_effect_2 +
          litterinitialN_current*litterN_effect +
          moisture_current* moisture_effect + 
          temperature_current*temp_effect +
          SIR_current*SIR_effect +
          Navail_current*soil_N_effect + es + e
        
        
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
             LitCloss = LitCloss
  )
  
  
  
}



test.data <- sim.field.data()
test.data











##### Regression by keeping means constant and changin variable of interest and plotting
# mean(test.data$Moisture)
# mean(test.data$litterinitialN)
# mean(test.data$SIR)
# mean(test.data$Navail)

regress.respiration.byT <- numeric()
test.temp <- seq(6, 25, .5)
for (i in 1:length(test.temp)){
  regress.respiration.byT[i] <- -17.1 + 1.05*test.temp[i] + (-0.047)*I(test.temp[i]^2) + .141*mean(test.data$Moisture) + 
    19.3*mean(test.data$litterinitialN) + 4.93*mean(test.data$SIR) + 0.014*mean(test.data$Navail)
  
}

plot(regress.respiration.byT ~ test.temp)


test.temp <- seq(6, 14, .1)


regress.respiration.byM <- numeric()
range(test.data$Moisture)

test.moist <- seq(10, 80, 1)
for (i in 1:length(test.moist)){
  regress.respiration.byM[i] <- -17.1 + 1.05*mean(test.data$Temperature) + (-0.047)*I(mean(test.data$Temperature)^2) + .141*test.moist[i] + 
    19.3*mean(test.data$litterinitialN) + 4.93*mean(test.data$SIR) + 0.014*mean(test.data$Navail)
  
}


plot(regress.respiration.byM ~ test.moist)




#### Look at breakup of data collected from field

plot(meanSoilM ~ Species, data = Field_data)
plot(Field_data$meanSoilM[Field_data$Species == "RO"])

sum(Field_data$Species == "RO")












##### Code for Oak-specific inoculum and Oak Litter microcosms ##### 
# 20200119


# add all interactions
# remake the anova model for calculating the c-mineralization
# test effect of coefficients
# see where you can see effect sizes
# draw the graphs of what we are trying to get
# make it able to confirm a null and alternate hypothesis

# right now, removing any interaction between inoculum and canopy so oak with oak. no home field advantage stuff
# only temp and moisture history effect
# u refers to microsite (usite); q refers to quadrat

# pulls data from the data table from the field data simulation

# but also expect initial SIR to have an effect on decomposition rate. correlation with any other factors??

# dry microcosm by quadrat moisture
intercept <- 10
slope <- -1

# opt microcosm by quadrat moisture
intercept <- 30
slope <- 0

# wet microcosm by quadrat moisture
intercept <- 20
slope <- 1

# cold microcosm by quadrat temperature
intercept <- 10
slope <- -0.5

# warm microcosm by quadrat temperature
intercept <- 30
slope <- 0

uTemp_factor <- factor(rep(c(12, 22), each = 3), levels = c(12, 22))
uMoist_factor <- factor(rep(c(25, 50, 100), 2), levels = c("25", "50", "100"))


udf <- data.frame(uTemp_factor = uTemp_factor, uMoist_factor = uMoist_factor)


dd <- data.frame(a = gl(3,4), b = gl(4,1,12))

options("contrasts")
model.matrix(~ uTemp_factor + uMoist_factor, udf)



b <- data.frame(matrix(NA,nrow=nrow(udf),ncol=1)) #make empty data.frame to initiate b

for ( i in 1:ncol(udf)) { #start loop
  a <- data.frame(model.matrix(~udf[,i])) #make dummies here
  b <- cbind(b, a[-1]) #remove intercept and combine dummies
}
b <- data.frame(b[-1]) #make a data.frame
#the reference dummy gets excluded automatically by model.matrix

colnames(b) <- c('Temperature' , '50WHC' , '100WHC') #you will probably want to change the names to sth smaller




cmin <- numeric()

for(i in 1:6)
  mymod <- lm(Time ~ intercept + Temperature*50WHC+Temperature*100WHC, data=new_data) #compute lm with interactions


10 + 0.2*quadrat_temp*b[1,1] + 0.2*quadrat_temp*b[1,1]


model.matrix(udf)


current_quadrat_characteristics <- oak.data[1:32, c("Moisture", "Temperature")]


dry.ucosm <- current_quadrat_characteristics[1,1]

# Making dataset with the hypothesis graphs
#   intercept = 3, 
#   uMoist_coeff = 0.6, quad.moist_coeff = 0, 
# quad.temp_coeff = 0, uTqTinter = 0, uTqMinter = 0, uMqMinter = 0, uMqTinter = 0, 
# inoc.litter.inter = 0, uLitNinit = 0.9, uLitN_effect = 0,

# Overparameterized... 


sim.microcosm.data <- function(overall_intercept = 50, 
                               uLow_intercept = -15, uLow_coeff = -0.2, # cold microcosm parameters
                               uhigh_intercept = 0, uhigh_coeff = 0, # warm microcosm parameters
                               uMoistdry_intercept = -15, uMoistdry_coeff = -0.05, # dry microcosm parameters
                               uMoistopt_intercept = 0, uMoistopt_coeff = -0.005,# optimal microcosm parameters
                               uMoistwet_intercept = -8, uMoistwet_coeff = 0.1,# wet microcosm parameters
                               uLitNinit = 0.9, rep = 3, sigma.residual = 3){
  
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
  
  
  # ONLY USING OAK DATA
  oak.data <- test.data[test.data$Canopy == "Oak",]
  to.run <- dim(oak.data)[1]
  
  for(i in 1:to.run){
    current_quadrat_characteristics <- oak.data[i, 1:11]
    
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
            uCmin[row.number] <- overall_intercept + uLow_intercept + uMoistdry_intercept + uLow_coeff*quad.temp_current + uMoistwet_coeff*quad.moist_current+ e
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


utemp.test <- sim.microcosm.data()

str(utemp.test)


ggplot(utemp.test, aes(x = Temperature, y = uCmin, color = uMoisture, linetype = uTemperature)) + 
  geom_point(aes(shape = uTemperature)) + geom_smooth(method='lm', formula= y~x)


ggplot(utemp.test, aes(x = Moisture, y = uCmin, color = uMoisture, linetype = uTemperature)) + 
  geom_point(aes(shape = uTemperature))+ geom_smooth(method='lm', formula= y~x)





#Convert to numeric for ANOVA
utemp.test$uMoisture <- as.numeric(as.character(utemp.test$uMoisture))
utemp.test$uTemperature <- as.numeric(as.character(utemp.test$uTemperature))





#
utemp.model <- lm(uCmin ~ uMoisture*uTemperature*Temperature*Moisture, data = utemp.test)

summary(utemp.model)
anova(utemp.model)

#
utemp.model1 <- lm(uCmin ~ uMoisture*uTemperature*Temperature, data = utemp.test)


summary(utemp.model1)
anova(utemp.model1)
#
utemp.model2 <- lm(uCmin ~ uMoisture*uTemperature*Moisture, data = utemp.test)


summary(utemp.model2)



anova.lm2 <- anova(utemp.model2)
anova.lm2$`Pr(>F)`[5]


# Test of p.value assessment 
uMoistdry_coeff = -0.05 # dry microcosm parameters
uMoistopt_coeff = -0.005 # optimal microcosm parameters
uMoistwet_coeff = 0.1

cor(utemp.test[,c("Temperature", "Moisture")])

utemp.test <- sim.microcosm.data(uLow_coeff = -0.05, uMoistdry_coeff = 0.05, uMoistopt_coeff = 0.1, uMoistwet_coeff = 0.05, sigma.residual = 0.01)


#Convert to numeric for ANOVA
utemp.test$uMoisture <- as.numeric(as.character(utemp.test$uMoisture))
utemp.test$uTemperature <- as.numeric(as.character(utemp.test$uTemperature))

utemp.test$log_uCmin <- log(utemp.test$uCmin)
utemp.test$log_Moisture <- log(utemp.test$Moisture)
utemp.test$log_Temperature <- log(utemp.test$Temperature)


utemp.model2 <- lm(log_uCmin ~ uMoisture*uTemperature*Moisture*Temperature, data = utemp.test)




summary(utemp.model2)
anova.lm2 <- anova(utemp.model2)
anova.lm2

ggplot(utemp.test, aes(x = Moisture , y = uCmin, color = uMoisture)) + 
  geom_point() + facet_grid(uMoisture~uTemperature) + geom_smooth(method='lm', formula= y~x)

ggplot(utemp.test, aes(x = Temperature , y = uCmin, color = uMoisture)) + 
  geom_point() + facet_grid(uMoisture~uTemperature) + geom_smooth(method='lm', formula= y~x)


ggplot(utemp.test, aes(x = uMoisture, y = uCmin, fill = uMoisture)) + 
  geom_bar(stat = "identity", position = position_dodge()) + geom_point(position = position_dodge(0.9)) + facet_grid(.~uTemperature)






# mixed effects. moisture or temperature are random effects with variable slopes

utemp.test <- sim.microcosm.data(uLow_coeff = -0.5, uMoistdry_coeff = 0.05, uMoistopt_coeff = 0.1, uMoistwet_coeff = 0.05, sigma.residual = 0.01)

utemp.model3 <- lmer(uCmin ~ uMoisture*uTemperature + (1|Moisture), data = utemp.test)

summary(utemp.model3)









sim.microcosm.data <- function(intercept = 3, uTemp_coeff = 1, uMoist_coeff = 0.6, quad.moist_coeff = 0, 
                               quad.temp_coeff = 0, uTqTinter = 0, uTqMinter = 0, uMqMinter = 0, uMqTinter = 0, 
                               inoc.litter.inter = 0, uLitNinit = 0.9, uLitN_effect = 0, rep = 3, sigma.residual = .1){
  
  ## ucosm effect names
  
  uTemp_factor <- c(12, 22)
  
  uMoist_factor <- c(25, 50, 100)
  
  mu <- intercept
  
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
  
  
  # ONLY USING OAK DATA
  oak.data <- test.data[test.data$Canopy == "Oak",]
  to.run <- dim(oak.data)[1]
  
  for(i in 1:to.run){
    current_quadrat_characteristics <- oak.data[i, 1:11]
    
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
          uMoisture[row.number] <- uMoist_current
          uTemperature[row.number] <- uTemp_current
          uLitN[row.number] <- uLitN_current
          
          
          e <- residual_variation <- rnorm(1, 0, sigma.residual) ## Residual variation used here
          
          # Model to calcuate decomposition rates for microcosm
          
          # intercept = 3, uTemp_coeff = 0.2, uMoist_coeff = 1, quad.moist_coeff = 0.04, 
          # quad.temp_coeff = 0.04, uTqTinter = .5, uTqMinter = .5,uMqMinter = .5, uMqTinter = .5, 
          # inoc.litter.inter = 0.1, uLitNinit = 0.9, rep = 3, sigma.residual = .1
          # 
          
          uCmin[row.number] <- mu + 
            uTemp_current*uTemp_coeff +                   # microsite temperature effect
            uMoist_current*uMoist_coeff +                 # microsite moisture effect
            quad.moist_current*quad.moist_coeff +         # quadrat moisture effect 
            quad.temp_current*quad.temp_coeff +           # quadrat temperature effect
            uTqTinter*uTemp_current*quad.temp_current +   # microsite T by quadrat T interaction
            uTqMinter*uTemp_current*quad.moist_coeff +    # microsite T by quadrat M interaction
            uMqMinter*uMoist_current*quad.moist_current + # microsite M by quadrat M interaction
            uMqTinter*uMoist_current*quad.temp_current +  # microsite M by quadrat T interaction
            uLitN_current*uLitN_effect +
            e                                             # microcosm residual error
          
          
          
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




utest.data <- sim.microcosm.data()
# Convert microcosm factors into numeric
# or not
utest.data$uMoisture <- as.numeric(as.character(utest.data$uMoisture))
utest.data$uTemperature <- as.numeric(as.character(utest.data$uTemperature))

ggplot(data = utest.data, aes(x = uMoisture, y = uCmin)) + geom_point() + geom_line(aes(linetype = uTemperature))




dim(utest.data)




plot(utest.data$uCmin ~ utest.data$uTemperature)
plot(utest.data$uCmin ~ utest.data$uMoisture)























# does not meet assumption of normality
hist(utemp.test$uCmin)
qqnorm(utemp.test$uCmin)
qqline(utemp.test$uCmin)

# try log transform
utemp.test$log_uCmin <- log(utemp.test$uCmin)
hist(utemp.test$log_uCmin)
qqnorm(utemp.test$log_uCmin)
qqline(utemp.test$log_uCmin)

utemp.test$STD_uCmin <- ((utemp.test$uCmin)-(mean(utemp.test$uCmin,na.rm=T)))/(2*sd(utemp.test$uCmin,na.rm=T))
hist(utemp.test$STD_uCmin)
qqnorm(utemp.test$STD_uCmin)
qqline(utemp.test$STD_uCmin)



# require

library(Johnson)
#Applying Johnson transformation 
utemp.test$uCmin_JT <- RE.Johnson(utemp.test$uCmin)
# check p-value of the Anderson-Darling test


hist(utemp.test$uCmin_JT[[4]])
qqnorm(utemp.test$uCmin_JT[[4]])
qqline(utemp.test$uCmin_JT[[4]])

























###### Microcosm dummy data #####

# What are the variables that create the latent variable historical conditoins?


# microcosm coefficients
uTemp_effect
uMoist_effect


uTemp_effect_name <- c(0, 1)
uTemp_effect_name <-  factor(uTemp_effect_name, levels = c(0, 1), labels = c("Low", "High"))



# Should pull relevant information from the field data simulation to inform "historical legacy" aspect... 
# Whether moisture, temperature or LitCloss, or even Navail? Should try them all!

# single substrate experiment

# Inoculum from HARV - Oak, HARV - Pine, SCBI - Oak, SCBI- Tulip Poplar

# hist effect depends on what variable(s) selected for within the function that represents historical legacy... moisture?






# using a single litter type
# create new factor that is a combination of historical temp, moist, and canopy
# for oaks with oak canopy inoculum, should be no historical affect of the canopy, but could be from temp and moisture



sim.microcosm.data <- function(intercept = 10, uTemp_coeff = 5, uMoist_coeff = 5, canopy_coeff = .1, 
                               quad.moist_coeff = 0.04, quad.temp_coeff = 0.04, MT_interaction_coeff = 5, 
                               rep = 3, sigma.residual = .1){
  
  
  ## ucosm effect names
  
  uTemp_effect <- c(0, 1)
  uTemp_effect_name <-   c("Low", "High")
  
  
  #uMoist is zero-to-sum contrasts
  uMoist_effect <- c(-1, 0, 1)
  uMoist_effect_name <-  c("Dry", "Opt", "Wet")
  
  
  # ONLY OAK 20200108
  ## Canopy Type
  # canopy_names <- canopy.group.names <- c("Oak", "Pine", "Oak", "TulPop")
  
  
  
  mu <- intercept
  
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
  uLitCloss <- numeric()
  replicate <- numeric()
  
  
  
  
  LitCloss <- numeric()
  
  row.number <- 1
  
  
  for(j in 1:64){
    current_quadrat_characteristics <- test.data[j, 1:11]
    
    for(j in 1:2){
      uTemp_current_name <- uTemp_effect_name[j]
      uTemp_current <- uTemp_effect[j]
      for(k in 1:3){
        uMoist_current_name <- uMoist_effect_name[k]
        uMoist_current <- uMoist_effect[k]
        
        
        
        uLitN_current <- runif(1, uLitNinit-0.1, uLitNinit+0.1)
        
        # Historical variable, figure this out 2020112
        
        
        hist_current_1 <- current_quadrat_characteristics[,"Moisture"] * current_quadrat_characteristics[,"Temperature"]
        
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
          
          Unique_Microcosm[row.number] <- row.number
          replicate[row.number] <- rep_current
          uMoisture[row.number] <- uMoist_current_name
          uTemperature[row.number] <- uTemp_current_name
          litterinitialN[row.number] <- uLitN_current
          hist_current[row.number] <- hist_current_1
          
          
          e <- residual_variation <- rnorm(1, 0, sigma.residual) ## Residual variation used here
          
          # Model to calcuate decomposition rates for microcosm
          
          uLitCloss[row.number] <- mu + uTemp_current*uTemp_coeff + uMoist_current*uMoist_coeff + 
            MT_interaction_coeff*uMoist_current*uTemp_current + hist_current_1*hist_coeff + e
          
          
          
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
             hist_current = hist_current,
             uLitCloss = uLitCloss
  )
}




tes.resid <- seq(0.01, 20, 0.5)
r.test.v <- numeric(length(tes.resid))
for(i in 1:length(tes.resid)){
  test.udata <- sim.microcosm.data(sigma.residual = tes.resid[i])
  umodel <- lm(uLitCloss ~ hist_current + uTemperature*uMoisture, data = test.udata)
  sum.umodel <- summary(umodel)
  r.test <- sum.umodel$adj.r.squared
  r.test.v[i] <- r.test
}

plot(r.test.v~tes.resid)

plot(uLitCloss ~ hist_current  , col =  uMoisture, data = test.udata)
plot(uLitCloss ~ uTemperature  , col =  uMoisture, data = test.udata)
plot(uLitCloss ~ uMoisture  , col =  uMoisture, data = test.udata)
plot(uLitCloss ~ Moisture  , col =  uMoisture, data = test.udata)


umodel <- lm(uLitCloss ~ hist_current + uTemperature*uMoisture, data = test.udata)

summary(umodel)


sum.umodel <- summary(umodel)

sum.umodel$adj.r.squared

umodel$terms
dd






#### sum-to-zero contrasts simulation

sim.microcosm.data <- function(intercept = 10, uTemp_coeff = 5, uMoist_coeff = 5, hist_coeff = .1, 
                               uLitNinit = .9, uLitN.e = 4, MT_interaction_coeff = 5, rep = 2, sigma.residual = .1){
  
  
  ## ucosm effect names
  
  uTemp_effect <- c(0, 1)
  uTemp_effect_name <-   c("Low", "High")
  
  
  uMoist_effect <- c(0, 1, 2)
  uMoist_effect_name <-  c("Dry", "Opt", "Wet")
  
  
  # ONLY OAK 20200108
  ## Canopy Type
  # canopy_names <- canopy.group.names <- c("Oak", "Pine", "Oak", "TulPop")
  
  
  
  mu <- intercept
  
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
  uLitCloss <- numeric()
  replicate <- numeric()
  
  
  
  
  LitCloss <- numeric()
  
  row.number <- 1
  
  
  for(j in 1:64){
    current_quadrat_characteristics <- test.data[j, 1:11]
    
    for(j in 1:2){
      uTemp_current_name <- uTemp_effect_name[j]
      uTemp_current <- uTemp_effect[j]
      for(k in 1:3){
        uMoist_current_name <- uMoist_effect_name[k]
        uMoist_current <- uMoist_effect[k]
        
        
        
        uLitN_current <- runif(1, uLitNinit-0.1, uLitNinit+0.1)
        
        # Historical variable, figure this out 2020112
        
        
        hist_current_1 <- current_quadrat_characteristics[,"Moisture"] * current_quadrat_characteristics[,"Temperature"]
        
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
          
          Unique_Microcosm[row.number] <- row.number
          replicate[row.number] <- rep_current
          uMoisture[row.number] <- uMoist_current_name
          uTemperature[row.number] <- uTemp_current_name
          litterinitialN[row.number] <- uLitN_current
          hist_current[row.number] <- hist_current_1
          
          
          e <- residual_variation <- rnorm(1, 0, sigma.residual) ## Residual variation used here
          
          # Model to calcuate decomposition rates for microcosm
          
          uLitCloss[row.number] <- mu + uTemp_current*uTemp_coeff + uMoist_current*uMoist_coeff + 
            MT_interaction_coeff*uMoist_current*uTemp_current + hist_current_1*hist_coeff + e
          
          
          
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
             hist_current = hist_current,
             uLitCloss = uLitCloss
  )
}




#### backwords making regression by fabricating data

umoist <- rep(c(1, 2, 3), each = 3)
quadrat.moist <- rep(c("qwet", "qopt", "qdry"), 3)
cmin <- c(5, 10, 15, 35, 35, 35, 45, 42, 40)
df.test <- data.frame(umoist = umoist, quadrat.moist = quadrat.moist, cmin = cmin)

lm.test <- lm(cmin~umoist*quadrat.moist)
summary(lm.test)
anova(lm.test)
# ggplot it 

pred.0 <- augment(lm.test, newdata = df.test)
base_cmin <- ggplot(df.test, aes(x = umoist, y = cmin, group = quadrat.moist)) + geom_point(aes(shape = quadrat.moist)) + 
  xlab("ucosm Moisture") + ylab("C-mineralization")
base_cmin + geom_line(aes(y = .fitted, color = quadrat.moist), data = pred.0)


?anova()
###### Try again with replicates now

umoist <- rep(c(1, 2, 3), each = 3)
quadrat.moist <- rep(c("qwet", "qopt", "qdry"), 3)
cmin <- c(5, 10, 15, 50, 50, 50, 40, 42, 45)
df.test <- data.frame(umoist = umoist, quadrat.moist = quadrat.moist, cmin = cmin)

lm.test <- lm(cmin~umoist*quadrat.moist)
summary(lm.test)

# ggplot it 

pred.0 <- augment(lm.test, newdata = df.test)
base_cmin <- ggplot(df.test, aes(x = umoist, y = cmin, group = quadrat.moist)) + geom_point(aes(shape = quadrat.moist)) + 
  xlab("ucosm Moisture") + ylab("C-mineralization")
base_cmin + geom_line(aes(y = .fitted, color = quadrat.moist), data = pred.0)








reps = 3, ureps = 3, intercept = 50, moisture.effect = 10, temp.effect = 10, 
hist.t.effect = 0, hist.m.effect = 10, litter.effect = 30,usiterep.effect = 5, 
sigma.urep = 3, sigma.rep = 3, sigma.residual = 3

ureps, intercept, moisture.effect, temp.effect, 
hist.t.effect, hist.m.effect, litter.effect,usiterep.effect, 
sigma.urep, sigma.rep, sigma.residual

# number of replicates testing 



many.reps.plotted <- function(reps = 3, ureps = 3, intercept = 50, moisture.effect = 10, temp.effect = 10, 
                              hist.t.effect = 0, hist.m.effect = 10, litter.effect = 30,usiterep.effect = 5, 
                              sigma.urep = 3, sigma.rep = 3, sigma.residual = 3) {
  
  rep.test.data <- simulate.data(reps, ureps, intercept, moisture.effect, temp.effect, 
                                 hist.t.effect, hist.m.effect, litter.effect,usiterep.effect, 
                                 sigma.urep, sigma.rep, sigma.residual)
  
  litter <- rep.test.data$LITTER
  y.values <- rep.test.data$Y
  trt <- rep.test.data$MOIST
  
  plot(
    litter,
    y.values,
    col = (trt),
    xlab = "Litter Type",
    ylab = "Respiration",
    main = paste("reps = ", reps, 
                 "Litter effect = ", litter.effect,
                 "Moisture effect = ", moisture.effect,
                 "rep variation = ", sigma.rep, 
                 "residual variation = ",sigma.residual)) 
  
  legend('bottom', legend = levels(rep.test.data$MOIST),
         col = 1:2, cex = 1, pch = 1)
}


many.reps.plotted()





library(lme4)




basic.lm <- lm(Y ~ LITTER + HIST_M + HIST_T + MOIST + TEMP + LITTER*HIST_M + LITTER*HIST_T + LITTER*MOIST, data = test)



summary(basic.lm)



# Need to correct for spatial autocorrelation so will nest quadrat within site as so (1|site:quadrat)

# Model 1: common litter substrate
# creating a linear mixed effects model that is including microsite variation and main fixed effects
# controlling for litter type
# random effects will be the spatial autocorrelation of quadrat nested in site... check with book here


Litcloss ~ Site + Quadrat + Moist + Temp + WHC + (1|Site:Quadrat)



LitCmodel15<-lmer(SLitCloss~LitinitN+meanT+I(meanT^2)+mLitMoist+Navail+SIR+(1|SITE2:TRANSECT:QUADRAT),data=Grad2[-c(157),])





# From Bradford 2017

lmer(SLitCloss~LitNum+meanT+mLitMoist+Navail+TpH+SIR+LitNum:meanT+LitNum:Navail+(1|SITE2:TRANSECT:QUADRAT),data=Grad2)
