# microcosm with standardized oak litter and only factor by moisture
# February 6, 2020

# Libraries
library(ggplot2)
library(lme4)
library(standardize)
library(export)

# Field data from code "field_data_exploration.R" and get masterdata dataframe

source("field_data_exploration.R")

str(masterdata)

# Add a few "simulated" fields from the field data

Unique_q <- 1:length(masterdata$Quadrat) 
masterdata <- cbind(Unique_q, masterdata)



sim.microcosm.data <- function(overall_intercept = 50, 
                               uMoistdry_intercept = -35, uMoistdry_coeff = 0, # dry microcosm parameters
                               uMoistopt_intercept = 0, uMoistopt_coeff = 0,# optimal microcosm parameters
                               uMoistwet_intercept = -25, uMoistwet_coeff = 0,# wet microcosm parameters
                               uLitNinit = 0.9, rep = 3, sigma.residual = 0){
  
  ## ucosm effect names
  uMoist_factor <- factor(c(35, 60, 100), levels = c(35, 60, 100))
  
  
  # quadrat characteristics
  Unique_q <- numeric()
  Site <- character()
  Species <- character()
  Quadrat <- character()
  Moisture <- numeric()
  Temperature <- numeric()
  # SIR <- numeric() # Will have this data later
  # Navail <- numeric()
  # LitCloss <- numeric()

  
  # microcosm characteristics
  Unique_Microcosm <- character()
  uLitN <- numeric() # Still using a made up variable for oak litter N concentrations
  uMoisture <- character()
  uCmin <- numeric()
  replicate <- numeric()
  

  row.number <- 1
  
  to.run <- dim(masterdata)[1]
  
  for(i in 1:to.run){
    
    # Pulling data from FIELD DATA

    
    current_quadrat_characteristics <- masterdata[i, c(
      "Unique_q",
      "Site",
      "Species",
      "SoilGWC",
      "Aspect",
      "Slope",
      "meanSoilT"
    )]
    
      for(k in 1:3){
        uMoist_current <- uMoist_factor[k]
        
        # introduce some randomness for nitrogen content for standardized oak litter samples 
        uLitN_current <- runif(1, 0.75, 0.9)
        
        # Historical variable, figure this out 2020112
        # me, 20200119, using simply temperature and moisture from the quadrats.
        
        quad.moist_current <- current_quadrat_characteristics[,"SoilGWC"]
        quad.temp_current <- current_quadrat_characteristics[,"meanSoilT"]
        
        for(l in 1:rep){
          rep_current <- l
          
          Unique_q[row.number] <- current_quadrat_characteristics[,"Unique_q"]
          Site[row.number] <- current_quadrat_characteristics[,"Site"]
          Species[row.number] <- current_quadrat_characteristics[,"Species"]
          Moisture[row.number] <- current_quadrat_characteristics[,"SoilGWC"]
          Temperature[row.number] <- current_quadrat_characteristics[,"meanSoilT"]
          # SIR[row.number] <- current_quadrat_characteristics[,"SIR"]
          # Navail[row.number] <- current_quadrat_characteristics[,"Navail"]
          # LitCloss[row.number] <- current_quadrat_characteristics[,"LitCloss"]
          # litterinitialN[row.number] <- current_quadrat_characteristics[,"litterinitialN"]
          
          
          Unique_Microcosm[row.number] <- row.number
          replicate[row.number] <- rep_current
          uMoisture[row.number] <- as.character(uMoist_current)
          uLitN[row.number] <- uLitN_current
          
          
          e <- residual_variation <- rnorm(1, 0, sigma.residual) ## Residual variation used here
          
          # Model to calcuate decomposition rates for microcosm
          

          # All held at 22 C
          if(uMoist_current == 35){ # dry microcosm, no interactions
            uCmin[row.number] <- overall_intercept + uMoistdry_intercept + uMoistdry_coeff*quad.moist_current+ e
            
          } else if(uMoist_current == 60){ # optimal microcosm, no interactions
            uCmin[row.number] <- overall_intercept + uMoistopt_intercept + uMoistopt_coeff*quad.moist_current+ e
            
          } else if(uMoist_current == 100){ # wet microcosm, no interactions
            uCmin[row.number] <- overall_intercept + uMoistwet_intercept  + uMoistwet_coeff*quad.moist_current+ e
          }
          
          
          else{
            uCmin[row.number] <- NA
          }
          
          
          row.number <- row.number + 1
        }
      }
  }
  
  # create data frame
  
  data.frame(Unique_q = Unique_q,
             Site = Site, 
             Species = Species,
             Moisture = Moisture,
             Temperature = Temperature,
             Unique_Microcosm = Unique_Microcosm,
             replicate = replicate,
             uMoisture = uMoisture,
             uCmin = uCmin
  )
}

umoist.test <- sim.microcosm.data()

dim(umoist.test)

umoist.test <- sim.microcosm.data(sigma.residual = 0.5, uMoistdry_coeff = -0.05, uMoistopt_coeff = -0.05, uMoistwet_coeff = -0.05)

str(umoist.test)

####### For powerpoint presentation ##### 

umoist.test <- sim.microcosm.data(sigma.residual = .5, uMoistdry_coeff = -.2, uMoistopt_coeff = 0, uMoistwet_coeff = .2)
umoist.test$uMoisture <- factor(umoist.test$uMoisture, levels = c("35", "60", "100"))


# microcosm effects plots

ggplot(umoist.test, aes(x = uMoisture, y = uCmin, fill = uMoisture)) + 
  geom_boxplot(outlier.alpha = 0) + geom_point(position =position_jitterdodge(), alpha = .2)



graph2ppt(file="microcosm simulation by uMoisture wet and dry ineraction.pptx", width=7, height=5) 

# Cmin by Quadrat Moisture
ggplot(umoist.test, aes(x = Moisture, y = uCmin, color = uMoisture)) + 
  geom_point()+ geom_smooth(method='lm', formula= y~x)

graph2ppt(file="microcosm simulation by quadrat moisture_wet and dry interaction.pptx", width=7, height=5) 


# microcosm effects plots

ggplot(umoist.test, aes(x = uMoisture, y = uCmin, fill = uMoisture)) + 
  geom_boxplot(outlier.alpha = 0) + geom_point(position =position_jitterdodge(), alpha = .2)

# graph2ppt(file="microcosm simulation by uMoisture.pptx", width=7, height=5) 



umoist.test <- sim.microcosm.data(sigma.residual = 0.5, uMoistdry_coeff = -0.005, uMoistopt_coeff = -0.005, uMoistwet_coeff = -0.005)


utemp.model <- lm(uCmin ~ uMoisture*Moisture*Temperature, data = umoist.test)

summary(utemp.model)
anova(utemp.model)










