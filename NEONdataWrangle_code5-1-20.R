# NEON data wrangling notes 3/15/20, NSF Macrodecomposition project
# Code to wrangle litterfall data from Neon site 
# Mike Maier

# amended 4/29/20

install.packages('neonUtilities')
library(neonUtilities)
library(tidyverse)

# first goal is to bring in litterfall data from Harvard Forest measurements and compile in  
# a logical way that will be comparable to our experimental data 

# access NEON databases using neon utilities package

# for reference it helps to look at 'explore data products' tab to find 
# ie temporal parameters etc

HarvLitter<-loadByProduct(dpID = "DP1.10033.001", # designates litterfall
              site = 'HARV', startdate = '2016-01', enddate = '2020-01')
str(HarvLitter)
summary(HarvLitter)

# ie mass data from HARV from 2016-2020, or alternately if we just want by a single year, that 
# can be subsetted out or imported with a narrower time range than the above comprehensive litter
# fall data from HARV
# dataframes can then be parsed by measurement within the entire timespan

mass_dataHARV<-HarvLitter$ltr_massdata # subsets for mass data
summary(mass_dataHARV)
hist(log10(mass_dataHARV$dryMass)) # transformation for data skewed toward 0

# or brought wholesale into global environment for ease of use 

list2env(HarvLitter, .GlobalEnv)

#################################################################################
# so what does this do for us in terms of utilizing neon data in our project? 
# some questions from the slack chats: 
# what measurements are available? are they available for all of the plots we sample or just the tower plot? 
# what's the time frequency of measurement? 
# Knowing that, we can figure out how to best aggregate the data to make them relevant to our plots. 
# Like, do we do total annual litterfall mass? 
# Is that something we could even get with the existing data?
################################################################################


write_csv(mass_dataHARV, #write subset into shared dropbox folder
          "~/Dropbox (Yale_FES)/Macrosystems Biol Bradford Wieder Wood 2019-2024/raw-data/neon/Neon_April2020wrangle/litter-mass_HARV_april-29-2020.csv")

# litterfall mass data for SCBI

SCBILitter<-loadByProduct(dpID = "DP1.10033.001", # designates litterfall
                          site = 'SCBI', startdate = '2016-01', enddate = '2020-01')
mass_dataSCBI<-SCBILitter$ltr_massdata

hist(log10(mass_dataSCBI$dryMass))

write_csv(mass_dataHARV, #write subset into shared dropbox folder
          "~/Dropbox (Yale_FES)/Macrosystems Biol Bradford Wieder Wood 2019-2024/raw-data/neon/Neon_April2020wrangle/litter-mass_SCBI_april-29-2020.csv")

# measures of soil CO2 

HARVsoilCO2 <-loadByProduct(dpID = "DP1.00095.001", site = 'HARV', startdate = '2016-12', enddate = '2020-03')

HARVCO2<- HARVsoilCO2$SCO2C_1_minute 
 # mean CO2 concentration values, one minute intervals 

HARV30_CO2<- HARVsoilCO2$SCO2C_30_minute # same but for 30 min intervals 

write_csv(HARVCO2, #write subset into shared dropbox folder
          "~/Dropbox (Yale_FES)/Macrosystems Biol Bradford Wieder Wood 2019-2024/raw-data/neon/Neon_April2020wrangle/soil_CO2_1min_HARV_may-1-2020.csv")

write_csv(HARV30_CO2, #write subset into shared dropbox folder
          "~/Dropbox (Yale_FES)/Macrosystems Biol Bradford Wieder Wood 2019-2024/raw-data/neon/Neon_April2020wrangle/soil_CO2_30min_HARV_may-1-2020.csv")
