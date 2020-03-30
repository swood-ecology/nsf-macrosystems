# last worked on 20200319 AP
# function to process on IRGA raw data
# output is a file with co2 flux per hour for each microcosm

library(tidyverse)

#' Add together two numbers.
#' 
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' add(1, 1)
#' add(10, 1)

cmin_calc_fun <- function(cmin, date){
  
  # Calculate standard values for all places with standards
  not_any_na <- function(x) all(!is.na(x))
  
  cmin[is.na(cmin$std.value.1)==FALSE,] -> stds
  stds %>% add_column(meanStandard='NA') -> stds
  
  for(i in 1:nrow(stds)){
    bind_rows(
      stds[i,c("std.value.1","std.value.2","std.value.3","std.value.4")] %>%
        select_if(not_any_na) %>% gather()
    ) %>%
      summarize(mean = mean(value)) -> stds[(i),'meanStandard']
  }

  # calculate differences in standards by using slope method
  
  
  corr_std <- numeric() # corrected standard based on slope between the standards that delimit each set
  the.time <- numeric()
  the.slope <- numeric()
  
  # corr_std[1] <- stds$meanStandard[1] # manually enter first standard value for the first sample
  # 
  # v.num <- 2
  
  v.num <- 1
  for(j in 2:length(stds$cmin.id)){
    
    for(i in 1:(stds$cmin.id[j] - stds$cmin.id[j-1])){
      # interpolate between two sets of standards
      # standard integral + [(change in standards)/(change in time)]*(change in time between start of set to sample in set)
      
      corr_std[v.num] <- as.numeric(stds$meanStandard[j]) +  
        ((as.numeric(stds$meanStandard[j]) - as.numeric(stds$meanStandard[j-1])) / 
           as.numeric(difftime(as.POSIXct(stds$std.start.time[j], format = "%m/%d/%Y %H:%M"),
                               as.POSIXct(stds$std.start.time[j-1], format = "%m/%d/%Y %H:%M"), 
                               units = "min"))) *
        as.numeric(difftime(as.POSIXct(cmin$time.irga[stds$cmin.id[j-1] + i - 1], format = "%H:%M"),
                            as.POSIXct(cmin$time.irga[stds$cmin.id[j-1]], format = "%H:%M"), 
                            units = "min"))
      
      
      the.time[v.num] <- as.numeric(difftime(as.POSIXct(cmin$time.irga[stds$cmin.id[j-1] + i - 1], format = "%H:%M"),
                                             as.POSIXct(cmin$time.irga[stds$cmin.id[j-1]], format = "%H:%M"),
                                             units = "min"))
      
      the.slope[v.num] <- ((as.numeric(stds$meanStandard[j]) - as.numeric(stds$meanStandard[j-1])) / 
                             as.numeric(difftime(as.POSIXct(stds$std.start.time[j], format = "%m/%d/%Y %H:%M"),
                                                 as.POSIXct(stds$std.start.time[j-1], format = "%m/%d/%Y %H:%M"), 
                                        units = "min")))
      v.num <- v.num + 1
    }
    
  }
  cmin <- cmin[1:(dim(cmin)[1] - 1),] # remove last row, it is where the last standard is placed
  
  mutate(cmin, 
         correctedStandard = as.numeric(corr_std),
         the.time = the.time,
         the.slope = the.slope) %>% 
    select(-c("std.value.1","std.value.2" , "std.value.3", "std.value.4",
              "std.start.time", "std.end.time", "notes.flushing", 
              "notes.irga")) -> cmin
  
  # calculate co2 flux for all microcosms
  
  cmin %>%
    mutate(
      incubationTime = as.numeric(strptime(paste(date.irga, time.irga, sep = " "), "%m/%d/%Y %H:%M") - 
                                    strptime(paste(date.flush, time.flush, sep = " "), "%m/%d/%Y %H:%M")), # Hours
      dilutionFactor = ((5*times.sampled)/(57.15-soil.volume))+1,    
      measuredCO2 = irga.integral*(standard.co2/correctedStandard),             # ppm
      concentrationCO2 = measuredCO2*dilutionFactor,                       # ppm
      volumeCO2 = concentrationCO2*((57.15-soil.volume)/1000),             # L
      molesCO2 = (volumeCO2/22.414)*273.15/293.15,                         # mol
      CO2C = molesCO2*12.011,                                              # g
      CO2CperHour = CO2C/incubationTime                                    # g h-1
    ) %>% select(c(lab.id, unique.id, soil.volume, standard.co2, correctedStandard, the.time, the.slope,
                   irga.integral, incubationTime, CO2C, CO2CperHour)) -> cmin_calc 
  
  write.csv(cmin_calc, paste("irga_calc_", date, ".csv")) 
  
  cmin_calc %>% select(c(lab.id, unique.id, soil.volume, CO2CperHour)) -> cmin_calc
  
  return(cmin_calc)
}