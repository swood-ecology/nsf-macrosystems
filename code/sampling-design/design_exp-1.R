# Microcosm experimental design

library(tidyverse)

setwd("C:/Users/alexa/Dropbox (Yale_FES)/Macrosystems Biol Bradford Wieder Wood 2019-2024/")


site_data <- read_csv("metadata/sample_IDs.csv")
microcosm_select <- read_csv("metadata/exp-1_site_selection.csv") # all chosen quadrats for experiment 1
microcosm_select2 <- read_csv("metadata/exp-1_highrep_site_selection.csv") # quadrats chosen for high replication


microcosm_select <- microcosm_select$microcosm_select # selected quadrats
microcosm_select2 <- microcosm_select2$microcosm_select2 # high replication selected quadrats
 
masterdata <- cbind(site_data, microcosm_select,microcosm_select2)


# 

experiment_num <- c("a", "b")  # "a" corresponds to litter-soil expererimental microcosms
                                # "b" corresponds to soil only standards to correct for co2 from soils
moist_trt <- c(35, 60, 100)     # water holding capacity treatments for experiment 
                                # units are in % moisture fraction




##### soil and litter experimental microcosms #####
# For experiment 1 three moisture treatments @20C with 1 replicate

experiment <- experiment_num[1] # first moisture treat experiment
PlotID_exp1 <- masterdata$unique.id[masterdata$microcosm_select == TRUE]
    # initial code had scbi site first, harv after. Need to switch them to match correct lab.id 
unique.id_exp <- c(PlotID_exp1[28:54], PlotID_exp1[1:27]) 

PlotID_highrep <- masterdata$unique.id[masterdata$microcosm_select2 == TRUE]
    # initial code had scbi site first, harv after. Need to switch them to match correct lab.id 
unique.id_highrep <- c(PlotID_highrep[3:4], PlotID_highrep[1:2])

microcosm.type <- character()
microcosm.id <- numeric()
lab.id <- numeric()
unique.id <- character()
moist.trt <- numeric()
replicate <- numeric()


row.number <- 1
microcosm.number <- 1



for(l in 1:length(unique.id_exp)){
  unique.id_current <- unique.id_exp[l]
  
  for(k in 1:3){
    
    moist.trt_current <- moist_trt[k]
      
      microcosm.type_current <- experiment
      microcosm.id_current <- microcosm.number
      microcosm.number <- microcosm.number + 1

      if(sum(unique.id_current == unique.id_highrep) > 0){
        
        for(i in 1:8){
        replicate_current <- i
        
        
        unique.id[row.number] <- unique.id_current
        microcosm.type[row.number] <- microcosm.type_current
        microcosm.id[row.number] <- microcosm.id_current
        lab.id[row.number] <- row.number
        moist.trt[row.number] <- moist.trt_current
        replicate[row.number] <- replicate_current
        
        row.number <- row.number + 1
        
        }
        
        }else{
      replicate_current <- 1
      
      unique.id[row.number] <- unique.id_current
      microcosm.type[row.number] <- microcosm.type_current
      microcosm.id[row.number] <- microcosm.id_current
      lab.id[row.number] <- row.number
      moist.trt[row.number] <- moist.trt_current
      replicate[row.number] <- replicate_current
      
      row.number <- row.number + 1
      }
        
        
         
  }
  
  }

microcosm_exp_design_1 <- data.frame(unique.id = unique.id, microcosm.type = microcosm.type,
                                     microcosm.id = microcosm.id , replicate = replicate,
                                     lab.id = lab.id,  moist.trt = moist.trt)

##### soil only controls ##### 


row.number <- 1
experiment <- experiment_num[2]

lab.id.number <- 247
microcosm.number <- 163



microcosm.type <- character()
microcosm.id <- numeric()
lab.id <- numeric()
unique.id <- character()
moist.trt <- numeric()
replicate <- numeric()

for(l in 1:length(unique.id_exp)){
  
  unique.id_current <- unique.id_exp[l]
  
  for(k in 1:3){
    
    moist.trt_current <- moist_trt[k]
    
    microcosm.type_current <- experiment
    microcosm.id_current <- microcosm.number
    lab.id_current <- lab.id.number

      replicate_current <- 1
      
      unique.id[row.number] <- unique.id_current
      microcosm.type[row.number] <- microcosm.type_current
      microcosm.id[row.number] <- microcosm.id_current
      lab.id[row.number] <- lab.id_current
      moist.trt[row.number] <- moist.trt_current
      replicate[row.number] <- replicate_current
      
      row.number <- row.number + 1
      microcosm.number <- microcosm.number + 1
      lab.id.number <- lab.id.number + 1
    
  }
    
  }
  

microcosm_exp_design_2 <- data.frame(unique.id = unique.id, microcosm.type = microcosm.type, 
                                     microcosm.id = microcosm.id , replicate = replicate,
                                     lab.id = lab.id,  moist.trt = moist.trt)


microcosm_exp_design <- rbind(microcosm_exp_design_1, microcosm_exp_design_2)

write.csv(microcosm_exp_design, "metadata/exp-1_design.csv")



