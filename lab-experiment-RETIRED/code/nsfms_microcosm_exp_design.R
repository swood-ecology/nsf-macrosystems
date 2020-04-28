# Microcosm experimental design

site_data <- read_csv("~/Bradford Lab/Box_microcosm/metadata/nsfms_site_data.csv")
microcosm_select <- read_csv("~/Bradford Lab/Box_microcosm/metadata/nsfms_micocosm_selection.csv")
microcosm_select2 <- read_csv("~/Bradford Lab/Box_microcosm/metadata/nsfms_micocosm_high_rep_selection.csv")


microcosm_select <- microcosm_select$microcosm_select # selected quadrats
microcosm_select2 <- microcosm_select2$microcosm_select2 # high replication selected quadrats
 
masterdata <- cbind(site_data, microcosm_select,microcosm_select2)


# Site characteristics 


experiement_num <- c("a", "b")
moist_trt <- c(35, 60, 100)

rep <- c(1,2,3)



##### Experiment 1 #####
# For experiement 1 three moisture treatments @20C with 1 replicate
experiement <- experiement_num[1] # first moisture treat experiement
PlotID_exp1 <- masterdata$plotID[masterdata$microcosm_select == TRUE]
PlotID_highrep <- masterdata$plotID[masterdata$microcosm_select2 == TRUE]


microcosmExp <- character()
microcosmNum <- numeric()
labID <- numeric()
plotID <- character()
moistTRT <- numeric()
tempTRT <- numeric()
Rep <- numeric()


row.number <- 1
microcosm.number <- 1



for(l in 1:length(PlotID_exp1)){
  plotID_current <- PlotID_exp1[l]
  
  for(k in 1:3){
    
    moistTRT_current <- moist_trt[k]
      tempTRT_current <- temp_trt[2]
      
      microcosmExp_current <- experiement
      microcosmNum_current <- microcosm.number
      microcosm.number <- microcosm.number + 1

      if(sum(plotID_current == PlotID_highrep) > 0){
        
        for(i in 1:8){
        Rep_current <- i
        
        
        plotID[row.number] <- plotID_current
        microcosmExp[row.number] <- microcosmExp_current
        microcosmNum[row.number] <- microcosmNum_current
        labID[row.number] <- row.number
        moistTRT[row.number] <- moistTRT_current
        tempTRT[row.number] <- tempTRT_current
        Rep[row.number] <- Rep_current
        
        row.number <- row.number + 1
        
        }
        
        }else{
      Rep_current <- 1
      
      plotID[row.number] <- plotID_current
      microcosmExp[row.number] <- microcosmExp_current
      microcosmNum[row.number] <- microcosmNum_current
      labID[row.number] <- row.number
      moistTRT[row.number] <- moistTRT_current
      tempTRT[row.number] <- tempTRT_current
      Rep[row.number] <- Rep_current
      
      row.number <- row.number + 1
      }
        
        
         
  }
  
  }

microcosm_exp_design_1 <- data.frame(plotID = plotID, microcosmExp = microcosmExp,microcosmNum = microcosmNum , Rep = Rep,labID = labID,  moistTRT = moistTRT, tempTRT = tempTRT)

write.csv(microcosm_exp_design_1, "~/nsf-macrosystems-master/nsf-macrosystems-master/metadata/nsfms_microcosm_exp_design_1.csv")


##### Experiment 2 ##### 

# Soils only controls across 

row.number <- 1
experiement <- experiement_num[2]

labID.number <- 247
microcosm.number <- 163



microcosmExp <- character()
microcosmNum <- numeric()
labID <- numeric()
plotID <- character()
moistTRT <- numeric()
tempTRT <- numeric()
Rep <- numeric()

for(l in 1:length(PlotID_exp1)){
  
  plotID_current <- PlotID_exp1[l]
  
  for(k in 1:3){
    
    moistTRT_current <- moist_trt[k]
    tempTRT_current <- temp_trt[2]
    
    microcosmExp_current <- experiement
    microcosmNum_current <- microcosm.number
    labID_current <- labID.number

      Rep_current <- 1
      
      plotID[row.number] <- plotID_current
      microcosmExp[row.number] <- microcosmExp_current
      microcosmNum[row.number] <- microcosmNum_current
      labID[row.number] <- labID_current
      moistTRT[row.number] <- moistTRT_current
      tempTRT[row.number] <- tempTRT_current
      Rep[row.number] <- Rep_current
      
      row.number <- row.number + 1
      microcosm.number <- microcosm.number + 1
      labID.number <- labID.number + 1
    
  }
    
  }
  

microcosm_exp_design_2 <- data.frame(plotID = plotID, microcosmExp = microcosmExp, microcosmNum = microcosmNum , Rep = Rep,labID = labID,  moistTRT = moistTRT, tempTRT = tempTRT)


microcosm_exp_design <- rbind(microcosm_exp_design_1, microcosm_exp_design_2)

write.csv(microcosm_exp_design, "microcosm_exp_design.csv")



