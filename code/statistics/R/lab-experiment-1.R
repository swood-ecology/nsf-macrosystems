library(tidyverse)
library(rstan)


#### Global options for running model ####
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

#### READ CUMULATIVE DATA ####
cumulCMin <- read_csv("calculated-data/lab-experiment/experiment-1/cumulative_cmin_calc_exp-1.csv")
cumulCMinAggr <- read_csv("calculated-data/lab-experiment/experiment-1/cumulative_aggregate_cmin_calc_exp-1.csv")
metaData <- read_csv("metadata/lab-experiment/exp-1_design.csv")

#### MANIPULATE DATA ####
### Drop first column of both data frames
metaData <- metaData %>% select(-X1)

### Merge data files together based on unique id
cumulAggr <- full_join(cumulCMinAggr, metaData)

### Create lists of data for Stan ###
cumulAggr$moistSq <- cumulAggr$moist.trt^2

## No measurement error
cA.dat <- list(
  N = nrow(cumulAggr),
  y = cumulAggr$cumulativeCO2Flux,
  x = cumulAggr$moist.trt
)

## Universal error
cA.err.dat <- list(
  N = nrow(cumulAggr),
  y_obs = cumulAggr$cumulativeCO2Flux,
  y_err = rep(5,nrow(cumulAggr)),
  x = cumulAggr$moist.trt
)


### Run models ###
m1 <- stan(file = "code/statistics/stan/polynomial.stan", data = cA.dat,
           iter = 5000,
           warmup = 1000,
           control=list(adapt_delta=0.95),
           chains = 3)

m2 <- stan(file = "code/statistics/stan/polynomial_meas-err.stan", 
           data = cA.err.dat2,
           iter = 5000,
           warmup = 1000,
           control=list(adapt_delta=0.99,
                        max_treedepth=15),
           chains = 3)

print(m1)
print(m2, pars=c("alpha","beta1","beta2","sigma"))




######## TEMPORARILY DEPRECATED FOR RAW DATA ##########
#### READ RAW DATA ####
expData <- read_csv("calculated-data/lab-experiment/experiment-1/cmin_calc_exp-1.csv")
metaData <- read_csv("metadata/exp-1_design.csv")


#### MANIPULATE DATA ####
### Drop first column of both data frames
expData <- expData %>% select(-X1)
metaData <- metaData %>% select(-X1)

### Merge data files together based on unique id
data <- full_join(expData, metaData)

### Figure out why expData and the merged data are different lengths
diffdf::diffdf(data,expData) %>% print() 

### Drop rows with NA
dataNoNA <- data %>% drop_na()

### Separate out replicated data
repSamps <- dataNoNA %>% filter(replicate > 1) %>% select(unique.id) %>% unique()
dataRep <- left_join(repSamps, dataNoNA)
rm(repSamps)

### Create new ID that combines the microcosm id and time period
dataRep <- dataRep %>%
  mutate(id.day = paste0(microcosm.id,"_",day),
         moist.day = paste0(moist.trt,"_",day))

### Calculate the standard deviation for eacy id.day
dataSumm <- dataRep %>%
  group_by(id.day) %>%
  summarize(mean=mean(CO2CpergLitter),sd=sd(CO2CpergLitter))

### Merge data with microcosm descriptions
dataSumm <- dataSumm %>% 
  left_join(
    dataRep %>% select(unique.id,microcosm.id,day,moist.trt,id.day,moist.day) %>% unique()
    )
ungroup(dataSumm)

### Summarize the average standard deviation by day and moisture treatment of the experiment
dataSumm %>% group_by(moist.day) %>% summarize(sd.day = mean(sd)) %>% View()
