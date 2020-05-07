library(tidyverse)
library(rstan)


#### GLOBAL STAN SPECIFICATIONS ####
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

path <- "Box Sync/Work/The Nature Conservancy/NSF Macrosystems/"
setwd(path)


#### READ CUMULATIVE DATA ####
# All C mineralization data
cumulCMin <- read_csv("calculated-data/lab-experiment/experiment-1/cumulative_cmin_calc_exp-1.csv")
# Data aggregated per tube
cumulCMinAggr <- read_csv("calculated-data/lab-experiment/experiment-1/cumulative_aggregate_cmin_calc_exp-1.csv")
# Treatment description data
metaData <- read_csv("metadata/lab-experiment/exp-1_design.csv")
# Soil moisture data
plotMoisture <- read_csv("calculated-data/field-experiment/prelim/soilGWC_prelim-1_Fall-2019.csv")


#### MANIPULATE DATA ####
# Drop first column of data frames
metaData <- metaData %>% select(-X1)
plotMoisture <- plotMoisture %>% select(-X1)

# Merge data files together based on unique id
cumulAggr <- full_join(cumulCMinAggr, metaData) %>% left_join(plotMoisture)

# Create lists of data for Stan
cumulAggr$moistSq <- cumulAggr$moist.trt^2

# Subset to only replicated data
cumul.rep <- cumulCMin %>% filter(
  replicate!=1 | lead(replicate) != 1 
)

# Calculate average SD
cumul.rep %>% group_by(moist.trt) %>% summarize(sd=sd(cumulativeCO2Flux))

# Construct site-level ID
cumulAggr$site <- cumulAggr$unique.id %>% str_sub(1,4)


#### CONSTRUCT DATA LISTS ####
# No measurement error
cA.dat <- list(
  N = nrow(cumulAggr),
  y = cumulAggr$cumulativeCO2Flux,
  moistTreat = cumulAggr$moist.trt,
  moistPlot = cumulAggr$moisturePercent
)
cA.dat.ml <- list(
  N = nrow(cumulAggr),
  J = 2,
  y = cumulAggr$cumulativeCO2Flux,
  moistTreat = cumulAggr$moist.trt,
  moistPlot = cumulAggr$moisturePercent,
  site = as.numeric(as.factor(cumulAggr$site))
)
cA.dat.err.ml <- list(
  N = nrow(cumulAggr),
  J = 2,
  y_obs = cumulAggr$cumulativeCO2Flux,
  tau = 3,
  y_err = rep(5,nrow(cumulAggr)),
  # sd_known = 5,
  moistTreat = cumulAggr$moist.trt,
  moistPlot_meas = cumulAggr$moisturePercent,
  site = as.numeric(as.factor(cumulAggr$site))
)

# Universal error
## MODIFY y_err to be moisture treatment specific
cA.err.dat <- list(
  N = nrow(cumulAggr),
  y_obs = cumulAggr$cumulativeCO2Flux,
  y_err = rep(5,nrow(cumulAggr)),
  x = cumulAggr$moist.trt
)


#### RUN STAN MODELS ####
m1 <- stan(file = "code/statistics/stan/polynomial.stan", data = cA.dat,
           iter = 5000,
           warmup = 2000,
           control=list(adapt_delta=0.99,
                        max_treedepth=15),
           chains = 3)
m2 <- stan(file = "code/statistics/stan/polynomial_multilevel.stan", data = cA.dat.ml,
           iter = 5000,
           warmup = 2000,
           control=list(adapt_delta=0.99,
                        max_treedepth=15),
           chains = 3)
m3 <- stan(file = "code/statistics/stan/polynomial_multilevel_meas-err.stan", data = cA.dat.err.ml,
              iter = 5000,
              warmup = 2000,
              control=list(adapt_delta=0.99,
                           max_treedepth=15),
              chains = 3)

#### EVALUATE STAN MODELS ####
print(m1)
print(m2)
print(m3, pars=c("alpha","betaMoistTreat","betaMoistTreatSq","betaMoistPlot","sd_parameter","sd_total"))
print(m1_ml_err, pars=c("alpha","betaMoistTreat","betaMoistTreatSq","betaMoistPlot","sd_parameter","sd_total"))




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
