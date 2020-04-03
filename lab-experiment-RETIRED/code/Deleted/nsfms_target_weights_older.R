# Target Weights

# Last modified March 5, 2020 by AP

# Uses data from the experiemental design file which includes plotID, treatments, and replicates
# Includes the 4 plots chosen for many replicates
# Calculates Target weight using WHC for soil and litter matrix for the experiemental microcosms (micorocsmExp == a)
# Calculates targed weight using WHC for soil for the soil standards (microcosmExp == b)

# These calculations are without the 50mL tube weight which will be weight as soil and litter is added
# It will then be used in an excel sheet on the set-up day

# exports CSV into calculated-data folder




path <- "C:/Users/alexa/Dropbox (Yale_FES)/Macrosystems Biol Bradford Wieder Wood 2019-2024/Microcosm experiments/"

microcosmDesign <- read.csv(paste(path, "metadata/microcosm_exp_design.csv", sep =""))
microcosmSoilGWC <- read.csv(paste(path, "calculated-data/nsfms_gravimetric_calcs.csv", sep = ""))
microcosmSoilWHC <- read.csv(paste(path, "calculated-data/nsfms_soil_WHC_calcs.csv", sep = ""))
microcosmLitterWHC <- read.csv(paste(path, "/calculated-data/nsfms_litter_WHC_calcs_aggregated.csv", sep = ""))



# Experiment a: Litter + Soil
exp_a <- microcosmDesign[microcosmDesign$microcosmExp == "a",]
# Experiment b: Soil only (standards)
exp_b <- microcosmDesign[microcosmDesign$microcosmExp == "b",]


# Experiement a tagert soil dry weight 0.25 g
soilDryWt <- .25


soilGWC <- microcosmSoilGWC$moistureFraction[match(exp_a$plotID, microcosmSoilGWC$plotID)]

# Calculate target fresh weight
soil_FW_a <- soilDryWt/(1-soilGWC)


# Litter target weight, will be from oven so will be oven dry already
litDryWt <- 1


# Target FW for soil
soilWHC <- microcosmSoilWHC$moistureFraction[match(exp_a$plotID, microcosmSoilGWC$plotID)]
targetWHC <- (exp_a$moistTRT)/100

soil_FWtarget_a <- soilDryWt/(1-soilWHC*targetWHC)



litWHC <- microcosmLitterWHC$moistureFraction
targetWHC <- (exp_a$moistTRT)/100

lit_FWtarget_a <- litDryWt/(1-litWHC*targetWHC)

totalDW <- litDryWt + soilDryWt


total_FW <- soil_FW_a + litDryWt
target_FW <- soil_FWtarget_a + lit_FWtarget_a


initial_water_to_add <- targetFW - totalFW


expa_water <- data.frame(soilDryWt = soilDryWt, soilGWC = soilGWC, soil_FW = soil_FW_a,  soilWHC = soilWHC, litDryWt = litDryWt,litWHC = litWHC,
                         totalFW = totalFW, targetFW = targetFW, initial_water_to_add = initial_water_to_add)

# Experiment 2 addition of water

# Experiement a tagert soil dry weight 0.25 g
soilDryWt <- 6


soilGWC <- microcosmSoilGWC$moistureFraction[match(exp_b$plotID, microcosmSoilGWC$plotID)]

# Calculate target fresh weight
soil_FW_b <- soilDryWt/(1-soilGWC)

# Target FW for soil
soilWHC <- microcosmSoilWHC$moistureFraction[match(exp_b$plotID, microcosmSoilGWC$plotID)]

targetWHC <- (exp_b$moistTRT)/100

soil_FWtarget_b <- soilDryWt/(1-soilWHC*targetWHC)

initial_water_to_add <- soil_FWtarget_b - soil_FW_b

expb_water <- data.frame(soilDryWt = soilDryWt, soilGWC = soilGWC, soil_FW= soil_FW_b, soilWHC = soilWHC, litDryWt = 0, litWHC = litWHC,
                         totalFW = soil_FW_b, targetFW = soil_FWtarget_b, initial_water_to_add = initial_water_to_add)




additions <- rbind(expa_water, expb_water)


setwd(paste(path, "calculated-data/", sep =""))

write.csv(cbind(microcosmDesign, additions),"target_water_to_add_forexcel.csv")





          