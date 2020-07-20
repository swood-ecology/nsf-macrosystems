# litter decomposition measurements

# 1. convert initial air dry mass to initial oven dry mass
# 2. convert 4 month fresh weight to 4 month oven dry mass
# 3. estimate loss from travel bag assay
# 4. calculate mass loss between prelim-1 and prelim-2

##### Import Data #####
library(tidyverse)
setwd("C:/Users/alexa/Dropbox (Yale_FES)/Macrosystems Biol Bradford Wieder Wood 2019-2024/")

# initial, prelim-1
litter_prelim_1 <- read_csv("raw-data/field-experiment/prelim/litter_chemistry_prelim-1_Nov-2019.csv")

# 4 months, prelim-2
litter_prelim_2 <- read_csv("raw-data/field-experiment/prelim/litter_chemistry_prelim-2_Spring-2020.csv")

# travel bag
travel_bag <- read_csv("raw-data/field-experiment/prelim/travel_bags_prelim-1.csv")

##### mass loss #####

litter_prelim_1 %>%
  mutate(
    ovenAirDryFraction = (vial.oven.dried.litter-vial.mass)/(vial.air.dried.litter-vial.mass),
    litterDryMass_1 = litterbag.leaf.mass * ovenAirDryFraction # oven dry leaf mass (g)
  )  %>%  dplyr::select(tag, site, plot, species, litterDryMass_1) -> litter_prelim_1_conversion


litter_prelim_2[is.na(litter_prelim_2$tin.label)==FALSE,] %>%
  mutate(
    ovenFreshFraction = (tin.oven.dried.litter-tin.mass)/(tin.fresh.litter-tin.mass),
    moistureFraction = ((tin.fresh.litter-tin.mass)- (tin.oven.dried.litter-tin.mass))/(tin.fresh.litter-tin.mass),
    litterDryMass_2 = litterbag.leaf.mass * ovenFreshFraction # oven dry leaf mass (g)
  ) %>%  dplyr::select(unique.id, tag, site, plot, species, litterDryMass_2) -> litter_prelim_2_conversion


travel_bag %>% 
  mutate(
    ovenAirDryFraction = (vial.oven.dried.litter-vial.mass)/(vial.air.dried.litter-vial.mass),
    initialLitterDryMass = litterbag.leaf.mass * ovenAirDryFraction,
    travelMassLoss = oven.dry.litter.mass - initialLitterDryMass,
    travelLossFraction = travelMassLoss/initialLitterDryMass # loss of litter as fraction of initial mass
    ) %>%
  dplyr::select(species, travelLossFraction)%>%
  group_by(species) %>%
  dplyr::summarize(travelLossFraction = mean(travelLossFraction)) -> travelBagCorrection

# travel bags increased in weight mostly

# in final dataframe, "_wcorr" means with travel litterbag correction
# exaggerates decomposition 

left_join(litter_prelim_2_conversion, litter_prelim_1_conversion, by = c("tag", "site", "plot", "species")) %>%
  left_join(., travelBagCorrection, by = "species") %>%
  mutate(
    litterDryMass_initial_wcorr = litterDryMass_1 + (litterDryMass_1*travelLossFraction), # with litterbag correction
    fractionMassLoss_wcorr = (litterDryMass_2 - litterDryMass_initial_wcorr)/litterDryMass_initial_wcorr,
    litterDryMass_initial = litterDryMass_1,
    fractionMassLoss = (litterDryMass_2 - litterDryMass_initial)/litterDryMass_initial
  ) %>% group_by(unique.id) %>%
  summarize(meanFractionMassLoss = mean(fractionMassLoss), # fraction of initial leaf mass lost 
            sdFractionMassLoss = sd(fractionMassLoss),
            meanFractionMassLoss_wcorr = mean(fractionMassLoss_wcorr),
            sdFractionMassLoss_wcorr = sd(fractionMassLoss_wcorr)
            ) -> litterMassLoss


write.csv(litterMassLoss, "calculated-data/field-experiment/prelim/litterbag_decomp_prelim-2_Spring-2020.csv")
