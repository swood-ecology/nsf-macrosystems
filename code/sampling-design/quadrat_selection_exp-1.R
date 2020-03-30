##### Sampling from moisture regimes by species ##### 
# sorting df based on gwc into three moisture regimes
# sampling 3 quadrats from each of these regimes

library(tidyverse)

setwd("C:/Users/alexa/Dropbox (Yale_FES)/Macrosystems Biol Bradford Wieder Wood 2019-2024/")

site_data <- read_csv("metadata/sample_IDs.csv")
soil_GWC <- read_csv("calculated-data/field-experiment/prelim/soilGWC_prelim-1_Fall-2019.csv")

SoilGWC <- soil_GWC$moisturePercent

masterdata <- cbind(site_data, SoilGWC)

##### quadrats chosen for lab microcosm expereriment 1 #####

set.seed(19)
str(site_data)
{
  # RO
  
  scbiRO <- masterdata[masterdata$site == "scbi" & masterdata$species == "RO",]
  scbiRO_ord <- scbiRO[order(scbiRO$SoilGWC),]
  microcosm_scbiRO <- c(sample(scbiRO_ord$unique.id[1:5], 3),
                        sample(scbiRO_ord$unique.id[6:11], 3),
                        sample(scbiRO_ord$unique.id[12:16], 3))
  #HI
  scbiHI <- masterdata[masterdata$site == "scbi" & masterdata$species == "HI",] 
  scbiHI_ord <- scbiHI[order(scbiHI$SoilGWC),]
  microcosm_scbiHI <- c(sample(scbiHI_ord$unique.id[1:5], 3),
                        sample(scbiHI_ord$unique.id[6:11], 3),
                        sample(scbiHI_ord$unique.id[12:16], 3))
  # TP
  scbiTP <- masterdata[masterdata$site == "scbi" & masterdata$species == "TP",]
  scbiTP_ord <- scbiTP[order(scbiTP$SoilGWC),]
  microcosm_scbiTP <- c(sample(scbiTP_ord$unique.id[1:5], 3),
                        sample(scbiTP_ord$unique.id[6:11], 3),
                        sample(scbiTP_ord$unique.id[12:16], 3))
  
  
  #harv
  # RO
  harvRO <- masterdata[masterdata$site == "harv" & masterdata$species == "RO",]
  harvRO_ord <- harvRO[order(harvRO$SoilGWC),]
  microcosm_harvRO <- c(sample(harvRO_ord$unique.id[1:5], 3),
                        sample(harvRO_ord$unique.id[6:11], 3),
                        sample(harvRO_ord$unique.id[12:16], 3))
  #WP
  harvWP <- masterdata[masterdata$site == "harv" & masterdata$species == "WP",]
  harvWP_ord <- harvWP[order(harvWP$SoilGWC),]
  microcosm_harvWP <- c(sample(harvWP_ord$unique.id[1:5], 3),
                        sample(harvWP_ord$unique.id[6:11], 3),
                        sample(harvWP_ord$unique.id[12:16], 3))
  #RM
  harvRM <- masterdata[masterdata$site == "harv" & masterdata$species == "RM",]
  harvRM_ord <- harvRM[order(harvRM$SoilGWC),]
  microcosm_harvRM <- c(sample(harvRM_ord$unique.id[1:5], 3),
                        sample(harvRM_ord$unique.id[6:11], 3),
                        sample(harvRM_ord$unique.id[12:16], 3))
  
  
  
  microcosm_quadrat_number <- c(microcosm_scbiRO, microcosm_scbiHI, microcosm_scbiTP, 
                                microcosm_harvRO, microcosm_harvWP, microcosm_harvRM)
  
  
  microcosm_quadrats <- masterdata[masterdata$unique.id %in% microcosm_quadrat_number,]
  masterdata$microcosm_select <- masterdata$unique.id %in% microcosm_quadrat_number
  
  micocosm_selection <- data.frame(unique.id = masterdata$unique.id, microcosm_select = masterdata$microcosm_select)
}

setwd("metadata/")
write.csv(micocosm_selection, "exp-1_site_selection.csv")

##### high replication quadrat selection #####

# selected sites that are going to be subset and replicated 8 times for the distribution of variation.
# one high and one low moisture from each q. rubra plot at scbi and harv sites

{
  scbiRO2 <- masterdata[masterdata$site == "scbi" & masterdata$species == "RO" & masterdata$microcosm_select == "TRUE",] 
  scbiRO_ord2 <- scbiRO2[order(scbiRO2$SoilGWC),]
  microcosm_scbiRO2 <- c(sample(scbiRO_ord2$unique.id[1:3], 1),
                         sample(scbiRO_ord2$unique.id[7:9], 1))
  
  
  harvRO2 <- masterdata[masterdata$site == "harv" & masterdata$species == "RO" & masterdata$microcosm_select == "TRUE",] 
  harvRO_ord2 <- harvRO2[order(harvRO2$SoilGWC),]
  microcosm_harvRO2 <- c(sample(harvRO_ord2$unique.id[1:3], 1),
                         sample(harvRO_ord2$unique.id[7:9], 1))
  
  microcosm_quadrat_number2 <- c(microcosm_scbiRO2, microcosm_harvRO2)
  
  
  microcosm_quadrats2 <- masterdata[masterdata$unique.id %in% microcosm_quadrat_number2,]
  masterdata$microcosm_select2 <- masterdata$unique.id %in% microcosm_quadrat_number2
  
  high.rep.selection <- data.frame(unique.id = masterdata$unique.id, microcosm_select2 = masterdata$microcosm_select2)
}


write.csv(high.rep.selection, "exp-1_highrep_site_selection.csv")



write.csv(masterdata[masterdata$microcosm_select == "TRUE", c("site", "Plot", "species", "microcosm_select2")], "Selected_soils.csv")


###### Visualize chosen plots  #####

# Soil GWC 
ggplot(masterdata, aes(x = SoilGWC, fill = site)) + geom_histogram(binwidth = 2.5) + xlim(25,95) + 
  geom_histogram(data = microcosm_quadrats, aes(color = I("black")), fill = "white", alpha = 0.6, linetype="dashed", binwidth = 2.5) +
  facet_grid(species~.) + labs(title = "Subset of quadrats for microcosm experiement", 
                               subtitle = "Dashed are the subset used for microcosm. In red oak, at 40%, both chosen are from harv, not 1 from harv 1 from harv. visialization incorrect")
# graph2ppt(file = "Chosen_microsites_vs_allGWC.ppt", width = 7, height = 7)

    # subset chosen RO 
# Soil GWC 
ggplot(masterdata, aes(x = SoilGWC, fill = site)) + geom_histogram(binwidth = 2.5) + xlim(25,95) + 
  geom_histogram(data = microcosm_quadrats2, aes(color = I("black")), fill = "white", alpha = 0.6, linetype="dashed", binwidth = 2.5) +
  facet_grid(species~.) + labs(title = "Subset of quadrats for microcosm experiement", 
                               subtitle = "Dashed are the subset used for microcosm. In red oak, at 40%, both chosen are from harv, not 1 from harv 1 from harv. visialization incorrect")

