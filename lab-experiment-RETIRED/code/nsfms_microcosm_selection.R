##### Sampling from moisture regimes by species ##### 
# sorting df based on gwc into three moisture regimes
# sampling 3 quadrats from each of these regimes



site_data <- read_csv("~/Bradford Lab/Box_microcosm/metadata/nsfms_site_data.csv")
nsfms_soil_GWC <- read_csv("~/Bradford Lab/Box_microcosm/calculated-data/nsfms_gravimetric_calcs.csv")

SoilGWC <- nsfms_soil_GWC$moisturePercent

masterdata <- cbind(site_data, SoilGWC)

set.seed(19)

{
# RO

  SCBIRO <- masterdata[masterdata$Site == "SCBI" & masterdata$Species == "RO",] # note removing skewed pH lower than 6
  SCBIRO_ord <- SCBIRO[order(SCBIRO$SoilGWC),]
  microcosm_SCBIRO <- c(sample(SCBIRO_ord$plotID[1:5], 3),
                        sample(SCBIRO_ord$plotID[6:11], 3),
                        sample(SCBIRO_ord$plotID[12:16], 3))
  #HI
  SCBIHI <- masterdata[masterdata$Site == "SCBI" & masterdata$Species == "HI",] # note removing skewed pH lower than 6
  SCBIHI_ord <- SCBIHI[order(SCBIHI$SoilGWC),]
  microcosm_SCBIHI <- c(sample(SCBIHI_ord$plotID[1:5], 3),
                        sample(SCBIHI_ord$plotID[6:11], 3),
                        sample(SCBIHI_ord$plotID[12:16], 3))
  # TP
  SCBITP <- masterdata[masterdata$Site == "SCBI" & masterdata$Species == "TP",]
  SCBITP_ord <- SCBITP[order(SCBITP$SoilGWC),]
  microcosm_SCBITP <- c(sample(SCBITP_ord$plotID[1:5], 3),
                        sample(SCBITP_ord$plotID[6:11], 3),
                        sample(SCBITP_ord$plotID[12:16], 3))

  
  #HARV
  # RO
  HARVRO <- masterdata[masterdata$Site == "HARV" & masterdata$Species == "RO",]
  HARVRO_ord <- HARVRO[order(HARVRO$SoilGWC),]
  microcosm_HARVRO <- c(sample(HARVRO_ord$plotID[1:5], 3),
                        sample(HARVRO_ord$plotID[6:11], 3),
                        sample(HARVRO_ord$plotID[12:16], 3))
  #WP
  HARVWP <- masterdata[masterdata$Site == "HARV" & masterdata$Species == "WP",]
  HARVWP_ord <- HARVWP[order(HARVWP$SoilGWC),]
  microcosm_HARVWP <- c(sample(HARVWP_ord$plotID[1:5], 3),
                        sample(HARVWP_ord$plotID[6:11], 3),
                        sample(HARVWP_ord$plotID[12:16], 3))
  #RM
  HARVRM <- masterdata[masterdata$Site == "HARV" & masterdata$Species == "RM",]
  HARVRM_ord <- HARVRM[order(HARVRM$SoilGWC),]
  microcosm_HARVRM <- c(sample(HARVRM_ord$plotID[1:5], 3),
                        sample(HARVRM_ord$plotID[6:11], 3),
                        sample(HARVRM_ord$plotID[12:16], 3))
  
  
  
  microcosm_quadrat_number <- c(microcosm_SCBIRO, microcosm_SCBIHI, microcosm_SCBITP, 
                                microcosm_HARVRO, microcosm_HARVWP, microcosm_HARVRM)
  
  
  microcosm_quadrats <- masterdata[masterdata$plotID %in% microcosm_quadrat_number,]
  masterdata$microcosm_select <- masterdata$plotID %in% microcosm_quadrat_number

micocosm_selection <- data.frame(plotID = masterdata$plotID, microcosm_select = masterdata$microcosm_select)
}
setwd("~/Bradford Lab/Box_microcosm/metadata/")

write.csv(micocosm_selection, "nsfms_micocosm_selection.csv")



# selected sites that are going to be subset and replicated 7 more times to get distributions of variation...
# one high and one low moisture from each q. rubra plot in scbi and harv

SCBIRO2 <- masterdata[masterdata$Site == "SCBI" & masterdata$Species == "RO" & microcosm_select == "TRUE",] 
SCBIRO_ord2 <- SCBIRO2[order(SCBIRO2$SoilGWC),]
microcosm_SCBIRO2 <- c(sample(SCBIRO_ord2$plotID[1:3], 1),
                      sample(SCBIRO_ord2$plotID[7:9], 1))


HARVRO2 <- masterdata[masterdata$Site == "HARV" & masterdata$Species == "RO" & microcosm_select == "TRUE",] 
HARVRO_ord2 <- HARVRO2[order(HARVRO2$SoilGWC),]
microcosm_HARVRO2 <- c(sample(HARVRO_ord2$plotID[1:3], 1),
                       sample(HARVRO_ord2$plotID[7:9], 1))

microcosm_quadrat_number2 <- c(microcosm_SCBIRO2, microcosm_HARVRO2)


microcosm_quadrats2 <- masterdata[masterdata$plotID %in% microcosm_quadrat_number2,]
masterdata$microcosm_select2 <- masterdata$plotID %in% microcosm_quadrat_number2

high.rep.selection <- data.frame(plotID = masterdata$plotID, microcosm_select2 = masterdata$microcosm_select2)

setwd("~/Bradford Lab/Box_microcosm/metadata/")

write.csv(high.rep.selection, "nsfms_micocosm_high_rep_selection.csv")



write.csv(masterdata[masterdata$microcosm_select == "TRUE", c("Site", "Plot", "Species", "microcosm_select2")], "Selected_soils.csv")


###### Visualize chosen plots  ####

# Soil GWC 


ggplot(masterdata, aes(x = SoilGWC, fill = Site)) + geom_histogram(binwidth = 2.5) + xlim(25,95) + 
  geom_histogram(data = microcosm_quadrats, aes(color = I("black")), fill = "white", alpha = 0.6, linetype="dashed", binwidth = 2.5) +
  facet_grid(Species~.) + labs(title = "Subset of quadrats for microcosm experiement", 
                               subtitle = "Dashed are the subset used for microcosm. In red oak, at 40%, both chosen are from HARV, not 1 from SCBI 1 from HARV. visialization incorrect")

# graph2ppt(file = "Chosen_microsites_vs_allGWC.ppt", width = 7, height = 7)

# SIR
ggplot(masterdata, aes(x = SIR, fill = Site)) + geom_histogram(binwidth = .2) +
  geom_histogram(data = microcosm_quadrats, aes(color = I("black")), fill = "white", alpha = 0.6, linetype="dashed", binwidth = .2) +
  facet_grid(Species~.) + labs(title = "Subset of quadrats for microcosm experiement", 
                               subtitle = "Dashed are the subset used for microcosm. In red oak, at 40%, both chosen are from HARV, not 1 from SCBI 1 from HARV. visialization incorrect")
# graph2ppt(file = "Chosen_microsites_vs_allSIR.ppt", width = 7, height = 7)

# pH

ggplot(masterdata, aes(x = pH, fill = Site)) + geom_histogram(binwidth = .1) +
  geom_histogram(data = microcosm_quadrats, aes(color = I("black")), fill = "white", alpha = 0.6, linetype="dashed", binwidth = .1) +
  facet_grid(Species~.) + labs(title = "Subset of quadrats for microcosm experiement", 
                               subtitle = "Dashed are the subset used for microcosm. In red oak, at 40%, both chosen are from HARV, not 1 from SCBI 1 from HARV. visialization incorrect")
# graph2ppt(file = "Chosen_microsites_vs_allpH.ppt", width = 7, height = 7)

# ggplot(masterdata, aes(x = meanSoilM, fill = Site)) + geom_histogram(binwidth = 2.5) +
#   geom_histogram(data = microcosm_quadrats, aes(color = I("black")), fill = "white", alpha = 0.6, linetype="dashed", binwidth = 2.5) +
#   facet_grid(Species~.) + labs(title = "Subset of quadrats for microcosm experiement", 
#                                subtitle = "Dashed are the subset used for microcosm. In red oak, at 40%, both chosen are from HARV, not 1 from SCBI 1 from HARV. visialization incorrect")
# 





# subset chosen RO 

# Soil GWC 


ggplot(masterdata, aes(x = SoilGWC, fill = Site)) + geom_histogram(binwidth = 2.5) + xlim(25,95) + 
  geom_histogram(data = microcosm_quadrats2, aes(color = I("black")), fill = "white", alpha = 0.6, linetype="dashed", binwidth = 2.5) +
  facet_grid(Species~.) + labs(title = "Subset of quadrats for microcosm experiement", 
                               subtitle = "Dashed are the subset used for microcosm. In red oak, at 40%, both chosen are from HARV, not 1 from SCBI 1 from HARV. visialization incorrect")

