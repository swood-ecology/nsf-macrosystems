


# Second option of using 10 soils from each tree!! 20200215
{
  set.seed(101)
  # RO
  
  SCBIRO <- masterdata[masterdata$Site == "SCBI" & masterdata$Species == "RO" & masterdata$pH > 6,] # note removing skewed pH lower than 6
  SCBIRO_ord <- SCBIRO[order(SCBIRO$SoilGWC),]
  microcosm_SCBIRO <- c(sample(SCBIRO_ord$uniqueQuadrat[1:4], 3),
                        sample(SCBIRO_ord$uniqueQuadrat[5:9], 3),
                        sample(SCBIRO_ord$uniqueQuadrat[10:13], 4))
  #HI
  SCBIHI <- masterdata[masterdata$Site == "SCBI" & masterdata$Species == "HI"& masterdata$pH > 6,] # note removing skewed pH lower than 6
  SCBIHI_ord <- SCBIHI[order(SCBIHI$SoilGWC),]
  microcosm_SCBIHI <- c(sample(SCBIHI_ord$uniqueQuadrat[1:3], 3),
                        sample(SCBIHI_ord$uniqueQuadrat[4:7], 3),
                        sample(SCBIHI_ord$uniqueQuadrat[8:10], 3))
  #TP
  SCBITP <- masterdata[masterdata$Site == "SCBI" & masterdata$Species == "TP",]
  SCBITP_ord <- SCBITP[order(SCBITP$SoilGWC),]
  microcosm_SCBITP <- c(sample(SCBITP_ord$uniqueQuadrat[1:5], 4),
                        sample(SCBITP_ord$uniqueQuadrat[6:11], 2),
                        sample(SCBITP_ord$uniqueQuadrat[12:16], 4))
  
  
  #HARV
  # RO
  HARVRO <- masterdata[masterdata$Site == "HARV" & masterdata$Species == "RO",]
  HARVRO_ord <- HARVRO[order(HARVRO$SoilGWC),]
  microcosm_HARVRO <- c(sample(HARVRO_ord$uniqueQuadrat[1:5], 4),
                        sample(HARVRO_ord$uniqueQuadrat[6:11], 2),
                        sample(HARVRO_ord$uniqueQuadrat[12:16], 4))
  #WP
  HARVWP <- masterdata[masterdata$Site == "HARV" & masterdata$Species == "WP",]
  HARVWP_ord <- HARVWP[order(HARVWP$SoilGWC),]
  microcosm_HARVWP <- c(sample(HARVWP_ord$uniqueQuadrat[1:5], 4),
                        sample(HARVWP_ord$uniqueQuadrat[6:11], 2),
                        sample(HARVWP_ord$uniqueQuadrat[12:16], 4))
  #RM
  HARVRM <- masterdata[masterdata$Site == "HARV" & masterdata$Species == "RM",]
  HARVRM_ord <- HARVRM[order(HARVRM$SoilGWC),]
  microcosm_HARVRM <- c(sample(HARVRM_ord$uniqueQuadrat[1:5], 4),
                        sample(HARVRM_ord$uniqueQuadrat[6:11], 2),
                        sample(HARVRM_ord$uniqueQuadrat[12:16], 4))
  
  
  
  microcosm_quadrat_number <- c(microcosm_SCBIRO, microcosm_SCBIHI, microcosm_SCBITP, 
                                microcosm_HARVRO, microcosm_HARVWP, microcosm_HARVRM)
  
  
  microcosm_quadrats <- masterdata[masterdata$uniqueQuadra %in% microcosm_quadrat_number,]
  
  masterdata$microcosm <- masterdata$uniqueQuadra %in% microcosm_quadrat_number
  
}

# not removing below 6 pH from scbi

set.seed(19)
# RO
{
SCBIRO <- masterdata[masterdata$Site == "SCBI" & masterdata$Species == "RO",] # note removing skewed pH lower than 6
SCBIRO_ord <- SCBIRO[order(SCBIRO$SoilGWC),]
microcosm_SCBIRO <- c(sample(SCBIRO_ord$uniqueQuadrat[1:5], 3),
                      sample(SCBIRO_ord$uniqueQuadrat[6:11], 3),
                      sample(SCBIRO_ord$uniqueQuadrat[12:16], 3))
#HI
SCBIHI <- masterdata[masterdata$Site == "SCBI" & masterdata$Species == "HI",] # note removing skewed pH lower than 6
SCBIHI_ord <- SCBIHI[order(SCBIHI$SoilGWC),]
microcosm_SCBIHI <- c(sample(SCBIHI_ord$uniqueQuadrat[1:5], 3),
                      sample(SCBIHI_ord$uniqueQuadrat[6:11], 3),
                      sample(SCBIHI_ord$uniqueQuadrat[12:16], 3))
#TP
SCBITP <- masterdata[masterdata$Site == "SCBI" & masterdata$Species == "TP",]
SCBITP_ord <- SCBITP[order(SCBITP$SoilGWC),]
microcosm_SCBITP <- c(sample(SCBITP_ord$uniqueQuadrat[1:5], 3),
                      sample(SCBITP_ord$uniqueQuadrat[6:11], 3),
                      sample(SCBITP_ord$uniqueQuadrat[12:16], 3))


#HARV
# RO
HARVRO <- masterdata[masterdata$Site == "HARV" & masterdata$Species == "RO",]
HARVRO_ord <- HARVRO[order(HARVRO$SoilGWC),]
microcosm_HARVRO <- c(sample(HARVRO_ord$uniqueQuadrat[1:5], 3),
                      sample(HARVRO_ord$uniqueQuadrat[6:11], 3),
                      sample(HARVRO_ord$uniqueQuadrat[12:16], 3))
#WP
HARVWP <- masterdata[masterdata$Site == "HARV" & masterdata$Species == "WP",]
HARVWP_ord <- HARVWP[order(HARVWP$SoilGWC),]
microcosm_HARVWP <- c(sample(HARVWP_ord$uniqueQuadrat[1:5], 3),
                      sample(HARVWP_ord$uniqueQuadrat[6:11], 3),
                      sample(HARVWP_ord$uniqueQuadrat[12:16], 3))
#RM
HARVRM <- masterdata[masterdata$Site == "HARV" & masterdata$Species == "RM",]
HARVRM_ord <- HARVRM[order(HARVRM$SoilGWC),]
microcosm_HARVRM <- c(sample(HARVRM_ord$uniqueQuadrat[1:5], 3),
                      sample(HARVRM_ord$uniqueQuadrat[6:11], 3),
                      sample(HARVRM_ord$uniqueQuadrat[12:16], 3))



microcosm_quadrat_number <- c(microcosm_SCBIRO, microcosm_SCBIHI, microcosm_SCBITP, 
                              microcosm_HARVRO, microcosm_HARVWP, microcosm_HARVRM)


microcosm_quadrats <- masterdata[masterdata$uniqueQuadrat %in% microcosm_quadrat_number,]
masterdata$microcosm <- masterdata$uniqueQuadrat %in% microcosm_quadrat_number
}

# Visualize plots



# Soil GWC 


ggplot(masterdata, aes(x = SoilGWC, fill = Site)) + geom_histogram(binwidth = 2.5) + xlim(25,95) + 
  geom_histogram(data = microcosm_quadrats, aes(color = I("black")), fill = "white", alpha = 0.6, linetype="dashed", binwidth = 2.5) +
  facet_grid(Species~.) + labs(title = "Subset of quadrats for microcosm experiement", 
                               subtitle = "Dashed are the subset used for microcosm. In red oak, at 40%, both chosen are from HARV, not 1 from SCBI 1 from HARV. visialization incorrect")

# graph2ppt(file = "Chosen_microsites_vs_all.ppt", width = 7, height = 7)

# SIR
ggplot(masterdata, aes(x = SIR, fill = Site)) + geom_histogram(binwidth = .2) +
  geom_histogram(data = microcosm_quadrats, aes(color = I("black")), fill = "white", alpha = 0.6, linetype="dashed", binwidth = .2) +
  facet_grid(Species~.) + labs(title = "Subset of quadrats for microcosm experiement", 
                               subtitle = "Dashed are the subset used for microcosm. In red oak, at 40%, both chosen are from HARV, not 1 from SCBI 1 from HARV. visialization incorrect")


# pH

ggplot(masterdata, aes(x = pH, fill = Site)) + geom_histogram(binwidth = .1) +
  geom_histogram(data = microcosm_quadrats, aes(color = I("black")), fill = "white", alpha = 0.6, linetype="dashed", binwidth = .1) +
  facet_grid(Species~.) + labs(title = "Subset of quadrats for microcosm experiement", 
                               subtitle = "Dashed are the subset used for microcosm. In red oak, at 40%, both chosen are from HARV, not 1 from SCBI 1 from HARV. visialization incorrect")


ggplot(masterdata, aes(x = meanSoilM, fill = Site)) + geom_histogram(binwidth = 2.5) +
  geom_histogram(data = microcosm_quadrats, aes(color = I("black")), fill = "white", alpha = 0.6, linetype="dashed", binwidth = 2.5) +
  facet_grid(Species~.) + labs(title = "Subset of quadrats for microcosm experiement", 
                               subtitle = "Dashed are the subset used for microcosm. In red oak, at 40%, both chosen are from HARV, not 1 from SCBI 1 from HARV. visialization incorrect")


