# data exploration for preliminary MSB studies in HARV and SCBI
# last worked on 20200206
# a polussa


# libraries
library(ggplot2)
library(export)
library(dplyr)

setwd("~/Bradford Lab/Microcosm Work/MSB_microcosm")
masterdata <- read.csv("../Field Data/masterdata.csv")
masterdata$uniqueQuadrat <- 1:length(masterdata$Site)

# remove SIR outliers for prelim study

masterdata$SIR[which(masterdata$SIR > 10)] <- NA

# calculate means 
masterdata$meanSoilT<-((masterdata$SoilT1 +masterdata$SoilT2 +masterdata$SoilT3 )/3) 
masterdata$meanLitT <-((masterdata$LitT1 +masterdata$LitT2 +masterdata$LitT3 )/3)
masterdata$meanSoilM <-((masterdata$SoilM1  +masterdata$SoilM2  +masterdata$SoilM3 )/3)

# Site split
HARV <- masterdata[masterdata$Site == "HARV", ]
SCBI <- masterdata[masterdata$Site == "SCBI", ]


# Use this dataframe: masterdata for input Gravimetric Soil Moisture in microcosm simulations


# Violin Plots

# Soil M
ggplot(masterdata, aes(x = Species, y = meanSoilM, fill = Species)) + geom_violin() + 
  geom_jitter(width = 0.1, alpha = 0.3) + facet_grid(.~Site)

# Litter T
ggplot(masterdata, aes(x = Species, y = meanLitT, fill = Species)) + geom_violin() + 
  geom_jitter(width = 0.1, alpha = 0.3) + facet_grid(.~Site)

# Soil T
ggplot(masterdata, aes(x = Species, y = meanSoilT, fill = Species)) + geom_violin() + 
  geom_jitter(width = 0.1, alpha = 0.3) + facet_grid(.~Site)

# GWC
ggplot(masterdata, aes(x = Species, y = SoilGWC, fill = Species)) + geom_violin() + 
  geom_jitter(width = 0.1, alpha = 0.3) + facet_grid(.~Site)

# pH
ggplot(masterdata, aes(x = Species, y = pH, fill = Species)) + geom_violin() + 
  geom_jitter(width = 0.1, alpha = 0.3) + facet_grid(.~Site)

# SIR 
ggplot(masterdata, aes(x = Species, y = SIR, fill = Species)) + geom_violin() + 
  geom_jitter(width = 0.1, alpha = 0.3) + facet_grid(.~Site)

# SIR boxplot
ggplot(masterdata, aes(x = Species, y = SIR, fill = Species)) + geom_boxplot() + 
  geom_jitter(width = 0.1, alpha = 0.3) + facet_grid(.~Site)

# Soil GWC by site
ggplot(masterdata, aes(x = Site, y = SoilGWC, fill = Site)) + geom_violin() + 
  geom_jitter(width = 0.1, alpha = 0.3)


# soil probe moisture by site
ggplot(masterdata, aes(x = Site, y = meanSoilM, fill = Site)) + geom_violin() + 
  geom_jitter(width = 0.1, alpha = 0.3)

# graph2ppt(file = "Moisture_by_Site.ppt", width = 5, height = 5)




##### PCA #####

# Taken from PCA examples created by J. Reuning-Scherer


# SCBI
SCBI_PCA <- masterdata[masterdata$Site == "SCBI" ,c("Species", "DBH", "SoilGWC", "pH", "SIR", "Aspect", "Elevation", "Slope", "meanSoilT", "meanLitT", "meanSoilM")]

#We need complete cases for code to work
SCBI_PCA <- SCBI_PCA[complete.cases(SCBI_PCA),]
dim(SCBI_PCA)

#make correlation matrix to see if PCA will work well - remove column of country names
round(cor(SCBI_PCA[, -1]), 2)

#Cooler visual representation of correlations
corrplot(cor(SCBI_PCA[, -1]), method = "ellipse")

#Cooler visual representation of correlations
#Order option orders variables based on data order, alphabetically, results of cluster analysis, etc.
#  See help file or link above to get details.

corrplot(cor(SCBI_PCA[,-1]),method = "ellipse", order="FPC")
corrplot(cor(SCBI_PCA[,-1]),method = "ellipse", order="AOE")
corrplot(cor(SCBI_PCA[,-1]),method = "ellipse", order="hclust")


#Here is a cool way to look for non-linearity, get correlation, make histograms all at once.
chart.Correlation(SCBI_PCA[, -1], histogram=TRUE, pch=19)

graph2ppt(file = "SCBI_corr_plot.ppt", width = 7, height = 7)


#get online function
source("http://www.reuningscherer.net/STAT660/R/CSQPlot.r.txt")

#run the function
CSQPlot(SCBI_PCA[,-1],label="SCBI_PCA SCBI Data")


pc1 <- princomp(SCBI_PCA[,-1], cor = TRUE)
names(pc1)

#print results
print(summary(pc1), digits = 2, loadings = pc1$loadings, cutoff=0)
round(pc1$sdev^2,2)
screeplot(pc1,type="lines",col="red",lwd=2,pch=19,cex=1.2,main="Scree Plot of Raw WB Data")
source("http://www.reuningscherer.net/STAT660/R/parallel.r.txt")

#make the parallel analysis plot using the parallelplot function
parallelplot(pc1)
#  c(1,2) specifies to use components 1 and 2
#get function from online
source("http://reuningscherer.net/stat660/r/ciscoreplot.R.txt")

#run the function
ciscoreplot(pc1,c(1,2),SCBI_PCA[,1])

#make a biplot for first two components
biplot(pc1,choices=c(1,2),pc.biplot=T, main = "SCBI PCA")
graph2ppt(file = "SCBI_PCA_plot.ppt", width = 7, height = 7)

# HARV
HARV_PCA <- masterdata[masterdata$Site == "HARV" ,c("Species", "DBH", "SoilGWC", "pH", "SIR", "Aspect", "Elevation", "Slope", "meanSoilT", "meanLitT", "meanSoilM")]
  
  #We need complete cases for code to work
  HARV_PCA <- HARV_PCA[complete.cases(HARV_PCA),]
  dim(HARV_PCA)
  
  #make correlation matrix to see if PCA will work well - remove column of country names
  round(cor(HARV_PCA[, -1]), 2)
  
  #Cooler visual representation of correlations
  corrplot(cor(HARV_PCA[, -1]), method = "ellipse")
  
  #Cooler visual representation of correlations
  #Order option orders variables based on data order, alphabetically, results of cluster analysis, etc.
  #  See help file or link above to get details.
  
  corrplot(cor(HARV_PCA[,-1]),method = "ellipse", order="FPC")
  corrplot(cor(HARV_PCA[,-1]),method = "ellipse", order="AOE")
  corrplot(cor(HARV_PCA[,-1]),method = "ellipse", order="hclust")
  
  
  #Here is a cool way to look for non-linearity, get correlation, make histograms all at once.
  chart.Correlation(HARV_PCA[, -1], histogram=TRUE, pch=19)
  graph2ppt(file = "HARV_corr_plot.ppt", width = 7, height = 7)
  
  #get online function
  source("http://www.reuningscherer.net/STAT660/R/CSQPlot.r.txt")
  
  #run the function
  CSQPlot(HARV_PCA[,-1],label="HARV_PCA HARV Data")
  
  
  pc1 <- princomp(HARV_PCA[,-1], cor = TRUE)
  names(pc1)
  
  #print results
  print(summary(pc1), digits = 2, loadings = pc1$loadings, cutoff=0)
  round(pc1$sdev^2,2)
  screeplot(pc1,type="lines",col="red",lwd=2,pch=19,cex=1.2,main="Scree Plot of Raw WB Data")
  source("http://www.reuningscherer.net/STAT660/R/parallel.r.txt")
  
  #make the parallel analysis plot using the parallelplot function
  parallelplot(pc1)
  #  c(1,2) specifies to use components 1 and 2
  #get function from online
  source("http://reuningscherer.net/stat660/r/ciscoreplot.R.txt")
  
  #run the function
  ciscoreplot(pc1,c(1,2),HARV_PCA[,1])
  
  #make a biplot for first two components
  biplot(pc1,choices=c(1,2),pc.biplot=T, main = "HARV PCA")

  graph2ppt(file = "HARV_PCA_plot.ppt", width = 7, height = 7)
  


  
  
###### Visualize histograms by species ######
library(plyr)
# creage groups if I want to add quantile lines to the histograms 
# I don't
mu <- ddply(masterdata, c("Site", "Species"), summarise, q=quantile(pH, c(.25, .75)))


# Soil GWC 
ggplot(masterdata, aes(x = SoilGWC, fill = Site)) + geom_histogram(binwidth = 2.5) + xlim(25,95) +
  facet_grid(Species~.) 

# graph2ppt(file = "SoilGWCbySpeciesHist.ppt", width = 3, height = 4)

# pH

ggplot(masterdata, aes(x = pH, fill = Site)) + geom_histogram(binwidth = .1)  +
  facet_grid(Species~.) + geom_vline(data = mu, aes(xintercept=q,color=paste(Site, Species), linetype="dashed"))

# graph2ppt(file = "pHbySpeciesHist.ppt", width = 3, height = 4)


# mean soil T
ggplot(masterdata, aes(x = meanSoilT, fill = Site)) + geom_histogram(binwidth = 1) +
  facet_grid(Species~.)

# mean lit T
ggplot(masterdata, aes(x = meanLitT, fill = Site)) + geom_histogram(binwidth = 1) + 
  facet_grid(Species~.)

# SIR

ggplot(masterdata, aes(x = SIR, fill = Site)) + geom_histogram(binwidth = .25) +
  facet_grid(Species~.)



SCBI$SoilGWC[SCBI$pH > 6.5]

# these are highlighting the SCBI soils with a pH less than 6.5 
ggplot(SCBI, aes(x = SoilGWC)) + geom_histogram(binwidth = 2.5) +
  facet_grid(Species~.) + geom_histogram(binwidth = 2.5, data = SCBI[SCBI$pH < 6.5,], aes(x = SoilGWC, color = "Over 6.5pH", fill= 1)) 


###### visualizing to help choose plots:  #####



# Plotting quadrats as points to visualize spread of other variables against GWC

# SCBI quadrats by gwc and pH
ggplot(data = masterdata[masterdata$Site == "SCBI", ], 
       aes(x = SoilGWC, y = pH, label = uniqueQuadrat, fill = Species, 
           alpha  = as.character(masterdata$microcosm[masterdata$Site == "SCBI"]))) + 
  geom_point() + geom_label() +  
   ylim(4.5, 7.6) + labs(title = "SCBI quadrats moisture by pH")
# graph2ppt(file = "SCBI_GWC_by_pH_quadrats.ppt", width = 7, height = 7)





# SCBI quadrats by gwc and DBH
ggplot(data = masterdata[masterdata$Site == "SCBI", ], 
       aes(x = SoilGWC, y = DBH, label = uniqueQuadrat, fill = Species)) + 
  geom_point() +geom_label() + labs(title = "SCBI quadrats moisture by DBH")

# graph2ppt(file = "SCBI_GWC_by_DBH_quadrats.ppt", width = 7, height = 7)


ggplot(data = masterdata[masterdata$Site == "SCBI", ], 
       aes(x = SoilGWC, y = SIR, label = uniqueQuadrat, fill = Species)) + 
  geom_point() +geom_label() + labs(title = "SCBI quadrats moisture by SIR")
# graph2ppt(file = "SCBI_GWC_by_SIR_quadrats.ppt", width = 7, height = 7)


# HARV quadrats by gwc and pH
ggplot(data = masterdata[masterdata$Site == "HARV", ], 
       aes(x = SoilGWC, y = pH, label = uniqueQuadrat, fill = Species)) + 
  geom_point() +geom_label() + labs(title = "HARV quadrats moisture by pH")
# graph2ppt(file = "HARV_GWC_by_pH_quadrats.ppt", width = 7, height = 7)

# HARV quadrats by gwc and DBH
ggplot(data = masterdata[masterdata$Site == "HARV", ], 
       aes(x = SoilGWC, y = DBH, label = uniqueQuadrat, fill = Species)) + 
  geom_point() +geom_label() + labs(title = "HARV quadrats moisture by DBH")

# graph2ppt(file = "HARV_GWC_by_DBH_quadrats.ppt", width = 7, height = 7)

ggplot(data = masterdata[masterdata$Site == "HARV", ], 
       aes(x = SoilGWC, y = SIR, label = uniqueQuadrat, fill = Species)) + 
  geom_point() +geom_label() + labs(title = "HARV quadrats moisture by SIR")



# Data summeries
summary(masterdata$pH[masterdata$Site == "SCBI" & masterdata$Species == "TP"])

summary(masterdata$pH[masterdata$Site == "SCBI"])


##### Random sampling from moisture regimes by species ##### 
# sorting df based on gwc into three moisture regimes
# sampling 3 quadrats from each of these regimes


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



masterdata

write.csv(masterdata, "masterdata_with_microcosm2.csv")


## Visualize chosen plots 

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
graph2ppt(file = "Chosen_microsites_vs_allpH.ppt", width = 7, height = 7)

# ggplot(masterdata, aes(x = meanSoilM, fill = Site)) + geom_histogram(binwidth = 2.5) +
#   geom_histogram(data = microcosm_quadrats, aes(color = I("black")), fill = "white", alpha = 0.6, linetype="dashed", binwidth = 2.5) +
#   facet_grid(Species~.) + labs(title = "Subset of quadrats for microcosm experiement", 
#                                subtitle = "Dashed are the subset used for microcosm. In red oak, at 40%, both chosen are from HARV, not 1 from SCBI 1 from HARV. visialization incorrect")
# 




###### Correlation Plots ##### 
library(psych)

?pairs.panels()
pairs.panels(masterdata[,c('pH', "SIR", 'SoilGWC' , "Aspect", "Elevation", "Slope","meanSoilT", "meanLitT", "meanSoilM")], 
             method = "spearman", # correlation method
             hist.col = "#00AFBB",
             lm = TRUE,
             density = FALSE,
             ellipses= FALSE
)

graph2ppt(file = "CorrMatAllSites.ppt", width = 5, height = 5)


pairs.panels(HARV[,c('pH', "SIR", 'SoilGWC' , "Aspect", "Elevation", "Slope","meanSoilT", "meanLitT", "meanSoilM")], 
             method = "spearman", # correlation method
             hist.col = "#00AFBB",
             lm = TRUE,
             density = FALSE,
             ellipses= FALSE
)
graph2ppt(file = "CorrMatHARV.ppt", width = 5, height = 5)

pairs.panels(SCBI[,c('pH', "SIR" , 'SoilGWC' , "Aspect", "Elevation", "Slope","meanSoilT", "meanLitT", "meanSoilM")], 
             method = "spearman", # correlation method
             hist.col = "#00AFBB",
             lm = TRUE,
             density = FALSE,
             ellipses= FALSE
)

graph2ppt(file = "CorrMatSCBI.ppt", width = 5, height = 5)

?pairs.panels()


##### Do distributions change when selecting for microcosm data ####

ggplot(masterdata, aes(x= SoilGWC)) + 
  geom_histogram(aes(y=..density..), binwidth = 2.5, col = "blue", alpha = 0.5) + 
  geom_density(kernel = "gaussian")  


ggplot(masterdata[masterdata$microcosm == TRUE,], aes(x= SoilGWC)) + 
  geom_histogram(aes(y=..density..), binwidth = 2.5, col = "blue", alpha = 0.5) + 
  geom_density(kernel = "gaussian")  

ggplot(masterdata[masterdata$Site == "SCBI",], aes(x= log(SoilGWC))) + 
  geom_histogram(col = "blue", alpha = 0.5) + 
  
  geom_density(kernel = "gaussian")  
ggplot(masterdata[masterdata$Site == "SCBI",], aes(x= sqrt(SoilGWC))) + 
  geom_histogram(col = "blue", alpha = 0.5) + 
  geom_density(kernel = "gaussian") 

ggplot(masterdata[masterdata$Site == "SCBI",], aes(x= SoilGWC)) + 
  geom_histogram(col = "blue", alpha = 0.5) + 
  geom_density(kernel = "gaussian")  


ggplot(masterdata[masterdata$Site == "SCBI",], aes(x= log(pH))) + 
  geom_histogram(col = "blue", alpha = 0.5) + 
  
  geom_density(kernel = "gaussian")  
ggplot(masterdata[masterdata$Site == "SCBI",], aes(x= sqrt(pH))) + 
  geom_histogram(col = "blue", alpha = 0.5) + 
  geom_density(kernel = "gaussian") 

ggplot(masterdata[masterdata$Site == "SCBI",], aes(x= pH)) + 
  geom_histogram(col = "blue", alpha = 0.5) + 
  geom_density(kernel = "gaussian")  


ggplot(masterdata[masterdata$Site == "SCBI",], aes(x= log(SIR))) + 
  geom_histogram(col = "blue", alpha = 0.5) + 
  
  geom_density(kernel = "gaussian")  
ggplot(masterdata[masterdata$Site == "SCBI",], aes(x= sqrt(SIR))) + 
  geom_histogram(col = "blue", alpha = 0.5) + 
  geom_density(kernel = "gaussian") 

ggplot(masterdata[masterdata$Site == "SCBI",], aes(x= SIR)) + 
  geom_histogram(col = "blue", alpha = 0.5) + 
  geom_density(kernel = "gaussian")  

# Notice the correlation between Slope and GWC






###### Initial anaylsis #####

library(lme4)

hist(log(masterdata$SoilGWC))

masterdata$Date


masterdata$GWC_log <- log(masterdata$SoilGWC)

lmem <- lmer(GWC_log ~ Aspect+Elevation+Slope+Site +(1|Species), data = masterdata)

# simple linear model of GWC using slope aspect elevation site and species explains .66 of variation
lmodel1 <- lm(SoilGWC ~ Slope+Aspect+Elevation*Site+Species, data = masterdata)

ggplot(data = masterdata, aes(x = Slope, y = SoilGWC, color = Species, shape = Site)) + 
  geom_point(size = 3, alpha= 0.7) + geom_smooth(aes(linetype = Site), method='lm', formula= y~x, se =F)



anova(lmodel1)
summary(lmodel1)


lmodel1 <- lm(SoilGWC ~ Slope+Aspect+Elevation*Site+Species, data = masterdata)
lmodel1 <- lm(SoilGWC ~ Slope+Aspect+Elevation*Site+Species, data = masterdata)
lmodel1 <- lm(SoilGWC ~ Slope+Aspect+Elevation*Site+Species, data = masterdata)

anova(lmodel2)
summary(lmodel2)

anova(lmodel2, lmodel3)
anova(lmem)
summary(lmem)

?predict()
predict()

