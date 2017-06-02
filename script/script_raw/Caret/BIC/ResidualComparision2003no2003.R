## Comparision of Residuals for 20003 ##

## Description ##
'
Here I take the resiudals of the modelling with 2003 and compare it to predicted residuals for modelling without the year 2003. 
First, I do this for each month (May to October). Than, I try to generate a plot where all the month are integrated.
'

## Dependencies
' In den automatisierten Skripten der jeweiligen Monate werde die Modelle berechnet, welche jeweil die Daten liefern.'

## Libraries #
library(plm)
library(boot)
library(gtools)
library(lme4)
library(lmtest)
library(car)
library(sp)
library(rgdal)
library(raster)
library(rasterVis)
library(maptools)
library(reshape)
library(stringr)
library(classInt)
library(RColorBrewer)
library(stargazer)
library(ggplot2)
library(ggthemes)

###################################################################################################################################################################

#########
## May ##

## Load Residuals of the year 2003 for normal modell ##
Yield_Covariates_SM_May_Resid_only2003 <- read.csv( file = "./figures/figures_exploratory/BIC/Silomaize/May/Yield_Covariates_SM_May_Resid_only2003.csv")
Yield_Covariates_SM_May_Resid_only2003$X <- NULL
head(Yield_Covariates_SM_May_Resid_only2003)

## Load residuals for the model neglecting 2003 predicted on the year 2003
resid2003_May_no2003 <- read.csv(file="./figures//figures_exploratory/BIC/Silomaize/May_no2003/resid2003_May_no2003.csv")
resid2003_May_no2003$X  <- NULL
head(resid2003_May_no2003)

par(mfrow=c(1,2))
hist(Yield_Covariates_SM_May_Resid_only2003$Residual_only2003, freq=FALSE, main="Distribution of Residuals of 2003")
hist(resid2003_May_no2003$resid_May_no2003_predicted2003, freq=FALSE, main="Distribution of Residuals of 2003 predicted from no2003 Model")

## Compare residuals 
pdf(".//figures/figures_exploratory/Residuals/Silomaize/May_Comp_2003.pdf", width=12, height=3)
par(mfrow=c(1,2))
hist(Yield_Covariates_SM_May_Resid_only2003$Residual_only2003, freq=FALSE, main="Distribution of Residuals of 2003")
hist(resid2003_May_no2003$resid_May_no2003_predicted2003, freq=FALSE, main="Distribution of Residuals of 2003 predicted from no2003 Model")
dev.off()


#########
## Jun ##

## Load Residuals of the year 2003 for normal modell ##
Yield_Covariates_SM_Jun_Resid_only2003 <- read.csv( file = "./figures/figures_exploratory/BIC/Silomaize/Jun/Yield_Covariates_SM_Jun_Resid_only2003.csv")
Yield_Covariates_SM_Jun_Resid_only2003$X <- NULL
head(Yield_Covariates_SM_Jun_Resid_only2003)

## Load residuals for the model neglecting 2003 predicted on the year 2003
resid2003_Jun_no2003 <- read.csv(file="./figures//figures_exploratory/BIC/Silomaize/Jun_no2003/resid2003_Jun_no2003.csv")
resid2003_Jun_no2003$X  <- NULL
head(resid2003_Jun_no2003)


## Compare residuals 
par(mfrow=c(1,2))
hist(Yield_Covariates_SM_Jun_Resid_only2003$Residual_only2003, freq=FALSE, main="Distribution of Residuals of 2003")
hist(resid2003_Jun_no2003$resid_Jun_no2003_predicted2003, freq=FALSE, main="Distribution of Residuals of 2003 predicted from no2003 Model")

## Plot residuals in pdf
pdf(".//figures/figures_exploratory/Residuals/Silomaize/Jun_Comp_2003.pdf", width=12, height=3)
par(mfrow=c(1,2))
hist(Yield_Covariates_SM_Jun_Resid_only2003$Residual_only2003, freq=FALSE, main="Distribution of Residuals of 2003")
hist(resid2003_Jun_no2003$resid_Jun_no2003_predicted2003, freq=FALSE, main="Distribution of Residuals of 2003 predicted from no2003 Model")
dev.off()


#########
## Jul ##

## Load Residuals of the year 2003 for normal modell ##
Yield_Covariates_SM_Jul_Resid_only2003 <- read.csv( file = "./figures/figures_exploratory/BIC/Silomaize/Jul/Yield_Covariates_SM_Jul_Resid_only2003.csv")
Yield_Covariates_SM_Jul_Resid_only2003$X <- NULL
head(Yield_Covariates_SM_Jul_Resid_only2003)

## Load residuals for the model neglecting 2003 predicted on the year 2003
resid2003_Jul_no2003 <- read.csv(file="./figures//figures_exploratory/BIC/Silomaize/Jul_no2003/resid2003_Jul_no2003.csv")
resid2003_Jul_no2003$X  <- NULL
head(resid2003_Jul_no2003)


## Compare residuals 
par(mfrow=c(1,2))
hist(Yield_Covariates_SM_Jul_Resid_only2003$Residual_only2003, freq=FALSE, main="Distribution of Residuals of 2003")
hist(resid2003_Jul_no2003$resid_Jul_no2003_predicted2003, freq=FALSE, main="Distribution of Residuals of 2003 predicted from no2003 Model")

## Plot residuals in pdf
pdf(".//figures/figures_exploratory/Residuals/Silomaize/Jul_Comp_2003.pdf", width=12, height=3)
par(mfrow=c(1,2))
hist(Yield_Covariates_SM_Jul_Resid_only2003$Residual_only2003, freq=FALSE, main="Distribution of Residuals of 2003")
hist(resid2003_Jul_no2003$resid_Jul_no2003_predicted2003, freq=FALSE, main="Distribution of Residuals of 2003 predicted from no2003 Model")
dev.off()


#########
## Aug ##

## Load Residuals of the year 2003 for normal modell ##
Yield_Covariates_SM_Aug_Resid_only2003 <- read.csv( file = "./figures/figures_exploratory/BIC/Silomaize/Aug/Yield_Covariates_SM_Aug_Resid_only2003.csv")
Yield_Covariates_SM_Aug_Resid_only2003$X <- NULL
head(Yield_Covariates_SM_Aug_Resid_only2003)

## Load residuals for the model neglecting 2003 predicted on the year 2003
resid2003_Aug_no2003 <- read.csv(file="./figures//figures_exploratory/BIC/Silomaize/Aug_no2003/resid2003_Aug_no2003.csv")
resid2003_Aug_no2003$X  <- NULL
head(resid2003_Aug_no2003)


## Compare residuals 
par(mfrow=c(1,2))
hist(Yield_Covariates_SM_Aug_Resid_only2003$Residual_only2003, freq=FALSE, main="Distribution of Residuals of 2003")
hist(resid2003_Aug_no2003$resid_Aug_no2003_predicted2003, freq=FALSE, main="Distribution of Residuals of 2003 predicted from no2003 Model")

## Plot residuals in pdf
pdf(".//figures/figures_exploratory/Residuals/Silomaize/Aug_Comp_2003.pdf", width=12, height=3)
par(mfrow=c(1,2))
hist(Yield_Covariates_SM_Aug_Resid_only2003$Residual_only2003, freq=FALSE, main="Distribution of Residuals of 2003")
hist(resid2003_Aug_no2003$resid_Aug_no2003_predicted2003, freq=FALSE, main="Distribution of Residuals of 2003 predicted from no2003 Model")
dev.off()


#########
## Sep ##

## Load Residuals of the year 2003 for normal modell ##
Yield_Covariates_SM_Sep_Resid_only2003 <- read.csv( file = "./figures/figures_exploratory/BIC/Silomaize/Sep/Yield_Covariates_SM_Sep_Resid_only2003.csv")
Yield_Covariates_SM_Sep_Resid_only2003$X <- NULL
head(Yield_Covariates_SM_Sep_Resid_only2003)

## Load residuals for the model neglecting 2003 predicted on the year 2003
resid2003_Sep_no2003 <- read.csv(file="./figures//figures_exploratory/BIC/Silomaize/Sep_no2003/resid2003_Sep_no2003.csv")
resid2003_Sep_no2003$X  <- NULL
head(resid2003_Sep_no2003)


## Compare residuals 
par(mfrow=c(1,2))
hist(Yield_Covariates_SM_Sep_Resid_only2003$Residual_only2003, freq=FALSE, main="Distribution of Residuals of 2003")
hist(resid2003_Sep_no2003$resid_Sep_no2003_predicted2003, freq=FALSE, main="Distribution of Residuals of 2003 predicted from no2003 Model")


## Plot residuals in pdf
pdf(".//figures/figures_exploratory/Residuals/Silomaize/Sep_Comp_2003.pdf", width=12, height=3)
par(mfrow=c(1,2))
hist(Yield_Covariates_SM_Sep_Resid_only2003$Residual_only2003, freq=FALSE, main="Distribution of Residuals of 2003")
hist(resid2003_Sep_no2003$resid_Sep_no2003_predicted2003, freq=FALSE, main="Distribution of Residuals of 2003 predicted from no2003 Model")
dev.off()

#########
## Oct ##

## Load Residuals of the year 2003 for normal modell ##
Yield_Covariates_SM_Oct_Resid_only2003 <- read.csv( file = "./figures/figures_exploratory/BIC/Silomaize/Oct/Yield_Covariates_SM_Oct_Resid_only2003.csv")
Yield_Covariates_SM_Oct_Resid_only2003$X <- NULL
head(Yield_Covariates_SM_Oct_Resid_only2003)

## Load residuals for the model neglecting 2003 predicted on the year 2003
resid2003_Oct_no2003 <- read.csv(file="./figures//figures_exploratory/BIC/Silomaize/Oct_no2003/resid2003_Oct_no2003.csv")
resid2003_Oct_no2003$X  <- NULL
head(resid2003_Oct_no2003)


## Compare residuals 
par(mfrow=c(1,2))
hist(Yield_Covariates_SM_Oct_Resid_only2003$Residual_only2003, freq=FALSE, main="Distribution of Residuals of 2003")
hist(resid2003_Oct_no2003$resid_Oct_no2003_predicted2003, freq=FALSE, main="Distribution of Residuals of 2003 predicted from no2003 Model")

## Plot residuals in pdf
pdf(".//figures/figures_exploratory/Residuals/Silomaize/Oct_Comp_2003.pdf", width=12, height=3)
par(mfrow=c(1,2))
hist(Yield_Covariates_SM_Oct_Resid_only2003$Residual_only2003, freq=FALSE, main="Distribution of Residuals of 2003")
hist(resid2003_Oct_no2003$resid_Oct_no2003_predicted2003, freq=FALSE, main="Distribution of Residuals of 2003 predicted from no2003 Model")
dev.off()

############
## Season ##

## Make a plot of the residuals over the entire season
Yield_Covariates_SM_seasonMayOct_Resid_only2003 <- rbind(Yield_Covariates_SM_May_Resid_only2003,Yield_Covariates_SM_Jun_Resid_only2003,Yield_Covariates_SM_Jul_Resid_only2003,Yield_Covariates_SM_Aug_Resid_only2003,Yield_Covariates_SM_Sep_Resid_only2003,Yield_Covariates_SM_Oct_Resid_only2003)
colnames(resid2003_May_no2003) <- c("comId", "year",  "com","resid_no2003_predicted2003")
colnames(resid2003_Jun_no2003) <- c("comId", "year",  "com","resid_no2003_predicted2003")
colnames(resid2003_Jul_no2003) <- c("comId", "year",  "com","resid_no2003_predicted2003")
colnames(resid2003_Aug_no2003) <- c("comId", "year",  "com","resid_no2003_predicted2003")
colnames(resid2003_Sep_no2003) <- c("comId", "year",  "com","resid_no2003_predicted2003")
colnames(resid2003_Oct_no2003) <- c("comId", "year",  "com","resid_no2003_predicted2003")

resid2003_seasonMayOct_no2003 <- rbind(resid2003_May_no2003,resid2003_Jun_no2003,resid2003_Jul_no2003,resid2003_Aug_no2003,resid2003_Sep_no2003,resid2003_Oct_no2003)
dim(resid2003_May_no2003)


## Plot residuals in pdf
pdf(".//figures/figures_exploratory/Residuals/Silomaize/Season_Comp_2003.pdf", width=12, height=3)
par(mfrow=c(1,2))
hist(Yield_Covariates_SM_seasonMayOct_Resid_only2003$Residual_only2003, freq=FALSE, main="Distribution of Residuals of the 2003 season")
hist(resid2003_seasonMayOct_no2003$resid_no2003_predicted2003, freq=FALSE, main="Distribution of Residuals of 2003 predicted from no2003 Model")
dev.off()

summary(Yield_Covariates_SM_seasonMayOct_Resid_only2003$Residual_only2003)
summary(resid2003_seasonMayOct_no2003$resid_no2003_predicted2003)
