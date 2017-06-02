############################
####  SiloMaize in Mai ####
############################
' Here with different stepwise function: 
Yield_Covariates_SM_Mai_full$SMI_GDM_Full<- cut(Yield_Covariates_SM_Mai_full$SMI, breaks = c(0, 0.1, 0.2, 0.3,0.4,0.49,0.51,0.6, 0.7, 0.8, 0.9, 1),                                            ,
                                       labels = c("severe drought","moderate drought","abnormally dry",
                                                  "dry", 
                                                  "normally dry", 
                                                  "normal",
                                                  "normally wet",
                                                  "wet",  
                                                  "abnormally wet" ,"abundantly wet", "severely wet"))
'

'
######################
## File Discription ##

The purpose of this script is to estimate the impact of weather fluctuations in the month mentionend above on yearly crop yield.

This is done by the following the steps:
- Create data frame with siloMaize as dependent and variables of the month above as independent variables
- Create stepwise function which is based on drought categories of german drought monitor
- Remove comIds with less than 7 observations to avoid leveage issues
- Remove log trend of indepedent variable
- Delete outliers which appear to be measurement error
- Use BIC to choose the degrees of the polynomial and to compare various model configurations
- Loop through polynomials configuration of each model; highest possible polynomial is of degree 3
- Compare models graphically
- Explore Models
  - Model with lowest BIC in general: Pet, Prec
  - Model with lowest BIC of standard configuration: Tavg, Prec, SMI
  - Model with lowest BIC with SMI: Pet, Prec, SMI

- Correct Standard Errors with either Driscoll Kray or Cameron et al /Thompson estimator

      The --vcovHC– function estimates three heteroskedasticity-consistent covariance estimators:
          • "white1" - for general heteroskedasticity but no serial correlation. Recommended for random effects.
          • "white2" - is "white1" restricted to a common variance within groups. Recommended for random effects.
         • "arellano" - both heteroskedasticity and serial correlation. Recommended for fixed effects.
      The following options apply*:
          • HC0 - heteroskedasticity consistent. The default.
          • HC1,HC2, HC3 – Recommended for small samples. HC3 gives less weight to influential
            observations.
          • HC4 - small samples with influential observations
          • HAC - heteroskedasticity and autocorrelation consistent (type ?vcovHAC for more
                                                                 details)
      Solution for serial correlation: Cluster by groups.
      Solution for cross sectional correlation: Cluster by time
      
      Ich arbeitet vorerst mir Driscoll Kraay und weighting von 1 (maxlag=0). Die Ergebnisse sollten solide sein, da Cameron/Thompson ähnlich ist



## Input ##

- aus 4km_tmax: Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin_Tmax_nodemean_nozscore_sm.csv (komplete data.frame)


## Output ##
- Yield_Covariates_SM_Mai_full.csv (auf Mai reduzierter Data.Frame)
- Export Data frame for use in BIC_Graphic: file="./data/data_raw/BIC/BIC_SM_Mai_full.csv")

- Export Data Frame of Fixed Effects to be used in Script FixedEffects_Graphic: 
  "./figures/figures_exploratory/FixedEffects/Silomaize/Mai/..."
'

###################
## Load Packages ##
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
####################################################################################################################################################################

#################################################################################################################
#### Create data frame with siloMaize as dependent and variables of the month above as independent variables ####
#################################################################################################################

## Read in large Dataframe for Maize ##
Yield_Covariates <- read.csv("~/Documents/projects/correlation/data/data_processed/Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin_Tmax_nodemean_nozscore_sm.csv")
Yield_Covariates$X <- NULL


## For publication worth regression output need to change data names ##
'Get rid of variables which are not necessary: other months and other not needed variables'
names(Yield_Covariates)
names <- names(Yield_Covariates)
names_Mai_full <- grep(c("*_Mai"), names)
names_Mai_full
Yield_Covariates_Mai_full <- Yield_Covariates[,names_Mai_full]
names(Yield_Covariates_Mai_full)
dim(Yield_Covariates_Mai_full)

## Delete all but SMI, Prec, Tavg and Pet
names(Yield_Covariates_Mai_full)
Yield_Covariates_Mai_full <- Yield_Covariates_Mai_full[,c(1:4)]

## Establish first part of data frame_ time and spatial reference plus Silomaize ##
names(Yield_Covariates[,c(2,1,3:5,7)])
Yield_Covariates_SM <- Yield_Covariates[,c(2,1,3:5,7)] # Achtung, darauf achten, dass comId und year in der richtigen Reihenfolge sind.
names(Yield_Covariates_SM)
head(Yield_Covariates_SM)

Yield_Covariates_SM_Mai_full <- cbind(Yield_Covariates_SM, Yield_Covariates_Mai_full)
names(Yield_Covariates_SM_Mai_full)
names(Yield_Covariates_SM_Mai_full) <- c( "comId" , "year","com","stateId","state","siloMaize","SMI", "Prec","Tavg", "Pet")
names(Yield_Covariates_SM_Mai_full)

#########################################
#### Create stepwise function of SMI ####
#########################################
' Drought Monitor Spezification - FULL'

Yield_Covariates_SM_Mai_full$SMI_GDM_Full <- cut(Yield_Covariates_SM_Mai_full$SMI, breaks = c(0, 0.1, 0.2, 0.3,0.4,0.49,0.51,0.6, 0.7, 0.8, 0.9, 1),                                            ,
                                                 labels = c("severe drought","moderate drought","abnormally dry",
                                                            "dry", 
                                                            "normally dry", 
                                                            "normal",
                                                            "normally wet",
                                                            "wet",  
                                                            "abnormally wet" ,"abundantly wet", "severely wet"))
# Yield_Covariates_SM_Mai_full$SMI_GDM_Full <- cut(Yield_Covariates_SM_Mai_full$SMI, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1),                                            ,
#                                        labels = c("severe drought","moderate drought","abnormal dry", "normal","abnormal wet" ,"abundant wet", "severe wet"))
head(Yield_Covariates_SM_Mai_full$SMI_GDM_Full )
table(Yield_Covariates_SM_Mai_full$SMI_GDM_Full )

################
## Delete NAs ##
################
sum(is.na(Yield_Covariates_SM_Mai_full) )
dim(Yield_Covariates_SM_Mai_full)
Yield_Covariates_SM_Mai_full_nna <- na.omit(Yield_Covariates_SM_Mai_full) 
dim(Yield_Covariates_SM_Mai_full_nna)

## Check for NAs
any(is.na(Yield_Covariates_SM_Mai_full_nna))
## Reset Rownames
rownames(Yield_Covariates_SM_Mai_full_nna) <- NULL

## Further work with DataFrame without Yield_Covariates_SM_Mai_full index ##
Yield_Covariates_SM_Mai_full <- Yield_Covariates_SM_Mai_full_nna

#########################################################################
## Remove comIds with less than 7 observations to avoid leveage issues ## 
#########################################################################

#####################################################
## Delete all comIds with less than 7 observations ##
sum(table(Yield_Covariates_SM_Mai_full$comId) < 7 )
table(Yield_Covariates_SM_Mai_full$comId) < 7 

## comIds mit weniger als 7 Beoachtungen: ##
list <- c(3101, 3102, 3158, 3402, 5114, 5117, 5314,5315, 5334,5378, 5512, 5911, 5916, 6413, 7131, 7135, 7233, 7331, 7332, 7337, 7338, 7339, 8111,12052, 14612, 15001, 15082, 15083, 15084, 15085, 15086, 15087, 15088, 15089, 15091, 16051, 16052 )
length(list)
list[[1]]

temp <- Yield_Covariates_SM_Mai_full
for (i in 1:34)
{
  print(Yield_Covariates_SM_Mai_full[Yield_Covariates_SM_Mai_full$comId==list[i],])
  temp <- (temp[!temp$comId==list[i],])
}

## Number of deleted rows ##
dim(temp) - dim(Yield_Covariates_SM_Mai_full)

## Further use old name for data.frame
Yield_Covariates_SM_Mai_full <- temp

################################
## Befehle nach jedem löschen ##
Yield_Covariates_SM_Mai_full <- na.omit(Yield_Covariates_SM_Mai_full) 
rownames(Yield_Covariates_SM_Mai_full) <- NULL
Yield_Covariates_SM_Mai_full <- plm.data(Yield_Covariates_SM_Mai_full, index=c("comId", "year"))
Yield_Covariates_SM_Mai_full[,c("comId","stateId")] <- lapply(Yield_Covariates_SM_Mai_full[,c("comId","stateId")], factor )

#################################################
#### Remove log trend of indepedent variable ####
#################################################
'Fit log of yield on log of time and use the residuals of that for yields'
logtrend <- lm(log(siloMaize) ~ log(as.integer(year)), data= Yield_Covariates_SM_Mai_full)

##########################
## Issue with Outliers ###
##########################
par(mfrow = c(2,2))
plot(logtrend) 

## Look at Outliers Values ##
Yield_Covariates_SM_Mai_full[c(1276, 3262, 3283, 3171,3255),]

## Look at other values of outliers com #
Yield_Covariates_SM_Mai_full[Yield_Covariates_SM_Mai_full$comId == "6532",] # 2008 hier scheint nur ein Jahr ein Messfehler zu sein: diesen Lösche ich 
Yield_Covariates_SM_Mai_full[Yield_Covariates_SM_Mai_full$comId == "12067",] # 2006 verändere ich nicht 
Yield_Covariates_SM_Mai_full[Yield_Covariates_SM_Mai_full$comId == "12069",] # 2003 verändere ich nicht 
Yield_Covariates_SM_Mai_full[Yield_Covariates_SM_Mai_full$comId == "12060",] # 1999 verändere ich nicht 
Yield_Covariates_SM_Mai_full[Yield_Covariates_SM_Mai_full$comId == "12067",] # 2006 verändere ich nicht 


## Delete outliers ##
' Da sich die Patterns bei den BIC Vergleichen nicht ändert, kümmere ich micht nicht weiter um die Outlier. 
Ich nehme nur sehr offensichtliche Messfehler raus.'
Yield_Covariates_SM_Mai_full <- Yield_Covariates_SM_Mai_full[!(Yield_Covariates_SM_Mai_full$comId == "6532" & Yield_Covariates_SM_Mai_full$year == "2008"),]

Yield_Covariates_SM_Mai_full <- na.omit(Yield_Covariates_SM_Mai_full)
rownames(Yield_Covariates_SM_Mai_full) <- NULL


#################################################
#### Remove log trend of indepedent variable ####
logtrend <- lm(log(siloMaize) ~ log(as.integer(year)), data= Yield_Covariates_SM_Mai_full)
summary(logtrend)
Yield_Covariates_SM_Mai_full$siloMaize_logtrend <- resid(logtrend)

#######################################
## Prepare dataframe for plm package ##
'Change Indexing so that it can be used in plm package'
Yield_Covariates_SM_Mai_full <- plm.data(Yield_Covariates_SM_Mai_full, index=c("comId", "year"))
str(Yield_Covariates_SM_Mai_full)

###########################################
## Transform comId and stateId to factor ##
Yield_Covariates_SM_Mai_full[,c("comId","stateId")] <- lapply(Yield_Covariates_SM_Mai_full[,c("comId","stateId")], factor )
lapply(Yield_Covariates_SM_Mai_full, class)


###################################################
##### Save Yield_Covariates_SM_Mai_full extern ####
write.csv(Yield_Covariates_SM_Mai_full, file="./data/data_raw/Yield_Covariates_SM_Mai_full.csv")

#######################################################
#### BIC to choose the degrees of the polynomials  ####
#######################################################
## create a matrix which contains all possible degree combinations, here for three variables ##
degree <- permutations(n=3,r=2,v=c(1:3),repeats.allowed=T)
degree

################################################
## Formulas for Model Variations to be tested ##

## with SMI
formula_Mai_full_sm_detrendlog_SMIPrecTavg <- siloMaize_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  poly(Tavg, degree[r, 2], raw = T) + 
  dummy(SMI_GDM_Full,c("severe drought","moderate drought","abnormally dry",
                       "dry", 
                       "normally dry", 
                       "normally wet",
                       "wet",  
                       "abnormally wet" ,"abundantly wet", "severely wet")) + dummy(comId)

formula_Mai_full_sm_detrendlog_SMIPrecPet <- siloMaize_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  poly(Pet, degree[r, 2], raw = T) + 
  dummy(SMI_GDM_Full,c("severe drought","moderate drought","abnormally dry",
                       "dry", 
                       "normally dry", 
                       "normally wet",
                       "wet",  
                       "abnormally wet" ,"abundantly wet", "severely wet")) + dummy(comId)

formula_Mai_full_sm_detrendlog_SMIPrec <- siloMaize_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  
  dummy(SMI_GDM_Full,c("severe drought","moderate drought","abnormally dry",
                       "dry", 
                       "normally dry", 
                       "normally wet",
                       "wet",  
                       "abnormally wet" ,"abundantly wet", "severely wet")) + dummy(comId)

formula_Mai_full_sm_detrendlog_SMIPet <- siloMaize_logtrend ~ poly(Pet, degree[r, 2], raw = T) +  
  dummy(SMI_GDM_Full,c("severe drought","moderate drought","abnormally dry",
                       "dry", 
                       "normally dry", 
                       "normally wet",
                       "wet",  
                       "abnormally wet" ,"abundantly wet", "severely wet")) + dummy(comId)

formula_Mai_full_sm_detrendlog_SMITavg <- siloMaize_logtrend ~ poly(Tavg, degree[r, 2], raw = T) + 
  dummy(SMI_GDM_Full,c("severe drought","moderate drought","abnormally dry",
                       "dry", 
                       "normally dry", 
                       "normally wet",
                       "wet",  
                       "abnormally wet" ,"abundantly wet", "severely wet")) + dummy(comId)

formula_Mai_full_sm_detrendlog_SMI <- siloMaize_logtrend ~  
  dummy(SMI_GDM_Full,c("severe drought","moderate drought","abnormally dry",
                       "dry", 
                       "normally dry", 
                       "normally wet",
                       "wet",  
                       "abnormally wet" ,"abundantly wet", "severely wet")) + dummy(comId)

## no SMI
formula_Mai_full_sm_detrendlog_PrecTavg <- siloMaize_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  poly(Tavg, degree[r, 2], raw = T) + dummy(comId)

formula_Mai_full_sm_detrendlog_PrecPet <- siloMaize_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  poly(Pet, degree[r, 2], raw = T) + dummy(comId)

formula_Mai_full_sm_detrendlog_Prec <- siloMaize_logtrend ~ poly(Prec, degree[r, 1], raw = T) + dummy(comId)

formula_Mai_full_sm_detrendlog_Pet <- siloMaize_logtrend ~ poly(Pet, degree[r, 2], raw = T)  + dummy(comId)

formula_Mai_full_sm_detrendlog_Tavg <- siloMaize_logtrend ~ poly(Tavg, degree[r, 2], raw = T)  + dummy(comId)

## Generate list of formula
formula_list_Mai_full_sm_detrendlog <- c(
formula_Mai_full_sm_detrendlog_SMIPrecTavg,
formula_Mai_full_sm_detrendlog_SMIPrecPet,
formula_Mai_full_sm_detrendlog_SMIPrec,
formula_Mai_full_sm_detrendlog_SMIPet,
formula_Mai_full_sm_detrendlog_SMITavg,
formula_Mai_full_sm_detrendlog_SMI,
formula_Mai_full_sm_detrendlog_Prec,
formula_Mai_full_sm_detrendlog_Tavg,
formula_Mai_full_sm_detrendlog_Pet ,
formula_Mai_full_sm_detrendlog_PrecTavg,
formula_Mai_full_sm_detrendlog_PrecPet


)
formula_list_Mai_full_sm_detrendlog[[11]]
#################################################################################################
# Loop through the container list to cover all permutations of posssible degree of freedoms of ##
# of the polynomials of the variables                                                          ##
#################################################################################################

##################################################
## Loop through various variable configurations ##
BIC_SMIPrecTavg <- rep(0,9)
for(r in 1:9){
  glm.fit_SMIPrecTavg  <- glm(formula = formula_Mai_full_sm_detrendlog_SMIPrecTavg, data = Yield_Covariates_SM_Mai_full) 
  BIC_SMIPrecTavg[r] <- BIC(glm.fit_SMIPrecTavg)
}

BIC_SMIPrecPet <- rep(0,9)
for(r in 1:9){
  glm.fit_SMIPrecPet  <- glm(formula = formula_Mai_full_sm_detrendlog_SMIPrecPet,  data = Yield_Covariates_SM_Mai_full)
  BIC_SMIPrecPet[r] <- BIC(glm.fit_SMIPrecPet)
}

BIC_SMIPrec <- rep(0,9)
for(r in 1:9){  
  glm.fit_SMIPrec      <- glm(formula = formula_Mai_full_sm_detrendlog_SMIPrec,     data = Yield_Covariates_SM_Mai_full) 
  BIC_SMIPrec[r] <- BIC(glm.fit_SMIPrec)
}

BIC_SMIPet <- rep(0,9)
for(r in 1:9){  
  glm.fit_SMIPet       <- glm(formula = formula_Mai_full_sm_detrendlog_SMIPet,      data = Yield_Covariates_SM_Mai_full) 
  BIC_SMIPet[r] <- BIC(glm.fit_SMIPet)
}

BIC_SMITavg <- rep(0,9)
for(r in 1:9){  
  glm.fit_SMITavg      <- glm(formula = formula_Mai_full_sm_detrendlog_SMITavg,     data = Yield_Covariates_SM_Mai_full) 
  BIC_SMITavg[r] <- BIC(glm.fit_SMITavg)
}

BIC_SMI <- rep(0,9)
for(r in 1:9){  
  glm.fit_SMI      <- glm(formula = formula_Mai_full_sm_detrendlog_SMI,     data = Yield_Covariates_SM_Mai_full) 
  BIC_SMI[r] <- BIC(glm.fit_SMI)
}

BIC_PrecTavg <- rep(0,9)
for(r in 1:9){
  glm.fit_PrecTavg  <- glm(formula = formula_Mai_full_sm_detrendlog_PrecTavg, data = Yield_Covariates_SM_Mai_full) 
  BIC_PrecTavg[r] <- BIC(glm.fit_PrecTavg)
}

BIC_PrecPet <- rep(0,9)
for(r in 1:9){
  glm.fit_PrecPet  <- glm(formula = formula_Mai_full_sm_detrendlog_PrecPet, data = Yield_Covariates_SM_Mai_full) 
  BIC_PrecPet[r] <- BIC(glm.fit_PrecPet)
}

BIC_Prec <- rep(0,9)
for(r in 1:9){
  glm.fit_Prec  <- glm(formula = formula_Mai_full_sm_detrendlog_Prec, data = Yield_Covariates_SM_Mai_full) 
  BIC_Prec[r] <- BIC(glm.fit_Prec)
}

BIC_Pet  <- rep(0,9)
for(r in 1:9){
  glm.fit_Pet   <- glm(formula = formula_Mai_full_sm_detrendlog_Pet , data = Yield_Covariates_SM_Mai_full) 
  BIC_Pet [r] <- BIC(glm.fit_Pet )
}

BIC_Tavg  <- rep(0,9)
for(r in 1:9){
  glm.fit_Tavg   <- glm(formula = formula_Mai_full_sm_detrendlog_Tavg , data = Yield_Covariates_SM_Mai_full) 
  BIC_Tavg [r] <- BIC(glm.fit_Tavg )
}

## Compare BIC values ##
BIC <- c(BIC_SMIPrecTavg, BIC_SMIPrecPet, BIC_SMIPrec, BIC_SMIPet, BIC_SMITavg, BIC_SMI, BIC_Prec, BIC_Tavg, BIC_Pet, BIC_PrecTavg, BIC_PrecPet)
BIC
par(mfrow=c(1,1))
plot(BIC)

###########################
## Plot BIC with ggplot2 ##
###########################


##############################################
## Create Dataframe for plotting in ggplot2 ##

## repeat name of modelconfiguration ##
list <-c("01_SMIPrecTavg", "02_SMIPrecPet", "03_SMIPrec", "04_SMIPet",
         "05_SMITavg", "06_SMI", "07_Prec", "08_Tavg", "09_Pet", "10_PrecTavg", "11_PrecPet")
list2 <- 1:11

model <- NULL
model_index <- NULL

for (i in 1:11)
{
  x <- rep(list[i],9)
  y <- rep(list2[i],9)
  model <- append(model, x)
  model_index <- as.numeric(append(model_index, y))
}


###################################
## Combine data in on data.frame ##
BIC <- as.data.frame(BIC)
model <- as.data.frame(model)
model_index <- as.data.frame(model_index)
index <- 1:99
month <-rep("Mai",99)

BIC_Mai_full <- cbind(BIC, model ,model_index, index, month)

#######################
## Delete Duplicates ##
which(duplicated(BIC_Mai_full$BIC))
list3 <- c(20,21,23,24,26,27,31,32,33,34,35,36,40,41,42,43,44,45,47,48,49,50,51,52,53,54,56,57,59,60,62,63,67,68,69,70,71,72,76,77,78,79,80,81)
length(list3)
temp <- BIC_Mai_full

for (i in 1:44)
{
  print(BIC_Mai_full[BIC_Mai_full$index ==list3[i],])
  temp <- (temp[!temp$index==list3[i],])
}
dim(BIC_Mai_full)
dim(temp)

################################
## Correct created data.frame ##
rownames(temp) <- NULL
BIC_Mai_full <- temp
lapply(BIC_Mai_full, class)

############################
## Plot data with ggplot2 ##
g <- ggplot(BIC_Mai_full,aes(y=BIC, x=index))
g_save <- g + geom_point(aes(color=model)) + labs(title="BIC_SM_Mai_full", x="") + 
  theme(plot.title=element_text(size=15, face="bold")) + theme_few() +
  facet_wrap( ~ month)
g_save 
ggsave( "./figures/figures_exploratory/BIC/Silomaize/May/BIC_SM_Mai_full.png", width= 8, height=8, dpi=40 )


## Export Data frame for use in BIC_Grafic
BIC_SM_Mai_full <- BIC_Mai_full
class(BIC_SM_Mai_full)
write.csv(BIC_SM_Mai_full, file="./data/data_raw/BIC/BIC_SM_Mai_full.csv")
which.min(BIC_SM_Mai_full$BIC)

BIC_SM_Mai_full[which.min(BIC_SM_Mai_full$BIC),]

################################################################
################################### Explore Models #############
################################################################
###################
## Load Data Set ##
# Yield_Covariates_SM_Mai_full <- read.csv( file="./data/data_raw/Yield_Covariates_SM_Mai_full.csv")
# names(Yield_Covariates_SM_Mai_full)
# Yield_Covariates_SM_Mai_full$X <- NULL

#######################################
## Prepare dataframe for plm package ##
'Change Indexing so that it can be used in plm package'
Yield_Covariates_SM_Mai_full <- plm.data(Yield_Covariates_SM_Mai_full, index=c("comId", "year"))

## Transform comId and stateId to factor ##
Yield_Covariates_SM_Mai_full[,c("comId","stateId")] <- lapply(Yield_Covariates_SM_Mai_full[,c("comId","stateId")], factor )
str(Yield_Covariates_SM_Mai_full)

#################################
###############################
## Results with smallest BIC ##
###############################

BIC_SM_Mai_full[which.min(BIC_SM_Mai_full$BIC),]

##  Extract model index
BIC_SM_Mai_full[which.min(BIC_SM_Mai_full$BIC),][[3]]

## Extract model with lowest BIC of the models with that particular model index 
which.min(BIC_SM_Mai_full[BIC_SM_Mai_full$model_index==BIC_SM_Mai_full[which.min(BIC_SM_Mai_full$BIC),][[3]],][,1])

## Determine degree 
r = which.min(BIC_SM_Mai_full[BIC_SM_Mai_full$model_index==BIC_SM_Mai_full[which.min(BIC_SM_Mai_full$BIC),][[3]],][,1])

## Determine adequate formula for model by the means of the model index
best_formula <-  formula_list_Mai_full_sm_detrendlog[[BIC_SM_Mai_full[which.min(BIC_SM_Mai_full$BIC),][[3]]]]

###################
## GLM Ergebniss ##
glm.fit_SM_BEST_Mai_full  <- glm(formula = best_formula,  data = Yield_Covariates_SM_Mai_full)
best_glm_summary <- summary(glm.fit_SM_BEST_Mai_full)
best_glm_summary


capture.output(best_glm_summary, file = "./figures/figures_exploratory/BIC/Silomaize/May/Output_SM_Mai.txt")

best_glm_summary_qual <- print(c("best_glm_AIC:", round(best_glm_summary$aic,4)))
capture.output(best_glm_summary_qual, file = "./figures/figures_exploratory/BIC/Silomaize/May/Output_SM_Mai_fullqual.txt")

'AIC: -6494.3'

####################
## PLM Ergebnisse ##
plm.fit_SM_BEST_Mai_full <- plm(formula = update(best_formula, .~. - dummy(comId)),  data = Yield_Covariates_SM_Mai_full,  effect="individual", model=("within"), index = c("comId","year"))

## Output
best_plm_summary <- summary(plm.fit_SM_BEST_Mai_full)
best_plm_summary
capture.output(best_plm_summary, file = "./figures/figures_exploratory/BIC/Silomaize/May/Output_SM_Mai.txt", append=T)

best_plm_summary_qual <- print(c("best_plm:", round(best_plm_summary$r.squared,4)))
capture.output(best_plm_summary_qual, file = "./figures/figures_exploratory/BIC/Silomaize/May/Output_SM_Mai_fullqual.txt", append=T)

'Adj. R-Squared: 0.054164'

## Fixed Effects ##
fixef <-  fixef(plm.fit_SM_BEST_Mai_full)
fixef_summary <- as.matrix(summary(fixef))
fixef_summary[33,3]
capture.output(fixef_summary, file = "./figures/figures_exploratory/FixedEffects/Silomaize/plm.fit_SM_BEST_Mai_full_FE_summary.txt")
# fixef_summary <- read.table( file = "./figures/figures_exploratory/FixedEffects/Silomaize/plm.fit_SM_BEST_Mai_full_FE_summary.txt")


##################
## LM Ergebniss ##
lm.fit_SM_BEST_Mai_full  <-lm(formula = best_formula,  data = Yield_Covariates_SM_Mai_full)
best_lm_summary <- summary(lm.fit_SM_BEST_Mai_full) 
best_lm_summary$r.squared
capture.output(best_lm_summary, file = "./figures/figures_exploratory/BIC/Silomaize/May/Output_SM_Mai.txt", append=T)


best_lm_summary_qual1 <- print(c("best_lm_r.squared:", round(best_lm_summary$r.squared,4)))
best_lm_summary_qual2 <- print(c("best_lm_adj.r.squared:", round(best_lm_summary$adj.r.squared,4)))
capture.output(best_lm_summary_qual1, file = "./figures/figures_exploratory/BIC/Silomaize/May/Output_SM_Mai_fullqual.txt", append=T)
capture.output(best_lm_summary_qual2, file = "./figures/figures_exploratory/BIC/Silomaize/May/Output_SM_Mai_fullqual.txt", append=T)
## Output
'Adjusted R-squared:   0.6345  '

capture.output(best_lm_summary, file = "./figures/figures_exploratory/BIC/Silomaize/May/Output_SM_Mai.txt", append=T)

################################################
## Assessing Influence (Leverage*discrepancy) ##
cutoff_SM_Mai_full <- 4/((nrow(Yield_Covariates_SM_Mai_full)-length(lm.fit_SM_BEST_Mai_full$coefficients)-1)) 
cutoff_SM_Mai_full
plot(lm.fit_SM_BEST_Mai_full, which=4)
cook_Mai_full <- cooks.distance(lm.fit_SM_BEST_Mai_full)


nrow_cooks <- nrow(Yield_Covariates_SM_Mai_full[cook_Mai_full > cutoff_SM_Mai_full,]) # 195
capture.output(nrow_cooks, file = "./figures/figures_exploratory/BIC/Silomaize/May/best_SM_Mai_full_cooks.txt")

year_cooks_SM_Mai_full <- table(Yield_Covariates_SM_Mai_full$year[cook_Mai_full > cutoff_SM_Mai_full ]) 
year_cooks_SM_Mai_full
capture.output(year_cooks_SM_Mai_full, file = "./figures/figures_exploratory/BIC/Silomaize/May/best_SM_Mai_full_cooks.txt", append=T)

'1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 
   7    5    6    7   81    6    7   36   16    6    5   13
'

com_cooks_SM_Mai_full <- sort(table(Yield_Covariates_SM_Mai_full$com[cook_Mai_full > cutoff_SM_Mai_full ] ))
capture.output(com_cooks_SM_Mai_full, file = "./figures/figures_exploratory/BIC/Silomaize/May/best_SM_Mai_full_cooks.txt", append=T)
'Rh\xf6n-Grabfeld, Landkreis          Saarbr\xfccken, Regionalverband                   Werra-Mei\xdfner-Kreis                             Wesel, Kreis 
                                       2                                        2                                        2                                        2 
                   Wittenberg, Landkreis                  Hagen, Kreisfreie Stadt                    Oder-Spree, Landkreis            Potsdam-Mittelmark, Landkreis 
                                       2                                        3                                        3                                        3 
                      Stendal, Landkreis                     Uckermark, Landkreis                   Altmarkkreis Salzwedel                        Barnim, Landkreis 
                                       3                                        3                                        4                                        4 
              Dahme-Spreewald, Landkreis         Oberspreewald-Lausitz, Landkreis                Spree-Nei\xdfe, Landkreis                   Elbe-Elster, Landkreis 
                                       4                                        4                                        4                                        5 
            Teltow-Fl\xe4ming, Landkreis       Frankfurt (Oder), Kreisfreie Stadt                              Olpe, Kreis               Siegen-Wittgenstein, Kreis 
                                       5                                        6                                        6                                        7'



########################
## Heteroskedasdicity ##
bptest(glm.fit_SM_BEST_Mai_full) # Breusch Pagan Test of Heteroskedastie in den Störgrößen: Null: Homoskedasdicity.
bptest(plm.fit_SM_BEST_Mai_full)
' In beiden Fällen kann die Null widerlegt werden. Es gibt also heteroskedasdicity '

## Koenkers Version on BP Test: robuste Modification wenn die Störgrößen nicht normalverteilt sind. 
bptest(plm.fit_SM_BEST_Mai_full, studentize = TRUE)
'Auch hier kann die Null widerlegt werden. Need to use robust covariance variance matrix to correct standard errors'


######################################
## Tests for serial autocorrelation ##
pwartest(plm.fit_SM_BEST_Mai_full)
pbgtest(plm.fit_SM_BEST_Mai_full) 
'
both, H_1 of serial autocorrelation cannot be rejected
'

#################################
## Correct the Standard Errors ##
#################################

## Correct Standard Errors used in table ##
coeftest(plm.fit_SM_BEST_Mai_full)

## Robust covariance matrix estimators a la White ##
# coeftest(plm.fit_SM_BEST_Mai_full,vcov=vcovHC(plm.fit_SM_BEST_Mai_full,method = "arellano", type = "HC0")) 
cov0_SM_BEST_Mai_full <- vcovHC(plm.fit_SM_BEST_Mai_full,method = "arellano", type = "HC0", cluster="group")
Wh.se_serial_SM_BEST_Mai_full <- sqrt(diag(cov0_SM_BEST_Mai_full))

cov0.1_SM_BEST_Mai_full <- vcovHC(plm.fit_SM_BEST_Mai_full,method = "arellano", type = "HC0", cluster="time")
Wh.se_cross_SM_BEST_Mai_full <- sqrt(diag(cov0.1_SM_BEST_Mai_full))
# 
# ## Beck Katz:
# # coeftest(plm.fit_SM_BEST_Mai_full, vcov = function(x) vcovBK(plm.fit_SM_BEST_Mai_full,method = "arellano", type = "HC0"))
# cov1 <- vcovBK(plm.fit_SM_BEST_Mai_full,method = "arellano", type = "HC0",  cluster="time")
# BK.se <- sqrt(diag(cov1))

# ## Driscoll Kraay ##
# summary(plm.fit_SM_BEST_Mai_full)
coeftest(plm.fit_SM_BEST_Mai_full,  vcov=function(x) vcovSCC(plm.fit_SM_BEST_Mai_full,method = "arellano",type = "HC0")) 
cov2_SM_BEST_Mai_full     <- vcovSCC(plm.fit_SM_BEST_Mai_full,method = "arellano",type = "HC0")
DK.se_SM_BEST_Mai_full    <- sqrt(diag(cov2_SM_BEST_Mai_full))
# 
# cov2.1_SM_BEST_Mai_full   <- vcovSCC(plm.fit_SM_BEST_Mai_full,method = "arellano",type = "HC0", maxlag=1)
# DK2.1.se_SM_BEST_Mai_full <- sqrt(diag(cov2.1_SM_BEST_Mai_full))

# cov2.2_SM_BEST_Mai_full   <- vcovSCC(plm.fit_SM_BEST_Mai_full,method = "arellano",type = "HC0", maxlag=2)
# DK2.2.se_SM_BEST_Mai_full <- sqrt(diag(cov2.2_SM_BEST_Mai_full))
# 
# cov2.3_SM_BEST_Mai_full   <- vcovSCC(plm.fit_SM_BEST_Mai_full,method = "arellano",type = "HC0", maxlag=3)
# DK2.3.se_SM_BEST_Mai_full <- sqrt(diag(cov2.3_SM_BEST_Mai_full))
# 
# cov2.4_SM_BEST_Mai_full   <- vcovSCC(plm.fit_SM_BEST_Mai_full,method = "arellano",type = "HC0", maxlag=4)
# DK2.4.se_SM_BEST_Mai_full <- sqrt(diag(cov2.4_SM_BEST_Mai_full))
# 
cov2.5_SM_BEST_Mai_full   <- vcovSCC(plm.fit_SM_BEST_Mai_full,method = "arellano",type = "HC0", maxlag=5)
DK2.5.se_SM_BEST_Mai_full <- sqrt(diag(cov2.5_SM_BEST_Mai_full))

## Cameron et al /Thompson : doouble-clustering estimator  ##
# coeftest(plm.fit_SM_BEST_Mai_full, vcovDC(plm.fit_SM_BEST_Mai_full, method = "arellano", type = "HC0"))
cov3_SM_BEST_Mai_full <- vcovDC(plm.fit_SM_BEST_Mai_full, method = "arellano", type = "HC0")
CT.se_SM_BEST_Mai_full <- sqrt(diag(cov3_SM_BEST_Mai_full))
'Our estimator is qualitatively similar to the ones presented in White and Domowitz (1984), for
time series data, and Conley (1999), for spatial data. '

## Generate Table with Output ##
se <- list(NULL, Wh.se_cross_SM_BEST_Mai_full, Wh.se_serial_SM_BEST_Mai_full,  DK.se_SM_BEST_Mai_full, DK2.5.se_SM_BEST_Mai_full, CT.se_SM_BEST_Mai_full)
labels1 <-c("NULL","WhiteCross","WhiteSerial", "DriscollKraay", "DriscollKray2.5","CameronThompson")
stargazer(plm.fit_SM_BEST_Mai_full, plm.fit_SM_BEST_Mai_full, plm.fit_SM_BEST_Mai_full, plm.fit_SM_BEST_Mai_full, plm.fit_SM_BEST_Mai_full,plm.fit_SM_BEST_Mai_full,
          se = se,   
          dep.var.caption  = "Model with smallest BIC - Mai",
          dep.var.labels = "log(Silomaize)",
          style="default",
          model.numbers = FALSE,
          column.labels = labels1,          
          type="text", out="./figures/figures_exploratory/BIC/Silomaize/May/SM_Mai_full_best.txt"
)


#########################################################
## Results with smallest BIC of Standard Configuration ##
#########################################################

# Look for smallest BIC of Standar Config
BIC_SM_Mai_full[which.min(BIC_SM_Mai_full$BIC[1:9]),]

# Extract smallest BIC of Standard Config
BIC_SM_Mai_full[which.min(BIC_SM_Mai_full$BIC[1:9]),][[4]]

plot(BIC_SMIPrecTavg)
which.min(BIC_SMIPrecTavg)
BIC_SM_Mai_full[which.min(BIC_SM_Mai_full$BIC[1:9]),][4]
# Define degree
r = BIC_SM_Mai_full[which.min(BIC_SM_Mai_full$BIC[1:9]),][[4]]

# Chose formula
bestStandard_formula <- formula_Mai_full_sm_detrendlog_SMIPrecTavg

###################
## GLM Ergebniss ##
glm.fit_SM_bestStandard_Mai_full  <- glm(formula = bestStandard_formula,  data = Yield_Covariates_SM_Mai_full)

bestStandard_glm_summary <- summary(glm.fit_SM_bestStandard_Mai_full)
capture.output(bestStandard_glm_summary, file = "./figures/figures_exploratory/BIC/Silomaize/May/Output_SM_Mai.txt", append=T)

bestStandard_glm_summary_qual <- print(c("bestStandard_glm_AIC:", round(bestStandard_glm_summary$aic,4)))
capture.output(bestStandard_glm_summary_qual, file = "./figures/figures_exploratory/BIC/Silomaize/May/Output_SM_Mai_fullqual.txt", append=T)

summary(glm.fit_SM_bestStandard_Mai_full)
'AIC:-6410.5'

####################
## PLM Ergebnisse ##
plm.fit_SM_bestStandard_Mai_full <- plm(formula = update(bestStandard_formula, .~. - dummy(comId)),  data = Yield_Covariates_SM_Mai_full,  effect="individual", model=("within"), index = c("comId","year"))

## Output
bestStandard_plm_summary <- summary(plm.fit_SM_bestStandard_Mai_full)
bestStandard_plm_summary
capture.output(bestStandard_plm_summary, file = "./figures/figures_exploratory/BIC/Silomaize/May/Output_SM_Mai.txt", append=T)

bestStandard_plm_summary_qual <- print(c("bestStandard_plm:", round(bestStandard_plm_summary$r.squared,4)))
capture.output(bestStandard_plm_summary_qual, file = "./figures/figures_exploratory/BIC/Silomaize/May/Output_SM_Mai_fullqual.txt", append=T)

'Adj. R-Squared: 0.040653'

## Fixed Effects ##
fixef <-  fixef(plm.fit_SM_bestStandard_Mai_full)
fixef_summary <- as.matrix(summary(fixef))
capture.output(fixef_summary, file = "./figures/figures_exploratory/FixedEffects/Silomaize/plm.fit_SM_bestStandard_Mai_full_FE_summary.txt")
# fixef_summary <- read.table( file = "./figures/figures_exploratory/FixedEffects/Silomaize/plm.fit_SM_BEST_Mai_full_FE_summary.txt")

##################
## LM Ergebniss ##
lm.fit_SM_bestStandard_Mai_full  <-lm(formula = bestStandard_formula,  data = Yield_Covariates_SM_Mai_full)
summary(lm.fit_SM_bestStandard_Mai_full) 
'Adjusted R-squared:  0.6276'

bestStandard_lm_summary <- summary(lm.fit_SM_bestStandard_Mai_full) 
bestStandard_lm_summary$r.squared
capture.output(bestStandard_lm_summary, file = "./figures/figures_exploratory/BIC/Silomaize/May/Output_SM_Mai.txt", append=T)


bestStandard_lm_summary_qual1 <- print(c("bestStandard_lm_r.squared:", round(bestStandard_lm_summary$r.squared,4)))
bestStandard_lm_summary_qual2 <- print(c("bestStandard_lm_adj.r.squared:", round(bestStandard_lm_summary$adj.r.squared,4)))
capture.output(bestStandard_lm_summary_qual1, file = "./figures/figures_exploratory/BIC/Silomaize/May/Output_SM_Mai_fullqual.txt", append=T)
capture.output(bestStandard_lm_summary_qual2, file = "./figures/figures_exploratory/BIC/Silomaize/May/Output_SM_Mai_fullqual.txt", append=T)

########################
## Heteroskedasdicity ##
bptest(glm.fit_SM_bestStandard_Mai_full) # Breusch Pagan Test of Heteroskedastie in den Störgrößen: Null: Homoskedasdicity.
bptest(plm.fit_SM_bestStandard_Mai_full)
' In beiden Fällen kann die Null widerlegt werden. Es gibt also heteroskedasdicity '

## Koenkers Version on BP Test: robuste Modification wenn die Störgrößen nicht normalverteilt sind. 
bptest(plm.fit_SM_bestStandard_Mai_full, studentize = TRUE)
'Auch hier kann die Null widerlegt werden. Need to use robust covariance variance matrix to correct standard errors'

#########################
#### Autocorrelation ####

######################################
## Tests for serial autocorrelation ##
pwartest(plm.fit_SM_bestStandard_Mai_full)
' Hier serielle Korrelation festzustellen'
pbgtest(plm.fit_SM_bestStandard_Mai_full) 

'Solution for serial correlation: Cluster by groups.
Solution for cross sectional correlation: Cluster by time'

#################################
## Correct the Standard Errors ##

## Correct Standard Errors used in table ##
coeftest(plm.fit_SM_bestStandard_Mai_full)

## Robust covariance matrix estimators a la White 
# coeftest(plm.fit_SM_bestStandard_Mai_full,vcov=vcovHC(plm.fit_SM_bestStandard_Mai_full,method = "arellano", type = "HC0")) 
cov0_SM_bestStandard_Mai_full <- vcovHC(plm.fit_SM_bestStandard_Mai_full,method = "arellano", type = "HC0", cluster="group")
Wh.se_serial_SM_bestStandard_Mai_full <- sqrt(diag(cov0_SM_bestStandard_Mai_full))

cov0.1_SM_bestStandard_Mai_full <- vcovHC(plm.fit_SM_bestStandard_Mai_full,method = "arellano", type = "HC0", cluster="time")
Wh.se_cross_SM_bestStandard_Mai_full <- sqrt(diag(cov0.1_SM_bestStandard_Mai_full))

# ## Beck Katz ##
# # coeftest(plm.fit_SM_bestStandard_Mai_full, vcov = function(x) vcovBK(plm.fit_SM_bestStandard_Mai_full,method = "arellano", type = "HC0"))
# cov1_SM_bestStandard_Mai_full <- vcovBK(plm.fit_SM_bestStandard_Mai_full,method = "arellano", type = "HC0",  cluster="time")
# BK.se_SM_bestStandard_Mai_full <- sqrt(diag(cov1_SM_bestStandard_Mai_full))

## Driscoll Kraay: ##
summary(plm.fit_SM_bestStandard_Mai_full)
cov2_SM_bestStandard_Mai_full     <- vcovSCC(plm.fit_SM_bestStandard_Mai_full,method = "arellano",type = "HC0")
DK.se_SM_bestStandard_Mai_full    <- sqrt(diag(cov2_SM_bestStandard_Mai_full))

# cov2.1_SM_bestStandard_Mai_full   <- vcovSCC(plm.fit_SM_bestStandard_Mai_full,method = "arellano",type = "HC0", maxlag=1)
# DK2.1.se_SM_bestStandard_Mai_full <- sqrt(diag(cov2.1_SM_bestStandard_Mai_full))

# cov2.2_SM_bestStandard_Mai_full   <- vcovSCC(plm.fit_SM_bestStandard_Mai_full,method = "arellano",type = "HC0", maxlag=2)
# DK2.2.se_SM_bestStandard_Mai_full <- sqrt(diag(cov2.2_SM_bestStandard_Mai_full))
# 
# cov2.3_SM_bestStandard_Mai_full   <- vcovSCC(plm.fit_SM_bestStandard_Mai_full,method = "arellano",type = "HC0", maxlag=3)
# DK2.3.se_SM_bestStandard_Mai_full <- sqrt(diag(cov2.3_SM_bestStandard_Mai_full))
# 
# cov2.4_SM_bestStandard_Mai_full   <- vcovSCC(plm.fit_SM_bestStandard_Mai_full,method = "arellano",type = "HC0", maxlag=4)
# DK2.4.se_SM_bestStandard_Mai_full <- sqrt(diag(cov2.4_SM_bestStandard_Mai_full))
# 
cov2.5_SM_bestStandard_Mai_full   <- vcovSCC(plm.fit_SM_bestStandard_Mai_full,method = "arellano",type = "HC0", maxlag=5)
DK2.5.se_SM_bestStandard_Mai_full <- sqrt(diag(cov2.5_SM_bestStandard_Mai_full))

## Cameron et al /Thompson : doouble-clustering estimator 
# coeftest(plm.fit_SM_bestStandard_Mai_full, vcovDC(plm.fit_SM_bestStandard_Mai_full, method = "arellano", type = "HC0"))
cov3_SM_bestStandard_Mai_full <- vcovDC(plm.fit_SM_bestStandard_Mai_full, method = "arellano", type = "HC0")
CT.se_SM_bestStandard_Mai_full <- sqrt(diag(cov3_SM_bestStandard_Mai_full))
'Our estimator is qualitatively similar to the ones presented in White and Domowitz (1984), for
time series data, and Conley (1999), for spatial data. '

## Generate Table with Output ##
se <- list(NULL, Wh.se_cross_SM_bestStandard_Mai_full, Wh.se_serial_SM_bestStandard_Mai_full, DK.se_SM_bestStandard_Mai_full, DK2.5.se_SM_bestStandard_Mai_full, CT.se_SM_bestStandard_Mai_full)
labels1 <-c("NULL","WhiteCross","WhiteSerial", "DriscollKraay", "DriscollKray2.5","CameronThompson")
stargazer(plm.fit_SM_bestStandard_Mai_full, plm.fit_SM_bestStandard_Mai_full, plm.fit_SM_bestStandard_Mai_full, plm.fit_SM_bestStandard_Mai_full, plm.fit_SM_bestStandard_Mai_full,plm.fit_SM_bestStandard_Mai_full,
          se = se,   
          dep.var.caption  = "Model with smallest BIC of Standard Configuration - Mai",
          dep.var.labels = "log(Silomaize)",
          style="default",
          model.numbers = FALSE,
          column.labels = labels1,          
          type="text", out="./figures/figures_exploratory/BIC/Silomaize/May/SM_Mai_full_bestStandard.txt"
)

########################################
## Results with smallest BIC with SMI ##
########################################

# Only look at models with SMI
BIC_SM_Mai_full[1:28,]

# Smallest BIC of those models
BIC_SM_Mai_full$BIC[1:28]
which.min(BIC_SM_Mai_full$BIC[1:28])

# Look at Modelinformation
BIC_SM_Mai_full[which.min(BIC_SM_Mai_full$BIC[1:28]), ]

# Extract Model Index
BIC_SM_Mai_full[which.min(BIC_SM_Mai_full$BIC[1:28]), ][[3]]

## Look at model with lowest BIC with SMI
BIC_SM_Mai_full[BIC_SM_Mai_full$model_index==BIC_SM_Mai_full[which.min(BIC_SM_Mai_full$BIC[1:28]), ][[3]],]
which.min(BIC_SM_Mai_full[BIC_SM_Mai_full$model_index==BIC_SM_Mai_full[which.min(BIC_SM_Mai_full$BIC[1:28]), ][[3]],][,1])



plot(BIC_SMIPrecPet)
which.min(BIC_SMIPrecPet)
r = which.min(BIC_SM_Mai_full[BIC_SM_Mai_full$model_index==BIC_SM_Mai_full[which.min(BIC_SM_Mai_full$BIC[1:28]), ][[3]],][,1])
bestSMI_formula <- formula_list_Mai_full_sm_detrendlog


###################
## GLM Ergebniss ##
glm.fit_SM_bestSMI_Mai_full  <- glm(formula = bestSMI_formula,  data = Yield_Covariates_SM_Mai_full)

bestSMI_glm_summary <- summary(glm.fit_SM_bestSMI_Mai_full)
bestSMI_glm_summary
capture.output(bestSMI_glm_summary, file = "./figures/figures_exploratory/BIC/Silomaize/May/Output_SM_Mai.txt", append=T)

bestSMI_glm_summary_qual <- print(c("bestSMI_glm_AIC:", round(bestSMI_glm_summary$aic,4)))
capture.output(bestSMI_glm_summary_qual, file = "./figures/figures_exploratory/BIC/Silomaize/May/Output_SM_Mai_fullqual.txt", append=T)

####################
## PLM Ergebnisse ##
plm.fit_SM_bestSMI_Mai_full <- plm(formula = update(bestSMI_formula, .~. - dummy(comId)),  data = Yield_Covariates_SM_Mai_full,  effect="individual", model=("within"), index = c("comId","year"))

## Output
bestSMI_plm_summary <- summary(plm.fit_SM_bestSMI_Mai_full)
bestSMI_plm_summary
capture.output(bestSMI_plm_summary, file = "./figures/figures_exploratory/BIC/Silomaize/May/Output_SM_Mai.txt", append=T)

bestSMI_plm_summary_qual <- print(c("bestSMI_plm:", round(bestSMI_plm_summary$r.squared,4)))
capture.output(bestSMI_plm_summary_qual, file = "./figures/figures_exploratory/BIC/Silomaize/May/Output_SM_Mai_fullqual.txt", append=T)


## Fixed Effects ##
fixef <-  fixef(plm.fit_SM_bestSMI_Mai_full)
fixef_summary <- as.matrix(summary(fixef))
capture.output(fixef_summary, file = "./figures/figures_exploratory/FixedEffects/Silomaize/plm.fit_SM_bestSMI_Mai_full_FE_summary.txt")
# fixef_summary <- read.table( file = "./figures/figures_exploratory/FixedEffects/Silomaize/plm.fit_SM_BEST_Mai_full_FE_summary.txt")

##################
## LM Ergebniss ##
lm.fit_SM_bestSMI_Mai_full  <-lm(formula = bestSMI_formula,  data = Yield_Covariates_SM_Mai_full)
summary(lm.fit_SM_bestSMI_Mai_full) 
'Adjusted R-squared:  0.6276'

bestSMI_lm_summary <- summary(lm.fit_SM_bestSMI_Mai_full) 
capture.output(bestSMI_lm_summary, file = "./figures/figures_exploratory/BIC/Silomaize/May/Output_SM_Mai.txt", append=T)


bestSMI_lm_summary_qual1 <- print(c("bestSMI_lm_r.squared:", round(bestSMI_lm_summary$r.squared,4)))
bestSMI_lm_summary_qual2 <- print(c("bestSMI_lm_adj.r.squared:", round(bestSMI_lm_summary$adj.r.squared,4)))
capture.output(bestSMI_lm_summary_qual1, file = "./figures/figures_exploratory/BIC/Silomaize/May/Output_SM_Mai_fullqual.txt", append=T)
capture.output(bestSMI_lm_summary_qual2, file = "./figures/figures_exploratory/BIC/Silomaize/May/Output_SM_Mai_fullqual.txt", append=T)

########################
## Heteroskedasdicity ##
bptest(glm.fit_SM_bestSMI_Mai_full) # Breusch Pagan Test of Heteroskedastie in den Störgrößen: Null: Homoskedasdicity.
bptest(plm.fit_SM_bestSMI_Mai_full)
' In beiden Fällen kann die Null widerlegt werden. Es gibt also heteroskedasdicity '

## Koenkers Version on BP Test: robuste Modification wenn die Störgrößen nicht normalverteilt sind. 
bptest(plm.fit_SM_bestSMI_Mai_full, studentize = TRUE)
'Auch hier kann die Null widerlegt werden. Need to use robust covariance variance matrix to correct standard errors'

#########################
#### Autocorrelation ####

######################################
## Tests for serial autocorrelation ##
pwartest(plm.fit_SM_bestSMI_Mai_full)
pbgtest(plm.fit_SM_bestSMI_Mai_full) 
'Hier serielle Korrelation festzustellen'

###########################################
## Correct Standard Errors used in table ##
coeftest(plm.fit_SM_bestSMI_Mai_full)

## Robust covariance matrix estimators a la White ##
# coeftest(plm.fit_SM_bestSMI_Mai_full,vcov=vcovHC(plm.fit_SM_bestSMI_Mai_full,method = "arellano", type = "HC0")) 
cov0_SM_bestSMI_Mai_full <- vcovHC(plm.fit_SM_bestSMI_Mai_full,method = "arellano", type = "HC0", cluster="group")
Wh.se_serial_SM_bestSMI_Mai_full <- sqrt(diag(cov0_SM_bestSMI_Mai_full))

cov0.1_SM_bestSMI_Mai_full <- vcovHC(plm.fit_SM_bestSMI_Mai_full,method = "arellano", type = "HC0", cluster="time")
Wh.se_cross_SM_bestSMI_Mai_full <- sqrt(diag(cov0.1_SM_bestSMI_Mai_full))
# 
# ## Beck Katz:
# # coeftest(plm.fit_SM_bestSMI_Mai_full, vcov = function(x) vcovBK(plm.fit_SM_bestSMI_Mai_full,method = "arellano", type = "HC0"))
# cov1 <- vcovBK(plm.fit_SM_bestSMI_Mai_full,method = "arellano", type = "HC0",  cluster="time")
# BK.se <- sqrt(diag(cov1))

## Driscoll Kraay ##
# summary(plm.fit_SM_bestSMI_Mai_full)
cov2_SM_bestSMI_Mai_full     <- vcovSCC(plm.fit_SM_bestSMI_Mai_full,method = "arellano",type = "HC0")
DK.se_SM_bestSMI_Mai_full    <- sqrt(diag(cov2_SM_bestSMI_Mai_full))

# cov2.1_SM_bestSMI_Mai_full   <- vcovSCC(plm.fit_SM_bestSMI_Mai_full,method = "arellano",type = "HC0", maxlag=1)
# DK2.1.se_SM_bestSMI_Mai_full <- sqrt(diag(cov2.1_SM_bestSMI_Mai_full))

# cov2.2_SM_bestSMI_Mai_full   <- vcovSCC(plm.fit_SM_bestSMI_Mai_full,method = "arellano",type = "HC0", maxlag=2)
# DK2.2.se_SM_bestSMI_Mai_full <- sqrt(diag(cov2.2_SM_bestSMI_Mai_full))
# 
# cov2.3_SM_bestSMI_Mai_full   <- vcovSCC(plm.fit_SM_bestSMI_Mai_full,method = "arellano",type = "HC0", maxlag=3)
# DK2.3.se_SM_bestSMI_Mai_full <- sqrt(diag(cov2.3_SM_bestSMI_Mai_full))
# 
# cov2.4_SM_bestSMI_Mai_full   <- vcovSCC(plm.fit_SM_bestSMI_Mai_full,method = "arellano",type = "HC0", maxlag=4)
# DK2.4.se_SM_bestSMI_Mai_full <- sqrt(diag(cov2.4_SM_bestSMI_Mai_full))
# 
cov2.5_SM_bestSMI_Mai_full   <- vcovSCC(plm.fit_SM_bestSMI_Mai_full,method = "arellano",type = "HC0", maxlag=5)
DK2.5.se_SM_bestSMI_Mai_full <- sqrt(diag(cov2.5_SM_bestSMI_Mai_full))

' Achtung: Driscoll Kraay ist nicht geeignet für kurze Panel. Ist das bei mir der Fall? '

## Cameron et al /Thompson : doouble-clustering estimator ##
cov3_SM_bestSMI_Mai_full <- vcovDC(plm.fit_SM_bestSMI_Mai_full, method = "arellano", type = "HC0")
CT.se_SM_bestSMI_Mai_full <- sqrt(diag(cov3_SM_bestSMI_Mai_full))

################################
## Generate Table with Output ##
se <- list(NULL, Wh.se_cross_SM_bestSMI_Mai_full, Wh.se_serial_SM_bestSMI_Mai_full,  DK.se_SM_bestSMI_Mai_full, DK2.5.se_SM_bestSMI_Mai_full, CT.se_SM_bestSMI_Mai_full)
labels1 <-c("NULL","WhiteCross","WhiteSerial", "DriscollKraay", "DriscollKray2.5","CameronThompson")
stargazer(plm.fit_SM_bestSMI_Mai_full, plm.fit_SM_bestSMI_Mai_full, plm.fit_SM_bestSMI_Mai_full, plm.fit_SM_bestSMI_Mai_full, plm.fit_SM_bestSMI_Mai_full,plm.fit_SM_bestSMI_Mai_full,
          se = se,   
          dep.var.caption  = "Model with smallest BIC with SMI - Mai",
          dep.var.labels = "log(Silomaize)",
          style="default",
          model.numbers = FALSE,
          column.labels = labels1,          
          type="text", out="./figures/figures_exploratory/BIC/Silomaize/May/SM_Mai_full_bestSM.txt"
)




