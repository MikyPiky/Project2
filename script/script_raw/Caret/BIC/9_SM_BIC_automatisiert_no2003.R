############################
####  SiloMaize in September_no2003 ####
############################
'
## Variables to change when transferring to other months
- Sep_no2003


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
- Models with lowest BIC not considering PET: Tavg, Prec, SMI


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
- Yield_Covariates_SM_Sep.csv (auf September reduzierter Data.Frame)
- Export Data frame for use in BIC_Graphic: file="./data/data_raw/BIC/BIC_SM_Sep.csv")

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
names_Sep <- grep(c("*_Sep"), names)
names_Sep
Yield_Covariates_Sep <- Yield_Covariates[,names_Sep]
names(Yield_Covariates_Sep)
dim(Yield_Covariates_Sep)

## Delete all but SMI, Prec, Tavg and Pet
names(Yield_Covariates_Sep)
Yield_Covariates_Sep <- Yield_Covariates_Sep[,c(1,3,5,7)]
names(Yield_Covariates_Sep)

## Establish first part of data frame_ time and spatial reference plus Silomaize ##
names(Yield_Covariates[,c(2,1,3:5,7)])
Yield_Covariates_SM <- Yield_Covariates[,c(2,1,3:5,7)] # Achtung, darauf achten, dass comId und year in der richtigen Reihenfolge sind.
names(Yield_Covariates_SM)
head(Yield_Covariates_SM)

Yield_Covariates_SM_Sep <- cbind(Yield_Covariates_SM, Yield_Covariates_Sep)
names(Yield_Covariates_SM_Sep)
names(Yield_Covariates_SM_Sep) <- c( "comId" , "year","com","stateId","state","siloMaize","SMI", "Prec","Tavg", "Pet")
names(Yield_Covariates_SM_Sep)

###########################
##### Delete year 2003 ####
###########################
table(Yield_Covariates_SM_Sep$year)
dim(Yield_Covariates_SM_Sep[Yield_Covariates_SM_Sep$year==2003,])
dim(Yield_Covariates_SM_Sep[Yield_Covariates_SM_Sep$year!=2003,])
dim(Yield_Covariates_SM_Sep)

Yield_Covariates_SM_Sep_no2003 <- Yield_Covariates_SM_Sep[Yield_Covariates_SM_Sep$year!=2003,]
dim(Yield_Covariates_SM_Sep_no2003)

head(Yield_Covariates_SM_Sep_no2003)

#########################################
#### Create stepwise function of SMI ####
#########################################
' Drought Monitor Spezification - FULL'

# Yield_Covariates_SM_Sep_no2003$SMI_GDM <- cut(Yield_Covariates_SM_Sep_no2003$SMI, breaks = c(0, 0.1, 0.2, 0.3,0.4,0.49,0.51,0.6, 0.7, 0.8, 0.9, 1),                                            ,
#                                                  labels = c("severe drought","moderate drought","abnormally dry",
#                                                             "dry", 
#                                                             "normally dry", 
#                                                             "normal",
#                                                             "normally wet",
#                                                             "wet",  
#                                                             "abnormally wet" ,"abundantly wet", "severely wet"))
Yield_Covariates_SM_Sep_no2003$SMI_GDM <- cut(Yield_Covariates_SM_Sep_no2003$SMI, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1),                                            ,
                                              labels = c("severe drought","moderate drought","abnormally dry", "normal",
                                                         "abnormally wet" ,"abundantly wet", "severely wet"))

head(Yield_Covariates_SM_Sep_no2003$SMI_GDM )
table(Yield_Covariates_SM_Sep_no2003$SMI_GDM )

################
## Delete NAs ##
################
sum(is.na(Yield_Covariates_SM_Sep_no2003) )
dim(Yield_Covariates_SM_Sep_no2003)
Yield_Covariates_SM_Sep_no2003_nna <- na.omit(Yield_Covariates_SM_Sep_no2003) 
dim(Yield_Covariates_SM_Sep_no2003_nna)

## Check for NAs
any(is.na(Yield_Covariates_SM_Sep_no2003_nna))
## Reset Rownames
rownames(Yield_Covariates_SM_Sep_no2003_nna) <- NULL

## Further work with DataFrame without Yield_Covariates_SM_Sep_no2003 index ##
Yield_Covariates_SM_Sep_no2003 <- Yield_Covariates_SM_Sep_no2003_nna

#####################
## Scale Covariates ##
#####################
head(Yield_Covariates_SM_Sep_no2003)[,8:length(Yield_Covariates_SM_Sep_no2003)-1]
Yield_Covariates_SM_Sep_no2003_norm <- Yield_Covariates_SM_Sep_no2003[,9:length(Yield_Covariates_SM_Sep_no2003)-1]
head(Yield_Covariates_SM_Sep_no2003_norm)
scale <- scale(Yield_Covariates_SM_Sep_no2003_norm)
summary(Yield_Covariates_SM_Sep_no2003_norm)
summary(scale)
Yield_Covariates_SM_Sep_no2003[,9:length(Yield_Covariates_SM_Sep_no2003)-1] <- scale
head(Yield_Covariates_SM_Sep_no2003)


#########################################################################
## Remove comIds with less than 7 observations to avoid leveage issues ## 
#########################################################################

#####################################################
## Delete all comIds with less than 7 observations ##
sum(table(Yield_Covariates_SM_Sep_no2003$comId) < 7 )
table(Yield_Covariates_SM_Sep_no2003$comId) < 7 

## comIds mit weniger als 7 Beoachtungen: ##
list <- c(3101, 3102, 3158, 3402, 5114, 5117, 5314,5315, 5334,5378, 5512, 5911, 5916, 6413, 7131, 7135, 7233, 7331, 7332, 7337, 7338, 7339, 8111,12052, 14612, 15001, 15082, 15083, 15084, 15085, 15086, 15087, 15088, 15089, 15091, 16051, 16052 )
length(list)
list[[1]]

temp <- Yield_Covariates_SM_Sep_no2003
for (i in 1:34)
{
  print(Yield_Covariates_SM_Sep_no2003[Yield_Covariates_SM_Sep_no2003$comId==list[i],])
  temp <- (temp[!temp$comId==list[i],])
}

## Number of deleted rows ##
dim(temp) - dim(Yield_Covariates_SM_Sep_no2003)

## Further use old name for data.frame
Yield_Covariates_SM_Sep_no2003 <- temp

################################
## Befehle nach jedem löschen ##
Yield_Covariates_SM_Sep_no2003 <- na.omit(Yield_Covariates_SM_Sep_no2003) 
rownames(Yield_Covariates_SM_Sep_no2003) <- NULL
Yield_Covariates_SM_Sep_no2003 <- plm.data(Yield_Covariates_SM_Sep_no2003, index=c("comId", "year"))
Yield_Covariates_SM_Sep_no2003[,c("comId","stateId")] <- lapply(Yield_Covariates_SM_Sep_no2003[,c("comId","stateId")], factor )

#################################################
#### Remove log trend of indepedent variable ####
#################################################
'Fit log of yield on log of time and use the residuals of that for yields'
logtrend <- lm(log(siloMaize) ~ log(as.integer(year)), data= Yield_Covariates_SM_Sep_no2003)

##########################
## Issue with Outliers ###
##########################
par(mfrow = c(2,2))
plot(logtrend) 

## Look at Outliers Values ##
Yield_Covariates_SM_Sep_no2003[c(1276, 3262, 3283, 3171,3255),]

## Look at other values of outliers com #
Yield_Covariates_SM_Sep_no2003[Yield_Covariates_SM_Sep_no2003$comId == "6532",] # 2008 hier scheint nur ein Jahr ein Messfehler zu sein: diesen Lösche ich 
Yield_Covariates_SM_Sep_no2003[Yield_Covariates_SM_Sep_no2003$comId == "12067",] # 2006 verändere ich nicht 
Yield_Covariates_SM_Sep_no2003[Yield_Covariates_SM_Sep_no2003$comId == "12069",] # 2003 verändere ich nicht 
Yield_Covariates_SM_Sep_no2003[Yield_Covariates_SM_Sep_no2003$comId == "12060",] # 1999 verändere ich nicht 
Yield_Covariates_SM_Sep_no2003[Yield_Covariates_SM_Sep_no2003$comId == "12067",] # 2006 verändere ich nicht 


## Delete outliers ##
' Da sich die Patterns bei den BIC Vergleichen nicht ändert, kümmere ich micht nicht weiter um die Outlier. 
Ich nehme nur sehr offensichtliche Messfehler raus.'
Yield_Covariates_SM_Sep_no2003 <- Yield_Covariates_SM_Sep_no2003[!(Yield_Covariates_SM_Sep_no2003$comId == "6532" & Yield_Covariates_SM_Sep_no2003$year == "2008"),]

Yield_Covariates_SM_Sep_no2003 <- na.omit(Yield_Covariates_SM_Sep_no2003)
rownames(Yield_Covariates_SM_Sep_no2003) <- NULL


#################################################
#### Remove log trend of indepedent variable ####
logtrend <- lm(log(siloMaize) ~ log(as.integer(year)), data= Yield_Covariates_SM_Sep_no2003)
summary(logtrend)
Yield_Covariates_SM_Sep_no2003$siloMaize_logtrend <- resid(logtrend)

#######################################
## Prepare dataframe for plm package ##
'Change Indexing so that it can be used in plm package'
Yield_Covariates_SM_Sep_no2003 <- plm.data(Yield_Covariates_SM_Sep_no2003, index=c("comId", "year"))
str(Yield_Covariates_SM_Sep_no2003)

###########################################
## Transform comId and stateId to factor ##
Yield_Covariates_SM_Sep_no2003[,c("comId","stateId")] <- lapply(Yield_Covariates_SM_Sep_no2003[,c("comId","stateId")], factor )
lapply(Yield_Covariates_SM_Sep_no2003, class)


###################################################
##### Save Yield_Covariates_SM_Sep_no2003 extern ####
write.csv(Yield_Covariates_SM_Sep_no2003, file="./data/data_raw/Yield_Covariates_SM_Sep_no2003.csv")

#######################################################
#### BIC to choose the degrees of the polynomials  ####
#######################################################
## create a matrix which contains all possible degree combinations, here for three variables ##
degree <- permutations(n=3,r=2,v=c(1:3),repeats.allowed=T)
degree

################################################
## Formulas for Model Variations to be tested ##

## with SMI
formula_Sep_no2003_sm_detrendlog_SMIPrecTavg <- siloMaize_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  poly(Tavg, degree[r, 2], raw = T) + 
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormally dry",
                  "abnormally wet" ,"abundantly wet", "severely wet")) + dummy(comId)

formula_Sep_no2003_sm_detrendlog_SMIPrecPet <- siloMaize_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  poly(Pet, degree[r, 2], raw = T) + 
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormally dry", "abnormally wet" ,"abundantly wet", "severely wet")) + dummy(comId)

formula_Sep_no2003_sm_detrendlog_SMIPrec <- siloMaize_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormally dry", "abnormally wet" ,"abundantly wet", "severely wet")) + dummy(comId)

formula_Sep_no2003_sm_detrendlog_SMIPet <- siloMaize_logtrend ~ poly(Pet, degree[r, 2], raw = T) +  
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormally dry", "abnormally wet" ,"abundantly wet", "severely wet")) + dummy(comId)

formula_Sep_no2003_sm_detrendlog_SMITavg <- siloMaize_logtrend ~ poly(Tavg, degree[r, 2], raw = T) + 
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormally dry", "abnormally wet" ,"abundantly wet", "severely wet")) + dummy(comId)

formula_Sep_no2003_sm_detrendlog_SMI <- siloMaize_logtrend ~  
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormally dry","abnormally wet" ,"abundantly wet", "severely wet")) + dummy(comId)

## no SMI
formula_Sep_no2003_sm_detrendlog_PrecTavg <- siloMaize_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  poly(Tavg, degree[r, 2], raw = T) + dummy(comId)

formula_Sep_no2003_sm_detrendlog_PrecPet <- siloMaize_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  poly(Pet, degree[r, 2], raw = T) + dummy(comId)

formula_Sep_no2003_sm_detrendlog_Prec <- siloMaize_logtrend ~ poly(Prec, degree[r, 1], raw = T) + dummy(comId)

formula_Sep_no2003_sm_detrendlog_Pet <- siloMaize_logtrend ~ poly(Pet, degree[r, 2], raw = T)  + dummy(comId)

formula_Sep_no2003_sm_detrendlog_Tavg <- siloMaize_logtrend ~ poly(Tavg, degree[r, 2], raw = T)  + dummy(comId)

## Generate list of formula
formula_list_Sep_no2003_sm_detrendlog <- c(
  formula_Sep_no2003_sm_detrendlog_SMIPrecTavg,
  formula_Sep_no2003_sm_detrendlog_SMIPrecPet,
  formula_Sep_no2003_sm_detrendlog_SMIPrec,
  formula_Sep_no2003_sm_detrendlog_SMIPet,
  formula_Sep_no2003_sm_detrendlog_SMITavg,
  formula_Sep_no2003_sm_detrendlog_SMI,
  formula_Sep_no2003_sm_detrendlog_Prec,
  formula_Sep_no2003_sm_detrendlog_Tavg,
  formula_Sep_no2003_sm_detrendlog_Pet ,
  formula_Sep_no2003_sm_detrendlog_PrecTavg,
  formula_Sep_no2003_sm_detrendlog_PrecPet
  
  
)
formula_list_Sep_no2003_sm_detrendlog[[11]]
#################################################################################################
# Loop through the container list to cover all permutations of posssible degree of freedoms of ##
# of the polynomials of the variables                                                          ##
#################################################################################################

##################################################
## Loop through various variable configurations ##
BIC_SMIPrecTavg <- rep(0,9)
for(r in 1:9){
  glm.fit_SMIPrecTavg  <- glm(formula = formula_Sep_no2003_sm_detrendlog_SMIPrecTavg, data = Yield_Covariates_SM_Sep_no2003) 
  BIC_SMIPrecTavg[r] <- BIC(glm.fit_SMIPrecTavg)
}

BIC_SMIPrecPet <- rep(0,9)
for(r in 1:9){
  glm.fit_SMIPrecPet  <- glm(formula = formula_Sep_no2003_sm_detrendlog_SMIPrecPet,  data = Yield_Covariates_SM_Sep_no2003)
  BIC_SMIPrecPet[r] <- BIC(glm.fit_SMIPrecPet)
}

BIC_SMIPrec <- rep(0,9)
for(r in 1:9){  
  glm.fit_SMIPrec      <- glm(formula = formula_Sep_no2003_sm_detrendlog_SMIPrec,     data = Yield_Covariates_SM_Sep_no2003) 
  BIC_SMIPrec[r] <- BIC(glm.fit_SMIPrec)
}

BIC_SMIPet <- rep(0,9)
for(r in 1:9){  
  glm.fit_SMIPet       <- glm(formula = formula_Sep_no2003_sm_detrendlog_SMIPet,      data = Yield_Covariates_SM_Sep_no2003) 
  BIC_SMIPet[r] <- BIC(glm.fit_SMIPet)
}

BIC_SMITavg <- rep(0,9)
for(r in 1:9){  
  glm.fit_SMITavg      <- glm(formula = formula_Sep_no2003_sm_detrendlog_SMITavg,     data = Yield_Covariates_SM_Sep_no2003) 
  BIC_SMITavg[r] <- BIC(glm.fit_SMITavg)
}

BIC_SMI <- rep(0,9)
for(r in 1:9){  
  glm.fit_SMI      <- glm(formula = formula_Sep_no2003_sm_detrendlog_SMI,     data = Yield_Covariates_SM_Sep_no2003) 
  BIC_SMI[r] <- BIC(glm.fit_SMI)
}

BIC_PrecTavg <- rep(0,9)
for(r in 1:9){
  glm.fit_PrecTavg  <- glm(formula = formula_Sep_no2003_sm_detrendlog_PrecTavg, data = Yield_Covariates_SM_Sep_no2003) 
  BIC_PrecTavg[r] <- BIC(glm.fit_PrecTavg)
}

BIC_PrecPet <- rep(0,9)
for(r in 1:9){
  glm.fit_PrecPet  <- glm(formula = formula_Sep_no2003_sm_detrendlog_PrecPet, data = Yield_Covariates_SM_Sep_no2003) 
  BIC_PrecPet[r] <- BIC(glm.fit_PrecPet)
}

BIC_Prec <- rep(0,9)
for(r in 1:9){
  glm.fit_Prec  <- glm(formula = formula_Sep_no2003_sm_detrendlog_Prec, data = Yield_Covariates_SM_Sep_no2003) 
  BIC_Prec[r] <- BIC(glm.fit_Prec)
}

BIC_Pet  <- rep(0,9)
for(r in 1:9){
  glm.fit_Pet   <- glm(formula = formula_Sep_no2003_sm_detrendlog_Pet , data = Yield_Covariates_SM_Sep_no2003) 
  BIC_Pet [r] <- BIC(glm.fit_Pet )
}

BIC_Tavg  <- rep(0,9)
for(r in 1:9){
  glm.fit_Tavg   <- glm(formula = formula_Sep_no2003_sm_detrendlog_Tavg , data = Yield_Covariates_SM_Sep_no2003) 
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
month <-rep("Sep_no2003",99)

BIC_Sep_no2003 <- cbind(BIC, model ,model_index, index, month)

#######################
## Delete Duplicates ##
which(duplicated(BIC_Sep_no2003$BIC))
list3 <- c(20,21,23,24,26,27,31,32,33,34,35,36,40,41,42,43,44,45,47,48,49,50,51,52,53,54,56,57,59,60,62,63,67,68,69,70,71,72,76,77,78,79,80,81)
length(list3)
temp <- BIC_Sep_no2003

for (i in 1:44)
{
  print(BIC_Sep_no2003[BIC_Sep_no2003$index ==list3[i],])
  temp <- (temp[!temp$index==list3[i],])
}
dim(BIC_Sep_no2003)
dim(temp)

################################
## Correct created data.frame ##
rownames(temp) <- NULL
BIC_Sep_no2003 <- temp
lapply(BIC_Sep_no2003, class)
BIC_Sep_no2003$index <-  c(1:55)

############################
## Plot data with ggplot2 ##
g <- ggplot(BIC_Sep_no2003,aes(y=BIC, x=index))
g_save <- g + geom_point(aes(color=model)) + labs(title="BIC_SM_Sep_no2003", x="") + 
  theme(plot.title=element_text(size=15, face="bold")) + theme_few() +
  facet_wrap( ~ month)
g_save 
ggsave( "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/BIC_SM_Sep_no2003.png", width= 8, height=8, dpi=400)


## Export Data frame for use in BIC_Grafic
BIC_SM_Sep_no2003 <- BIC_Sep_no2003
class(BIC_SM_Sep_no2003)
write.csv(BIC_SM_Sep_no2003, file="./data/data_raw/BIC/BIC_SM_Sep_no2003.csv")
which.min(BIC_SM_Sep_no2003$BIC)

BIC_SM_Sep_no2003[which.min(BIC_SM_Sep_no2003$BIC),]



###################################################################################################################################################################
################################### Explore Models ################################################################################################################
###################################################################################################################################################################

###################
## Load Data Set ##
# Yield_Covariates_SM_Sep_no2003 <- read.csv( file="./data/data_raw/Yield_Covariates_SM_Sep_no2003.csv")
# names(Yield_Covariates_SM_Sep_no2003)
# Yield_Covariates_SM_Sep_no2003$X <- NULL

#######################################
## Prepare dataframe for plm package ##
'Change Indexing so that it can be used in plm package'
Yield_Covariates_SM_Sep_no2003 <- plm.data(Yield_Covariates_SM_Sep_no2003, index=c("comId", "year"))

## Transform comId and stateId to factor ##
Yield_Covariates_SM_Sep_no2003[,c("comId","stateId")] <- lapply(Yield_Covariates_SM_Sep_no2003[,c("comId","stateId")], factor )
str(Yield_Covariates_SM_Sep_no2003)

#################################
###############################
## Results with smallest BIC ##
###############################

BIC_SM_Sep_no2003[which.min(BIC_SM_Sep_no2003$BIC),]
BIC_SM_Sep_no2003[order(BIC_SM_Sep_no2003$BIC)[1],]

## Define Difference between first and second best BIC Value ##
-(BIC_SM_Sep_no2003$BIC[order(BIC_SM_Sep_no2003$BIC)[1]]-BIC_SM_Sep_no2003$BIC[order(BIC_SM_Sep_no2003$BIC)[2]])

## Make condition for if ... else loop
-(BIC_SM_Sep_no2003$BIC[order(BIC_SM_Sep_no2003$BIC)[1]]-BIC_SM_Sep_no2003$BIC[order(BIC_SM_Sep_no2003$BIC)[2]]) < 6

BIC_SM_Sep_no2003[order(BIC_SM_Sep_no2003$BIC),]
BIC_order_Sep_no2003 <- BIC_SM_Sep_no2003[order(BIC_SM_Sep_no2003$BIC),]
config <- rep("Best", 55)
BIC_order_Sep_no2003_Best <- cbind(BIC_order_Sep_no2003, config)

capture.output(BIC_order_Sep_no2003_Best, file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/BIC_order_Sep_no2003")



############################################################################################
## If difference is larger than six between first and second best model, take first model ##
## However, it can not really be assumed that there is no difference though
x <- -(BIC_SM_Sep_no2003$BIC[order(BIC_SM_Sep_no2003$BIC)[1]]-BIC_SM_Sep_no2003$BIC[order(BIC_SM_Sep_no2003$BIC)[2]])
# 
# if ( x > 6)
# {
model_best <- print("Best Formula - Best BIC")
model_best
#  Extract model index
BIC_SM_Sep_no2003[order(BIC_SM_Sep_no2003$BIC)[1],][[3]]

## Extract model with lowest BIC of the models with that particular model index 
order(BIC_SM_Sep_no2003[BIC_SM_Sep_no2003$model_index==BIC_SM_Sep_no2003[order(BIC_SM_Sep_no2003$BIC)[1],][[3]],][,1])[1]

## Determine degree 
r = order(BIC_SM_Sep_no2003[BIC_SM_Sep_no2003$model_index==BIC_SM_Sep_no2003[order(BIC_SM_Sep_no2003$BIC)[1],][[3]],][,1])[1]

## Determine adequate formula for model by the means of the model index
best_formula <-  formula_list_Sep_no2003_sm_detrendlog[[BIC_SM_Sep_no2003[order(BIC_SM_Sep_no2003$BIC)[1],][[3]]]]
best_formula


# } else {
#   
#   #############################################################
#   ## If difference is smaller than 6, take second best model ##
#   model_best <- print("Best Formula - Second Best BIC")
#   model_best 
#   ## Look at second best model ##
#   BIC_SM_Sep_no2003[order(BIC_SM_Sep_no2003$BIC)[2],]
#   
#   ##  Extract model index
#   BIC_SM_Sep_no2003[order(BIC_SM_Sep_no2003$BIC)[2],][[3]]
#   
#   ## Extract model with second lowest BIC of the models with that particular model index 
#   order(BIC_SM_Sep_no2003[BIC_SM_Sep_no2003$model_index==BIC_SM_Sep_no2003[order(BIC_SM_Sep_no2003$BIC)[1],][[3]],][,1])[2]
#   
#   ## Determine degree 
#   r = order(BIC_SM_Sep_no2003[BIC_SM_Sep_no2003$model_index==BIC_SM_Sep_no2003[order(BIC_SM_Sep_no2003$BIC)[1],][[3]],][,1])[2]
#   
#   ## Determine adequate formula for model by the means of the model index
#   best_formula <-  formula_list_Sep_no2003_sm_detrendlog[[BIC_SM_Sep_no2003[order(BIC_SM_Sep_no2003$BIC)[2],][[3]]]]
#   best_formula
#   
# }
capture.output(model_best, file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/BIC_order_Sep_no2003", append=T)
capture.output(best_formula, file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/BIC_order_Sep_no2003", append=T)

###################
## GLM Ergebniss ##
glm.fit_SM_BEST_Sep_no2003  <- glm(formula = best_formula,  data = Yield_Covariates_SM_Sep_no2003)

## Summary
best_glm_summary <- summary(glm.fit_SM_BEST_Sep_no2003)
best_glm_summary
capture.output(best_glm_summary, file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/Output_SM_Sep_no2003_qual")

## AIC
best_glm_summary_qual <- print(c("best_glm_AIC:", round(best_glm_summary$aic,4)))

capture.output(print("BEST MODELS"), file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/Output_SM_Sep_no2003_qual.txt")
capture.output(best_glm_summary_qual, file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/Output_SM_Sep_no2003_qual.txt", append=T)

'AIC: -6494.3'

####################
## PLM Ergebnisse ##
plm.fit_SM_BEST_Sep_no2003 <- plm(formula = update(best_formula, .~. - dummy(comId)),  data = Yield_Covariates_SM_Sep_no2003,  effect="individual", model=("within"), index = c("comId","year"))

## Output
best_plm_summary <- summary(plm.fit_SM_BEST_Sep_no2003)
best_plm_summary
capture.output(best_plm_summary, file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/Output_SM_Sep_no2003_qual", append=T)

best_plm_summary_qual <- print(c("best_plm:", round(best_plm_summary$r.squared,4)))
capture.output(best_plm_summary_qual, file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/Output_SM_Sep_no2003_qual.txt", append=T)

'Adj. R-Squared: 0.054164'

## Fixed Effects ##
fixef <-  fixef(plm.fit_SM_BEST_Sep_no2003)
fixef_summary <- as.matrix(summary(fixef))
fixef_summary[33,3]
capture.output(fixef_summary, file = "./figures/figures_exploratory/FixedEffects/Silomaize/Sep_no2003/plm.fit_SM_BEST_Sep_no2003_FE_summary.txt")
# fixef_summary <- read.table( file = "./figures/figures_exploratory/FixedEffects/Silomaize/Sep_no2003/plm.fit_SM_BEST_Sep_no2003_FE_summary.txt")


##################
## LM Ergebniss ##
lm.fit_SM_BEST_Sep_no2003  <-lm(formula = best_formula,  data = Yield_Covariates_SM_Sep_no2003)
best_lm_summary <- summary(lm.fit_SM_BEST_Sep_no2003) 
best_lm_summary$r.squared
capture.output(best_lm_summary, file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/Output_SM_Sep_no2003_qual", append=T)


best_lm_summary_qual1 <- print(c("best_lm_r.squared:", round(best_lm_summary$r.squared,4)))
best_lm_summary_qual2 <- print(c("best_lm_adj.r.squared:", round(best_lm_summary$adj.r.squared,4)))
capture.output(best_lm_summary_qual1, file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/Output_SM_Sep_no2003_qual.txt", append=T)
capture.output(best_lm_summary_qual2, file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/Output_SM_Sep_no2003_qual.txt", append=T)
## Output
'Adjusted R-squared:   0.6345  '

capture.output(best_lm_summary, file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/Output_SM_Sep_no2003_qual", append=T)

################################################
## Assessing Influence (Leverage*discrepancy) ##

##############################
## Take a look at Residuals ##
# Hier sollte ich einfach das vorgehen aus mit 2003 rausnehmen und allgemeiner halten. Was ich aber raussschreiben muss sind die predicted values für 2003

## Read out residuals of that model ##
hist(lm.fit_SM_BEST_Sep_no2003$residuals, freq=FALSE)
Yield_Covariates_SM_Sep_Resid_no2003 <- as.data.frame( as.numeric(lm.fit_SM_BEST_Sep_no2003$residuals))
names(Yield_Covariates_SM_Sep_Resid_no2003) <- "Residual"
head(Yield_Covariates_SM_Sep_Resid_no2003)

### Include reference data.set for residuals
dim(Yield_Covariates_SM_Sep_no2003)
dim(Yield_Covariates_SM_Sep_Resid_no2003)
head(Yield_Covariates_SM_Sep_no2003)
Yield_Covariates_SM_Sep_Resid_no2003 <- (cbind(Yield_Covariates_SM_Sep_no2003[,1:3], Yield_Covariates_SM_Sep_Resid_no2003))
head(Yield_Covariates_SM_Sep_Resid_no2003)

### Write Residuals
write.csv(Yield_Covariates_SM_Sep_Resid_no2003, file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/Yield_Covariates_SM_Sep_Resid_no2003.csv")

###############################################################
## Generate residuals of 2003 with the model neglecting 2003 ##
## Load SiloMaize Data with 2003
Yield_Covariates_SM_Sep <- read.csv(file="./data/data_raw/Yield_Covariates_SM_Sep.csv")
str(Yield_Covariates_SM_Sep_no2003)

## Transfor it to plm.data
Yield_Covariates_SM_Sep <- plm.data(Yield_Covariates_SM_Sep)
Yield_Covariates_SM_Sep$X <- NULL
str(Yield_Covariates_SM_Sep)

## Read out Residuals ##
## Prepare Data.Frame only including the observation of 2003
newdata_Sep_no2003 = Yield_Covariates_SM_Sep[Yield_Covariates_SM_Sep$year==2003,]
dim(newdata_Sep_no2003)
rownames(newdata_Sep_no2003) <- NULL

## Create predicted values of the year 2003 based on the estimation without 2003 - out of sample prediction
y_hat_no2003 <- predict(lm.fit_SM_BEST_Sep_no2003, newdata = newdata_Sep_no2003)
length(y_hat_no2003)

## Compare predicted and real values
summary(y_hat_no2003)
summary(Yield_Covariates_SM_Sep_no2003$siloMaize_logtrend)
hist(y_hat_no2003, main="Predicted Values of Silage Maize of year 2003 from model without 2003")
hist(Yield_Covariates_SM_Sep_no2003$siloMaize_logtrend, main="log detrended valued of 2003")

## Make residuals 
resid2003_Sep_no2003 <- as.data.frame(newdata_Sep_no2003$siloMaize_logtrend - y_hat_no2003 )
head(resid2003_Sep_no2003)
names(resid2003_Sep_no2003) <- "resid_Sep_no2003_predicted2003"
summary(resid2003_Sep_no2003)
resid2003_Sep_no2003_df <- cbind(newdata_Sep_no2003[,1:3],resid2003_Sep_no2003)
head(resid2003_Sep_no2003_df)

## Write out residual data.frame 
write.csv(resid2003_Sep_no2003_df, file="./figures//figures_exploratory/BIC/Silomaize/Sep_no2003/resid2003_Sep_no2003.csv" )

## Plot residuals estimated for 2003
par(mfrow=c(1,2))
hist(resid2003_Sep_no2003_df$resid_Sep_no2003_predicted2003, freq=FALSE, main="Distribution of Residuals of model without 2003 predicted on 2003")

## Compare to original residuals ##
Yield_Covariates_SM_Sep_Resid <- read.csv(file="./figures//figures_exploratory/BIC/Silomaize/Sep_no2003/Yield_Covariates_SM_Sep_Resid.csv")
Yield_Covariates_SM_Sep_Resid$X <- NULL
Yield_Covariates_SM_Sep2 <- cbind(Yield_Covariates_SM_Sep,Yield_Covariates_SM_Sep_Resid)

hist(Yield_Covariates_SM_Sep2$Residual[Yield_Covariates_SM_Sep2$year==2003], freq=FALSE, 
     main="Distribution of Residuals of 2003")

####################
## Cooks Distance ##

## Plot Cooks Distance
plot(lm.fit_SM_BEST_Sep_no2003, which=4)

## Read out Cooks Distance ##
cook_Sep_no2003 <- as.data.frame(cooks.distance(lm.fit_SM_BEST_Sep_no2003))
colnames(cook_Sep_no2003) <- "CooksDistance"

## Reference Cooks Distance Values
dim(Yield_Covariates_SM_Sep_no2003[,1:3])
dim(cook_Sep_no2003)
cook_Sep_no2003_ref <- cbind(Yield_Covariates_SM_Sep_no2003[,1:3],cook_Sep_no2003)

## Write out Cooks Distance
write.csv(cook_Sep_no2003_ref, file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/best_SM_Sep_no2003_cooksDistance.csv")

## Define Cutoffs ##
cutoff_SM_Sep_no2003_small <- 4/((nrow(Yield_Covariates_SM_Sep_no2003)-length(lm.fit_SM_BEST_Sep_no2003$coefficients)-1)) 
cutoff_SM_Sep_no2003_small
cutoff_SM_Sep_no2003_large <- 1

## Number of Cooks Distance Values below the cutoffs ##
nrow_cooks_small <- nrow(Yield_Covariates_SM_Sep_no2003[cook_Sep_no2003 > cutoff_SM_Sep_no2003_small,]) 
nrow_cooks_large <- nrow(Yield_Covariates_SM_Sep_no2003[cook_Sep_no2003 > cutoff_SM_Sep_no2003_large,]) 

## Make a output
capture.output(c("Cutoff_small",cutoff_SM_Sep_no2003_small), file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/best_SM_Sep_no2003_cooks.txt")
capture.output(c("Cutoff_small_number",nrow_cooks_small), file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/best_SM_Sep_no2003_cooks.txt", append=T)
capture.output(c("Cutoff_large",cutoff_SM_Sep_no2003_large), file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/best_SM_Sep_no2003_cooks.txt", append=T)
capture.output(c("Cutoff_large_number",nrow_cooks_large), file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/best_SM_Sep_no2003_cooks.txt", append=T)

## Make a table of numbers of values above small_cutoff conditional on years 
year_cooks_SM_Sep_no2003_small <- as.data.frame(table(Yield_Covariates_SM_Sep_no2003$year[cook_Sep_no2003 > cutoff_SM_Sep_no2003_small ]) )
names(year_cooks_SM_Sep_no2003_small) <- c("year", "Freq")
year_cooks_SM_Sep_no2003_small
plot(year_cooks_SM_Sep_no2003_small)

## Make a table of numbers of valued above small_cutoff conditional on comId
comId_cooks_SM_Sep_no2003_small <- as.data.frame(table(Yield_Covariates_SM_Sep_no2003$comId[cook_Sep_no2003 > cutoff_SM_Sep_no2003_small ]) )
names(comId_cooks_SM_Sep_no2003_small) <- c("comId", "Freq")
comId_cooks_SM_Sep_no2003_small
plot(comId_cooks_SM_Sep_no2003_small)

## Make ordered table of cutoff above frequancies per comId
comId_cooks_SM_Sep_no2003_small2 <-cbind(comId_cooks_SM_Sep_no2003_small, unique(Yield_Covariates_SM_Sep_no2003$com))
comId_cooks_SM_Sep_no2003_small2[order(comId_cooks_SM_Sep_no2003_small2$Freq),]
comId_cooks_SM_Sep_no2003_small_ordered <- comId_cooks_SM_Sep_no2003_small2[order(comId_cooks_SM_Sep_no2003_small2$Freq),]

## Write Tables
capture.output(year_cooks_SM_Sep_no2003_small, file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/best_SM_Sep_no2003_cooks.txt", append=T)
capture.output(comId_cooks_SM_Sep_no2003_small, file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/best_SM_Sep_no2003_cooks.txt", append=T)
capture.output(c("Ordered table of comIds", comId_cooks_SM_Sep_no2003_small_ordered), file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/best_SM_Sep_no2003_cooks.txt", append=T)

write.csv(year_cooks_SM_Sep_no2003_small, file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/best_SM_Sep_no2003_cooks_year.csv")
write.csv(comId_cooks_SM_Sep_no2003_small2, file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/best_SM_Sep_no2003_cooks_comId.csv")
write.csv(comId_cooks_SM_Sep_no2003_small_ordered, file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/best_SM_Sep_no2003_cooks_comId_ordered.csv")

################################################################################################################################################################## 

########################
## Heteroskedasdicity ##
bptest(glm.fit_SM_BEST_Sep_no2003) # Breusch Pagan Test of Heteroskedastie in den Störgrößen: Null: Homoskedasdicity.
bptest(plm.fit_SM_BEST_Sep_no2003)
' In beiden Fällen kann die Null widerlegt werden. Es gibt also heteroskedasdicity '

## Koenkers Version on BP Test: robuste Modification wenn die Störgrößen nicht normalverteilt sind. 
bptest(plm.fit_SM_BEST_Sep_no2003, studentize = TRUE)
'Auch hier kann die Null widerlegt werden. Need to use robust covariance variance matrix to correct standard errors'


######################################
## Tests for serial autocorrelation ##
pwartest(plm.fit_SM_BEST_Sep_no2003)
pbgtest(plm.fit_SM_BEST_Sep_no2003) 
'
both, H_1 of serial autocorrelation cannot be rejected
'

#################################
## Correct the Standard Errors ##
#################################

## Correct Standard Errors used in table ##
coeftest(plm.fit_SM_BEST_Sep_no2003)

## Robust covariance matrix estimators a la White ##
# coeftest(plm.fit_SM_BEST_Sep_no2003,vcov=vcovHC(plm.fit_SM_BEST_Sep_no2003,method = "arellano", type = "HC0")) 
cov0_SM_BEST_Sep_no2003 <- vcovHC(plm.fit_SM_BEST_Sep_no2003,method = "arellano", type = "HC0", cluster="group")
Wh.se_serial_SM_BEST_Sep_no2003 <- sqrt(diag(cov0_SM_BEST_Sep_no2003))

cov0.1_SM_BEST_Sep_no2003 <- vcovHC(plm.fit_SM_BEST_Sep_no2003,method = "arellano", type = "HC0", cluster="time")
Wh.se_cross_SM_BEST_Sep_no2003 <- sqrt(diag(cov0.1_SM_BEST_Sep_no2003))
# 
# ## Beck Katz:
# # coeftest(plm.fit_SM_BEST_Sep_no2003, vcov = function(x) vcovBK(plm.fit_SM_BEST_Sep_no2003,method = "arellano", type = "HC0"))
# cov1 <- vcovBK(plm.fit_SM_BEST_Sep_no2003,method = "arellano", type = "HC0",  cluster="time")
# BK.se <- sqrt(diag(cov1))

# ## Driscoll Kraay ##
# summary(plm.fit_SM_BEST_Sep_no2003)
coeftest(plm.fit_SM_BEST_Sep_no2003,  vcov=function(x) vcovSCC(plm.fit_SM_BEST_Sep_no2003,method = "arellano",type = "HC0")) 
cov2_SM_BEST_Sep_no2003     <- vcovSCC(plm.fit_SM_BEST_Sep_no2003,method = "arellano",type = "HC0")
DK.se_SM_BEST_Sep_no2003    <- sqrt(diag(cov2_SM_BEST_Sep_no2003))
# 
# cov2.1_SM_BEST_Sep_no2003   <- vcovSCC(plm.fit_SM_BEST_Sep_no2003,method = "arellano",type = "HC0", maxlag=1)
# DK2.1.se_SM_BEST_Sep_no2003 <- sqrt(diag(cov2.1_SM_BEST_Sep_no2003))

# cov2.2_SM_BEST_Sep_no2003   <- vcovSCC(plm.fit_SM_BEST_Sep_no2003,method = "arellano",type = "HC0", maxlag=2)
# DK2.2.se_SM_BEST_Sep_no2003 <- sqrt(diag(cov2.2_SM_BEST_Sep_no2003))
# 
# cov2.3_SM_BEST_Sep_no2003   <- vcovSCC(plm.fit_SM_BEST_Sep_no2003,method = "arellano",type = "HC0", maxlag=3)
# DK2.3.se_SM_BEST_Sep_no2003 <- sqrt(diag(cov2.3_SM_BEST_Sep_no2003))
# 
# cov2.4_SM_BEST_Sep_no2003   <- vcovSCC(plm.fit_SM_BEST_Sep_no2003,method = "arellano",type = "HC0", maxlag=4)
# DK2.4.se_SM_BEST_Sep_no2003 <- sqrt(diag(cov2.4_SM_BEST_Sep_no2003))
# 
cov2.5_SM_BEST_Sep_no2003   <- vcovSCC(plm.fit_SM_BEST_Sep_no2003,method = "arellano",type = "HC0", maxlag=5)
DK2.5.se_SM_BEST_Sep_no2003 <- sqrt(diag(cov2.5_SM_BEST_Sep_no2003))

## Cameron et al /Thompson : doouble-clustering estimator  ##
# coeftest(plm.fit_SM_BEST_Sep_no2003, vcovDC(plm.fit_SM_BEST_Sep_no2003, method = "arellano", type = "HC0"))
cov3_SM_BEST_Sep_no2003 <- vcovDC(plm.fit_SM_BEST_Sep_no2003, method = "arellano", type = "HC0")
CT.se_SM_BEST_Sep_no2003 <- sqrt(diag(cov3_SM_BEST_Sep_no2003))
'Our estimator is qualitatively similar to the ones presented in White and Domowitz (1984), for
time series data, and Conley (1999), for spatial data. '

## Generate Table with Output ##
se <- list(NULL, Wh.se_cross_SM_BEST_Sep_no2003, Wh.se_serial_SM_BEST_Sep_no2003,  DK.se_SM_BEST_Sep_no2003, DK2.5.se_SM_BEST_Sep_no2003, CT.se_SM_BEST_Sep_no2003)
labels1 <-c("NULL","WhiteCross","WhiteSerial", "DriscollKraay", "DriscollKray2.5","CameronThompson")
stargazer(plm.fit_SM_BEST_Sep_no2003, plm.fit_SM_BEST_Sep_no2003, plm.fit_SM_BEST_Sep_no2003, plm.fit_SM_BEST_Sep_no2003, plm.fit_SM_BEST_Sep_no2003,plm.fit_SM_BEST_Sep_no2003,
          se = se,   
          dep.var.caption  = "Model with smallest BIC - September_no2003",
          dep.var.labels = "log(Silomaize)",
          style="default",
          model.numbers = FALSE,
          column.labels = labels1,          
          type="text", out="./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/SM_Sep_no2003_best.txt"
)



#########################################################
## Results with smallest BIC of Standard Configuration ##
#########################################################

## Look for smallest BIC of Standar Config
BIC_SM_Sep_no2003[which.min(BIC_SM_Sep_no2003$BIC[1:9]),]
BIC_SM_Sep_no2003[order(BIC_SM_Sep_no2003$BIC[1:9])[1],]


## Define difference between first and second best BIC Value ##
-(BIC_SM_Sep_no2003$BIC[order(BIC_SM_Sep_no2003$BIC[1:9])[1]] -  BIC_SM_Sep_no2003$BIC[order(BIC_SM_Sep_no2003$BIC[1:9])[2]])

## Make condition for if ... else loop
-(BIC_SM_Sep_no2003$BIC[order(BIC_SM_Sep_no2003$BIC[1:9])[1]] -  BIC_SM_Sep_no2003$BIC[order(BIC_SM_Sep_no2003$BIC[1:9])[2]])
x <- -(BIC_SM_Sep_no2003$BIC[order(BIC_SM_Sep_no2003$BIC[1:9])[1]] -  BIC_SM_Sep_no2003$BIC[order(BIC_SM_Sep_no2003$BIC[1:9])[2]])

## Look at order of StandardConfig and print it
BIC_SM_Sep_no2003[order(BIC_SM_Sep_no2003$BIC[1:9]),]
BIC_order_Sep_no2003_standard <- BIC_SM_Sep_no2003[order(BIC_SM_Sep_no2003$BIC[1:9]),]
config <- rep("Standard_Config", 9)
BIC_order_Sep_no2003_standard <- cbind(BIC_order_Sep_no2003_standard, config)

capture.output(BIC_order_Sep_no2003_standard, file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/BIC_order_Sep_no2003", append=T)

## Extract position of smallest and second smallest BIC of Standard Config ##
BIC_SM_Sep_no2003[order(BIC_SM_Sep_no2003$BIC[1:9])[1],][[4]]
BIC_SM_Sep_no2003[order(BIC_SM_Sep_no2003$BIC[1:9])[2],][[4]]


## Plot BIC Distribution of Standard Configuration ##
plot(BIC_SMIPrecTavg)

############################################################################################
## If difference is larger than six between first and second best model, take first model ##
## However, it can not really be assumed that there is no difference though


# if (x > 6)
# {
model_standard <- print("Best Formula (Standard Configuration) - Best BIC")
model_standard

#  Extract model index
BIC_SM_Sep_no2003[order(BIC_SM_Sep_no2003$BIC[1:9])[1],][[3]]


## Determine degree 
r = BIC_SM_Sep_no2003[order(BIC_SM_Sep_no2003$BIC[1:9])[1],][[4]]

## Extract model with lowest BIC of the models with that particular model index 
order(BIC_SM_Sep_no2003[BIC_SM_Sep_no2003$model_index==BIC_SM_Sep_no2003[order(BIC_SM_Sep_no2003$BIC[1:9])[1],][[3]],][,1])[1]


## Determine adequate formula for model by the means of the model index
bestStandard_formula <-  formula_list_Sep_no2003_sm_detrendlog[[BIC_SM_Sep_no2003[order(BIC_SM_Sep_no2003$BIC[1:9])[1],][[3]]]]
bestStandard_formula


# } else {
#   
#   #############################################################
#   ## If difference is smaller than 6, take second best model ##
#   model_standard <- print("Standard Formula (Standard Configuration) - Second Best BIC")
#   model_standard
#   
#   #  Extract model index
#   BIC_SM_Sep_no2003[order(BIC_SM_Sep_no2003$BIC[1:9])[2],][[3]]
#   
#   ## Determine degree 
#   r = BIC_SM_Sep_no2003[order(BIC_SM_Sep_no2003$BIC[1:9])[2],][[4]]
#   
#   ## Extract model with lowest BIC of the models with that particular model index 
#   order(BIC_SM_Sep_no2003[BIC_SM_Sep_no2003$model_index==BIC_SM_Sep_no2003[order(BIC_SM_Sep_no2003$BIC[1:9])[1],][[3]],][,1])[2]
#   
#   ## Determine adequate formula for model by the means of the model index
#   bestStandard_formula <-  formula_list_Sep_no2003_sm_detrendlog[[BIC_SM_Sep_no2003[order(BIC_SM_Sep_no2003$BIC[1:9])[2],][[3]]]]
#   bestStandard_formula
#   
# }
capture.output(model_standard, file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/BIC_order_Sep_no2003", append=T)
capture.output(bestStandard_formula, file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/BIC_order_Sep_no2003", append=T)
r

###################
## GLM Ergebniss ##
glm.fit_SM_bestStandard_Sep_no2003  <- glm(formula = bestStandard_formula,  data = Yield_Covariates_SM_Sep_no2003)

## Summary
bestStandard_glm_summary <- summary(glm.fit_SM_bestStandard_Sep_no2003)
capture.output(bestStandard_glm_summary, file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/Output_SM_Sep_no2003_qual", append=T)

## AIC
bestStandard_glm_summary_qual <- print(c("bestStandard_glm_AIC:", round(bestStandard_glm_summary$aic,4)))
capture.output(print("BEST STANDARD MODELS"), file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/Output_SM_Sep_no2003_qual.txt", append=T)
capture.output(bestStandard_glm_summary_qual, file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/Output_SM_Sep_no2003_qual.txt", append=T)

summary(glm.fit_SM_bestStandard_Sep_no2003)
'AIC:-6410.5'

####################
## PLM Ergebnisse ##
plm.fit_SM_bestStandard_Sep_no2003 <- plm(formula = update(bestStandard_formula, .~. - dummy(comId)),  data = Yield_Covariates_SM_Sep_no2003,  effect="individual", model=("within"), index = c("comId","year"))

## Output
bestStandard_plm_summary <- summary(plm.fit_SM_bestStandard_Sep_no2003)
bestStandard_plm_summary
capture.output(bestStandard_plm_summary, file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/Output_SM_Sep_no2003_qual", append=T)

bestStandard_plm_summary_qual <- print(c("bestStandard_plm:", round(bestStandard_plm_summary$r.squared,4)))
capture.output(bestStandard_plm_summary_qual, file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/Output_SM_Sep_no2003_qual.txt", append=T)

'Adj. R-Squared: 0.040653'

## Fixed Effects ##
fixef <-  fixef(plm.fit_SM_bestStandard_Sep_no2003)
fixef_summary <- as.matrix(summary(fixef))
capture.output(fixef_summary, file = "./figures/figures_exploratory/FixedEffects/Silomaize/Sep_no2003/plm.fit_SM_bestStandard_Sep_no2003_FE_summary.txt")
# fixef_summary <- read.table( file = "./figures/figures_exploratory/FixedEffects/Silomaize/Sep_no2003/plm.fit_SM_BEST_Sep_no2003_FE_summary.txt")

##################
## LM Ergebniss ##
lm.fit_SM_bestStandard_Sep_no2003  <-lm(formula = bestStandard_formula,  data = Yield_Covariates_SM_Sep_no2003)
summary(lm.fit_SM_bestStandard_Sep_no2003) 
'Adjusted R-squared:  0.6276'

bestStandard_lm_summary <- summary(lm.fit_SM_bestStandard_Sep_no2003) 
bestStandard_lm_summary$r.squared
capture.output(bestStandard_lm_summary, file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/Output_SM_Sep_no2003_qual", append=T)


bestStandard_lm_summary_qual1 <- print(c("bestStandard_lm_r.squared:", round(bestStandard_lm_summary$r.squared,4)))
bestStandard_lm_summary_qual2 <- print(c("bestStandard_lm_adj.r.squared:", round(bestStandard_lm_summary$adj.r.squared,4)))
capture.output(bestStandard_lm_summary_qual1, file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/Output_SM_Sep_no2003_qual.txt", append=T)
capture.output(bestStandard_lm_summary_qual2, file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/Output_SM_Sep_no2003_qual.txt", append=T)

########################
## Heteroskedasdicity ##
bptest(glm.fit_SM_bestStandard_Sep_no2003) # Breusch Pagan Test of Heteroskedastie in den Störgrößen: Null: Homoskedasdicity.
bptest(plm.fit_SM_bestStandard_Sep_no2003)
' In beiden Fällen kann die Null widerlegt werden. Es gibt also heteroskedasdicity '

## Koenkers Version on BP Test: robuste Modification wenn die Störgrößen nicht normalverteilt sind. 
bptest(plm.fit_SM_bestStandard_Sep_no2003, studentize = TRUE)
'Auch hier kann die Null widerlegt werden. Need to use robust covariance variance matrix to correct standard errors'

#########################
#### Autocorrelation ####

######################################
## Tests for serial autocorrelation ##
pwartest(plm.fit_SM_bestStandard_Sep_no2003)
' Hier serielle Korrelation festzustellen'
pbgtest(plm.fit_SM_bestStandard_Sep_no2003) 

'Solution for serial correlation: Cluster by groups.
Solution for cross sectional correlation: Cluster by time'

#################################
## Correct the Standard Errors ##

## Correct Standard Errors used in table ##
coeftest(plm.fit_SM_bestStandard_Sep_no2003)

## Robust covariance matrix estimators a la White 
# coeftest(plm.fit_SM_bestStandard_Sep_no2003,vcov=vcovHC(plm.fit_SM_bestStandard_Sep_no2003,method = "arellano", type = "HC0")) 
cov0_SM_bestStandard_Sep_no2003 <- vcovHC(plm.fit_SM_bestStandard_Sep_no2003,method = "arellano", type = "HC0", cluster="group")
Wh.se_serial_SM_bestStandard_Sep_no2003 <- sqrt(diag(cov0_SM_bestStandard_Sep_no2003))

cov0.1_SM_bestStandard_Sep_no2003 <- vcovHC(plm.fit_SM_bestStandard_Sep_no2003,method = "arellano", type = "HC0", cluster="time")
Wh.se_cross_SM_bestStandard_Sep_no2003 <- sqrt(diag(cov0.1_SM_bestStandard_Sep_no2003))

# ## Beck Katz ##
# # coeftest(plm.fit_SM_bestStandard_Sep_no2003, vcov = function(x) vcovBK(plm.fit_SM_bestStandard_Sep_no2003,method = "arellano", type = "HC0"))
# cov1_SM_bestStandard_Sep_no2003 <- vcovBK(plm.fit_SM_bestStandard_Sep_no2003,method = "arellano", type = "HC0",  cluster="time")
# BK.se_SM_bestStandard_Sep_no2003 <- sqrt(diag(cov1_SM_bestStandard_Sep_no2003))

## Driscoll Kraay: ##
summary(plm.fit_SM_bestStandard_Sep_no2003)
cov2_SM_bestStandard_Sep_no2003     <- vcovSCC(plm.fit_SM_bestStandard_Sep_no2003,method = "arellano",type = "HC0")
DK.se_SM_bestStandard_Sep_no2003    <- sqrt(diag(cov2_SM_bestStandard_Sep_no2003))

# cov2.1_SM_bestStandard_Sep_no2003   <- vcovSCC(plm.fit_SM_bestStandard_Sep_no2003,method = "arellano",type = "HC0", maxlag=1)
# DK2.1.se_SM_bestStandard_Sep_no2003 <- sqrt(diag(cov2.1_SM_bestStandard_Sep_no2003))

# cov2.2_SM_bestStandard_Sep_no2003   <- vcovSCC(plm.fit_SM_bestStandard_Sep_no2003,method = "arellano",type = "HC0", maxlag=2)
# DK2.2.se_SM_bestStandard_Sep_no2003 <- sqrt(diag(cov2.2_SM_bestStandard_Sep_no2003))
# 
# cov2.3_SM_bestStandard_Sep_no2003   <- vcovSCC(plm.fit_SM_bestStandard_Sep_no2003,method = "arellano",type = "HC0", maxlag=3)
# DK2.3.se_SM_bestStandard_Sep_no2003 <- sqrt(diag(cov2.3_SM_bestStandard_Sep_no2003))
# 
# cov2.4_SM_bestStandard_Sep_no2003   <- vcovSCC(plm.fit_SM_bestStandard_Sep_no2003,method = "arellano",type = "HC0", maxlag=4)
# DK2.4.se_SM_bestStandard_Sep_no2003 <- sqrt(diag(cov2.4_SM_bestStandard_Sep_no2003))
# 
cov2.5_SM_bestStandard_Sep_no2003   <- vcovSCC(plm.fit_SM_bestStandard_Sep_no2003,method = "arellano",type = "HC0", maxlag=5)
DK2.5.se_SM_bestStandard_Sep_no2003 <- sqrt(diag(cov2.5_SM_bestStandard_Sep_no2003))

## Cameron et al /Thompson : doouble-clustering estimator 
# coeftest(plm.fit_SM_bestStandard_Sep_no2003, vcovDC(plm.fit_SM_bestStandard_Sep_no2003, method = "arellano", type = "HC0"))
cov3_SM_bestStandard_Sep_no2003 <- vcovDC(plm.fit_SM_bestStandard_Sep_no2003, method = "arellano", type = "HC0")
CT.se_SM_bestStandard_Sep_no2003 <- sqrt(diag(cov3_SM_bestStandard_Sep_no2003))
'Our estimator is qualitatively similar to the ones presented in White and Domowitz (1984), for
time series data, and Conley (1999), for spatial data. '

## Generate Table with Output ##
se <- list(NULL, Wh.se_cross_SM_bestStandard_Sep_no2003, Wh.se_serial_SM_bestStandard_Sep_no2003, DK.se_SM_bestStandard_Sep_no2003, DK2.5.se_SM_bestStandard_Sep_no2003, CT.se_SM_bestStandard_Sep_no2003)
labels1 <-c("NULL","WhiteCross","WhiteSerial", "DriscollKraay", "DriscollKray2.5","CameronThompson")
stargazer(plm.fit_SM_bestStandard_Sep_no2003, plm.fit_SM_bestStandard_Sep_no2003, plm.fit_SM_bestStandard_Sep_no2003, plm.fit_SM_bestStandard_Sep_no2003, plm.fit_SM_bestStandard_Sep_no2003,plm.fit_SM_bestStandard_Sep_no2003,
          se = se,   
          dep.var.caption  = "Model with smallest BIC (Standard Configuration) - September_no2003",
          dep.var.labels = "log(Silomaize)",
          style="default",
          model.numbers = FALSE,
          column.labels = labels1,          
          type="text", out="./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/SM_Sep_no2003_bestStandard.txt"
)

########################################
## Results with smallest BIC with SMI ##
########################################

## Look for smallest BIC with SMI
BIC_SM_Sep_no2003_onlySMI<- BIC_SM_Sep_no2003[1:28,]
BIC_SM_Sep_no2003_onlySMI[which.min(BIC_SM_Sep_no2003_onlySMI$BIC),]
BIC_SM_Sep_no2003_onlySMI[order(BIC_SM_Sep_no2003_onlySMI$BIC)[1],]


## Define difference between first and second best BIC Value ##
-(BIC_SM_Sep_no2003_onlySMI$BIC[order(BIC_SM_Sep_no2003_onlySMI$BIC)[1]] -  BIC_SM_Sep_no2003_onlySMI$BIC[order(BIC_SM_Sep_no2003_onlySMI$BIC)[2]])

## Make condition for if ... else loop
-(BIC_SM_Sep_no2003_onlySMI$BIC[order(BIC_SM_Sep_no2003_onlySMI$BIC)[1]] -  BIC_SM_Sep_no2003_onlySMI$BIC[order(BIC_SM_Sep_no2003_onlySMI$BIC)[2]])
x <--(BIC_SM_Sep_no2003_onlySMI$BIC[order(BIC_SM_Sep_no2003_onlySMI$BIC)[1]] -  BIC_SM_Sep_no2003_onlySMI$BIC[order(BIC_SM_Sep_no2003_onlySMI$BIC)[2]])

## Look at order of smalles BIC of Models with SMI and print it
BIC_SM_Sep_no2003_onlySMI[order(BIC_SM_Sep_no2003_onlySMI$BIC),]
BIC_order_Sep_no2003_BestSMI <- BIC_SM_Sep_no2003_onlySMI[order(BIC_SM_Sep_no2003_onlySMI$BIC),]
config <- rep("Best_SMI_config", 28)
BIC_order_Sep_no2003_BestSMI <- cbind(BIC_order_Sep_no2003_BestSMI, config)

capture.output(BIC_order_Sep_no2003_BestSMI, file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/BIC_order_Sep_no2003", append=T)

## Extract position of smallest and second smallest BIC of Standard Config ##
BIC_SM_Sep_no2003_onlySMI[order(BIC_SM_Sep_no2003_onlySMI$BIC)[1],][[4]]
BIC_SM_Sep_no2003_onlySMI[order(BIC_SM_Sep_no2003_onlySMI$BIC)[2],][[4]]



############################################################################################
## If difference is larger than six between first and second best model, take first model ##
## However, it can not really be assumed that there is no difference though

# if ( x > 6)
# {
model_bestSMI <- print("Best Formula (with SMI) - Best BIC")
model_bestSMI
#  Extract model index
BIC_SM_Sep_no2003_onlySMI[order(BIC_SM_Sep_no2003_onlySMI$BIC)[1],][[3]]

## Extract model with lowest BIC of the models with that particular model index 
order(BIC_SM_Sep_no2003_onlySMI[BIC_SM_Sep_no2003_onlySMI$model_index==BIC_SM_Sep_no2003_onlySMI[order(BIC_SM_Sep_no2003_onlySMI$BIC)[1],][[3]],][,1])[1]

## Determine degree 
r = order(BIC_SM_Sep_no2003_onlySMI[BIC_SM_Sep_no2003_onlySMI$model_index==BIC_SM_Sep_no2003_onlySMI[order(BIC_SM_Sep_no2003_onlySMI$BIC)[1],][[3]],][,1])[1]

## Determine adequate formula for model by the means of the model index
bestSMI_formula <-  formula_list_Sep_no2003_sm_detrendlog[[BIC_SM_Sep_no2003_onlySMI[order(BIC_SM_Sep_no2003_onlySMI$BIC)[1],][[3]]]]
bestSMI_formula

#   
# } else {
#   
#   #############################################################
#   ## If difference is smaller than 6, take second best model ##
#   model_bestSMI <- print("Best Formula (with SMI) - Second Best BIC")
#   model_bestSMI
#   ## Look at second best model ##
#   BIC_SM_Sep_no2003_onlySMI[order(BIC_SM_Sep_no2003_onlySMI$BIC)[2],]
#   
#   ##  Extract model index
#   BIC_SM_Sep_no2003_onlySMI[order(BIC_SM_Sep_no2003_onlySMI$BIC)[2],][[3]]
#   
#   ## Extract model with second lowest BIC of the models with that particular model index 
#   order(BIC_SM_Sep_no2003_onlySMI[BIC_SM_Sep_no2003_onlySMI$model_index==BIC_SM_Sep_no2003_onlySMI[order(BIC_SM_Sep_no2003_onlySMI$BIC)[1],][[3]],][,1])[2]
#   
#   ## Determine degree 
#   r = order(BIC_SM_Sep_no2003_onlySMI[BIC_SM_Sep_no2003_onlySMI$model_index==BIC_SM_Sep_no2003_onlySMI[order(BIC_SM_Sep_no2003_onlySMI$BIC)[1],][[3]],][,1])[2]
#   ## Determine adequate formula for model by the means of the model index
#   bestSMI_formula <-  formula_list_Sep_no2003_sm_detrendlog[[BIC_SM_Sep_no2003_onlySMI[order(BIC_SM_Sep_no2003_onlySMI$BIC)[2],][[3]]]]
#   bestSMI_formula
#   
# }
capture.output(model_bestSMI, file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/BIC_order_Sep_no2003", append=T)
capture.output(bestSMI_formula, file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/BIC_order_Sep_no2003", append=T)
r

###################
## GLM Ergebniss ##
glm.fit_SM_bestSMI_Sep_no2003  <- glm(formula = bestSMI_formula,  data = Yield_Covariates_SM_Sep_no2003)

## Summary
bestSMI_glm_summary <- summary(glm.fit_SM_bestSMI_Sep_no2003)
bestSMI_glm_summary
capture.output(bestSMI_glm_summary, file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/Output_SM_Sep_no2003_qual", append=T)

## AIC
capture.output(print("BEST SMI MODELS"), file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/Output_SM_Sep_no2003_qual.txt", append=T)
bestSMI_glm_summary_qual <- print(c("bestSMI_glm_AIC:", round(bestSMI_glm_summary$aic,4)))
capture.output(bestSMI_glm_summary_qual, file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/Output_SM_Sep_no2003_qual.txt", append=T)

####################
## PLM Ergebnisse ##
plm.fit_SM_bestSMI_Sep_no2003 <- plm(formula = update(bestSMI_formula, .~. - dummy(comId)),  data = Yield_Covariates_SM_Sep_no2003,  effect="individual", model=("within"), index = c("comId","year"))

## Output
bestSMI_plm_summary <- summary(plm.fit_SM_bestSMI_Sep_no2003)
bestSMI_plm_summary
capture.output(bestSMI_plm_summary, file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/Output_SM_Sep_no2003_qual", append=T)

bestSMI_plm_summary_qual <- print(c("bestSMI_plm:", round(bestSMI_plm_summary$r.squared,4)))
capture.output(bestSMI_plm_summary_qual, file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/Output_SM_Sep_no2003_qual.txt", append=T)


## Fixed Effects ##
fixef <-  fixef(plm.fit_SM_bestSMI_Sep_no2003)
fixef_summary <- as.matrix(summary(fixef))
capture.output(fixef_summary, file = "./figures/figures_exploratory/FixedEffects/Silomaize/Sep_no2003/plm.fit_SM_bestSMI_Sep_no2003_FE_summary.txt")
# fixef_summary <- read.table( file = "./figures/figures_exploratory/FixedEffects/Silomaize/Sep_no2003/plm.fit_SM_BEST_Sep_no2003_FE_summary.txt")

##################
## LM Ergebniss ##
lm.fit_SM_bestSMI_Sep_no2003  <-lm(formula = bestSMI_formula,  data = Yield_Covariates_SM_Sep_no2003)
summary(lm.fit_SM_bestSMI_Sep_no2003) 
'Adjusted R-squared:  0.6276'

bestSMI_lm_summary <- summary(lm.fit_SM_bestSMI_Sep_no2003) 
capture.output(bestSMI_lm_summary, file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/Output_SM_Sep_no2003_qual", append=T)


bestSMI_lm_summary_qual1 <- print(c("bestSMI_lm_r.squared:", round(bestSMI_lm_summary$r.squared,4)))
bestSMI_lm_summary_qual2 <- print(c("bestSMI_lm_adj.r.squared:", round(bestSMI_lm_summary$adj.r.squared,4)))
capture.output(bestSMI_lm_summary_qual1, file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/Output_SM_Sep_no2003_qual.txt", append=T)
capture.output(bestSMI_lm_summary_qual2, file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/Output_SM_Sep_no2003_qual.txt", append=T)

########################
## Heteroskedasdicity ##
bptest(glm.fit_SM_bestSMI_Sep_no2003) # Breusch Pagan Test of Heteroskedastie in den Störgrößen: Null: Homoskedasdicity.
bptest(plm.fit_SM_bestSMI_Sep_no2003)
' In beiden Fällen kann die Null widerlegt werden. Es gibt also heteroskedasdicity '

## Koenkers Version on BP Test: robuste Modification wenn die Störgrößen nicht normalverteilt sind. 
bptest(plm.fit_SM_bestSMI_Sep_no2003, studentize = TRUE)
'Auch hier kann die Null widerlegt werden. Need to use robust covariance variance matrix to correct standard errors'

#########################
#### Autocorrelation ####

######################################
## Tests for serial autocorrelation ##
pwartest(plm.fit_SM_bestSMI_Sep_no2003)
pbgtest(plm.fit_SM_bestSMI_Sep_no2003) 
'Hier serielle Korrelation festzustellen'

###########################################
## Correct Standard Errors used in table ##
coeftest(plm.fit_SM_bestSMI_Sep_no2003)

## Robust covariance matrix estimators a la White ##
# coeftest(plm.fit_SM_bestSMI_Sep_no2003,vcov=vcovHC(plm.fit_SM_bestSMI_Sep_no2003,method = "arellano", type = "HC0")) 
cov0_SM_bestSMI_Sep_no2003 <- vcovHC(plm.fit_SM_bestSMI_Sep_no2003,method = "arellano", type = "HC0", cluster="group")
Wh.se_serial_SM_bestSMI_Sep_no2003 <- sqrt(diag(cov0_SM_bestSMI_Sep_no2003))

cov0.1_SM_bestSMI_Sep_no2003 <- vcovHC(plm.fit_SM_bestSMI_Sep_no2003,method = "arellano", type = "HC0", cluster="time")
Wh.se_cross_SM_bestSMI_Sep_no2003 <- sqrt(diag(cov0.1_SM_bestSMI_Sep_no2003))
# 
# ## Beck Katz:
# # coeftest(plm.fit_SM_bestSMI_Sep_no2003, vcov = function(x) vcovBK(plm.fit_SM_bestSMI_Sep_no2003,method = "arellano", type = "HC0"))
# cov1 <- vcovBK(plm.fit_SM_bestSMI_Sep_no2003,method = "arellano", type = "HC0",  cluster="time")
# BK.se <- sqrt(diag(cov1))

## Driscoll Kraay ##
# summary(plm.fit_SM_bestSMI_Sep_no2003)
cov2_SM_bestSMI_Sep_no2003     <- vcovSCC(plm.fit_SM_bestSMI_Sep_no2003,method = "arellano",type = "HC0")
DK.se_SM_bestSMI_Sep_no2003    <- sqrt(diag(cov2_SM_bestSMI_Sep_no2003))

# cov2.1_SM_bestSMI_Sep_no2003   <- vcovSCC(plm.fit_SM_bestSMI_Sep_no2003,method = "arellano",type = "HC0", maxlag=1)
# DK2.1.se_SM_bestSMI_Sep_no2003 <- sqrt(diag(cov2.1_SM_bestSMI_Sep_no2003))

# cov2.2_SM_bestSMI_Sep_no2003   <- vcovSCC(plm.fit_SM_bestSMI_Sep_no2003,method = "arellano",type = "HC0", maxlag=2)
# DK2.2.se_SM_bestSMI_Sep_no2003 <- sqrt(diag(cov2.2_SM_bestSMI_Sep_no2003))
# 
# cov2.3_SM_bestSMI_Sep_no2003   <- vcovSCC(plm.fit_SM_bestSMI_Sep_no2003,method = "arellano",type = "HC0", maxlag=3)
# DK2.3.se_SM_bestSMI_Sep_no2003 <- sqrt(diag(cov2.3_SM_bestSMI_Sep_no2003))
# 
# cov2.4_SM_bestSMI_Sep_no2003   <- vcovSCC(plm.fit_SM_bestSMI_Sep_no2003,method = "arellano",type = "HC0", maxlag=4)
# DK2.4.se_SM_bestSMI_Sep_no2003 <- sqrt(diag(cov2.4_SM_bestSMI_Sep_no2003))
# 
cov2.5_SM_bestSMI_Sep_no2003   <- vcovSCC(plm.fit_SM_bestSMI_Sep_no2003,method = "arellano",type = "HC0", maxlag=5)
DK2.5.se_SM_bestSMI_Sep_no2003 <- sqrt(diag(cov2.5_SM_bestSMI_Sep_no2003))

' Achtung: Driscoll Kraay ist nicht geeignet für kurze Panel. Ist das bei mir der Fall? '

## Cameron et al /Thompson : doouble-clustering estimator ##
cov3_SM_bestSMI_Sep_no2003 <- vcovDC(plm.fit_SM_bestSMI_Sep_no2003, method = "arellano", type = "HC0")
CT.se_SM_bestSMI_Sep_no2003 <- sqrt(diag(cov3_SM_bestSMI_Sep_no2003))

################################
## Generate Table with Output ##
se <- list(NULL, Wh.se_cross_SM_bestSMI_Sep_no2003, Wh.se_serial_SM_bestSMI_Sep_no2003,  DK.se_SM_bestSMI_Sep_no2003, DK2.5.se_SM_bestSMI_Sep_no2003, CT.se_SM_bestSMI_Sep_no2003)
labels1 <-c("NULL","WhiteCross","WhiteSerial", "DriscollKraay", "DriscollKray2.5","CameronThompson")
stargazer(plm.fit_SM_bestSMI_Sep_no2003, plm.fit_SM_bestSMI_Sep_no2003, plm.fit_SM_bestSMI_Sep_no2003, plm.fit_SM_bestSMI_Sep_no2003, plm.fit_SM_bestSMI_Sep_no2003,plm.fit_SM_bestSMI_Sep_no2003,
          se = se,   
          dep.var.caption  = "Model with smallest BIC (with SMI) - September_no2003",
          dep.var.labels = "log(Silomaize)",
          style="default",
          model.numbers = FALSE,
          column.labels = labels1,          
          type="text", out="./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/SM_Sep_no2003_bestSM.txt"
)

###########################################
## Results with smallest BIC without PET ##
###########################################

## Look for smallest BIC without PET

BIC_SM_Sep_no2003_noPet <- subset(BIC_SM_Sep_no2003, model_index==c(1) |  model_index==c(3)|  model_index==c(5)|  model_index==c(6)|  model_index==c(7)|  model_index==c(8)|  model_index==c(10))
rownames(BIC_SM_Sep_no2003_noPet ) <- NULL
BIC_SM_Sep_no2003_noPet
## Substitute Model Indeces ##
BIC_SM_Sep_no2003_noPet$model_index <- c(1,1,1,1,1,1,1,1,1,
                                         2,2,2,
                                         3,3,3,
                                         4,
                                         5,5,5,
                                         6,6,6,
                                         7,7,7,7,7,7,7,7,7)
BIC_SM_Sep_no2003_noPet$model <- c("01_SMIPrecTavg","01_SMIPrecTavg","01_SMIPrecTavg","01_SMIPrecTavg","01_SMIPrecTavg","01_SMIPrecTavg","01_SMIPrecTavg","01_SMIPrecTavg","01_SMIPrecTavg",
                                   "02_SMIPrec","02_SMIPrec","02_SMIPrec",
                                   "03_SMITavg","03_SMITavg","03_SMITavg",
                                   "04_SMI",
                                   "05_Prec","05_Prec","05_Prec",
                                   "06_Tavg","06_Tavg","06_Tavg",
                                   "07_PrecTavg","07_PrecTavg","07_PrecTavg","07_PrecTavg","07_PrecTavg","07_PrecTavg","07_PrecTavg","07_PrecTavg","07_PrecTavg")

BIC_SM_Sep_no2003_noPet$index <- c(1:31)

## Adapt Formula Model List ##
formula_list_Sep_no2003_sm_detrendlog_noPet <- formula_list_Sep_no2003_sm_detrendlog[c(1,3,5,6,7,8,10)]

## Save BIC Data without PET
write.csv(BIC_SM_Sep_no2003_noPet, "./data/data_raw/BIC/BIC_SM_Sep_no2003_noPet.csv")

## Chechout models with smallest BIC
BIC_SM_Sep_no2003_noPet[which.min(BIC_SM_Sep_no2003_noPet$BIC),]
BIC_SM_Sep_no2003_noPet[order(BIC_SM_Sep_no2003_noPet$BIC)[1],]
BIC_SM_Sep_no2003_noPet[order(BIC_SM_Sep_no2003_noPet$BIC)[2],]

BIC_SM_Sep_no2003_noPet$BIC[order(BIC_SM_Sep_no2003_noPet$BIC)]

## Define difference between first and second best BIC Value ##
-(BIC_SM_Sep_no2003_noPet$BIC[order(BIC_SM_Sep_no2003_noPet$BIC)[1]] -  BIC_SM_Sep_no2003_noPet$BIC[order(BIC_SM_Sep_no2003_noPet$BIC)[2]])

## Make condition for if ... else loop
-(BIC_SM_Sep_no2003_noPet$BIC[order(BIC_SM_Sep_no2003_noPet$BIC)[1]] -  BIC_SM_Sep_no2003_noPet$BIC[order(BIC_SM_Sep_no2003_noPet$BIC)[2]])
x <- -(BIC_SM_Sep_no2003_noPet$BIC[order(BIC_SM_Sep_no2003_noPet$BIC)[1]] -  BIC_SM_Sep_no2003_noPet$BIC[order(BIC_SM_Sep_no2003_noPet$BIC)[2]])

## Look at order 
BIC_SM_Sep_no2003_noPet[order(BIC_SM_Sep_no2003_noPet$BIC),]
BIC_order_Sep_no2003_noPet <- BIC_SM_Sep_no2003_noPet[order(BIC_SM_Sep_no2003_noPet$BIC),]
config <- rep("Best_noPet_config", 31)
BIC_order_Sep_no2003_noPet <- cbind(BIC_order_Sep_no2003_noPet, config)

capture.output(BIC_order_Sep_no2003_noPet, file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/BIC_order_Sep_no2003", append=T)

## Extract position of smallest and second smallest BIC of Standard Config ##
BIC_SM_Sep_no2003_noPet[order(BIC_SM_Sep_no2003_noPet$BIC)[1],][[4]]
BIC_SM_Sep_no2003_noPet[order(BIC_SM_Sep_no2003_noPet$BIC)[2],][[4]]





############################################################################################
## If difference is larger than six between first and second best model, take first model ##
## However, it can not really be assumed that there is no difference though


# if (x > 6)
# {
model_noPet <- print("Best Formula (no PET)- Best BIC")
model_noPet

#  Extract model index
BIC_SM_Sep_no2003_noPet[order(BIC_SM_Sep_no2003_noPet$BIC)[1],][[3]]

## Extract model with lowest BIC of the models with that particular model index 
order(BIC_SM_Sep_no2003_noPet[BIC_SM_Sep_no2003_noPet$model_index==BIC_SM_Sep_no2003_noPet[order(BIC_SM_Sep_no2003_noPet$BIC)[1],][[3]],][,1])[1]

## Determine degree 
r = order(BIC_SM_Sep_no2003_noPet[BIC_SM_Sep_no2003_noPet$model_index==BIC_SM_Sep_no2003_noPet[order(BIC_SM_Sep_no2003_noPet$BIC)[1],][[3]],][,1])[1]

## Determine adequate formula for model by the means of the model index
best_noPet_formula <-  formula_list_Sep_no2003_sm_detrendlog_noPet[[BIC_SM_Sep_no2003_noPet[order(BIC_SM_Sep_no2003_noPet$BIC)[1],][[3]]]]
best_noPet_formula

#   
# } else {
#   
#   #############################################################
#   ## If difference is smaller than 6, take second best model ##
#   model_noPet <- print("Best Formula (no PET)  -  Second Best BIC")
#   model_noPet
#   
#   ## Look at second best model ##
#   BIC_SM_Sep_no2003_noPet[order(BIC_SM_Sep_no2003_noPet$BIC)[2],]
#   
#   ##  Extract model index
#   BIC_SM_Sep_no2003_noPet[order(BIC_SM_Sep_no2003_noPet$BIC)[2],][[3]]
#   
#   ## Extract model with second lowest BIC of the models with that particular model index 
#   order(BIC_SM_Sep_no2003_noPet[BIC_SM_Sep_no2003_noPet$model_index==BIC_SM_Sep_no2003_noPet[order(BIC_SM_Sep_no2003_noPet$BIC)[1],][[3]],][,1])[2]
#   
#   ## Determine degree 
#   r = order(BIC_SM_Sep_no2003_noPet[BIC_SM_Sep_no2003_noPet$model_index==BIC_SM_Sep_no2003_noPet[order(BIC_SM_Sep_no2003_noPet$BIC)[1],][[3]],][,1])[2]
#   
#   ## Determine adequate formula for model by the means of the model index
#   best_noPet_formula <-  formula_list_Sep_no2003_sm_detrendlog_noPet[[BIC_SM_Sep_no2003_noPet[order(BIC_SM_Sep_no2003_noPet$BIC)[2],][[3]]]]
#   best_noPet_formula
#   
# }
capture.output(model_noPet, file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/BIC_order_Sep_no2003", append=T)
capture.output(best_noPet_formula, file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/BIC_order_Sep_no2003", append=T)
r

###################
## GLM Ergebniss ##
glm.fit_SM_noPet_Sep_no2003  <- glm(formula = best_noPet_formula,  data = Yield_Covariates_SM_Sep_no2003)

## Summary
best_noPet_glm_summary <- summary(glm.fit_SM_noPet_Sep_no2003)
best_noPet_glm_summary
capture.output(best_noPet_glm_summary, file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/Output_SM_Sep_no2003_qual", append=T)


## AIC
best_noPet_glm_summary_qual <- print(c("best_noPet_glm_AIC:", round(best_noPet_glm_summary$aic,4)))

capture.output(print("BEST NO PET MODEL"), file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/Output_SM_Sep_no2003_qual.txt", append=T)
capture.output(best_noPet_glm_summary_qual, file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/Output_SM_Sep_no2003_qual.txt", append=T)

####################
## PLM Ergebnisse ##
plm.fit_SM_noPet_Sep_no2003 <- plm(formula = update(best_noPet_formula, .~. - dummy(comId)),  data = Yield_Covariates_SM_Sep_no2003,  effect="individual", model=("within"), index = c("comId","year"))

## Output
best_noPet_plm_summary <- summary(plm.fit_SM_noPet_Sep_no2003)
best_noPet_plm_summary
capture.output(best_noPet_plm_summary, file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/Output_SM_Sep_no2003_qual", append=T)

best_noPet_plm_summary_qual <- print(c("best_noPet_plm:", round(best_noPet_plm_summary$r.squared,4)))
capture.output(best_noPet_plm_summary_qual, file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/Output_SM_Sep_no2003_qual.txt", append=T)


## Fixed Effects ##
fixef <-  fixef(plm.fit_SM_noPet_Sep_no2003)
fixef_summary <- as.matrix(summary(fixef))
capture.output(fixef_summary, file = "./figures/figures_exploratory/FixedEffects/Silomaize/Sep_no2003/plm.fit_SM_noPet_Sep_no2003_FE_summary.txt")
# fixef_summary <- read.table( file = "./figures/figures_exploratory/FixedEffects/Silomaize/Sep_no2003/plm.fit_SM_BEST_Sep_no2003_FE_summary.txt")

##################
## LM Ergebniss ##
lm.fit_SM_noPet_Sep_no2003  <-lm(formula = best_noPet_formula,  data = Yield_Covariates_SM_Sep_no2003)
summary(lm.fit_SM_noPet_Sep_no2003) 

best_noPet_lm_summary <- summary(lm.fit_SM_noPet_Sep_no2003) 
capture.output(best_noPet_lm_summary, file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/Output_SM_Sep_no2003_qual", append=T)


best_noPet_lm_summary_qual1 <- print(c("best_noPet_lm_r.squared:", round(best_noPet_lm_summary$r.squared,4)))
best_noPet_lm_summary_qual2 <- print(c("best_noPet_lm_adj.r.squared:", round(best_noPet_lm_summary$adj.r.squared,4)))
capture.output(best_noPet_lm_summary_qual1, file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/Output_SM_Sep_no2003_qual.txt", append=T)
capture.output(best_noPet_lm_summary_qual2, file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/Output_SM_Sep_no2003_qual.txt", append=T)

################################################
## Assessing Influence (Leverage*discrepancy) ##
cutoff_SM_Sep_no2003 <- 4/((nrow(Yield_Covariates_SM_Sep_no2003)-length(lm.fit_SM_noPet_Sep_no2003$coefficients)-1)) 
cutoff_SM_Sep_no2003
plot(lm.fit_SM_noPet_Sep_no2003, which=4)
cook_Sep_no2003 <- cooks.distance(lm.fit_SM_noPet_Sep_no2003)

nrow_cooks <- nrow(Yield_Covariates_SM_Sep_no2003[cook_Sep_no2003 > cutoff_SM_Sep_no2003,]) 
capture.output(nrow_cooks, file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/best_SM_Sep_no2003_cooks_noPet.txt")

year_cooks_SM_Sep_no2003 <- table(Yield_Covariates_SM_Sep_no2003$year[cook_Sep_no2003 > cutoff_SM_Sep_no2003 ]) 
year_cooks_SM_Sep_no2003
capture.output(year_cooks_SM_Sep_no2003, file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/best_SM_Sep_no2003_cooks_noPet.txt", append=T)

com_cooks_SM_Sep_no2003 <- sort(table(Yield_Covariates_SM_Sep_no2003$com[cook_Sep_no2003 > cutoff_SM_Sep_no2003 ] ))
capture.output(com_cooks_SM_Sep_no2003, file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/best_SM_Sep_no2003_cooks_noPet.txt", append=T)


########################
## Heteroskedasdicity ##
bptest(glm.fit_SM_noPet_Sep_no2003) # Breusch Pagan Test of Heteroskedastie in den Störgrößen: Null: Homoskedasdicity.
bptest(plm.fit_SM_noPet_Sep_no2003)
' In beiden Fällen kann die Null widerlegt werden. Es gibt also heteroskedasdicity '

## Koenkers Version on BP Test: robuste Modification wenn die Störgrößen nicht normalverteilt sind. 
bptest(plm.fit_SM_noPet_Sep_no2003, studentize = TRUE)
'Auch hier kann die Null widerlegt werden. Need to use robust covariance variance matrix to correct standard errors'

#########################
#### Autocorrelation ####

######################################
## Tests for serial autocorrelation ##
pwartest(plm.fit_SM_noPet_Sep_no2003)
pbgtest(plm.fit_SM_noPet_Sep_no2003) 
'Hier serielle Korrelation festzustellen'

###########################################
## Correct Standard Errors used in table ##
coeftest(plm.fit_SM_noPet_Sep_no2003)

## Robust covariance matrix estimators a la White ##
# coeftest(plm.fit_SM_noPet_Sep_no2003,vcov=vcovHC(plm.fit_SM_noPet_Sep_no2003,method = "arellano", type = "HC0")) 
cov0_SM_noPet_Sep_no2003 <- vcovHC(plm.fit_SM_noPet_Sep_no2003,method = "arellano", type = "HC0", cluster="group")
Wh.se_serial_SM_noPet_Sep_no2003 <- sqrt(diag(cov0_SM_noPet_Sep_no2003))

cov0.1_SM_noPet_Sep_no2003 <- vcovHC(plm.fit_SM_noPet_Sep_no2003,method = "arellano", type = "HC0", cluster="time")
Wh.se_cross_SM_noPet_Sep_no2003 <- sqrt(diag(cov0.1_SM_noPet_Sep_no2003))
# 
# ## Beck Katz:
# # coeftest(plm.fit_SM_noPet_Sep_no2003, vcov = function(x) vcovBK(plm.fit_SM_noPet_Sep_no2003,method = "arellano", type = "HC0"))
# cov1 <- vcovBK(plm.fit_SM_noPet_Sep_no2003,method = "arellano", type = "HC0",  cluster="time")
# BK.se <- sqrt(diag(cov1))

## Driscoll Kraay ##
# summary(plm.fit_SM_noPet_Sep_no2003)
cov2_SM_noPet_Sep_no2003     <- vcovSCC(plm.fit_SM_noPet_Sep_no2003,method = "arellano",type = "HC0")
DK.se_SM_noPet_Sep_no2003    <- sqrt(diag(cov2_SM_noPet_Sep_no2003))

# cov2.1_SM_noPet_Sep_no2003   <- vcovSCC(plm.fit_SM_noPet_Sep_no2003,method = "arellano",type = "HC0", maxlag=1)
# DK2.1.se_SM_noPet_Sep_no2003 <- sqrt(diag(cov2.1_SM_noPet_Sep_no2003))

# cov2.2_SM_noPet_Sep_no2003   <- vcovSCC(plm.fit_SM_noPet_Sep_no2003,method = "arellano",type = "HC0", maxlag=2)
# DK2.2.se_SM_noPet_Sep_no2003 <- sqrt(diag(cov2.2_SM_noPet_Sep_no2003))
# 
# cov2.3_SM_noPet_Sep_no2003   <- vcovSCC(plm.fit_SM_noPet_Sep_no2003,method = "arellano",type = "HC0", maxlag=3)
# DK2.3.se_SM_noPet_Sep_no2003 <- sqrt(diag(cov2.3_SM_noPet_Sep_no2003))
# 
# cov2.4_SM_noPet_Sep_no2003   <- vcovSCC(plm.fit_SM_noPet_Sep_no2003,method = "arellano",type = "HC0", maxlag=4)
# DK2.4.se_SM_noPet_Sep_no2003 <- sqrt(diag(cov2.4_SM_noPet_Sep_no2003))
# 
cov2.5_SM_noPet_Sep_no2003   <- vcovSCC(plm.fit_SM_noPet_Sep_no2003,method = "arellano",type = "HC0", maxlag=5)
DK2.5.se_SM_noPet_Sep_no2003 <- sqrt(diag(cov2.5_SM_noPet_Sep_no2003))

' Achtung: Driscoll Kraay ist nicht geeignet für kurze Panel. Ist das bei mir der Fall? '

## Cameron et al /Thompson : doouble-clustering estimator ##
cov3_SM_noPet_Sep_no2003 <- vcovDC(plm.fit_SM_noPet_Sep_no2003, method = "arellano", type = "HC0")
CT.se_SM_noPet_Sep_no2003 <- sqrt(diag(cov3_SM_noPet_Sep_no2003))

################################
## Generate Table with Output ##
se <- list(NULL, Wh.se_cross_SM_noPet_Sep_no2003, Wh.se_serial_SM_noPet_Sep_no2003,  DK.se_SM_noPet_Sep_no2003, DK2.5.se_SM_noPet_Sep_no2003, CT.se_SM_noPet_Sep_no2003)
labels1 <-c("NULL","WhiteCross","WhiteSerial", "DriscollKraay", "DriscollKray2.5","CameronThompson")
stargazer(plm.fit_SM_noPet_Sep_no2003, plm.fit_SM_noPet_Sep_no2003, plm.fit_SM_noPet_Sep_no2003, plm.fit_SM_noPet_Sep_no2003, plm.fit_SM_noPet_Sep_no2003,plm.fit_SM_noPet_Sep_no2003,
          se = se,   
          dep.var.caption  = "Model with smallest BIC (no PET) - September_no2003",
          dep.var.labels = "log(Silomaize)",
          style="default",
          model.numbers = FALSE,
          column.labels = labels1,          
          type="text", out="./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/SM_Sep_no2003_best_noPet.txt"
)

################################################################
## Generate output of Standard Model with highest Polynomials ##
r <- 9
formula_Sep_sm_detrendlog_SMIPrecTavg <- siloMaize_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  poly(Tavg, degree[r, 2], raw = T) + 
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormally dry", "abnormally wet" ,"abundantly wet", "severely wet")) 

plm.fit_SM_BestStdFull_Sep_no2003 <- plm(formula_Sep_sm_detrendlog_SMIPrecTavg,  data = Yield_Covariates_SM_Sep_no2003,  effect="individual", model=("within"), index = c("comId","year"))
summary(plm.fit_SM_BestStdFull_Sep_no2003)

cov2_SM_BestStdFull_Sep_no2003     <- vcovSCC(plm.fit_SM_BestStdFull_Sep_no2003,method = "arellano",type = "HC0")
DK.se_SM_BestStdFull_Sep_no2003    <- sqrt(diag(cov2_SM_BestStdFull_Sep_no2003 ))

## LM ##
lm.fit_SM_BestStdFull_Sep_no2003 <- lm(update(formula_Sep_sm_detrendlog_SMIPrecTavg , .~. + dummy(comId)),  data = Yield_Covariates_SM_Sep_no2003)
summary(lm.fit_SM_BestStdFull_Sep_no2003)

BestStdFull_lm_summary_no2003 <- summary(lm.fit_SM_BestStdFull_Sep_no2003) 
BestStdFull_lm_summary_no2003$r.squared
capture.output(BestStdFull_lm_summary_no2003, file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/Output_SM_Sep_qual", append=T)

Strich <- print("--------------------------")
BestStdFull_lm_summary_qual1 <- print(c("BestStdFull_lm_r.squared:", round(BestStdFull_lm_summary_no2003$r.squared,4)))
BestStdFull_lm_summary_qual2 <- print(c("BestStdFull_lm_adj.r.squared:", round(BestStdFull_lm_summary_no2003$adj.r.squared,4)))
capture.output(Strich, file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/Output_SM_Sep_qual.txt", append=T)
capture.output(BestStdFull_lm_summary_qual1, file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/Output_SM_Sep_qual.txt", append=T)
capture.output(BestStdFull_lm_summary_qual2, file = "./figures/figures_exploratory/BIC/Silomaize/Sep_no2003/Output_SM_Sep_qual.txt", append=T)


