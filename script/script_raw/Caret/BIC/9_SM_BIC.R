#################################
####  SiloMaize in September ####
#################################

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
- Model with lowest BIC in general: Tavg, SMI
- Model with lowest BIC of standard configuration: Tavg, Prec, SMI
- Model with lowest BIC with SMI: Tavg, SMI

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
"./figures/figures_exploratory/FixedEffects/Silomaize/..."
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
Yield_Covariates_Sep <- Yield_Covariates_Sep[,c(1:4)]

## Establish first part of data frame_ time and spatial reference plus Silomaize ##
names(Yield_Covariates[,c(2,1,3:5,7)])
Yield_Covariates_SM <- Yield_Covariates[,c(2,1,3:5,7)] # Achtung, darauf achten, dass comId und year in der richtigen Reihenfolge sind.
names(Yield_Covariates_SM)
head(Yield_Covariates_SM)

Yield_Covariates_SM_Sep <- cbind(Yield_Covariates_SM, Yield_Covariates_Sep)
names(Yield_Covariates_SM_Sep)
names(Yield_Covariates_SM_Sep) <- c( "comId" , "year","com","stateId","state","siloMaize","SMI", "Prec","Tavg", "Pet")
names(Yield_Covariates_SM_Sep)

#########################################
#### Create stepwise function of SMI ####
#########################################
' Drought Monitor Spezification '
Yield_Covariates_SM_Sep$SMI_GDM <- cut(Yield_Covariates_SM_Sep$SMI, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1),                                            ,
                                       labels = c("severe drought","moderate drought","abnormal dry", "normal","abnormal wet" ,"abundant wet", "severe wet"))
#############
## Na-omit ##
sum(is.na(Yield_Covariates_SM_Sep) )
dim(Yield_Covariates_SM_Sep)
Yield_Covariates_SM_Sep_nna <- na.omit(Yield_Covariates_SM_Sep) 
dim(Yield_Covariates_SM_Sep_nna)

## Check for NAs
any(is.na(Yield_Covariates_SM_Sep_nna))
## Reset Rownames
rownames(Yield_Covariates_SM_Sep_nna) <- NULL

## Further work with DataFrame without Yield_Covariates_SM_Sep index ##
Yield_Covariates_SM_Sep <- Yield_Covariates_SM_Sep_nna

#########################################################################
## Remove comIds with less than 7 observations to avoid leveage issues ## 
#########################################################################

#####################################################
## Delete all comIds with less than 7 observations ##
sum(table(Yield_Covariates_SM_Sep$comId) < 7 )
table(Yield_Covariates_SM_Sep$comId) < 7 

## comIds mit weniger als 7 Beoachtungen: ##
list <- c(3101, 3102, 3158, 3402, 5114, 5117, 5314,5315, 5334,5378, 5512, 5911, 5916, 6413, 7131, 7135, 7233, 7331, 7332, 7337, 7338, 7339, 8111,12052, 14612, 15001, 15082, 15083, 15084, 15085, 15086, 15087, 15088, 15089, 15091, 16051, 16052 )
length(list)
list[[1]]

temp <- Yield_Covariates_SM_Sep
for (i in 1:34)
{
  print(Yield_Covariates_SM_Sep[Yield_Covariates_SM_Sep$comId==list[i],])
  temp <- (temp[!temp$comId==list[i],])
}

## Number of deleted rows
dim(temp)-dim(Yield_Covariates_SM_Sep)

## Further use old name for data.frame
Yield_Covariates_SM_Sep <- temp

################################
## Befehle nach jedem löschen ##
Yield_Covariates_SM_Sep <- na.omit(Yield_Covariates_SM_Sep) 
rownames(Yield_Covariates_SM_Sep) <- NULL
Yield_Covariates_SM_Sep <- plm.data(Yield_Covariates_SM_Sep, index=c("comId", "year"))
Yield_Covariates_SM_Sep[,c("comId","stateId")] <- lapply(Yield_Covariates_SM_Sep[,c("comId","stateId")], factor )

#################################################
#### Remove log trend of indepedent variable ####
#################################################
'Fit log of yield on log of time and use the residuals of that for yields'
logtrend <- lm(log(siloMaize) ~ log(as.integer(year)), data= Yield_Covariates_SM_Sep)

##########################
## Issue with Outliers ###
##########################
par(mfrow = c(2,2))
plot(logtrend) 

## Look Outliers Values ##
Yield_Covariates_SM_Sep[c(1276, 3262, 3283, 3171,3255),]

## Look at other values of outliers com #
Yield_Covariates_SM_Sep[Yield_Covariates_SM_Sep$comId == "6532",] # 2008 hier scheint nur ein Jahr ein Messfehler zu sein: diesen Lösche ich 
Yield_Covariates_SM_Sep[Yield_Covariates_SM_Sep$comId == "12067",] # 2006 verändere ich nicht 
Yield_Covariates_SM_Sep[Yield_Covariates_SM_Sep$comId == "12069",] # 2003 verändere ich nicht 
Yield_Covariates_SM_Sep[Yield_Covariates_SM_Sep$comId == "12060",] # 1999 verändere ich nicht 
Yield_Covariates_SM_Sep[Yield_Covariates_SM_Sep$comId == "12067",] # 2006 verändere ich nicht 


## Delete outliers ##
' Da sich die Patterns bei den BIC Vergleichen nicht ändert, kümmere ich micht nicht weiter um die Outlier. 
Ich nehme nur sehr offensichtliche Messfehler raus.'
Yield_Covariates_SM_Sep <- Yield_Covariates_SM_Sep[!(Yield_Covariates_SM_Sep$comId == "6532" & Yield_Covariates_SM_Sep$year == "2008"),]

Yield_Covariates_SM_Sep <- na.omit(Yield_Covariates_SM_Sep)
rownames(Yield_Covariates_SM_Sep) <- NULL


#################################################
#### Remove log trend of indepedent variable ####
logtrend <- lm(log(siloMaize) ~ log(as.integer(year)), data= Yield_Covariates_SM_Sep)
summary(logtrend)
Yield_Covariates_SM_Sep$siloMaize_logtrend <- resid(logtrend)

#######################################
## Prepare dataframe for plm package ##
'Change Indexing so that it can be used in plm package'
Yield_Covariates_SM_Sep <- plm.data(Yield_Covariates_SM_Sep, index=c("comId", "year"))
str(Yield_Covariates_SM_Sep)


## Transform comId and stateId to factor ##
Yield_Covariates_SM_Sep[,c("comId","stateId")] <- lapply(Yield_Covariates_SM_Sep[,c("comId","stateId")], factor )
lapply(Yield_Covariates_SM_Sep, class)


###############################################
##### Save Yield_Covariates_SM_September extern ####
write.csv(Yield_Covariates_SM_Sep, file="./data/data_raw/Yield_Covariates_SM_Sep.csv")

#######################################################
#### BIC to choose the degrees of the polynomials  ####
#######################################################
## create a matrix which contains all possible degree combinations, here for three variables ##
degree <- permutations(n=3,r=2,v=c(1:3),repeats.allowed=T)
degree

################################################
## Formulas for Model Variations to be tested ##

## with SMI
formula_Sep_sm_detrendlog_SMIPrecTavg <- siloMaize_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  poly(Tavg, degree[r, 2], raw = T) + 
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)

formula_Sep_sm_detrendlog_SMIPrecPet  <- siloMaize_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  poly(Pet, degree[r, 2], raw = T) + 
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)

formula_Sep_sm_detrendlog_SMIPrec     <- siloMaize_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)

formula_Sep_sm_detrendlog_SMIPet      <- siloMaize_logtrend ~ poly(Pet, degree[r, 2], raw = T) + 
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)

formula_Sep_sm_detrendlog_SMITavg     <- siloMaize_logtrend ~ poly(Tavg, degree[r, 2], raw = T) + 
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)

formula_Sep_sm_detrendlog_SMI         <- siloMaize_logtrend ~  
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)

## no SMI
formula_Sep_sm_detrendlog_PrecTavg <- siloMaize_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  poly(Tavg, degree[r, 2], raw = T) + dummy(comId)

formula_Sep_sm_detrendlog_PrecPet  <- siloMaize_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  poly(Pet, degree[r, 2], raw = T) + dummy(comId)

formula_Sep_sm_detrendlog_Prec     <- siloMaize_logtrend ~ poly(Prec, degree[r, 1], raw = T) + dummy(comId)

formula_Sep_sm_detrendlog_Pet      <- siloMaize_logtrend ~ poly(Pet, degree[r, 2], raw = T)  +  dummy(comId)

formula_Sep_sm_detrendlog_Tavg     <- siloMaize_logtrend ~ poly(Tavg, degree[r, 2], raw = T) + dummy(comId)

## Print formula
# formula_Sep_sm_detrendlog_SMIPrecTavg
# formula_Sep_sm_detrendlog_SMIPrecPet
# formula_Sep_sm_detrendlog_SMIPrec
# formula_Sep_sm_detrendlog_SMIPet
# formula_Sep_sm_detrendlog_SMITavg
# formula_Sep_sm_detrendlog_SMI
# formula_Sep_sm_detrendlog_PrecTavg
# formula_Sep_sm_detrendlog_PrecPet
# formula_Sep_sm_detrendlog_Prec
# formula_Sep_sm_detrendlog_Pet 
# formula_Sep_sm_detrendlog_Tavg

#################################################################################################
# Loop through the container list to cover all permutations of posssible degree of freedoms of ##
# of the polynomials of the variables                                                          ##
#################################################################################################

##################################################
## Loop through various variable configurations ##
BIC_SMIPrecTavg <- rep(0,9)
for(r in 1:9){
  glm.fit_SMIPrecTavg  <- glm(formula = formula_Sep_sm_detrendlog_SMIPrecTavg, data = Yield_Covariates_SM_Sep) 
  BIC_SMIPrecTavg[r] <- BIC(glm.fit_SMIPrecTavg)
}

BIC_SMIPrecPet <- rep(0,9)
for(r in 1:9){
  glm.fit_SMIPrecPet  <- glm(formula = formula_Sep_sm_detrendlog_SMIPrecPet,  data = Yield_Covariates_SM_Sep)
  BIC_SMIPrecPet[r] <- BIC(glm.fit_SMIPrecPet)
}

BIC_SMIPrec <- rep(0,9)
for(r in 1:9){  
  glm.fit_SMIPrec      <- glm(formula = formula_Sep_sm_detrendlog_SMIPrec,     data = Yield_Covariates_SM_Sep) 
  BIC_SMIPrec[r] <- BIC(glm.fit_SMIPrec)
}

BIC_SMIPet <- rep(0,9)
for(r in 1:9){  
  glm.fit_SMIPet       <- glm(formula = formula_Sep_sm_detrendlog_SMIPet,      data = Yield_Covariates_SM_Sep) 
  BIC_SMIPet[r] <- BIC(glm.fit_SMIPet)
}

BIC_SMITavg <- rep(0,9)
for(r in 1:9){  
  glm.fit_SMITavg      <- glm(formula = formula_Sep_sm_detrendlog_SMITavg,     data = Yield_Covariates_SM_Sep) 
  BIC_SMITavg[r] <- BIC(glm.fit_SMITavg)
}

BIC_SMI <- rep(0,9)
for(r in 1:9){  
  glm.fit_SMI      <- glm(formula = formula_Sep_sm_detrendlog_SMI,     data = Yield_Covariates_SM_Sep) 
  BIC_SMI[r] <- BIC(glm.fit_SMI)
}

BIC_PrecTavg <- rep(0,9)
for(r in 1:9){
  glm.fit_PrecTavg  <- glm(formula = formula_Sep_sm_detrendlog_PrecTavg, data = Yield_Covariates_SM_Sep) 
  BIC_PrecTavg[r] <- BIC(glm.fit_PrecTavg)
}

BIC_PrecPet <- rep(0,9)
for(r in 1:9){
  glm.fit_PrecPet  <- glm(formula = formula_Sep_sm_detrendlog_PrecPet, data = Yield_Covariates_SM_Sep) 
  BIC_PrecPet[r] <- BIC(glm.fit_PrecPet)
}

BIC_Prec <- rep(0,9)
for(r in 1:9){
  glm.fit_Prec  <- glm(formula = formula_Sep_sm_detrendlog_Prec, data = Yield_Covariates_SM_Sep) 
  BIC_Prec[r] <- BIC(glm.fit_Prec)
}

BIC_Pet  <- rep(0,9)
for(r in 1:9){
  glm.fit_Pet   <- glm(formula = formula_Sep_sm_detrendlog_Pet , data = Yield_Covariates_SM_Sep) 
  BIC_Pet [r] <- BIC(glm.fit_Pet )
}

BIC_Tavg  <- rep(0,9)
for(r in 1:9){
  glm.fit_Tavg   <- glm(formula = formula_Sep_sm_detrendlog_Tavg , data = Yield_Covariates_SM_Sep) 
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
month <-rep("September",99)

BIC_Sep <- cbind(BIC, model ,model_index, index, month)

#######################
## Delete Duplicates ##
which(duplicated(BIC_Sep$BIC))
list3 <- c(20,21,23,24,26,27,31,32,33,34,35,36,40,41,42,43,44,45,47,48,49,50,51,52,53,54,56,57,59,60,62,63,67,68,69,70,71,72,76,77,78,79,80,81)
           
length(list3)
temp <- BIC_Sep

for (i in 1:44)
{
  print(BIC_Sep[BIC_Sep$index ==list3[i],])
  temp <- (temp[!temp$index==list3[i],])
}
dim(BIC_Sep)
dim(temp)

################################
## Correct created data.frame ##
rownames(temp) <- NULL
BIC_Sep <- temp
lapply(BIC_Sep, class)

############################
## Plot data with ggplot2 ##
g <- ggplot(BIC_Sep,aes(y=BIC, x=index))
g + geom_point(aes(color=model)) + labs(title="BIC of various model configurations", x="") + theme(plot.title=element_text(size=15, face="bold")) + theme_dark()
g + geom_point(aes(color=model)) + labs(title="BIC of various model configurations", x="") + theme(plot.title=element_text(size=15, face="bold")) + theme_dark() +
  facet_wrap( ~ month)
BIC_Sep

## Export Data frame for use in BIC_Grafic
BIC_SM_Sep <- BIC_Sep
class(BIC_SM_Sep)
write.csv(BIC_SM_Sep, file="./data/data_raw/BIC/BIC_SM_Sep.csv")

################################################################
################################### Explore Models #############
################################################################
###################
## Load Data Set ##
# Yield_Covariates_SM_Sep <- read.csv( file="./data/data_raw/Yield_Covariates_SM_Sep.csv")
# names(Yield_Covariates_SM_Sep)
# Yield_Covariates_SM_Sep$X <- NULL

#######################################
## Prepare dataframe for plm package ##
'Change Indexing so that it can be used in plm package'
Yield_Covariates_SM_Sep <- plm.data(Yield_Covariates_SM_Sep, index=c("comId", "year"))

## Transform comId and stateId to factor ##
Yield_Covariates_SM_Sep[,c("comId","stateId")] <- lapply(Yield_Covariates_SM_Sep[,c("comId","stateId")], factor )
str(Yield_Covariates_SM_Sep)

#################################
###############################
## Results with smallest BIC ##
###############################
plot(BIC_SMITavg)
which.min(BIC_SMITavg)  
r = 3 
best_formula <- formula_Sep_sm_detrendlog_SMITavg
degree

###################
## GLM Ergebniss ##
glm.fit_SM_BEST_Sep  <- glm(formula = best_formula,  data = Yield_Covariates_SM_Sep)
summary(glm.fit_SM_BEST_Sep)
'AIC: -6600.2'

####################
## PLM Ergebnisse ##
plm.fit_SM_BEST_Sep <- plm(formula = update(best_formula, .~. - dummy(comId)),  data = Yield_Covariates_SM_Sep,  effect="individual", model=("within"), index = c("comId","year"))
summary(plm.fit_SM_BEST_Sep)
'Adj. R-Squared: 0.071596'

fixef <-  fixef(plm.fit_SM_BEST_Sep)
fixef <- as.data.frame(as.matrix(fixef))
head(fixef)
fixef <- cbind(rownames(fixef), fixef)
rownames(fixef) <- NULL
names(fixef) <- c("comId", "FE")
fixef
write.csv(fixef, "./figures/figures_exploratory/FixedEffects/Silomaize/plm.fit_SM_BEST_Sep_FE.csv")

##################
## LM Ergebniss ##
lm.fit_SM_BEST_Sep  <-lm(formula = best_formula,  data = Yield_Covariates_SM_Sep)
summary(lm.fit_SM_BEST_Sep) 
'Adjusted R-squared:  0.6449'

################################################
## Assessing Influence (Leverage*discrepancy) ##
cutoff_SM_Sep <- 4/((nrow(Yield_Covariates_SM_Sep)-length(lm.fit_SM_BEST_Sep$coefficients)-1)) 
cutoff_SM_Sep
plot(lm.fit_SM_BEST_Sep, which=4)
cook_Sep <- cooks.distance(lm.fit_SM_BEST_Sep)

nrow(Yield_Covariates_SM_Sep[cook_Sep > cutoff_SM_Sep,]) # 189

year_cooks_SM_Sep <- table(Yield_Covariates_SM_Sep$year[cook_Sep > cutoff_SM_Sep ]) 
year_cooks_SM_Sep
'1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 
   7    4    8    7   52    4    6   25   24    8   17   23
'

com_cooks_SM_Sep <- sort(table(Yield_Covariates_SM_Sep$com[cook_Sep > cutoff_SM_Sep ] ) )
tail(com_cooks_SM_Sep,20)
'
          Schmalkalden-Meiningen, Kreis                       S\xf6mmerda, Kreis                       Stendal, Landkreis                  Hagen, Kreisfreie Stadt 
                                       2                                        2                                        2                                        3 
            Leverkusen, Kreisfreie Stadt                      M\xe4rkischer Kreis          Neum\xfcnster, Kreisfreie Stadt                     Oberbergischer Kreis 
                                       3                                        3                                        3                                        3 
                   Oder-Spree, Landkreis             Teltow-Fl\xe4ming, Landkreis                     Uckermark, Landkreis                    Wittenberg, Landkreis 
                                       3                                        3                                        3                                        3 
                       Barnim, Landkreis               Dahme-Spreewald, Landkreis                   Elbe-Elster, Landkreis                Spree-Nei\xdfe, Landkreis 
                                       4                                        4                                        4                                        4 
      Frankfurt (Oder), Kreisfreie Stadt         Oberspreewald-Lausitz, Landkreis                              Olpe, Kreis               Siegen-Wittgenstein, Kreis 
                                       5                                        5                                        6                                        7 '


########################
## Heteroskedasdicity ##
bptest(glm.fit_SM_BEST_Sep) # Breusch Pagan Test of Heteroskedastie in den Störgrößen: Null: Homoskedasdicity.
bptest(plm.fit_SM_BEST_Sep)
' In beiden Fällen kann die Null widerlegt werden. Es gibt also heteroskedasdicity '

## Koenkers Version on BP Test: robuste Modification wenn die Störgrößen nicht normalverteilt sind. 
bptest(plm.fit_SM_BEST_Sep, studentize = TRUE)
'Auch hier kann die Null widerlegt werden. Need to use robust covariance variance matrix to correct standard errors'

######################################
## Tests for serial autocorrelation ##
pwartest(plm.fit_SM_BEST_Sep)
pbgtest(plm.fit_SM_BEST_Sep) 
'
both, H_1 of serial autocorrelation cannot be rejected
'

#################################
## Correct the Standard Errors ##
#################################

## Correct Standard Errors used in table ##
coeftest(plm.fit_SM_BEST_Sep)

## Robust covariance matrix estimators a la White ##
# coeftest(plm.fit_SM_BEST_Sep,vcov=vcovHC(plm.fit_SM_BEST_Sep,method = "arellano", type = "HC0")) 
cov0_SM_BEST_Sep <- vcovHC(plm.fit_SM_BEST_Sep,method = "arellano", type = "HC0", cluster="group")
Wh.se_serial_SM_BEST_Sep <- sqrt(diag(cov0_SM_BEST_Sep))

cov0.1_SM_BEST_Sep <- vcovHC(plm.fit_SM_BEST_Sep,method = "arellano", type = "HC0", cluster="time")
Wh.se_cross_SM_BEST_Sep <- sqrt(diag(cov0.1_SM_BEST_Sep))
# 
# ## Beck Katz:
# # coeftest(plm.fit_SM_BEST_Sep, vcov = function(x) vcovBK(plm.fit_SM_BEST_Sep,method = "arellano", type = "HC0"))
# cov1 <- vcovBK(plm.fit_SM_BEST_Sep,method = "arellano", type = "HC0",  cluster="time")
# BK.se <- sqrt(diag(cov1))

# ## Driscoll Kraay ##
# summary(plm.fit_SM_BEST_Sep)
coeftest(plm.fit_SM_BEST_Sep,  vcov=function(x) vcovSCC(plm.fit_SM_BEST_Sep,method = "arellano",type = "HC0")) 
cov2_SM_BEST_Sep     <- vcovSCC(plm.fit_SM_BEST_Sep,method = "arellano",type = "HC0")
DK.se_SM_BEST_Sep    <- sqrt(diag(cov2_SM_BEST_Sep))
# 
# cov2.1_SM_BEST_Sep   <- vcovSCC(plm.fit_SM_BEST_Sep,method = "arellano",type = "HC0", maxlag=1)
# DK2.1.se_SM_BEST_Sep <- sqrt(diag(cov2.1_SM_BEST_Sep))

# cov2.2_SM_BEST_Sep   <- vcovSCC(plm.fit_SM_BEST_Sep,method = "arellano",type = "HC0", maxlag=2)
# DK2.2.se_SM_BEST_Sep <- sqrt(diag(cov2.2_SM_BEST_Sep))
# 
# cov2.3_SM_BEST_Sep   <- vcovSCC(plm.fit_SM_BEST_Sep,method = "arellano",type = "HC0", maxlag=3)
# DK2.3.se_SM_BEST_Sep <- sqrt(diag(cov2.3_SM_BEST_Sep))
# 
# cov2.4_SM_BEST_Sep   <- vcovSCC(plm.fit_SM_BEST_Sep,method = "arellano",type = "HC0", maxlag=4)
# DK2.4.se_SM_BEST_Sep <- sqrt(diag(cov2.4_SM_BEST_Sep))
# 
cov2.5_SM_BEST_Sep   <- vcovSCC(plm.fit_SM_BEST_Sep,method = "arellano",type = "HC0", maxlag=5)
DK2.5.se_SM_BEST_Sep <- sqrt(diag(cov2.5_SM_BEST_Sep))

## Cameron et al /Thompson : doouble-clustering estimator  ##
# coeftest(plm.fit_SM_BEST_Sep, vcovDC(plm.fit_SM_BEST_Sep, method = "arellano", type = "HC0"))
cov3_SM_BEST_Sep <- vcovDC(plm.fit_SM_BEST_Sep, method = "arellano", type = "HC0")
CT.se_SM_BEST_Sep <- sqrt(diag(cov3_SM_BEST_Sep))
'Our estimator is qualitatively similar to the ones presented in White and Domowitz (1984), for
time series data, and Conley (1999), for spatial data. '

## Generate Table with Output ##
se <- list(NULL, Wh.se_cross_SM_BEST_Sep, Wh.se_serial_SM_BEST_Sep,  DK.se_SM_BEST_Sep, DK2.5.se_SM_BEST_Sep, CT.se_SM_BEST_Sep)
labels1 <-c("NULL","WhiteCross","WhiteSerial", "DriscollKraay", "DriscollKray2.5","CameronThompson")
stargazer(plm.fit_SM_BEST_Sep, plm.fit_SM_BEST_Sep, plm.fit_SM_BEST_Sep, plm.fit_SM_BEST_Sep, plm.fit_SM_BEST_Sep,plm.fit_SM_BEST_Sep,
          se = se,   
          dep.var.caption  = "Model with smallest BIC - September",
          dep.var.labels = "log(Silomaize)",
          style="default",
          model.numbers = FALSE,
          column.labels = labels1,          
          type="text", out="./figures/figures_exploratory/BIC/Silomaize/SM_Sep_best.txt"
)


#########################################################
## Results with smallest BIC of Standard Configuration ##
#########################################################
plot(BIC_SMIPrecTavg)
which.min(BIC_SMIPrecTavg) 
r = 6
bestStandard_formula <- formula_Sep_sm_detrendlog_SMIPrecTavg
'Hier ist zwar 9 am besten, aufgrund von Singularität nehme ich aber 6. Der Abstand zwischen 9 und 6 ist auch kleiner als sechs Einheiten,
daher ist dies nach Wikipedia auch berechtigt.'

###################
## GLM Ergebniss ##
glm.fit_SM_bestStandard_Sep  <- glm(formula = bestStandard_formula,  data = Yield_Covariates_SM_Sep)
summary(glm.fit_SM_bestStandard_Sep)
'AIC: -6609.1'

####################
## PLM Ergebnisse ##
plm.fit_SM_bestStandard_Sep <- plm(formula = update(bestStandard_formula, .~. - dummy(comId)),  data = Yield_Covariates_SM_Sep,  effect="individual", model=("within"), index = c("comId","year"))
summary(plm.fit_SM_bestStandard_Sep)
'Adj. R-Squared: 0.082556'

## Generate Fixed Effects data.frame and export it ##
fixef <- fixef(plm.fit_SM_bestStandard_Sep)
fixef <- as.data.frame(as.matrix(fixef))
head(fixef)
fixef <- cbind(rownames(fixef), fixef)
rownames(fixef) <- NULL
names(fixef) <- c("comId", "FE")
write.csv(fixef, "./figures/figures_exploratory/FixedEffects/Silomaize/plm.fit_SM_bestStandard_Sep_FE.csv")

##################
## LM Ergebniss ##
lm.fit_SM_bestStandard_Sep  <-lm(formula = bestStandard_formula,  data = Yield_Covariates_SM_Sep)
summary(lm.fit_SM_bestStandard_Sep) 
'Adjusted R-squared:  0.6459 '

########################
## Heteroskedasdicity ##
bptest(glm.fit_SM_bestStandard_Sep) # Breusch Pagan Test of Heteroskedastie in den Störgrößen: Null: Homoskedasdicity.
bptest(plm.fit_SM_bestStandard_Sep)
' In beiden Fällen kann die Null widerlegt werden. Es gibt also heteroskedasdicity '

## Koenkers Version on BP Test: robuste Modification wenn die Störgrößen nicht normalverteilt sind. 
bptest(plm.fit_SM_bestStandard_Sep, studentize = TRUE)
'Auch hier kann die Null widerlegt werden. Need to use robust covariance variance matrix to correct standard errors'

#########################
#### Autocorrelation ####

######################################
## Tests for serial autocorrelation ##
pwartest(plm.fit_SM_bestStandard_Sep)
' Hier serielle Korrelation festzustellen'
pbgtest(plm.fit_SM_bestStandard_Sep) 

'Solution for serial correlation: Cluster by groups.
Solution for cross sectional correlation: Cluster by time'

#################################
## Correct the Standard Errors ##

## Correct Standard Errors used in table ##
coeftest(plm.fit_SM_bestStandard_Sep)

## Robust covariance matrix estimators a la White 
# coeftest(plm.fit_SM_bestStandard_Sep,vcov=vcovHC(plm.fit_SM_bestStandard_Sep,method = "arellano", type = "HC0")) 
cov0_SM_bestStandard_Sep <- vcovHC(plm.fit_SM_bestStandard_Sep,method = "arellano", type = "HC0", cluster="group")
Wh.se_serial_SM_bestStandard_Sep <- sqrt(diag(cov0_SM_bestStandard_Sep))

cov0.1_SM_bestStandard_Sep <- vcovHC(plm.fit_SM_bestStandard_Sep,method = "arellano", type = "HC0", cluster="time")
Wh.se_cross_SM_bestStandard_Sep <- sqrt(diag(cov0.1_SM_bestStandard_Sep))

# ## Beck Katz ##
# # coeftest(plm.fit_SM_bestStandard_Sep, vcov = function(x) vcovBK(plm.fit_SM_bestStandard_Sep,method = "arellano", type = "HC0"))
# cov1_SM_bestStandard_Sep <- vcovBK(plm.fit_SM_bestStandard_Sep,method = "arellano", type = "HC0",  cluster="time")
# BK.se_SM_bestStandard_Sep <- sqrt(diag(cov1_SM_bestStandard_Sep))

## Driscoll Kraay: ##
summary(plm.fit_SM_bestStandard_Sep)
cov2_SM_bestStandard_Sep     <- vcovSCC(plm.fit_SM_bestStandard_Sep,method = "arellano",type = "HC0")
DK.se_SM_bestStandard_Sep    <- sqrt(diag(cov2_SM_bestStandard_Sep))

# cov2.1_SM_bestStandard_Sep   <- vcovSCC(plm.fit_SM_bestStandard_Sep,method = "arellano",type = "HC0", maxlag=1)
# DK2.1.se_SM_bestStandard_Sep <- sqrt(diag(cov2.1_SM_bestStandard_Sep))

# cov2.2_SM_bestStandard_Sep   <- vcovSCC(plm.fit_SM_bestStandard_Sep,method = "arellano",type = "HC0", maxlag=2)
# DK2.2.se_SM_bestStandard_Sep <- sqrt(diag(cov2.2_SM_bestStandard_Sep))
# 
# cov2.3_SM_bestStandard_Sep   <- vcovSCC(plm.fit_SM_bestStandard_Sep,method = "arellano",type = "HC0", maxlag=3)
# DK2.3.se_SM_bestStandard_Sep <- sqrt(diag(cov2.3_SM_bestStandard_Sep))
# 
# cov2.4_SM_bestStandard_Sep   <- vcovSCC(plm.fit_SM_bestStandard_Sep,method = "arellano",type = "HC0", maxlag=4)
# DK2.4.se_SM_bestStandard_Sep <- sqrt(diag(cov2.4_SM_bestStandard_Sep))
# 
cov2.5_SM_bestStandard_Sep   <- vcovSCC(plm.fit_SM_bestStandard_Sep,method = "arellano",type = "HC0", maxlag=5)
DK2.5.se_SM_bestStandard_Sep <- sqrt(diag(cov2.5_SM_bestStandard_Sep))

## Cameron et al /Thompson : doouble-clustering estimator 
# coeftest(plm.fit_SM_bestStandard_Sep, vcovDC(plm.fit_SM_bestStandard_Sep, method = "arellano", type = "HC0"))
cov3_SM_bestStandard_Sep <- vcovDC(plm.fit_SM_bestStandard_Sep, method = "arellano", type = "HC0")
CT.se_SM_bestStandard_Sep <- sqrt(diag(cov3_SM_bestStandard_Sep))
'Our estimator is qualitatively similar to the ones presented in White and Domowitz (1984), for
time series data, and Conley (1999), for spatial data. '

## Generate Table with Output ##
se <- list(NULL, Wh.se_cross_SM_bestStandard_Sep, Wh.se_serial_SM_bestStandard_Sep, DK.se_SM_bestStandard_Sep, DK2.5.se_SM_bestStandard_Sep, CT.se_SM_bestStandard_Sep)
labels1 <-c("NULL","WhiteCross","WhiteSerial", "DriscollKraay", "DriscollKray2.5","CameronThompson")
stargazer(plm.fit_SM_bestStandard_Sep, plm.fit_SM_bestStandard_Sep, plm.fit_SM_bestStandard_Sep, plm.fit_SM_bestStandard_Sep, plm.fit_SM_bestStandard_Sep,plm.fit_SM_bestStandard_Sep,
          se = se,   
          dep.var.caption  = "Model with smallest BIC of Standard Configuration - September",
          dep.var.labels = "log(Silomaize)",
          style="default",
          model.numbers = FALSE,
          column.labels = labels1,          
          type="text", out="./figures/figures_exploratory/BIC/Silomaize/SM_Sep_bestStandard.txt"
)

########################################
## Results with smallest BIC with SMI ##
########################################
plot(BIC_SMITavg)
which.min(BIC_SMITavg) 
r = 3
bestSMI_formula <- formula_Sep_sm_detrendlog_SMITavg

###################
## GLM Ergebniss ##
glm.fit_SM_bestSMI_Sep  <- glm(formula = bestSMI_formula,  data = Yield_Covariates_SM_Sep)
summary(glm.fit_SM_bestSMI_Sep)
'AIC: -6600.2'

####################
## PLM Ergebnisse ##
plm.fit_SM_bestSMI_Sep <- plm(formula = update(bestSMI_formula, .~. - dummy(comId)),  data = Yield_Covariates_SM_Sep,  effect="individual", model=("within"), index = c("comId","year"))
summary(plm.fit_SM_bestSMI_Sep)
'Adj. R-Squared: 0.079843'

fixef <- fixef(plm.fit_SM_bestSMI_Sep)
fixef <- as.data.frame(as.matrix(fixef))
head(fixef)
fixef <- cbind(rownames(fixef), fixef)
rownames(fixef) <- NULL
names(fixef) <- c("comId", "FE")
write.csv(fixef, "./figures/figures_exploratory/FixedEffects/Silomaize/plm.fit_SM_bestSMI_Sep_FE.csv")

##################
## LM Ergebniss ##
lm.fit_SM_bestSMI_Sep  <-lm(formula = bestSMI_formula,  data = Yield_Covariates_SM_Sep)
summary(lm.fit_SM_bestSMI_Sep) 
'Adjusted R-squared:  0.6449'

########################
## Heteroskedasdicity ##
bptest(glm.fit_SM_bestSMI_Sep) # Breusch Pagan Test of Heteroskedastie in den Störgrößen: Null: Homoskedasdicity.
bptest(plm.fit_SM_bestSMI_Sep)
' In beiden Fällen kann die Null widerlegt werden. Es gibt also heteroskedasdicity '

## Koenkers Version on BP Test: robuste Modification wenn die Störgrößen nicht normalverteilt sind. 
bptest(plm.fit_SM_bestSMI_Sep, studentize = TRUE)
'Auch hier kann die Null widerlegt werden. Need to use robust covariance variance matrix to correct standard errors'

#########################
#### Autocorrelation ####

######################################
## Tests for serial autocorrelation ##
pwartest(plm.fit_SM_bestSMI_Sep)
pbgtest(plm.fit_SM_bestSMI_Sep) 
'Hier serielle Korrelation festzustellen'

###########################################
## Correct Standard Errors used in table ##
coeftest(plm.fit_SM_bestSMI_Sep)

## Robust covariance matrix estimators a la White ##
# coeftest(plm.fit_SM_bestSMI_Sep,vcov=vcovHC(plm.fit_SM_bestSMI_Sep,method = "arellano", type = "HC0")) 
cov0_SM_bestSMI_Sep <- vcovHC(plm.fit_SM_bestSMI_Sep,method = "arellano", type = "HC0", cluster="group")
Wh.se_serial_SM_bestSMI_Sep <- sqrt(diag(cov0_SM_bestSMI_Sep))

cov0.1_SM_bestSMI_Sep <- vcovHC(plm.fit_SM_bestSMI_Sep,method = "arellano", type = "HC0", cluster="time")
Wh.se_cross_SM_bestSMI_Sep <- sqrt(diag(cov0.1_SM_bestSMI_Sep))
# 
# ## Beck Katz:
# # coeftest(plm.fit_SM_bestSMI_Sep, vcov = function(x) vcovBK(plm.fit_SM_bestSMI_Sep,method = "arellano", type = "HC0"))
# cov1 <- vcovBK(plm.fit_SM_bestSMI_Sep,method = "arellano", type = "HC0",  cluster="time")
# BK.se <- sqrt(diag(cov1))

## Driscoll Kraay ##
# summary(plm.fit_SM_bestSMI_Sep)
cov2_SM_bestSMI_Sep     <- vcovSCC(plm.fit_SM_bestSMI_Sep,method = "arellano",type = "HC0")
DK.se_SM_bestSMI_Sep    <- sqrt(diag(cov2_SM_bestSMI_Sep))

# cov2.1_SM_bestSMI_Sep   <- vcovSCC(plm.fit_SM_bestSMI_Sep,method = "arellano",type = "HC0", maxlag=1)
# DK2.1.se_SM_bestSMI_Sep <- sqrt(diag(cov2.1_SM_bestSMI_Sep))

# cov2.2_SM_bestSMI_Sep   <- vcovSCC(plm.fit_SM_bestSMI_Sep,method = "arellano",type = "HC0", maxlag=2)
# DK2.2.se_SM_bestSMI_Sep <- sqrt(diag(cov2.2_SM_bestSMI_Sep))
# 
# cov2.3_SM_bestSMI_Sep   <- vcovSCC(plm.fit_SM_bestSMI_Sep,method = "arellano",type = "HC0", maxlag=3)
# DK2.3.se_SM_bestSMI_Sep <- sqrt(diag(cov2.3_SM_bestSMI_Sep))
# 
# cov2.4_SM_bestSMI_Sep   <- vcovSCC(plm.fit_SM_bestSMI_Sep,method = "arellano",type = "HC0", maxlag=4)
# DK2.4.se_SM_bestSMI_Sep <- sqrt(diag(cov2.4_SM_bestSMI_Sep))
# 
cov2.5_SM_bestSMI_Sep   <- vcovSCC(plm.fit_SM_bestSMI_Sep,method = "arellano",type = "HC0", maxlag=5)
DK2.5.se_SM_bestSMI_Sep <- sqrt(diag(cov2.5_SM_bestSMI_Sep))


## Cameron et al /Thompson : doouble-clustering estimator ##
cov3_SM_bestSMI_Sep <- vcovDC(plm.fit_SM_bestSMI_Sep, method = "arellano", type = "HC0")
CT.se_SM_bestSMI_Sep <- sqrt(diag(cov3_SM_bestSMI_Sep))

################################
## Generate Table with Output ##
se <- list(NULL, Wh.se_cross_SM_bestSMI_Sep, Wh.se_serial_SM_bestSMI_Sep,  DK.se_SM_bestSMI_Sep, DK2.5.se_SM_bestSMI_Sep, CT.se_SM_bestSMI_Sep)
labels1 <-c("NULL","WhiteCross","WhiteSerial", "DriscollKraay", "DriscollKray2.5","CameronThompson")
stargazer(plm.fit_SM_bestSMI_Sep, plm.fit_SM_bestSMI_Sep, plm.fit_SM_bestSMI_Sep, plm.fit_SM_bestSMI_Sep, plm.fit_SM_bestSMI_Sep,plm.fit_SM_bestSMI_Sep,
          se = se,   
          dep.var.caption  = "Model with smallest BIC with SMI - September",
          dep.var.labels = "log(Silomaize)",
          style="default",
          model.numbers = FALSE,
          column.labels = labels1,          
          type="text", out="./figures/figures_exploratory/BIC/Silomaize/SM_Sep_bestSM.txt"
)
