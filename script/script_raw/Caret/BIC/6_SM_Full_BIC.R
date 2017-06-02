############################
####  SiloMaize in June ####
############################

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
- Model with lowest BIC in general: Pet, Prec, SMI
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
- Yield_Covariates_SM_Jun.csv (auf June reduzierter Data.Frame)
- Export Data frame for use in BIC_Graphic: file="./data/data_raw/BIC/BIC_SM_Jun.csv")

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
names_Jun <- grep(c("*_Jun"), names)
names_Jun
Yield_Covariates_Jun <- Yield_Covariates[,names_Jun]
names(Yield_Covariates_Jun)
dim(Yield_Covariates_Jun)

## Delete all but SMI, Prec, Tavg and Pet
names(Yield_Covariates_Jun)
Yield_Covariates_Jun <- Yield_Covariates_Jun[,c(1:4)]

## Establish first part of data frame_ time and spatial reference plus Silomaize ##
names(Yield_Covariates[,c(2,1,3:5,7)])
Yield_Covariates_SM <- Yield_Covariates[,c(2,1,3:5,7)] # Achtung, darauf achten, dass comId und year in der richtigen Reihenfolge sind.
names(Yield_Covariates_SM)
head(Yield_Covariates_SM)

Yield_Covariates_SM_Jun <- cbind(Yield_Covariates_SM, Yield_Covariates_Jun)
names(Yield_Covariates_SM_Jun)
names(Yield_Covariates_SM_Jun) <- c( "comId" , "year","com","stateId","state","siloMaize","SMI", "Prec","Tavg", "Pet")
names(Yield_Covariates_SM_Jun)

#########################################
#### Create stepwise function of SMI ####
#########################################
' Drought Monitor Spezification '
Yield_Covariates_SM_Jun$SMI_GDM <- cut(Yield_Covariates_SM_Jun$SMI, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1),                                            ,
                                       labels = c("severe drought","moderate drought","abnormal dry", "normal","abnormal wet" ,"abundant wet", "severe wet"))
#############
## Na-omit ##
sum(is.na(Yield_Covariates_SM_Jun) )
dim(Yield_Covariates_SM_Jun)
Yield_Covariates_SM_Jun_nna <- na.omit(Yield_Covariates_SM_Jun) 
dim(Yield_Covariates_SM_Jun_nna)

## Check for NAs
any(is.na(Yield_Covariates_SM_Jun_nna))
## Reset Rownames
rownames(Yield_Covariates_SM_Jun_nna) <- NULL

## Further work with DataFrame without Yield_Covariates_SM_Jun index ##
Yield_Covariates_SM_Jun <- Yield_Covariates_SM_Jun_nna

#########################################################################
## Remove comIds with less than 7 observations to avoid leveage issues ## 
#########################################################################

#####################################################
## Delete all comIds with less than 7 observations ##
sum(table(Yield_Covariates_SM_Jun$comId) < 7 )
table(Yield_Covariates_SM_Jun$comId) < 7 

## comIds mit weniger als 7 Beoachtungen: ##
list <- c(3101, 3102, 3158, 3402, 5114, 5117, 5314,5315, 5334,5378, 5512, 5911, 5916, 6413, 7131, 7135, 7233, 7331, 7332, 7337, 7338, 7339, 8111,12052, 14612, 15001, 15082, 15083, 15084, 15085, 15086, 15087, 15088, 15089, 15091, 16051, 16052 )
length(list)
list[[1]]

temp <- Yield_Covariates_SM_Jun
for (i in 1:34)
{
  print(Yield_Covariates_SM_Jun[Yield_Covariates_SM_Jun$comId==list[i],])
  temp <- (temp[!temp$comId==list[i],])
}

## Number of deleted rows
dim(temp)-dim(Yield_Covariates_SM_Jun)

## Further use old name for data.frame
Yield_Covariates_SM_Jun <- temp

################################
## Befehle nach jedem löschen ##
Yield_Covariates_SM_Jun <- na.omit(Yield_Covariates_SM_Jun) 
rownames(Yield_Covariates_SM_Jun) <- NULL
Yield_Covariates_SM_Jun <- plm.data(Yield_Covariates_SM_Jun, index=c("comId", "year"))
Yield_Covariates_SM_Jun[,c("comId","stateId")] <- lapply(Yield_Covariates_SM_Jun[,c("comId","stateId")], factor )

#################################################
#### Remove log trend of indepedent variable ####
#################################################
'Fit log of yield on log of time and use the residuals of that for yields'
logtrend <- lm(log(siloMaize) ~ log(as.integer(year)), data= Yield_Covariates_SM_Jun)

##########################
## Issue with Outliers ###
##########################
par(mfrow = c(2,2))
plot(logtrend) 

## Look Outliers Values ##
Yield_Covariates_SM_Jun[c(1276, 3262, 3283, 3171,3255),]

## Look at other values of outliers com #
Yield_Covariates_SM_Jun[Yield_Covariates_SM_Jun$comId == "6532",] # 2008 hier scheint nur ein Jahr ein Messfehler zu sein: diesen Lösche ich 
Yield_Covariates_SM_Jun[Yield_Covariates_SM_Jun$comId == "12067",] # 2006 verändere ich nicht 
Yield_Covariates_SM_Jun[Yield_Covariates_SM_Jun$comId == "12069",] # 2003 verändere ich nicht 
Yield_Covariates_SM_Jun[Yield_Covariates_SM_Jun$comId == "12060",] # 1999 verändere ich nicht 
Yield_Covariates_SM_Jun[Yield_Covariates_SM_Jun$comId == "12067",] # 2006 verändere ich nicht 


## Delete outliers ##
' Da sich die Patterns bei den BIC Vergleichen nicht ändert, kümmere ich micht nicht weiter um die Outlier. 
Ich nehme nur sehr offensichtliche Messfehler raus.'
Yield_Covariates_SM_Jun <- Yield_Covariates_SM_Jun[!(Yield_Covariates_SM_Jun$comId == "6532" & Yield_Covariates_SM_Jun$year == "2008"),]

Yield_Covariates_SM_Jun <- na.omit(Yield_Covariates_SM_Jun)
rownames(Yield_Covariates_SM_Jun) <- NULL


#################################################
#### Remove log trend of indepedent variable ####
logtrend <- lm(log(siloMaize) ~ log(as.integer(year)), data= Yield_Covariates_SM_Jun)
summary(logtrend)
Yield_Covariates_SM_Jun$siloMaize_logtrend <- resid(logtrend)

#######################################
## Prepare dataframe for plm package ##
'Change Indexing so that it can be used in plm package'
Yield_Covariates_SM_Jun <- plm.data(Yield_Covariates_SM_Jun, index=c("comId", "year"))
str(Yield_Covariates_SM_Jun)


## Transform comId and stateId to factor ##
Yield_Covariates_SM_Jun[,c("comId","stateId")] <- lapply(Yield_Covariates_SM_Jun[,c("comId","stateId")], factor )
lapply(Yield_Covariates_SM_Jun, class)


###############################################
##### Save Yield_Covariates_SM_Juny extern ####
write.csv(Yield_Covariates_SM_Jun, file="./data/data_raw/Yield_Covariates_SM_Jun.csv")

#######################################################
#### BIC to choose the degrees of the polynomials  ####
#######################################################
## create a matrix which contains all possible degree combinations, here for three variables ##
degree <- permutations(n=3,r=2,v=c(1:3),repeats.allowed=T)
degree

################################################
## Formulas for Model Variations to be tested ##

## with SMI
formula_Jun_sm_detrendlog_SMIPrecTavg <- siloMaize_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  poly(Tavg, degree[r, 2], raw = T) + 
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)

formula_Jun_sm_detrendlog_SMIPrecPet <- siloMaize_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  poly(Pet, degree[r, 2], raw = T) + 
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)

formula_Jun_sm_detrendlog_SMIPrec <- siloMaize_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)

formula_Jun_sm_detrendlog_SMIPet <- siloMaize_logtrend ~ poly(Pet, degree[r, 2], raw = T) +  
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)

formula_Jun_sm_detrendlog_SMITavg <- siloMaize_logtrend ~ poly(Tavg, degree[r, 2], raw = T) + 
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)

formula_Jun_sm_detrendlog_SMI <- siloMaize_logtrend ~  
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)

## no SMI
formula_Jun_sm_detrendlog_PrecTavg <- siloMaize_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  poly(Tavg, degree[r, 2], raw = T) + dummy(comId)

formula_Jun_sm_detrendlog_PrecPet <- siloMaize_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  poly(Pet, degree[r, 2], raw = T) + dummy(comId)

formula_Jun_sm_detrendlog_Prec <- siloMaize_logtrend ~ poly(Prec, degree[r, 1], raw = T) + dummy(comId)

formula_Jun_sm_detrendlog_Pet <- siloMaize_logtrend ~ poly(Pet, degree[r, 2], raw = T)  + dummy(comId)

formula_Jun_sm_detrendlog_Tavg <- siloMaize_logtrend ~ poly(Tavg, degree[r, 2], raw = T)  + dummy(comId)

## Print formula
# formula_Jun_sm_detrendlog_SMIPrecTavg
# formula_Jun_sm_detrendlog_SMIPrecPet
# formula_Jun_sm_detrendlog_SMIPrec
# formula_Jun_sm_detrendlog_SMIPet
# formula_Jun_sm_detrendlog_SMITavg
# formula_Jun_sm_detrendlog_SMI
# formula_Jun_sm_detrendlog_PrecTavg
# formula_Jun_sm_detrendlog_PrecPet
# formula_Jun_sm_detrendlog_Prec
# formula_Jun_sm_detrendlog_Pet 
# formula_Jun_sm_detrendlog_Tavg

#################################################################################################
# Loop through the container list to cover all permutations of posssible degree of freedoms of ##
# of the polynomials of the variables                                                          ##
#################################################################################################

##################################################
## Loop through various variable configurations ##
BIC_SMIPrecTavg <- rep(0,9)
for(r in 1:9){
  glm.fit_SMIPrecTavg  <- glm(formula = formula_Jun_sm_detrendlog_SMIPrecTavg, data = Yield_Covariates_SM_Jun) 
  BIC_SMIPrecTavg[r] <- BIC(glm.fit_SMIPrecTavg)
}

BIC_SMIPrecPet <- rep(0,9)
for(r in 1:9){
  glm.fit_SMIPrecPet  <- glm(formula = formula_Jun_sm_detrendlog_SMIPrecPet,  data = Yield_Covariates_SM_Jun)
  BIC_SMIPrecPet[r] <- BIC(glm.fit_SMIPrecPet)
}

BIC_SMIPrec <- rep(0,9)
for(r in 1:9){  
  glm.fit_SMIPrec      <- glm(formula = formula_Jun_sm_detrendlog_SMIPrec,     data = Yield_Covariates_SM_Jun) 
  BIC_SMIPrec[r] <- BIC(glm.fit_SMIPrec)
}

BIC_SMIPet <- rep(0,9)
for(r in 1:9){  
  glm.fit_SMIPet       <- glm(formula = formula_Jun_sm_detrendlog_SMIPet,      data = Yield_Covariates_SM_Jun) 
  BIC_SMIPet[r] <- BIC(glm.fit_SMIPet)
}

BIC_SMITavg <- rep(0,9)
for(r in 1:9){  
  glm.fit_SMITavg      <- glm(formula = formula_Jun_sm_detrendlog_SMITavg,     data = Yield_Covariates_SM_Jun) 
  BIC_SMITavg[r] <- BIC(glm.fit_SMITavg)
}

BIC_SMI <- rep(0,9)
for(r in 1:9){  
  glm.fit_SMI      <- glm(formula = formula_Jun_sm_detrendlog_SMI,     data = Yield_Covariates_SM_Jun) 
  BIC_SMI[r] <- BIC(glm.fit_SMI)
}

BIC_PrecTavg <- rep(0,9)
for(r in 1:9){
  glm.fit_PrecTavg  <- glm(formula = formula_Jun_sm_detrendlog_PrecTavg, data = Yield_Covariates_SM_Jun) 
  BIC_PrecTavg[r] <- BIC(glm.fit_PrecTavg)
}

BIC_PrecPet <- rep(0,9)
for(r in 1:9){
  glm.fit_PrecPet  <- glm(formula = formula_Jun_sm_detrendlog_PrecPet, data = Yield_Covariates_SM_Jun) 
  BIC_PrecPet[r] <- BIC(glm.fit_PrecPet)
}

BIC_Prec <- rep(0,9)
for(r in 1:9){
  glm.fit_Prec  <- glm(formula = formula_Jun_sm_detrendlog_Prec, data = Yield_Covariates_SM_Jun) 
  BIC_Prec[r] <- BIC(glm.fit_Prec)
}

BIC_Pet  <- rep(0,9)
for(r in 1:9){
  glm.fit_Pet   <- glm(formula = formula_Jun_sm_detrendlog_Pet , data = Yield_Covariates_SM_Jun) 
  BIC_Pet [r] <- BIC(glm.fit_Pet )
}

BIC_Tavg  <- rep(0,9)
for(r in 1:9){
  glm.fit_Tavg   <- glm(formula = formula_Jun_sm_detrendlog_Tavg , data = Yield_Covariates_SM_Jun) 
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
month <-rep("June",99)

BIC_Jun <- cbind(BIC, model ,model_index, index, month)

#######################
## Delete Duplicates ##
which(duplicated(BIC_Jun$BIC))
list3 <- c(20,21,23,24,26,27,31,32,33,34,35,36,40,41,42,43,44,45,47,48,49,50,51,52,53,54,56,57,59,60,62,63,67,68,69,70,71,72,76,77,78,79,80,81))
length(list3)
temp <- BIC_Jun

for (i in 1:44)
{
  print(BIC_Jun[BIC_Jun$index ==list3[i],])
  temp <- (temp[!temp$index==list3[i],])
}
dim(BIC_Jun)
dim(temp)

################################
## Correct created data.frame ##
rownames(temp) <- NULL
BIC_Jun <- temp
lapply(BIC_Jun, class)

############################
## Plot data with ggplot2 ##
g <- ggplot(BIC_Jun,aes(y=BIC, x=index))
g + geom_point(aes(color=model)) + labs(title="BIC of various model configurations", x="") + theme(plot.title=element_text(size=15, face="bold")) + theme_dark()
g + geom_point(aes(color=model)) + labs(title="BIC of various model configurations", x="") + theme(plot.title=element_text(size=15, face="bold")) + theme_dark() +
  facet_wrap( ~ month)
BIC_Jun

## Export Data frame for use in BIC_Grafic
BIC_SM_Jun <- BIC_Jun
class(BIC_SM_Jun)
write.csv(BIC_SM_Jun, file="./data/data_raw/BIC/BIC_SM_Jun.csv")

################################################################
################################### Explore Models #############
################################################################
###################
## Load Data Set ##
# Yield_Covariates_SM_Jun <- read.csv( file="./data/data_raw/Yield_Covariates_SM_Jun.csv")
# names(Yield_Covariates_SM_Jun)
# Yield_Covariates_SM_Jun$X <- NULL

#######################################
## Prepare dataframe for plm package ##
'Change Indexing so that it can be used in plm package'
Yield_Covariates_SM_Jun <- plm.data(Yield_Covariates_SM_Jun, index=c("comId", "year"))

## Transform comId and stateId to factor ##
Yield_Covariates_SM_Jun[,c("comId","stateId")] <- lapply(Yield_Covariates_SM_Jun[,c("comId","stateId")], factor )
str(Yield_Covariates_SM_Jun)

#################################
###############################
## Results with smallest BIC ##
###############################
plot(BIC_SMIPrecPet)
which.min(BIC_SMIPrecPet)
r = 9
best_formula <- formula_Jun_sm_detrendlog_SMIPrecPet

###################
## GLM Ergebniss ##
glm.fit_SM_BEST_Jun  <- glm(formula = best_formula,  data = Yield_Covariates_SM_Jun)
summary(glm.fit_SM_BEST_Jun)
'AIC: -6494.3'

####################
## PLM Ergebnisse ##
plm.fit_SM_BEST_Jun <- plm(formula = update(best_formula, .~. - dummy(comId)),  data = Yield_Covariates_SM_Jun,  effect="individual", model=("within"), index = c("comId","year"))
summary(plm.fit_SM_BEST_Jun)
'Adj. R-Squared: 0.054164'

fixef <-  fixef(plm.fit_SM_BEST_Jun)
fixef <- as.data.frame(as.matrix(fixef))
head(fixef)
fixef <- cbind(rownames(fixef), fixef)
rownames(fixef) <- NULL
names(fixef) <- c("comId", "FE")
fixef
write.csv(fixef, "./figures/figures_exploratory/FixedEffects/Silomaize/plm.fit_SM_BEST_Jun_FE.csv")

##################
## LM Ergebniss ##
lm.fit_SM_BEST_Jun  <-lm(formula = best_formula,  data = Yield_Covariates_SM_Jun)
summary(lm.fit_SM_BEST_Jun) 
'Adjusted R-squared:   0.6345  '

################################################
## Assessing Influence (Leverage*discrepancy) ##
cutoff_SM_Jun <- 4/((nrow(Yield_Covariates_SM_Jun)-length(lm.fit_SM_BEST_Jun$coefficients)-1)) 
cutoff_SM_Jun
plot(lm.fit_SM_BEST_Jun, which=4)
cook_Jun <- cooks.distance(lm.fit_SM_BEST_Jun)

nrow(Yield_Covariates_SM_Jun[cook_Jun > cutoff_SM_Jun,]) # 189

year_cooks_SM_Jun <- table(Yield_Covariates_SM_Jun$year[cook_Jun > cutoff_SM_Jun ]) 
year_cooks_SM_Jun
'1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 
  11   10    7   10   51    5   14   27   20   13    8   13
'

com_cooks_SM_Jun <- sort(table(Yield_Covariates_SM_Jun$com[cook_Jun > cutoff_SM_Jun ] ) )
tail(com_cooks_SM_Jun,20)
'Darmstadt-Dieburg, Landkreis                  Hagen, Kreisfreie Stadt                     Havelland, Landkreis             M\xfcnster, Kreisfreie Stadt 
                                       3                                        3                                        3                                        3 
         M\xe4rkisch-Oderland, Landkreis          Neum\xfcnster, Kreisfreie Stadt                       Stendal, Landkreis                     Uckermark, Landkreis 
                                       3                                        3                                        3                                        3 
              Dahme-Spreewald, Landkreis         Oberspreewald-Lausitz, Landkreis                    Oder-Spree, Landkreis            Potsdam-Mittelmark, Landkreis 
                                       4                                        4                                        4                                        4 
               Spree-Nei\xdfe, Landkreis             Teltow-Fl\xe4ming, Landkreis                        Barnim, Landkreis                   Elbe-Elster, Landkreis 
                                       4                                        4                                        5                                        5 
      Frankfurt (Oder), Kreisfreie Stadt                     Oberhavel, Landkreis                              Olpe, Kreis               Siegen-Wittgenstein, Kreis 
                                       5                                        5                                        6                                        7 '




########################
## Heteroskedasdicity ##
bptest(glm.fit_SM_BEST_Jun) # Breusch Pagan Test of Heteroskedastie in den Störgrößen: Null: Homoskedasdicity.
bptest(plm.fit_SM_BEST_Jun)
' In beiden Fällen kann die Null widerlegt werden. Es gibt also heteroskedasdicity '

## Koenkers Version on BP Test: robuste Modification wenn die Störgrößen nicht normalverteilt sind. 
bptest(plm.fit_SM_BEST_Jun, studentize = TRUE)
'Auch hier kann die Null widerlegt werden. Need to use robust covariance variance matrix to correct standard errors'


######################################
## Tests for serial autocorrelation ##
pwartest(plm.fit_SM_BEST_Jun)
pbgtest(plm.fit_SM_BEST_Jun) 
'
both, H_1 of serial autocorrelation cannot be rejected
'

#################################
## Correct the Standard Errors ##
#################################

## Correct Standard Errors used in table ##
coeftest(plm.fit_SM_BEST_Jun)

## Robust covariance matrix estimators a la White ##
# coeftest(plm.fit_SM_BEST_Jun,vcov=vcovHC(plm.fit_SM_BEST_Jun,method = "arellano", type = "HC0")) 
cov0_SM_BEST_Jun <- vcovHC(plm.fit_SM_BEST_Jun,method = "arellano", type = "HC0", cluster="group")
Wh.se_serial_SM_BEST_Jun <- sqrt(diag(cov0_SM_BEST_Jun))

cov0.1_SM_BEST_Jun <- vcovHC(plm.fit_SM_BEST_Jun,method = "arellano", type = "HC0", cluster="time")
Wh.se_cross_SM_BEST_Jun <- sqrt(diag(cov0.1_SM_BEST_Jun))
# 
# ## Beck Katz:
# # coeftest(plm.fit_SM_BEST_Jun, vcov = function(x) vcovBK(plm.fit_SM_BEST_Jun,method = "arellano", type = "HC0"))
# cov1 <- vcovBK(plm.fit_SM_BEST_Jun,method = "arellano", type = "HC0",  cluster="time")
# BK.se <- sqrt(diag(cov1))

# ## Driscoll Kraay ##
# summary(plm.fit_SM_BEST_Jun)
coeftest(plm.fit_SM_BEST_Jun,  vcov=function(x) vcovSCC(plm.fit_SM_BEST_Jun,method = "arellano",type = "HC0")) 
cov2_SM_BEST_Jun     <- vcovSCC(plm.fit_SM_BEST_Jun,method = "arellano",type = "HC0")
DK.se_SM_BEST_Jun    <- sqrt(diag(cov2_SM_BEST_Jun))
# 
# cov2.1_SM_BEST_Jun   <- vcovSCC(plm.fit_SM_BEST_Jun,method = "arellano",type = "HC0", maxlag=1)
# DK2.1.se_SM_BEST_Jun <- sqrt(diag(cov2.1_SM_BEST_Jun))

# cov2.2_SM_BEST_Jun   <- vcovSCC(plm.fit_SM_BEST_Jun,method = "arellano",type = "HC0", maxlag=2)
# DK2.2.se_SM_BEST_Jun <- sqrt(diag(cov2.2_SM_BEST_Jun))
# 
# cov2.3_SM_BEST_Jun   <- vcovSCC(plm.fit_SM_BEST_Jun,method = "arellano",type = "HC0", maxlag=3)
# DK2.3.se_SM_BEST_Jun <- sqrt(diag(cov2.3_SM_BEST_Jun))
# 
# cov2.4_SM_BEST_Jun   <- vcovSCC(plm.fit_SM_BEST_Jun,method = "arellano",type = "HC0", maxlag=4)
# DK2.4.se_SM_BEST_Jun <- sqrt(diag(cov2.4_SM_BEST_Jun))
# 
cov2.5_SM_BEST_Jun   <- vcovSCC(plm.fit_SM_BEST_Jun,method = "arellano",type = "HC0", maxlag=5)
DK2.5.se_SM_BEST_Jun <- sqrt(diag(cov2.5_SM_BEST_Jun))

## Cameron et al /Thompson : doouble-clustering estimator  ##
# coeftest(plm.fit_SM_BEST_Jun, vcovDC(plm.fit_SM_BEST_Jun, method = "arellano", type = "HC0"))
cov3_SM_BEST_Jun <- vcovDC(plm.fit_SM_BEST_Jun, method = "arellano", type = "HC0")
CT.se_SM_BEST_Jun <- sqrt(diag(cov3_SM_BEST_Jun))
'Our estimator is qualitatively similar to the ones presented in White and Domowitz (1984), for
time series data, and Conley (1999), for spatial data. '

## Generate Table with Output ##
se <- list(NULL, Wh.se_cross_SM_BEST_Jun, Wh.se_serial_SM_BEST_Jun,  DK.se_SM_BEST_Jun, DK2.5.se_SM_BEST_Jun, CT.se_SM_BEST_Jun)
labels1 <-c("NULL","WhiteCross","WhiteSerial", "DriscollKraay", "DriscollKray2.5","CameronThompson")
stargazer(plm.fit_SM_BEST_Jun, plm.fit_SM_BEST_Jun, plm.fit_SM_BEST_Jun, plm.fit_SM_BEST_Jun, plm.fit_SM_BEST_Jun,plm.fit_SM_BEST_Jun,
          se = se,   
          dep.var.caption  = "Model with smallest BIC - June",
          dep.var.labels = "log(Silomaize)",
          style="default",
          model.numbers = FALSE,
          column.labels = labels1,          
          type="text", out="./figures/figures_exploratory/BIC/Silomaize/SM_Jun_best.txt"
)


#########################################################
## Results with smallest BIC of Standard Configuration ##
#########################################################
plot(BIC_SMIPrecTavg)
which.min(BIC_SMIPrecTavg)
r = 8
bestStandard_formula <- formula_Jun_sm_detrendlog_SMIPrecTavg

###################
## GLM Ergebniss ##
glm.fit_SM_bestStandard_Jun  <- glm(formula = bestStandard_formula,  data = Yield_Covariates_SM_Jun)
summary(glm.fit_SM_bestStandard_Jun)
'AIC: -7085.9'

####################
## PLM Ergebnisse ##
plm.fit_SM_bestStandard_Jun <- plm(formula = update(bestStandard_formula, .~. - dummy(comId)),  data = Yield_Covariates_SM_Jun,  effect="individual", model=("within"), index = c("comId","year"))
summary(plm.fit_SM_bestStandard_Jun)
'Adj. R-Squared: 0.17871'

## Generate Fixed Effects data.frame and export it ##
fixef <- fixef(plm.fit_SM_bestStandard_Jun)
fixef <- as.data.frame(as.matrix(fixef))
head(fixef)
fixef <- cbind(rownames(fixef), fixef)
rownames(fixef) <- NULL
names(fixef) <- c("comId", "FE")
write.csv(fixef, "./figures/figures_exploratory/FixedEffects/Silomaize/plm.fit_SM_bestStandard_Jun_FE.csv")

##################
## LM Ergebniss ##
lm.fit_SM_bestStandard_Jun  <-lm(formula = bestStandard_formula,  data = Yield_Covariates_SM_Jun)
summary(lm.fit_SM_bestStandard_Jun) 
'Adjusted R-squared:   0.6869'

########################
## Heteroskedasdicity ##
bptest(glm.fit_SM_bestStandard_Jun) # Breusch Pagan Test of Heteroskedastie in den Störgrößen: Null: Homoskedasdicity.
bptest(plm.fit_SM_bestStandard_Jun)
' In beiden Fällen kann die Null widerlegt werden. Es gibt also heteroskedasdicity '

## Koenkers Version on BP Test: robuste Modification wenn die Störgrößen nicht normalverteilt sind. 
bptest(plm.fit_SM_bestStandard_Jun, studentize = TRUE)
'Auch hier kann die Null widerlegt werden. Need to use robust covariance variance matrix to correct standard errors'

#########################
#### Autocorrelation ####

######################################
## Tests for serial autocorrelation ##
pwartest(plm.fit_SM_bestStandard_Jun)
' Hier serielle Korrelation festzustellen'
pbgtest(plm.fit_SM_bestStandard_Jun) 

'Solution for serial correlation: Cluster by groups.
Solution for cross sectional correlation: Cluster by time'

#################################
## Correct the Standard Errors ##

## Correct Standard Errors used in table ##
coeftest(plm.fit_SM_bestStandard_Jun)

## Robust covariance matrix estimators a la White 
# coeftest(plm.fit_SM_bestStandard_Jun,vcov=vcovHC(plm.fit_SM_bestStandard_Jun,method = "arellano", type = "HC0")) 
cov0_SM_bestStandard_Jun <- vcovHC(plm.fit_SM_bestStandard_Jun,method = "arellano", type = "HC0", cluster="group")
Wh.se_serial_SM_bestStandard_Jun <- sqrt(diag(cov0_SM_bestStandard_Jun))

cov0.1_SM_bestStandard_Jun <- vcovHC(plm.fit_SM_bestStandard_Jun,method = "arellano", type = "HC0", cluster="time")
Wh.se_cross_SM_bestStandard_Jun <- sqrt(diag(cov0.1_SM_bestStandard_Jun))

# ## Beck Katz ##
# # coeftest(plm.fit_SM_bestStandard_Jun, vcov = function(x) vcovBK(plm.fit_SM_bestStandard_Jun,method = "arellano", type = "HC0"))
# cov1_SM_bestStandard_Jun <- vcovBK(plm.fit_SM_bestStandard_Jun,method = "arellano", type = "HC0",  cluster="time")
# BK.se_SM_bestStandard_Jun <- sqrt(diag(cov1_SM_bestStandard_Jun))

## Driscoll Kraay: ##
summary(plm.fit_SM_bestStandard_Jun)
cov2_SM_bestStandard_Jun     <- vcovSCC(plm.fit_SM_bestStandard_Jun,method = "arellano",type = "HC0")
DK.se_SM_bestStandard_Jun    <- sqrt(diag(cov2_SM_bestStandard_Jun))

# cov2.1_SM_bestStandard_Jun   <- vcovSCC(plm.fit_SM_bestStandard_Jun,method = "arellano",type = "HC0", maxlag=1)
# DK2.1.se_SM_bestStandard_Jun <- sqrt(diag(cov2.1_SM_bestStandard_Jun))

# cov2.2_SM_bestStandard_Jun   <- vcovSCC(plm.fit_SM_bestStandard_Jun,method = "arellano",type = "HC0", maxlag=2)
# DK2.2.se_SM_bestStandard_Jun <- sqrt(diag(cov2.2_SM_bestStandard_Jun))
# 
# cov2.3_SM_bestStandard_Jun   <- vcovSCC(plm.fit_SM_bestStandard_Jun,method = "arellano",type = "HC0", maxlag=3)
# DK2.3.se_SM_bestStandard_Jun <- sqrt(diag(cov2.3_SM_bestStandard_Jun))
# 
# cov2.4_SM_bestStandard_Jun   <- vcovSCC(plm.fit_SM_bestStandard_Jun,method = "arellano",type = "HC0", maxlag=4)
# DK2.4.se_SM_bestStandard_Jun <- sqrt(diag(cov2.4_SM_bestStandard_Jun))
# 
cov2.5_SM_bestStandard_Jun   <- vcovSCC(plm.fit_SM_bestStandard_Jun,method = "arellano",type = "HC0", maxlag=5)
DK2.5.se_SM_bestStandard_Jun <- sqrt(diag(cov2.5_SM_bestStandard_Jun))

## Cameron et al /Thompson : doouble-clustering estimator 
# coeftest(plm.fit_SM_bestStandard_Jun, vcovDC(plm.fit_SM_bestStandard_Jun, method = "arellano", type = "HC0"))
cov3_SM_bestStandard_Jun <- vcovDC(plm.fit_SM_bestStandard_Jun, method = "arellano", type = "HC0")
CT.se_SM_bestStandard_Jun <- sqrt(diag(cov3_SM_bestStandard_Jun))
'Our estimator is qualitatively similar to the ones presented in White and Domowitz (1984), for
time series data, and Conley (1999), for spatial data. '

## Generate Table with Output ##
se <- list(NULL, Wh.se_cross_SM_bestStandard_Jun, Wh.se_serial_SM_bestStandard_Jun, DK.se_SM_bestStandard_Jun, DK2.5.se_SM_bestStandard_Jun, CT.se_SM_bestStandard_Jun)
labels1 <-c("NULL","WhiteCross","WhiteSerial", "DriscollKraay", "DriscollKray2.5","CameronThompson")
stargazer(plm.fit_SM_bestStandard_Jun, plm.fit_SM_bestStandard_Jun, plm.fit_SM_bestStandard_Jun, plm.fit_SM_bestStandard_Jun, plm.fit_SM_bestStandard_Jun,plm.fit_SM_bestStandard_Jun,
          se = se,   
          dep.var.caption  = "Model with smallest BIC of Standard Configuration - June",
          dep.var.labels = "log(Silomaize)",
          style="default",
          model.numbers = FALSE,
          column.labels = labels1,          
          type="text", out="./figures/figures_exploratory/BIC/Silomaize/SM_Jun_bestStandard.txt"
)

########################################
## Results with smallest BIC with SMI ##
########################################
plot(BIC_SMIPrecPet)
which.min(BIC_SMIPrecPet)
r = 9
bestSMI_formula <- formula_Jun_sm_detrendlog_SMIPrecPet

###################
## GLM Ergebniss ##
glm.fit_SM_bestSMI_Jun  <- glm(formula = bestSMI_formula,  data = Yield_Covariates_SM_Jun)
summary(glm.fit_SM_bestSMI_Jun)
'AIC: -7124.9'

####################
## PLM Ergebnisse ##
plm.fit_SM_bestSMI_Jun <- plm(formula = update(bestSMI_formula, .~. - dummy(comId)),  data = Yield_Covariates_SM_Jun,  effect="individual", model=("within"), index = c("comId","year"))
summary(plm.fit_SM_bestSMI_Jun)
'Adj. R-Squared: 0.18638'

fixef <- fixef(plm.fit_SM_bestSMI_Jun)
fixef <- as.data.frame(as.matrix(fixef))
head(fixef)
fixef <- cbind(rownames(fixef), fixef)
rownames(fixef) <- NULL
names(fixef) <- c("comId", "FE")
write.csv(fixef, "./figures/figures_exploratory/FixedEffects/Silomaize/plm.fit_SM_bestSMI_Jun_FE.csv")

##################
## LM Ergebniss ##
lm.fit_SM_bestSMI_Jun  <-lm(formula = bestSMI_formula,  data = Yield_Covariates_SM_Jun)
summary(lm.fit_SM_bestSMI_Jun) 
'Adjusted R-squared:  0.6901'

########################
## Heteroskedasdicity ##
bptest(glm.fit_SM_bestSMI_Jun) # Breusch Pagan Test of Heteroskedastie in den Störgrößen: Null: Homoskedasdicity.
bptest(plm.fit_SM_bestSMI_Jun)
' In beiden Fällen kann die Null widerlegt werden. Es gibt also heteroskedasdicity '

## Koenkers Version on BP Test: robuste Modification wenn die Störgrößen nicht normalverteilt sind. 
bptest(plm.fit_SM_bestSMI_Jun, studentize = TRUE)
'Auch hier kann die Null widerlegt werden. Need to use robust covariance variance matrix to correct standard errors'

#########################
#### Autocorrelation ####

######################################
## Tests for serial autocorrelation ##
pwartest(plm.fit_SM_bestSMI_Jun)
pbgtest(plm.fit_SM_bestSMI_Jun) 
'Hier serielle Korrelation festzustellen'

###########################################
## Correct Standard Errors used in table ##
coeftest(plm.fit_SM_bestSMI_Jun)

## Robust covariance matrix estimators a la White ##
# coeftest(plm.fit_SM_bestSMI_Jun,vcov=vcovHC(plm.fit_SM_bestSMI_Jun,method = "arellano", type = "HC0")) 
cov0_SM_bestSMI_Jun <- vcovHC(plm.fit_SM_bestSMI_Jun,method = "arellano", type = "HC0", cluster="group")
Wh.se_serial_SM_bestSMI_Jun <- sqrt(diag(cov0_SM_bestSMI_Jun))

cov0.1_SM_bestSMI_Jun <- vcovHC(plm.fit_SM_bestSMI_Jun,method = "arellano", type = "HC0", cluster="time")
Wh.se_cross_SM_bestSMI_Jun <- sqrt(diag(cov0.1_SM_bestSMI_Jun))
# 
# ## Beck Katz:
# # coeftest(plm.fit_SM_bestSMI_Jun, vcov = function(x) vcovBK(plm.fit_SM_bestSMI_Jun,method = "arellano", type = "HC0"))
# cov1 <- vcovBK(plm.fit_SM_bestSMI_Jun,method = "arellano", type = "HC0",  cluster="time")
# BK.se <- sqrt(diag(cov1))

## Driscoll Kraay ##
# summary(plm.fit_SM_bestSMI_Jun)
cov2_SM_bestSMI_Jun     <- vcovSCC(plm.fit_SM_bestSMI_Jun,method = "arellano",type = "HC0")
DK.se_SM_bestSMI_Jun    <- sqrt(diag(cov2_SM_bestSMI_Jun))

# cov2.1_SM_bestSMI_Jun   <- vcovSCC(plm.fit_SM_bestSMI_Jun,method = "arellano",type = "HC0", maxlag=1)
# DK2.1.se_SM_bestSMI_Jun <- sqrt(diag(cov2.1_SM_bestSMI_Jun))

# cov2.2_SM_bestSMI_Jun   <- vcovSCC(plm.fit_SM_bestSMI_Jun,method = "arellano",type = "HC0", maxlag=2)
# DK2.2.se_SM_bestSMI_Jun <- sqrt(diag(cov2.2_SM_bestSMI_Jun))
# 
# cov2.3_SM_bestSMI_Jun   <- vcovSCC(plm.fit_SM_bestSMI_Jun,method = "arellano",type = "HC0", maxlag=3)
# DK2.3.se_SM_bestSMI_Jun <- sqrt(diag(cov2.3_SM_bestSMI_Jun))
# 
# cov2.4_SM_bestSMI_Jun   <- vcovSCC(plm.fit_SM_bestSMI_Jun,method = "arellano",type = "HC0", maxlag=4)
# DK2.4.se_SM_bestSMI_Jun <- sqrt(diag(cov2.4_SM_bestSMI_Jun))
# 
cov2.5_SM_bestSMI_Jun   <- vcovSCC(plm.fit_SM_bestSMI_Jun,method = "arellano",type = "HC0", maxlag=5)
DK2.5.se_SM_bestSMI_Jun <- sqrt(diag(cov2.5_SM_bestSMI_Jun))

' Achtung: Driscoll Kraay ist nicht geeignet für kurze Panel. Ist das bei mir der Fall? '

## Cameron et al /Thompson : doouble-clustering estimator ##
cov3_SM_bestSMI_Jun <- vcovDC(plm.fit_SM_bestSMI_Jun, method = "arellano", type = "HC0")
CT.se_SM_bestSMI_Jun <- sqrt(diag(cov3_SM_bestSMI_Jun))

################################
## Generate Table with Output ##
se <- list(NULL, Wh.se_cross_SM_bestSMI_Jun, Wh.se_serial_SM_bestSMI_Jun,  DK.se_SM_bestSMI_Jun, DK2.5.se_SM_bestSMI_Jun, CT.se_SM_bestSMI_Jun)
labels1 <-c("NULL","WhiteCross","WhiteSerial", "DriscollKraay", "DriscollKray2.5","CameronThompson")
stargazer(plm.fit_SM_bestSMI_Jun, plm.fit_SM_bestSMI_Jun, plm.fit_SM_bestSMI_Jun, plm.fit_SM_bestSMI_Jun, plm.fit_SM_bestSMI_Jun,plm.fit_SM_bestSMI_Jun,
          se = se,   
          dep.var.caption  = "Model with smallest BIC with SMI - June",
          dep.var.labels = "log(Silomaize)",
          style="default",
          model.numbers = FALSE,
          column.labels = labels1,          
          type="text", out="./figures/figures_exploratory/BIC/Silomaize/SM_Jun_bestSM.txt"
)

