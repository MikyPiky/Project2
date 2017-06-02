#################################
####  SiloMaize in October ####
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
- Yield_Covariates_SM_Oct.csv (auf October reduzierter Data.Frame)
- Export Data frame for use in BIC_Graphic: file="./data/data_raw/BIC/BIC_SM_Oct.csv")

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
names_Oct <- grep(c("*_Oct"), names)
names_Oct
Yield_Covariates_Oct <- Yield_Covariates[,names_Oct]
names(Yield_Covariates_Oct)
dim(Yield_Covariates_Oct)

## Delete all but SMI, Prec, Tavg and Pet
names(Yield_Covariates_Oct)
Yield_Covariates_Oct <- Yield_Covariates_Oct[,c(1:4)]

## Establish first part of data frame_ time and spatial reference plus Silomaize ##
names(Yield_Covariates[,c(2,1,3:5,7)])
Yield_Covariates_SM <- Yield_Covariates[,c(2,1,3:5,7)] # Achtung, darauf achten, dass comId und year in der richtigen Reihenfolge sind.
names(Yield_Covariates_SM)
head(Yield_Covariates_SM)

Yield_Covariates_SM_Oct <- cbind(Yield_Covariates_SM, Yield_Covariates_Oct)
names(Yield_Covariates_SM_Oct)
names(Yield_Covariates_SM_Oct) <- c( "comId" , "year","com","stateId","state","siloMaize","SMI", "Prec","Tavg", "Pet")
names(Yield_Covariates_SM_Oct)

#########################################
#### Create stepwise function of SMI ####
#########################################
' Drought Monitor Spezification '
Yield_Covariates_SM_Oct$SMI_GDM <- cut(Yield_Covariates_SM_Oct$SMI, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1),                                            ,
                                       labels = c("severe drought","moderate drought","abnormal dry", "normal","abnormal wet" ,"abundant wet", "severe wet"))
#############
## Na-omit ##
sum(is.na(Yield_Covariates_SM_Oct) )
dim(Yield_Covariates_SM_Oct)
Yield_Covariates_SM_Oct_nna <- na.omit(Yield_Covariates_SM_Oct) 
dim(Yield_Covariates_SM_Oct_nna)

## Check for NAs
any(is.na(Yield_Covariates_SM_Oct_nna))
## Reset Rownames
rownames(Yield_Covariates_SM_Oct_nna) <- NULL

## Further work with DataFrame without Yield_Covariates_SM_Oct index ##
Yield_Covariates_SM_Oct <- Yield_Covariates_SM_Oct_nna

#########################################################################
## Remove comIds with less than 7 observations to avoid leveage issues ## 
#########################################################################

#####################################################
## Delete all comIds with less than 7 observations ##
sum(table(Yield_Covariates_SM_Oct$comId) < 7 )
table(Yield_Covariates_SM_Oct$comId) < 7 

## comIds mit weniger als 7 Beoachtungen: ##
list <- c(3101, 3102, 3158, 3402, 5114, 5117, 5314,5315, 5334,5378, 5512, 5911, 5916, 6413, 7131, 7135, 7233, 7331, 7332, 7337, 7338, 7339, 8111,12052, 14612, 15001, 15082, 15083, 15084, 15085, 15086, 15087, 15088, 15089, 15091, 16051, 16052 )
length(list)
list[[1]]

temp <- Yield_Covariates_SM_Oct
for (i in 1:34)
{
  print(Yield_Covariates_SM_Oct[Yield_Covariates_SM_Oct$comId==list[i],])
  temp <- (temp[!temp$comId==list[i],])
}

## Number of deleted rows
dim(temp)-dim(Yield_Covariates_SM_Oct)

## Further use old name for data.frame
Yield_Covariates_SM_Oct <- temp

################################
## Befehle nach jedem löschen ##
Yield_Covariates_SM_Oct <- na.omit(Yield_Covariates_SM_Oct) 
rownames(Yield_Covariates_SM_Oct) <- NULL
Yield_Covariates_SM_Oct <- plm.data(Yield_Covariates_SM_Oct, index=c("comId", "year"))
Yield_Covariates_SM_Oct[,c("comId","stateId")] <- lapply(Yield_Covariates_SM_Oct[,c("comId","stateId")], factor )

#################################################
#### Remove log trend of indepedent variable ####
#################################################
'Fit log of yield on log of time and use the residuals of that for yields'
logtrend <- lm(log(siloMaize) ~ log(as.integer(year)), data= Yield_Covariates_SM_Oct)

##########################
## Issue with Outliers ###
##########################
par(mfrow = c(2,2))
plot(logtrend) 

## Look Outliers Values ##
Yield_Covariates_SM_Oct[c(1276, 3262, 3283, 3171,3255),]

## Look at other values of outliers com #
Yield_Covariates_SM_Oct[Yield_Covariates_SM_Oct$comId == "6532",] # 2008 hier scheint nur ein Jahr ein Messfehler zu sein: diesen Lösche ich 
Yield_Covariates_SM_Oct[Yield_Covariates_SM_Oct$comId == "12067",] # 2006 verändere ich nicht 
Yield_Covariates_SM_Oct[Yield_Covariates_SM_Oct$comId == "12069",] # 2003 verändere ich nicht 
Yield_Covariates_SM_Oct[Yield_Covariates_SM_Oct$comId == "12060",] # 1999 verändere ich nicht 
Yield_Covariates_SM_Oct[Yield_Covariates_SM_Oct$comId == "12067",] # 2006 verändere ich nicht 


## Delete outliers ##
' Da sich die Patterns bei den BIC Vergleichen nicht ändert, kümmere ich micht nicht weiter um die Outlier. 
Ich nehme nur sehr offensichtliche Messfehler raus.'
Yield_Covariates_SM_Oct <- Yield_Covariates_SM_Oct[!(Yield_Covariates_SM_Oct$comId == "6532" & Yield_Covariates_SM_Oct$year == "2008"),]

Yield_Covariates_SM_Oct <- na.omit(Yield_Covariates_SM_Oct)
rownames(Yield_Covariates_SM_Oct) <- NULL


#################################################
#### Remove log trend of indepedent variable ####
logtrend <- lm(log(siloMaize) ~ log(as.integer(year)), data= Yield_Covariates_SM_Oct)
summary(logtrend)
Yield_Covariates_SM_Oct$siloMaize_logtrend <- resid(logtrend)

#######################################
## Prepare dataframe for plm package ##
'Change Indexing so that it can be used in plm package'
Yield_Covariates_SM_Oct <- plm.data(Yield_Covariates_SM_Oct, index=c("comId", "year"))
str(Yield_Covariates_SM_Oct)


## Transform comId and stateId to factor ##
Yield_Covariates_SM_Oct[,c("comId","stateId")] <- lapply(Yield_Covariates_SM_Oct[,c("comId","stateId")], factor )
lapply(Yield_Covariates_SM_Oct, class)


###############################################
##### Save Yield_Covariates_SM_October extern ####
write.csv(Yield_Covariates_SM_Oct, file="./data/data_raw/Yield_Covariates_SM_Oct.csv")

#######################################################
#### BIC to choose the degrees of the polynomials  ####
#######################################################
## create a matrix which contains all possible degree combinations, here for three variables ##
degree <- permutations(n=3,r=2,v=c(1:3),repeats.allowed=T)
degree

################################################
## Formulas for Model Variations to be tested ##

## with SMI
formula_Oct_sm_detrendlog_SMIPrecTavg <- siloMaize_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  poly(Tavg, degree[r, 2], raw = T) + 
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)

formula_Oct_sm_detrendlog_SMIPrecPet  <- siloMaize_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  poly(Pet, degree[r, 2], raw = T) + 
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)

formula_Oct_sm_detrendlog_SMIPrec     <- siloMaize_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)

formula_Oct_sm_detrendlog_SMIPet      <- siloMaize_logtrend ~ poly(Pet, degree[r, 2], raw = T) + 
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)

formula_Oct_sm_detrendlog_SMITavg     <- siloMaize_logtrend ~ poly(Tavg, degree[r, 2], raw = T) + 
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)

formula_Oct_sm_detrendlog_SMI         <- siloMaize_logtrend ~  
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)

## no SMI
formula_Oct_sm_detrendlog_PrecTavg <- siloMaize_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  poly(Tavg, degree[r, 2], raw = T) + dummy(comId)

formula_Oct_sm_detrendlog_PrecPet  <- siloMaize_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  poly(Pet, degree[r, 2], raw = T) + dummy(comId)

formula_Oct_sm_detrendlog_Prec     <- siloMaize_logtrend ~ poly(Prec, degree[r, 1], raw = T) + dummy(comId)

formula_Oct_sm_detrendlog_Pet      <- siloMaize_logtrend ~ poly(Pet, degree[r, 2], raw = T)  +  dummy(comId)

formula_Oct_sm_detrendlog_Tavg     <- siloMaize_logtrend ~ poly(Tavg, degree[r, 2], raw = T) + dummy(comId)

## Print formula
# formula_Oct_sm_detrendlog_SMIPrecTavg
# formula_Oct_sm_detrendlog_SMIPrecPet
# formula_Oct_sm_detrendlog_SMIPrec
# formula_Oct_sm_detrendlog_SMIPet
# formula_Oct_sm_detrendlog_SMITavg
# formula_Oct_sm_detrendlog_SMI
# formula_Oct_sm_detrendlog_PrecTavg
# formula_Oct_sm_detrendlog_PrecPet
# formula_Oct_sm_detrendlog_Prec
# formula_Oct_sm_detrendlog_Pet 
# formula_Oct_sm_detrendlog_Tavg

#################################################################################################
# Loop through the container list to cover all permutations of posssible degree of freedoms of ##
# of the polynomials of the variables                                                          ##
#################################################################################################

##################################################
## Loop through various variable configurations ##
BIC_SMIPrecTavg <- rep(0,9)
for(r in 1:9){
  glm.fit_SMIPrecTavg  <- glm(formula = formula_Oct_sm_detrendlog_SMIPrecTavg, data = Yield_Covariates_SM_Oct) 
  BIC_SMIPrecTavg[r] <- BIC(glm.fit_SMIPrecTavg)
}

BIC_SMIPrecPet <- rep(0,9)
for(r in 1:9){
  glm.fit_SMIPrecPet  <- glm(formula = formula_Oct_sm_detrendlog_SMIPrecPet,  data = Yield_Covariates_SM_Oct)
  BIC_SMIPrecPet[r] <- BIC(glm.fit_SMIPrecPet)
}

BIC_SMIPrec <- rep(0,9)
for(r in 1:9){  
  glm.fit_SMIPrec      <- glm(formula = formula_Oct_sm_detrendlog_SMIPrec,     data = Yield_Covariates_SM_Oct) 
  BIC_SMIPrec[r] <- BIC(glm.fit_SMIPrec)
}

BIC_SMIPet <- rep(0,9)
for(r in 1:9){  
  glm.fit_SMIPet       <- glm(formula = formula_Oct_sm_detrendlog_SMIPet,      data = Yield_Covariates_SM_Oct) 
  BIC_SMIPet[r] <- BIC(glm.fit_SMIPet)
}

BIC_SMITavg <- rep(0,9)
for(r in 1:9){  
  glm.fit_SMITavg      <- glm(formula = formula_Oct_sm_detrendlog_SMITavg,     data = Yield_Covariates_SM_Oct) 
  BIC_SMITavg[r] <- BIC(glm.fit_SMITavg)
}

BIC_SMI <- rep(0,9)
for(r in 1:9){  
  glm.fit_SMI      <- glm(formula = formula_Oct_sm_detrendlog_SMI,     data = Yield_Covariates_SM_Oct) 
  BIC_SMI[r] <- BIC(glm.fit_SMI)
}

BIC_PrecTavg <- rep(0,9)
for(r in 1:9){
  glm.fit_PrecTavg  <- glm(formula = formula_Oct_sm_detrendlog_PrecTavg, data = Yield_Covariates_SM_Oct) 
  BIC_PrecTavg[r] <- BIC(glm.fit_PrecTavg)
}

BIC_PrecPet <- rep(0,9)
for(r in 1:9){
  glm.fit_PrecPet  <- glm(formula = formula_Oct_sm_detrendlog_PrecPet, data = Yield_Covariates_SM_Oct) 
  BIC_PrecPet[r] <- BIC(glm.fit_PrecPet)
}

BIC_Prec <- rep(0,9)
for(r in 1:9){
  glm.fit_Prec  <- glm(formula = formula_Oct_sm_detrendlog_Prec, data = Yield_Covariates_SM_Oct) 
  BIC_Prec[r] <- BIC(glm.fit_Prec)
}

BIC_Pet  <- rep(0,9)
for(r in 1:9){
  glm.fit_Pet   <- glm(formula = formula_Oct_sm_detrendlog_Pet , data = Yield_Covariates_SM_Oct) 
  BIC_Pet [r] <- BIC(glm.fit_Pet )
}

BIC_Tavg  <- rep(0,9)
for(r in 1:9){
  glm.fit_Tavg   <- glm(formula = formula_Oct_sm_detrendlog_Tavg , data = Yield_Covariates_SM_Oct) 
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
month <-rep("October",99)

BIC_Oct <- cbind(BIC, model ,model_index, index, month)

#######################
## Delete Duplicates ##
which(duplicated(BIC_Oct$BIC))
list3 <- c(20,21,23,24,26,27,31,32,33,34,35,36,40,41,42,43,44,45,47,48,49,50,51,52,53,54,56,57,59,60,62,63,67,68,69,70,71,72,76,77,78,79,80,81)

length(list3)
temp <- BIC_Oct

for (i in 1:44)
{
  print(BIC_Oct[BIC_Oct$index ==list3[i],])
  temp <- (temp[!temp$index==list3[i],])
}
dim(BIC_Oct)
dim(temp)

################################
## Correct created data.frame ##
rownames(temp) <- NULL
BIC_Oct <- temp
lapply(BIC_Oct, class)

############################
## Plot data with ggplot2 ##
g <- ggplot(BIC_Oct,aes(y=BIC, x=index))
g + geom_point(aes(color=model)) + labs(title="BIC of various model configurations", x="") + theme(plot.title=element_text(size=15, face="bold")) + theme_dark()
g + geom_point(aes(color=model)) + labs(title="BIC of various model configurations", x="") + theme(plot.title=element_text(size=15, face="bold")) + theme_dark() +
  facet_wrap( ~ month)
BIC_Oct

## Export Data frame for use in BIC_Grafic
BIC_SM_Oct <- BIC_Oct
class(BIC_SM_Oct)
write.csv(BIC_SM_Oct, file="./data/data_raw/BIC/BIC_SM_Oct.csv")

################################################################
################################### Explore Models #############
################################################################
###################
## Load Data Set ##
# Yield_Covariates_SM_Oct <- read.csv( file="./data/data_raw/Yield_Covariates_SM_Oct.csv")
# names(Yield_Covariates_SM_Oct)
# Yield_Covariates_SM_Oct$X <- NULL

#######################################
## Prepare dataframe for plm package ##
'Change Indexing so that it can be used in plm package'
Yield_Covariates_SM_Oct <- plm.data(Yield_Covariates_SM_Oct, index=c("comId", "year"))

## Transform comId and stateId to factor ##
Yield_Covariates_SM_Oct[,c("comId","stateId")] <- lapply(Yield_Covariates_SM_Oct[,c("comId","stateId")], factor )
str(Yield_Covariates_SM_Oct)

#################################
###############################
## Results with smallest BIC ##
###############################
plot(BIC_SMIPrecPet)
which.min(BIC_SMIPrecPet)  
r = 3 
best_formula <- formula_Oct_sm_detrendlog_SMIPrecPet


###################
## GLM Ergebniss ##
glm.fit_SM_BEST_Oct  <- glm(formula = best_formula,  data = Yield_Covariates_SM_Oct)
summary(glm.fit_SM_BEST_Oct)
'AIC: -6799.2'

####################
## PLM Ergebnisse ##
plm.fit_SM_BEST_Oct <- plm(formula = update(best_formula, .~. - dummy(comId)),  data = Yield_Covariates_SM_Oct,  effect="individual", model=("within"), index = c("comId","year"))
summary(plm.fit_SM_BEST_Oct)
'Adj. R-Squared: 0.12195'

fixef <-  fixef(plm.fit_SM_BEST_Oct)
fixef <- as.data.frame(as.matrix(fixef))
head(fixef)
fixef <- cbind(rownames(fixef), fixef)
rownames(fixef) <- NULL
names(fixef) <- c("comId", "FE")
fixef
write.csv(fixef, "./figures/figures_exploratory/FixedEffects/Silomaize/plm.fit_SM_BEST_Oct_FE.csv")

##################
## LM Ergebniss ##
lm.fit_SM_BEST_Oct  <-lm(formula = best_formula,  data = Yield_Covariates_SM_Oct)
summary(lm.fit_SM_BEST_Oct) 
'Adjusted R-squared:  0.6628'

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
   7    4    8    7   52    4    6   25   24    8   17   23'

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
bptest(glm.fit_SM_BEST_Oct) # Breusch Pagan Test of Heteroskedastie in den Störgrößen: Null: Homoskedasdicity.
bptest(plm.fit_SM_BEST_Oct)
' In beiden Fällen kann die Null widerlegt werden. Es gibt also heteroskedasdicity '

## Koenkers Version on BP Test: robuste Modification wenn die Störgrößen nicht normalverteilt sind. 
bptest(plm.fit_SM_BEST_Oct, studentize = TRUE)
'Auch hier kann die Null widerlegt werden. Need to use robust covariance variance matrix to correct standard errors'

######################################
## Tests for serial autocorrelation ##
pwartest(plm.fit_SM_BEST_Oct)
pbgtest(plm.fit_SM_BEST_Oct) 
'
both, H_1 of serial autocorrelation cannot be rejected
'

#################################
## Correct the Standard Errors ##
#################################

## Correct Standard Errors used in table ##
coeftest(plm.fit_SM_BEST_Oct)

## Robust covariance matrix estimators a la White ##
# coeftest(plm.fit_SM_BEST_Oct,vcov=vcovHC(plm.fit_SM_BEST_Oct,method = "arellano", type = "HC0")) 
cov0_SM_BEST_Oct <- vcovHC(plm.fit_SM_BEST_Oct,method = "arellano", type = "HC0", cluster="group")
Wh.se_serial_SM_BEST_Oct <- sqrt(diag(cov0_SM_BEST_Oct))

cov0.1_SM_BEST_Oct <- vcovHC(plm.fit_SM_BEST_Oct,method = "arellano", type = "HC0", cluster="time")
Wh.se_cross_SM_BEST_Oct <- sqrt(diag(cov0.1_SM_BEST_Oct))
# 
# ## Beck Katz:
# # coeftest(plm.fit_SM_BEST_Oct, vcov = function(x) vcovBK(plm.fit_SM_BEST_Oct,method = "arellano", type = "HC0"))
# cov1 <- vcovBK(plm.fit_SM_BEST_Oct,method = "arellano", type = "HC0",  cluster="time")
# BK.se <- sqrt(diag(cov1))

# ## Driscoll Kraay ##
# summary(plm.fit_SM_BEST_Oct)
coeftest(plm.fit_SM_BEST_Oct,  vcov=function(x) vcovSCC(plm.fit_SM_BEST_Oct,method = "arellano",type = "HC0")) 
cov2_SM_BEST_Oct     <- vcovSCC(plm.fit_SM_BEST_Oct,method = "arellano",type = "HC0")
DK.se_SM_BEST_Oct    <- sqrt(diag(cov2_SM_BEST_Oct))
# 
# cov2.1_SM_BEST_Oct   <- vcovSCC(plm.fit_SM_BEST_Oct,method = "arellano",type = "HC0", maxlag=1)
# DK2.1.se_SM_BEST_Oct <- sqrt(diag(cov2.1_SM_BEST_Oct))

# cov2.2_SM_BEST_Oct   <- vcovSCC(plm.fit_SM_BEST_Oct,method = "arellano",type = "HC0", maxlag=2)
# DK2.2.se_SM_BEST_Oct <- sqrt(diag(cov2.2_SM_BEST_Oct))
# 
# cov2.3_SM_BEST_Oct   <- vcovSCC(plm.fit_SM_BEST_Oct,method = "arellano",type = "HC0", maxlag=3)
# DK2.3.se_SM_BEST_Oct <- sqrt(diag(cov2.3_SM_BEST_Oct))
# 
# cov2.4_SM_BEST_Oct   <- vcovSCC(plm.fit_SM_BEST_Oct,method = "arellano",type = "HC0", maxlag=4)
# DK2.4.se_SM_BEST_Oct <- sqrt(diag(cov2.4_SM_BEST_Oct))
# 
cov2.5_SM_BEST_Oct   <- vcovSCC(plm.fit_SM_BEST_Oct,method = "arellano",type = "HC0", maxlag=5)
DK2.5.se_SM_BEST_Oct <- sqrt(diag(cov2.5_SM_BEST_Oct))

## Cameron et al /Thompson : doouble-clustering estimator  ##
# coeftest(plm.fit_SM_BEST_Oct, vcovDC(plm.fit_SM_BEST_Oct, method = "arellano", type = "HC0"))
cov3_SM_BEST_Oct <- vcovDC(plm.fit_SM_BEST_Oct, method = "arellano", type = "HC0")
CT.se_SM_BEST_Oct <- sqrt(diag(cov3_SM_BEST_Oct))
'Our estimator is qualitatively similar to the ones presented in White and Domowitz (1984), for
time series data, and Conley (1999), for spatial data. '

## Generate Table with Output ##
se <- list(NULL, Wh.se_cross_SM_BEST_Oct, Wh.se_serial_SM_BEST_Oct,  DK.se_SM_BEST_Oct, DK2.5.se_SM_BEST_Oct, CT.se_SM_BEST_Oct)
labels1 <-c("NULL","WhiteCross","WhiteSerial", "DriscollKraay", "DriscollKray2.5","CameronThompson")
stargazer(plm.fit_SM_BEST_Oct, plm.fit_SM_BEST_Oct, plm.fit_SM_BEST_Oct, plm.fit_SM_BEST_Oct, plm.fit_SM_BEST_Oct,plm.fit_SM_BEST_Oct,
          se = se,   
          dep.var.caption  = "Model with smallest BIC - October",
          dep.var.labels = "log(Silomaize)",
          style="default",
          model.numbers = FALSE,
          column.labels = labels1,          
          type="text", out="./figures/figures_exploratory/BIC/Silomaize/SM_Oct_best.txt"
)


#########################################################
## Results with smallest BIC of Standard Configuration ##
#########################################################
plot(BIC_SMIPrecTavg)
which.min(BIC_SMIPrecTavg) 
r = 5
bestStandard_formula <- formula_Oct_sm_detrendlog_SMIPrecTavg


###################
## GLM Ergebniss ##
glm.fit_SM_bestStandard_Oct  <- glm(formula = bestStandard_formula,  data = Yield_Covariates_SM_Oct)
summary(glm.fit_SM_bestStandard_Oct)
'AIC: -6704'

####################
## PLM Ergebnisse ##
plm.fit_SM_bestStandard_Oct <- plm(formula = update(bestStandard_formula, .~. - dummy(comId)),  data = Yield_Covariates_SM_Oct,  effect="individual", model=("within"), index = c("comId","year"))
summary(plm.fit_SM_bestStandard_Oct)
'Adj. R-Squared: 0.082556'

## Generate Fixed Effects data.frame and export it ##
fixef <- fixef(plm.fit_SM_bestStandard_Oct)
fixef <- as.data.frame(as.matrix(fixef))
head(fixef)
fixef <- cbind(rownames(fixef), fixef)
rownames(fixef) <- NULL
names(fixef) <- c("comId", "FE")
write.csv(fixef, "./figures/figures_exploratory/FixedEffects/Silomaize/plm.fit_SM_bestStandard_Oct_FE.csv")

##################
## LM Ergebniss ##
lm.fit_SM_bestStandard_Oct  <-lm(formula = bestStandard_formula,  data = Yield_Covariates_SM_Oct)
summary(lm.fit_SM_bestStandard_Oct) 
'Adjusted R-squared:  0.6459 '

########################
## Heteroskedasdicity ##
bptest(glm.fit_SM_bestStandard_Oct) # Breusch Pagan Test of Heteroskedastie in den Störgrößen: Null: Homoskedasdicity.
bptest(plm.fit_SM_bestStandard_Oct)
' In beiden Fällen kann die Null widerlegt werden. Es gibt also heteroskedasdicity '

## Koenkers Version on BP Test: robuste Modification wenn die Störgrößen nicht normalverteilt sind. 
bptest(plm.fit_SM_bestStandard_Oct, studentize = TRUE)
'Auch hier kann die Null widerlegt werden. Need to use robust covariance variance matrix to correct standard errors'

#########################
#### Autocorrelation ####

######################################
## Tests for serial autocorrelation ##
pwartest(plm.fit_SM_bestStandard_Oct)
' Hier serielle Korrelation festzustellen'
pbgtest(plm.fit_SM_bestStandard_Oct) 

'Solution for serial correlation: Cluster by groups.
Solution for cross sectional correlation: Cluster by time'

#################################
## Correct the Standard Errors ##

## Correct Standard Errors used in table ##
coeftest(plm.fit_SM_bestStandard_Oct)

## Robust covariance matrix estimators a la White 
# coeftest(plm.fit_SM_bestStandard_Oct,vcov=vcovHC(plm.fit_SM_bestStandard_Oct,method = "arellano", type = "HC0")) 
cov0_SM_bestStandard_Oct <- vcovHC(plm.fit_SM_bestStandard_Oct,method = "arellano", type = "HC0", cluster="group")
Wh.se_serial_SM_bestStandard_Oct <- sqrt(diag(cov0_SM_bestStandard_Oct))

cov0.1_SM_bestStandard_Oct <- vcovHC(plm.fit_SM_bestStandard_Oct,method = "arellano", type = "HC0", cluster="time")
Wh.se_cross_SM_bestStandard_Oct <- sqrt(diag(cov0.1_SM_bestStandard_Oct))

# ## Beck Katz ##
# # coeftest(plm.fit_SM_bestStandard_Oct, vcov = function(x) vcovBK(plm.fit_SM_bestStandard_Oct,method = "arellano", type = "HC0"))
# cov1_SM_bestStandard_Oct <- vcovBK(plm.fit_SM_bestStandard_Oct,method = "arellano", type = "HC0",  cluster="time")
# BK.se_SM_bestStandard_Oct <- sqrt(diag(cov1_SM_bestStandard_Oct))

## Driscoll Kraay: ##
summary(plm.fit_SM_bestStandard_Oct)
cov2_SM_bestStandard_Oct     <- vcovSCC(plm.fit_SM_bestStandard_Oct,method = "arellano",type = "HC0")
DK.se_SM_bestStandard_Oct    <- sqrt(diag(cov2_SM_bestStandard_Oct))

# cov2.1_SM_bestStandard_Oct   <- vcovSCC(plm.fit_SM_bestStandard_Oct,method = "arellano",type = "HC0", maxlag=1)
# DK2.1.se_SM_bestStandard_Oct <- sqrt(diag(cov2.1_SM_bestStandard_Oct))

# cov2.2_SM_bestStandard_Oct   <- vcovSCC(plm.fit_SM_bestStandard_Oct,method = "arellano",type = "HC0", maxlag=2)
# DK2.2.se_SM_bestStandard_Oct <- sqrt(diag(cov2.2_SM_bestStandard_Oct))
# 
# cov2.3_SM_bestStandard_Oct   <- vcovSCC(plm.fit_SM_bestStandard_Oct,method = "arellano",type = "HC0", maxlag=3)
# DK2.3.se_SM_bestStandard_Oct <- sqrt(diag(cov2.3_SM_bestStandard_Oct))
# 
# cov2.4_SM_bestStandard_Oct   <- vcovSCC(plm.fit_SM_bestStandard_Oct,method = "arellano",type = "HC0", maxlag=4)
# DK2.4.se_SM_bestStandard_Oct <- sqrt(diag(cov2.4_SM_bestStandard_Oct))
# 
cov2.5_SM_bestStandard_Oct   <- vcovSCC(plm.fit_SM_bestStandard_Oct,method = "arellano",type = "HC0", maxlag=5)
DK2.5.se_SM_bestStandard_Oct <- sqrt(diag(cov2.5_SM_bestStandard_Oct))

## Cameron et al /Thompson : doouble-clustering estimator 
# coeftest(plm.fit_SM_bestStandard_Oct, vcovDC(plm.fit_SM_bestStandard_Oct, method = "arellano", type = "HC0"))
cov3_SM_bestStandard_Oct <- vcovDC(plm.fit_SM_bestStandard_Oct, method = "arellano", type = "HC0")
CT.se_SM_bestStandard_Oct <- sqrt(diag(cov3_SM_bestStandard_Oct))
'Our estimator is qualitatively similar to the ones presented in White and Domowitz (1984), for
time series data, and Conley (1999), for spatial data. '

## Generate Table with Output ##
se <- list(NULL, Wh.se_cross_SM_bestStandard_Oct, Wh.se_serial_SM_bestStandard_Oct, DK.se_SM_bestStandard_Oct, DK2.5.se_SM_bestStandard_Oct, CT.se_SM_bestStandard_Oct)
labels1 <-c("NULL","WhiteCross","WhiteSerial", "DriscollKraay", "DriscollKray2.5","CameronThompson")
stargazer(plm.fit_SM_bestStandard_Oct, plm.fit_SM_bestStandard_Oct, plm.fit_SM_bestStandard_Oct, plm.fit_SM_bestStandard_Oct, plm.fit_SM_bestStandard_Oct,plm.fit_SM_bestStandard_Oct,
          se = se,   
          dep.var.caption  = "Model with smallest BIC of Standard Configuration - October",
          dep.var.labels = "log(Silomaize)",
          style="default",
          model.numbers = FALSE,
          column.labels = labels1,          
          type="text", out="./figures/figures_exploratory/BIC/Silomaize/SM_Oct_bestStandard.txt"
)

########################################
## Results with smallest BIC with SMI ##
########################################
plot(BIC_SMIPrecPet)
which.min(BIC_SMIPrecPet) 
r = 3
bestSMI_formula <- formula_Oct_sm_detrendlog_SMIPrecPet

###################
## GLM Ergebniss ##
glm.fit_SM_bestSMI_Oct  <- glm(formula = bestSMI_formula,  data = Yield_Covariates_SM_Oct)
summary(glm.fit_SM_bestSMI_Oct)
'AIC: -6799.2'

####################
## PLM Ergebnisse ##
plm.fit_SM_bestSMI_Oct <- plm(formula = update(bestSMI_formula, .~. - dummy(comId)),  data = Yield_Covariates_SM_Oct,  effect="individual", model=("within"), index = c("comId","year"))
summary(plm.fit_SM_bestSMI_Oct)
'Adj. R-Squared: 0.12195'

fixef <- fixef(plm.fit_SM_bestSMI_Oct)
fixef <- as.data.frame(as.matrix(fixef))
head(fixef)
fixef <- cbind(rownames(fixef), fixef)
rownames(fixef) <- NULL
names(fixef) <- c("comId", "FE")
write.csv(fixef, "./figures/figures_exploratory/FixedEffects/Silomaize/plm.fit_SM_bestSMI_Oct_FE.csv")

##################
## LM Ergebniss ##
lm.fit_SM_bestSMI_Oct  <-lm(formula = bestSMI_formula,  data = Yield_Covariates_SM_Oct)
summary(lm.fit_SM_bestSMI_Oct) 
'Adjusted R-squared:  0.6628'

########################
## Heteroskedasdicity ##
bptest(glm.fit_SM_bestSMI_Oct) # Breusch Pagan Test of Heteroskedastie in den Störgrößen: Null: Homoskedasdicity.
bptest(plm.fit_SM_bestSMI_Oct)
' In beiden Fällen kann die Null widerlegt werden. Es gibt also heteroskedasdicity '

## Koenkers Version on BP Test: robuste Modification wenn die Störgrößen nicht normalverteilt sind. 
bptest(plm.fit_SM_bestSMI_Oct, studentize = TRUE)
'Auch hier kann die Null widerlegt werden. Need to use robust covariance variance matrix to correct standard errors'

#########################
#### Autocorrelation ####

######################################
## Tests for serial autocorrelation ##
pwartest(plm.fit_SM_bestSMI_Oct)
pbgtest(plm.fit_SM_bestSMI_Oct) 
'Hier serielle Korrelation festzustellen'

###########################################
## Correct Standard Errors used in table ##
coeftest(plm.fit_SM_bestSMI_Oct)

## Robust covariance matrix estimators a la White ##
# coeftest(plm.fit_SM_bestSMI_Oct,vcov=vcovHC(plm.fit_SM_bestSMI_Oct,method = "arellano", type = "HC0")) 
cov0_SM_bestSMI_Oct <- vcovHC(plm.fit_SM_bestSMI_Oct,method = "arellano", type = "HC0", cluster="group")
Wh.se_serial_SM_bestSMI_Oct <- sqrt(diag(cov0_SM_bestSMI_Oct))

cov0.1_SM_bestSMI_Oct <- vcovHC(plm.fit_SM_bestSMI_Oct,method = "arellano", type = "HC0", cluster="time")
Wh.se_cross_SM_bestSMI_Oct <- sqrt(diag(cov0.1_SM_bestSMI_Oct))
# 
# ## Beck Katz:
# # coeftest(plm.fit_SM_bestSMI_Oct, vcov = function(x) vcovBK(plm.fit_SM_bestSMI_Oct,method = "arellano", type = "HC0"))
# cov1 <- vcovBK(plm.fit_SM_bestSMI_Oct,method = "arellano", type = "HC0",  cluster="time")
# BK.se <- sqrt(diag(cov1))

## Driscoll Kraay ##
# summary(plm.fit_SM_bestSMI_Oct)
cov2_SM_bestSMI_Oct     <- vcovSCC(plm.fit_SM_bestSMI_Oct,method = "arellano",type = "HC0")
DK.se_SM_bestSMI_Oct    <- sqrt(diag(cov2_SM_bestSMI_Oct))

# cov2.1_SM_bestSMI_Oct   <- vcovSCC(plm.fit_SM_bestSMI_Oct,method = "arellano",type = "HC0", maxlag=1)
# DK2.1.se_SM_bestSMI_Oct <- sqrt(diag(cov2.1_SM_bestSMI_Oct))

# cov2.2_SM_bestSMI_Oct   <- vcovSCC(plm.fit_SM_bestSMI_Oct,method = "arellano",type = "HC0", maxlag=2)
# DK2.2.se_SM_bestSMI_Oct <- sqrt(diag(cov2.2_SM_bestSMI_Oct))
# 
# cov2.3_SM_bestSMI_Oct   <- vcovSCC(plm.fit_SM_bestSMI_Oct,method = "arellano",type = "HC0", maxlag=3)
# DK2.3.se_SM_bestSMI_Oct <- sqrt(diag(cov2.3_SM_bestSMI_Oct))
# 
# cov2.4_SM_bestSMI_Oct   <- vcovSCC(plm.fit_SM_bestSMI_Oct,method = "arellano",type = "HC0", maxlag=4)
# DK2.4.se_SM_bestSMI_Oct <- sqrt(diag(cov2.4_SM_bestSMI_Oct))
# 
cov2.5_SM_bestSMI_Oct   <- vcovSCC(plm.fit_SM_bestSMI_Oct,method = "arellano",type = "HC0", maxlag=5)
DK2.5.se_SM_bestSMI_Oct <- sqrt(diag(cov2.5_SM_bestSMI_Oct))


## Cameron et al /Thompson : doouble-clustering estimator ##
cov3_SM_bestSMI_Oct <- vcovDC(plm.fit_SM_bestSMI_Oct, method = "arellano", type = "HC0")
CT.se_SM_bestSMI_Oct <- sqrt(diag(cov3_SM_bestSMI_Oct))

################################
## Generate Table with Output ##
se <- list(NULL, Wh.se_cross_SM_bestSMI_Oct, Wh.se_serial_SM_bestSMI_Oct,  DK.se_SM_bestSMI_Oct, DK2.5.se_SM_bestSMI_Oct, CT.se_SM_bestSMI_Oct)
labels1 <-c("NULL","WhiteCross","WhiteSerial", "DriscollKraay", "DriscollKray2.5","CameronThompson")
stargazer(plm.fit_SM_bestSMI_Oct, plm.fit_SM_bestSMI_Oct, plm.fit_SM_bestSMI_Oct, plm.fit_SM_bestSMI_Oct, plm.fit_SM_bestSMI_Oct,plm.fit_SM_bestSMI_Oct,
          se = se,   
          dep.var.caption  = "Model with smallest BIC with SMI - October",
          dep.var.labels = "log(Silomaize)",
          style="default",
          model.numbers = FALSE,
          column.labels = labels1,          
          type="text", out="./figures/figures_exploratory/BIC/Silomaize/SM_Oct_bestSM.txt"
)

