##### Supermodel of Silo Maize ####
'
In this script I combine the results from the seperate models to estimate a comprehensive model to improve the prediction accuracy.

Moreover, it serves to show the persistence effect of soil moisture.

'

'
## Correct for Standard Errors ##
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
'


###################
## Load Packages ##
library("plm")
library("boot")
library("gtools")
library("lme4")
library(lmtest)
library(car)
library(ggplot2)

#################################################################################################################
#### Create data frame with siloMaize as dependent and variables of the month above as independent variables ####

## Read in large Dataframe for Maize ##
Yield_Covariates <- read.csv("~/Documents/projects/correlation/data/data_processed/Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin_Tmax_nodemean_nozscore_sm.csv")

## Chech out content of the data.frame ##
names(Yield_Covariates)

## Delete X column ##
Yield_Covariates$X <- NULL
head(Yield_Covariates)

## Rename Data.frame to ##
Yield_Covariates_SM_super <- Yield_Covariates

############################################
#### Prepare data for stepwise function ####
' Drought Monitor Spezification '
Yield_Covariates_SM_super$SMI_Jul_GDM <- cut(Yield_Covariates_SM_super$SMI_Jul, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1),                                            ,
                                             labels = c("severe drought","moderate drought","abnormal dry", "normal","abnormal wet" ,"abundant wet", "severe wet"))

Yield_Covariates_SM_super$SMI_Aug_GDM <- cut(Yield_Covariates_SM_super$SMI_Aug, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1),                                            ,
                                       labels = c("severe drought","moderate drought","abnormal dry", "normal","abnormal wet" ,"abundant wet", "severe wet"))

Yield_Covariates_SM_super$SMI_Sep_GDM <- cut(Yield_Covariates_SM_super$SMI_Sep, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1),                                            ,
                                             labels = c("severe drought","moderate drought","abnormal dry", "normal","abnormal wet" ,"abundant wet", "severe wet"))

Yield_Covariates_SM_super$SMI_Oct_GDM <- cut(Yield_Covariates_SM_super$SMI_Oct, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1),                                            ,
                                             labels = c("severe drought","moderate drought","abnormal dry", "normal","abnormal wet" ,"abundant wet", "severe wet"))
#############
## Na-omit ##
sum(is.na(Yield_Covariates_SM_super) )
dim(Yield_Covariates_SM_super)
Yield_Covariates_SM_super_nna <- na.omit(Yield_Covariates_SM_super) 
dim(Yield_Covariates_SM_super_nna)
any(is.na(Yield_Covariates_SM_super_nna))
rownames(Yield_Covariates_SM_super_nna) <- NULL
head(Yield_Covariates_SM_super_nna)

## Further work with DataFrame without Yield_Covariates_SM_super index ##
Yield_Covariates_SM_super <- Yield_Covariates_SM_super_nna

#####################################################
## Delete all comIds with less than 7 observations ##
sum(table(Yield_Covariates_SM_super$comId) < 7 )
table(Yield_Covariates_SM_super$comId) < 7 

## comIds mit weniger als 7 Beoachtungen: ##
list <- c(3101, 3102, 3158, 3402, 5114, 5117, 5314,5315, 5334, 5512, 5911, 5916, 6413, 7131, 7135, 7233, 7331, 7332, 7337, 7338, 7339, 8111,12052, 14612, 15001, 15082, 15083, 15084, 15085, 15086, 15087, 15088, 15089, 15091, 16051, 16052 )
length(list)
list[[35]]

temp <- Yield_Covariates_SM_super
for (i in 1:36)
{
  print(Yield_Covariates_SM_super[Yield_Covariates_SM_super$comId==list[i],])
  temp <- (temp[!temp$comId==list[i],])
}

dim(temp)-dim(Yield_Covariates_SM_super)

Yield_Covariates_SM_super <- temp

################################
## Befehle nach jedem löschen ##
Yield_Covariates_SM_super <- na.omit(Yield_Covariates_SM_super) 
rownames(Yield_Covariates_SM_super) <- NULL
Yield_Covariates_SM_super <- plm.data(Yield_Covariates_SM_super, index=c("comId", "year"))
names(Yield_Covariates_SM_super_nna)

Yield_Covariates_SM_super[,c("comId","comIdState")] <- lapply(Yield_Covariates_SM_super[,c("comId","comIdState")], factor )
Yield_Covariates_SM_super[,c("comId","comIdState")]
attach(Yield_Covariates_SM_super)

#################################################
#### Remove log trend of indepedent variable ####
##################################################
'Fit log of yield on log of time and use the residuals of that for yields'
logtrend <- lm(log(siloMaize) ~ log(as.integer(year)), data= Yield_Covariates_SM_super)

##########################
## Issue with Outliers ###
##########################
par(mfrow = c(2,2))
plot(logtrend) 


## Look Outliers Values ##
Yield_Covariates_SM_super[c(1283,3290,822),]
## Look at other values of outliers com #
Yield_Covariates_SM_super[Yield_Covariates_SM_super$comId == "6532",][1:10] # 2008 hier scheint nur ein Jahr ein Messfehler zu sein: diesen Lösche ich 
Yield_Covariates_SM_super[Yield_Covariates_SM_super$comId == "5378",][1:10] # 2006 hier scheint nur ein Jahr ein Messfehler zu sein: diesen Lösche ich 


## Delete outliers ##
Yield_Covariates_SM_super <- Yield_Covariates_SM_super[!(Yield_Covariates_SM_super$comId == "6532" & Yield_Covariates_SM_super$year == "2008"),]

## Lösche den ganzen Landkreis,da sonst zu wenige Beobachtungen
Yield_Covariates_SM_super <- Yield_Covariates_SM_super[!(Yield_Covariates_SM_super$comId == "5378") ,]



Yield_Covariates_SM_super <- na.omit(Yield_Covariates_SM_super)
rownames(Yield_Covariates_SM_super) <- NULL

#################################################
#### Remove log trend of indepedent variable ####
##################################################
'Fit log of yield on log of time and use the residuals of that for yields'
logtrend <- lm(log(siloMaize) ~ log(as.integer(year)), data= Yield_Covariates_SM_super)
Yield_Covariates_SM_super$siloMaize_logtrend <- resid(logtrend)


#######################################
## Prepare dataframe for plm package ##
'Change Indexing so that it can be used in plm package'
Yield_Covariates_SM_super <- plm.data(Yield_Covariates_SM_super, index=c("comId", "year"))
str(Yield_Covariates_SM_super)


#### BIC to choose the degrees of the polynomials  ####
##########################################################################

## create a matrix which contains all possible degree combinations, here for three variables ##
degree <- permutations(n=3,r=2,v=c(1:3),repeats.allowed=T)
degree
## June ##
formula_sm_detrendlog_PrecJunTavgJunSMIJul <- siloMaize_logtrend ~ poly(Prec_Jun, degree[r, 1], raw = T) +  poly(Tavg_Jun, degree[r, 2], raw = T) + 
  dummy(SMI_Jul_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)

formula_sm_detrendlog_PrecJunTavgJunSMIAug <- siloMaize_logtrend ~ poly(Prec_Jun, degree[r, 1], raw = T) +  poly(Tavg_Jun, degree[r, 2], raw = T) + 
  dummy(SMI_Aug_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)

formula_sm_detrendlog_PrecJunTavgJunSMISep <- siloMaize_logtrend ~ poly(Prec_Jun, degree[r, 1], raw = T) +  poly(Tavg_Jun, degree[r, 2], raw = T) + 
  dummy(SMI_Sep_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)

formula_sm_detrendlog_PrecJunTavgJunSMIOct <- siloMaize_logtrend ~ poly(Prec_Jun, degree[r, 1], raw = T) +  poly(Tavg_Jun, degree[r, 2], raw = T) + 
  dummy(SMI_Oct_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)


## July ##
formula_sm_detrendlog_PrecJulTavgJulSMIAug <- siloMaize_logtrend ~ poly(Prec_Jul, degree[r, 1], raw = T) +  poly(Tavg_Jul, degree[r, 2], raw = T) + 
  dummy(SMI_Aug_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)

formula_sm_detrendlog_PrecJulTavgJulSMISep <- siloMaize_logtrend ~ poly(Prec_Jul, degree[r, 1], raw = T) +  poly(Tavg_Jul, degree[r, 2], raw = T) + 
  dummy(SMI_Sep_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)

formula_sm_detrendlog_PrecJulTavgJulSMIOct <- siloMaize_logtrend ~ poly(Prec_Jul, degree[r, 1], raw = T) +  poly(Tavg_Jul, degree[r, 2], raw = T) + 
  dummy(SMI_Oct_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)

## August ##
formula_sm_detrendlog_PrecAugTavgAugSMISep <- siloMaize_logtrend ~ poly(Prec_Aug, degree[r, 1], raw = T) +  poly(Tavg_Aug, degree[r, 2], raw = T) + 
  dummy(SMI_Sep_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)

formula_sm_detrendlog_PrecAugTavgAugSMIOct <- siloMaize_logtrend ~ poly(Prec_Aug, degree[r, 1], raw = T) +  poly(Tavg_Aug, degree[r, 2], raw = T) + 
  dummy(SMI_Oct_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)


#################################################################################################
# Loop through the container list to cover all permutations of posssible degree of freedoms of ##
# of the polynomials of the variables                                                          ##
#################################################################################################

##################################################
## Loop through various variable configurations ##

## June ##
BIC_PrecJunTavgJunSMIJul <- rep(0,9)
for(r in 1:9){
  glm.fit_PrecJunTavgJunSMIJul  <- glm(formula = formula_sm_detrendlog_PrecJunTavgJunSMIJul,  data = Yield_Covariates_SM_super)
  BIC_PrecJunTavgJunSMIJul[r] <- BIC(glm.fit_PrecJunTavgJunSMIJul)
}


BIC_PrecJunTavgJunSMIAug <- rep(0,9)
for(r in 1:9){
  glm.fit_PrecJunTavgJunSMIAug  <- glm(formula = formula_sm_detrendlog_PrecJunTavgJunSMIAug,  data = Yield_Covariates_SM_super)
  BIC_PrecJunTavgJunSMIAug[r] <- BIC(glm.fit_PrecJunTavgJunSMIAug)
}

BIC_PrecJunTavgJunSMISep <- rep(0,9)
for(r in 1:9){
  glm.fit_PrecJunTavgJunSMISep  <- glm(formula = formula_sm_detrendlog_PrecJunTavgJunSMISep,  data = Yield_Covariates_SM_super)
  BIC_PrecJunTavgJunSMISep[r] <- BIC(glm.fit_PrecJunTavgJunSMISep)
}

BIC_PrecJunTavgJunSMIOct <- rep(0,9)
for(r in 1:9){
  glm.fit_PrecJunTavgJunSMIOct  <- glm(formula = formula_sm_detrendlog_PrecJunTavgJunSMIOct,  data = Yield_Covariates_SM_super)
  BIC_PrecJunTavgJunSMIOct[r] <- BIC(glm.fit_PrecJunTavgJunSMIOct)
}

## July ##
BIC_PrecJulTavgJulSMIAug <- rep(0,9)
for(r in 1:9){
  glm.fit_PrecJulTavgJulSMIAug  <- glm(formula = formula_sm_detrendlog_PrecJulTavgJulSMIAug,  data = Yield_Covariates_SM_super)
  BIC_PrecJulTavgJulSMIAug[r] <- BIC(glm.fit_PrecJulTavgJulSMIAug)
}

BIC_PrecJulTavgJulSMISep <- rep(0,9)
for(r in 1:9){
  glm.fit_PrecJulTavgJulSMISep  <- glm(formula = formula_sm_detrendlog_PrecJulTavgJulSMISep,  data = Yield_Covariates_SM_super)
  BIC_PrecJulTavgJulSMISep[r] <- BIC(glm.fit_PrecJulTavgJulSMISep)
}

BIC_PrecJulTavgJulSMIOct <- rep(0,9)
for(r in 1:9){
  glm.fit_PrecJulTavgJulSMIOct  <- glm(formula = formula_sm_detrendlog_PrecJulTavgJulSMIOct,  data = Yield_Covariates_SM_super)
  BIC_PrecJulTavgJulSMIOct[r] <- BIC(glm.fit_PrecJulTavgJulSMIOct)
}

## August ##
BIC_PrecAugTavgAugSMISep <- rep(0,9)
for(r in 1:9){
  glm.fit_PrecAugTavgAugSMISep  <- glm(formula = formula_sm_detrendlog_PrecAugTavgAugSMISep,  data = Yield_Covariates_SM_super)
  BIC_PrecAugTavgAugSMISep[r] <- BIC(glm.fit_PrecAugTavgAugSMISep)
}

BIC_PrecAugTavgAugSMIOct <- rep(0,9)
for(r in 1:9){
  glm.fit_PrecAugTavgAugSMIOct  <- glm(formula = formula_sm_detrendlog_PrecAugTavgAugSMIOct,  data = Yield_Covariates_SM_super)
  BIC_PrecAugTavgAugSMIOct[r] <- BIC(glm.fit_PrecAugTavgAugSMIOct)
}

#####################################################
## Compare plots of various formula specifications ##
#####################################################
BIC <- c(BIC_PrecJunTavgJunSMIJul,BIC_PrecJunTavgJunSMIAug, BIC_PrecJunTavgJunSMISep, BIC_PrecJunTavgJunSMIOct, BIC_PrecJulTavgJulSMIAug, BIC_PrecJulTavgJulSMISep, BIC_PrecJulTavgJulSMIOct, BIC_PrecAugTavgAugSMISep,BIC_PrecAugTavgAugSMIOct)
BIC
par(mfrow=c(1,1))
plot(BIC)

###########################
## Plot BIC with ggplot2 ##
###########################
##############################################
## Create Dataframe for plotting in ggplot2 ##

## repeat name of modelconfiguration ##
list <-c("12_PrecJunTavgJunSMIJul","13_PrecJunTavgJunSMIAug", "14_PrecJunTavgJunSMISep", "15_PrecJunTavgJunSMIOct","16_PrecJulTavgJulSMIAug", "17_PrecJulTavgJulSMISep", "18_PrecJulTavgJulSMIOct", "19_PrecAugTavgAugSMISep", "20_PrecAugTavgAugSMIOct")
list2 <- 12:20

model <- NULL
model_index <- NULL

for (i in 1:9)
{
  x <- rep(list[i],9)
  y <- rep(list2[i],9)
  model <- append(model, x)
  model_index <- as.numeric(append(model_index, y))
}

model <- as.data.frame(model)
model_index <- as.data.frame(model_index)
index <- 1:81
month <-rep("Super",9)

####################################
## Combine data in one data.frame ##
class(BIC)
BIC <- as.data.frame(BIC)
BIC2 <-cbind(BIC, model,model_index, index, month)
head(BIC2)

############################
## Plot data with ggplot2 ##
g <- ggplot(BIC2,aes(y=BIC, x=index))
g + geom_point(aes(color=model)) + labs(title="BIC of various model configurations", x="") + theme(plot.title=element_text(size=15, face="bold")) + theme_dark() +
  facet_wrap( ~ month)


## Write Dataframe ##
BIC_SM_super <- BIC2
class(BIC_SM_super)
write.csv(BIC_SM_super, file="./data/data_raw/BIC/BIC_SM_super.csv")


BIC_PrecJulTavgJulSMIAug <- BIC_PrecJulTavgJulSMIAug[3]
names(BIC_PrecJulTavgJulSMIAug) <- "BIC_PrecJulTavgJulSMIAug"
model_index_PrecJulTavgJulSMIAug <- rep(12,9)
model_PrecJulTavgJulSMIAug <- rep("12_PrecJulTavgJulSMIAug",9)
index_PrecJulTavgJulSMIAug <- 1:9
month_PrecJulTavgJulSMIAug <-rep("Super",9)
BIC_PrecJulTavgJulSMIAug1<-NULL
BIC_PrecJulTavgJulSMIAug1 <- cbind(BIC_PrecJulTavgJulSMIAug, model_PrecJulTavgJulSMIAug,model_index_PrecJulTavgJulSMIAug, index_PrecJulTavgJulSMIAug, month_PrecJulTavgJulSMIAug)
write.csv(BIC_PrecJulTavgJulSMIAug1, file="./data/data_raw/BIC/BIC_PrecJulTavgJulSMIAug.csv")
###########################################################################################################################################################################
#############################
#### Explore Supermodel  ####
#############################

################################################################################################################################################################
##########
## June ##
##########
################################
## Model PrecJunTavgJunSMIAug ## 
plot(BIC_PrecJunTavgJunSMIAug)
which.min(BIC_PrecJunTavgJunSMIAug) #9
r=8

formula_sm_detrendlog_PrecJunTavgJunSMIAug

###################
## GLM Ergebniss ##
glm.fit_SM_PrecJunTavgJunSMIAug  <- glm(formula = formula_sm_detrendlog_PrecJunTavgJunSMIAug,  data = Yield_Covariates_SM_super)
summary(glm.fit_SM_PrecJunTavgJunSMIAug)
'AIC: -7225.5'

## lm ##
lm.fit_SM_PrecJunTavgJunSMIAug  <-  lm(formula = formula_sm_detrendlog_PrecJunTavgJunSMIAug,  data = Yield_Covariates_SM_super)
summary(lm.fit_SM_PrecJunTavgJunSMIAug)
"Adjusted R-squared:  0.6973"

## plm ##
plm.fit_SM_PrecJunTavgJunSMIAug  <- plm(formula = update(formula_sm_detrendlog_PrecJunTavgJunSMIAug, .~. - dummy(comId)),  data = Yield_Covariates_SM_super,  effect="individual", model=("within"), index = c("comId","year"))
summary(plm.fit_SM_PrecJunTavgJunSMIAug)
'Adj. R-Squared: 0.20534'

fixef <- fixef(plm.fit_SM_PrecJunTavgJunSMIAug)
fixef <- as.data.frame(as.matrix(fixef))
head(fixef)
fixef <- cbind(rownames(fixef), fixef)
rownames(fixef) <- NULL
names(fixef) <- c("comId", "FE")
write.csv(fixef, "./figures/figures_exploratory/FixedEffects/Silomaize/plm.fit_SM_PrecJunTavgJunSMIAug.csv")

########################
## Heteroskedasdicity ##
bptest(glm.fit_SM_PrecJunTavgJunSMIAug) # Breusch Pagan Test of Heteroskedastie in den Störgrößen: Null: Homoskedasdicity.
bptest(plm.fit_SM_PrecJunTavgJunSMIAug)
' In beiden Fällen kann die Null widerlegt werden. Es gibt also heteroskedasdicity '

## Koenkers Version on BP Test: robuste Modification wenn die Störgrößen nicht normalverteilt sind. 
bptest(plm.fit_SM_PrecJunTavgJunSMIAug, studentize = TRUE)
'Auch hier kann die Null widerlegt werden. Need to use robust covariance variance matrix to correct standard errors'

#########################
#### Autocorrelation ####

######################################
## Tests for serial autocorrelation ##
pwartest(plm.fit_SM_PrecJunTavgJunSMIAug)
' Hier gibt es keine Serielle Korrelation festzustellen'
pbgtest(plm.fit_SM_PrecJunTavgJunSMIAug) 


#################################
## Correct the Standard Errors ##
## Compare ordinary with Robust covariance matrix estimators a la White for panel models (heteroscedasticity-consistent (HC) standard errors)
coeftest(plm.fit_SM_PrecJunTavgJunSMIAug)

## Robust covariance matrix estimators a la White 
# coeftest(plm.fit_SM_PrecJunTavgJunSMIAug,vcov=vcovHC(plm.fit_SM_PrecJunTavgJunSMIAug,method = "arellano", type = "HC0")) 
cov0_SM_PrecJunTavgJunSMIAug <- vcovHC(plm.fit_SM_PrecJunTavgJunSMIAug,method = "arellano", type = "HC0", cluster="group")
Wh.se_serial_SM_PrecJunTavgJunSMIAug <- sqrt(diag(cov0_SM_PrecJunTavgJunSMIAug))

cov0.1_SM_PrecJunTavgJunSMIAug <- vcovHC(plm.fit_SM_PrecJunTavgJunSMIAug,method = "arellano", type = "HC0", cluster="time")
Wh.se_cross_SM_PrecJunTavgJunSMIAug <- sqrt(diag(cov0.1_SM_PrecJunTavgJunSMIAug))
# 
# ## Beck Katz:
# # coeftest(plm.fit_SM_PrecJunTavgJunSMIAug, vcov = function(x) vcovBK(plm.fit_SM_PrecJunTavgJunSMIAug,method = "arellano", type = "HC0"))
# cov1 <- vcovBK(plm.fit_SM_PrecJunTavgJunSMIAug,method = "arellano", type = "HC0",  cluster="time")
# BK.se <- sqrt(diag(cov1))

# Driscoll Kraay:
# summary(plm.fit_SM_PrecJunTavgJunSMIAug)
coeftest(plm.fit_SM_PrecJunTavgJunSMIAug,  vcov=function(x) vcovSCC(plm.fit_SM_PrecJunTavgJunSMIAug,method = "arellano",type = "HC0")) 
cov2_SM_PrecJunTavgJunSMIAug     <- vcovSCC(plm.fit_SM_PrecJunTavgJunSMIAug,method = "arellano",type = "HC0")
DK.se_SM_PrecJunTavgJunSMIAug    <- sqrt(diag(cov2_SM_PrecJunTavgJunSMIAug))

# cov2.1   <- vcovSCC(plm.fit_SM_PrecJunTavgJunSMIAug,method = "arellano",type = "HC0", maxlag=1)
# DK2.1.se <- sqrt(diag(cov2.1))

# cov2.2   <- vcovSCC(plm.fit_SM_PrecJunTavgJunSMIAug,method = "arellano",type = "HC0", maxlag=2)
# DK2.2.se <- sqrt(diag(cov2.2))
# 
# cov2.3   <- vcovSCC(plm.fit_SM_PrecJunTavgJunSMIAug,method = "arellano",type = "HC0", maxlag=3)
# DK2.3.se <- sqrt(diag(cov2.3))
# 
# cov2.4   <- vcovSCC(plm.fit_SM_PrecJunTavgJunSMIAug,method = "arellano",type = "HC0", maxlag=4)
# DK2.4.se <- sqrt(diag(cov2.4))
# 
cov2.5_SM_PrecJunTavgJunSMIAug   <- vcovSCC(plm.fit_SM_PrecJunTavgJunSMIAug,method = "arellano",type = "HC0", maxlag=5)
DK2.5.se_SM_PrecJunTavgJunSMIAug <- sqrt(diag(cov2.5_SM_PrecJunTavgJunSMIAug))


## Cameron et al /Thompson : doouble-clustering estimator 
# coeftest(plm.fit_SM_PrecJunTavgJunSMIAug, vcovDC(plm.fit_SM_PrecJunTavgJunSMIAug, method = "arellano", type = "HC0"))
cov3_SM_PrecJunTavgJunSMIAug <- vcovDC(plm.fit_SM_PrecJunTavgJunSMIAug, method = "arellano", type = "HC0")
CT.se_SM_PrecJunTavgJunSMIAug <- sqrt(diag(cov3_SM_PrecJunTavgJunSMIAug))

## Generate Table with Output ##
se <- list(NULL, Wh.se_cross_SM_PrecJunTavgJunSMIAug, Wh.se_serial_SM_PrecJunTavgJunSMIAug,  DK.se_SM_PrecJunTavgJunSMIAug, DK2.5.se_SM_PrecJunTavgJunSMIAug, CT.se_SM_PrecJunTavgJunSMIAug)
labels1 <-c("NULL","WhiteCross","WhiteSerial", "DriscollKraay", "DriscollKray2.5","CameronThompson")
stargazer(plm.fit_SM_PrecJunTavgJunSMIAug, plm.fit_SM_PrecJunTavgJunSMIAug, plm.fit_SM_PrecJunTavgJunSMIAug, plm.fit_SM_PrecJunTavgJunSMIAug, plm.fit_SM_PrecJunTavgJunSMIAug,plm.fit_SM_PrecJunTavgJunSMIAug,
          se = se,   
          dep.var.caption  = "Super Model with _SM_PrecJunTavgJunSMIAug",
          dep.var.labels = "log(Silomaize)",
          style="default",
          model.numbers = FALSE,
          column.labels = labels1,          
          type="text", out="./figures/figures_exploratory/BIC/Silomaize/SM_Super_SM_PrecJunTavgJunSMIAug.txt"
)

################################################################################################################################################################
################################
## Model PrecJunTavgJunSMISep ## 
plot(BIC_PrecJunTavgJunSMISep)
which.min(BIC_PrecJunTavgJunSMISep) #9
r=8

formula_sm_detrendlog_PrecJunTavgJunSMISep

###################
## GLM Ergebniss ##
glm.fit_SM_PrecJunTavgJunSMISep  <- glm(formula = formula_sm_detrendlog_PrecJunTavgJunSMISep,  data = Yield_Covariates_SM_super)
summary(glm.fit_SM_PrecJunTavgJunSMISep)
'AIC: -7084.8'

## lm ##
lm.fit_SM_PrecJunTavgJunSMISep  <-  lm(formula = formula_sm_detrendlog_PrecJunTavgJunSMISep,  data = Yield_Covariates_SM_super)
summary(lm.fit_SM_PrecJunTavgJunSMISep)
"Adjusted R-squared:  0.686"

## plm ##
plm.fit_SM_PrecJunTavgJunSMISep  <- plm(formula = update(formula_sm_detrendlog_PrecJunTavgJunSMISep, .~. - dummy(comId)),  data = Yield_Covariates_SM_super,  effect="individual", model=("within"), index = c("comId","year"))
summary(plm.fit_SM_PrecJunTavgJunSMISep)
'Adj. R-Squared: 0.17908'

fixef <- fixef(plm.fit_SM_PrecJunTavgJunSMISep)
fixef <- as.data.frame(as.matrix(fixef))
head(fixef)
fixef <- cbind(rownames(fixef), fixef)
rownames(fixef) <- NULL
names(fixef) <- c("comId", "FE")
write.csv(fixef, "./figures/figures_exploratory/FixedEffects/Silomaize/plm.fit_SM_PrecJunTavgJunSMISep.csv")

########################
## Heteroskedasdicity ##
bptest(glm.fit_SM_PrecJunTavgJunSMISep) # Breusch Pagan Test of Heteroskedastie in den Störgrößen: Null: Homoskedasdicity.
bptest(plm.fit_SM_PrecJunTavgJunSMISep)
' In beiden Fällen kann die Null widerlegt werden. Es gibt also heteroskedasdicity '

## Koenkers Version on BP Test: robuste Modification wenn die Störgrößen nicht normalverteilt sind. 
bptest(plm.fit_SM_PrecJunTavgJunSMISep, studentize = TRUE)
'Auch hier kann die Null widerlegt werden. Need to use robust covariance variance matrix to correct standard errors'

#########################
#### Autocorrelation ####

######################################
## Tests for serial autocorrelation ##
pwartest(plm.fit_SM_PrecJunTavgJunSMISep)
' Hier gibt es keine Serielle Korrelation festzustellen'
pbgtest(plm.fit_SM_PrecJunTavgJunSMISep) 


#################################
## Correct the Standard Errors ##
## Compare ordinary with Robust covariance matrix estimators a la White for panel models (heteroscedasticity-consistent (HC) standard errors)
coeftest(plm.fit_SM_PrecJunTavgJunSMISep)

## Robust covariance matrix estimators a la White 
# coeftest(plm.fit_SM_PrecJunTavgJunSMISep,vcov=vcovHC(plm.fit_SM_PrecJunTavgJunSMISep,method = "arellano", type = "HC0")) 
cov0_SM_PrecJunTavgJunSMISep <- vcovHC(plm.fit_SM_PrecJunTavgJunSMISep,method = "arellano", type = "HC0", cluster="group")
Wh.se_serial_SM_PrecJunTavgJunSMISep <- sqrt(diag(cov0_SM_PrecJunTavgJunSMISep))

cov0.1_SM_PrecJunTavgJunSMISep <- vcovHC(plm.fit_SM_PrecJunTavgJunSMISep,method = "arellano", type = "HC0", cluster="time")
Wh.se_cross_SM_PrecJunTavgJunSMISep <- sqrt(diag(cov0.1_SM_PrecJunTavgJunSMISep))
# 
# ## Beck Katz:
# # coeftest(plm.fit_SM_PrecJunTavgJunSMISep, vcov = function(x) vcovBK(plm.fit_SM_PrecJunTavgJunSMISep,method = "arellano", type = "HC0"))
# cov1 <- vcovBK(plm.fit_SM_PrecJunTavgJunSMISep,method = "arellano", type = "HC0",  cluster="time")
# BK.se <- sqrt(diag(cov1))

# Driscoll Kraay:
# summary(plm.fit_SM_PrecJunTavgJunSMISep)
coeftest(plm.fit_SM_PrecJunTavgJunSMISep,  vcov=function(x) vcovSCC(plm.fit_SM_PrecJunTavgJunSMISep,method = "arellano",type = "HC0")) 
cov2_SM_PrecJunTavgJunSMISep     <- vcovSCC(plm.fit_SM_PrecJunTavgJunSMISep,method = "arellano",type = "HC0")
DK.se_SM_PrecJunTavgJunSMISep    <- sqrt(diag(cov2_SM_PrecJunTavgJunSMISep))

# cov2.1   <- vcovSCC(plm.fit_SM_PrecJunTavgJunSMISep,method = "arellano",type = "HC0", maxlag=1)
# DK2.1.se <- sqrt(diag(cov2.1))

# cov2.2   <- vcovSCC(plm.fit_SM_PrecJunTavgJunSMISep,method = "arellano",type = "HC0", maxlag=2)
# DK2.2.se <- sqrt(diag(cov2.2))
# 
# cov2.3   <- vcovSCC(plm.fit_SM_PrecJunTavgJunSMISep,method = "arellano",type = "HC0", maxlag=3)
# DK2.3.se <- sqrt(diag(cov2.3))
# 
# cov2.4   <- vcovSCC(plm.fit_SM_PrecJunTavgJunSMISep,method = "arellano",type = "HC0", maxlag=4)
# DK2.4.se <- sqrt(diag(cov2.4))
# 
cov2.5_SM_PrecJunTavgJunSMISep   <- vcovSCC(plm.fit_SM_PrecJunTavgJunSMISep,method = "arellano",type = "HC0", maxlag=5)
DK2.5.se_SM_PrecJunTavgJunSMISep <- sqrt(diag(cov2.5_SM_PrecJunTavgJunSMISep))

## Cameron et al /Thompson : doouble-clustering estimator 
# coeftest(plm.fit_SM_PrecJunTavgJunSMISep, vcovDC(plm.fit_SM_PrecJunTavgJunSMISep, method = "arellano", type = "HC0"))
cov3_SM_PrecJunTavgJunSMISep <- vcovDC(plm.fit_SM_PrecJunTavgJunSMISep, method = "arellano", type = "HC0")
CT.se_SM_PrecJunTavgJunSMISep <- sqrt(diag(cov3_SM_PrecJunTavgJunSMISep))

## Generate Table with Output ##
se <- list(NULL, Wh.se_cross_SM_PrecJunTavgJunSMISep, Wh.se_serial_SM_PrecJunTavgJunSMISep,  DK.se_SM_PrecJunTavgJunSMISep, DK2.5.se_SM_PrecJunTavgJunSMISep, CT.se_SM_PrecJunTavgJunSMISep)
labels1 <-c("NULL","WhiteCross","WhiteSerial", "DriscollKraay", "DriscollKray2.5","CameronThompson")
stargazer(plm.fit_SM_PrecJunTavgJunSMISep, plm.fit_SM_PrecJunTavgJunSMISep, plm.fit_SM_PrecJunTavgJunSMISep, plm.fit_SM_PrecJunTavgJunSMISep, plm.fit_SM_PrecJunTavgJunSMISep,plm.fit_SM_PrecJunTavgJunSMISep,
          se = se,   
          dep.var.caption  = "Super Model with _SM_PrecJunTavgJunSMISep",
          dep.var.labels = "log(Silomaize)",
          style="default",
          model.numbers = FALSE,
          column.labels = labels1,          
          type="text", out="./figures/figures_exploratory/BIC/Silomaize/SM_Super_SM_PrecJunTavgJunSMISep.txt"
)

##########
## July ##
##########
################################
## Model PrecJulTavgJulSMIAug ##
plot(BIC_PrecJulTavgJulSMIAug)
which.min(BIC_PrecJulTavgJulSMIAug)
r=6
' Wegen Singularität gehe ich auf r = 6'

formula_sm_detrendlog_PrecJulTavgJulSMIAug

###################
## GLM Ergebniss ##
glm.fit_SM_PrecJulTavgJulSMIAug  <- glm(formula = formula_sm_detrendlog_PrecJulTavgJulSMIAug,  data = Yield_Covariates_SM_super)
summary(glm.fit_SM_PrecJulTavgJulSMIAug)
'AIC: -7688.2'

## lm ##
lm.fit_SM_PrecJulTavgJulSMIAug  <-  lm(formula = formula_sm_detrendlog_PrecJulTavgJulSMIAug,  data = Yield_Covariates_SM_super)
summary(lm.fit_SM_PrecJulTavgJulSMIAug)
'Adjusted R-squared:  0.7315'

## plm ##
plm.fit_SM_PrecJulTavgJulSMIAug  <- plm(formula = update(formula_sm_detrendlog_PrecJulTavgJulSMIAug, .~. - dummy(comId)),  data = Yield_Covariates_SM_super,  effect="individual", model=("within"), index = c("comId","year"))
summary(plm.fit_SM_PrecJulTavgJulSMIAug)
'Adj. R-Squared: 0.28523'

fixef <- fixef(plm.fit_SM_PrecJulTavgJulSMIAug)
fixef <- as.data.frame(as.matrix(fixef))
head(fixef)
fixef <- cbind(rownames(fixef), fixef)
rownames(fixef) <- NULL
names(fixef) <- c("comId", "FE")
write.csv(fixef, "./figures/figures_exploratory/FixedEffects/Silomaize/plm.fit_SM_PrecJulTavgJulSMIAug.csv")

########################
## Heteroskedasdicity ##
bptest(glm.fit_SM_PrecJulTavgJulSMIAug) # Breusch Pagan Test of Heteroskedastie in den Störgrößen: Null: Homoskedasdicity.
bptest(plm.fit_SM_PrecJulTavgJulSMIAug)
' In beiden Fällen kann die Null widerlegt werden. Es gibt also heteroskedasdicity '

## Koenkers Version on BP Test: robuste Modification wenn die Störgrößen nicht normalverteilt sind. 
bptest(plm.fit_SM_PrecJulTavgJulSMIAug, studentize = TRUE)
'Auch hier kann die Null widerlegt werden. Need to use robust covariance variance matrix to correct standard errors'

#########################
#### Autocorrelation ####

######################################
## Tests for serial autocorrelation ##
pwartest(plm.fit_SM_PrecJulTavgJulSMIAug)
' Hier serielle Korrelation festzustellen'
pbgtest(plm.fit_SM_PrecJulTavgJulSMIAug) 

'Solution for serial correlation: Cluster by groups.
Solution for cross sectional correlation: Cluster by time'

#################################
## Correct the Standard Errors ##
## Compare ordinary with Robust covariance matrix estimators a la White for panel models (heteroscedasticity-consistent (HC) standard errors)
coeftest(plm.fit_SM_PrecJulTavgJulSMIAug)

## Robust covariance matrix estimators a la White 
# coeftest(plm.fit_SM_PrecJulTavgJulSMIAug,vcov=vcovHC(plm.fit_SM_PrecJulTavgJulSMIAug,method = "arellano", type = "HC0")) 
cov0_SM_PrecJulTavgJulSMIAug <- vcovHC(plm.fit_SM_PrecJulTavgJulSMIAug,method = "arellano", type = "HC0", cluster="group")
Wh.se_serial_SM_PrecJulTavgJulSMIAug <- sqrt(diag(cov0_SM_PrecJulTavgJulSMIAug))

cov0.1_SM_PrecJulTavgJulSMIAug <- vcovHC(plm.fit_SM_PrecJulTavgJulSMIAug,method = "arellano", type = "HC0", cluster="time")
Wh.se_cross_SM_PrecJulTavgJulSMIAug <- sqrt(diag(cov0.1_SM_PrecJulTavgJulSMIAug))
# 
# ## Beck Katz:
# # coeftest(plm.fit_SM_PrecJulTavgJulSMIAug, vcov = function(x) vcovBK(plm.fit_SM_PrecJulTavgJulSMIAug,method = "arellano", type = "HC0"))
# cov1 <- vcovBK(plm.fit_SM_PrecJulTavgJulSMIAug,method = "arellano", type = "HC0",  cluster="time")
# BK.se <- sqrt(diag(cov1))

# Driscoll Kraay:
# summary(plm.fit_SM_PrecJulTavgJulSMIAug)
coeftest(plm.fit_SM_PrecJulTavgJulSMIAug,  vcov=function(x) vcovSCC(plm.fit_SM_PrecJulTavgJulSMIAug,method = "arellano",type = "HC0")) 
cov2_SM_PrecJulTavgJulSMIAug     <- vcovSCC(plm.fit_SM_PrecJulTavgJulSMIAug,method = "arellano",type = "HC0")
DK.se_SM_PrecJulTavgJulSMIAug    <- sqrt(diag(cov2_SM_PrecJulTavgJulSMIAug))

# cov2.1   <- vcovSCC(plm.fit_SM_PrecJulTavgJulSMIAug,method = "arellano",type = "HC0", maxlag=1)
# DK2.1.se <- sqrt(diag(cov2.1))

# cov2.2   <- vcovSCC(plm.fit_SM_PrecJulTavgJulSMIAug,method = "arellano",type = "HC0", maxlag=2)
# DK2.2.se <- sqrt(diag(cov2.2))
# 
# cov2.3   <- vcovSCC(plm.fit_SM_PrecJulTavgJulSMIAug,method = "arellano",type = "HC0", maxlag=3)
# DK2.3.se <- sqrt(diag(cov2.3))
# 
# cov2.4   <- vcovSCC(plm.fit_SM_PrecJulTavgJulSMIAug,method = "arellano",type = "HC0", maxlag=4)
# DK2.4.se <- sqrt(diag(cov2.4))
# 
cov2.5_SM_PrecJulTavgJulSMIAug   <- vcovSCC(plm.fit_SM_PrecJulTavgJulSMIAug,method = "arellano",type = "HC0", maxlag=5)
DK2.5.se_SM_PrecJulTavgJulSMIAug <- sqrt(diag(cov2.5_SM_PrecJulTavgJulSMIAug))


## Cameron et al /Thompson : doouble-clustering estimator 
# coeftest(plm.fit_SM_PrecJulTavgJulSMIAug, vcovDC(plm.fit_SM_PrecJulTavgJulSMIAug, method = "arellano", type = "HC0"))
cov3_SM_PrecJulTavgJulSMIAug <- vcovDC(plm.fit_SM_PrecJulTavgJulSMIAug, method = "arellano", type = "HC0")
CT.se_SM_PrecJulTavgJulSMIAug <- sqrt(diag(cov3_SM_PrecJulTavgJulSMIAug))

## Generate Table with Output ##
se <- list(NULL, Wh.se_cross_SM_PrecJulTavgJulSMIAug, Wh.se_serial_SM_PrecJulTavgJulSMIAug,  DK.se_SM_PrecJulTavgJulSMIAug, DK2.5.se_SM_PrecJulTavgJulSMIAug, CT.se_SM_PrecJulTavgJulSMIAug)
labels1 <-c("NULL","WhiteCross","WhiteSerial", "DriscollKraay", "DriscollKray2.5","CameronThompson")
stargazer(plm.fit_SM_PrecJulTavgJulSMIAug, plm.fit_SM_PrecJulTavgJulSMIAug, plm.fit_SM_PrecJulTavgJulSMIAug, plm.fit_SM_PrecJulTavgJulSMIAug, plm.fit_SM_PrecJulTavgJulSMIAug,plm.fit_SM_PrecJulTavgJulSMIAug,
          se = se,   
          dep.var.caption  = "Super Model with _SM_PrecJulTavgJulSMIAug",
          dep.var.labels = "log(Silomaize)",
          style="default",
          model.numbers = FALSE,
          column.labels = labels1,          
          type="text", out="./figures/figures_exploratory/BIC/Silomaize/SM_Super_SM_PrecJulTavgJulSMIAug.txt"
)

################################################################################################################################################################
################################
## Model PrecJulTavgJulSMISep ##
plot(BIC_PrecJulTavgJulSMISep)
which.min(BIC_PrecJulTavgJulSMISep)
r=6
'Da das Model r=9 weniger als sechs Einheiten von r=6 ist, nehme ich das weniger komplexe Model'

formula_sm_detrendlog_PrecJulTavgJulSMISep

###################
## GLM Ergebniss ##
glm.fit_SM_PrecJulTavgJulSMISep  <- glm(formula = formula_sm_detrendlog_PrecJulTavgJulSMISep,  data = Yield_Covariates_SM_super)
summary(glm.fit_SM_PrecJulTavgJulSMISep)
'AIC: -7536.5'

## lm ##
lm.fit_SM_PrecJulTavgJulSMISep  <-  lm(formula = formula_sm_detrendlog_PrecJulTavgJulSMISep,  data = Yield_Covariates_SM_super)
summary(lm.fit_SM_PrecJulTavgJulSMISep)
'Adjusted R-squared:  0.7207'

## plm ##
plm.fit_SM_PrecJulTavgJulSMISep  <- plm(formula = update(formula_sm_detrendlog_PrecJulTavgJulSMISep, .~. - dummy(comId)),  data = Yield_Covariates_SM_super,  effect="individual", model=("within"), index = c("comId","year"))
summary(plm.fit_SM_PrecJulTavgJulSMISep)
'Adj. R-Squared: 0.26009'

fixef <- fixef(plm.fit_SM_PrecJulTavgJulSMISep)
fixef <- as.data.frame(as.matrix(fixef))
head(fixef)
fixef <- cbind(rownames(fixef), fixef)
rownames(fixef) <- NULL
names(fixef) <- c("comId", "FE")
write.csv(fixef, "./figures/figures_exploratory/FixedEffects/Silomaize/plm.fit_SM_PrecJulTavgJulSMISep.csv")

########################
## Heteroskedasdicity ##
bptest(glm.fit_SM_PrecJulTavgJulSMISep) # Breusch Pagan Test of Heteroskedastie in den Störgrößen: Null: Homoskedasdicity.
bptest(plm.fit_SM_PrecJulTavgJulSMISep)
' In beiden Fällen kann die Null widerlegt werden. Es gibt also heteroskedasdicity '

## Koenkers Version on BP Test: robuste Modification wenn die Störgrößen nicht normalverteilt sind. 
bptest(plm.fit_SM_PrecJulTavgJulSMISep, studentize = TRUE)
'Auch hier kann die Null widerlegt werden. Need to use robust covariance variance matrix to correct standard errors'

#########################
#### Autocorrelation ####

######################################
## Tests for serial autocorrelation ##
pwartest(plm.fit_SM_PrecJulTavgJulSMISep)
' Hier serielle Korrelation festzustellen'
pbgtest(plm.fit_SM_PrecJulTavgJulSMISep) 

'Solution for serial correlation: Cluster by groups.
Solution for cross sectional correlation: Cluster by time'

#################################
## Correct the Standard Errors ##
## Compare ordinary with Robust covariance matrix estimators a la White for panel models (heteroscedasticity-consistent (HC) standard errors)
coeftest(plm.fit_SM_PrecJulTavgJulSMISep)

## Robust covariance matrix estimators a la White 
# coeftest(plm.fit_SM_PrecJulTavgJulSMISep,vcov=vcovHC(plm.fit_SM_PrecJulTavgJulSMISep,method = "arellano", type = "HC0")) 
cov0_SM_PrecJulTavgJulSMISep <- vcovHC(plm.fit_SM_PrecJulTavgJulSMISep,method = "arellano", type = "HC0", cluster="group")
Wh.se_serial_SM_PrecJulTavgJulSMISep <- sqrt(diag(cov0_SM_PrecJulTavgJulSMISep))

cov0.1_SM_PrecJulTavgJulSMISep <- vcovHC(plm.fit_SM_PrecJulTavgJulSMISep,method = "arellano", type = "HC0", cluster="time")
Wh.se_cross_SM_PrecJulTavgJulSMISep <- sqrt(diag(cov0.1_SM_PrecJulTavgJulSMISep))
# 
# ## Beck Katz:
# # coeftest(plm.fit_SM_PrecJulTavgJulSMISep, vcov = function(x) vcovBK(plm.fit_SM_PrecJulTavgJulSMISep,method = "arellano", type = "HC0"))
# cov1 <- vcovBK(plm.fit_SM_PrecJulTavgJulSMISep,method = "arellano", type = "HC0",  cluster="time")
# BK.se <- sqrt(diag(cov1))

# Driscoll Kraay:
# summary(plm.fit_SM_PrecJulTavgJulSMISep)
coeftest(plm.fit_SM_PrecJulTavgJulSMISep,  vcov=function(x) vcovSCC(plm.fit_SM_PrecJulTavgJulSMISep,method = "arellano",type = "HC0")) 
cov2_SM_PrecJulTavgJulSMISep     <- vcovSCC(plm.fit_SM_PrecJulTavgJulSMISep,method = "arellano",type = "HC0")
DK.se_SM_PrecJulTavgJulSMISep    <- sqrt(diag(cov2_SM_PrecJulTavgJulSMISep))

# cov2.1   <- vcovSCC(plm.fit_SM_PrecJulTavgJulSMISep,method = "arellano",type = "HC0", maxlag=1)
# DK2.1.se <- sqrt(diag(cov2.1))

# cov2.2   <- vcovSCC(plm.fit_SM_PrecJulTavgJulSMISep,method = "arellano",type = "HC0", maxlag=2)
# DK2.2.se <- sqrt(diag(cov2.2))
# 
# cov2.3   <- vcovSCC(plm.fit_SM_PrecJulTavgJulSMISep,method = "arellano",type = "HC0", maxlag=3)
# DK2.3.se <- sqrt(diag(cov2.3))
# 
# cov2.4   <- vcovSCC(plm.fit_SM_PrecJulTavgJulSMISep,method = "arellano",type = "HC0", maxlag=4)
# DK2.4.se <- sqrt(diag(cov2.4))
# 
cov2.5_SM_PrecJulTavgJulSMISep   <- vcovSCC(plm.fit_SM_PrecJulTavgJulSMISep,method = "arellano",type = "HC0", maxlag=5)
DK2.5.se_SM_PrecJulTavgJulSMISep <- sqrt(diag(cov2.5_SM_PrecJulTavgJulSMISep))


## Cameron et al /Thompson : doouble-clustering estimator 
# coeftest(plm.fit_SM_PrecJulTavgJulSMISep, vcovDC(plm.fit_SM_PrecJulTavgJulSMISep, method = "arellano", type = "HC0"))
cov3_SM_PrecJulTavgJulSMISep <- vcovDC(plm.fit_SM_PrecJulTavgJulSMISep, method = "arellano", type = "HC0")
CT.se_SM_PrecJulTavgJulSMISep <- sqrt(diag(cov3_SM_PrecJulTavgJulSMISep))

## Generate Table with Output ##
se <- list(NULL, Wh.se_cross_SM_PrecJulTavgJulSMISep, Wh.se_serial_SM_PrecJulTavgJulSMISep,  DK.se_SM_PrecJulTavgJulSMISep, DK2.5.se_SM_PrecJulTavgJulSMISep, CT.se_SM_PrecJulTavgJulSMISep)
labels1 <-c("NULL","WhiteCross","WhiteSerial", "DriscollKraay", "DriscollKray2.5","CameronThompson")
stargazer(plm.fit_SM_PrecJulTavgJulSMISep, plm.fit_SM_PrecJulTavgJulSMISep, plm.fit_SM_PrecJulTavgJulSMISep, plm.fit_SM_PrecJulTavgJulSMISep, plm.fit_SM_PrecJulTavgJulSMISep,plm.fit_SM_PrecJulTavgJulSMISep,
          se = se,   
          dep.var.caption  = "Super Model with _SM_PrecJulTavgJulSMISep",
          dep.var.labels = "log(Silomaize)",
          style="default",
          model.numbers = FALSE,
          column.labels = labels1,          
          type="text", out="./figures/figures_exploratory/BIC/Silomaize/SM_Super_SM_PrecJulTavgJulSMISep.txt"
)

################################################################################################################################################################
################################
## Model PrecJulTavgJulSMIOct ##
plot(BIC_PrecJulTavgJulSMIOct)
which.min(BIC_PrecJulTavgJulSMIOct)
r=6

formula_sm_detrendlog_PrecJulTavgJulSMIOct

###################
## GLM Ergebniss ##
glm.fit_SM_PrecJulTavgJulSMIOct  <- glm(formula = formula_sm_detrendlog_PrecJulTavgJulSMIOct,  data = Yield_Covariates_SM_super)
summary(glm.fit_SM_PrecJulTavgJulSMIOct)
'AIC: -7501'

## lm ##
lm.fit_SM_PrecJulTavgJulSMIOct  <-  lm(formula = formula_sm_detrendlog_PrecJulTavgJulSMIOct,  data = Yield_Covariates_SM_super)
summary(lm.fit_SM_PrecJulTavgJulSMIOct)
'Adjusted R-squared:  0.7182 '

## plm ##
plm.fit_SM_PrecJulTavgJulSMIOct  <- plm(formula = update(formula_sm_detrendlog_PrecJulTavgJulSMIOct, .~. - dummy(comId)),  data = Yield_Covariates_SM_super,  effect="individual", model=("within"), index = c("comId","year"))
summary(plm.fit_SM_PrecJulTavgJulSMIOct)
'Adj. R-Squared: 0.25405'

fixef <- fixef(plm.fit_SM_PrecJulTavgJulSMIOct)
fixef <- as.data.frame(as.matrix(fixef))
head(fixef)
fixef <- cbind(rownames(fixef), fixef)
rownames(fixef) <- NULL
names(fixef) <- c("comId", "FE")
write.csv(fixef, "./figures/figures_exploratory/FixedEffects/Silomaize/plm.fit_SM_PrecJulTavgJulSMIOct.csv")

########################
## Heteroskedasdicity ##
bptest(glm.fit_SM_PrecJulTavgJulSMIOct) # Breusch Pagan Test of Heteroskedastie in den Störgrößen: Null: Homoskedasdicity.
bptest(plm.fit_SM_PrecJulTavgJulSMIOct)
' In beiden Fällen kann die Null widerlegt werden. Es gibt also heteroskedasdicity '

## Koenkers Version on BP Test: robuste Modification wenn die Störgrößen nicht normalverteilt sind. 
bptest(plm.fit_SM_PrecJulTavgJulSMIOct, studentize = TRUE)
'Auch hier kann die Null widerlegt werden. Need to use robust covariance variance matrix to correct standard errors'

#########################
#### Autocorrelation ####

######################################
## Tests for serial autocorrelation ##
pwartest(plm.fit_SM_PrecJulTavgJulSMIOct)
' Hier serielle Korrelation festzustellen'
pbgtest(plm.fit_SM_PrecJulTavgJulSMIOct) 

'Solution for serial correlation: Cluster by groups.
Solution for cross sectional correlation: Cluster by time'

#################################
## Correct the Standard Errors ##
## Compare ordinary with Robust covariance matrix estimators a la White for panel models (heteroscedasticity-consistent (HC) standard errors)
coeftest(plm.fit_SM_PrecJulTavgJulSMIOct)

## Robust covariance matrix estimators a la White 
# coeftest(plm.fit_SM_PrecJulTavgJulSMIOct,vcov=vcovHC(plm.fit_SM_PrecJulTavgJulSMIOct,method = "arellano", type = "HC0")) 
cov0_SM_PrecJulTavgJulSMIOct <- vcovHC(plm.fit_SM_PrecJulTavgJulSMIOct,method = "arellano", type = "HC0", cluster="group")
Wh.se_serial_SM_PrecJulTavgJulSMIOct <- sqrt(diag(cov0_SM_PrecJulTavgJulSMIOct))

cov0.1_SM_PrecJulTavgJulSMIOct <- vcovHC(plm.fit_SM_PrecJulTavgJulSMIOct,method = "arellano", type = "HC0", cluster="time")
Wh.se_cross_SM_PrecJulTavgJulSMIOct <- sqrt(diag(cov0.1_SM_PrecJulTavgJulSMIOct))
# 
# ## Beck Katz:
# # coeftest(plm.fit_SM_PrecJulTavgJulSMIOct, vcov = function(x) vcovBK(plm.fit_SM_PrecJulTavgJulSMIOct,method = "arellano", type = "HC0"))
# cov1 <- vcovBK(plm.fit_SM_PrecJulTavgJulSMIOct,method = "arellano", type = "HC0",  cluster="time")
# BK.se <- sqrt(diag(cov1))

# Driscoll Kraay:
# summary(plm.fit_SM_PrecJulTavgJulSMIOct)
coeftest(plm.fit_SM_PrecJulTavgJulSMIOct,  vcov=function(x) vcovSCC(plm.fit_SM_PrecJulTavgJulSMIOct,method = "arellano",type = "HC0")) 
cov2_SM_PrecJulTavgJulSMIOct     <- vcovSCC(plm.fit_SM_PrecJulTavgJulSMIOct,method = "arellano",type = "HC0")
DK.se_SM_PrecJulTavgJulSMIOct    <- sqrt(diag(cov2_SM_PrecJulTavgJulSMIOct))

# cov2.1   <- vcovSCC(plm.fit_SM_PrecJulTavgJulSMIOct,method = "arellano",type = "HC0", maxlag=1)
# DK2.1.se <- sqrt(diag(cov2.1))

# cov2.2   <- vcovSCC(plm.fit_SM_PrecJulTavgJulSMIOct,method = "arellano",type = "HC0", maxlag=2)
# DK2.2.se <- sqrt(diag(cov2.2))
# 
# cov2.3   <- vcovSCC(plm.fit_SM_PrecJulTavgJulSMIOct,method = "arellano",type = "HC0", maxlag=3)
# DK2.3.se <- sqrt(diag(cov2.3))
# 
# cov2.4   <- vcovSCC(plm.fit_SM_PrecJulTavgJulSMIOct,method = "arellano",type = "HC0", maxlag=4)
# DK2.4.se <- sqrt(diag(cov2.4))
# 
cov2.5_SM_PrecJulTavgJulSMIOct   <- vcovSCC(plm.fit_SM_PrecJulTavgJulSMIOct,method = "arellano",type = "HC0", maxlag=5)
DK2.5.se_SM_PrecJulTavgJulSMIOct <- sqrt(diag(cov2.5_SM_PrecJulTavgJulSMIOct))


## Cameron et al /Thompson : doouble-clustering estimator 
# coeftest(plm.fit_SM_PrecJulTavgJulSMIOct, vcovDC(plm.fit_SM_PrecJulTavgJulSMIOct, method = "arellano", type = "HC0"))
cov3_SM_PrecJulTavgJulSMIOct <- vcovDC(plm.fit_SM_PrecJulTavgJulSMIOct, method = "arellano", type = "HC0")
CT.se_SM_PrecJulTavgJulSMIOct <- sqrt(diag(cov3_SM_PrecJulTavgJulSMIOct))

## Generate Table with Output ##
se <- list(NULL, Wh.se_cross_SM_PrecJulTavgJulSMIOct, Wh.se_serial_SM_PrecJulTavgJulSMIOct,  DK.se_SM_PrecJulTavgJulSMIOct, DK2.5.se_SM_PrecJulTavgJulSMIOct, CT.se_SM_PrecJulTavgJulSMIOct)
labels1 <-c("NULL","WhiteCross","WhiteSerial", "DriscollKraay", "DriscollKray2.5","CameronThompson")
stargazer(plm.fit_SM_PrecJulTavgJulSMIOct, plm.fit_SM_PrecJulTavgJulSMIOct, plm.fit_SM_PrecJulTavgJulSMIOct, plm.fit_SM_PrecJulTavgJulSMIOct, plm.fit_SM_PrecJulTavgJulSMIOct,plm.fit_SM_PrecJulTavgJulSMIOct,
          se = se,   
          dep.var.caption  = "Super Model with _SM_PrecJulTavgJulSMIOct",
          dep.var.labels = "log(Silomaize)",
          style="default",
          model.numbers = FALSE,
          column.labels = labels1,          
          type="text", out="./figures/figures_exploratory/BIC/Silomaize/SM_Super_SM_PrecJulTavgJulSMIOct.txt"
)

################################################################################################################################################################
############
## August ##
############
################################
## Model PrecAugTavgAugSMISep ##
plot(BIC_PrecAugTavgAugSMISep)
which.min(BIC_PrecAugTavgAugSMISep) #9
r=8
'Wegen Singularität gehen ich auf 8'
formula_sm_detrendlog_PrecAugTavgAugSMISep

###################
## GLM Ergebniss ##
glm.fit_SM_PrecAugTavgAugSMISep  <- glm(formula = formula_sm_detrendlog_PrecAugTavgAugSMISep,  data = Yield_Covariates_SM_super)
summary(glm.fit_SM_PrecAugTavgAugSMISep)
'AIC: -7052.8'

## lm ##
lm.fit_SM_PrecAugTavgAugSMISep  <-  lm(formula = formula_sm_detrendlog_PrecAugTavgAugSMISep,  data = Yield_Covariates_SM_super)
summary(lm.fit_SM_PrecAugTavgAugSMISep)
"Adjusted R-squared:  0.6834"

## plm ##
plm.fit_SM_PrecAugTavgAugSMISep  <- plm(formula = update(formula_sm_detrendlog_PrecAugTavgAugSMISep, .~. - dummy(comId)),  data = Yield_Covariates_SM_super,  effect="individual", model=("within"), index = c("comId","year"))
summary(plm.fit_SM_PrecAugTavgAugSMISep)
'Adj. R-Squared: 0.17297'

fixef <- fixef(plm.fit_SM_PrecAugTavgAugSMISep)
fixef <- as.data.frame(as.matrix(fixef))
head(fixef)
fixef <- cbind(rownames(fixef), fixef)
rownames(fixef) <- NULL
names(fixef) <- c("comId", "FE")
write.csv(fixef, "./figures/figures_exploratory/FixedEffects/Silomaize/plm.fit_SM_PrecAugTavgAugSMISep.csv")

########################
## Heteroskedasdicity ##
bptest(glm.fit_SM_PrecAugTavgAugSMISep) # Breusch Pagan Test of Heteroskedastie in den Störgrößen: Null: Homoskedasdicity.
bptest(plm.fit_SM_PrecAugTavgAugSMISep)
' In beiden Fällen kann die Null widerlegt werden. Es gibt also heteroskedasdicity '

## Koenkers Version on BP Test: robuste Modification wenn die Störgrößen nicht normalverteilt sind. 
bptest(plm.fit_SM_PrecAugTavgAugSMISep, studentize = TRUE)
'Auch hier kann die Null widerlegt werden. Need to use robust covariance variance matrix to correct standard errors'

#########################
#### Autocorrelation ####

######################################
## Tests for serial autocorrelation ##
pwartest(plm.fit_SM_PrecAugTavgAugSMISep)
' Hier gibt es keine Serielle Korrelation festzustellen'
pbgtest(plm.fit_SM_PrecAugTavgAugSMISep) 


#################################
## Correct the Standard Errors ##
## Compare ordinary with Robust covariance matrix estimators a la White for panel models (heteroscedasticity-consistent (HC) standard errors)
coeftest(plm.fit_SM_PrecAugTavgAugSMISep)

## Robust covariance matrix estimators a la White 
# coeftest(plm.fit_SM_PrecAugTavgAugSMISep,vcov=vcovHC(plm.fit_SM_PrecAugTavgAugSMISep,method = "arellano", type = "HC0")) 
cov0_SM_PrecAugTavgAugSMISep <- vcovHC(plm.fit_SM_PrecAugTavgAugSMISep,method = "arellano", type = "HC0", cluster="group")
Wh.se_serial_SM_PrecAugTavgAugSMISep <- sqrt(diag(cov0_SM_PrecAugTavgAugSMISep))

cov0.1_SM_PrecAugTavgAugSMISep <- vcovHC(plm.fit_SM_PrecAugTavgAugSMISep,method = "arellano", type = "HC0", cluster="time")
Wh.se_cross_SM_PrecAugTavgAugSMISep <- sqrt(diag(cov0.1_SM_PrecAugTavgAugSMISep))
# 
# ## Beck Katz:
# # coeftest(plm.fit_SM_PrecAugTavgAugSMISep, vcov = function(x) vcovBK(plm.fit_SM_PrecAugTavgAugSMISep,method = "arellano", type = "HC0"))
# cov1 <- vcovBK(plm.fit_SM_PrecAugTavgAugSMISep,method = "arellano", type = "HC0",  cluster="time")
# BK.se <- sqrt(diag(cov1))

# Driscoll Kraay:
# summary(plm.fit_SM_PrecAugTavgAugSMISep)
coeftest(plm.fit_SM_PrecAugTavgAugSMISep,  vcov=function(x) vcovSCC(plm.fit_SM_PrecAugTavgAugSMISep,method = "arellano",type = "HC0")) 
cov2_SM_PrecAugTavgAugSMISep     <- vcovSCC(plm.fit_SM_PrecAugTavgAugSMISep,method = "arellano",type = "HC0")
DK.se_SM_PrecAugTavgAugSMISep    <- sqrt(diag(cov2_SM_PrecAugTavgAugSMISep))

# cov2.1   <- vcovSCC(plm.fit_SM_PrecAugTavgAugSMISep,method = "arellano",type = "HC0", maxlag=1)
# DK2.1.se <- sqrt(diag(cov2.1))

# cov2.2   <- vcovSCC(plm.fit_SM_PrecAugTavgAugSMISep,method = "arellano",type = "HC0", maxlag=2)
# DK2.2.se <- sqrt(diag(cov2.2))
# 
# cov2.3   <- vcovSCC(plm.fit_SM_PrecAugTavgAugSMISep,method = "arellano",type = "HC0", maxlag=3)
# DK2.3.se <- sqrt(diag(cov2.3))
# 
# cov2.4   <- vcovSCC(plm.fit_SM_PrecAugTavgAugSMISep,method = "arellano",type = "HC0", maxlag=4)
# DK2.4.se <- sqrt(diag(cov2.4))
# 
cov2.5_SM_PrecAugTavgAugSMISep   <- vcovSCC(plm.fit_SM_PrecAugTavgAugSMISep,method = "arellano",type = "HC0", maxlag=5)
DK2.5.se_SM_PrecAugTavgAugSMISep <- sqrt(diag(cov2.5_SM_PrecAugTavgAugSMISep))


## Cameron et al /Thompson : doouble-clustering estimator 
# coeftest(plm.fit_SM_PrecAugTavgAugSMISep, vcovDC(plm.fit_SM_PrecAugTavgAugSMISep, method = "arellano", type = "HC0"))
cov3_SM_PrecAugTavgAugSMISep <- vcovDC(plm.fit_SM_PrecAugTavgAugSMISep, method = "arellano", type = "HC0")
CT.se_SM_PrecAugTavgAugSMISep <- sqrt(diag(cov3_SM_PrecAugTavgAugSMISep))

## Generate Table with Output ##
se <- list(NULL, Wh.se_cross_SM_PrecAugTavgAugSMISep, Wh.se_serial_SM_PrecAugTavgAugSMISep,  DK.se_SM_PrecAugTavgAugSMISep, DK2.5.se_SM_PrecAugTavgAugSMISep, CT.se_SM_PrecAugTavgAugSMISep)
labels1 <-c("NULL","WhiteCross","WhiteSerial", "DriscollKraay", "DriscollKray2.5","CameronThompson")
stargazer(plm.fit_SM_PrecAugTavgAugSMISep, plm.fit_SM_PrecAugTavgAugSMISep, plm.fit_SM_PrecAugTavgAugSMISep, plm.fit_SM_PrecAugTavgAugSMISep, plm.fit_SM_PrecAugTavgAugSMISep,plm.fit_SM_PrecAugTavgAugSMISep,
          se = se,   
          dep.var.caption  = "Super Model with _SM_PrecAugTavgAugSMISep",
          dep.var.labels = "log(Silomaize)",
          style="default",
          model.numbers = FALSE,
          column.labels = labels1,          
          type="text", out="./figures/figures_exploratory/BIC/Silomaize/SM_Super_SM_PrecAugTavgAugSMISep.txt"
)

################################################################################################################################################################
################################
## Model PrecAugTavgAugSMIOct ##
plot(BIC_PrecAugTavgAugSMIOct)
which.min(BIC_PrecAugTavgAugSMIOct) #9
r=8
'Wegen des geringen Abstandes im BIC gehe ich auf 8'
formula_sm_detrendlog_PrecAugTavgAugSMIOct

###################
## GLM Ergebniss ##
glm.fit_SM_PrecAugTavgAugSMIOct  <- glm(formula = formula_sm_detrendlog_PrecAugTavgAugSMIOct,  data = Yield_Covariates_SM_super)
summary(glm.fit_SM_PrecAugTavgAugSMIOct)
'AIC: -7088.3'

## lm ##
lm.fit_SM_PrecAugTavgAugSMIOct  <-  lm(formula = formula_sm_detrendlog_PrecAugTavgAugSMIOct,  data = Yield_Covariates_SM_super)
summary(lm.fit_SM_PrecAugTavgAugSMIOct)
"Adjusted R-squared:  0.6863"

## plm ##
plm.fit_SM_PrecAugTavgAugSMIOct  <- plm(formula = update(formula_sm_detrendlog_PrecAugTavgAugSMIOct, .~. - dummy(comId)),  data = Yield_Covariates_SM_super,  effect="individual", model=("within"), index = c("comId","year"))
summary(plm.fit_SM_PrecAugTavgAugSMIOct)
'Adj. R-Squared: 0.17974'

fixef <- fixef(plm.fit_SM_PrecAugTavgAugSMIOct)
fixef <- as.data.frame(as.matrix(fixef))
head(fixef)
fixef <- cbind(rownames(fixef), fixef)
rownames(fixef) <- NULL
names(fixef) <- c("comId", "FE")
write.csv(fixef, "./figures/figures_exploratory/FixedEffects/Silomaize/plm.fit_SM_PrecAugTavgAugSMIOct.csv")

########################
## Heteroskedasdicity ##
bptest(glm.fit_SM_PrecAugTavgAugSMIOct) # Breusch Pagan Test of Heteroskedastie in den Störgrößen: Null: Homoskedasdicity.
bptest(plm.fit_SM_PrecAugTavgAugSMIOct)
' In beiden Fällen kann die Null widerlegt werden. Es gibt also heteroskedasdicity '

## Koenkers Version on BP Test: robuste Modification wenn die Störgrößen nicht normalverteilt sind. 
bptest(plm.fit_SM_PrecAugTavgAugSMIOct, studentize = TRUE)
'Auch hier kann die Null widerlegt werden. Need to use robust covariance variance matrix to correct standard errors'

#########################
#### Autocorrelation ####

######################################
## Tests for serial autocorrelation ##
pwartest(plm.fit_SM_PrecAugTavgAugSMIOct)
' Hier gibt es keine Serielle Korrelation festzustellen'
pbgtest(plm.fit_SM_PrecAugTavgAugSMIOct) 


#################################
## Correct the Standard Errors ##
## Compare ordinary with Robust covariance matrix estimators a la White for panel models (heteroscedasticity-consistent (HC) standard errors)
coeftest(plm.fit_SM_PrecAugTavgAugSMIOct)

## Robust covariance matrix estimators a la White 
# coeftest(plm.fit_SM_PrecAugTavgAugSMIOct,vcov=vcovHC(plm.fit_SM_PrecAugTavgAugSMIOct,method = "arellano", type = "HC0")) 
cov0_SM_PrecAugTavgAugSMIOct <- vcovHC(plm.fit_SM_PrecAugTavgAugSMIOct,method = "arellano", type = "HC0", cluster="group")
Wh.se_serial_SM_PrecAugTavgAugSMIOct <- sqrt(diag(cov0_SM_PrecAugTavgAugSMIOct))

cov0.1_SM_PrecAugTavgAugSMIOct <- vcovHC(plm.fit_SM_PrecAugTavgAugSMIOct,method = "arellano", type = "HC0", cluster="time")
Wh.se_cross_SM_PrecAugTavgAugSMIOct <- sqrt(diag(cov0.1_SM_PrecAugTavgAugSMIOct))
# 
# ## Beck Katz:
# # coeftest(plm.fit_SM_PrecAugTavgAugSMIOct, vcov = function(x) vcovBK(plm.fit_SM_PrecAugTavgAugSMIOct,method = "arellano", type = "HC0"))
# cov1 <- vcovBK(plm.fit_SM_PrecAugTavgAugSMIOct,method = "arellano", type = "HC0",  cluster="time")
# BK.se <- sqrt(diag(cov1))

# Driscoll Kraay:
# summary(plm.fit_SM_PrecAugTavgAugSMIOct)
coeftest(plm.fit_SM_PrecAugTavgAugSMIOct,  vcov=function(x) vcovSCC(plm.fit_SM_PrecAugTavgAugSMIOct,method = "arellano",type = "HC0")) 
cov2_SM_PrecAugTavgAugSMIOct     <- vcovSCC(plm.fit_SM_PrecAugTavgAugSMIOct,method = "arellano",type = "HC0")
DK.se_SM_PrecAugTavgAugSMIOct    <- sqrt(diag(cov2_SM_PrecAugTavgAugSMIOct))

# cov2.1   <- vcovSCC(plm.fit_SM_PrecAugTavgAugSMIOct,method = "arellano",type = "HC0", maxlag=1)
# DK2.1.se <- sqrt(diag(cov2.1))

# cov2.2   <- vcovSCC(plm.fit_SM_PrecAugTavgAugSMIOct,method = "arellano",type = "HC0", maxlag=2)
# DK2.2.se <- sqrt(diag(cov2.2))
# 
# cov2.3   <- vcovSCC(plm.fit_SM_PrecAugTavgAugSMIOct,method = "arellano",type = "HC0", maxlag=3)
# DK2.3.se <- sqrt(diag(cov2.3))
# 
# cov2.4   <- vcovSCC(plm.fit_SM_PrecAugTavgAugSMIOct,method = "arellano",type = "HC0", maxlag=4)
# DK2.4.se <- sqrt(diag(cov2.4))
# 
cov2.5_SM_PrecAugTavgAugSMIOct   <- vcovSCC(plm.fit_SM_PrecAugTavgAugSMIOct,method = "arellano",type = "HC0", maxlag=5)
DK2.5.se_SM_PrecAugTavgAugSMIOct <- sqrt(diag(cov2.5_SM_PrecAugTavgAugSMIOct))


## Cameron et al /Thompson : doouble-clustering estimator 
# coeftest(plm.fit_SM_PrecAugTavgAugSMIOct, vcovDC(plm.fit_SM_PrecAugTavgAugSMIOct, method = "arellano", type = "HC0"))
cov3_SM_PrecAugTavgAugSMIOct <- vcovDC(plm.fit_SM_PrecAugTavgAugSMIOct, method = "arellano", type = "HC0")
CT.se_SM_PrecAugTavgAugSMIOct <- sqrt(diag(cov3_SM_PrecAugTavgAugSMIOct))

## Generate Table with Output ##
se <- list(NULL, Wh.se_cross_SM_PrecAugTavgAugSMIOct, Wh.se_serial_SM_PrecAugTavgAugSMIOct,  DK.se_SM_PrecAugTavgAugSMIOct, DK2.5.se_SM_PrecAugTavgAugSMIOct, CT.se_SM_PrecAugTavgAugSMIOct)
labels1 <-c("NULL","WhiteCross","WhiteSerial", "DriscollKraay", "DriscollKray2.5","CameronThompson")
stargazer(plm.fit_SM_PrecAugTavgAugSMIOct, plm.fit_SM_PrecAugTavgAugSMIOct, plm.fit_SM_PrecAugTavgAugSMIOct, plm.fit_SM_PrecAugTavgAugSMIOct, plm.fit_SM_PrecAugTavgAugSMIOct,plm.fit_SM_PrecAugTavgAugSMIOct,
          se = se,   
          dep.var.caption  = "Super Model with _SM_PrecAugTavgAugSMIOct",
          dep.var.labels = "log(Silomaize)",
          style="default",
          model.numbers = FALSE,
          column.labels = labels1,          
          type="text", out="./figures/figures_exploratory/BIC/Silomaize/SM_Super_SM_PrecAugTavgAugSMIOct.txt"
)
