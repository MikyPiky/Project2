##### Supermodel of Silo Maize ####
'
In this script I combine the results from the seperate models to estimate a comprehensive model to improve the prediction accuracy 

'


###################
## Load Packages ##
library("plm")
library("boot")
library("gtools")
library("lme4")
library(lmtest)
library(car)

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
Yield_Covariates_SM_Super <- Yield_Covariates

############################################
#### Prepare data for stepwise function ####
' Drought Monitor Spezification '
Yield_Covariates_SM_Super$SMI_Aug_GDM <- cut(Yield_Covariates_SM_Super$SMI_Aug, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1),                                            ,
                                       labels = c("severe drought","moderate drought","abnormal dry", "normal","abnormal wet" ,"abundant wet", "severe wet"))

Yield_Covariates_SM_Super$SMI_Sep_GDM <- cut(Yield_Covariates_SM_Super$SMI_Sep, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1),                                            ,
                                             labels = c("severe drought","moderate drought","abnormal dry", "normal","abnormal wet" ,"abundant wet", "severe wet"))
#############
## Na-omit ##
sum(is.na(Yield_Covariates_SM_Super) )
dim(Yield_Covariates_SM_Super)
Yield_Covariates_SM_Super_nna <- na.omit(Yield_Covariates_SM_Super) 
dim(Yield_Covariates_SM_Super_nna)
any(is.na(Yield_Covariates_SM_Super_nna))
rownames(Yield_Covariates_SM_Super_nna) <- NULL
head(Yield_Covariates_SM_Super_nna)

## Further work with DataFrame without Yield_Covariates_SM_Super index ##
Yield_Covariates_SM_Super <- Yield_Covariates_SM_Super_nna

#####################################################
## Delete all comIds with less than 7 observations ##
sum(table(Yield_Covariates_SM_Super$comId) < 7 )
table(Yield_Covariates_SM_Super$comId) < 7 

## comIds mit weniger als 7 Beoachtungen: ##
list <- c(3101, 3102, 3158, 3402, 5114, 5117, 5314,5315, 5334, 5512, 5911, 5916, 6413, 7131, 7135, 7233, 7331, 7332, 7337, 7338, 7339, 8111,12052, 14612, 15001, 15082, 15083, 15084, 15085, 15086, 15087, 15088, 15089, 15091, 16051, 16052 )
length(list)
list[[35]]

temp <- Yield_Covariates_SM_Super
for (i in 1:36)
{
  print(Yield_Covariates_SM_Super[Yield_Covariates_SM_Super$comId==list[i],])
  temp <- (temp[!temp$comId==list[i],])
}

dim(temp)-dim(Yield_Covariates_SM_Super)

Yield_Covariates_SM_Super <- temp

################################
## Befehle nach jedem löschen ##
Yield_Covariates_SM_Super <- na.omit(Yield_Covariates_SM_Super) 
rownames(Yield_Covariates_SM_Super) <- NULL
Yield_Covariates_SM_Super <- plm.data(Yield_Covariates_SM_Super, index=c("comId", "year"))
names(Yield_Covariates_SM_Super_nna)

Yield_Covariates_SM_Super[,c("comId","comIdState")] <- lapply(Yield_Covariates_SM_Super[,c("comId","comIdState")], factor )
Yield_Covariates_SM_Super[,c("comId","comIdState")]
attach(Yield_Covariates_SM_Super)

#################################################
#### Remove log trend of indepedent variable ####
##################################################
'Fit log of yield on log of time and use the residuals of that for yields'
logtrend <- lm(log(siloMaize) ~ log(as.integer(year)), data= Yield_Covariates_SM_Super)

##########################
## Issue with Outliers ###
##########################
par(mfrow = c(2,2))
plot(logtrend) 


## Look Outliers Values ##
Yield_Covariates_SM_Super[c(1283,3290,822),]
## Look at other values of outliers com #
Yield_Covariates_SM_Super[Yield_Covariates_SM_Super$comId == "6532",][1:10] # 2008 hier scheint nur ein Jahr ein Messfehler zu sein: diesen Lösche ich 
Yield_Covariates_SM_Super[Yield_Covariates_SM_Super$comId == "5378",][1:10] # 2006 hier scheint nur ein Jahr ein Messfehler zu sein: diesen Lösche ich 


## Delete outliers ##
Yield_Covariates_SM_Super <- Yield_Covariates_SM_Super[!(Yield_Covariates_SM_Super$comId == "6532" & Yield_Covariates_SM_Super$year == "2008"),]

## Lösche den ganzen Landkreis,da sonst zu wenige Beobachtungen
Yield_Covariates_SM_Super <- Yield_Covariates_SM_Super[!(Yield_Covariates_SM_Super$comId == "5378") ,]



Yield_Covariates_SM_Super <- na.omit(Yield_Covariates_SM_Super)
rownames(Yield_Covariates_SM_Super) <- NULL

#################################################
#### Remove log trend of indepedent variable ####
##################################################
'Fit log of yield on log of time and use the residuals of that for yields'
logtrend <- lm(log(siloMaize) ~ log(as.integer(year)), data= Yield_Covariates_SM_Super)
Yield_Covariates_SM_Super$siloMaize_logtrend <- resid(logtrend)


#######################################
## Prepare dataframe for plm package ##
'Change Indexing so that it can be used in plm package'
Yield_Covariates_SM_Super <- plm.data(Yield_Covariates_SM_Super, index=c("comId", "year"))
str(Yield_Covariates_SM_Super)

#####################
## Attach Data Set ##
head(Yield_Covariates_SM_Super)
dim(Yield_Covariates_SM_Super)
attach(Yield_Covariates_SM_Super)


#### BIC to choose the degrees of the polynomials  ####
##########################################################################

## create a matrix which contains all possible degree combinations, here for three variables ##
degree <- permutations(n=3,r=2,v=c(1:3),repeats.allowed=T)
degree

## Change degrees of Precipitation and Temperature, leave PET linear
formula_Oct_sm_detrendlog_PetMaiPrecJulTavgAugSMIAug <- siloMaize_logtrend ~ Pet_Mai + poly(Prec_Jul, degree[r, 1], raw = T) +  poly(Tavg_Aug, degree[r, 2], raw = T) + 
  dummy(SMI_Aug_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)

formula_Oct_sm_detrendlog_PetMaiPrecJunTavgJulSMIAug <- siloMaize_logtrend ~ Pet_Mai + poly(Prec_Jun, degree[r, 1], raw = T) +  poly(Tavg_Jul, degree[r, 2], raw = T) + 
  dummy(SMI_Aug_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)

formula_Oct_sm_detrendlog_PetMaiPrecJunTavgJulSMISep <- siloMaize_logtrend ~ Pet_Mai + poly(Prec_Jun, degree[r, 1], raw = T) +  poly(Tavg_Jul, degree[r, 2], raw = T) + 
  dummy(SMI_Sep_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)

formula_Oct_sm_detrendlog_PetMaiPrecJulTavgJulSMIAug <- siloMaize_logtrend ~ Pet_Mai + poly(Prec_Jul, degree[r, 1], raw = T) +  poly(Tavg_Jul, degree[r, 2], raw = T) + 
  dummy(SMI_Aug_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)

formula_Oct_sm_detrendlog_PetJunPrecJulTavgJulSMIAug <- siloMaize_logtrend ~ Pet_Jun + poly(Prec_Jul, degree[r, 1], raw = T) +  poly(Tavg_Jul, degree[r, 2], raw = T) + 
  dummy(SMI_Aug_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)

formula_Oct_sm_detrendlog_PrecJulTavgJulSMIAug <- siloMaize_logtrend ~ poly(Prec_Jul, degree[r, 1], raw = T) +  poly(Tavg_Jul, degree[r, 2], raw = T) + 
  dummy(SMI_Aug_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)

formula_Oct_sm_detrendlog_PrecJulTavgJulSMISep <- siloMaize_logtrend ~ poly(Prec_Jul, degree[r, 1], raw = T) +  poly(Tavg_Jul, degree[r, 2], raw = T) + 
  dummy(SMI_Sep_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)

formula_Oct_sm_detrendlog_PrecJunTavgJunSMISep <- siloMaize_logtrend ~ poly(Prec_Jun, degree[r, 1], raw = T) +  poly(Tavg_Jun, degree[r, 2], raw = T) + 
  dummy(SMI_Aug_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)


' Pet Polynomials fügen keine zusätzlichen Informationen hinzu, daher lasse ich dies linear'


#################################################################################################
# Loop through the container list to cover all permutations of posssible degree of freedoms of ##
# of the polynomials of the variables                                                          ##
#################################################################################################

##################################################
## Loop through various variable configurations ##
# print("start loop")
# 
# BIC_PetMaiPrecJulTavgAugSMIAug <- rep(0,9)
# for(r in 1:9){
#   glm.fit_PetMaiPrecJulTavgAugSMIAug  <- glm(formula = formula_Oct_sm_detrendlog_PetMaiPrecJulTavgAugSMIAug, data = Yield_Covariates_SM_Super) 
#   BIC_PetMaiPrecJulTavgAugSMIAug[r] <- BIC(glm.fit_PetMaiPrecJulTavgAugSMIAug)
# }
# 
# 
# BIC_PetMaiPrecJunTavgJulSMIAug <- rep(0,9)
# for(r in 1:9){
#   glm.fit_PetMaiPrecJunTavgJulSMIAug  <- glm(formula = formula_Oct_sm_detrendlog_PetMaiPrecJunTavgJulSMIAug,  data = Yield_Covariates_SM_Super)
#   BIC_PetMaiPrecJunTavgJulSMIAug[r] <- BIC(glm.fit_PetMaiPrecJunTavgJulSMIAug)
# }
# 
# BIC_PetMaiPrecJunTavgJulSMISep <- rep(0,9)
# for(r in 1:9){
#   glm.fit_PetMaiPrecJunTavgJulSMISep  <- glm(formula = formula_Oct_sm_detrendlog_PetMaiPrecJunTavgJulSMISep,  data = Yield_Covariates_SM_Super)
#   BIC_PetMaiPrecJunTavgJulSMISep[r] <- BIC(glm.fit_PetMaiPrecJunTavgJulSMISep)
# }
# 
# BIC_PetMaiPrecJulTavgJulSMIAug <- rep(0,9)
# for(r in 1:9){
#   glm.fit_PetMaiPrecJulTavgJulSMIAug  <- glm(formula = formula_Oct_sm_detrendlog_PetMaiPrecJulTavgJulSMIAug,  data = Yield_Covariates_SM_Super)
#   BIC_PetMaiPrecJulTavgJulSMIAug[r] <- BIC(glm.fit_PetMaiPrecJulTavgJulSMIAug)
# }
# 
# BIC_PetJunPrecJulTavgJulSMIAug <- rep(0,9)
# for(r in 1:9){
#   glm.fit_PetJunPrecJulTavgJulSMIAug  <- glm(formula = formula_Oct_sm_detrendlog_PetJunPrecJulTavgJulSMIAug,  data = Yield_Covariates_SM_Super)
#   BIC_PetJunPrecJulTavgJulSMIAug[r] <- BIC(glm.fit_PetJunPrecJulTavgJulSMIAug)
# }


BIC_PrecJulTavgJulSMIAug <- rep(0,9)
for(r in 1:9){
  glm.fit_PrecJulTavgJulSMIAug  <- glm(formula = formula_Oct_sm_detrendlog_PrecJulTavgJulSMIAug,  data = Yield_Covariates_SM_Super)
  BIC_PrecJulTavgJulSMIAug[r] <- BIC(glm.fit_PrecJulTavgJulSMIAug)
}

BIC_PrecJulTavgJulSMISep <- rep(0,9)
for(r in 1:9){
  glm.fit_PrecJulTavgJulSMISep  <- glm(formula = formula_Oct_sm_detrendlog_PrecJulTavgJulSMISep,  data = Yield_Covariates_SM_Super)
  BIC_PrecJulTavgJulSMISep[r] <- BIC(glm.fit_PrecJulTavgJulSMISep)
}


BIC_PrecJunTavgJunSMISep <- rep(0,9)
for(r in 1:9){
  glm.fit_PrecJunTavgJunSMISep  <- glm(formula = formula_Oct_sm_detrendlog_PrecJunTavgJunSMISep,  data = Yield_Covariates_SM_Super)
  BIC_PrecJunTavgJunSMISep[r] <- BIC(glm.fit_PrecJunTavgJunSMISep)
}

#####################################################
## Compare plots of various formula specifications ##
#####################################################
BIC <- c(
#         BIC_PetMaiPrecJulTavgAugSMIAug ,BIC_PetMaiPrecJunTavgJulSMIAug, BIC_PetMaiPrecJunTavgJulSMISep, BIC_PetMaiPrecJulTavgJulSMIAug, BIC_PetJunPrecJulTavgJulSMIAug,
#          ,BIC_PrecJulTavgJulSMISep,BIC_PrecJunTavgJunSMISep
          BIC_PrecJulTavgJulSMIAug)
BIC
par(mfrow=c(1,1))
plot(BIC)

# ################################################################
# ## Model PetMaiPrecJulTavgJulSMIAug scheint das beste zu sein ##
# 
# plot(BIC_PetMaiPrecJulTavgJulSMIAug)
# r=5
# ## glm ##
# glm.fit_PetMaiPrecJulTavgJulSMIAug  <- glm(formula = formula_Oct_sm_detrendlog_PetMaiPrecJulTavgJulSMIAug,  data = Yield_Covariates_SM_Super)
# 
# ## lm ##
# lm.fit_PetMaiPrecJulTavgJulSMIAug  <- lm(formula = formula_Oct_sm_detrendlog_PetMaiPrecJulTavgJulSMIAug,  data = Yield_Covariates_SM_Super)
# summary(lm.fit_PetMaiPrecJulTavgJulSMIAug) 
# 'Adjusted R-squared:  0.738 '
# 
# ## plm ##
# plm.fit_PetMaiPrecJulTavgJulSMIAug  <- plm(formula = update(formula_Oct_sm_detrendlog_PetMaiPrecJulTavgJulSMIAug, .~. - dummy(comId)),  data = Yield_Covariates_SM_Super,  effect="individual", model=("within"), index = c("comId","year"))
# summary(plm.fit_SMIPrecTavg)
# 'Adj. R-Squared: 0.23733'

###################################################################
## Model PrecJulTavgJulSMIAug scheint das beste ohne PET zu sein ##
plot(BIC_PrecJulTavgJulSMIAug)
r=5

## lm ##
lm.fit_PrecJulTavgJulSMIAug  <- lm(formula = formula_Oct_sm_detrendlog_PrecJulTavgJulSMIAug,  data = Yield_Covariates_SM_Super)
summary(lm.fit_PrecJulTavgJulSMIAug )
'Adjusted R-squared:   0.73 '

## plm ##
plm.fit_PrecJulTavgJulSMIAug  <- plm(formula = update(formula_Oct_sm_detrendlog_PrecJulTavgJulSMIAug, .~. - dummy(comId)),  data = Yield_Covariates_SM_Super,  effect="individual", model=("within"), index = c("comId","year"))
summary(plm.fit_PrecJulTavgJulSMIAug )
'Adjusted R-squared:   0.28151'

# ######################################################################################
# ## Model PetMaiPrecJunTavgJulSMIAug scheint das beste mit seperaten Monaten zu sein ##
# plot(BIC_PetMaiPrecJunTavgJulSMIAug)
# r=5
# 
# ## lm ##
# lm.fit_PetMaiPrecJunTavgJulSMIAug  <- lm(formula = formula_Oct_sm_detrendlog_PetMaiPrecJunTavgJulSMIAug,  data = Yield_Covariates_SM_Super)
# summary(lm.fit_PetMaiPrecJunTavgJulSMIAug )
# 'Adjusted R-squared:   0.7273 '
# 
# ## plm ##
# plm.fit_PetMaiPrecJunTavgJulSMIAug  <- plm(formula = update(formula_Oct_sm_detrendlog_PetMaiPrecJunTavgJulSMIAug, .~. - dummy(comId)),  data = Yield_Covariates_SM_Super,  effect="individual", model=("within"), index = c("comId","year"))
# summary(plm.fit_PetMaiPrecJunTavgJulSMIAug )
# 'Adjusted R-squared:  0.27533 '


###########################
## Plot BIC with ggplot2 ##
###########################
library(ggplot2)

##############################################
## Create Dataframe for plotting in ggplot2 ##

## repeat name of modelconfiguration ##
list <-c(#"PetMaiPrecJulTavgAugSMIAug","PetMaiPrecJunTavgJulSMIAug", "PetMaiPrecJunTavgJulSMISep", "PetMaiPrecJulTavgJulSMIAug", "PetJunPrecJulTavgJulSMIAug","PrecJulTavgJulSMISep","PrecJunTavgJunSMISep"
          "PrecJulTavgJulSMIAug")
list2 <- 1

model <- NULL
model_index <- NULL

for (i in 1)
{
  x <- rep(list[i],9)
  y <- rep(list2[i],9)
  model <- append(model, x)
  model_index <- as.numeric(append(model_index, y))
}

model <- as.data.frame(model)
model_index <- as.data.frame(model_index)

####################################
## Combine data in one data.frame ##
class(BIC)
BIC <- as.data.frame(BIC)
BIC2 <-cbind(BIC, model)
BIC2 <- cbind(BIC2,model_index)
index <- 1:9
BIC2 <- cbind(BIC2, index)
month <-rep("Super",9)
BIC2 <- cbind(BIC2, month)
BIC2


############################
## Plot data with ggplot2 ##
g <- ggplot(BIC2,aes(y=BIC, x=index))
g + geom_point(aes(color=model)) + labs(title="BIC of various model configurations", x="") + theme(plot.title=element_text(size=15, face="bold")) + theme_dark()
g + geom_point(aes(color=model)) + labs(title="BIC of various model configurations", x="") + theme(plot.title=element_text(size=15, face="bold")) + theme_dark() +
  facet_wrap( ~ month)
BIC2


BIC_SM_Super <- BIC2
class(BIC_SM_Super)
write.csv(BIC_SM_Super, file="./data/data_raw/BIC/BIC_SM_Super.csv")

