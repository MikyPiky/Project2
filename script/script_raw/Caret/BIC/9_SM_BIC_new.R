############################
####  SiloMaize in Sep ####
############################
'
## Variables to change when transferring to other months
- Sep


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
- Model with lowest BIC in general: PET, Prec
- Model with lowest BIC of standard configuration: Tavg, Prec, SMI
- Model with lowest BIC with SMI: PET, Prec, SMI
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

- aus 4km_tmax: Yield_SMI_Prec_Tavg_PET_Dem_Por_Tmin_Tmax_nodemean_nozscore_sm.csv (komplete data.frame)


## Output ##
- Yield_Covariates_SM_Sep.csv (auf Sep reduzierter Data.Frame)
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
Yield_Covariates <- read.csv("./data/data_processed/yieldData_meteo")
Yield_Covariates$X <- NULL
str(Yield_Covariates)

## For publication worth regression output need to change data names ##
'Get rid of variables which are not necessary: other months and other not needed variables'
names(Yield_Covariates)
names <- names(Yield_Covariates)
names_Sep <- grep(c("*_Sep"), names)
names_Sep
Yield_Covariates_Sep <- Yield_Covariates[,names_Sep]
names(Yield_Covariates_Sep)

names_Sep_lag <-    grep(c("*_lag"), names(Yield_Covariates_Sep ))
names_Sep_lag-1

Yield_Covariates_Sep <- Yield_Covariates_Sep[,names_Sep_lag-1]
names(Yield_Covariates_Sep )

dim(Yield_Covariates_Sep)

## Delete all but SMI, Prec, Tavg and PET
names(Yield_Covariates_Sep)
Yield_Covariates_Sep <- Yield_Covariates_Sep[,c(1:4)]
names(Yield_Covariates_Sep)
## Establish first part of data frame_ time and spatial reference plus Silomaize ##
names(Yield_Covariates[,c(1:5,15)])
Yield_Covariates_SM <- Yield_Covariates[,c(1:5,15)] # Achtung, darauf achten, dass comId und year in der richtigen Reihenfolge sind.
names(Yield_Covariates_SM)
head(Yield_Covariates_SM)

Yield_Covariates_SM_Sep <- cbind(Yield_Covariates_SM, Yield_Covariates_Sep)
names(Yield_Covariates_SM_Sep)
names(Yield_Covariates_SM_Sep) <- c( "comId" , "year","com","stateId","state","siloMaize","PET", "Prec","Tavg", "SMI")
names(Yield_Covariates_SM_Sep)

#########################################
#### Create stepwise function of SMI ####
#########################################
' Drought Monitor Spezification - FULL'

# Yield_Covariates_SM_Sep$SMI_GDM <- cut(Yield_Covariates_SM_Sep$SMI, breaks = c(0, 0.1, 0.2, 0.3,0.4,0.49,0.51,0.6, 0.7, 0.8, 0.9, 1),                                            ,
#                                                  labels = c("severe drought","moderate drought","abnormally dry",
#                                                             "dry", 
#                                                             "normally dry", 
#                                                             "normal",
#                                                             "normally wet",
#                                                             "wet",  
#                                                             "abnormally wet" ,"abundantly wet", "severely wet"))
Yield_Covariates_SM_Sep$SMI_GDM <- cut(Yield_Covariates_SM_Sep$SMI, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1),                                            ,
                                       labels = c("severe drought","moderate drought","abnormally dry", "normal",
                                                  "abnormally wet" ,"abundantly wet", "severely wet"))

head(Yield_Covariates_SM_Sep$SMI_GDM )
table(Yield_Covariates_SM_Sep$SMI_GDM )
table <- table(Yield_Covariates_SM_Sep$SMI_GDM,Yield_Covariates_SM_Sep$year  )
write.csv(table, "./figures/figures_exploratory/SMI/Sep_SMI_year" )
################
## Delete NAs ##
################
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



######################
## Scale Covariates ##
######################
Yield_Covariates_SM_Sep_unscaled <- Yield_Covariates_SM_Sep

summary(Yield_Covariates_SM_Sep_unscaled$Prec)
summary(Yield_Covariates_SM_Sep_unscaled$Tavg)
summary(Yield_Covariates_SM_Sep_unscaled$PET)

mean(Yield_Covariates_SM_Sep_unscaled$Prec)
mean(Yield_Covariates_SM_Sep_unscaled$Tavg)
mean(Yield_Covariates_SM_Sep_unscaled$PET)


sd(Yield_Covariates_SM_Sep_unscaled$Prec)
sd(Yield_Covariates_SM_Sep_unscaled$Tavg)
sd(Yield_Covariates_SM_Sep_unscaled$PET)

head(Yield_Covariates_SM_Sep)[,7:length(Yield_Covariates_SM_Sep)]
head(Yield_Covariates_SM_Sep)[,7: (length(Yield_Covariates_SM_Sep)-2) ]
Yield_Covariates_SM_Sep_norm <- Yield_Covariates_SM_Sep[,7: (length(Yield_Covariates_SM_Sep)-2) ]
head(Yield_Covariates_SM_Sep_norm)
scale <- scale(Yield_Covariates_SM_Sep_norm)
summary(Yield_Covariates_SM_Sep_norm)
summary(scale)

Yield_Covariates_SM_Sep[,7: (length(Yield_Covariates_SM_Sep)-2) ] <- scale
head(Yield_Covariates_SM_Sep)
sd(Yield_Covariates_SM_Sep_norm$Prec)
sd(Yield_Covariates_SM_Sep_norm$Tavg)
sd(Yield_Covariates_SM_Sep_norm$PET)


#########################################################################
## Remove comIds with less than 7 observations to avoid leveage issues ## 
#########################################################################

#####################################################
## Delete all comIds with less than 7 observations ##
sum(table(Yield_Covariates_SM_Sep$comId) < 9 )
table(Yield_Covariates_SM_Sep$comId) < 9 

## comIds mit weniger als 9 Beoachtungen: ##
list <- c(3101, 3102, 3402, 5111 , 5112 , 5113 , 5114, 5117, 5124, 5314,
          5315, 5334,5378,  5512, 5911,   5916,  7131,  7133, 7135, 7233, 
          7331, 7332,7334, 7335, 7337,    7338, 7339,   8111,12052, 14612, 16052 )
length(list)
list[[1]]


temp <- Yield_Covariates_SM_Sep
for (i in 1:length(list))
{
  print(Yield_Covariates_SM_Sep[Yield_Covariates_SM_Sep$comId==list[i],])
  temp <- (temp[!temp$comId==list[i],])
}

## Number of deleted rows ##
dim(temp) - dim(Yield_Covariates_SM_Sep)
head(temp)

## Further use old name for data.frame
Yield_Covariates_SM_Sep <- temp

################################
## Befehle nach jedem löschen ##
Yield_Covariates_SM_Sep <- na.omit(Yield_Covariates_SM_Sep) 
rownames(Yield_Covariates_SM_Sep) <- NULL
head(Yield_Covariates_SM_Sep)
Yield_Covariates_SM_Sep <- plm.data(Yield_Covariates_SM_Sep, index=c("comId", "year"))
Yield_Covariates_SM_Sep[,c("comId","stateId")] <- lapply(Yield_Covariates_SM_Sep[,c("comId","stateId")], factor )

#################################################
#### Remove log trend of indepedent variable ####
#################################################
'Fit log of yield on log of time and use the residuals of that for yields'
logtrend <- lm(log(siloMaize) ~ log(as.integer(year)), data= Yield_Covariates_SM_Sep)
summary(logtrend)
##########################
## Issue with Outliers ###
##########################
par(mfrow = c(2,2))
plot(logtrend) 

## Look at Outliers Values ##
Yield_Covariates_SM_Sep[c(1785, 4485, 4516),]

## Look at other values of outliers com #
Yield_Covariates_SM_Sep[Yield_Covariates_SM_Sep$comId == "6532",] # 2008 hier scheint nur ein Jahr ein Messfehler zu sein: diesen Lösche ich 
Yield_Covariates_SM_Sep[Yield_Covariates_SM_Sep$comId == "12067",] # 2006 verändere ich nicht 
Yield_Covariates_SM_Sep[Yield_Covariates_SM_Sep$comId == "12069",] # 2003 verändere ich nicht 



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

###########################################
## Transform comId and stateId to factor ##
Yield_Covariates_SM_Sep[,c("comId","stateId")] <- lapply(Yield_Covariates_SM_Sep[,c("comId","stateId")], factor )
lapply(Yield_Covariates_SM_Sep, class)

################################
#### Make Correlation Plots ####
library(corrplot)
par(mfrow = c(1,1))

## Correlation all exogenous Variables ##
M <- cor(Yield_Covariates_SM_Sep[,7:10])
corrplot.mixed(M)

pdf ("/Storage/ownCloud/Home/Klimabuero/Proj1/figures/figures_exploratory/Corrplots/Sep.pdf")
M_cor <- corrplot.mixed(M)
dev.off()

## Correlation all PET & Tavg ##
M_small <- cor(Yield_Covariates_SM_Sep[,c("PET","Tavg")])
corrplot.mixed(M_small)

pdf ("/Storage/ownCloud/Home/Klimabuero/Proj1/figures/figures_exploratory/Corrplots/Sep_TavgPET.pdf")
corrplot.mixed(M_small, lower="ellipse", upper="number")
dev.off()


###################################################
##### Save Yield_Covariates_SM_Sep extern ####
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
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormally dry",
                  "abnormally wet" ,"abundantly wet", "severely wet")) + dummy(comId)

formula_Sep_sm_detrendlog_SMIPrecPET <- siloMaize_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  poly(PET, degree[r, 2], raw = T) + 
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormally dry", "abnormally wet" ,"abundantly wet", "severely wet")) + dummy(comId)

formula_Sep_sm_detrendlog_SMIPrec <- siloMaize_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormally dry", "abnormally wet" ,"abundantly wet", "severely wet")) + dummy(comId)

formula_Sep_sm_detrendlog_SMIPET <- siloMaize_logtrend ~ poly(PET, degree[r, 2], raw = T) +  
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormally dry", "abnormally wet" ,"abundantly wet", "severely wet")) + dummy(comId)

formula_Sep_sm_detrendlog_SMITavg <- siloMaize_logtrend ~ poly(Tavg, degree[r, 2], raw = T) + 
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormally dry", "abnormally wet" ,"abundantly wet", "severely wet")) + dummy(comId)

formula_Sep_sm_detrendlog_SMI <- siloMaize_logtrend ~  
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormally dry","abnormally wet" ,"abundantly wet", "severely wet")) + dummy(comId)

## no SMI
formula_Sep_sm_detrendlog_PrecTavg <- siloMaize_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  poly(Tavg, degree[r, 2], raw = T) + dummy(comId)

formula_Sep_sm_detrendlog_PrecPET <- siloMaize_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  poly(PET, degree[r, 2], raw = T) + dummy(comId)

formula_Sep_sm_detrendlog_Prec <- siloMaize_logtrend ~ poly(Prec, degree[r, 1], raw = T) + dummy(comId)

formula_Sep_sm_detrendlog_PET <- siloMaize_logtrend ~ poly(PET, degree[r, 2], raw = T)  + dummy(comId)

formula_Sep_sm_detrendlog_Tavg <- siloMaize_logtrend ~ poly(Tavg, degree[r, 2], raw = T)  + dummy(comId)

## Generate list of formula
formula_list_Sep_sm_detrendlog <- c(
  formula_Sep_sm_detrendlog_SMIPrecTavg,
  formula_Sep_sm_detrendlog_SMIPrecPET,
  formula_Sep_sm_detrendlog_SMIPrec,
  formula_Sep_sm_detrendlog_SMIPET,
  formula_Sep_sm_detrendlog_SMITavg,
  formula_Sep_sm_detrendlog_SMI,
  formula_Sep_sm_detrendlog_Prec,
  formula_Sep_sm_detrendlog_Tavg,
  formula_Sep_sm_detrendlog_PET ,
  formula_Sep_sm_detrendlog_PrecTavg,
  formula_Sep_sm_detrendlog_PrecPET
  
  
)
formula_list_Sep_sm_detrendlog[[11]]
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

BIC_SMIPrecPET <- rep(0,9)
for(r in 1:9){
  glm.fit_SMIPrecPET  <- glm(formula = formula_Sep_sm_detrendlog_SMIPrecPET,  data = Yield_Covariates_SM_Sep)
  BIC_SMIPrecPET[r] <- BIC(glm.fit_SMIPrecPET)
}

BIC_SMIPrec <- rep(0,9)
for(r in 1:9){  
  glm.fit_SMIPrec      <- glm(formula = formula_Sep_sm_detrendlog_SMIPrec,     data = Yield_Covariates_SM_Sep) 
  BIC_SMIPrec[r] <- BIC(glm.fit_SMIPrec)
}

BIC_SMIPET <- rep(0,9)
for(r in 1:9){  
  glm.fit_SMIPET       <- glm(formula = formula_Sep_sm_detrendlog_SMIPET,      data = Yield_Covariates_SM_Sep) 
  BIC_SMIPET[r] <- BIC(glm.fit_SMIPET)
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

BIC_PrecPET <- rep(0,9)
for(r in 1:9){
  glm.fit_PrecPET  <- glm(formula = formula_Sep_sm_detrendlog_PrecPET, data = Yield_Covariates_SM_Sep) 
  BIC_PrecPET[r] <- BIC(glm.fit_PrecPET)
}

BIC_Prec <- rep(0,9)
for(r in 1:9){
  glm.fit_Prec  <- glm(formula = formula_Sep_sm_detrendlog_Prec, data = Yield_Covariates_SM_Sep) 
  BIC_Prec[r] <- BIC(glm.fit_Prec)
}

BIC_PET  <- rep(0,9)
for(r in 1:9){
  glm.fit_PET   <- glm(formula = formula_Sep_sm_detrendlog_PET , data = Yield_Covariates_SM_Sep) 
  BIC_PET [r] <- BIC(glm.fit_PET )
}

BIC_Tavg  <- rep(0,9)
for(r in 1:9){
  glm.fit_Tavg   <- glm(formula = formula_Sep_sm_detrendlog_Tavg , data = Yield_Covariates_SM_Sep) 
  BIC_Tavg [r] <- BIC(glm.fit_Tavg )
}

## Compare BIC values ##
BIC <- c(BIC_SMIPrecTavg, BIC_SMIPrecPET, BIC_SMIPrec, BIC_SMIPET, BIC_SMITavg, BIC_SMI, BIC_Prec, BIC_Tavg, BIC_PET, BIC_PrecTavg, BIC_PrecPET)
BIC
par(mfrow=c(1,1))
plot(BIC)

###########################
## Plot BIC with ggplot2 ##
###########################


##############################################
## Create Dataframe for plotting in ggplot2 ##

## repeat name of modelconfiguration ##
list <-c("01_SMIPrecTavg", "02_SMIPrecPET", "03_SMIPrec", "04_SMIPET",
         "05_SMITavg", "06_SMI", "07_Prec", "08_Tavg", "09_PET", "10_PrecTavg", "11_PrecPET")
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
month <-rep("Sep",99)

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
BIC_Sep$index <-  c(1:55)

############################
## Plot data with ggplot2 ##
g <- ggplot(BIC_Sep,aes(y=BIC, x=index))
g_save <- g + geom_point(aes(color=model)) + labs(title="BIC_SM_Sep", x="") + 
  theme(plot.title=element_text(size=15, face="bold")) + theme_few() +
  facet_wrap( ~ month)
g_save 
ggsave( "./figures/figures_exploratory/BIC/Silomaize/Sep/BIC_SM_Sep.png", width= 8, height=8, dpi=400)


## Export Data frame for use in BIC_Grafic
BIC_SM_Sep <- BIC_Sep
class(BIC_SM_Sep)
write.csv(BIC_SM_Sep, file="./data/data_raw/BIC/BIC_SM_Sep.csv")
which.min(BIC_SM_Sep$BIC)

BIC_SM_Sep[which.min(BIC_SM_Sep$BIC),]



###################################################################################################################################################################
################################### Explore Models ################################################################################################################
###################################################################################################################################################################

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

BIC_SM_Sep[which.min(BIC_SM_Sep$BIC),]
BIC_SM_Sep[order(BIC_SM_Sep$BIC)[1],]

## Define Difference between first and second best BIC Value ##
-(BIC_SM_Sep$BIC[order(BIC_SM_Sep$BIC)[1]]-BIC_SM_Sep$BIC[order(BIC_SM_Sep$BIC)[2]])

## Make condition for if ... else loop
-(BIC_SM_Sep$BIC[order(BIC_SM_Sep$BIC)[1]]-BIC_SM_Sep$BIC[order(BIC_SM_Sep$BIC)[2]]) < 6

BIC_SM_Sep[order(BIC_SM_Sep$BIC),]
BIC_order_Sep <- BIC_SM_Sep[order(BIC_SM_Sep$BIC),]
config <- rep("Best", 55)
BIC_order_Sep_Best <- cbind(BIC_order_Sep, config)

capture.output(BIC_order_Sep_Best, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/BIC_order_Sep")



############################################################################################
## If difference is larger than six between first and second best model, take first model ##
## However, it can not really be assumed that there is no difference though
x <- -(BIC_SM_Sep$BIC[order(BIC_SM_Sep$BIC)[1]]-BIC_SM_Sep$BIC[order(BIC_SM_Sep$BIC)[2]])
# 
# if ( x > 6)
# {
  model_best <- print("Best Formula - Best BIC")
  model_best
  #  Extract model index
  BIC_SM_Sep[order(BIC_SM_Sep$BIC)[1],][[3]]
  
  ## Extract model with lowest BIC of the models with that particular model index 
  order(BIC_SM_Sep[BIC_SM_Sep$model_index==BIC_SM_Sep[order(BIC_SM_Sep$BIC)[1],][[3]],][,1])[1]
  
  ## Determine degree 
  r = order(BIC_SM_Sep[BIC_SM_Sep$model_index==BIC_SM_Sep[order(BIC_SM_Sep$BIC)[1],][[3]],][,1])[1]
  
  ## Determine adequate formula for model by the means of the model index
  best_formula <-  formula_list_Sep_sm_detrendlog[[BIC_SM_Sep[order(BIC_SM_Sep$BIC)[1],][[3]]]]
  best_formula
  
  
# } else {
#   
#   #############################################################
#   ## If difference is smaller than 6, take second best model ##
#   model_best <- print("Best Formula - Second Best BIC")
#   model_best 
#   ## Look at second best model ##
#   BIC_SM_Sep[order(BIC_SM_Sep$BIC)[2],]
#   
#   ##  Extract model index
#   BIC_SM_Sep[order(BIC_SM_Sep$BIC)[2],][[3]]
#   
#   ## Extract model with second lowest BIC of the models with that particular model index 
#   order(BIC_SM_Sep[BIC_SM_Sep$model_index==BIC_SM_Sep[order(BIC_SM_Sep$BIC)[1],][[3]],][,1])[2]
#   
#   ## Determine degree 
#   r = order(BIC_SM_Sep[BIC_SM_Sep$model_index==BIC_SM_Sep[order(BIC_SM_Sep$BIC)[1],][[3]],][,1])[2]
#   
#   ## Determine adequate formula for model by the means of the model index
#   best_formula <-  formula_list_Sep_sm_detrendlog[[BIC_SM_Sep[order(BIC_SM_Sep$BIC)[2],][[3]]]]
#   best_formula
#   
# }
capture.output(model_best, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/BIC_order_Sep", append=T)
capture.output(best_formula, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/BIC_order_Sep", append=T)

###################
## GLM Ergebniss ##
glm.fit_SM_BEST_Sep  <- glm(formula = best_formula,  data = Yield_Covariates_SM_Sep)

## Summary
best_glm_summary <- summary(glm.fit_SM_BEST_Sep)
best_glm_summary
capture.output(best_glm_summary, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/Output_SM_Sep_qual")

## AIC
best_glm_summary_qual <- print(c("best_glm_AIC:", round(best_glm_summary$aic,4)))

capture.output(print("BEST MODELS"), file = "./figures/figures_exploratory/BIC/Silomaize/Sep/Output_SM_Sep_qual.txt")
capture.output(best_glm_summary_qual, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/Output_SM_Sep_qual.txt", append=T)

'AIC: -6494.3'

####################
## PLM Ergebnisse ##
plm.fit_SM_BEST_Sep <- plm(formula = update(best_formula, .~. - dummy(comId)),  data = Yield_Covariates_SM_Sep,  effect="individual", model=("within"), index = c("comId","year"))

## Output
best_plm_summary <- summary(plm.fit_SM_BEST_Sep)
best_plm_summary
capture.output(best_plm_summary, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/Output_SM_Sep_qual", append=T)

best_plm_summary_qual <- print(c("best_plm:", round(best_plm_summary$r.squared,4)))
capture.output(best_plm_summary_qual, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/Output_SM_Sep_qual.txt", append=T)

'Adj. R-Squared: 0.054164'

## Fixed Effects ##
fixef <-  fixef(plm.fit_SM_BEST_Sep)
fixef_summary <- as.matrix(summary(fixef))
fixef_summary[33,3]
capture.output(fixef_summary, file = "./figures/figures_exploratory/FixedEffects/Silomaize/Sep/plm.fit_SM_BEST_Sep_FE_summary.txt")
# fixef_summary <- read.table( file = "./figures/figures_exploratory/FixedEffects/Silomaize/Sep/plm.fit_SM_BEST_Sep_FE_summary.txt")


##################
## LM Ergebniss ##
lm.fit_SM_BEST_Sep  <-lm(formula = best_formula,  data = Yield_Covariates_SM_Sep)
best_lm_summary <- summary(lm.fit_SM_BEST_Sep) 
best_lm_summary$r.squared
capture.output(best_lm_summary, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/Output_SM_Sep_qual", append=T)


best_lm_summary_qual1 <- print(c("best_lm_r.squared:", round(best_lm_summary$r.squared,4)))
best_lm_summary_qual2 <- print(c("best_lm_adj.r.squared:", round(best_lm_summary$adj.r.squared,4)))
capture.output(best_lm_summary_qual1, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/Output_SM_Sep_qual.txt", append=T)
capture.output(best_lm_summary_qual2, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/Output_SM_Sep_qual.txt", append=T)
## Output
'Adjusted R-squared:   0.6345  '

capture.output(best_lm_summary, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/Output_SM_Sep_qual", append=T)

################################################
## Assessing Influence (Leverage*discrepancy) ##
################################################

##################################
## Take a look at the residuals ##

## Histogram of the Residuals ##
hist(lm.fit_SM_BEST_Sep$residuals, freq=FALSE)

# Make a data.frame of the residuals
Yield_Covariates_SM_Sep_Resid <- as.data.frame(as.numeric(lm.fit_SM_BEST_Sep$residuals))
names(Yield_Covariates_SM_Sep_Resid) <- "Residual"

## Make a dataframe with referential variables
dim(Yield_Covariates_SM_Sep)
head(Yield_Covariates_SM_Sep[,1:3])
dim(Yield_Covariates_SM_Sep_Resid)
Yield_Covariates_SM_Sep_Resid <- cbind(Yield_Covariates_SM_Sep[,1:3], Yield_Covariates_SM_Sep_Resid)

##W Write data.frame of the Residuals
write.csv(Yield_Covariates_SM_Sep_Resid, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/Yield_Covariates_SM_Sep_Resid.csv")

## Make Residuals of years 2003 only ##
Yield_Covariates_SM_Sep_Resid2003 <- as.data.frame(Yield_Covariates_SM_Sep_Resid[Yield_Covariates_SM_Sep_Resid$year==2003,])
names(Yield_Covariates_SM_Sep_Resid2003) <- c("comId","year","com","Residual_only2003")
rownames(Yield_Covariates_SM_Sep_Resid2003) <- NULL
head(Yield_Covariates_SM_Sep_Resid2003)
tail(Yield_Covariates_SM_Sep_Resid2003)

# Histogramm of only 2003 residuals
hist(Yield_Covariates_SM_Sep_Resid2003$Residual_only2003, freq=FALSE, main="Distribution of Residuals of 2003")

## Write Residuals of only year 2003
write.csv(Yield_Covariates_SM_Sep_Resid2003,  file = "./figures/figures_exploratory/BIC/Silomaize/Sep/Yield_Covariates_SM_Sep_Resid_only2003.csv")
## Influence Plot ##
# influencePlot(lm.fit_SM_BEST_Sep,  id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

####################
## Cooks Distance ##

## Plot Cooks Distance
plot(lm.fit_SM_BEST_Sep, which=4)

## Read out Cooks Distance ##
cook_Sep <- as.data.frame(cooks.distance(lm.fit_SM_BEST_Sep))
colnames(cook_Sep) <- "CooksDistance"

## Reference Cooks Distance Values
dim(Yield_Covariates_SM_Sep[,1:3])
dim(cook_Sep)
cook_Sep_ref <- cbind(Yield_Covariates_SM_Sep[,1:3],cook_Sep)

## Write out Cooks Distance
write.csv(cook_Sep_ref, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/best_SM_Sep_cooksDistance.csv")

## Define Cutoffs ##
cutoff_SM_Sep_small <- 4/((nrow(Yield_Covariates_SM_Sep)-length(lm.fit_SM_BEST_Sep$coefficients)-1)) 
cutoff_SM_Sep_small
cutoff_SM_Sep_large <- 1

## Number of Cooks Distance Values below the cutoffs ##
nrow_cooks_small <- nrow(Yield_Covariates_SM_Sep[cook_Sep > cutoff_SM_Sep_small,]) 
nrow_cooks_large <- nrow(Yield_Covariates_SM_Sep[cook_Sep > cutoff_SM_Sep_large,]) 

## Make a output
capture.output(c("Cutoff_small",cutoff_SM_Sep_small), file = "./figures/figures_exploratory/BIC/Silomaize/Sep/best_SM_Sep_cooks.txt")
capture.output(c("Cutoff_small_number",nrow_cooks_small), file = "./figures/figures_exploratory/BIC/Silomaize/Sep/best_SM_Sep_cooks.txt", append=T)
capture.output(c("Cutoff_large",cutoff_SM_Sep_large), file = "./figures/figures_exploratory/BIC/Silomaize/Sep/best_SM_Sep_cooks.txt", append=T)
capture.output(c("Cutoff_large_number",nrow_cooks_large), file = "./figures/figures_exploratory/BIC/Silomaize/Sep/best_SM_Sep_cooks.txt", append=T)

## Make a table of numbers of values above small_cutoff conditional on years 
year_cooks_SM_Sep_small <- as.data.frame(table(Yield_Covariates_SM_Sep$year[cook_Sep > cutoff_SM_Sep_small ]) )
names(year_cooks_SM_Sep_small) <- c("year", "Freq")
year_cooks_SM_Sep_small
plot(year_cooks_SM_Sep_small)

## Make a table of numbers of valued above small_cutoff conditional on comId
comId_cooks_SM_Sep_small <- as.data.frame(table(Yield_Covariates_SM_Sep$comId[cook_Sep > cutoff_SM_Sep_small ]) )
names(comId_cooks_SM_Sep_small) <- c("comId", "Freq")
comId_cooks_SM_Sep_small
plot(comId_cooks_SM_Sep_small)

## Make ordered table of cutoff above frequancies per comId
comId_cooks_SM_Sep_small2 <-cbind(comId_cooks_SM_Sep_small, unique(Yield_Covariates_SM_Sep$com))
comId_cooks_SM_Sep_small2[order(comId_cooks_SM_Sep_small2$Freq),]
comId_cooks_SM_Sep_small_ordered <- comId_cooks_SM_Sep_small2[order(comId_cooks_SM_Sep_small2$Freq),]

## Write Tables
capture.output(year_cooks_SM_Sep_small, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/best_SM_Sep_cooks.txt", append=T)
capture.output(comId_cooks_SM_Sep_small, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/best_SM_Sep_cooks.txt", append=T)
capture.output(c("Ordered table of comIds", comId_cooks_SM_Sep_small_ordered), file = "./figures/figures_exploratory/BIC/Silomaize/Sep/best_SM_Sep_cooks.txt", append=T)

write.csv(year_cooks_SM_Sep_small, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/best_SM_Sep_cooks_year.csv")
write.csv(comId_cooks_SM_Sep_small2, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/best_SM_Sep_cooks_comId.csv")
write.csv(comId_cooks_SM_Sep_small_ordered, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/best_SM_Sep_cooks_comId_ordered.csv")

################################################################################################################################################################## 

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
          dep.var.caption  = "Model with smallest BIC - Sepe",
          dep.var.labels = "log(Silomaize)",
          style="default",
          model.numbers = FALSE,
          column.labels = labels1,          
          type="text", out="./figures/figures_exploratory/BIC/Silomaize/Sep/SM_Sep_best.txt"
)



#########################################################
## Results with smallest BIC of Standard Configuration ##
#########################################################

## Look for smallest BIC of Standar Config
BIC_SM_Sep[which.min(BIC_SM_Sep$BIC[1:9]),]
BIC_SM_Sep[order(BIC_SM_Sep$BIC[1:9])[1],]


## Define difference between first and second best BIC Value ##
-(BIC_SM_Sep$BIC[order(BIC_SM_Sep$BIC[1:9])[1]] -  BIC_SM_Sep$BIC[order(BIC_SM_Sep$BIC[1:9])[2]])

## Make condition for if ... else loop
-(BIC_SM_Sep$BIC[order(BIC_SM_Sep$BIC[1:9])[1]] -  BIC_SM_Sep$BIC[order(BIC_SM_Sep$BIC[1:9])[2]])
x <- -(BIC_SM_Sep$BIC[order(BIC_SM_Sep$BIC[1:9])[1]] -  BIC_SM_Sep$BIC[order(BIC_SM_Sep$BIC[1:9])[2]])

## Look at order of StandardConfig and print it
BIC_SM_Sep[order(BIC_SM_Sep$BIC[1:9]),]
BIC_order_Sep_standard <- BIC_SM_Sep[order(BIC_SM_Sep$BIC[1:9]),]
config <- rep("Standard_Config", 9)
BIC_order_Sep_standard <- cbind(BIC_order_Sep_standard, config)

capture.output(BIC_order_Sep_standard, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/BIC_order_Sep", append=T)

## Extract position of smallest and second smallest BIC of Standard Config ##
BIC_SM_Sep[order(BIC_SM_Sep$BIC[1:9])[1],][[4]]
BIC_SM_Sep[order(BIC_SM_Sep$BIC[1:9])[2],][[4]]


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
  BIC_SM_Sep[order(BIC_SM_Sep$BIC[1:9])[1],][[3]]
  
  
  ## Determine degree 
  r = BIC_SM_Sep[order(BIC_SM_Sep$BIC[1:9])[1],][[4]]
  
  ## Extract model with lowest BIC of the models with that particular model index 
  order(BIC_SM_Sep[BIC_SM_Sep$model_index==BIC_SM_Sep[order(BIC_SM_Sep$BIC[1:9])[1],][[3]],][,1])[1]
  
  
  ## Determine adequate formula for model by the means of the model index
  bestStandard_formula <-  formula_list_Sep_sm_detrendlog[[BIC_SM_Sep[order(BIC_SM_Sep$BIC[1:9])[1],][[3]]]]
  bestStandard_formula
  
  
# } else {
#   
#   #############################################################
#   ## If difference is smaller than 6, take second best model ##
#   model_standard <- print("Standard Formula (Standard Configuration) - Second Best BIC")
#   model_standard
#   
#   #  Extract model index
#   BIC_SM_Sep[order(BIC_SM_Sep$BIC[1:9])[2],][[3]]
#   
#   ## Determine degree 
#   r = BIC_SM_Sep[order(BIC_SM_Sep$BIC[1:9])[2],][[4]]
#   
#   ## Extract model with lowest BIC of the models with that particular model index 
#   order(BIC_SM_Sep[BIC_SM_Sep$model_index==BIC_SM_Sep[order(BIC_SM_Sep$BIC[1:9])[1],][[3]],][,1])[2]
#   
#   ## Determine adequate formula for model by the means of the model index
#   bestStandard_formula <-  formula_list_Sep_sm_detrendlog[[BIC_SM_Sep[order(BIC_SM_Sep$BIC[1:9])[2],][[3]]]]
#   bestStandard_formula
#   
# }
capture.output(model_standard, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/BIC_order_Sep", append=T)
capture.output(bestStandard_formula, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/BIC_order_Sep", append=T)
r

###################
## GLM Ergebniss ##
glm.fit_SM_bestStandard_Sep  <- glm(formula = bestStandard_formula,  data = Yield_Covariates_SM_Sep)

## Summary
bestStandard_glm_summary <- summary(glm.fit_SM_bestStandard_Sep)
capture.output(bestStandard_glm_summary, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/Output_SM_Sep_qual", append=T)

## AIC
bestStandard_glm_summary_qual <- print(c("bestStandard_glm_AIC:", round(bestStandard_glm_summary$aic,4)))
capture.output(print("BEST STANDARD MODELS"), file = "./figures/figures_exploratory/BIC/Silomaize/Sep/Output_SM_Sep_qual.txt", append=T)
capture.output(bestStandard_glm_summary_qual, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/Output_SM_Sep_qual.txt", append=T)

summary(glm.fit_SM_bestStandard_Sep)
'AIC:-6410.5'

####################
## PLM Ergebnisse ##
plm.fit_SM_bestStandard_Sep <- plm(formula = update(bestStandard_formula, .~. - dummy(comId)),  data = Yield_Covariates_SM_Sep,  effect="individual", model=("within"), index = c("comId","year"))

## Output
bestStandard_plm_summary <- summary(plm.fit_SM_bestStandard_Sep)
bestStandard_plm_summary
capture.output(bestStandard_plm_summary, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/Output_SM_Sep_qual", append=T)

bestStandard_plm_summary_qual <- print(c("bestStandard_plm:", round(bestStandard_plm_summary$r.squared,4)))
capture.output(bestStandard_plm_summary_qual, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/Output_SM_Sep_qual.txt", append=T)

'Adj. R-Squared: 0.040653'

## Fixed Effects ##
fixef <-  fixef(plm.fit_SM_bestStandard_Sep)
fixef_summary <- as.matrix(summary(fixef))
capture.output(fixef_summary, file = "./figures/figures_exploratory/FixedEffects/Silomaize/Sep/plm.fit_SM_bestStandard_Sep_FE_summary.txt")
# fixef_summary <- read.table( file = "./figures/figures_exploratory/FixedEffects/Silomaize/Sep/plm.fit_SM_BEST_Sep_FE_summary.txt")

##################
## LM Ergebniss ##
lm.fit_SM_bestStandard_Sep  <-lm(formula = bestStandard_formula,  data = Yield_Covariates_SM_Sep)
summary(lm.fit_SM_bestStandard_Sep) 
'Adjusted R-squared:  0.6276'

bestStandard_lm_summary <- summary(lm.fit_SM_bestStandard_Sep) 
bestStandard_lm_summary$r.squared
capture.output(bestStandard_lm_summary, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/Output_SM_Sep_qual", append=T)


bestStandard_lm_summary_qual1 <- print(c("bestStandard_lm_r.squared:", round(bestStandard_lm_summary$r.squared,4)))
bestStandard_lm_summary_qual2 <- print(c("bestStandard_lm_adj.r.squared:", round(bestStandard_lm_summary$adj.r.squared,4)))
capture.output(bestStandard_lm_summary_qual1, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/Output_SM_Sep_qual.txt", append=T)
capture.output(bestStandard_lm_summary_qual2, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/Output_SM_Sep_qual.txt", append=T)

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
          dep.var.caption  = "Model with smallest BIC (Standard Configuration) - Sepe",
          dep.var.labels = "log(Silomaize)",
          style="default",
          model.numbers = FALSE,
          column.labels = labels1,          
          type="text", out="./figures/figures_exploratory/BIC/Silomaize/Sep/SM_Sep_bestStandard.txt"
)

########################################
## Results with smallest BIC with SMI ##
########################################

## Look for smallest BIC with SMI
BIC_SM_Sep_onlySMI<- BIC_SM_Sep[1:28,]
BIC_SM_Sep_onlySMI[which.min(BIC_SM_Sep_onlySMI$BIC),]
BIC_SM_Sep_onlySMI[order(BIC_SM_Sep_onlySMI$BIC)[1],]


## Define difference between first and second best BIC Value ##
-(BIC_SM_Sep_onlySMI$BIC[order(BIC_SM_Sep_onlySMI$BIC)[1]] -  BIC_SM_Sep_onlySMI$BIC[order(BIC_SM_Sep_onlySMI$BIC)[2]])

## Make condition for if ... else loop
-(BIC_SM_Sep_onlySMI$BIC[order(BIC_SM_Sep_onlySMI$BIC)[1]] -  BIC_SM_Sep_onlySMI$BIC[order(BIC_SM_Sep_onlySMI$BIC)[2]])
x <--(BIC_SM_Sep_onlySMI$BIC[order(BIC_SM_Sep_onlySMI$BIC)[1]] -  BIC_SM_Sep_onlySMI$BIC[order(BIC_SM_Sep_onlySMI$BIC)[2]])

## Look at order of smalles BIC of Models with SMI and print it
BIC_SM_Sep_onlySMI[order(BIC_SM_Sep_onlySMI$BIC),]
BIC_order_Sep_BestSMI <- BIC_SM_Sep_onlySMI[order(BIC_SM_Sep_onlySMI$BIC),]
config <- rep("Best_SMI_config", 28)
BIC_order_Sep_BestSMI <- cbind(BIC_order_Sep_BestSMI, config)

capture.output(BIC_order_Sep_BestSMI, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/BIC_order_Sep", append=T)

## Extract position of smallest and second smallest BIC of Standard Config ##
BIC_SM_Sep_onlySMI[order(BIC_SM_Sep_onlySMI$BIC)[1],][[4]]
BIC_SM_Sep_onlySMI[order(BIC_SM_Sep_onlySMI$BIC)[2],][[4]]



############################################################################################
## If difference is larger than six between first and second best model, take first model ##
## However, it can not really be assumed that there is no difference though

# if ( x > 6)
# {
  model_bestSMI <- print("Best Formula (with SMI) - Best BIC")
  model_bestSMI
  #  Extract model index
  BIC_SM_Sep_onlySMI[order(BIC_SM_Sep_onlySMI$BIC)[1],][[3]]
  
  ## Extract model with lowest BIC of the models with that particular model index 
  order(BIC_SM_Sep_onlySMI[BIC_SM_Sep_onlySMI$model_index==BIC_SM_Sep_onlySMI[order(BIC_SM_Sep_onlySMI$BIC)[1],][[3]],][,1])[1]
  
  ## Determine degree 
  r = order(BIC_SM_Sep_onlySMI[BIC_SM_Sep_onlySMI$model_index==BIC_SM_Sep_onlySMI[order(BIC_SM_Sep_onlySMI$BIC)[1],][[3]],][,1])[1]
  
  ## Determine adequate formula for model by the means of the model index
  bestSMI_formula <-  formula_list_Sep_sm_detrendlog[[BIC_SM_Sep_onlySMI[order(BIC_SM_Sep_onlySMI$BIC)[1],][[3]]]]
  bestSMI_formula
  
#   
# } else {
#   
#   #############################################################
#   ## If difference is smaller than 6, take second best model ##
#   model_bestSMI <- print("Best Formula (with SMI) - Second Best BIC")
#   model_bestSMI
#   ## Look at second best model ##
#   BIC_SM_Sep_onlySMI[order(BIC_SM_Sep_onlySMI$BIC)[2],]
#   
#   ##  Extract model index
#   BIC_SM_Sep_onlySMI[order(BIC_SM_Sep_onlySMI$BIC)[2],][[3]]
#   
#   ## Extract model with second lowest BIC of the models with that particular model index 
#   order(BIC_SM_Sep_onlySMI[BIC_SM_Sep_onlySMI$model_index==BIC_SM_Sep_onlySMI[order(BIC_SM_Sep_onlySMI$BIC)[1],][[3]],][,1])[2]
#   
#   ## Determine degree 
#   r = order(BIC_SM_Sep_onlySMI[BIC_SM_Sep_onlySMI$model_index==BIC_SM_Sep_onlySMI[order(BIC_SM_Sep_onlySMI$BIC)[1],][[3]],][,1])[2]
#   ## Determine adequate formula for model by the means of the model index
#   bestSMI_formula <-  formula_list_Sep_sm_detrendlog[[BIC_SM_Sep_onlySMI[order(BIC_SM_Sep_onlySMI$BIC)[2],][[3]]]]
#   bestSMI_formula
#   
# }
capture.output(model_bestSMI, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/BIC_order_Sep", append=T)
capture.output(bestSMI_formula, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/BIC_order_Sep", append=T)
r

###################
## GLM Ergebniss ##
glm.fit_SM_bestSMI_Sep  <- glm(formula = bestSMI_formula,  data = Yield_Covariates_SM_Sep)

## Summary
bestSMI_glm_summary <- summary(glm.fit_SM_bestSMI_Sep)
bestSMI_glm_summary
capture.output(bestSMI_glm_summary, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/Output_SM_Sep_qual", append=T)

## AIC
capture.output(print("BEST SMI MODELS"), file = "./figures/figures_exploratory/BIC/Silomaize/Sep/Output_SM_Sep_qual.txt", append=T)
bestSMI_glm_summary_qual <- print(c("bestSMI_glm_AIC:", round(bestSMI_glm_summary$aic,4)))
capture.output(bestSMI_glm_summary_qual, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/Output_SM_Sep_qual.txt", append=T)

####################
## PLM Ergebnisse ##
plm.fit_SM_bestSMI_Sep <- plm(formula = update(bestSMI_formula, .~. - dummy(comId)),  data = Yield_Covariates_SM_Sep,  effect="individual", model=("within"), index = c("comId","year"))

## Output
bestSMI_plm_summary <- summary(plm.fit_SM_bestSMI_Sep)
bestSMI_plm_summary
capture.output(bestSMI_plm_summary, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/Output_SM_Sep_qual", append=T)

bestSMI_plm_summary_qual <- print(c("bestSMI_plm:", round(bestSMI_plm_summary$r.squared,4)))
capture.output(bestSMI_plm_summary_qual, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/Output_SM_Sep_qual.txt", append=T)


## Fixed Effects ##
fixef <-  fixef(plm.fit_SM_bestSMI_Sep)
fixef_summary <- as.matrix(summary(fixef))
capture.output(fixef_summary, file = "./figures/figures_exploratory/FixedEffects/Silomaize/Sep/plm.fit_SM_bestSMI_Sep_FE_summary.txt")
# fixef_summary <- read.table( file = "./figures/figures_exploratory/FixedEffects/Silomaize/Sep/plm.fit_SM_BEST_Sep_FE_summary.txt")

##################
## LM Ergebniss ##
lm.fit_SM_bestSMI_Sep  <-lm(formula = bestSMI_formula,  data = Yield_Covariates_SM_Sep)
summary(lm.fit_SM_bestSMI_Sep) 
'Adjusted R-squared:  0.6276'

bestSMI_lm_summary <- summary(lm.fit_SM_bestSMI_Sep) 
capture.output(bestSMI_lm_summary, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/Output_SM_Sep_qual", append=T)


bestSMI_lm_summary_qual1 <- print(c("bestSMI_lm_r.squared:", round(bestSMI_lm_summary$r.squared,4)))
bestSMI_lm_summary_qual2 <- print(c("bestSMI_lm_adj.r.squared:", round(bestSMI_lm_summary$adj.r.squared,4)))
capture.output(bestSMI_lm_summary_qual1, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/Output_SM_Sep_qual.txt", append=T)
capture.output(bestSMI_lm_summary_qual2, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/Output_SM_Sep_qual.txt", append=T)

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

' Achtung: Driscoll Kraay ist nicht geeignet für kurze Panel. Ist das bei mir der Fall? '

## Cameron et al /Thompson : doouble-clustering estimator ##
cov3_SM_bestSMI_Sep <- vcovDC(plm.fit_SM_bestSMI_Sep, method = "arellano", type = "HC0")
CT.se_SM_bestSMI_Sep <- sqrt(diag(cov3_SM_bestSMI_Sep))

################################
## Generate Table with Output ##
se <- list(NULL, Wh.se_cross_SM_bestSMI_Sep, Wh.se_serial_SM_bestSMI_Sep,  DK.se_SM_bestSMI_Sep, DK2.5.se_SM_bestSMI_Sep, CT.se_SM_bestSMI_Sep)
labels1 <-c("NULL","WhiteCross","WhiteSerial", "DriscollKraay", "DriscollKray2.5","CameronThompson")
stargazer(plm.fit_SM_bestSMI_Sep, plm.fit_SM_bestSMI_Sep, plm.fit_SM_bestSMI_Sep, plm.fit_SM_bestSMI_Sep, plm.fit_SM_bestSMI_Sep,plm.fit_SM_bestSMI_Sep,
          se = se,   
          dep.var.caption  = "Model with smallest BIC (with SMI) - Sepe",
          dep.var.labels = "log(Silomaize)",
          style="default",
          model.numbers = FALSE,
          column.labels = labels1,          
          type="text", out="./figures/figures_exploratory/BIC/Silomaize/Sep/SM_Sep_bestSM.txt"
)

###########################################
## Results with smallest BIC without PET ##
###########################################

## Look for smallest BIC without PET

BIC_SM_Sep_noPET <- subset(BIC_SM_Sep, model_index==c(1) |  model_index==c(3)|  model_index==c(5)|  model_index==c(6)|  model_index==c(7)|  model_index==c(8)|  model_index==c(10))
rownames(BIC_SM_Sep_noPET ) <- NULL
BIC_SM_Sep_noPET
## Substitute Model Indeces ##
BIC_SM_Sep_noPET$model_index <- c(1,1,1,1,1,1,1,1,1,
                                  2,2,2,
                                  3,3,3,
                                  4,
                                  5,5,5,
                                  6,6,6,
                                  7,7,7,7,7,7,7,7,7)
BIC_SM_Sep_noPET$model <- c("01_SMIPrecTavg","01_SMIPrecTavg","01_SMIPrecTavg","01_SMIPrecTavg","01_SMIPrecTavg","01_SMIPrecTavg","01_SMIPrecTavg","01_SMIPrecTavg","01_SMIPrecTavg",
                            "02_SMIPrec","02_SMIPrec","02_SMIPrec",
                            "03_SMITavg","03_SMITavg","03_SMITavg",
                            "04_SMI",
                            "05_Prec","05_Prec","05_Prec",
                            "06_Tavg","06_Tavg","06_Tavg",
                            "07_PrecTavg","07_PrecTavg","07_PrecTavg","07_PrecTavg","07_PrecTavg","07_PrecTavg","07_PrecTavg","07_PrecTavg","07_PrecTavg")

BIC_SM_Sep_noPET$index <- c(1:31)

## Adapt Formula Model List ##
formula_list_Sep_sm_detrendlog_noPET <- formula_list_Sep_sm_detrendlog[c(1,3,5,6,7,8,10)]

## Save BIC Data without PET
write.csv(BIC_SM_Sep_noPET, "./data/data_raw/BIC/BIC_SM_Sep_noPET.csv")

## Chechout models with smallest BIC
BIC_SM_Sep_noPET[which.min(BIC_SM_Sep_noPET$BIC),]
BIC_SM_Sep_noPET[order(BIC_SM_Sep_noPET$BIC)[1],]
BIC_SM_Sep_noPET[order(BIC_SM_Sep_noPET$BIC)[2],]

BIC_SM_Sep_noPET$BIC[order(BIC_SM_Sep_noPET$BIC)]

## Define difference between first and second best BIC Value ##
-(BIC_SM_Sep_noPET$BIC[order(BIC_SM_Sep_noPET$BIC)[1]] -  BIC_SM_Sep_noPET$BIC[order(BIC_SM_Sep_noPET$BIC)[2]])

## Make condition for if ... else loop
-(BIC_SM_Sep_noPET$BIC[order(BIC_SM_Sep_noPET$BIC)[1]] -  BIC_SM_Sep_noPET$BIC[order(BIC_SM_Sep_noPET$BIC)[2]])
x <- -(BIC_SM_Sep_noPET$BIC[order(BIC_SM_Sep_noPET$BIC)[1]] -  BIC_SM_Sep_noPET$BIC[order(BIC_SM_Sep_noPET$BIC)[2]])

## Look at order 
BIC_SM_Sep_noPET[order(BIC_SM_Sep_noPET$BIC),]
BIC_order_Sep_noPET <- BIC_SM_Sep_noPET[order(BIC_SM_Sep_noPET$BIC),]
config <- rep("Best_noPET_config", 31)
BIC_order_Sep_noPET <- cbind(BIC_order_Sep_noPET, config)

capture.output(BIC_order_Sep_noPET, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/BIC_order_Sep", append=T)

## Extract position of smallest and second smallest BIC of Standard Config ##
BIC_SM_Sep_noPET[order(BIC_SM_Sep_noPET$BIC)[1],][[4]]
BIC_SM_Sep_noPET[order(BIC_SM_Sep_noPET$BIC)[2],][[4]]





############################################################################################
## If difference is larger than six between first and second best model, take first model ##
## However, it can not really be assumed that there is no difference though


# if (x > 6)
# {
  model_noPET <- print("Best Formula (no PET)- Best BIC")
  model_noPET
  
  #  Extract model index
  BIC_SM_Sep_noPET[order(BIC_SM_Sep_noPET$BIC)[1],][[3]]
  
  ## Extract model with lowest BIC of the models with that particular model index 
  order(BIC_SM_Sep_noPET[BIC_SM_Sep_noPET$model_index==BIC_SM_Sep_noPET[order(BIC_SM_Sep_noPET$BIC)[1],][[3]],][,1])[1]
  
  ## Determine degree 
  r = order(BIC_SM_Sep_noPET[BIC_SM_Sep_noPET$model_index==BIC_SM_Sep_noPET[order(BIC_SM_Sep_noPET$BIC)[1],][[3]],][,1])[1]
  
  ## Determine adequate formula for model by the means of the model index
  best_noPET_formula <-  formula_list_Sep_sm_detrendlog_noPET[[BIC_SM_Sep_noPET[order(BIC_SM_Sep_noPET$BIC)[1],][[3]]]]
  best_noPET_formula
  
#   
# } else {
#   
#   #############################################################
#   ## If difference is smaller than 6, take second best model ##
#   model_noPET <- print("Best Formula (no PET)  -  Second Best BIC")
#   model_noPET
#   
#   ## Look at second best model ##
#   BIC_SM_Sep_noPET[order(BIC_SM_Sep_noPET$BIC)[2],]
#   
#   ##  Extract model index
#   BIC_SM_Sep_noPET[order(BIC_SM_Sep_noPET$BIC)[2],][[3]]
#   
#   ## Extract model with second lowest BIC of the models with that particular model index 
#   order(BIC_SM_Sep_noPET[BIC_SM_Sep_noPET$model_index==BIC_SM_Sep_noPET[order(BIC_SM_Sep_noPET$BIC)[1],][[3]],][,1])[2]
#   
#   ## Determine degree 
#   r = order(BIC_SM_Sep_noPET[BIC_SM_Sep_noPET$model_index==BIC_SM_Sep_noPET[order(BIC_SM_Sep_noPET$BIC)[1],][[3]],][,1])[2]
#   
#   ## Determine adequate formula for model by the means of the model index
#   best_noPET_formula <-  formula_list_Sep_sm_detrendlog_noPET[[BIC_SM_Sep_noPET[order(BIC_SM_Sep_noPET$BIC)[2],][[3]]]]
#   best_noPET_formula
#   
# }
capture.output(model_noPET, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/BIC_order_Sep", append=T)
capture.output(best_noPET_formula, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/BIC_order_Sep", append=T)
r

###################
## GLM Ergebniss ##
glm.fit_SM_noPET_Sep  <- glm(formula = best_noPET_formula,  data = Yield_Covariates_SM_Sep)

## Summary
best_noPET_glm_summary <- summary(glm.fit_SM_noPET_Sep)
best_noPET_glm_summary
capture.output(best_noPET_glm_summary, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/Output_SM_Sep_qual", append=T)


## AIC
best_noPET_glm_summary_qual <- print(c("best_noPET_glm_AIC:", round(best_noPET_glm_summary$aic,4)))

capture.output(print("BEST NO PET MODEL"), file = "./figures/figures_exploratory/BIC/Silomaize/Sep/Output_SM_Sep_qual.txt", append=T)
capture.output(best_noPET_glm_summary_qual, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/Output_SM_Sep_qual.txt", append=T)

####################
## PLM Ergebnisse ##
plm.fit_SM_noPET_Sep <- plm(formula = update(best_noPET_formula, .~. - dummy(comId)),  data = Yield_Covariates_SM_Sep,  effect="individual", model=("within"), index = c("comId","year"))

## Output
best_noPET_plm_summary <- summary(plm.fit_SM_noPET_Sep)
best_noPET_plm_summary
capture.output(best_noPET_plm_summary, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/Output_SM_Sep_qual", append=T)

best_noPET_plm_summary_qual <- print(c("best_noPET_plm:", round(best_noPET_plm_summary$r.squared,4)))
capture.output(best_noPET_plm_summary_qual, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/Output_SM_Sep_qual.txt", append=T)


## Fixed Effects ##
fixef <-  fixef(plm.fit_SM_noPET_Sep)
fixef_summary <- as.matrix(summary(fixef))
capture.output(fixef_summary, file = "./figures/figures_exploratory/FixedEffects/Silomaize/Sep/plm.fit_SM_noPET_Sep_FE_summary.txt")
# fixef_summary <- read.table( file = "./figures/figures_exploratory/FixedEffects/Silomaize/Sep/plm.fit_SM_BEST_Sep_FE_summary.txt")

##################
## LM Ergebniss ##
lm.fit_SM_noPET_Sep  <-lm(formula = best_noPET_formula,  data = Yield_Covariates_SM_Sep)
summary(lm.fit_SM_noPET_Sep) 

best_noPET_lm_summary <- summary(lm.fit_SM_noPET_Sep) 
capture.output(best_noPET_lm_summary, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/Output_SM_Sep_qual", append=T)


best_noPET_lm_summary_qual1 <- print(c("best_noPET_lm_r.squared:", round(best_noPET_lm_summary$r.squared,4)))
best_noPET_lm_summary_qual2 <- print(c("best_noPET_lm_adj.r.squared:", round(best_noPET_lm_summary$adj.r.squared,4)))
capture.output(best_noPET_lm_summary_qual1, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/Output_SM_Sep_qual.txt", append=T)
capture.output(best_noPET_lm_summary_qual2, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/Output_SM_Sep_qual.txt", append=T)

################################################
## Assessing Influence (Leverage*discrepancy) ##
cutoff_SM_Sep <- 4/((nrow(Yield_Covariates_SM_Sep)-length(lm.fit_SM_noPET_Sep$coefficients)-1)) 
cutoff_SM_Sep
plot(lm.fit_SM_noPET_Sep, which=4)
cook_Sep <- cooks.distance(lm.fit_SM_noPET_Sep)

nrow_cooks <- nrow(Yield_Covariates_SM_Sep[cook_Sep > cutoff_SM_Sep,]) 
capture.output(nrow_cooks, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/best_SM_Sep_cooks_noPET.txt")

year_cooks_SM_Sep <- table(Yield_Covariates_SM_Sep$year[cook_Sep > cutoff_SM_Sep ]) 
year_cooks_SM_Sep
capture.output(year_cooks_SM_Sep, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/best_SM_Sep_cooks_noPET.txt", append=T)

com_cooks_SM_Sep <- sort(table(Yield_Covariates_SM_Sep$com[cook_Sep > cutoff_SM_Sep ] ))
capture.output(com_cooks_SM_Sep, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/best_SM_Sep_cooks_noPET.txt", append=T)


########################
## Heteroskedasdicity ##
bptest(glm.fit_SM_noPET_Sep) # Breusch Pagan Test of Heteroskedastie in den Störgrößen: Null: Homoskedasdicity.
bptest(plm.fit_SM_noPET_Sep)
' In beiden Fällen kann die Null widerlegt werden. Es gibt also heteroskedasdicity '

## Koenkers Version on BP Test: robuste Modification wenn die Störgrößen nicht normalverteilt sind. 
bptest(plm.fit_SM_noPET_Sep, studentize = TRUE)
'Auch hier kann die Null widerlegt werden. Need to use robust covariance variance matrix to correct standard errors'

#########################
#### Autocorrelation ####

######################################
## Tests for serial autocorrelation ##
pwartest(plm.fit_SM_noPET_Sep)
pbgtest(plm.fit_SM_noPET_Sep) 
'Hier serielle Korrelation festzustellen'

###########################################
## Correct Standard Errors used in table ##
coeftest(plm.fit_SM_noPET_Sep)

## Robust covariance matrix estimators a la White ##
# coeftest(plm.fit_SM_noPET_Sep,vcov=vcovHC(plm.fit_SM_noPET_Sep,method = "arellano", type = "HC0")) 
cov0_SM_noPET_Sep <- vcovHC(plm.fit_SM_noPET_Sep,method = "arellano", type = "HC0", cluster="group")
Wh.se_serial_SM_noPET_Sep <- sqrt(diag(cov0_SM_noPET_Sep))

cov0.1_SM_noPET_Sep <- vcovHC(plm.fit_SM_noPET_Sep,method = "arellano", type = "HC0", cluster="time")
Wh.se_cross_SM_noPET_Sep <- sqrt(diag(cov0.1_SM_noPET_Sep))
# 
# ## Beck Katz:
# # coeftest(plm.fit_SM_noPET_Sep, vcov = function(x) vcovBK(plm.fit_SM_noPET_Sep,method = "arellano", type = "HC0"))
# cov1 <- vcovBK(plm.fit_SM_noPET_Sep,method = "arellano", type = "HC0",  cluster="time")
# BK.se <- sqrt(diag(cov1))

## Driscoll Kraay ##
# summary(plm.fit_SM_noPET_Sep)
cov2_SM_noPET_Sep     <- vcovSCC(plm.fit_SM_noPET_Sep,method = "arellano",type = "HC0")
DK.se_SM_noPET_Sep    <- sqrt(diag(cov2_SM_noPET_Sep))

# cov2.1_SM_noPET_Sep   <- vcovSCC(plm.fit_SM_noPET_Sep,method = "arellano",type = "HC0", maxlag=1)
# DK2.1.se_SM_noPET_Sep <- sqrt(diag(cov2.1_SM_noPET_Sep))

# cov2.2_SM_noPET_Sep   <- vcovSCC(plm.fit_SM_noPET_Sep,method = "arellano",type = "HC0", maxlag=2)
# DK2.2.se_SM_noPET_Sep <- sqrt(diag(cov2.2_SM_noPET_Sep))
# 
# cov2.3_SM_noPET_Sep   <- vcovSCC(plm.fit_SM_noPET_Sep,method = "arellano",type = "HC0", maxlag=3)
# DK2.3.se_SM_noPET_Sep <- sqrt(diag(cov2.3_SM_noPET_Sep))
# 
# cov2.4_SM_noPET_Sep   <- vcovSCC(plm.fit_SM_noPET_Sep,method = "arellano",type = "HC0", maxlag=4)
# DK2.4.se_SM_noPET_Sep <- sqrt(diag(cov2.4_SM_noPET_Sep))
# 
cov2.5_SM_noPET_Sep   <- vcovSCC(plm.fit_SM_noPET_Sep,method = "arellano",type = "HC0", maxlag=5)
DK2.5.se_SM_noPET_Sep <- sqrt(diag(cov2.5_SM_noPET_Sep))

' Achtung: Driscoll Kraay ist nicht geeignet für kurze Panel. Ist das bei mir der Fall? '

## Cameron et al /Thompson : doouble-clustering estimator ##
cov3_SM_noPET_Sep <- vcovDC(plm.fit_SM_noPET_Sep, method = "arellano", type = "HC0")
CT.se_SM_noPET_Sep <- sqrt(diag(cov3_SM_noPET_Sep))

################################
## Generate Table with Output ##
se <- list(NULL, Wh.se_cross_SM_noPET_Sep, Wh.se_serial_SM_noPET_Sep,  DK.se_SM_noPET_Sep, DK2.5.se_SM_noPET_Sep, CT.se_SM_noPET_Sep)
labels1 <-c("NULL","WhiteCross","WhiteSerial", "DriscollKraay", "DriscollKray2.5","CameronThompson")
stargazer(plm.fit_SM_noPET_Sep, plm.fit_SM_noPET_Sep, plm.fit_SM_noPET_Sep, plm.fit_SM_noPET_Sep, plm.fit_SM_noPET_Sep,plm.fit_SM_noPET_Sep,
          se = se,   
          dep.var.caption  = "Model with smallest BIC (no PET) - Sepe",
          dep.var.labels = "log(Silomaize)",
          style="default",
          model.numbers = FALSE,
          column.labels = labels1,          
          type="text", out="./figures/figures_exploratory/BIC/Silomaize/Sep/SM_Sep_best_noPET.txt"
)

###################################################################################################################################################################
###################################################################################################################################################################

################################################################
## Generate output of Standard Model with highest Polynomials ##
r <- 9
formula_Sep_sm_detrendlog_SMIPrecTavg <- siloMaize_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  poly(Tavg, degree[r, 2], raw = T) + 
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormally dry", "abnormally wet" ,"abundantly wet", "severely wet")) 

plm.fit_SM_BestStdFull_Sep <- plm(formula_Sep_sm_detrendlog_SMIPrecTavg,  data = Yield_Covariates_SM_Sep,  effect="individual", model=("within"), index = c("comId","year"))
summary(plm.fit_SM_BestStdFull_Sep)

cov2_SM_BestStdFull_Sep     <- vcovSCC(plm.fit_SM_BestStdFull_Sep,method = "arellano",type = "HC0")
DK.se_SM_BestStdFull_Sep    <- sqrt(diag(cov2_SM_BestStdFull_Sep ))

# LM ##
lm.fit_SM_BestStdFull_Sep <- lm(update(formula_Sep_sm_detrendlog_SMIPrecTavg , .~. + dummy(comId)),  data = Yield_Covariates_SM_Sep)
summary(lm.fit_SM_BestStdFull_Sep)

BestStdFull_lm_summary <- summary(lm.fit_SM_BestStdFull_Sep) 
BestStdFull_lm_summary$r.squared
capture.output(BestStdFull_lm_summary, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/Output_SM_Sep_qual", append=T)

Strich <- print("--------------------------")
BestStdFull_lm_summary_qual1 <- print(c("BestStdFull_lm_r.squared:", round(BestStdFull_lm_summary$r.squared,4)))
BestStdFull_lm_summary_qual2 <- print(c("BestStdFull_lm_adj.r.squared:", round(BestStdFull_lm_summary$adj.r.squared,4)))
capture.output(Strich, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/Output_SM_Sep_qual.txt", append=T)
capture.output(BestStdFull_lm_summary_qual1, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/Output_SM_Sep_qual.txt", append=T)
capture.output(BestStdFull_lm_summary_qual2, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/Output_SM_Sep_qual.txt", append=T)


#########################################
## Best Standard model neglecting Prec ##
r <- 9
formula_Sep_sm_detrendlog_SMITavg <- siloMaize_logtrend ~ poly(Tavg, degree[r, 2], raw = T) +  
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormally dry", "abnormally wet" ,"abundantly wet", "severely wet")) 

plm.fit_SM_BestStdFull_Sep_noPrec <- plm(formula_Sep_sm_detrendlog_SMITavg,  data = Yield_Covariates_SM_Sep,  effect="individual", model=("within"), index = c("comId","year"))
summary(plm.fit_SM_BestStdFull_Sep_noPrec)

cov2_SM_BestStdFull_Sep_noPrec     <- vcovSCC(plm.fit_SM_BestStdFull_Sep_noPrec, method = "arellano",type = "HC0")
DK.se_SM_BestStdFull_Sep_noPrec    <- sqrt(diag(cov2_SM_BestStdFull_Sep_noPrec ))


#########################################
## Best Standard model neglecting Tavg ##
r <- 9
formula_Sep_sm_detrendlog_SMITavg <- siloMaize_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormally dry", "abnormally wet" ,"abundantly wet", "severely wet")) 

plm.fit_SM_BestStdFull_Sep_noTavg <- plm(formula_Sep_sm_detrendlog_SMITavg,  data = Yield_Covariates_SM_Sep,  effect="individual", model=("within"), index = c("comId","year"))
summary(plm.fit_SM_BestStdFull_Sep_noTavg)

cov2_SM_BestStdFull_Sep_noTavg     <- vcovSCC(plm.fit_SM_BestStdFull_Sep_noTavg, method = "arellano",type = "HC0")
DK.se_SM_BestStdFull_Sep_noTavg    <- sqrt(diag(cov2_SM_BestStdFull_Sep_noTavg))


########################################
## Best Standard model neglecting SMI ##
formula_Sep_sm_detrendlog_PrecTavg <- siloMaize_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  poly(Tavg, degree[r, 2], raw = T) 
plm.fit_SM_BestStdFull_Sep_noSMI <- plm(formula_Sep_sm_detrendlog_PrecTavg,  data = Yield_Covariates_SM_Sep,  effect="individual", model=("within"), index = c("comId","year"))

summary(plm.fit_SM_BestStdFull_Sep_noSMI)

cov2_SM_BestStdFull_Sep_noSMI     <- vcovSCC(plm.fit_SM_BestStdFull_Sep_noSMI, method = "arellano",type = "HC0")
DK.se_SM_BestStdFull_Sep_noSMI    <- sqrt(diag(cov2_SM_BestStdFull_Sep_noSMI))

################################
## Best model only using SMI  ##
formula_Sep_sm_detrendlog_SMI <- siloMaize_logtrend ~ dummy(SMI_GDM,c("severe drought","moderate drought","abnormally dry", "abnormally wet" ,"abundantly wet", "severely wet")) 

plm.fit_SM_SMI_Sep <- plm(formula_Sep_sm_detrendlog_SMI,  data = Yield_Covariates_SM_Sep,  effect="individual", model=("within"), index = c("comId","year"))
summary(plm.fit_SM_SMI_Sep)

cov2_SM_SMI_Sep    <- vcovSCC(plm.fit_SM_SMI_Sep, method = "arellano",type = "HC0")
DK.se_SM_SMI_Sep    <- sqrt(diag(cov2_SM_SMI_Sep))

################################################################
## Generate output of PET model with highest Polynomials ##
r <- 9
formula_Sep_sm_detrendlog_SMIPrecPET <- siloMaize_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  poly(PET, degree[r, 2], raw = T) + 
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormally dry", "abnormally wet" ,"abundantly wet", "severely wet")) 

plm.fit_SM_BestPETFull_Sep <- plm(formula_Sep_sm_detrendlog_SMIPrecPET,  data = Yield_Covariates_SM_Sep,  effect="individual", model=("within"), index = c("comId","year"))
summary(plm.fit_SM_BestPETFull_Sep)

cov2_SM_BestPETFull_Sep     <- vcovSCC(plm.fit_SM_BestPETFull_Sep, method = "arellano",type = "HC0")
DK.se_SM_BestPETFull_Sep    <- sqrt(diag(cov2_SM_BestPETFull_Sep))

##########################
## Table of the Models ##
se_results_Sep <- list(DK.se_SM_BestStdFull_Sep, DK.se_SM_BestStdFull_Sep_noPrec, DK.se_SM_BestStdFull_Sep_noTavg, DK.se_SM_BestStdFull_Sep_noSMI, 
                       DK.se_SM_SMI_Sep, DK.se_SM_BestPETFull_Sep)

stargazer(plm.fit_SM_BestStdFull_Sep, plm.fit_SM_BestStdFull_Sep_noPrec, plm.fit_SM_BestStdFull_Sep_noTavg, plm.fit_SM_BestStdFull_Sep_noSMI, plm.fit_SM_SMI_Sep,
          plm.fit_SM_BestPETFull_Sep,
          se = se_results_Sep,   
          title  = "Results of Sep - Different Variations of Standard Configuration",
          dep.var.caption = "Dependent Variable: log(Silomaize)",
          #           align=TRUE, 
          model.numbers = FALSE,
          covariate.labels = c("Precipitation$^{1}$","Precipitation$^{2}$", "Precipitation$^{3}$", "AverageTemperature$^{1}$", "AverageTemperature$^{2}$",
                               "AverageTemperature$^{3}$"    ,  
                               "PET$^{1}$", "PET$^{2}$",  "PET$^{3}$",
                               "SMI:severe drought", "SMI:moderate drought",  "SMI:abnormal dry", "SMI:abnormal wet", 
                               "SMI:abundant wet", "SMI:severe wet" ),
          type="text", out="./figures/figures_exploratory/BIC/Silomaize/Sep/SM_BestStandard_Sep.txt",
          #          out="./figures/figures_exploratory/BIC/Silomaize/Sep/SM_BestStandard_Sep.txt",
          no.space=TRUE ,
          df = FALSE)

stargazer(plm.fit_SM_BestStdFull_Sep, plm.fit_SM_BestStdFull_Sep_noPrec, plm.fit_SM_BestStdFull_Sep_noTavg, plm.fit_SM_BestStdFull_Sep_noSMI, plm.fit_SM_SMI_Sep,
          plm.fit_SM_BestPETFull_Sep,
          se = se_results_Sep,   
          title  = "Results of Sep - Different Variations of Standard Configuration",
          dep.var.caption = "Dependent Variable: log(Silomaize)",
          #           align=TRUE, 
          model.numbers = FALSE,
          covariate.labels = c("Precipitation$^{1}$","Precipitation$^{2}$", "Precipitation$^{3}$", "AverageTemperature$^{1}$", "AverageTemperature$^{2}$",
                               "AverageTemperature$^{3}$"    ,  
                               "PET$^{1}$", "PET$^{2}$",  "PET$^{3}$",
                               "SMI:severe drought", "SMI:moderate drought",  "SMI:abnormal dry", "SMI:abnormal wet", 
                               "SMI:abundant wet", "SMI:severe wet" ),
          out="./figures/figures_exploratory/BIC/Silomaize/Sep/SM_BestStandard_Sep",
          no.space=TRUE ,
          df = FALSE)


###############################################################
## Cooks Distance of Standard Model with 3 degree polynomials ##

## Plot Cooks Distance
plot(lm.fit_SM_BestStdFull_Sep, which=4)

## Read out Cooks Distance ##
cook_Sep <- as.data.frame(cooks.distance(lm.fit_SM_BestStdFull_Sep))
colnames(cook_Sep) <- "CooksDistance"

## Reference Cooks Distance Values
dim(Yield_Covariates_SM_Sep[,1:3])
dim(cook_Sep)
cook_Sep_ref <- cbind(Yield_Covariates_SM_Sep[,1:3],cook_Sep)

## Write out Cooks Distance
write.csv(cook_Sep_ref, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/BestStdFull_Sep_cooksDistance.csv")

## Define Cutoffs ##
cutoff_SM_Sep_small <- 4/((nrow(Yield_Covariates_SM_Sep)-length(lm.fit_SM_BestStdFull_Sep$coefficients)-1)) 
cutoff_SM_Sep_small
cutoff_SM_Sep_large <- 1

## Number of Cooks Distance Values below the cutoffs ##
nrow_cooks_small <- nrow(Yield_Covariates_SM_Sep[cook_Sep > cutoff_SM_Sep_small,]) 
nrow_cooks_large <- nrow(Yield_Covariates_SM_Sep[cook_Sep > cutoff_SM_Sep_large,]) 

## Make a output
capture.output(c("Cutoff_small",cutoff_SM_Sep_small), file = "./figures/figures_exploratory/BIC/Silomaize/Sep/BestStdFull_Sep_cooks.txt")
capture.output(c("Cutoff_small_number",nrow_cooks_small), file = "./figures/figures_exploratory/BIC/Silomaize/Sep/BestStdFull_Sep_cooks.txt", append=T)
capture.output(c("Cutoff_large",cutoff_SM_Sep_large), file = "./figures/figures_exploratory/BIC/Silomaize/Sep/BestStdFull_Sep_cooks.txt", append=T)
capture.output(c("Cutoff_large_number",nrow_cooks_large), file = "./figures/figures_exploratory/BIC/Silomaize/Sep/BestStdFull_Sep_cooks.txt", append=T)

## Make a table of numbers of values above small_cutoff conditional on years 
year_cooks_SM_Sep_small <- as.data.frame(table(Yield_Covariates_SM_Sep$year[cook_Sep > cutoff_SM_Sep_small ]) )
names(year_cooks_SM_Sep_small) <- c("year", "Freq")
year_cooks_SM_Sep_small
plot(year_cooks_SM_Sep_small)

## Make a table of numbers of valued above small_cutoff conditional on comId
comId_cooks_SM_Sep_small <- as.data.frame(table(Yield_Covariates_SM_Sep$comId[cook_Sep > cutoff_SM_Sep_small ]) )
names(comId_cooks_SM_Sep_small) <- c("comId", "Freq")
comId_cooks_SM_Sep_small
plot(comId_cooks_SM_Sep_small)

## Make ordered table of cutoff above frequancies per comId
comId_cooks_SM_Sep_small2 <-cbind(comId_cooks_SM_Sep_small, unique(Yield_Covariates_SM_Sep$com))
comId_cooks_SM_Sep_small2[order(comId_cooks_SM_Sep_small2$Freq),]
comId_cooks_SM_Sep_small_ordered <- comId_cooks_SM_Sep_small2[order(comId_cooks_SM_Sep_small2$Freq),]

## Write Tables
capture.output(year_cooks_SM_Sep_small, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/BestStdFull_Sep_cooks.txt", append=T)
capture.output(comId_cooks_SM_Sep_small, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/BestStdFull_Sep_cooks.txt", append=T)
capture.output(c("Ordered table of comIds", comId_cooks_SM_Sep_small_ordered), file = "./figures/figures_exploratory/BIC/Silomaize/Sep/BestStdFull_Sep_cooks.txt", append=T)

write.csv(year_cooks_SM_Sep_small, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/BestStdFull_Sep_cooks_year.csv")
write.csv(comId_cooks_SM_Sep_small2, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/BestStdFull_Sep_cooks_comId.csv")
write.csv(comId_cooks_SM_Sep_small_ordered, file = "./figures/figures_exploratory/BIC/Silomaize/Sep/BestStdFull_Sep_cooks_comId_ordered.csv")


