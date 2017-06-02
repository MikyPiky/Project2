#######################################################################
## Base Model for Predictions of Soil Moisture Effects on Crop Yield ##
#######################################################################



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
library(caret)   
library(plyr)
library(sf)
####################################################################################################################################################################

#################################################################################################################
#### Create data frame with siloMaize as dependent and variables of the month above as independent variables ####
#################################################################################################################

## Read in large Dataframe for Maize ##
Yield_Covariates <- read.csv("./data/data_processed/yieldData_meteo")
Yield_Covariates$X <- NULL
str(Yield_Covariates)

length(unique(Yield_Covariates$comId))

###############################################
## Delete dependent Variables but silo Maize ##
###############################################
names <- c("winterWheat", "rye", "winterBarley", "summerBarley", "oats", "triticale", "potatoes", "sugarBeet", "winterRape")
Yield_Covariates <- Yield_Covariates[,  !names(Yield_Covariates)%in%names]
head(Yield_Covariates)


#########################################
#### Create stepwise function of SMI ####
#########################################

####################################
## Stepwise with only one anomaly ##

# ## Nasser Zeitraum May - June
# # May
# Yield_Covariates$SMI_May1 <- relevel(cut(Yield_Covariates$SMI_May, breaks = c(0, 0.8, 1),  labels = c("nrml", "wet")), ref= "nrml") 
# dim(Yield_Covariates)
# table(Yield_Covariates$SMI_May1)
# table <- table(Yield_Covariates$SMI_May1, Yield_Covariates$year  )
# sum(table )
# length(Yield_Covariates$SMI_May1)
# 
# write.csv(table, "./figures/figures_exploratory/SMI/SMI_May1" )

# # June
# Yield_Covariates$SMI_Jun1 <- relevel(cut(Yield_Covariates$SMI_Jun, breaks = c(0, 0.8, 1),labels = c("nrml", "wet")), ref= "nrml") 
# 
# table(Yield_Covariates$SMI_Jun1)
# 
# table <- table(Yield_Covariates$SMI_Jun1, Yield_Covariates$year  )
# 
# write.csv(table, "./figures/figures_exploratory/SMI/SMI_Jun1" )
# 

# ## Transition July
# Yield_Covariates$SMI_Jul1 <- relevel(cut(Yield_Covariates$SMI_Jul, breaks = c(0, 0.8, 1), labels = c("nrml", "wet")), ref= "nrml") 
# 
# table(Yield_Covariates$SMI_Jul1)
# table <- table(Yield_Covariates$SMI_Jul1, Yield_Covariates$year  )
# write.csv(table, "./figures/figures_exploratory/SMI/SMI_Jul1" )
# 
# ## Trockener Zeitraum August bis Octobr

# # Aug
# Yield_Covariates$SMI_Aug1 <- relevel(cut(Yield_Covariates$SMI_Aug, breaks = c(0, 0.2, 1), labels = c("dry", "nrml")), ref= "nrml") 
# 
# head(Yield_Covariates$SMI_Aug1 )
# table(Yield_Covariates$SMI_Aug1 )
# table <- table(Yield_Covariates$SMI_Aug1,Yield_Covariates$year  )
# write.csv(table, "./figures/figures_exploratory/SMI/SMI_Aug" )

# # Sep
# Yield_Covariates$SMI_Sep1 <- relevel(cut(Yield_Covariates$SMI_Sep, breaks = c(0, 0.2, 1), labels = c("dry", "nrml")), ref= "nrml") 
# 
# head(Yield_Covariates$SMI_Sep1)
# table(Yield_Covariates$SMI_Sep1)
# table <- table(Yield_Covariates$SMI_Sep1,Yield_Covariates$year  )
# write.csv(table, "./figures/figures_exploratory/SMI/SMI_Sep1" )
# 
# # Oct
# Yield_Covariates$SMI_Oct1 <- relevel(cut(Yield_Covariates$SMI_Oct, breaks = c(0, 0.2, 1), labels = c("dry", "nrml")), ref= "nrml") 
# 
# head(Yield_Covariates$SMI_Oct1 )
# table(Yield_Covariates$SMI_Oct1 )
# table <- table(Yield_Covariates$SMI_Oct1,Yield_Covariates$year  )
# write.csv(table, "./figures/figures_exploratory/SMI/SMI_Oct1" )


# ####################################
# ## Stepwise with four anomalies ##
# 
# ## Nasser Zeitraum May - June
# # May
# Yield_Covariates$SMI_May4 <- relevel(cut(Yield_Covariates$SMI_May, breaks = c(0, 0.1, 0.3, 0.7, 0.9, 1), 
#                                  labels = c("svr drght","dry", "nrml","wt", "svr wt")), ref= "nrml") 
# Yield_Covariates$SMI_May4relevel <- relevel(Yield_Covariates$SMI_May4, ref= "nrml") 
# contrasts(Yield_Covariates$SMI_May4)
# c("svr drght","dry", "nrml","wt", "svr wt")
# dim(Yield_Covariates)
# table(Yield_Covariates$SMI_May4)
# table <- table(Yield_Covariates$SMI_May4, Yield_Covariates$year  )
# sum(table )
# length(Yield_Covariates$SMI_May4)
# 
# write.csv(table, "./figures/figures_exploratory/SMI/SMI_May4" )
# 
# # June
# Yield_Covariates$SMI_Jun4 <- relevel(cut(Yield_Covariates$SMI_Jun, breaks = c(0, 0.1, 0.3, 0.7, 0.9, 1), 
#                                  labels = c("svr drght","dry", "nrml","wt", "svr wt")), ref= "nrml") 
# table(Yield_Covariates$SMI_Jun4)
# 
# table <- table(Yield_Covariates$SMI_Jun4, Yield_Covariates$year  )
# 
# write.csv(table, "./figures/figures_exploratory/SMI/SMI_Jun4" )
# 
# 
# ## Transition July
# Yield_Covariates$SMI_Jul4 <- relevel(cut(Yield_Covariates$SMI_Jul, breaks = c(0, 0.1, 0.3, 0.7, 0.9, 1), 
#                                  labels = c("svr drght","dry", "nrml","wt", "svr wt")), ref= "nrml") 
# 
# table(Yield_Covariates$SMI_Jul4)
# table <- table(Yield_Covariates$SMI_Jul4, Yield_Covariates$year  )
# write.csv(table, "./figures/figures_exploratory/SMI/SMI_Jul4" )
# 
# ## Trockener Zeitraum August bis Octobr
# # Aug
# Yield_Covariates$SMI_Aug4 <- relevel(cut(Yield_Covariates$SMI_Aug, breaks = c(0, 0.1, 0.3, 0.7, 0.9, 1),
#                                  labels = c("svr drght","dry", "nrml","wt", "svr wt")), ref= "nrml") 
# 
# head(Yield_Covariates$SMI_Aug4 )
# table(Yield_Covariates$SMI_Aug4 )
# table <- table(Yield_Covariates$SMI_Aug4,Yield_Covariates$year  )
# write.csv(table, "./figures/figures_exploratory/SMI/SMI_Aug" )
# 
# # Sep
# Yield_Covariates$SMI_Sep4 <- relevel(cut(Yield_Covariates$SMI_Sep, breaks = c(0, 0.1, 0.3, 0.7, 0.9, 1), 
#                                  labels = c("svr drght","dry", "nrml","wt", "svr wt")), ref= "nrml") 
# head(Yield_Covariates$SMI_Sep4)
# table(Yield_Covariates$SMI_Sep4)
# table <- table(Yield_Covariates$SMI_Sep4,Yield_Covariates$year  )
# write.csv(table, "./figures/figures_exploratory/SMI/SMI_Sep4" )
# 
# # Oct
# Yield_Covariates$SMI_Oct4 <- relevel(cut(Yield_Covariates$SMI_Oct, breaks = c(0, 0.1, 0.3, 0.7, 0.9, 1), 
#                                  labels = c("svr drght","dry", "nrml","wt", "svr wt")), ref= "nrml") 
# 

####################################
## Stepwise with six anomalies ##

# ## Nasser Zeitraum May - June
# # May
# Yield_Covariates$SMI_May6 <- relevel(cut(Yield_Covariates$SMI_May, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1), 
#                                  labels = c("svr drght","mdrt drght","abnrml dry", "nrml",
#                                             "abnrml wt" ,"abndnt wt", "svr wt")), ref= "nrml") 
# dim(Yield_Covariates)
# table(Yield_Covariates$SMI_May6)
# table <- table(Yield_Covariates$SMI_May6, Yield_Covariates$year  )
# sum(table )
# length(Yield_Covariates$SMI_May6)
# 
# write.csv(table, "./figures/figures_exploratory/SMI/SMI_May6" )

# June
Yield_Covariates$SMI_Jun6 <- relevel(cut(Yield_Covariates$SMI_Jun, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1), 
                                 labels = c("svr drght","mdrt drght","abnrml dry", "nrml",
                                            "abnrml wt" ,"abndnt wt", "svr wt")), ref= "nrml") 
table(Yield_Covariates$SMI_Jun6)

table <- table(Yield_Covariates$SMI_Jun6, Yield_Covariates$year  )

write.csv(table, "./figures/figures_exploratory/SMI/SMI_Jun6" )


# ## Transition July
# Yield_Covariates$SMI_Jul6 <- relevel(cut(Yield_Covariates$SMI_Jul, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1), 
#                                  labels = c("svr drght","mdrt drght","abnrml dry", "nrml",
#                                             "abnrml wt" ,"abndnt wt", "svr wt")), ref= "nrml") 
# 
# table(Yield_Covariates$SMI_Jul6)
# table <- table(Yield_Covariates$SMI_Jul6, Yield_Covariates$year  )
# write.csv(table, "./figures/figures_exploratory/SMI/SMI_Jul6" )

## Trockener Zeitraum August bis Octobr
# Aug
Yield_Covariates$SMI_Aug6 <- relevel(cut(Yield_Covariates$SMI_Aug, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1),
                                 labels = c("svr drght","mdrt drght","abnrml dry", "nrml",
                                            "abnrml wt" ,"abndnt wt", "svr wt")), ref= "nrml") 

head(Yield_Covariates$SMI_Aug6 )
table(Yield_Covariates$SMI_Aug6 )
table <- table(Yield_Covariates$SMI_Aug6,Yield_Covariates$year  )
write.csv(table, "./figures/figures_exploratory/SMI/SMI_Aug" )

# # Sep
# Yield_Covariates$SMI_Sep6 <- relevel(cut(Yield_Covariates$SMI_Sep, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1), 
#                                  labels = c("svr drght","mdrt drght","abnrml dry", "nrml",
#                                             "abnrml wt" ,"abndnt wt", "svr wt")), ref= "nrml") 
# head(Yield_Covariates$SMI_Sep6)
# table(Yield_Covariates$SMI_Sep6)
# table <- table(Yield_Covariates$SMI_Sep6,Yield_Covariates$year  )
# write.csv(table, "./figures/figures_exploratory/SMI/SMI_Sep6" )
# 
# # Oct
# Yield_Covariates$SMI_Oct6 <- relevel(cut(Yield_Covariates$SMI_Oct, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1), 
#                                  labels = c("svr drght","mdrt drght","abnrml dry", "nrml",
#                                             "abnrml wt" ,"abndnt wt", "svr wt")), ref= "nrml") 
# 
# head(Yield_Covariates$SMI_Oct6 )
# table(Yield_Covariates$SMI_Oct6 )
# table <- table(Yield_Covariates$SMI_Oct6,Yield_Covariates$year  )
# write.csv(table, "./figures/figures_exploratory/SMI/SMI_Oct6" )
# 


# 
# ################
# ## Delete NAs ##
# ################
# sum(is.na(Yield_Covariates) )
# dim(Yield_Covariates)
# Yield_Covariates_nna <- na.omit(Yield_Covariates) 
# dim(Yield_Covariates_nna)
# 
# ## Check for NAs
# any(is.na(Yield_Covariates_nna))
# ## Reset Rownames
# rownames(Yield_Covariates_nna) <- NULL
# 
# ## Further work with DataFrame without Yield_Covariates index ##
# Yield_Covariates <- Yield_Covariates_nna

# ######################
# ## Scale Covariates ##
# ######################
# Yield_Covariates_unscaled <- Yield_Covariates
# 
# summary(Yield_Covariates_unscaled$Prec)
# summary(Yield_Covariates_unscaled$Tavg)
# summary(Yield_Covariates_unscaled$PET)
# 
# mean(Yield_Covariates_unscaled$Prec)
# mean(Yield_Covariates_unscaled$Tavg)
# mean(Yield_Covariates_unscaled$PET)
# 
# 
# sd(Yield_Covariates_unscaled$Prec)
# sd(Yield_Covariates_unscaled$Tavg)
# sd(Yield_Covariates_unscaled$PET)
# 
# head(Yield_Covariates)[,7:length(Yield_Covariates)]
# head(Yield_Covariates)[,7: (length(Yield_Covariates)-2) ]
# Yield_Covariates_norm <- Yield_Covariates[,7: (length(Yield_Covariates)-2) ]
# head(Yield_Covariates_norm)
# scale <- scale(Yield_Covariates_norm)
# summary(Yield_Covariates_norm)
# summary(scale)
# 
# Yield_Covariates[,7: (length(Yield_Covariates)-2) ] <- scale
# head(Yield_Covariates)
# sd(Yield_Covariates_norm$Prec)
# sd(Yield_Covariates_norm$Tavg)
# sd(Yield_Covariates_norm$PET)


##############################################################################################################
## Remove comIds with less than 7 observation / more than one observations missing to avoid leveage issues ## 
############################################################################################################

############################################
## First delete all observations with NAs ##
Yield_Covariates <- na.omit(Yield_Covariates)
length(unique(Yield_Covariates$comId)) # 365

#####################################################
## Delete all comIds with less than 9 observations ##
table(Yield_Covariates$comId)
sum(table(Yield_Covariates$comId) < 17) # 103
table(Yield_Covariates$comId) < 17 

## comIds mit weniger als 9 Beoachtungen: ##
# list <- c(3101, 3102, 3402, 5111 , 5112 , 5113 , 5114, 5117, 5124, 5314,
#           5315, 5334,5378,  5512, 5911,   5916,  7131,  7133, 7135, 7233, 
#           7331, 7332,7334, 7335, 7337,    7338, 7339,   8111,12052, 14612, 16052 ) ## list for <9 

## comIds with at least one observation missing ##
list <- c( 3101, 3102, 3103, 3153, 3154, 3157, 3158,  
           3402, 3404, 
           5111, 5112, 5113, 5114, 5117, 5119, 5124, 5314, 5315, 5316, 5334, 5378,  5512, 5515 ,
           5711, 5911, 5913, 5914, 5916, 5954,  6411, 6412, 6413, 
           6438, 6611, 7131, 7132, 7133, 7134, 7135, 7137, 7138, 7140, 7141, 7143, 
           7233,  7331, 7332, 7333, 7334, 7335, 7337, 7338, 7339, 7340, 8111, 8115, 8117, 8135,  8215, 8216, 
           8235,  8315, 8316, 8317,  8326, 8327,  8336, 8415, 8416,  8435 ,
           9461,  
           12052, 12053, 13051, 13052, 13053, 13054, 13055, 13056, 13057, 13058, 13059, 13060, 13061, 
           13062, 14511, 14612, 14628, 14713, 15001, 15082, 15083, 15084, 15085, 15086, 15087, 15088, 15089, 15091, 16051, 16052, 16056, 
           16072)
length(list)
list[[1]]

## Look at comIds with missing depdent data #
i <- NULL
for (i in 1:length(list)){
print(Yield_Covariates[Yield_Covariates$comId == list[[i]],1:6]) # 2008 hier scheint nur ein Jahr ein Messfehler zu sein: diesen Lösche ich 
}
' Das Fehlen der Dateneinträge schein meistens systematisch zu sein, da es meisten Blöcke sind, welche fehlen'

## Delete comIds with less than nine values
temp <- Yield_Covariates
for (i in 1:length(list))
{
  print(Yield_Covariates[Yield_Covariates$comId==list[i],])
  temp <- (temp[!temp$comId==list[i],])
}


## Number of deleted rows ##
dim(temp) - dim(Yield_Covariates)
head(temp)
length(unique(temp$comId)) # 334
dim(temp)

## Further use old name for data.frame
Yield_Covariates <- temp

length(unique(Yield_Covariates$comId)) # 334 /262 (only full comIds)

################################
## Befehle nach jedem löschen ##
rownames(Yield_Covariates) <- NULL
Yield_Covariates <- plm.data(Yield_Covariates, index=c("comId", "year"))
Yield_Covariates[,c("comId","year")] <- lapply(Yield_Covariates[,c("comId","year")], factor )
str(Yield_Covariates)

#################################################
#### Remove log trend of indepedent variable ####
#################################################
'Fit log of yield on log of time and use the residuals of that for yields'
logtrend <- lm(log(siloMaize) ~ log(as.integer(year)), data = Yield_Covariates)
summary(logtrend)



##########################
## Issue with Outliers ###
##########################
par(mfrow = c(2,2))
plot(logtrend) 

## Look at Outliers Values ##
Yield_Covariates[c(1785, 4485, 4516),]

## Look at other values of outliers com #
Yield_Covariates[Yield_Covariates$comId == "6532",] # 2008 hier scheint nur ein Jahr ein Messfehler zu sein: diesen Lösche ich 
Yield_Covariates[Yield_Covariates$comId == "12067",] # 2006 verändere ich nicht 
Yield_Covariates[Yield_Covariates$comId == "12069",] # 2003 verändere ich nicht 



## Delete outliers ##
' Da sich die Patterns bei den BIC Vergleichen nicht ändert, kümmere ich micht nicht weiter um die Outlier. 
Ich nehme nur sehr offensichtliche Messfehler raus. Wenn ich aber ein balanced panel möchte, dann kann ich das nicht mehr machen. W'
# Yield_Covariates <- Yield_Covariates[!(Yield_Covariates$comId == "6532" & Yield_Covariates$year == "2008"),]

Yield_Covariates <- na.omit(Yield_Covariates)
rownames(Yield_Covariates) <- NULL


#################################################
#### Remove log trend of indepedent variable ####
logtrend <- lm(log(siloMaize) ~ log(as.integer(year)), data= Yield_Covariates[1:17,])
summary(logtrend)
Yield_Covariates$siloMaize_logtrend <- resid(logtrend)
plot(logtrend)

' Hier stellt sich die Frage, wie ich am besten bei Projectionen umgehe. Hier schaue ich mir dann ja quasi Anomalien an, da 
ich den mean plus einen logtrend entferne'

## Retrieve average differences in crop yield for each comID ##
'Hierzu nehme ich die Differenz der Durchschnitte von meinem Referenzraum, also comId1001'



## Append on data.frame ##
Yield_Covariates$siloMaize_log_demean <- siloMaize_demean$siloMaize_log_demean

names(Yield_Covariates)

###############################################
#### Compare different silage Maize Versions ##
summary(log(Yield_Covariates$siloMaize))
summary(Yield_Covariates$siloMaize)
summary(Yield_Covariates$siloMaize_logtrend)


#######################################
## Prepare dataframe for plm package ##
'Change Indexing so that it can be used in plm package'
Yield_Covariates <- plm.data(Yield_Covariates, index=c("comId", "year"))
str(Yield_Covariates)

###########################################
## Transform comId and comIdState to factor ##
Yield_Covariates[,c("comId","comIdState", "year")] <- lapply(Yield_Covariates[,c("comId","comIdState", "year")], factor )
sapply(Yield_Covariates, class)




###################################################
##### Save Yield_Covariates extern ####
names(Yield_Covariates) <- gsub("Prec", "Pre", names(Yield_Covariates))
write.csv(Yield_Covariates, file="./data/data_raw/Yield_Covariates.csv")


# ##################################################################################################################################################################
# ################################### Explore Models ################################################################################################################
# ###################################################################################################################################################################
# 
# ###################
# ## Load Data Set ##
# # Yield_Covariates <- read.csv( file="./data/data_raw/Yield_Covariates.csv")
# # names(Yield_Covariates)
# # Yield_Covariates$X <- NULL
# 
# ###################################################################################
# ## Estimate Base Model which might be later used for prediction with plm Package ##
# ###################################################################################
# 
# formula_SMI_1_May_Aug <- 
#   siloMaize_logtrend ~ dummy(Yield_Covariates$SMI_May1, "wet" ) + dummy(Yield_Covariates$SMI_Aug1, "nrml" ) + dummy(comId)
# 
# formula_SMI_1_Jun_Aug <- 
#   siloMaize_logtrend ~ dummy(Yield_Covariates$SMI_Jun1, "wet" ) + dummy(Yield_Covariates$SMI_Aug1, "dry") + dummy(comId)
# 
# formula_SMI_6_Jun_Aug <- 
#   siloMaize_logtrend ~ dummy(Yield_Covariates$SMI_Jun6, c("svr drght","mdrt drght","abnrml dry", "abnrml wt" ,"abndnt wt", "svr wt")) +
#   dummy(Yield_Covariates$SMI_Aug6, c("svr drght","mdrt drght","abnrml dry", "abnrml wt" ,"abndnt wt", "svr wt")) + dummy(comId)
# 
# formula_SMI_6.1_Jun_Aug <- 
#   siloMaize_logtrend ~ dummy(Yield_Covariates$SMI_Jun6, c("svr drght")) +
#   dummy(Yield_Covariates$SMI_Aug6, c("svr drght","mdrt drght","abnrml dry", "abnrml wt" ,"abndnt wt", "svr wt")) + dummy(comId)
# 
# formula_polySMI_Jun_Aug <- 
#   siloMaize_logtrend ~ poly(Yield_Covariates$SMI_Jun, 3) + poly(Yield_Covariates$SMI_Aug, 3) + dummy(comId)
# 
# formula_SMI_4_Jun_Aug <- 
#   siloMaize_logtrend ~ dummy(Yield_Covariates$SMI_Jun4, c("svr drght","dry", "wt", "svr wt")) +
#   dummy(Yield_Covariates$SMI_Aug4, c("svr drght","dry", "wt", "svr wt")) + dummy(comId)
# 
# formula_SMI_4_Jun_Aug_interaction <- 
#   siloMaize_logtrend ~ dummy(Yield_Covariates$SMI_Jun4, c("svr drght","dry", "wt", "svr wt")) * dummy(Yield_Covariates$SMI_Aug4, c("svr drght","dry", "wt", "svr wt")) + dummy(comId)
# 
# #######################################
# ## Prepare dataframe for plm package ##
# 'Change Indexing so that it can be used in plm package'
# Yield_Covariates <- plm.data(Yield_Covariates, index=c("comId", "year"))
# 
# ## Transform comId and comIdState to factor ##
# Yield_Covariates[,c("comId","comIdState")] <- lapply(Yield_Covariates[,c("comId","comIdState")], factor )
# str(Yield_Covariates)
# 
# 
# ####################
# ## PLM Ergebnisse ##
# 
# ####  only one Anomaly ####
# plm.fit_SMI_1_Jun_Aug <- plm(formula = update(formula_SMI_1_Jun_Aug, .~. - dummy(comId)),  data = Yield_Covariates,  effect="individual", model=("within"), index = c("comId","year"))
# summary(plm.fit_SMI_1_Jun_Aug)
# 
# 
# #### Six Anomalies ####
# ### SMI_Jun_Aug 
# plm.fit_SMI_6_Jun_Aug <- plm(formula = update(formula_SMI_6_Jun_Aug, .~. - dummy(comId)),  data = Yield_Covariates,  effect="individual", model=("within"), index = c("comId","year"))
# summary(plm.fit_SMI_6_Jun_Aug)
# 
# ### SMI_Jun_Aug_6, Tavg
# plm.fit_SMI_6_Tavg_Jun_Aug <- plm(formula = update(formula_SMI_6_Jun_Aug, .~. - dummy(comId)+ poly(Tavg_Jul, 3,raw=T)),  data = Yield_Covariates,  effect="individual", model=("within"), index = c("comId","year"))
# summary(plm.fit_SMI_6_Tavg_Jun_Aug)
# 
# 
# ### SMI_Jun_Aug_6, Prec
# plm.fit_SMI_6.1_Prec_Jun_Aug <- plm(formula = update(formula_SMI_6.1_Jun_Aug, .~. - dummy(comId) + poly(Prec_Jul, 3, raw=T)),  data = Yield_Covariates,  effect="individual", model=("within"), index = c("comId","year"))
# summary(plm.fit_SMI_6.1_Prec_Jun_Aug)
# ' Meteorologie im July verbessert die vorhersagekraft in sample sehr. '
# 
# 
# ### SMI_Jun_Aug_6, Tavg, Prec
# plm.fit_SMI_6_TavgPrec_Jun_Aug <- plm(formula = update(formula_SMI_6_Jun_Aug, .~. - dummy(comId) + poly(Prec_Jul, 3, raw=T)+ poly(Tavg_Jul, 3,raw=T)),  data = Yield_Covariates,  effect="individual", model=("within"), index = c("comId","year"))
# summary(plm.fit_SMI_6_TavgPrec_Jun_Aug)
# # Adj. R-Squared: 0.34778
# ' Interessant ist, dass wenn man für die Meteorologie kontrolliert, die SMI wet im August an Significanz verlieren.'
# 
# #############################################
# ### Polynomials of SMI_Jun_Aug, Tavg, Prec ##
# plm.fit_polySMI_TavgPrec_Jun_Aug <- plm(formula = update(formula_polySMI_Jun_Aug , .~. - dummy(comId) + poly(Prec_Jul,4) + poly(Tavg_Jul, 4)),  data = Yield_Covariates,  effect="individual", model=("within"), index = c("comId","year"))
# summary(plm.fit_polySMI_TavgPrec_Jun_Aug)
# # Adj. R-Squared: 0.34989, Polynome 3. Grades in Prec and Tavg 
# # Adj. R-Squared: 0.35036, Polynome 4. Grades in Prec and Tavg 
# # Adj. R-Squared: 0.35595, Polynome 4. Grades in Prec and Tavg and SMI
# 
# ###############################
# ### Functions with 4 Steps ####
# ### SMI_Jun_Aug_4 Tavg, Prec
# plm.fit_SMI_4_Jun_Aug <- plm(formula = update(formula_SMI_4_Jun_Aug, .~. - dummy(comId) + poly(Prec_Jul, 3)+ poly(Tavg_Jul, 3)),  data = Yield_Covariates,  effect="individual", model=("within"), index = c("comId","year"))
# summary(plm.fit_SMI_4_Jun_Aug)
# # Adj. R-Squared: 0.34353
# ' Liefert keine wirklich schlechteren Ergebnisse. '
# 

# ###############################
# #### Interaction accros SMI ###
# ### SMI_Jun_Aug_4 Tavg, Prec, Interaction
# plm.fit_SMI_4_TavgPrec_Jun_Aug_interaction <- plm(formula = update(formula_SMI_4_Jun_Aug_interaction, .~. - dummy(comId) + poly(Prec_Jul, 3, raw=T) + poly(Tavg_Jul, 3,raw=T)),  data = Yield_Covariates,  effect="individual", model=("within"), index = c("comId","year"))
# summary(plm.fit_SMI_4_TavgPrec_Jun_Aug_interaction)
# # Adj. R-Squared: 0.34989
# 
# ### SMI_Jun_Aug_4, Interaction
# plm.fit_SMI_4_Jun_Aug_interaction <- plm(formula = update(formula_SMI_4_Jun_Aug_interaction, .~. - dummy(comId)),  data = Yield_Covariates,  effect="individual", model=("within"), index = c("comId","year"))
# summary(plm.fit_SMI_4_Jun_Aug_interaction)
# # Adj. R-Squared: 0.23835
# 
# 'Interaction Terme verbessern das R2 nicht'
# 
# 
# 
# # Adj. R-Squared: 0.34989, Polynome 3. Grades in Prec and Tavg 
# # Adj. R-Squared: 0.35036, Polynome 4. Grades in Prec and Tavg 
# # Adj. R-Squared: 0.35595, Polynome 4. Grades in Prec and Tavg and SMI
# 
#################
## Demean data ##
##########################
## (Time) - Demean Data ##
Yield_Covariates_demean <- NULL

Yield_Covariates <- as.data.frame(Yield_Covariates)
str(Yield_Covariates)
summary(Yield_Covariates)
Yield_Covariates_demean1  <- ddply(Yield_Covariates,  ~ comId,  numcolwise(scale, scale=FALSE))

# Yield_Covariates_demean2 <-   ddply(Yield_Covariates, "comId", mutate, Tavg_Jul_scale <- scale(Tavg_Jul, scale=FALSE) )

head(Yield_Covariates_demean1,18)[4]

head(Yield_Covariates)[8]
Yield_Covariates[1:18,8] - mean(Yield_Covariates[1:17,8]) 
' Da der 18. Wert nicht mehr übereinstimmt scheint dass demeanen für die einzelnen comIds zu funktionieren.'

# summary(Yield_Covariates_demean2)

summary(Yield_Covariates_demean1)

# Yield_Covariates_demean$`Yield_Covariates$year` <- NULL
summary(Yield_Covariates_demean1)#
str(Yield_Covariates_demean1)

# mean(Yield_Covariates$SMI_Aug6)

## Append Year ##
year <- Yield_Covariates$year
Yield_Covariates_demean  <- cbind(year,Yield_Covariates_demean1 )
summary(Yield_Covariates_demean)
names(Yield_Covariates_demean)

## Change Order that comId is first column
Yield_Covariates_tree_SM <- Yield_Covariates_demean[,c(2,1,3:length(names(Yield_Covariates_demean)))]
names(Yield_Covariates_tree_SM )
Yield_Covariates_demean  <- Yield_Covariates_tree_SM
names(Yield_Covariates_demean)

## Append stepwise functions ##
names(Yield_Covariates)[(length(names(Yield_Covariates))-2):(length(names(Yield_Covariates))-1)]

names(cbind(Yield_Covariates_demean, Yield_Covariates[(length(names(Yield_Covariates))-2):(length(names(Yield_Covariates))-1)]))
Yield_Covariates_demean <- cbind(Yield_Covariates_demean, Yield_Covariates[(length(names(Yield_Covariates))-2):(length(names(Yield_Covariates))-1)])
str(Yield_Covariates_demean )

## Make quadratics of Tavg_Jul and Pre_Jul ##
summary(Yield_Covariates_demean$Tavg_Jul^2)
any(Yield_Covariates_demean$Tavg_Jul^2==0.0)
Yield_Covariates_demean$Tavg_Jul_quad <- Yield_Covariates_demean$Tavg_Jul^2

summary(Yield_Covariates_demean$Pre_Jul^2)
any(Yield_Covariates_demean$Pre_Jul^2==0.0)
Yield_Covariates_demean$Pre_Jul_quad <- Yield_Covariates_demean$Pre_Jul^2

## Make cubic of Tavg_Jul and Pre_Jul ##
summary(Yield_Covariates_demean$Tavg_Jul^3)
any(Yield_Covariates_demean$Tavg_Jul^3==0.0)
Yield_Covariates_demean$Tavg_Jul_cub <- Yield_Covariates_demean$Tavg_Jul^3

summary(Yield_Covariates_demean$Pre_Jul^3)
any(Yield_Covariates_demean$Pre_Jul^3==0)
Yield_Covariates_demean$Pre_Jul_cub <- Yield_Covariates_demean$Pre_Jul^3

summary(Yield_Covariates_demean)
names(Yield_Covariates_demean)

#######################################################
## Compare results of plm, demean, and lm with dummy ##
## Without Stepwise Functions ##
plm.fit <- plm(siloMaize_logtrend ~ SMI_Jun + SMI_Aug,
               data = Yield_Covariates,  effect="individual", model=("within"), index = c("comId","year"))
' + I(Tavg_Jul) + I(Tavg_Jul^2) '
summary(plm.fit)


lm.fit_demean <- lm(siloMaize_logtrend ~  SMI_Jun + SMI_Aug, data = Yield_Covariates_demean)
summary(lm.fit_demean)
' + Tavg_Jul + Tavg_Jul_quad  '


lm.fit_dummy <- lm(siloMaize_logtrend ~ SMI_Jun + SMI_Aug + factor(comId), data = Yield_Covariates)
'+ I(Tavg_Jul) + I(Tavg_Jul^2) '
summary(lm.fit_dummy)

## LSDv mit Tavg_quad
Yield_Covariates$Tavg_Jul_quad <- Yield_Covariates$Tavg_Jul^2

lm.fit_dummy_quad <- lm(siloMaize_logtrend ~ SMI_Jun + SMI_Aug+ factor(comId), data = Yield_Covariates)
' + Tavg_Jul + Tavg_Jul_quad '
summary(lm.fit_dummy)

coefficients(lm.fit_dummy)[1:5]
coefficients(plm.fit)[1:4]
coefficients(lm.fit_dummy_quad)[1:5]
coefficients(lm.fit_demean)[1:3]

'Bei linearer Konfiguration: Für die Coefficienten bekomme ich die gleichen Werte. 
Das adjusted R-square unterscheidet sich aber zwischen demean und plm.
Dummy liefert natürlich ein größeres R2, da dort die Fixed Effects explicit mit eingehen. 
Bei linearen Modellen kann man also Problemlos demeanen. Des weiteren ist der SMI 
wohl so wie er definiert ist nicht durch demeanen betroffen, da das demeaning framework nicht für die SMIs angewendet wurde 
(numcolwise berücksichtige alle Faktoren nicht), 
die Ergebnisse bei linearen Konfigurationen aber gleich sind 
Bei nichtlinearitäten: lm.fit_demean liefert andere cofficienten für die Polynome als die drei anderen Modelle. 
D.h. durch das demeanen werden die Poylnomberechnungen entscheidend verändert. '


## Nun muss ich das gleiche nochmals mit den Stepwise Functions machen
update(formula_SMI_6_Jun_Aug, .~. - dummy(comId) + poly(Prec_Jul, 3, raw=T)+ poly(Tavg_Jul, 3,raw=T))

plm.fit <- plm(siloMaize_logtrend ~ dummy(Yield_Covariates$SMI_Jun6, c("svr drght",
                                                                       "mdrt drght", "abnrml dry", "abnrml wt", "abndnt wt", "svr wt")) +
                 dummy(Yield_Covariates$SMI_Aug6, c("svr drght", "mdrt drght",
                                                    "abnrml dry", "abnrml wt", "abndnt wt", "svr wt")) +
                 poly(Prec_Jul, 3, raw =F) + poly(Tavg_Jul, 3, raw = F),
               data = Yield_Covariates,  effect="individual", model=("within"), index = c("comId","year"))
summary(plm.fit)

summary(Yield_Covariates_demean)
names(Yield_Covariates_demean)
lm.fit_demean <- lm(siloMaize_logtrend ~ dummy(Yield_Covariates_demean$SMI_Jun6, c("svr drght", "mdrt drght", "abnrml dry", "abnrml wt", "abndnt wt", "svr wt")) +
                      dummy(Yield_Covariates_demean$SMI_Aug6, c("svr drght", "mdrt drght", "abnrml dry", "abnrml wt", "abndnt wt", "svr wt")) +
                      poly(Yield_Covariates_demean$Prec_Jul, 3) + poly(Yield_Covariates_demean$Tavg_Jul), data = Yield_Covariates_demean)
summary(lm.fit_demean)


lm.fit_dummy <- lm(siloMaize_logtrend ~ dummy(Yield_Covariates$SMI_Jun6, c("svr drght",
                                                                           "mdrt drght", "abnrml dry", "abnrml wt", "abndnt wt", "svr wt")) +
                     dummy(Yield_Covariates$SMI_Aug6, c("svr drght", "mdrt drght",
                                                        "abnrml dry", "abnrml wt", "abndnt wt", "svr wt")) +
                     poly(Prec_Jul, 3, raw = T) + poly(Tavg_Jul, 3, raw = T)+ factor(comId), data = Yield_Covariates)
summary(lm.fit_dummy)

coefficients(lm.fit_dummy)[1:3]

coefficients(plm.fit)
coefficients(lm.fit_demean)

'Hier gibt es Probleme mit den Polynomials. Das liegt evtl. daran, dass ich raw polynomials habe - wenn ich auf raw=F gehe wird das Problem auch nicht gelöst.
Ich habe nun entweder die möglichkeit, dass ich Versuche das Polynomial Problem zu lösen um mit demeanded Daten zu arbeiten. Das machen Urban aber auch nicht.
Alternative könnte ich mit lm und dummies arbeiten.
Welche Probleme könnte es dann hierbei geben?
Ich glaube, es wäre schlecht, wenn die Daten gefitted werden, ohne dass jede räumliche Einheit betrachtet werden. In so einem Fall würde es nämlich keinen Fixed Effect
für die Daten in einem Ort geben. Das heißt, dort würde kein demeaning stattfinden. Daher sollte ich darauf achten, dass ich stratified samples für cross-validation nehme ()
'


# #######################
# ## use caret package ##
# 
# # Establish indeces for leave out one year crossvalidation
# table(Yield_Covariates_demean$year)
# in_train <- holdout <- vector(mode = "list", length = 17)
# str(in_train)
# list <-  list("Fold01", "Fold02", "Fold03", "Fold04", "Fold05", "Fold06", "Fold07", "Fold08" ,"Fold09", "Fold10", "Fold11", "Fold12", "Fold13", "Fold14", "Fold15",
#                     "Fold16", "Fold17")
# names(list) <- c("Fold01", "Fold02", "Fold03", "Fold04", "Fold05", "Fold06", "Fold07", "Fold08" ,"Fold09", "Fold10", "Fold11", "Fold12", "Fold13", "Fold14", "Fold15",
#                     "Fold16", "Fold17")
# dim(Yield_Covariates_demean)
# 
# in_train <- holdout <- list
# 
# str(in_train)
# 
# in_train[[1]] <- as.integer(row.names(subset(Yield_Covariates_demean, year==2000)))
# 
# x<- seq(1999,2015)
# 
# head(Yield_Covariates_demean,20)
# rownames(Yield_Covariates_demean) <- NULL
# row_index <- 1:nrow(Yield_Covariates_demean)
# i <- 1
# year <- as.integer(row.names(subset(Yield_Covariates_demean, year==x[[i]])))
# year
# length(row_index[Yield_Covariates_demean$year %in% x[1] ])
# length(row_index[!(Yield_Covariates_demean$year %in% x[1] )])
# 
# for(i in 1:17)
#   {
#   
# in_train[[i]] <- row_index[!(Yield_Covariates_demean$year %in% x[i])]
# holdout[[i]] <- row_index[Yield_Covariates_demean$year %in% x[i] ]
# 
# print(length(in_train[[i]]) + length(holdout[[i]]))
# print(length(in_train[[i]]) )
# print(length(holdout[[i]]) )
# }
# 
# 
# 
# Yield_Covariates_demean_ordered  <- Yield_Covariates_demean[order(Yield_Covariates_demean$year),]
# head(Yield_Covariates_demean_ordered)
# Yield_Covariates_demean <- Yield_Covariates_demean_ordered 
# dim(Yield_Covariates_demean)
# 
# ctrl <- trainControl(method = "cv",
#                      savePredictions = TRUE,
#                      index = holdout,
#                      indexOut = in_train)
# 
# # train_control <-  trainControl(method="repeatedcv", number=3, repeats=3)
# # 
# # stateCvFoldsIN <- createFolds(C(Yield_Covariates_demean$year), k = 3, returnTrain=TRUE)
# 
# table(Yield_Covariates_demean$year)
# 
# str(  stateCvFoldsIN )
# y <- Yield_Covariates_demean$year
# 
# sapply(stateCvFoldsIN, length)
# 
# sapply(stateCvFoldsIN, function(i) table(y[i]))
# 
# head(Yield_Covariates_demean, 34)[,1:5]
# 
# names(Yield_Covariates)
# model_lm.fit_demean <- train(siloMaize_logtrend ~ dummy(Yield_Covariates$SMI_Jun6, c("svr drght",  "mdrt drght", "abnrml dry", "abnrml wt", "abndnt wt", "svr wt")) + 
#                                dummy(Yield_Covariates$SMI_Aug6, c("svr drght", "mdrt drght", "abnrml dry", "abnrml wt", "abndnt wt", "svr wt")) + 
#                                poly(Yield_Covariates$Prec_Jul,2 , raw = T) + poly(Yield_Covariates$Tavg_Jul, 2, raw = T) + factor(Yield_Covariates$comId) , data = Yield_Covariates,trControl=ctrl,method="lm")
# 'Hier gibt es Probleme, ich habe aber gerade keine Zeit, mich darum zu kümmern.'
# 
# print(model_lm.fit_demean)
# warnings()
# 
# first_fold <- subset(model_lm.fit_demean$pred, Resample == "Fold17")
# 
# table(model_lm.fit_demean$pred$Resample)
# 
# ## These were used to fit the model
# table(Yield_Covariates_demean$year[-first_fold$rowIndex])
# ## These were heldout:
# table(Yield_Covariates_demean$year[first_fold$rowIndex])
# 
# model_lm.fit_demean$finalModel

###############################################################################################################################################################################
###############################################################################################################################################################################
######################
## Make Predictions ##
######################



##############################
## Prepare Yield_Covariates ##
'Here I include the model.matrix for the comID exclusively since the  predict command had issued to deal with those. 
Also, only the data used in the models are considered further.'

## Align names with future data ##
names(Yield_Covariates)


# names(Yield_Covariates)%in%c("year","comId", "siloMaize_logtrend", "SMI_Jun6", "SMI_Aug6", "Tavg_Jul", "Pre_Jul")
# sum(names(Yield_Covariates)%in%c("year","comId", "siloMaize_logtrend", "SMI_Jun6", "SMI_Aug6", "Tavg_Jul", "Pre_Jul"))
## Chose variables used for model ##
Yield_Covariates_short <- as.data.frame(Yield_Covariates[,names(Yield_Covariates)%in%c("year","comId", "siloMaize_logtrend", 
                                                                                       "SMI_Jun6", "SMI_Aug6", "Tavg_Jul",
                                                                                       "Pre_Jul")])
names(Yield_Covariates_short)
# str(Yield_Covariates_short)
# levels(Yield_Covariates_short$comId)

## Retrieve bias correction: comId specific mean on log detrended siloMaize)
bias_correction <- ddply(Yield_Covariates, c("comId"), summarise,
               mean = mean(siloMaize_logtrend)) 

## Append model.matrix explicitly ##
modelmatrix <- model.matrix(~ Yield_Covariates_short$comId)
dim(modelmatrix)

modelmatrix_Df <- as.data.frame((modelmatrix))
rm(modelmatrix)

## Cbind modelmatrix with short data.frame ##
Yield_Covariates_long <- cbind(Yield_Covariates_short, modelmatrix_Df)
Yield_Covariates_long$`(Intercept)` <- NULL


## Aling names for comIds
x <-make.names(names(Yield_Covariates_long))
str(x)
colnames <- gsub("Yield_Covariates_short.", "", x)
colnames(Yield_Covariates_long) <- colnames
head(Yield_Covariates_long)

######################################
### Fit model used for prediction ###

## fit model on data.frame with explizit model.matrix for comId
names(Yield_Covariates_long)
Yield_Covariates_long$year <- NULL
Yield_Covariates_long$comId <- NULL

lm.fit_SMI_6_Jun_Aug_modelmatrix <- lm(siloMaize_logtrend ~ I(Tavg_Jul^2) + I(Tavg_Jul^3)  +I(Pre_Jul^2) +  I(Pre_Jul^3) + .   ,  data = Yield_Covariates_long)
#  I(Tavg_Jul^2) + I(Tavg_Jul^3)  +I(Pre_Jul^2) +  I(Pre_Jul^3) +
summary(lm.fit_SMI_6_Jun_Aug_modelmatrix)

##########################################
## Generate frame of comIds to merge on ##
ComIdMerge <- as.data.frame(unique(Yield_Covariates$comId))
ComIdMerge 
colnames(ComIdMerge ) <- "comId"

# generate container to store predicted values #
predictData <- NULL
predictData <- ComIdMerge 
head(predictData)

#######################################
#### Load Projections of one Model ####

' Ab hier würde über die einzelenen Modelle geloopt'


namelist2 <- c("DMI","ICTP","KNMI","MPI","SMHIRCA")

for (i in 1:5){
NewValues <-  read.csv( paste("./data/data_proj/", namelist2[[i]],"_proj_MetVar",".csv", sep=""))
names(NewValues)  
NewValues$X <- NULL
unique(NewValues$year)
dim(NewValues)
str(NewValues)

####################################
## Stepwise with six anomalies ##
# June
NewValues$SMI_Jun6 <- relevel(cut(NewValues$SMI_Jun, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1), 
                                  labels = c("svr drght","mdrt drght","abnrml dry", "nrml",
                                             "abnrml wt" ,"abndnt wt", "svr wt")), ref= "nrml") 



# August
NewValues$SMI_Aug6 <- relevel(cut(NewValues$SMI_Aug, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1),
                                  labels = c("svr drght","mdrt drght","abnrml dry", "nrml",
                                             "abnrml wt" ,"abndnt wt", "svr wt")), ref= "nrml") 


str(NewValues)

## Ab hier loop über die Jahre 1999 - 2099 der prediction ##
listyear <- seq (1951, 2099)
length(listyear)
listyear[149]

###################################
## Make dataframe of projections ##
for (j  in 1:149){


print(listyear[[j]])
NewValuesyear <- NewValues[NewValues$year == listyear[[j]], ]
str(NewValuesyear)
dim(NewValues)


#####################################################
## Merge with Yield Data Frame to get same comIds ##
NewValues_merge <- merge(NewValuesyear, ComIdMerge ,  by="comId")
str(NewValues_merge)
dim(NewValues_merge)

###########################################
## Generate Factors for comIds and years ##
NewValues_merge[,c("comId","year")] <- lapply(NewValues_merge[,c("comId","year")], factor )

###########################################
#### Prepare Dataframe for Prediction ####
#########################################
NewValues_short <- as.data.frame(NewValues_merge[,names(NewValues_merge)%in%c("year","comId", "siloMaize_logtrend", 
                                                                              "SMI_Jun6", "SMI_Aug6", "Tavg_Jul", "Pre_Jul")])
names(NewValues_short)

dim(NewValues_short)

###########################################
## Generate Factors for comIds and years ##
NewValues_short[,c("comId","year")] <- lapply(NewValues_short[,c("comId","year")], factor )


#########################
## Model.Natrix comId ##

## Generate model.matrix from data.frame ##
modelmatrix <- model.matrix(~ NewValues_short$comId)
modelmatrix_Df <- as.data.frame((modelmatrix))
rm(modelmatrix)

## Use Cbind to generate Dataframe that includes modelmatrix ##
NewValues_long <- cbind(NewValues_short, modelmatrix_Df)
NewValues_long$`(Intercept)` <- NULL

## Adapt names 
x <- make.names(names(NewValues_long))

colnames <- gsub("NewValues_short.", "", x)
colnames
colnames(NewValues_long) <- colnames


# table(mergeComId)


##########################################################
NewValues_long$year<- NULL
predictlm <- as.data.frame(predict.lm(lm.fit_SMI_6_Jun_Aug_modelmatrix , newdata = NewValues_long))

predictlm[1,1]

summary(predictlm)

#### Clear Prediction for Bias ()
predictlm_bias_corrected <- predictlm - bias_correction
summary(predictlm_bias_corrected)
# summary(lm.fit_SMI_6_Jun_Aug_modelmatrix)

#### Manual Prediction for com1001 ####
# coef(lm.fit_SMI_6_Jun_Aug_modelmatrix)[1:21]
# NewValues_long[1,1:21]
# 
# coef(lm.fit_SMI_6_Jun_Aug_modelmatrix)[[1]]*1 +  #'Intercept'
#   coef(lm.fit_SMI_6_Jun_Aug_modelmatrix)[[2]] *  NewValues_long[1,2]^2 + #'I(Tavg_Jul^2)'
#   coef(lm.fit_SMI_6_Jun_Aug_modelmatrix)[[3]] *  NewValues_long[1,2]^3 + #'I(Tavg_Jul^3)'
#   coef(lm.fit_SMI_6_Jun_Aug_modelmatrix)[[7]] *  NewValues_long[1,2] + #'Tavg_Jul'
#   coef(lm.fit_SMI_6_Jun_Aug_modelmatrix)[[4]] *  NewValues_long[1,3]^2 + #'I(Pre_Jul^2)'
#   coef(lm.fit_SMI_6_Jun_Aug_modelmatrix)[[5]] *  NewValues_long[1,3]^3 + #'I(Pre_Jul^3) '
#   coef(lm.fit_SMI_6_Jun_Aug_modelmatrix)[[6]] *  NewValues_long[1,3] + #'Pre_Jul'
#   coef(lm.fit_SMI_6_Jun_Aug_modelmatrix)[[8]] *  0 + #'SMI_Jun6svr drght '
#   coef(lm.fit_SMI_6_Jun_Aug_modelmatrix)[[9]] *  0 + #'SMI_Jun6mdrt drght '
#   coef(lm.fit_SMI_6_Jun_Aug_modelmatrix)[[10]] * 0 + #'SMI_Jun6abnrml dry '
#   coef(lm.fit_SMI_6_Jun_Aug_modelmatrix)[[11]] * 0 +  #'SMI_Jun6abnrml wt'
#   coef(lm.fit_SMI_6_Jun_Aug_modelmatrix)[[12]] * 0 + #'SMI_Jun6abndnt wt'
#   coef(lm.fit_SMI_6_Jun_Aug_modelmatrix)[[13]] * 0 + #'SMI_Jun6svr wt'
#   coef(lm.fit_SMI_6_Jun_Aug_modelmatrix)[[14]] * 0 + #'SMI_Aug6svr drght'
#   coef(lm.fit_SMI_6_Jun_Aug_modelmatrix)[[15]] * 0 + #'SMI_Aug6mdrt drght'
#   coef(lm.fit_SMI_6_Jun_Aug_modelmatrix)[[16]] * 0 + #'SMI_Aug6abnrml dry'
#   coef(lm.fit_SMI_6_Jun_Aug_modelmatrix)[[17]] * 1 + #'SMI_Aug6abnrml wt'
#   coef(lm.fit_SMI_6_Jun_Aug_modelmatrix)[[18]] * 0 + #'SMI_Aug6abndnt wt'
#   coef(lm.fit_SMI_6_Jun_Aug_modelmatrix)[[19]] * 0 #  'SMI_Aug6svrwt' 
# 
# 
##### 


# str(predictlm)
# demeanPredictlm <- predictlm$`predict.lm(lm.fit_SMI_6_Jun_Aug_modelmatrix, newdata = NewValues_long)` - Demean$FE
# 
# predictlm <- cbind(predictlm, demeanPredictlm)
# 
# summary(predictlm)
# summary(Demean)
# dim(Demean)
# dim(predictlm)
# plot(seq(1,262,1),Demean$FE)
# plot(seq(1,262,1), predictlm$`predict.lm(lm.fit_SMI_6_Jun_Aug_modelmatrix, newdata = NewValues_long)`)
# plot(seq(1,262,1), predictlm$demeanPredictlm)
# head(NewValues_long)
# dim(predictlm)
names(predictlm_bias_corrected) <-paste(namelist2[[i]], listyear[j], sep="")


predictData  <- cbind(predictData , predictlm_bias_corrected )

print(head(predictData ))
}
}


names(predictData )

write.csv(predictData, "./data/data_processed/predictData_JunSMI6JulPoly3TavgPreAugSMI6_biasCorrected_1951-2099.csv")


######################################
#### Plot Maps of predicted data ####
####################################

#### Read in Predicted Data ####
predictData <- read.csv("./data/data_processed/predictData_JunSMI6JulPoly3TavgPreAugSMI6_biasCorrected.csv")
predictData$X  <- NULL
str(predictData) # 262 obs

#### Read in Spatial Data Frame with Spatial Reference from shape file ####
KreisPolOGR <- readOGR("/Storage/ownCloud/Home/Klimabuero/Proj1/data//data_raw/4_michael/adminitrative_borders_Ger/", "vg2500_krs")
str(KreisPolOGR,2)
KreisPolOGR@data$RS

names(KreisPolOGR) <- c("USE"   ,     "comId"    ,     "GEN"    ,    "SHAPE_LENG", "SHAPE_AREA")
names(KreisPolOGR)

KreisPolOGR@data$comId <- as.factor(as.numeric(str_sub(KreisPolOGR@data$comId,1,5)))

## Make data.frame with comIds only to merge ##
KreisPolOGR_merge <- as.data.frame(KreisPolOGR@data$comId)
KreisPolOGR_merge

str(KreisPolOGR_merge)
names(KreisPolOGR_merge) <- "comId"


#### Change order of KreisPolOGR_order ####
str(KreisPolOGR,2)

KreisPolOGRordered <- KreisPolOGR[order(KreisPolOGR$comId),]
rownames(KreisPolOGRordered)

KreisPolOGRordered$comId

rownames(KreisPolOGRordered@data) <- 0:411


### Merge comId Vector of KreisPolOGR  (KreisPolOGR_merge) and PredictData_train to get same number of rows (412) ####
predictData_comId <- merge(KreisPolOGR_merge, predictData, by="comId", all.x=T)
predictData_comId$comId

predictData_comId[1:20,1:10]

#### Make SpatialDataFrame for maps ####
rownames(predictData_comId) <- 0:411


predictData_comId_sp <- NULL
predictData_comId$comId <- NULL
predictData_comId_sp <- spCbind(KreisPolOGRordered, predictData_comId)
names(predictData_comId_sp)


#### Modify trellis theme for plotting ####
my.theme = trellis.par.get()
names(my.theme)
my.theme$panel.background

trellis.par.set("background", list(col = "white"))
trellis.par.set("panel.background", list(col = "white"))
trellis.par.set("strip.background", list(col = "white"))
trellis.par.set("fontsize", list(text=15, points=10))

my.theme$strip.background
my.theme$axis.line
my.theme$strip.border
# my.theme$strip.border$col <- c("#000000", "#000000","#000000", "#000000", "#000000", "#000000","#000000")

show.settings()


#### Set color scheme for plots ####
at=(seq(-0.4, 0.4, 0.05))
length(at)

cs1 <- colorRampPalette(c('#6d3f07','#bf812d','#dfc27d','#f6e8c3','#c7eae5','#35978f','#003c30','#002c3b'))(17)

#########################################################################
#### Loop over all models and years to produce maps of predictions ####
######################################################################
namelist2 <- c("DMI","ICTP","KNMI","MPI","SMHIRCA")
listyear <- seq (1999, 2099)

# i = 1; j = 1
  
zcol<- NULL
 
for(j in 1:101){
for (i in 1:5){
  zcol <- c(zcol,paste(namelist2[[i]], listyear[[j]], sep=""))}
zcol
plot <- spplot(predictData_comId_sp, zcol, at=at, col.regions= cs1)

pdf(paste("./figures/figures_proj/JunSMI6JulPol3PreTavgAugSMI6_biasCorrected/", listyear[[j]],".pdf", sep=""))
print(plot )
dev.off()

zcol<-NULL
}




###########################################
#### Make time series plots for model ####
# predictData <- read.csv("./data/data_processed/predictData_JunSMI6AugSMI6.csv")
predictData <- read.csv("./data/data_processed/predictData_JunSMI6JulPoly3TavgPreAugSMI6_biasCorrected.csv")
predictData$X <- NULL

summary(predictData)

names(predictData)[c(1,50:150, 199:299, 348:448, 497:597, 646:746)]
predictData <- predictData[, c(1,507:1251)]
dim(predictData)

predictData_19992099 <- predictData[,c(1,50:150, 199:299, 348:448, 497:597, 646:746)] 
names(predictData_19992099)

predictData_20492099 <- predictData[,c(1,100:150, 249:299, 398:448, 547:597, 696:746)] 
names(predictData_20492099)

for (l in 1:262){
comId <- predictData_20492099[l, 1]
time <-  predictData_20492099[l, 2:length(predictData_20492099)]
time <-  stack(time)
time
dim(time)
time$ind <- NULL

years <- rep(seq(2049,2099), 5)

head(time)
dim(time)
length(years)

time <- cbind(years, time)

DMI <- as.data.frame(rep("DMI", 51))
ICTP <-  as.data.frame(rep("ICTP", 51))
KNMI <- as.data.frame( rep("KNMI",51))
MPI <- as.data.frame( rep("MPI", 51))
SMHIRCA <-  as.data.frame(rep ("SMIHIRCA", 51))

names(DMI) <- names(ICTP) <- names(KNMI) <- names(MPI) <- names(SMHIRCA) <- "Model"

model <- rbind(DMI, ICTP, KNMI, MPI, SMHIRCA)

time <- cbind(model, time)

time$years <- as.numeric(time$years)

timeseries <- ggplot(time, aes(years, values)) + 
  ylim(-0.5, 0.5) +
  geom_line() + facet_wrap(~Model) + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle(paste(comId))

ggsave(paste("./figures/figures_proj/JunSMI6JulPol3PreTavgAugSMI6_biasCorrected_2049-2099/timeseries", comId[[1]],".pdf", sep=""), timeseries, device = "pdf", width=6, height=6 )
}

###########################################################################################################################################################
###########################################################################################################################################################
################ Predict on data which where used to train the model (one prediction for each year)  to make comparision ##################################
###########################################################################################################################################################
###########################################################################################################################################################

########################################
#### Fit model used for Predictions ####
' Make fits for various versions to figure out why there is a pattern in the predicted data'

names(Yield_Covariates)
lm.fit_SMI_6_Jun_Aug_modelmatrix <- lm(siloMaize_logtrend ~ Tavg_Jul * Pre_Jul+ I(Tavg_Jul^2)* I(Pre_Jul^2)  + SMI_Jun6 + SMI_Aug6 + comId,
                                       data = Yield_Covariates)
#   + I(Tavg_Jul^2) +  I(Tavg_Jul^3) + I(Pre_Jul^2) +  I(Pre_Jul^3)

summary(lm.fit_SMI_6_Jun_Aug_modelmatrix)

coef(lm.fit_SMI_6_Jun_Aug_modelmatrix)

##########################################################################################################################
#### Correct predicted data for locatioanl bias, i.e. the comparative deviation (average yield) of a particular site ####
########################################################################################################################

## Retrieve mean of log_silomaize for each comID ##

means <- ddply(Yield_Covariates, c("comId"), summarise,
                mean = mean(siloMaize_logtrend)) 
bias_correction <- means

# rownames(predict_year) <- NULL

# rownames(bias_correction)

# bias_correction$comId <- NULL

# str(predict_year)

# str(bias_correction)

#### Loop thorugh 1999 to 2015 to make predictions for each year ####
listyear <- seq(1999, 2015)

predictData_train <- NULL
predictData_train_biasCorrected <- NULL
predictData_train <- list(1:262)
predictData_train_biasCorrected  <- list(1:262)

for(j in 1:17){
Yield_Covariates_year <- Yield_Covariates[Yield_Covariates$year == listyear[[j]], ]
dim(Yield_Covariates_year)
length(predict(lm.fit_SMI_6_Jun_Aug_modelmatrix, newdata=Yield_Covariates_year ))

predict_year <- as.data.frame(predict(lm.fit_SMI_6_Jun_Aug_modelmatrix, newdata=Yield_Covariates_year ))
predict_year_bias_corrected <- predict_year - bias_correction

names(predict_year) <-  listyear[[j]] 
names(predict_year_bias_corrected) <- paste(listyear[[j]],"biasCorrected",sep="_")

predictData_train <- cbind(predictData_train, predict_year)
predictData_train_biasCorrected <- cbind(predictData_train_biasCorrected, predict_year_bias_corrected)

}

dim(predictData_train)
dim(predictData_train_biasCorrected)
str(predictData_train_biasCorrected)
str(predictData_train)

##########################################
#### Adapt data.frame with prediction ####
dim(Yield_Covariates_year[1])

## Prediction without correction
predictData_train <- cbind(Yield_Covariates_year[1], predictData_train)

rownames(predictData_train) <- NULL
predictData_train$`1:262` <- NULL
head(predictData_train, 15)

## Prediction with correction
predictData_train_biasCorrected <- cbind(Yield_Covariates_year[1], predictData_train_biasCorrected)

rownames(predictData_train_biasCorrected) <- NULL
predictData_train_biasCorrected$`1:262` <- NULL

## To simpliify procedure use predictData_train as names ##
predictData_train <- predictData_train_biasCorrected


###################################
#### Make maps of predictions ####
#################################


#### Read in Spatial Data Frame with Spatial Reference from shape file ####
KreisPolOGR <- readOGR("/Storage/ownCloud/Home/Klimabuero/Proj1/data//data_raw/4_michael/adminitrative_borders_Ger/", "vg2500_krs")
str(KreisPolOGR,2)
KreisPolOGR@data$RS

names(KreisPolOGR) <- c("USE"   ,     "comId"    ,     "GEN"    ,    "SHAPE_LENG", "SHAPE_AREA")
names(KreisPolOGR)

KreisPolOGR@data$comId <- as.factor(as.numeric(str_sub(KreisPolOGR@data$comId,1,5)))

## Make data.frame with comIds only to merge ##
KreisPolOGR_merge <- as.data.frame(KreisPolOGR@data$comId)
KreisPolOGR_merge

str(KreisPolOGR_merge)
names(KreisPolOGR_merge) <- "comId"

#### Change order of KreisPolOGR_order ####
str(KreisPolOGR,2)

KreisPolOGRordered <- KreisPolOGR[order(KreisPolOGR$comId),]
rownames(KreisPolOGRordered@data) <- 0:411

### Merge comId Vector of KreisPolOGR  (KreisPolOGR_merge) and PredictData_train to get same number of rows (412) ####
predictData_comId <- merge(KreisPolOGR_merge, predictData_train, by="comId", all.x=T)
predictData_comId$comId

all(KreisPolOGRordered$comId==predictData_comId$comId)

#### Make SpatialDataFrame for maps ####
rownames(predictData_comId) <- 0:411


predictData_comId_sp <- NULL
predictData_comId$comId <- NULL
predictData_comId_sp <- spCbind(KreisPolOGRordered, predictData_comId)
names(predictData_comId_sp)


## Modify trellis theme ##
my.theme = trellis.par.get()
names(my.theme)
my.theme$panel.background

trellis.par.set("background", list(col = "white"))
trellis.par.set("panel.background", list(col = "white"))
trellis.par.set("strip.background", list(col = "white"))
trellis.par.set("fontsize", list(text=15, points=10))

my.theme$strip.background
my.theme$axis.line
my.theme$strip.border
# my.theme$strip.border$col <- c("#000000", "#000000","#000000", "#000000", "#000000", "#000000","#000000")

show.settings()


#### Set color scheme for plots ####
summary(predictData_comId_sp)
at=(seq(-0.4, 0.4, 0.05))
length(at)

cs1 <- colorRampPalette(c('#6d3f07','#bf812d','#dfc27d','#f6e8c3','#c7eae5','#35978f','#003c30','#002c3b'))(17)
# '#543005','#8c510a',
#### Loop to make Maps ####
names(predictData_comId_sp) 
namelist3 <-names(predictData_comId_sp)[6:22]  

for(n in 1:17){
  plot <- spplot(predictData_comId_sp, namelist3[[n]], at=at, col.regions=cs1, main = namelist3[[n]] )
  
  plot
  pdf(paste("./figures/figures_proj/Train_data/PredTrainData_polTavgPreJul_SMI6JunAug", namelist3[[n]],".pdf", sep=""))
  print(plot )
  dev.off()

}



