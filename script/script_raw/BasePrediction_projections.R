#######################################################################
## Base Model for predictions of Soil Moisture Effects on Crop Yield ##
#######################################################################
#### Description of Script ####
' 
Script to make climate projectinos for the period 1951 - 2099 for each RCM () and preidciton model (SMI June and August vs July only)




'
#### Output ####
## Files
'
  - yieldData_meteo (data.frame after Preparation scheme described above) -> /Proj2/data/data_processed/
'
## Plots
''

## Descriptive Statistics of MeteoVar
''

#### Dependencies and Input ####
' Maize_meteo:"./data/data_processed/Maize_meteo_step_cut9_anomaly.csv" from KlimaMeteo_netcdfTo_sf&tidy

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
library(stringr)
library(classInt)
library(RColorBrewer)
library(stargazer)
library(ggthemes)
library(caret)   
library(plyr)
library(sf)
library(tidyverse)
library(ggplot2)  
library(grDevices)
##############################################################################################################################################################################

###################################################
##### Read Maize_meteo_step_cut9_anomaly.csv  ####
#################################################
Maize_meteo <- read.csv( file="./data/data_processed/Maize_meteo_step_cut9_anomaly.csv")
str(Maize_meteo)
Maize_meteo$comId <- as.factor(Maize_meteo$comId)
levels(Maize_meteo$comId)
Maize_meteo$X <- NULL

#################################################
#### Load shape of administrative districts ####
###############################################
vg2500_krs <- read_sf("./../Proj1/data/data_spatial/", "vg2500_krs")
str(vg2500_krs, 2)

#### Change RS to five digits #####
vg2500_krs$RS <- as.integer(str_sub(vg2500_krs$RS, 1,5))
vg2500_krs$RS

#################################
#### Load AnomalyCorrection ####
###############################
anomaly_correction <- read.csv( file="./data/data_processed/AnomalyCorrection.csv")
anomaly_correction$X <- NULL
anomaly_correction$comId <- as.factor(anomaly_correction$comId)
str(anomaly_correction)

##############################################################################################################################################################################
##############################################################################################################################################################################
#### Climate Projections #####
##############################

#########################################################################################
#### Preparations to make predictions for the data derived from five climate models ####
#######################################################################################
' Since I am not using the same data on which the model is trained I need to implement some changes. 
  In particular I cannot work with factor(comId) but need to derive the model matrix to be able to use predict().'


# #######################################################
# #### Read in Climate Data with Stepwise Funcktions ####
# 
# yieldData_meteo <- read.csv(file="./data/data_processed/yieldData_meteo")
# yieldData_meteo$X <- NULL
# str(yieldData_meteo)

###################################################################
#### Create data.frame including variables used for estimation ####
Maize_meteo_short <- as.data.frame(Maize_meteo[,names(Maize_meteo)%in%c("year","comId", "siloMaize", 
                                                                          "SMI_Jun6","SMI_Jul6", "SMI_Aug6", "T_Jul",
                                                                                       "P_Jul")])
names(Maize_meteo_short)

#########################################
#### Append model.matrix explicitly ####
#######################################
'Here I include the model.matrix for the comID exclusively since the  predict command had issues to deal with those. 
Also, only the data used in the models are considered further.'

## Create model.matrix ##
modelmatrix <- model.matrix(~ Maize_meteo$comId)
dim(modelmatrix)
str(modelmatrix)

## Convert model.matrix to data.frame ##
modelmatrix_Df <-as.data.frame((modelmatrix))
str(modelmatrix_Df)
modelmatrix_Df$`Maize_meteo$comId1002` # There is a one for each year in the data when comId == 1002 is true

# ## Convert all numeric to factor ##
# modelmatrix_Df <- lapply(modelmatrix_Df, factor )

## Delte modelmatrix ##
rm(modelmatrix)

## Cbind modelmatrix with short data.frame ##
yieldData_meteo_modelmatrix <- cbind(Maize_meteo_short, modelmatrix_Df)
yieldData_meteo_modelmatrix$`(Intercept)` <- NULL

## Clean up names  ##
x <- make.names(names(yieldData_meteo_modelmatrix))
str(x)
x
colnames <- gsub("Maize_meteo.", "", x)
colnames(yieldData_meteo_modelmatrix) <- colnames
str(yieldData_meteo_modelmatrix)


########################################
#### Fit model used for prediction ####
######################################

## Delete columns year and comId ##
names(yieldData_meteo_modelmatrix)
yieldData_meteo_modelmatrix$year <- NULL
yieldData_meteo_modelmatrix$comId <- NULL


## lm.fit ##
## June to August
drops <- c(("SMI_Jul6"))
lm.fit_SMI_6_Jun_Aug_modelmatrix <- 
  lm(siloMaize ~ I(T_Jul^2) + I(T_Jul^3)  +I(P_Jul^2) +  I(P_Jul^3) + .   ,
     data = yieldData_meteo_modelmatrix[ , !(names(yieldData_meteo_modelmatrix) %in% drops)])
summary(lm.fit_SMI_6_Jun_Aug_modelmatrix) # Adjusted R-squared:   0.6857 

## July
drops <- c("SMI_Jun6", "SMI_Aug6")
lm.fit_SMI_6_Jul_modelmatrix <- 
  lm(siloMaize ~ I(T_Jul^2) + I(T_Jul^3)  +I(P_Jul^2) +  I(P_Jul^3) + .   ,
     data = yieldData_meteo_modelmatrix[ , !(names(yieldData_meteo_modelmatrix) %in% drops)])
summary(lm.fit_SMI_6_Jul_modelmatrix) # Adjusted R-squared:   0.661 

###############################################
#### Generate frame of comIds to merge on ####
#############################################
' This data.frame only includes those comIds which have complete data for silage maize. Needed for projection data.'
ComIdMerge <- as.data.frame(unique(Maize_meteo$comId))
ComIdMerge 
colnames(ComIdMerge ) <- "comId"

#### Retrieve mean(Yield) for all comIds in the period 1999 - 2015 ####


########################################################################################################
#############################################################
#### Loop for different models used to make Predictions ####
###########################################################
' it is necessary to create folder which have the names of the models in data/data_pro/output'
modelListMatrix <- list(lm.fit_SMI_6_Jun_Aug_modelmatrix, lm.fit_SMI_6_Jul_modelmatrix )
modelListMatrixNames <- list("lm.fit_SMI_6_Jun_Aug_modelmatrix", "lm.fit_SMI_6_Jul_modelmatrix")

for (s in 1:length(modelListMatrix) ){
   
  dir.create(paste("./data/data_proj/output/",modelListMatrixNames[[s]], sep="" ), showWarnings = F) # does not overwrite automatically

  ###############################################################
  #### Loop to make predictions for all five climate models ####
  #############################################################
  ' Until here I was only working with the training data, i.e. the observation from the year 1999 - 2015. 
    From here on I am using the projections derived from the climate models to make predictions'
  
  ## Create Namelist used in the models ##
  namelist_models <- c("DMI","ICTP", "KNMI","MPI","SMHI")
  
  #### Create container to store tidy.data.frames of all models ####
  predictData_anomaly_tidy_all <- data.frame()
  predictData_tidy_all <- data.frame()
  
  predictData_anomaly_tidy_july_all <- data.frame()
  predictData_tidy_july_all <- data.frame()
  
  ####################
  #### Start loop ####

  for (r in 1:5){
  
    ##################################################
    ## generate container to store predicted values ##
    predictData <- NULL
    predictData <- ComIdMerge 
    # head(predictData)
    
    predictData_anomaly  <- NULL
    predictData_anomaly  <- ComIdMerge 
    # head(predictData_anomaly )
    
    #######################################
    #### Load Projections of one Model ####  
    NewValues <-  read.csv( paste("./data/data_proj/", "MeteoMonth_df_tidy_", namelist_models[[r]],".csv", sep=""))
    names(NewValues)  
    NewValues$X <- NULL
    unique(NewValues$year) # 1951 - 2099
    dim(NewValues)
    str(NewValues)
  
    ##################################################################################
    #############################################
    ##### Prepare data.frame for prediction ####
    
    #####################################
    #### Stepwise with six anomalies ####
    # June
    NewValues$SMI_Jun6 <- relevel(cut(NewValues$SMI_Jun, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1), 
                                      labels = c("svr drght","mdrt drght","abnrml dry", "nrml",
                                                 "abnrml wt" ,"abndnt wt", "svr wt")), ref= "nrml") 
    # JuLY
    NewValues$SMI_Jul6 <- relevel(cut(NewValues$SMI_Jul, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1), 
                                      labels = c("svr drght","mdrt drght","abnrml dry", "nrml",
                                                 "abnrml wt" ,"abndnt wt", "svr wt")), ref= "nrml") 
    # August
    NewValues$SMI_Aug6 <- relevel(cut(NewValues$SMI_Aug, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1),
                                      labels = c("svr drght","mdrt drght","abnrml dry", "nrml",
                                                 "abnrml wt" ,"abndnt wt", "svr wt")), ref= "nrml") 
    #################################
    #### Choose variables needed ####
    NewValues_short <- as.data.frame(NewValues[,names(NewValues)%in%c("year","comId", "siloMaize_logtrend", 
                                                                            "SMI_Jun6","SMI_Jul6", "SMI_Aug6", "T_Jul", "P_Jul")])

    #########################################################################################################
    #### Merge with Yield Data Frame to get same comIds, i.e. those which have a least nine observations ####
    NewValues_short$comId <- as.factor(NewValues_short$comId)
    NewValues_merge <- merge(NewValues_short, ComIdMerge ,  by="comId") #
    str(NewValues_merge) # 49766/149 = 334 : passt also, da wir nur noch 334 (262) comIds übrig haben mit cutoff 9 (17)
    
    ###########################################
    ## Generate Factors for comIds and years ##
    NewValues_merge[,c("comId","year")] <- lapply(NewValues_merge[,c("comId","year")], factor )
    
    #########################################
    #### Generate Model.Matrix of ComIds ####
    modelmatrix <- model.matrix(~ NewValues_merge$comId)
    modelmatrix_Df <- as.data.frame((modelmatrix))
    dim(modelmatrix_Df)
    dim(NewValues_merge)
    rm(modelmatrix)
    
    ## Use Cbind to generate Dataframe that includes modelmatrix ##
    NewValues_modelmatrix <- cbind(NewValues_merge, modelmatrix_Df)
    NewValues_modelmatrix$`(Intercept)` <- NULL
    str(NewValues_modelmatrix)
    
    ########################
    #### Clean up names ####
    x <- make.names(names(NewValues_modelmatrix))
    x
    colnames <- gsub("NewValues_merge.", "", x)
    colnames
    colnames(NewValues_modelmatrix) <- colnames
    
    ###################################################################################################
    #######################################################################
    #### Make prediction for each year derived from the climate models ####
    " Ab hier loop über die Jahre 1951 - 2099 der Projection des jeweiligen Klimamodels"
    
    #### Define list of years to loop through ####
    listyear <- seq (1951, 2099)
    length(listyear)
    listyear[149]
    
    #### Start loop over each year derived from the climate projections ####
    for (l  in 1:149){
      print(listyear[[l]])
      
      #### Filter for year l ####
      NewValuesyear <- NewValues_modelmatrix %>% filter(NewValues_modelmatrix$year == listyear[[l]] )
      rownames(NewValuesyear) <- NULL
      str(NewValuesyear)
      dim(NewValuesyear)
      
      #### Clean variables (comId, year) not needed for prediction ####
      NewValuesyear$year <- NewValuesyear$comId <- NULL
      
      ##########################################################
      #### Make predictions ###
      head(NewValuesyear)
      str(NewValuesyear)
      
      
      predictlm <- as.data.frame(predict.lm(modelListMatrix[[s]] , newdata = NewValuesyear))
      
      summary(predictlm)
      
      #### Change name of predictlm data.frame ####
      names(predictlm) <- paste(listyear[l])
      
      #########################################
      #### Clear prediction for mean yield ####
      dim(predictlm)
      dim(anomaly_correction)
      predictlm_anomaly  <- predictlm - anomaly_correction[,2]
      summary(predictlm_anomaly )
      # summary(lm.fit_SMI_6_Jun_Aug_modelmatrix)
      
      #### Change name of predictlm_anomaly  data.frame ####
      names(predictlm_anomaly ) <- paste(listyear[l])
      
      #### Manual prediction for com1001 ####
      ' The calculation of the manual prediction can be found in Baseprediction_long. However, it made sense.'
      
      names(predictlm_anomaly ) <- paste(listyear[l], sep="")
      
      #### Combine to one large data.frame including all the years ####
      predictData           <- cbind(predictData , predictlm )
      predictData_anomaly   <- cbind(predictData_anomaly  , predictlm_anomaly  )
      
      
    } ## End of loop through all the years of one climate model ##
    
    #########################################################
    #### Save one wide data.frame for each climate model ####
    str(predictData)
    str(predictData_anomaly )
    
    names(predictData)
    names(predictData_anomaly )
    

    ##########################
    #### Convert to tidy ####
    ########################
    
    #########################################
    ## Data.frame with absolut predictions ##
    predictData_tidy <- predictData %>% gather(year, Y_absolut, 2:150, factor_key = T)
    str(predictData_tidy)
    levels(predictData_tidy$year)
    
    #########################################
    ## Data.frane with anomaly predictions ##
    predictData_anomaly_tidy <- predictData_anomaly  %>% gather(year, Y_anomaly, 2:150, factor_key = T)
    str(predictData_anomaly_tidy )
    
    ################################################
    ## Create data.frame defining the model names ##
    model <- as.data.frame(rep(namelist_models[[r]], dim(predictData_tidy)[1]))
    names(model) <- "model"
    
    ##########################################
    ## Append Model Name to tidy data.frame ##
    predictData_tidy <- cbind(model, predictData_tidy)
    predictData_anomaly_tidy <- cbind(model, predictData_anomaly_tidy)
    
    ##########################################################
    ## Combine to one large data.frame including all models ##
    predictData_tidy_all <- rbind(predictData_tidy_all, predictData_tidy)
    predictData_anomaly_tidy_all <- rbind(predictData_anomaly_tidy_all, predictData_anomaly_tidy)
    str(predictData_tidy_all )
    
  
  } ## End of loop over different climate models


#####################################################################################
#### Combine absolute predictions and anomaly predictions to one tidy data.frame ####
str(predictData_tidy_all)
str(predictData_anomaly_tidy_all)

names(predictData_tidy_all)[4] <- "Y"

predictData_tidy_complete <- merge(predictData_tidy_all, predictData_anomaly_tidy_all)


write.csv(predictData_tidy_complete, paste("./data/data_proj/output/",modelListMatrixNames[[s]],"/Yield_predict_complete_1951-2099_tidy.csv", sep="") )

} ## End of loop which uses different models to make predictions

str(predictData_tidy_complete )

rm(list=ls())
