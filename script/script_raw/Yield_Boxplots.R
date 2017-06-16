#### Boxplots ####
' Script to make boxplots or similiar graphics which represent the distribution of the difference 
in predictions for the two climate periode  compared to the reference period.
  1) boxplot plot the difference in the means, that means the distribution is 
     defined by the average of each spatial unit. Since only means are considered, it is not 
     possible to judge about the distribution of extreme deviations. 
     Boxplot for each RCM and climate period is based on 262 observations. 
  2) Boxplot which consideres the deviations to the reference period (1971)
      For each RGM and climate period: # of regional units * 30 years = 262 * 30 = 7860 observations
  Caveat: yield and yield anomalies have the same results, because 
  Y_SubY_mean_ref_anomaly = Y_anomaly - Y_anomaly_mean_ref = Y - mean(Y_19992015|comId) - mean(Y_ref - mean(Y_19992015|comId)|comId) =
                                                             Y -  mean(Y_ref|comId) 

'
#### Input ####
'
Spatial Information: Shapefile of comdIDs ("vg2500_krs")
BasePrediction.R: tidy.data.frames of yield and yield anomaly predictions based on different estimation models

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
library(tidyr)
library(gridExtra)
library(cowplot)
library(grid)
library(tidyverse)
library(sf)
library(ggplot2) 
library(Hmisc)
####################################################################################################################################################################



##############################
#### Preparation for loop ####

#### Laden der Shapes mit den Polygonen der Kreise und deren räumliche Zuordnung ####
vg2500_krs <- read_sf("./../Proj1/data/data_spatial/", "vg2500_krs")
str(vg2500_krs, 2)

#### Change RS to five digits #####
vg2500_krs$RS <- as.integer(str_sub(vg2500_krs$RS, 1,5))
vg2500_krs$RS

## Create List of models to loop trrough##
namelist_models <- c("DMI","ICTP", "KNMI","MPI","SMHI")
# PredictData_df_tidy <- list(MPI=data.frame(), DMI=data.frame(), KNMI=data.frame(), ICTP=data.frame(), SMHI=data.frame())


#### Generate list of start and end dates of climate periods ####
'Those are necessary for the conditioning in filter'
climateyears_list <- list(c(1971, 2021, 2070), c(2000, 2050, 2099))

## Names used to store the figures
modelListMatrixNames <- list("lm.fit_SMI_6_Jun_Aug_modelmatrix", "lm.fit_SMI_6_Jul_modelmatrix")
# modelListMatrixNames <- list("lm.fit_SMI_6_Jun_Aug", "lm.fit_SMI_6_Jul")

## Names used in figures
modelListYieldNames <-list("Model: combined", "Model: July")

##########################################################################
#### Loop through different models on which the predictions are based ####

#### Start of loop through prediction models #####
for (s in 1:length(modelListMatrixNames)){
  
  dir.create(paste("./figures/figures_exploratory/Proj/Boxplots/", modelListMatrixNames[[s]] ,sep=""), showWarnings = FALSE)
  
  
  #### Load tidy data.frame of Yield and Yield_Anomaly Predictions  ####
  ' one large data.frame also including a marker for the model'
  PredictData_df_tidy <- read.csv(paste("./data/data_proj/output/", modelListMatrixNames[[s]],"/Yield_predict_complete_1951-2099_tidy.csv", sep="") )
  str(PredictData_df_tidy) # 195190/149/5 = 262
  
  PredictData_df_tidy$model <- as.factor(gsub("SMHIRCA", "SMHI", PredictData_df_tidy$model))
  levels(PredictData_df_tidy$model)
  
  # PredictData_df_tidy$X <- NULL
  # PredictData_df_tidy[1:10, ]
  # PredictData_df_tidy[(1+39038):(10+39038), ]
  # PredictData_df_tidy[(1+2*39038):(10+2*39038), ] # Die Modelle liefern unterschiedlicher Ergebnisse
  # 
  #############################################################################################
  #### Generate list with data.frame container for each climate period: Summary Statistics ####
  PredictData_df_tidy_summaries_list <- list(PredictData_df_tidy_summaries_1979 = data.frame(), 
                                             PredictData_df_tidy_summaries_2021 = data.frame(),
                                             PredictData_df_tidy_summaries_2070 = data.frame())
  
  PredictData_df_tidy_summaries_list_transmute <- list(PredictData_df_tidy_summaries_1979_transmute = data.frame(), 
                                                       PredictData_df_tidy_summaries_2021_transmute = data.frame(),
                                                       PredictData_df_tidy_summaries_2070_transmute = data.frame())
    
  PredictData_df_tidy_summaries_list_average <- list(PredictData_df_tidy_summaries_1979_average = data.frame(), 
                                                     PredictData_df_tidy_summaries_2021_average = data.frame(),
                                                     PredictData_df_tidy_summaries_2070_average = data.frame())
  
  #################################################################################################################
  #### Loop to generate data.frame with Means and SDs of Y and Y_anomaly for the three different climate zones ####
  
  ## Generate dummy for each climate period and create accordingly a data.frame for the subset of each climate period ##
  dummy_list <- list("1971 - 2000", "2021-2050", "2070-2099")
    
  for ( r in 1:3){
      PredictData_df_tidy_summaries_list[[r]]  <- 
        PredictData_df_tidy  %>% 
        filter(year >=  climateyears_list[[1]][r] & year <= climateyears_list[[2]][r]) %>% 
        mutate(climate_period = dummy_list[[r]])
  }
    
  ## Check results
  # View(PredictData_df_tidy_summaries_list[[1]])
  dim(PredictData_df_tidy) # 195190 /262/5 = 149
  dim(PredictData_df_tidy_summaries_list[[1]]) # The change of the dimension makes sense 39300/262/5 = 30
  str(PredictData_df_tidy_summaries_list,2)
  
  #################################################################################################
  #### Loop to generate the mean and sd conditional on the RCM and the administrative district ####
  for (r in 1:3){
      PredictData_df_tidy_summaries_list[[r]]  <- 
        PredictData_df_tidy_summaries_list[[r]]  %>%
        group_by(model, comId) %>% 
        mutate( Y_mean= mean(Y), Y_sd = sd(Y), Y_sum= sum(Y))  
  }
   

  # View(PredictData_df_tidy_summaries_list[[2]])
  summary(PredictData_df_tidy_summaries_list[[2]])
  str(PredictData_df_tidy_summaries_list[[1]],1)
  str(PredictData_df_tidy_summaries_list[[2]],1)
  str(PredictData_df_tidy_summaries_list[[3]],1)
  
  #####################################################################
  #### Append Y_mean der reference periode for each climate period ####
  for (r in 1:3){
  PredictData_df_tidy_summaries_list[[r]]$Y_mean_ref <- PredictData_df_tidy_summaries_list[[1]]$Y_mean
  }
  str(PredictData_df_tidy_summaries_list[[1]],1)
  # View(PredictData_df_tidy_summaries_list[[3]])
   
  # ################################################################################################################################
  # #### Create difference between Y and Y_mean_ref -> YSubY_mean_ref and between Y_mean and Y_mean_ref -> Y_meanSubY_mean_ref ####
  # ##############################################################################################################################
  # for (r in 1:3){
  #   PredictData_df_tidy_summaries_list[[r]]  <-
  #       PredictData_df_tidy_summaries_list[[r]]  %>%
  #             mutate( YSubY_mean_ref              = Y - Y_mean_ref,
  #                     Y_meanSubY_mean_ref         = Y_mean - Y_mean_ref,
  #                     YSubY_mean_ref_anomaly      = Y_anomaly - Y_anomaly_mean_ref,
  #                     Y_meanSubY_mean_ref_anomaly = Y_anomaly_mean - Y_anomaly_mean_ref )
  # }
  # # View(PredictData_df_tidy_summaries_list[[2]])
  # 
  # ########################################################################
  # #### Combine data.frame to a large one covering all climate periods ####
  # 
  # ## Large data.frame considering all three climate period
  # PredictData_df_tidy_climate <- rbind(rbind(PredictData_df_tidy_summaries_list[[1]], 
  #                                            PredictData_df_tidy_summaries_list[[2]]), 
  #                                      PredictData_df_tidy_summaries_list[[3]])
  # ## Data.frame considering the climate periods starting 2021 and 2070
  # PredictData_df_tidy_climate20212070 <- rbind(PredictData_df_tidy_summaries_list[[2]], 
  #                                              PredictData_df_tidy_summaries_list[[3]])
  # dim(PredictData_df_tidy_climate)
  

  ################################################################################################################################
  #### Create difference between Y and Y_mean_ref -> YSubY_mean_ref and between Y_mean and Y_mean_ref -> Y_meanSubY_mean_ref ####
  ##############################################################################################################################
  ## new data.frame -> transmute 
  for (r in 1:3){
    PredictData_df_tidy_summaries_list_transmute[[r]]  <- 
      PredictData_df_tidy_summaries_list[[r]]  %>%
        transmute(  YSubY_mean_ref              = Y - Y_mean_ref,  
                    Y_meanSubY_mean_ref         = Y_mean - Y_mean_ref ) 
    PredictData_df_tidy_summaries_list_transmute[[r]]$climate_period  <- PredictData_df_tidy_summaries_list[[r]]$climate_period
  }
  View(PredictData_df_tidy_summaries_list_transmute[[2]])
  

  ####################################################################################
  #### Combine data.frame to a large one covering all climate periods - transmute ####
  
  ## Large data.frame considering all three climate period
  PredictData_df_tidy_climate_transmute <- rbind(rbind(PredictData_df_tidy_summaries_list_transmute[[1]], 
                                                       PredictData_df_tidy_summaries_list_transmute[[2]]), 
                                                       PredictData_df_tidy_summaries_list_transmute[[3]])
  
  ## Data.frame considering the climate periods starting 2021 and 2070
  PredictData_df_tidy_climate20212070_transmute <- rbind(PredictData_df_tidy_summaries_list_transmute[[2]], 
                                                         PredictData_df_tidy_summaries_list_transmute[[3]])
  
  dim(PredictData_df_tidy_climate_transmute )
  dim(PredictData_df_tidy_climate20212070_transmute)
  ######################################################################
  #### Make data.frame which does not differentiate between modell #####
  PredictData_df_tidy_climate_average <- PredictData_df_tidy_climate_transmute
  PredictData_df_tidy_climate_average$model <- rep("All Models", 150300)
  str(PredictData_df_tidy_climate_average)
  
  PredictData_df_tidy_climate20212070_average <- PredictData_df_tidy_climate20212070_transmute
  PredictData_df_tidy_climate20212070_average$model <- rep("All Models", 100200)
  str(PredictData_df_tidy_climate20212070_average)
  
  #######################################################
  #### Cbind model average and transmute data.frames ####
  names(PredictData_df_tidy_climate_transmute)
  names(PredictData_df_tidy_climate_average)  
  dim(PredictData_df_tidy_climate_transmute)
  dim(PredictData_df_tidy_climate_average) 
  
  PredictData_df_tidy_climate_complete <- bind_rows(PredictData_df_tidy_climate_transmute, PredictData_df_tidy_climate_average)
  View(PredictData_df_tidy_climate_complete)
  dim(PredictData_df_tidy_climate_complete)
  
  PredictData_df_tidy_climate20212070_complete <- bind_rows(PredictData_df_tidy_climate20212070_transmute, PredictData_df_tidy_climate20212070_average)
  
  ##########################################################
  #### Make boxplot / violin plots for absolute values #### 
  ########################################################
  
  ## Define Data Summary Statistic used in ggplots
  data_summary <- function(x) {
      m <- mean(x)
      ymin <- m-sd(x)*2
      ymax <- m+sd(x)*2
      return(c(y=m,ymin=ymin,ymax=ymax))
  } # End of function
    
  #### Violin Plot for comparing the means - only considering climate period 2021 and 2070 ####
  p20212070 <- ggplot(PredictData_df_tidy_climate20212070_complete,  aes(climate_period, Y_meanSubY_mean_ref ))
  
  p20212070_plot <-  p20212070 + geom_hline(yintercept=0, color="gray", size=1) +
      geom_violin(aes(fill = model), draw_quantiles = c(0.25, 0.5, 0.75), width=1, color="blue")  + facet_grid(. ~ model)  +
      stat_summary(fun.data=data_summary, color="orange")   + theme_minimal(base_size = 14) +  theme(legend.position="none")  + scale_fill_brewer(palette="Greys")  +
      ggtitle(paste(modelListYieldNames[[s]])) + ylab("Mean(Y) of climate period - Mean(Y) of reference period") + 
      xlab("Climate Period") +
    scale_y_continuous(limits=c(-70, 40))  + 
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5)) + guides(fill=FALSE)
  ggsave(paste("./figures/figures_exploratory/Proj/Boxplots/", modelListMatrixNames[[s]],"/ViolinPlot_Means.pdf", sep="") , p20212070_plot, width=16, height=9) 
    
    
    
  #### Violin Plot for comparing the yield deviation from the reference period mean ####
  p <- ggplot(PredictData_df_tidy_climate_complete, aes(climate_period, YSubY_mean_ref ))
  p_plot <-  p + geom_hline(yintercept=0, color="gray", size=1) +
      geom_violin(aes(fill = model), draw_quantiles = c(0.25, 0.5, 0.75), width=1, color="blue")  + facet_grid(. ~ model)  +
      stat_summary(fun.data=data_summary, color="orange")  + 
      theme_minimal(base_size = 14) +  theme(legend.position="none", axis.text.y = element_text(size = 15))  + scale_fill_brewer(palette="Greys")  +
      ggtitle(paste(modelListYieldNames[[s]])) + ylab("Y of climate period - Mean(Y) of reference period ") + 
      xlab("Climate Period") +
    scale_y_continuous(limits=c(-150, 285))  + 
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5)) + guides(fill=FALSE)
  ggsave(paste("./figures/figures_exploratory/Proj/Boxplots/", modelListMatrixNames[[s]],"/ViolinPlot_Yield.pdf", sep="") , p_plot, width=16, height=9) 
 
  
  #### Check whether summaries are comparable ####   
  names(PredictData_df_tidy_summaries_list[[1]])
  
  PredictData_df_tidy_summaries_list[[2]] %>%
  group_by(model) %>%
   summarise(mean(Y), sd(Y)) 
  ' Sample show, that the summaries are the same.'
    
}   # End of loop through climate models
  
# rm(list=ls())

