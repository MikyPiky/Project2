##################################################################
#### Plots of Crop Yield (SM) predicted in BasePrediction.R  ####
################################################################
#### Description ####
'
Here I look at plots which average over all climate models ("MPI","DMI","KNMI","ICTP","SMHI")

- Loop through different prediction models  
- read in tidy data for each prediction model
- Loop to create Means and SD of the absolute values for the climate periods (1971 - 2000, 2021 - 2050, 2070 - 2099) -> output is list of data.frames
- Create differences in mean and sd between the reference climate period (1971 - 2000) and the projections (2021 - 2050, 2070 - 2099)
- Export summary statistics of means, sd, and the difference in both via stargazer
- Produce Plots
Plots:
Plots of Difference in Mean or SD, climate periods (2021-2050, 2070-2099), compared to reference period (1971-2000)


Plot of absolute values (same for Mean and SD)  for each climate period 
- Tav in July
- Pre in July 
- SMI in June
- SMI in July
- SMI in August


'



#### Input ####
'
Spatial Information: Shapefile of comdIDs ("vg2500_krs")_
  - vg2500_krs -> data_proj_Input/CLC 
BasePrediction.R: tidy.data.frames of yield and yield anomaly predictions based on different estimation models:
  - "./data/data_proj/output/",modelListMatrixNames[[s]],"/Yield_predict_complete_1951-2099_tidy.csv"

'

#### Output ####
'
Descriptive Statistics:
"./figures/figures_exploratory/Proj/", modelListMatrixNames[[s]] , "/DeskriptiveStats_2070_", "average_all_models",".txt"
Plots:
"./figures/figures_exploratory/Proj/", modelListMatrixNames[[s]],"/plot_mean_Y_", "average_all_models",".pdf"
*modelListMatrixNames[[s]] is prediction model
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
####################################################################################################################################################################



##############################################################################################################################################################################
##############################################################################################################################################################################
###########################################################################################################
#### Plot Maps of predicted data (averages of climate periods (1971 - 2000, 2021 - 2050, 2070 - 2099) ####
#########################################################################################################

##############################
#### Preparation for loop ####

#### Laden der Shapes mit den Polygonen der Kreise und deren räumliche Zuordnung ####
vg2500_krs <- read_sf("./../Proj1/data/data_spatial/", "vg2500_krs")
str(vg2500_krs, 2)

#### Change RS to five digits #####
vg2500_krs$RS <- as.integer(str_sub(vg2500_krs$RS, 1,5))
vg2500_krs$RS


#### Generate list of start and end dates of climate periods ####
'Those are necessary for the conditioning in filter'
climateyears_list <- list(c(1971,2021,2070), c(2000, 2050, 2099))


##########################################################################
#### Loop through different models on which the predictions are based ####

## Names used to store the figures
modelListMatrixNames <- list("lm.fit_SMI_6_Jun_Aug_modelmatrix", "lm.fit_SMI_6_Jul_modelmatrix")
# modelListMatrixNames <- list("lm.fit_SMI_6_Jun_Aug", "lm.fit_SMI_6_Jul")

## Names used in figures
modelListYieldNames <-list("Model: combined", "Model: July")

#### Create container for diff plots ####
' These containers store the plots of the yield predictions which are needed for the combined plots.'
plot_mean_1971_average_list <- list(lm.fit_SMI_6_Jun_Aug_modelmatrix = list(), 
                            lm.fit_SMI_6_Jul_modelmatrix = list() )

plot_mean_diff2021_average_list <- list(lm.fit_SMI_6_Jun_Aug_modelmatrix = list(), 
                                lm.fit_SMI_6_Jul_modelmatrix = list() )

plot_mean_diff2070_average_list <- list(lm.fit_SMI_6_Jun_Aug_modelmatrix = list(), 
                                lm.fit_SMI_6_Jul_modelmatrix = list() )

plot_sd_1971_average_list <- list(lm.fit_SMI_6_Jun_Aug_modelmatrix = list(), 
                          lm.fit_SMI_6_Jul_modelmatrix = list( ) )

plot_sd_diff2021_average_list <- list(lm.fit_SMI_6_Jun_Aug_modelmatrix = list(), 
                              lm.fit_SMI_6_Jul_modelmatrix = list() )

plot_sd_diff2070_average_list <- list(lm.fit_SMI_6_Jun_Aug_modelmatrix = list( ), 
                              lm.fit_SMI_6_Jul_modelmatrix = list() )


#### Start of loop through prediction models #####
for (s in 1:length(modelListMatrixNames)){

  dir.create(paste("./figures/figures_exploratory/Proj/", modelListMatrixNames[[s]] ,sep=""),showWarnings = FALSE)
  

#### Load tidy data.frame of Yield and Yield_Anomaly Predictions  ####
' one large data.frame also including a marker for the model'
PredictData_df_tidy <- read.csv(paste("./data/data_proj/output/", modelListMatrixNames[[s]],"/Yield_predict_complete_1951-2099_tidy.csv", sep="") )
str(PredictData_df_tidy) # 195190/149/5 = 262

PredictData_df_tidy$X <- NULL


###################################################################################################################################################
#### Loop through all 5 climate models to create Means of the absolute values for the climate periods (1971 - 2000, 2021 - 2050, 2070 - 2099)  ####
# for (t in 1:5){
  #### Generate list with data.frame container for each climate period: Summaries ####
  PredictData_df_tidy_summaries_average_list <- list(PredictData_df_tidy_summaries_1979 = data.frame(), PredictData_df_tidy_summaries_2021= data.frame(),
                                        PredictData_df_tidy_summaries_2070 = data.frame())
  
  #### List to store sf.data.frames ####
  PredictData_df_tidy_summaries_sf_average_list <- list(PredictData_df_tidy_summaries_sf_1979 = data.frame(), PredictData_df_tidy_summaries_sf_2021= data.frame(),
                                           PredictData_df_tidy_summaries_sf_2070 = data.frame())
  
  names(PredictData_df_tidy)
  summary(PredictData_df_tidy)
  
  #############################################################################################################################
  #### Loop to generate data.frame with Means, SDs,, Max, and Min of Y and Y_anomaly for the three different climate zones ####
  #############################################################################################################################
  for (r in 1:3){
    PredictData_df_tidy_summaries_average_list[[r]]  <- 
      PredictData_df_tidy  %>% 
      filter(year >=  climateyears_list[[1]][r] & year <= climateyears_list[[2]][r]) %>% 
      group_by(comId)     %>%      
      summarise( Y_mean= mean(Y), Y_sd = sd(Y), Y_sum= sum(Y), 
                 Y_anomaly_mean= mean(Y_anomaly), Y_anomaly_sd = sd(Y_anomaly), 
                 Y_max = max(Y), Y_min = min(Y), 
                 Y_anomaly_max = max(Y_anomaly), Y_anomaly_min = min(Y_anomaly))
    
    
    #### Merge with Spatial Information ####
    PredictData_df_tidy_summaries_sf_average_list[[r]] <- merge(vg2500_krs, PredictData_df_tidy_summaries_average_list[[r]], by.x = "RS", by.y = "comId", all.x=T, sort=T) 
  
  }
  summary(PredictData_df_tidy_summaries_average_list[[1]])
  str(PredictData_df_tidy_summaries_average_list[[1]])
  # summary(PredictData_df_tidy_summaries_sf_average_list[[2]])
  # str(PredictData_df_tidy_summaries_sf_average_list[[2]])

  ##################################################################################################################################
  #### Create differences in between the reference climate period (1971 - 2000) and the projections (2021 - 2050, 2070 - 2099) ####
  names(PredictData_df_tidy_summaries_average_list[[2]])
  
  PredictData_df_tidy_summaries_diff2021 <- PredictData_df_tidy_summaries_average_list[[2]][,2:6] - PredictData_df_tidy_summaries_average_list[[1]][,2:6]
  PredictData_df_tidy_summaries_diff2070 <- PredictData_df_tidy_summaries_average_list[[3]][,2:6] - PredictData_df_tidy_summaries_average_list[[1]][,2:6]
  str(PredictData_df_tidy_summaries_diff2021)
  
  #### Add comIds to allow for merging ####
  PredictData_df_tidy_summaries_diff2021$comId <- PredictData_df_tidy_summaries_average_list[[2]]$comId
  PredictData_df_tidy_summaries_diff2070$comId <- PredictData_df_tidy_summaries_average_list[[2]]$comId
  
  #### Merge difference data with vg2500_krs to get Spatial Attributes ####
  PredictData_df_tidy_summaries_diff2021_sf <- merge(vg2500_krs, PredictData_df_tidy_summaries_diff2021 , by.x = "RS", by.y = "comId", all.x=T, sort=T) 
  PredictData_df_tidy_summaries_diff2070_sf <- merge(vg2500_krs, PredictData_df_tidy_summaries_diff2070 , by.x = "RS", by.y = "comId", all.x=T, sort=T) 
  
  #### Check newly created datA ####
  summary(PredictData_df_tidy_summaries_diff2021)
  summary(PredictData_df_tidy_summaries_diff2070)

  str(PredictData_df_tidy_summaries_diff2021_sf)
  str(PredictData_df_tidy_summaries_diff2070_sf)
  
  ' The diff. data for Y and Y_anomaly are the same. By differencing the comId specific mean disappears by the means of the substraction, since 
  Y_anomaly =  Y - mean(Y of comId between 1999 and 2015), where Y is time dependent but mean(Y of comId between 1999 and 2015 is a constant) '
  
  ###################################
  #### Export summary statistics ####
  stargazer(PredictData_df_tidy_summaries_diff2021, type = "text", title="Descriptive statistics", digits=3,
            out=paste("./figures/figures_exploratory/Proj/", modelListMatrixNames[[s]] , "/DeskriptiveStats_diff2021_", "average_all_models", ".txt", sep=""))
  stargazer(PredictData_df_tidy_summaries_diff2070, type = "text", title="Descriptive statistics", digits=3,
            out=paste("./figures/figures_exploratory/Proj/", modelListMatrixNames[[s]] , "/DeskriptiveStats_diff2070_",  "average_all_models",".txt", sep=""))
  stargazer(as.data.frame(PredictData_df_tidy_summaries_average_list[[1]]), type = "text", title="Descriptive statistics", digits=3,
            out=paste("./figures/figures_exploratory/Proj/", modelListMatrixNames[[s]] , "/DeskriptiveStats_1971_", "average_all_models",".txt", sep=""))
  stargazer(as.data.frame(PredictData_df_tidy_summaries_average_list[[2]]), type = "text", title="Descriptive statistics", digits=3,
            out=paste("./figures/figures_exploratory/Proj/", modelListMatrixNames[[s]] , "/DeskriptiveStats_2021_", "average_all_models",".txt", sep=""))
  stargazer(as.data.frame(PredictData_df_tidy_summaries_average_list[[3]]), type = "text", title="Descriptive statistics", digits=3,
            out=paste("./figures/figures_exploratory/Proj/", modelListMatrixNames[[s]] , "/DeskriptiveStats_2070_", "average_all_models",".txt", sep=""))
  ##############################################################################################################################################################################
  ##############################################################################################################################################################################
  
  ##############################################################################################################################################################################
  #### Plot of Maximal and Minimal Values of Yield Anomales ####
  ##############################################################################################################################################################################
  summary((PredictData_df_tidy_summaries_sf_average_list[[1]]))
  summary((PredictData_df_tidy_summaries_sf_average_list[[2]]))
  summary(PredictData_df_tidy_summaries_sf_average_list[[3]])
  
  climate_periods <- list("1971 - 2000", "2021 - 2050", "2070 - 2099")
  
  #### Loop over all three climate periods ####
  for (r in 1:length(climate_periods)){
    ## Maximal Values
    sc <- scale_fill_gradientn("Yield Anomaly \n - Max", colours = myPalette(11), limits=c(-400, 400))
    plot_anomaly_max <-
      ggplot(PredictData_df_tidy_summaries_sf_average_list[[r]]) +
      geom_sf(aes(fill = Y_anomaly_max)) +
      ggtitle(paste( modelListYieldNames[[s]],", Period: ", climate_periods[r] , sep="")) + sc + 
      theme_bw() + 
      theme(plot.title = element_text(hjust = 0.5))
    
    ggsave(paste("./figures/figures_exploratory/Proj/", modelListMatrixNames[[s]],"/plot_max_Yanomaly_", climate_periods[r] ,"_", "all_models",".pdf", sep=""), 
           plot = plot_anomaly_max, width=21, height=8)
    
    ## Minimal Values    
    sc <- scale_fill_gradientn("Yield Anomaly \n - Min", colours = myPalette(11), limits=c(-400, 400))
    plot_anomaly_min <-
      ggplot(PredictData_df_tidy_summaries_sf_average_list[[r]]) +
      geom_sf(aes(fill = Y_anomaly_min)) +
      ggtitle(paste( modelListYieldNames[[s]],", Period: 1971 - 2000", sep="")) + sc +
      theme_bw()+ 
      theme(plot.title = element_text(hjust = 0.5))
    
    ggsave(paste("./figures/figures_exploratory/Proj/", modelListMatrixNames[[s]],"/plot_min_Yanomaly_", climate_periods[r] ,"_", "all_models",".pdf", sep=""), 
           plot = plot_anomaly_min, width=21, height=8)
  }
  
  
  
  
  ##############################################################################################################################################################################
  #### Plot of Means ####
  ##############################################################################################################################################################################

  #################################################
  #### Plot Mean of Y for each climate period ####
  ###############################################

  ## Define colorRamp ##
  str(PredictData_df_tidy_summaries_sf_average_list,1)
  summary(PredictData_df_tidy_summaries_sf_average_list [[1]]$Y_mean)
  summary(PredictData_df_tidy_summaries_sf_average_list [[2]]$Y_mean)
  summary(PredictData_df_tidy_summaries_sf_average_list [[3]]$Y_mean)


  myPalette <- colorRampPalette((brewer.pal(9, "YlGn")))
  sc <- scale_fill_gradientn("Yield", colours = myPalette(100), limits=c(250, 650))

  ## Plot Mean Y: 1971 - 2000 ##
  plot_mean_1971_Y <-
    ggplot(PredictData_df_tidy_summaries_sf_average_list [[1]]) +
    geom_sf(aes(fill = Y_mean)) +
    ggtitle(paste( modelListYieldNames[[s]],", Mean: 1971 - 2000", sep="")) + sc +
    theme_bw()

  # plot_mean_1971_Y

  ## Store to allow combined plots
  plot_mean_1971_average_list[[s]] <-  plot_mean_1971_Y

  ## Plot Mean Y: 2021 - 2050  ##
  plot_mean_2021_Y <-
    ggplot(PredictData_df_tidy_summaries_sf_average_list [[2]]) +
    geom_sf(aes(fill = Y_mean)) +
    ggtitle(paste( modelListYieldNames[[s]],", Mean: 2021 - 2050", sep="")) + sc +
    theme_bw()

  # plot_mean_2021_Y

  ## Plot Mean: 2070 - 2099  ##
  # plot_mean_2070_Y <-
  #   ggplot(PredictData_df_tidy_summaries_sf_average_list [[3]]) +
  #   geom_sf(aes(fill = Y_mean)) +
  #   ggtitle(paste( modelListYieldNames[[s]],",Mean: 2070 - 2099", sep="")) + sc +
  #   theme_bw()
  # 
  # # plot_mean_2070_Y
  # str(plot_mean_2070_Y)

  # plot_mean_Y <- grid.arrange(plot_mean_1971_Y, plot_mean_2021_Y, plot_mean_2070_Y, ncol=3, top=textGrob(paste("average_all_models"),gp=gpar(fontsize=30)))
  # plot_mean_Y

  # ggplot2::ggsave(paste("./figures/figures_exploratory/Proj/", modelListMatrixNames[[s]],"/plot_mean_Y_", "average_all_models",".pdf", sep=""), plot=plot_mean_Y, width=21, height=8)


  # ###############################################
  # #### Plot SD of Y for each climate period ####
  # #############################################
  # 
  # ## Define colorRamp ##
  # str(PredictData_df_tidy_summaries_sf_average_list,1)
  # summary(PredictData_df_tidy_summaries_sf_average_list [[1]]$Y_sd)
  # summary(PredictData_df_tidy_summaries_sf_average_list [[2]]$Y_sd)
  # summary(PredictData_df_tidy_summaries_sf_average_list [[3]]$Y_sd)
  # 
  # 
  # myPalette <- colorRampPalette((brewer.pal(9, "Purples")))
  # sc <- scale_fill_gradientn("Yield", colours = myPalette(100), limits=c(0, 60))
  # ' Here I take the mean of the period 1971 - 2000 as reference for colouring'
  # 
  # 
  # ## Plot SD Y: 1971 - 2000 ##
  # plot_sd_1971_Y <- 
  #   ggplot(PredictData_df_tidy_summaries_sf_average_list [[1]]) + 
  #   geom_sf(aes(fill = Y_sd)) + 
  #   ggtitle(paste( modelListYieldNames[[s]],", SD: 1971 - 2000", sep="")) + sc + 
  #   theme_bw()
  # 
  # # plot_sd_1971_Y
  # 
  # ## Store to allow combined plots
  # plot_sd_1971_average_list[[s]] <-  plot_sd_1971_Y
  # 
  # ## Plot SD Y: 2021 - 2050  ##
  # plot_sd_2021_Y <- 
  #   ggplot(PredictData_df_tidy_summaries_sf_average_list [[2]]) + 
  #   geom_sf(aes(fill = Y_sd)) + 
  #   ggtitle(paste( modelListYieldNames[[s]],", SD: 2021 - 2050", sep="")) + sc + 
  #   theme_bw()
  # 
  # # plot_sd_2021_Y
  # 
  # ## Plot SD: 2070 - 2099  ##
  # plot_sd_2070_Y <- 
  #   ggplot(PredictData_df_tidy_summaries_sf_average_list [[3]]) + 
  #   geom_sf(aes(fill = Y_sd)) + 
  #   ggtitle(paste( modelListYieldNames[[s]],", SD: 2070 - 2099", sep="")) + sc + 
  #   theme_bw()
  # 
  # # plot_sd_2070_Y
  # 
  # plot_sd_Y <- grid.arrange(plot_sd_1971_Y, plot_sd_2021_Y, plot_sd_2070_Y, ncol=3, top=textGrob(paste("average_all_models"), gp=gpar(fontsize=30))) 
  # # plot_sd_Y
  # 
  # # ggplot2::ggsave(paste("./figures/figures_exploratory/Proj/", modelListMatrixNames[[s]],"/plot_sd_Y_", "average_all_models",".pdf", sep=""), plot=plot_sd_Y, width=21, height=8)
  # 
  ##############################################################################################################################################################################
  #### Plot Differences ####
  ##############################################################################################################################################################################

  ###########################################################
  #### Plot difference in mean of climate periods of YD #####
  ###########################################################

  #### Define colorRamp for Y_mean ####
  summary(PredictData_df_tidy_summaries_diff2070_sf$Y_mean, digits = 2)
  summary(PredictData_df_tidy_summaries_diff2021_sf$Y_mean, digits = 2)

  myPalette <- colorRampPalette((brewer.pal(11, "BrBG")))
  sc <- scale_fill_gradientn("Yield", colours = myPalette(100), limits=c(-50, 50))

  #### Plot Difference of Mean (2070-2099) - (1971-2000) ####
  plot_mean_diff2070_Y <-
    ggplot(PredictData_df_tidy_summaries_diff2070_sf) +
    geom_sf(aes(fill = Y_mean)) +
    ggtitle(paste( modelListYieldNames[[s]],", Mean (2070-2099) - (1971-2000) ", sep="")) + sc +
    theme_bw()

  # plot_mean_diff2070_Y

  ## Store to allow combined plots
  plot_mean_diff2070_average_list[[s]] <-  plot_mean_diff2070_Y
  

  #### Plot Difference of Mean (2021-2050) - (1971-2000) ####
  plot_mean_diff2021_Y <-
    ggplot(PredictData_df_tidy_summaries_diff2021_sf) +
    geom_sf(aes(fill = Y_mean)) +
    ggtitle(paste( modelListYieldNames[[s]],", Mean (2021-2050) - (1971-2000)", sep=""))  + sc +
    theme_bw()

  # plot_mean_diff2021_Y


  ## Store to allow combined plots
  plot_mean_diff2021_average_list[[s]] <-   plot_mean_diff2021_Y

  # plot_mean_diff_Y <- grid.arrange(plot_mean_diff2021_Y, plot_mean_diff2070_Y, ncol=2, top=textGrob(paste("average_all_models"),gp=gpar(fontsize=30)))
  # # plot_mean_diff_Y
  # 
  # # ggplot2::ggsave(paste("./figures/figures_exploratory/Proj/", modelListMatrixNames[[s]],"/plot_mean_diff_Y_", "average_all_models",".pdf", sep=""), plot=plot_mean_diff_Y, width=14, height=8)
  # 
  # ###########################################################
  # #### Plot difference in sd of climate periods of YD #####
  # ##########################################################
  # 
  # #### Define colorRamp for Y_sd ####
  # summary(PredictData_df_tidy_summaries_diff2070_sf$Y_sd)
  # summary(PredictData_df_tidy_summaries_diff2021_sf$Y_sd)
  # 
  # myPalette <- colorRampPalette((brewer.pal(11, "PiYG")))
  # sc <- scale_fill_gradientn("Yield", colours = myPalette(100), limits=c(-30, 30))
  # 'Here I take zero a reference'
  # 
  # #### Plot Difference of SD (2070-2099) - (1971-2000) ####
  # plot_sd_diff2070_Y <- 
  #   ggplot(PredictData_df_tidy_summaries_diff2070_sf) + 
  #   geom_sf(aes(fill = Y_sd)) + 
  #   ggtitle(paste( modelListYieldNames[[s]],", SD (2070-2099) - (1971-2000) ", sep="")) + sc + 
  #   theme_bw()
  # 
  # # plot_sd_diff2070_Y
  # 
  # ## Store to allow combined plots
  # plot_sd_diff2070_average_list[[s]] <-  plot_sd_diff2070_Y
  # 
  # #### Plot Difference of SD (2021-2050) - (1971-2000) ####
  # plot_sd_diff2021_Y <- 
  #   ggplot(PredictData_df_tidy_summaries_diff2021_sf) + 
  #   geom_sf(aes(fill = Y_sd)) + 
  #   ggtitle(paste( modelListYieldNames[[s]],", SD (2021-2050) - (1971-2000)", sep=""))  + sc + 
  #   theme_bw()
  # 
  # # plot_sd_diff2021_Y
  # 
  # ## Store to allow combined plots
  # plot_sd_diff2021_average_list[[s]] <-   plot_sd_diff2021_Y
  # 
  # plot_sd_diff_Y <- grid.arrange(plot_sd_diff2021_Y, plot_sd_diff2070_Y, ncol=2, top=textGrob(paste("average_all_models"),gp=gpar(fontsize=30)))
  # # plot_sd_diff_Y
  # 
  # # ggplot2::ggsave(paste("./figures/figures_exploratory/Proj/", modelListMatrixNames[[s]],"/plot_sd_diff_Y_", "average_all_models",".pdf", sep=""), plot=plot_sd_diff_Y, width=14, height=8)
  # 
# } ## End of loop through all five climate models -> index is t


} ## End of loop through models -> index is s



