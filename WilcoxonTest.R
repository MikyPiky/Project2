#### Test for significant changes in the predicted yield values for the climate period ####
'
- Load the tidy data
- Filter for climate models
- Filter for the reference and climate periods
- Make on data.frame including the observations for all three climate periods
- Group by comIds and apply test to each cohort
- Combine with spatial information

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

#### Generate list of start and end dates of climate periods ####
climateyears_list <- list(c(1971, 2021, 2070), c(2000, 2050, 2099))

## Names used to store the figures
modelListMatrixNames <- list("lm.fit_SMI_6_Jun_Aug_modelmatrix", "lm.fit_SMI_6_Jul_modelmatrix")

##########################################################################
#### Loop through different models on which the predictions are based ####

#### Start of loop through prediction models #####
for (s in 1:length(modelListMatrixNames)){
  
  dir.create(paste("./figures/figures_exploratory/Proj/Boxplots/", modelListMatrixNames[[s]] ,sep=""), showWarnings = FALSE)
  
  
  #### Load tidy data.frame of Yield and Yield_Anomaly Predictions  ####
  PredictData_df_tidy <- read.csv(paste("./data/data_proj/output/", modelListMatrixNames[[s]],"/Yield_predict_complete_1951-2099_tidy.csv", sep="") )
  str(PredictData_df_tidy) # 195190/149/5 = 262
  
  PredictData_df_tidy$X <- NULL

  #######################################################################################################################################################
  #### Loop to create one data.frame for each climate period, i.e the reference period 1970 - 2000 and the climate periods 2021-2050 and 2070 - 2099 ####

  #### Generate list with data.frame container for each climate period: ####
  PredictData_df_tidy_test_list <- list(PredictData_df_tidy_test_1979 = data.frame(), PredictData_df_tidy_test_2021= data.frame(),
                                        PredictData_df_tidy_test_2070 = data.frame())
  
  for ( r in 1:3){
    PredictData_df_tidy_test_list[[r]]  <- 
      PredictData_df_tidy  %>% 
      filter(year >=  climateyears_list[[1]][r] & year <= climateyears_list[[2]][r]) 
  }
  
  dim(PredictData_df_tidy) # 195190 /262/5 = 149
  dim(PredictData_df_tidy_test_list[[1]]) # The change of the dimension makes sense 39300/262/5 = 30
  str(PredictData_df_tidy_test_list,2)
  summary(PredictData_df_tidy_test_list[[1]])
  summary(PredictData_df_tidy_test_list[[2]])
  summary(PredictData_df_tidy_test_list[[3]])
  
  #### Rename y and y_anomaly of each climate period accordingly ####
  names(PredictData_df_tidy_test_list[[1]]) <- c("model", "comId", "year",  "Y_1971", "Y_anomaly_1971")
  names(PredictData_df_tidy_test_list[[2]]) <- c("model", "comId", "year",  "Y_2021", "Y_anomaly_2021")
  names(PredictData_df_tidy_test_list[[3]]) <- c("model", "comId", "year",  "Y_2070", "Y_anomaly_2070")
  
  
  # PredictData_df_tidy_test_list[[1]]$year <- PredictData_df_tidy_test_list[[2]]$year <- 
    # PredictData_df_tidy_test_list[[3]]$year <- NULL
  
  #### Merge the data of the three climate periods ####
  dim(PredictData_df_tidy_test_list[[1]])
  dim(PredictData_df_tidy_test_list[[2]])
  dim(PredictData_df_tidy_test_list[[3]])
  
  test_data <- cbind(PredictData_df_tidy_test_list[[1]], cbind(PredictData_df_tidy_test_list[[2]][,4:5], PredictData_df_tidy_test_list[[3]][,4:5]))
  dim( test_data )
  str( test_data)
  test_data$year <- NULL
  #### Compare columns by WilcoxonText #####
  (wilcox.test( test_data$Y_anomaly_1971,  test_data$Y_anomaly_2070))
  (wilcox.test( test_data$Y_anomaly_1971,  test_data$Y_anomaly_2070))$p.value
  
  ########################################
  #### Group Data by comIds and model ####
  str(test_data)

  
  ##########################################################################################
  #### Loop though five climate models to provide maps of p-values of the Wilcoxon Test ####
  
  ## Preparation ##
  ## Container
  test_data_grouped_2070_anomaly_list <- list(MPI=list(), DMI=list(), KNMI=list(), ICTP=list(), SMI=list())
  test_data_grouped_2021_anomaly_list <- list(MPI=list(), DMI=list(), KNMI=list(), ICTP=list(), SMI=list())
  test_data_grouped_2070_list <- list(MPI=list(), DMI=list(), KNMI=list(), ICTP=list(), SMI=list())
  test_data_grouped_2021_list <- list(MPI=list(), DMI=list(), KNMI=list(), ICTP=list(), SMI=list())
  
  ## Names used in figures
  modelListYieldNames <-list("Yield: SMI_6_Jun_Aug", "Yield: SMI_6_Jul")
  
  ## Start of loop ##
  for (l in 1:5){
  test_data_grouped_2070_anomaly_list[[l]]  <- test_data %>% 
    filter(model == namelist_models[[l]]) %>%
    group_by(comId) %>%
      summarise(test = wilcox.test( test_data$Y_anomaly_1971,  test_data$Y_anomaly_2070)$p.value, 
                test_paired  = wilcox.test( test_data$Y_anomaly_1971,  test_data$Y_anomaly_2070, paired=T)$p.value )
  
  test_data_grouped_2021_anomaly_list[[l]]  <- test_data %>% 
    filter(model == namelist_models[[l]]) %>%
    group_by(comId) %>%
    summarise(test = wilcox.test( test_data$Y_anomaly_1971,  test_data$Y_anomaly_2021)$p.value,
              test_paired = wilcox.test(test_data$Y_anomaly_1971,  test_data$Y_anomaly_2021, paired=T)$p.value)
  
  test_data_grouped_2070_list[[l]]  <- test_data %>% 
    filter(model == namelist_models[[l]]) %>%
    group_by(comId) %>%
    summarise(test = wilcox.test( test_data$Y_1971,  test_data$Y_2070)$p.value,
              test_paired = wilcox.test( test_data$Y_1971,  test_data$Y_2070, paired=T)$p.value)
  
  test_data_grouped_2021_list[[l]] <- test_data %>% 
    filter(model == namelist_models[[l]]) %>%
    group_by(comId) %>%
    summarise(test = wilcox.test(Y_1971,  Y_2021)$p.value,
              test_paired = wilcox.test(Y_1971,  Y_2021, paired=T)$p.value)
  
  #### Add on Spatial Data ####
  test_data_grouped_2021_anomaly_spatial <- merge(vg2500_krs, test_data_grouped_2070_anomaly_list[[l]], by.x="RS", by.y="comId")
  test_data_grouped_2070_anomaly_spatial <- merge(vg2500_krs, test_data_grouped_2021_anomaly_list[[l]], by.x="RS", by.y="comId")
  test_data_grouped_2021_spatial <- merge(vg2500_krs, test_data_grouped_2070_list[[l]] , by.x="RS", by.y="comId")
  test_data_grouped_2070_spatial <- merge(vg2500_krs, test_data_grouped_2021_list[[l]], by.x="RS", by.y="comId")

  ##############  
  #### Maps ####
  
  ## non paired
  test_data_grouped_2021_anomaly_spatial_plot <-
  ggplot(test_data_grouped_2021_anomaly_spatial) + 
    geom_sf(data = vg2500_krs, colour="white") + 
    geom_sf(aes(fill = cut(test, c(-0.1,0.05,0.1,1), m=0) )) + 
    scale_fill_brewer(type = "seq", palette = "Blues", direction = -1,  drop = FALSE,
                      labels=c("< 0.05", "< 0.1", "> 0.1")) +  
      guides(fill = guide_legend(title = "p-values")) +
      ggtitle("2021 - Anomalies")
  
  test_data_grouped_2070_anomaly_spatial_plot <-
  ggplot(test_data_grouped_2070_anomaly_spatial) + 
    geom_sf(data = vg2500_krs, colour="white") + 
    geom_sf(aes(fill = cut(test, c(-0.1, 0.05,0.1,1), m=0) )) + 
    scale_fill_brewer(type = "seq", palette = "Blues", direction = -1,  drop = FALSE,
                      labels=c("< 0.05", "< 0.1", "> 0.1")) +  
    guides(fill = guide_legend(title="p-values")) +
    ggtitle("2070 - Anomalies")
  
  test_data_grouped_2021_spatial_plot <-
  ggplot(test_data_grouped_2021_spatial) + 
    geom_sf(data = vg2500_krs, colour="white") + 
    geom_sf(aes(fill = cut(test, c(-0.1,0.05,0.1,1), m=0) )) + 
    scale_fill_brewer(type="seq",palette = "Blues", direction = -1,  drop = FALSE,
                      labels=c("< 0.05", "< 0.1", "> 0.1")) +  
    guides(fill = guide_legend(title = "p-values")) +
    ggtitle("2021")
  
  test_data_grouped_2070_spatial_plot <- 
  ggplot(test_data_grouped_2070_spatial) + 
    geom_sf(data = vg2500_krs, colour="white") + 
    geom_sf(aes(fill = cut(test, c(-0.1,0.05,0.1,1), m=0) )) + 
    scale_fill_brewer(type = "seq", palette = "Blues", direction = -1,  drop = FALSE,
                      labels=c("< 0.05", "< 0.1", "> 0.1")) + 
    guides(fill = guide_legend(title="p-values")) +
    ggtitle("2070")
  
  ggsave(paste("./figures/figures_exploratory/Proj/Wilcoxon/", modelListMatrixNames[[s]],"/Wilcoxon_2070_anomaly_",namelist_models[[l]],".pdf", sep="") ,  test_data_grouped_2070_anomaly_spatial_plot, width=16, height=9) 
  ggsave(paste("./figures/figures_exploratory/Proj/Wilcoxon/", modelListMatrixNames[[s]],"/Wilcoxon_2021_anomaly_",namelist_models[[l]],".pdf", sep="") ,  test_data_grouped_2021_anomaly_spatial_plot, width=16, height=9) 
  ggsave(paste("./figures/figures_exploratory/Proj/Wilcoxon/", modelListMatrixNames[[s]],"/Wilcoxon_2070_",namelist_models[[l]],".pdf", sep="") ,  test_data_grouped_2070_spatial_plot, width=16, height=9) 
  ggsave(paste("./figures/figures_exploratory/Proj/Wilcoxon/", modelListMatrixNames[[s]],"/Wilcoxon_2021_",namelist_models[[l]],".pdf", sep="") ,  test_data_grouped_2021_spatial_plot, width=16, height=9) 
 
 ## non paired
  test_data_grouped_2021_anomaly_spatial_plot <-
    ggplot(test_data_grouped_2021_anomaly_spatial) + 
    geom_sf(data = vg2500_krs, colour="white") + 
    geom_sf(aes(fill = cut(test_paired, c(-0.1,0.05,0.1,1), m=0) )) + 
    scale_fill_brewer(type = "seq", palette = "Blues", direction = -1,  drop = FALSE,
                      labels=c("< 0.05", "< 0.1", "> 0.1")) +  
    guides(fill = guide_legend(title = "p-values")) +
    ggtitle("2021 - Anomalies - paired")
  
  test_data_grouped_2070_anomaly_spatial_plot <-
    ggplot(test_data_grouped_2070_anomaly_spatial) + 
    geom_sf(data = vg2500_krs, colour="white") + 
    geom_sf(aes(fill = cut(test_paired, c(-0.1,0.05,0.1,1), m=0) )) + 
    scale_fill_brewer(type = "seq", palette = "Blues", direction = -1,  drop = FALSE,
                      labels=c("< 0.05", "< 0.1", "> 0.1")) +  
    guides(fill = guide_legend(title="p-values")) +
    ggtitle("2070 - Anomalies- paired")
  
  test_data_grouped_2021_spatial_plot <-
    ggplot(test_data_grouped_2021_spatial) + 
    geom_sf(data = vg2500_krs, colour="white") + 
    geom_sf(aes(fill = cut(test_paired, c(-0.1,0.05,0.1,1), m=0) )) + 
    scale_fill_brewer(type="seq",palette = "Blues", direction = -1,  drop = FALSE,
                      labels=c("< 0.05", "< 0.1", "> 0.1")) +  
    guides(fill = guide_legend(title = "p-values")) +
    ggtitle("2021- paired")
  
  test_data_grouped_2070_spatial_plot <- 
    ggplot(test_data_grouped_2070_spatial) + 
    geom_sf(data = vg2500_krs, colour="white") + 
    geom_sf(aes(fill = cut(test_paired, c(-0.1,0.05,0.1,1), m=0) )) + 
    scale_fill_brewer(type = "seq", palette = "Blues", direction = -1,  drop = FALSE,
                      labels=c("< 0.05", "< 0.1", "> 0.1")) +  
    guides(fill = guide_legend(title="p-values")) +
    ggtitle("2070 - paired")
  
  ggsave(paste("./figures/figures_exploratory/Proj/Wilcoxon/", modelListMatrixNames[[s]],"/Wilcoxon_2070_anomaly_paired_",namelist_models[[l]],".pdf", sep="") ,  test_data_grouped_2070_anomaly_spatial_plot, width=16, height=9) 
  ggsave(paste("./figures/figures_exploratory/Proj/Wilcoxon/", modelListMatrixNames[[s]],"/Wilcoxon_2021_anomaly_paired_",namelist_models[[l]],".pdf", sep="") ,  test_data_grouped_2021_anomaly_spatial_plot, width=16, height=9) 
  ggsave(paste("./figures/figures_exploratory/Proj/Wilcoxon/", modelListMatrixNames[[s]],"/Wilcoxon_2070_paired_",namelist_models[[l]],".pdf", sep="") ,  test_data_grouped_2070_spatial_plot, width=16, height=9) 
  ggsave(paste("./figures/figures_exploratory/Proj/Wilcoxon/", modelListMatrixNames[[s]],"/Wilcoxon_2021_paired_",namelist_models[[l]],".pdf", sep="") ,  test_data_grouped_2021_spatial_plot, width=16, height=9) 
  
  
  }
}
   
