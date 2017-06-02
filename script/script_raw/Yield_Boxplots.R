#### Boxplots ####
' Script to make boxplots or similiar graphics which represent the distribution of the difference 
in predictions for the two climate periode  compared to the reference period.
  1) boxplot plot the difference in the means, that means the distribution is 
     defined by the average of each spatial unit. Since only means are considered, it is not 
     possible to judge about the distribution of extreme deviations. 
     Boxplot for each RCM and climate period is based on 262 observations. 
  2) Boxplot which consideres the deviations to the reference period (1971)
      For each RGM and climate period: # of regional units * 30 years = 262 * 30 = 7860 observations
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
library(reshape)
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
vg2500_krs <- read_sf("/Storage/ownCloud/Home/Klimabuero/Proj1/data//data_raw/4_michael/adminitrative_borders_Ger/", "vg2500_krs")
str(vg2500_krs, 2)

#### Change RS to five digits #####
vg2500_krs$RS <- as.integer(str_sub(vg2500_krs$RS, 1,5))
vg2500_krs$RS

## Create List of models to loop trrough##
namelist_models <- c("MPI","DMI","KNMI","ICTP","SMHIRCA")
# PredictData_df_tidy <- list(DMI=data.frame(), ICTP=data.frame(), KNMI=data.frame(), MPI=data.frame(), SMHIRCA=data.frame())


#### Generate list of start and end dates of climate periods ####
'Those are necessary for the conditioning in filter'
climateyears_list <- list(c(1971, 2021, 2070), c(2000, 2050, 2099))

## Names used to store the figures
modelListMatrixNames <- list("lm.fit_SMI_6_Jun_Aug_modelmatrix", "lm.fit_SMI_6_Jul_modelmatrix")
# modelListMatrixNames <- list("lm.fit_SMI_6_Jun_Aug", "lm.fit_SMI_6_Jul")

## Names used in figures
modelListYieldNames <-list("Yield: SMI_6_Jun_Aug", "Yield: SMI_6_Jul")

##########################################################################
#### Loop through different models on which the predictions are based ####

#### Start of loop through prediction models #####
for (s in 1:length(modelListMatrixNames)){
  
  dir.create(paste("./figures/figures_exploratory/Proj/Boxplots/", modelListMatrixNames[[s]] ,sep=""), showWarnings = FALSE)
  
  
  #### Load tidy data.frame of Yield and Yield_Anomaly Predictions  ####
  ' one large data.frame also including a marker for the model'
  PredictData_df_tidy <- read.csv(paste("./data/data_proj/output/", modelListMatrixNames[[s]],"/Yield_predict_complete_1951-2099_tidy.csv", sep="") )
  str(PredictData_df_tidy) # 195190/149/5 = 262
  
  PredictData_df_tidy$X <- NULL
  PredictData_df_tidy[1:10, ]
  PredictData_df_tidy[(1+39038):(10+39038), ]
  PredictData_df_tidy[(1+2*39038):(10+2*39038), ] # Die Modelle liefern unterschiedlicher Ergebnisse
  
  #############################################################################################
  #### Generate list with data.frame container for each climate period: Summary Statistics ####
  PredictData_df_tidy_summaries_list <- list(PredictData_df_tidy_summaries_1979 = data.frame(), PredictData_df_tidy_summaries_2021= data.frame(),
                                              PredictData_df_tidy_summaries_2070 = data.frame())
    
  #### Loop to generate data.frame with Means and SDs of Y and Y_anomaly for the three different climate zones ####
  ## Generate dummy for each climate period and create accordingly a data.frame for the subset of each climate period ##
  dummy_list <- list("1971 - 2000", "2021-2050", "2070-2099")
    
  for ( r in 1:3){
      PredictData_df_tidy_summaries_list[[r]]  <- 
        PredictData_df_tidy  %>% 
        filter(year >=  climateyears_list[[1]][r] & year <= climateyears_list[[2]][r]) %>% 
        mutate(climate_period = dummy_list[[r]])
  }
    
  View(PredictData_df_tidy_summaries_list[[1]])
  dim(PredictData_df_tidy) # 195190 /262/5 = 149
  dim(PredictData_df_tidy_summaries_list[[1]]) # The change of the dimension makes sense 39300/262/5 = 30
  str(PredictData_df_tidy_summaries_list,2)
      
  ## Loop to generate the mean and sd conditional the RCM and the administrative district
  for (r in 1:3){
      PredictData_df_tidy_summaries_list[[r]]  <- 
        PredictData_df_tidy_summaries_list[[r]]  %>%
        group_by(model, comId) %>% 
        mutate( Y_mean= mean(Y), Y_sd = sd(Y), Y_sum= sum(Y))  
  }
   
  ## Check for correctness of grouping by model
  DMI2070 <-   PredictData_df_tidy_summaries_list[[3]]  %>%
      filter(model == "DMI") 
  ICTP2070 <-   PredictData_df_tidy_summaries_list[[3]]  %>%
      filter(model == "ICTP") 
  summary(DMI2070)
  summary(ICTP2070)
      
  View(PredictData_df_tidy_summaries_list[[2]])
  summary(PredictData_df_tidy_summaries_list[[2]])
  str(PredictData_df_tidy_summaries_list[[1]],1)
  str(PredictData_df_tidy_summaries_list[[2]],1)
  str(PredictData_df_tidy_summaries_list[[3]],1)
  
  #### Append Y_mean der reference periode for each climate period ####
  for (r in 1:3){
  PredictData_df_tidy_summaries_list[[r]]$Y_mean_ref <- PredictData_df_tidy_summaries_list[[1]]$Y_mean
  }
  str(PredictData_df_tidy_summaries_list[[1]],1)
  View(PredictData_df_tidy_summaries_list[[3]])
    
  #### Create difference between Y and Y_mean_ref -> YSubY_mean_ref and between Y_mean and Y_mean_ref -> Y_meanSubY_mean_ref ####
  for (r in 1:3){
    PredictData_df_tidy_summaries_list[[r]]  <- 
        PredictData_df_tidy_summaries_list[[r]]  %>%
        mutate( YSubY_mean_ref = Y - Y_mean_ref,  Y_meanSubY_mean_ref = Y_mean - Y_mean_ref)  
  }
  View(PredictData_df_tidy_summaries_list[[2]])
    
  #### Combine data.frame to one ####
  ## Large data.frame considering all three climate period
  PredictData_df_tidy_climate <- rbind(rbind(PredictData_df_tidy_summaries_list[[1]], PredictData_df_tidy_summaries_list[[2]]), PredictData_df_tidy_summaries_list[[3]])
  ## Data.frame considering the climate periods starting 2021 and 2070
  PredictData_df_tidy_climate20212070 <- rbind(PredictData_df_tidy_summaries_list[[2]], PredictData_df_tidy_summaries_list[[3]])
    
  #####################################
  #### Make boxplot / violin plots #### 
  
  ## Define Data Summary Statistic used in ggplots
  data_summary <- function(x) {
      m <- mean(x)
      ymin <- m-sd(x)*2
      ymax <- m+sd(x)*2
      return(c(y=m,ymin=ymin,ymax=ymax))
  } # End of function
    
  #### Violin Plot for comparing the means - only considering climate period 2021 and 2070 ####
  p20212070 <- ggplot(PredictData_df_tidy_climate20212070, aes(climate_period, Y_meanSubY_mean_ref ))
  p20212070_plot <-  p20212070 + geom_hline(yintercept=0, color="gray", size=1) +
      geom_violin(aes(fill = model), draw_quantiles = c(0.25, 0.5, 0.75), width=1, color="blue")  + facet_grid(. ~ model)  +
      stat_summary(fun.data=data_summary, color="orange")   + theme_minimal(base_size = 14) +  theme(legend.position="none")  + scale_fill_brewer(palette="Greys")  +
      ggtitle(paste(modelListMatrixNames[[s]])) + ylab("Mean(Y) of climate period - Mean(Y) of reference period (cond. on climate period and adm. distr.) ") + 
      xlab("Climate Period") +
    scale_y_continuous(limits=c(-70, 40))
  ggsave(paste("./figures/figures_exploratory/Proj/Boxplots/", modelListMatrixNames[[s]],"/ViolinPlot_Means_202120170.pdf", sep="") , p20212070_plot, width=16, height=9) 
    
    
    
  #### Violin Plot for comparing the yield deviation from the reference period mean ####
  p <- ggplot(PredictData_df_tidy_climate, aes(climate_period, YSubY_mean_ref ))
  p_plot <-  p + geom_hline(yintercept=0, color="gray", size=1) +
      geom_violin(aes(fill = model), draw_quantiles = c(0.25, 0.5, 0.75), width=1, color="blue")  + facet_grid(. ~ model)  +
      stat_summary(fun.data=data_summary, color="orange")  +  theme_minimal(base_size = 14) +  theme(legend.position="none", axis.text.y = element_text(size = 15))  + scale_fill_brewer(palette="Greys")  +
      ggtitle(paste(modelListMatrixNames[[s]])) + ylab("Y of climate period - Mean(Y) of reference period ") + 
      xlab("Climate Period") +
    scale_y_continuous(limits=c(-180, 370))
  ggsave(paste("./figures/figures_exploratory/Proj/Boxplots/", modelListMatrixNames[[s]],"/ViolinPlot_Yield.pdf", sep="") , p_plot, width=16, height=9) 
 
  
  #### Check whether summaries are comparable ####   
  names(PredictData_df_tidy_summaries_list[[1]])
  
  PredictData_df_tidy_summaries_list[[2]] %>%
  group_by(model) %>%
   summarise(mean(Y), sd(Y)) 
  ' Sample show, that the summaries are the same.'
    
 
  
}   # End of loop through climate models
  
 