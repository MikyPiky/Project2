###################################################################################################
#### Make Plots of the absolute values of the periods and the differences between the periods ####
#################################################################################################

#### Description ####
'
Here I look at plots which average over all climate models ("MPI","DMI","KNMI","ICTP","SMHI")

- Loop through five climate models 
  - read in tidy data for each climate model
- Combine those to one large data.frame including all climate models
- Loop to create Means and SD of the absolute values for the climate periods (1971 - 2000, 2021 - 2050, 2070 - 2099) -> output is list of data.frames
- Create differences in mean and sd between the reference climate period (1971 - 2000) and the projections (2021 - 2050, 2070 - 2099)
- Export summary statistics of means, sd, and the difference in both via stargazer
- Produce Plots
  Plots:
    Plots of Difference in Mean or SD, climate periods (2021-2050, 2070-2099), compared to reference period (1971-2000)
    - Tav in July
    - Pre in July
    - SMI in June
    - SMI in July
    - SMI in August

    Plot of absolute values (same for Mean and SD)  for each climate period 
    - Tav in July
    - Pre in July 
    - SMI in June
    - SMI in July
    - SMI in August


    - Combined Plots
      - TavJul, PreJul, SMIJun, SMIAug
      - TavJul, PreJul, SMIJun, SMIAug, Yield
      - TavJul, PreJul, SMIJul
      - TavJul, PreJul, SMIJul, Yield
'

#### Output ####
## Plots
'/Proj2/figures/figures_exploratory/Proj/MeteoVar/'

## Descriptive Statistics of MeteoVar
'/Proj2/figures/figures_exploratory/Proj/MeteoVar/'

#### Dependencies and Input ####
'-  Meteorological Data in tidy format: MeteoMonth_df_tidy_* (* is climate model) -> /Proj2/data/data_proj/ (from KlimaMeteo_netcdfTo_sf&Tidy.R)
 - vg2500_krs -> data_proj_Input/CLC 
'

#### Packages ####
library(sp)
library(rgdal)
library(raster)
library(rasterVis)
library(maptools)
library(plyr)
library(ncdf4)
library(zoo)
library(foreign)
library(maps)
library(colorspace)
library(lattice)
library(stringr)
library(DataCombine)
library(reshape2)
library(sf)
library(dplyr)
library(readr)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(cowplot)
library(grid)
library(stargazer)


################################################################################################################################################################################
################################################################################################################################################################################

######################################################################################
#### Laden der Shapes mit den Polygonen der Kreise und deren räumliche Zuordnung ####
vg2500_krs <- read_sf("./../Proj1/data/data_spatial/", "vg2500_krs")
str(vg2500_krs, 2)

#### Change RS to five digits #####
vg2500_krs$RS <- as.integer(str_sub(vg2500_krs$RS, 1,5))
vg2500_krs$RS

## Create List ##
namelist_models <- c("DMI","ICTP", "KNMI","MPI","SMHI")
# MeteoMonth_df_tidy <- list(MPI=list(), DMI=list(), KNMI=list(), ICTP=list(), SMHI=list())


##########################################################################################################
#### Combine the Meteo_df_tidy data.frames to one large data.frame including a column for the models ####
########################################################################################################
## Create list to read in the five model data.frames ##
MeteoMonth_df_tidy_list <- list(MeteoMonth_df_tidy_summaries_MPI = data.frame(), MeteoMonth_df_tidy_DMI= data.frame(),
                                MeteoMonth_df_tidy_KNMI= data.frame(), MeteoMonth_df_tidy_ICTP = data.frame(), MeteoMonth_df_tidy_SMHI = data.frame())

#### Read in the tidy data frames of each model ####
for (l in 1:5){
  MeteoMonth_df_tidy_list[[l]]<- read.csv(paste("./data/data_proj/","MeteoMonth_df_tidy_", namelist_models[[l]],".csv", sep=""))
  MeteoMonth_df_tidy_list[[l]]$X <- NULL
  MeteoMonth_df_tidy_list[[l]]$model <- as.factor( rep(paste(namelist_models[[l]]), dim(MeteoMonth_df_tidy_list[[l]])[[1]] ) )
}
# summary(MeteoMonth_df_tidy)
str(MeteoMonth_df_tidy_list[[1]])

#### Combine data.frame ####
MeteoMonth_df_tidy_total <- rbind(rbind(rbind(rbind( MeteoMonth_df_tidy_list[[1]],  MeteoMonth_df_tidy_list[[2]]),  MeteoMonth_df_tidy_list[[3]]),  MeteoMonth_df_tidy_list[[4]]),  MeteoMonth_df_tidy_list[[5]])
str(MeteoMonth_df_tidy_total)

#############################################################################################################################
#### Loop to create Means and SD of the absolute values for the climate periods (1971 - 2000, 2021 - 2050, 2070 - 2099) ####
###########################################################################################################################

#### Generate list with data.frame container for each climate period: Mean ####
MeteoMonth_df_tidy_summaries_total_list <- list(MeteoMonth_df_tidy_summaries_total_1979 = data.frame(), MeteoMonth_df_tidy_summaries_total_2021= data.frame(),
                                                MeteoMonth_df_tidy_summaries_total_2070 = data.frame())

#### List to store sf.data.frames ####
MeteoMonth_df_tidy_summaries_total_sf_list <- list(MeteoMonth_df_tidy_summaries_total_sf_1979 = data.frame(), MeteoMonth_df_tidy_summaries_total_sf_2021= data.frame(),
                                                   MeteoMonth_df_tidy_summaries_total_sf_2070 = data.frame())



#### Start of loop  to create means and sd conditional on the reference period or the climate periods ####  
for (i in 1:3){
  #### Summarize variables ####
  MeteoMonth_df_tidy_summaries_total_list[[i]] <- MeteoMonth_df_tidy_total %>%  
    filter(year >=  climateyears_list[[1]][i] & year <= climateyears_list[[2]][i]) %>% 
    group_by(comId) %>%
    select_if(is.numeric) %>%
    summarise_all(.funs = c (Mean="mean", Sd="sd"))
  
  #### Merge with Spatial Information ####
  MeteoMonth_df_tidy_summaries_total_sf_list[[i]] <- merge(vg2500_krs, MeteoMonth_df_tidy_summaries_total_list[[i]], by.x = "RS", by.y = "comId", all.x, sort=F) 
  
} ## End of loop
str(MeteoMonth_df_tidy_summaries_total_list[[i]])


##################################################################################################################################
#### Create differences in between the reference climate period (1971 - 2000) and the projections (2021 - 2050, 2070 - 2099) ####
################################################################################################################################
names(MeteoMonth_df_tidy_summaries_total_list[[2]][,3:105])

MeteoMonth_df_tidy_summaries_total_diff2021 <- MeteoMonth_df_tidy_summaries_total_list[[2]][,3:105] - MeteoMonth_df_tidy_summaries_total_list[[1]][,3:105]
MeteoMonth_df_tidy_summaries_total_diff2070 <- MeteoMonth_df_tidy_summaries_total_list[[3]][,3:105] - MeteoMonth_df_tidy_summaries_total_list[[1]][,3:105]
str(MeteoMonth_df_tidy_summaries_total_diff2021)

MeteoMonth_df_tidy_summaries_total_diff2021$comId <- MeteoMonth_df_tidy_summaries_total_list[[2]]$comId
MeteoMonth_df_tidy_summaries_total_diff2070$comId <- MeteoMonth_df_tidy_summaries_total_list[[2]]$comId

summary(MeteoMonth_df_tidy_summaries_total_diff2021)
summary(MeteoMonth_df_tidy_summaries_total_diff2070)


##########################################################################
#### Merge difference data with vg2500_krs to get Spatial Attributes ####
MeteoMonth_df_tidy_summaries_total_diff2021_sf <- merge(vg2500_krs, MeteoMonth_df_tidy_summaries_total_diff2021, by.x = "RS", by.y = "comId", all.x, sort=F)
MeteoMonth_df_tidy_summaries_total_diff2070_sf <- merge(vg2500_krs, MeteoMonth_df_tidy_summaries_total_diff2070, by.x = "RS", by.y = "comId", all.x, sort=F)

MeteoMonth_df_tidy_summaries_total_diff2021_sf$RS ## order of vg2500_krs is kept (sort=F)

str(MeteoMonth_df_tidy_summaries_total_diff2021_sf)

summary(MeteoMonth_df_tidy_summaries_total_diff2070_sf)

####################################
#### Export summary statistics ####
##################################
stargazer(MeteoMonth_df_tidy_summaries_total_diff2021, type = "text", title="Descriptive statistics", digits=3,
          out=paste("./figures/figures_exploratory/Proj/MeteoVar/","DeskriptiveStats_diff2021_total_", "total", ".txt", sep=""))
stargazer(MeteoMonth_df_tidy_summaries_total_diff2070, type = "text", title="Descriptive statistics", digits=3,
          out=paste("./figures/figures_exploratory/Proj/MeteoVar/", "DeskriptiveStats_diff2070_total_",  "total",".txt", sep=""))
stargazer(as.data.frame(MeteoMonth_df_tidy_summaries_total_list[[1]]), type = "text", title="Descriptive statistics", digits=3,
          out=paste("./figures/figures_exploratory/Proj/MeteoVar/", "DeskriptiveStats_1970_total_", "total",".txt", sep=""))
stargazer(as.data.frame(MeteoMonth_df_tidy_summaries_total_list[[2]]), type = "text", title="Descriptive statistics", digits=3,
          out=paste("./figures/figures_exploratory/Proj/MeteoVar/", "DeskriptiveStats_2021_total_", "total",".txt", sep=""))
stargazer(as.data.frame(MeteoMonth_df_tidy_summaries_total_list[[3]]), type = "text", title="Descriptive statistics", digits=3,
          out=paste("./figures/figures_exploratory/Proj/MeteoVar/", "DeskriptiveStats_2070_total_", "total",".txt", sep=""))




##############################################################################################################################################################################
##############################################################################################################################################################################
#### Start of Plotting ####
##############################################################################################################################################################################
##############################################################################################################################################################################

##############################################################################################################################################################################
#### Plot Differences in Means ####
##############################################################################################################################################################################
' Plots of Difference in Mean , climate periods (2021-2050, 2070-2099), compared to reference period (1971-2000)
- Tav in July
- Pre in July
- SMI in June
- SMI in July
- SMI in August
'

#####################################################################
#### Plot difference in mean of climate periods of Tav in July #####
###################################################################

##### Define title used in all plots ####
title <- textGrob("All RCMs", gp=gpar(fontface="bold"))
######################################################

## Define colorRamp for July Temperature ##
summary(MeteoMonth_df_tidy_summaries_total_diff2070_sf$Tav_Jul_Mean)
summary(MeteoMonth_df_tidy_summaries_total_diff2021_sf$Tav_Jul_Mean)

myPalette <- colorRampPalette((brewer.pal(9, "Reds")))
sc <- scale_fill_gradientn("July Temp.", colours = myPalette(100), limits=c(0, 5))


#### (2070-2099) - (1971-2000) ####
plot_mean_total_diff2070_TavJul <-
  ggplot(MeteoMonth_df_tidy_summaries_total_diff2070_sf) +
  geom_sf(aes(fill = Tav_Jul_Mean)) +
  ggtitle("Mean: (2070-2099) - (1971-2000) ") + sc +
  theme_bw()
# plot_mean_total_diff2070_TavJul


#### (2021-2050) - (1971-2000) ####
plot_mean_total_diff2021_TavJul <-
  ggplot(MeteoMonth_df_tidy_summaries_total_diff2021_sf) +
  geom_sf(aes(fill = Tav_Jul_Mean)) +
  ggtitle("Mean: (2021-2050) - (1971-2000)")  + sc +
  theme_bw()
# plot_mean_total_diff2021_TavJul


plot_mean_total_diff_TavJul <- grid.arrange(plot_mean_total_diff2021_TavJul, plot_mean_total_diff2070_TavJul, ncol=2, top=title) 

# # ggsave(paste("./figures/figures_exploratory/Proj/MeteoVar/","plot_mean_diff_TavJul_", "total", ".pdf", sep=""), plot=plot_mean_total_diff_TavJul, "pdf",width=14, height=8)



#####################################################################
#### Plot difference in mean of climate periods of Pre in July #####
###################################################################

## Define colorRamp for July Temperature 
summary(MeteoMonth_df_tidy_summaries_total_diff2070_sf$Pre_Jul_Mean)
summary(MeteoMonth_df_tidy_summaries_total_diff2021_sf$Pre_Jul_Mean)

myPalette <- colorRampPalette((brewer.pal(11, "PuOr")))
sc <- scale_fill_gradientn("July Prec.",colours = myPalette(100), limits=c(-55, 55))


#### (2070-2099) - (1971-2000) ####
plot_mean_total_diff2070_PreJul <-
  ggplot(MeteoMonth_df_tidy_summaries_total_diff2070_sf) +
  geom_sf(aes(fill = Pre_Jul_Mean)) +
  ggtitle("Mean: (2070-2099) - (1971-2000) ") + sc +
  theme_bw()
# plot_mean_total_diff2070_PreJul


#### (2021-2050) - (1971-2000) ####
plot_mean_total_diff2021_PreJul <-
  ggplot(MeteoMonth_df_tidy_summaries_total_diff2021_sf) +
  geom_sf(aes(fill = Pre_Jul_Mean)) +
  ggtitle("Mean: (2021-2050) - (1971-2000)")  + sc +
  theme_bw()
# plot_mean_total_diff2021_PreJul

plot_mean_total_diff_PreJul <-grid.arrange(plot_mean_total_diff2021_PreJul, plot_mean_total_diff2070_PreJul, ncol=2, top=title ) 

# # ggsave(paste("./figures/figures_exploratory/Proj/MeteoVar/","plot_mean_diff_PreJul_", "total",".pdf", sep=""), plot=plot_mean_total_diff_PreJul, width=14, height=8)


#####################################################################
#### Plot difference in mean of climate periods of SMI in June #####
###################################################################

## Define colorRamp for June SMI ##
summary(MeteoMonth_df_tidy_summaries_total_diff2070_sf$SMI_Jun_Mean)
summary(MeteoMonth_df_tidy_summaries_total_diff2021_sf$SMI_Jun_Mean)

myPalette <- colorRampPalette((brewer.pal(11, "RdBu")))
sc <- scale_fill_gradientn("June SMI", colours = myPalette(100), limits=c(-0.4, 0.4))


#### (2070-2099) - (1971-2000) ####
plot_mean_total_diff2070_SMIJun <-
  ggplot(MeteoMonth_df_tidy_summaries_total_diff2070_sf) +
  geom_sf(aes(fill = SMI_Jun_Mean)) +
  ggtitle( "Mean: (2070-2099) - (1971-2000)") + sc +
  theme_bw()
# plot_mean_total_diff2070_SMIJun


#### (2021-2050) - (1971-2000) ####
plot_mean_total_diff2021_SMIJun <-
  ggplot(MeteoMonth_df_tidy_summaries_total_diff2021_sf) +
  geom_sf(aes(fill = SMI_Jun_Mean)) +
  ggtitle( "Mean: (2021-2050) - (1971-2000)")  + sc +
  theme_bw()
# plot_mean_total_diff2021_SMIJun

plot_mean_total_diff_SMIJun <- grid.arrange(plot_mean_total_diff2021_SMIJun, plot_mean_total_diff2070_SMIJun, ncol=2, top=title) 

# # ggsave(paste("./figures/figures_exploratory/Proj/MeteoVar/", "plot_mean_diff_SMIJun_", "total",".pdf", sep=""), plot=plot_mean_total_diff_SMIJun, width=14, height=8)


#####################################################################
#### Plot difference in mean of climate periods of SMI in July #####
###################################################################

## Define colorRamp for July SMI ##
summary(MeteoMonth_df_tidy_summaries_total_diff2070_sf$SMI_Jul_Mean)
summary(MeteoMonth_df_tidy_summaries_total_diff2021_sf$SMI_Jul_Mean)

myPalette <- colorRampPalette((brewer.pal(11, "RdBu")))
sc <- scale_fill_gradientn("July SMI", colours = myPalette(100), limits=c(-0.4, 0.4))


#### (2070-2099) - (1971-2000) ####
plot_mean_total_diff2070_SMIJul <-
  ggplot(MeteoMonth_df_tidy_summaries_total_diff2070_sf) +
  geom_sf(aes(fill = SMI_Jul_Mean)) +
  ggtitle( "Mean: (2070-2099) - (1971-2000)") + sc +
  theme_bw()
# plot_mean_total_diff2070_SMIJul


#### (2021-2050) - (1971-2000) ####
plot_mean_total_diff2021_SMIJul <-
  ggplot(MeteoMonth_df_tidy_summaries_total_diff2021_sf) +
  geom_sf(aes(fill = SMI_Jul_Mean)) +
  ggtitle( "Mean: (2021-2050) - (1971-2000)")  + sc +
  theme_bw()
# plot_mean_total_diff2021_SMIJul
plot_mean_total_diff_SMIJul <- grid.arrange(plot_mean_total_diff2021_SMIJul, plot_mean_total_diff2070_SMIJul, ncol=2, top=textGrob("All RCM" ,gp=gpar(fontsize=30)))

# # ggsave(paste("./figures/figures_exploratory/Proj/MeteoVar/", "plot_mean_diff_SMIJul_", "total",".pdf", sep=""), plot=plot_mean_total_diff_SMIJul, width=14, height=8)

#######################################################################
#### Plot difference in mean of climate periods of SMI in August #####
#####################################################################

## Define colorRamp for August SMI ##
summary(MeteoMonth_df_tidy_summaries_total_diff2070_sf$SMI_Aug_Mean)
summary(MeteoMonth_df_tidy_summaries_total_diff2021_sf$SMI_Aug_Mean)

myPalette <- colorRampPalette((brewer.pal(11, "RdBu")))
sc <- scale_fill_gradientn("August SMI", colours = myPalette(100), limits=c(-0.4, 0.4))


####  (2070-2099) - (1971-2000) ####
plot_mean_total_diff2070_SMIAug <-
  ggplot(MeteoMonth_df_tidy_summaries_total_diff2070_sf) +
  geom_sf(aes(fill = SMI_Aug_Mean)) +
  ggtitle("Mean: (2070-2099) - (1971-2000)") + sc +
  theme_bw()
# plot_mean_total_diff2070_SMIAug


####  (2021-2050) - (1971-2000) ####
plot_mean_total_diff2021_SMIAug <-
  ggplot(MeteoMonth_df_tidy_summaries_total_diff2021_sf) +
  geom_sf(aes(fill = SMI_Aug_Mean)) +
  ggtitle("Mean: (2021-2050) - (1971-2000)")  + sc +
  theme_bw()
# plot_mean_total_diff2021_SMIAug

plot_mean_total_diff_SMIAug <-grid.arrange(plot_mean_total_diff2021_SMIAug, plot_mean_total_diff2070_SMIAug, ncol=2, top=title) 

# # ggsave(paste("./figures/figures_exploratory/Proj/MeteoVar/", "plot_mean_diff_SMIAug_","total",".pdf", sep=""), plot=plot_mean_total_diff_SMIAug, width=14, height=8)



##############################################################################################################################################################################
#### Plots of absolute values ####
##############################################################################################################################################################################
'
Plot of absolute values (Mean)  for each climate period 
- Tav in July
- Pre in July 
- SMI in June
- SMI in July
- SMI in August

'
###########################################################
#### Plot Mean for each climate period of Tav in July ####
#########################################################

## Define colorRamp ##
str(MeteoMonth_df_tidy_summaries_total_sf_list,1)
summary(MeteoMonth_df_tidy_summaries_total_sf_list [[1]]$Tav_Jul_Mean)
summary(MeteoMonth_df_tidy_summaries_total_sf_list [[2]]$Tav_Jul_Mean)
summary(MeteoMonth_df_tidy_summaries_total_sf_list [[3]]$Tav_Jul_Mean)


myPalette <- colorRampPalette((brewer.pal(9, "RdPu")))
sc <- scale_fill_gradientn("July Temp.",colours = myPalette(100), limits=c(11, 24))

## Plot Mean: 1970 - 2000 - Tav July ##
plot_mean_total_1970_TavJul <-
  ggplot(MeteoMonth_df_tidy_summaries_total_sf_list [[1]]) +
  geom_sf(aes(fill = Tav_Jul_Mean)) +
  ggtitle("Mean: 1970 - 2000") + sc +
  theme_bw()

# plot_mean_total_1970_TavJul

## Plot Mean: 2021 - 2050 - Tav July ##
plot_mean_total_2021_TavJul <-
  ggplot(MeteoMonth_df_tidy_summaries_total_sf_list [[2]]) +
  geom_sf(aes(fill = Tav_Jul_Mean)) +
  ggtitle("Mean: 2021 - 2050") + sc +
  theme_bw()

# plot_mean_total_2021_TavJul

## Plot Mean: 2070 - 2099 - Tav July ##
plot_mean_total_2070_TavJul <-
  ggplot(MeteoMonth_df_tidy_summaries_total_sf_list [[3]]) +
  geom_sf(aes(fill = Tav_Jul_Mean)) +
  ggtitle("Mean: 2070 - 2099") + sc +
  theme_bw()

# plot_mean_total_2070_TavJul

plot_mean_TavJul <- grid.arrange(plot_mean_total_1970_TavJul, plot_mean_total_2021_TavJul, plot_mean_total_2070_TavJul, ncol=3, top=title) 
# plot_mean_TavJul

# # ggsave(paste("./figures/figures_exploratory/Proj/MeteoVar/", "plot_mean_TavJul_","total",".pdf", sep=""), plot=plot_mean_TavJul, width=21, height=8)

###########################################################
#### Plot Mean for each climate period of Pre in July ####
#########################################################

## Define colorRamp ##
str(MeteoMonth_df_tidy_summaries_total_sf_list,1)
summary(MeteoMonth_df_tidy_summaries_total_sf_list [[1]]$Pre_Jul_Mean)
summary(MeteoMonth_df_tidy_summaries_total_sf_list [[2]]$Pre_Jul_Mean)
summary(MeteoMonth_df_tidy_summaries_total_sf_list [[3]]$Pre_Jul_Mean)


myPalette <- colorRampPalette((brewer.pal(9, "PuBu")))
sc <- scale_fill_gradientn("July Prec.",colours = myPalette(100), limits=c(20, 220))

## Plot Mean: 1970 - 2000 - Tav July ##
plot_mean_total_1970_PreJul <-
  ggplot(MeteoMonth_df_tidy_summaries_total_sf_list [[1]]) +
  geom_sf(aes(fill = Pre_Jul_Mean)) +
  ggtitle("Mean: 1970 - 2000") + sc +
  theme_bw()

# plot_mean_total_1970_PreJul

## Plot Mean: 2021 - 2050 - Tav July ##
plot_mean_total_2021_PreJul <-
  ggplot(MeteoMonth_df_tidy_summaries_total_sf_list [[2]]) +
  geom_sf(aes(fill = Pre_Jul_Mean)) +
  ggtitle("Mean: 2021 - 2050") + sc +
  theme_bw()

# plot_mean_total_2021_PreJul

## Plot Mean: 2070 - 2099 - Tav July ##
plot_mean_total_2070_PreJul <-
  ggplot(MeteoMonth_df_tidy_summaries_total_sf_list [[3]]) +
  geom_sf(aes(fill = Pre_Jul_Mean)) +
  ggtitle("Mean: 2070 - 2099") + sc +
  theme_bw()

# plot_mean_total_2070_PreJul

## Combine all three plot
plot_mean_PreJul <- grid.arrange(plot_mean_total_1970_PreJul , plot_mean_total_2021_PreJul , plot_mean_total_2070_PreJul , ncol=3, top=title) 
# plot_mean_PreJul

# # ggsave(paste("./figures/figures_exploratory/Proj/MeteoVar/","plot_mean_PreJul_", "total",".pdf", sep=""), plot=plot_mean_PreJul , width=21, height=8)

###########################################################
#### Plot Mean for each climate period of SMI in June ####
#########################################################

## Define colorRamp ##
str(MeteoMonth_df_tidy_summaries_total_sf_list,1)
summary(MeteoMonth_df_tidy_summaries_total_sf_list [[1]]$SMI_Jun_Mean)
summary(MeteoMonth_df_tidy_summaries_total_sf_list [[2]]$SMI_Jun_Mean)
summary(MeteoMonth_df_tidy_summaries_total_sf_list [[3]]$SMI_Jun_Mean)

myPalette <- colorRampPalette((brewer.pal(9, "YlGnBu")))
sc <- scale_fill_gradientn("June SMI",colours = myPalette(100), limits=c(0.3, 0.7))

## Plot Mean: 1970 - 2000 - Tav July ##
plot_mean_total_1970_SMIJun <-
  ggplot(MeteoMonth_df_tidy_summaries_total_sf_list [[1]]) +
  geom_sf(aes(fill = SMI_Jun_Mean)) +
  ggtitle("Mean: 1970 - 2000") + sc +
  theme_bw()

# plot_mean_total_1970_SMIJun

## Plot Mean: 2021 - 2050 - Tav July ##
plot_mean_total_2021_SMIJun <-
  ggplot(MeteoMonth_df_tidy_summaries_total_sf_list [[2]]) +
  geom_sf(aes(fill = SMI_Jun_Mean)) +
  ggtitle("Mean: 2021 - 2050") + sc +
  theme_bw()

# plot_mean_total_2021_SMIJun

## Plot Mean: 2070 - 2099 - Tav July ##
plot_mean_total_2070_SMIJun <-
  ggplot(MeteoMonth_df_tidy_summaries_total_sf_list [[3]]) +
  geom_sf(aes(fill = SMI_Jun_Mean)) +
  ggtitle("Mean: 2070 - 2099") + sc +
  theme_bw()

# plot_mean_total_2070_SMIJun


## Combine all three plot
plot_mean_SMIJun <- grid.arrange(plot_mean_total_1970_SMIJun , plot_mean_total_2021_SMIJun , plot_mean_total_2070_SMIJun , ncol=3, top=title) 
# plot_mean_SMIJun

# # ggsave(paste("./figures/figures_exploratory/Proj/MeteoVar/","plot_mean_SMIJun_", "total",".pdf", sep=""), plot=plot_mean_SMIJun , width=21, height=8)

###########################################################
#### Plot Mean for each climate period of SMI in July ####
#########################################################

## Define colorRamp ##
str(MeteoMonth_df_tidy_summaries_total_sf_list,1)
summary(MeteoMonth_df_tidy_summaries_total_sf_list [[1]]$SMI_Jul_Mean)
summary(MeteoMonth_df_tidy_summaries_total_sf_list [[2]]$SMI_Jul_Mean)
summary(MeteoMonth_df_tidy_summaries_total_sf_list [[3]]$SMI_Jul_Mean)

myPalette <- colorRampPalette((brewer.pal(9, "YlGnBu")))
sc <- scale_fill_gradientn("July SMI",colours = myPalette(100), limits=c(0.3, 0.7))

## Plot Mean: 1970 - 2000 - Tav July ##
plot_mean_total_1970_SMIJul <-
  ggplot(MeteoMonth_df_tidy_summaries_total_sf_list [[1]]) +
  geom_sf(aes(fill = SMI_Jul_Mean)) +
  ggtitle("Mean: 1970 - 2000") + sc +
  theme_bw()

# plot_mean_total_1970_SMIJul

## Plot Mean: 2021 - 2050 - Tav July ##
plot_mean_total_2021_SMIJul <-
  ggplot(MeteoMonth_df_tidy_summaries_total_sf_list [[2]]) +
  geom_sf(aes(fill = SMI_Jul_Mean)) +
  ggtitle("Mean: 2021 - 2050") + sc +
  theme_bw()

# plot_mean_total_2021_SMIJul

## Plot Mean: 2070 - 2099 - Tav July ##
plot_mean_total_2070_SMIJul <-
  ggplot(MeteoMonth_df_tidy_summaries_total_sf_list [[3]]) +
  geom_sf(aes(fill = SMI_Jul_Mean)) +
  ggtitle("Mean: 2070 - 2099") + sc +
  theme_bw()

# plot_mean_total_2070_SMIJul


## Combine all three plot
plot_mean_SMIJul <- grid.arrange(plot_mean_total_1970_SMIJul , plot_mean_total_2021_SMIJul , plot_mean_total_2070_SMIJul , ncol=3, top=title) 
# plot_mean_SMIJul

# # ggsave(paste("./figures/figures_exploratory/Proj/MeteoVar/","plot_mean_SMIJul_", "total",".pdf", sep=""), plot=plot_mean_SMIJul , width=21, height=8)


#############################################################
#### Plot Mean for each climate period of SMI in August ####
###########################################################

## Define colorRamp ##
str(MeteoMonth_df_tidy_summaries_total_sf_list,1)
summary(MeteoMonth_df_tidy_summaries_total_sf_list [[1]]$SMI_Aug_Mean)
summary(MeteoMonth_df_tidy_summaries_total_sf_list [[2]]$SMI_Aug_Mean)
summary(MeteoMonth_df_tidy_summaries_total_sf_list [[3]]$SMI_Aug_Mean)

myPalette <- colorRampPalette((brewer.pal(9, "YlGnBu")))
sc <- scale_fill_gradientn("August SMI",colours = myPalette(100), limits=c(0.3, 0.7))

## Plot Mean: 1970 - 2000 - Tav July ##
plot_mean_total_1970_SMIAug <-
  ggplot(MeteoMonth_df_tidy_summaries_total_sf_list [[1]]) +
  geom_sf(aes(fill = SMI_Aug_Mean)) +
  ggtitle("Mean: 1970 - 2000") + sc +
  theme_bw()

# plot_mean_total_1970_SMIAug

## Plot Mean: 2021 - 2050 - Tav July ##
plot_mean_total_2021_SMIAug <-
  ggplot(MeteoMonth_df_tidy_summaries_total_sf_list [[2]]) +
  geom_sf(aes(fill = SMI_Aug_Mean)) +
  ggtitle("Mean: 2021 - 2050") + sc +
  theme_bw()

# plot_mean_total_2021_SMIAug

## Plot Mean: 2070 - 2099 - Tav July ##
plot_mean_total_2070_SMIAug <-
  ggplot(MeteoMonth_df_tidy_summaries_total_sf_list [[3]]) +
  geom_sf(aes(fill = SMI_Aug_Mean)) +
  ggtitle("Mean: 2070 - 2099") + sc +
  theme_bw()

# plot_mean_total_2070_SMIAug


## Combine all three plot
plot_mean_SMIAug <- grid.arrange(plot_mean_total_1970_SMIAug , plot_mean_total_2021_SMIAug , plot_mean_total_2070_SMIAug , ncol=3, top=title) 
# plot_mean_SMIAug

# # ggsave(paste("./figures/figures_exploratory/Proj/MeteoVar/","plot_mean_SMIAug_", "total",".pdf", sep=""), plot=plot_mean_SMIAug , width=21, height=8)

################################################################################################################################################################################################
#### Make combined Plots of Means ####
################################################################################################################################################################################################
' For the yield plots it is necessary to load plot_mean_diff...list via the BasePrediction_Plots Script. '


'Combined Plots
- TavJul, PreJul, SMIJun, SMIAug
- TavJul, PreJul, SMIJun, SMIAug, Yield
- TavJul, PreJul, SMIJul
- TavJul, PreJul, SMIJul, Yield'

'For the yield plots it is necessary to load plot_sd_total_diff...list via the BasePrediction_Plots Script. '

#######################################################
#### Mean plots for TavJul, PreJul, SMIJun, SMIAug ####
plot_mean_SMI_6_Jun_Aug <- grid.arrange(plot_mean_total_1970_TavJul , plot_mean_total_1970_PreJul,  plot_mean_total_1970_SMIJun, plot_mean_total_1970_SMIAug,
                                        plot_mean_total_diff2021_TavJul, plot_mean_total_diff2021_PreJul, plot_mean_total_diff2021_SMIJun, plot_mean_total_diff2021_SMIAug,
                                        plot_mean_total_diff2070_TavJul, plot_mean_total_diff2070_PreJul, plot_mean_total_diff2070_SMIJun, plot_mean_total_diff2070_SMIAug,
                                        ncol=4, top=title) 
plot_mean_SMI_6_Jun_Aug
# # ggsave(paste("./figures/figures_exploratory/Proj/MeteoVar/","plot_mean_SMI_6_Jun_Aug_", "total",".pdf", sep=""), plot=plot_mean_SMI_6_Jun_Aug , width=28, height=24)

# ##############################################################
# #### Mean plots for TavJul, PreJul, SMIJun, SMIAug, Yield ####
# plot_mean_yield_SMI_6_Jun_Aug <- grid.arrange(plot_mean_total_1970_TavJul , plot_mean_total_1970_PreJul,  plot_mean_total_1970_SMIJun, plot_mean_total_1970_SMIAug,plot_mean_total_1971_list[[1]][[l]],
#                                               plot_mean_total_diff2021_TavJul, plot_mean_total_diff2021_PreJul, plot_mean_total_diff2021_SMIJun, plot_mean_total_diff2021_SMIAug, plot_mean_total_diff2021_list[[1]][[l]],
#                                               plot_mean_total_diff2070_TavJul, plot_mean_total_diff2070_PreJul, plot_mean_total_diff2070_SMIJun, plot_mean_total_diff2070_SMIAug, plot_mean_total_diff2070_list[[1]][[l]],
#                                               ncol = 5, top=textGrob("All RCM"),gp=gpar(fontsize=25)))
# plot_mean_yield_SMI_6_Jun_Aug
# 
# # # ggsave(paste("./figures/figures_exploratory/Proj/Combined/","plot_mean_yield_SMI_6_Jun_Aug_", "total",".pdf", sep=""), plot=plot_mean_yield_SMI_6_Jun_Aug , width=35, height=24)

###############################################
#### Mean plots for TavJul, PreJul, SMIJul ####
plot_mean_SMI_6_Jul <- grid.arrange(plot_mean_total_1970_TavJul , plot_mean_total_1970_PreJul,  plot_mean_total_1970_SMIJul,
                                    plot_mean_total_diff2021_TavJul, plot_mean_total_diff2021_PreJul, plot_mean_total_diff2021_SMIJul,
                                    plot_mean_total_diff2070_TavJul, plot_mean_total_diff2070_PreJul, plot_mean_total_diff2070_SMIJul,
                                    ncol=3, top=title)  
plot_mean_SMI_6_Jul
# # ggsave(paste("./figures/figures_exploratory/Proj/MeteoVar/","plot_mean_SMI_6_Jul_", "total",".pdf", sep=""), plot=plot_mean_SMI_6_Jul , width=28, height=24)

# ######################################################
# #### Mean plots for TavJul, PreJul, SMIJul, Yield ####
# plot_mean_yield_SMI_6_Jul <- grid.arrange(plot_mean_total_1970_TavJul , plot_mean_total_1970_PreJul,  plot_mean_total_1970_SMIJul, plot_mean_total_1971_list[[2]][[l]],
#                                           plot_mean_total_diff2021_TavJul, plot_mean_total_diff2021_PreJul, plot_mean_total_diff2021_SMIJul,  plot_mean_total_diff2021_list[[2]][[l]],
#                                           plot_mean_total_diff2070_TavJul, plot_mean_total_diff2070_PreJul, plot_mean_total_diff2070_SMIJul, plot_mean_total_diff2070_list[[2]][[l]],
#                                           ncol = 4, top=textGrob("All RCM"),gp=gpar(fontsize=20)))
# plot_mean_yield_SMI_6_Jul

# # # ggsave(paste("./figures/figures_exploratory/Proj/Combined/","plot_mean_yield_SMI_6_Jul_", "total",".pdf", sep=""), plot=plot_mean_yield_SMI_6_Jul , width=28, height=24)



##############################################################################################################################################################################
##############################################################################################################################################################################
#### Plots of SDs ####
##############################################################################################################################################################################
'
Plot of absolute values (SD)  for each climate period 
- Tav in July
- Pre in July 
- SMI in June
- SMI in July
- SMI in August

'
#########################################################
#### Plot SD for each climate period of Tav in July ####
#######################################################

## Define colorRamp ##
str(MeteoMonth_df_tidy_summaries_total_sf_list,1)
summary(MeteoMonth_df_tidy_summaries_total_sf_list [[1]]$Tav_Jul_Sd)
summary(MeteoMonth_df_tidy_summaries_total_sf_list [[2]]$Tav_Jul_Sd)
summary(MeteoMonth_df_tidy_summaries_total_sf_list [[3]]$Tav_Jul_Sd)


myPalette <- colorRampPalette((brewer.pal(9, "RdPu")))
sc <- scale_fill_gradientn("July Temp.", colours = myPalette(100), limits=c(0, 2.5))

## Plot Standard Deviation: 1970 - 2000 - Tav July ##
plot_sd_total_1970_TavJul <-
  ggplot(MeteoMonth_df_tidy_summaries_total_sf_list [[1]]) +
  geom_sf(aes(fill = Tav_Jul_Sd)) +
  ggtitle("SD: 1970 - 2000") + sc +
  theme_bw()

plot_sd_total_1970_TavJul

## Plot Standard Deviation: 2021 - 2050 - Tav July ##
plot_sd_total_2021_TavJul <-
  ggplot(MeteoMonth_df_tidy_summaries_total_sf_list [[2]]) +
  geom_sf(aes(fill = Tav_Jul_Sd)) +
  ggtitle("SD: 2021 - 2050") + sc +
  theme_bw()

plot_sd_total_2021_TavJul

## Plot Standard Deviation: 2070 - 2099 - Tav July ##
plot_sd_total_2070_TavJul <-
  ggplot(MeteoMonth_df_tidy_summaries_total_sf_list [[3]]) +
  geom_sf(aes(fill = Tav_Jul_Sd)) +
  ggtitle("SD: 2070 - 2099") + sc +
  theme_bw()

plot_sd_total_2070_TavJul

plot_sd_TavJul <- grid.arrange(plot_sd_total_1970_TavJul, plot_sd_total_2021_TavJul, plot_sd_total_2070_TavJul, ncol=3, top=title) 
plot_sd_TavJul

# # ggsave(paste("./figures/figures_exploratory/Proj/MeteoVar/","plot_sd_TavJul_", "total",".pdf", sep=""), plot=plot_sd_TavJul, width=21, height=8)

#########################################################
#### Plot SD for each climate period of Pre in July ####
#######################################################

## Define colorRamp ##
str(MeteoMonth_df_tidy_summaries_total_sf_list,1)
summary(MeteoMonth_df_tidy_summaries_total_sf_list [[1]]$Pre_Jul_Sd)
summary(MeteoMonth_df_tidy_summaries_total_sf_list [[2]]$Pre_Jul_Sd)
summary(MeteoMonth_df_tidy_summaries_total_sf_list [[3]]$Pre_Jul_Sd)


myPalette <- colorRampPalette((brewer.pal(9, "PuBu")))
sc <- scale_fill_gradientn("July Prec.",colours = myPalette(100), limits=c(0, 100))

## Plot Standard Deviation: 1970 - 2000 - Tav July ##
plot_sd_total_1970_PreJul <-
  ggplot(MeteoMonth_df_tidy_summaries_total_sf_list [[1]]) +
  geom_sf(aes(fill = Pre_Jul_Sd)) +
  ggtitle("SD: 1970 - 2000") + sc +
  theme_bw()

plot_sd_total_1970_PreJul

## Plot Standard Deviation: 2021 - 2050 - Tav July ##
plot_sd_total_2021_PreJul <-
  ggplot(MeteoMonth_df_tidy_summaries_total_sf_list [[2]]) +
  geom_sf(aes(fill = Pre_Jul_Sd)) +
  ggtitle("SD: 2021 - 2050") + sc +
  theme_bw()

plot_sd_total_2021_PreJul

## Plot Standard Deviation: 2070 - 2099 - Tav July ##
plot_sd_total_2070_PreJul <-
  ggplot(MeteoMonth_df_tidy_summaries_total_sf_list [[3]]) +
  geom_sf(aes(fill = Pre_Jul_Sd)) +
  ggtitle("SD: 2070 - 2099") + sc +
  theme_bw()

plot_sd_total_2070_PreJul

## Combine all three plot
plot_sd_PreJul <- grid.arrange(plot_sd_total_1970_PreJul , plot_sd_total_2021_PreJul , plot_sd_total_2070_PreJul , ncol=3, top=title) 
plot_sd_PreJul

# # ggsave(paste("./figures/figures_exploratory/Proj/MeteoVar/","plot_sd_PreJul_", "total",".pdf", sep=""), plot=plot_sd_PreJul , width=21, height=8)


###########################################################
#### Plot Sd for each climate period of SMI in June ####
#########################################################

## Define colorRamp ##
str(MeteoMonth_df_tidy_summaries_total_sf_list,1)
summary(MeteoMonth_df_tidy_summaries_total_sf_list [[1]]$SMI_Jun_Sd)
summary(MeteoMonth_df_tidy_summaries_total_sf_list [[2]]$SMI_Jun_Sd)
summary(MeteoMonth_df_tidy_summaries_total_sf_list [[3]]$SMI_Jun_Sd)


myPalette <- colorRampPalette((brewer.pal(9, "GnBu")))
sc <- scale_fill_gradientn("June SMI", colours = myPalette(100), limits=c(0.1, 0.4))

## Plot SD: 1970 - 2000 - Tav July ##
plot_sd_total_1970_SMIJun <-
  ggplot(MeteoMonth_df_tidy_summaries_total_sf_list [[1]]) +
  geom_sf(aes(fill = SMI_Jun_Sd)) +
  ggtitle("SD: 1970 - 2000") + sc +
  theme_bw()

plot_sd_total_1970_SMIJun

## Plot SD: 2021 - 2050 - Tav July ##
plot_sd_total_2021_SMIJun <-
  ggplot(MeteoMonth_df_tidy_summaries_total_sf_list [[2]]) +
  geom_sf(aes(fill = SMI_Jun_Sd)) +
  ggtitle("SD: 2021 - 2050") + sc +
  theme_bw()

plot_sd_total_2021_SMIJun

## Plot SD: 2070 - 2099 - Tav July ##
plot_sd_total_2070_SMIJun <-
  ggplot(MeteoMonth_df_tidy_summaries_total_sf_list [[3]]) +
  geom_sf(aes(fill = SMI_Jun_Sd)) +
  ggtitle("SD: 2070 - 2099") + sc +
  theme_bw()

plot_sd_total_2070_SMIJun


## Combine all three plot
plot_sd_SMIJun <- grid.arrange(plot_sd_total_1970_SMIJun , plot_sd_total_2021_SMIJun , plot_sd_total_2070_SMIJun , ncol=3, top=title) 
plot_sd_SMIJun

# # ggsave(paste("./figures/figures_exploratory/Proj/MeteoVar/","plot_sd_SMIJun_", "total",".pdf", sep=""), plot=plot_sd_SMIJun , width=21, height=8)

###########################################################
#### Plot Sd for each climate period of SMI in July ####
#########################################################

## Define colorRamp ##
str(MeteoMonth_df_tidy_summaries_total_sf_list,1)
summary(MeteoMonth_df_tidy_summaries_total_sf_list [[1]]$SMI_Jul_Sd)
summary(MeteoMonth_df_tidy_summaries_total_sf_list [[2]]$SMI_Jul_Sd)
summary(MeteoMonth_df_tidy_summaries_total_sf_list [[3]]$SMI_Jul_Sd)


myPalette <- colorRampPalette((brewer.pal(9, "GnBu")))
sc <- scale_fill_gradientn("July SMI", colours = myPalette(100), limits=c(0.1, 0.4))

## Plot SD: 1970 - 2000 - Tav July ##
plot_sd_total_1970_SMIJul <-
  ggplot(MeteoMonth_df_tidy_summaries_total_sf_list [[1]]) +
  geom_sf(aes(fill = SMI_Jul_Sd)) +
  ggtitle("SD: 1970 - 2000") + sc +
  theme_bw()

plot_sd_total_1970_SMIJul

## Plot SD: 2021 - 2050 - Tav July ##
plot_sd_total_2021_SMIJul <-
  ggplot(MeteoMonth_df_tidy_summaries_total_sf_list [[2]]) +
  geom_sf(aes(fill = SMI_Jul_Sd)) +
  ggtitle("SD: 2021 - 2050") + sc +
  theme_bw()

plot_sd_total_2021_SMIJul

## Plot SD: 2070 - 2099 - Tav July ##
plot_sd_total_2070_SMIJul <-
  ggplot(MeteoMonth_df_tidy_summaries_total_sf_list [[3]]) +
  geom_sf(aes(fill = SMI_Jul_Sd)) +
  ggtitle("SD: 2070 - 2099") + sc +
  theme_bw()

plot_sd_total_2070_SMIJul


## Combine all three plot
plot_sd_SMIJul <- grid.arrange(plot_sd_total_1970_SMIJul , plot_sd_total_2021_SMIJul , plot_sd_total_2070_SMIJul , ncol=3, top=title) 
plot_sd_SMIJul

# # ggsave(paste("./figures/figures_exploratory/Proj/MeteoVar/","plot_sd_SMIJul_", "total",".pdf", sep=""), plot=plot_sd_SMIJul , width=21, height=8)

#############################################################
#### Plot Sd for each climate period of SMI in August ####
###########################################################

## Define colorRamp ##
str(MeteoMonth_df_tidy_summaries_total_sf_list,1)
summary(MeteoMonth_df_tidy_summaries_total_sf_list [[1]]$SMI_Aug_Sd)
summary(MeteoMonth_df_tidy_summaries_total_sf_list [[2]]$SMI_Aug_Sd)
summary(MeteoMonth_df_tidy_summaries_total_sf_list [[3]]$SMI_Aug_Sd)


myPalette <- colorRampPalette((brewer.pal(9, "GnBu")))
sc <- scale_fill_gradientn(colours = myPalette(100), limits=c(0.1, 0.4))

## Plot SD: 1970 - 2000 - Tav July ##
plot_sd_total_1970_SMIAug <-
  ggplot(MeteoMonth_df_tidy_summaries_total_sf_list [[1]]) +
  geom_sf(aes(fill = SMI_Aug_Sd)) +
  ggtitle("SD: 1970 - 2000") + sc +
  theme_bw()

plot_sd_total_1970_SMIAug

## Plot SD: 2021 - 2050 - Tav July ##
plot_sd_total_2021_SMIAug <-
  ggplot(MeteoMonth_df_tidy_summaries_total_sf_list [[2]]) +
  geom_sf(aes(fill = SMI_Aug_Sd)) +
  ggtitle("SD: 2021 - 2050") + sc +
  theme_bw()

plot_sd_total_2021_SMIAug

## Plot SD: 2070 - 2099 - Tav July ##
plot_sd_total_2070_SMIAug <-
  ggplot(MeteoMonth_df_tidy_summaries_total_sf_list [[3]]) +
  geom_sf(aes(fill = SMI_Aug_Sd)) +
  ggtitle("SD: 2070 - 2099") + sc +
  theme_bw()

plot_sd_total_2070_SMIAug


## Combine all three plot
plot_sd_SMIAug <- grid.arrange(plot_sd_total_1970_SMIAug , plot_sd_total_2021_SMIAug , plot_sd_total_2070_SMIAug , ncol=3, top=title) 
plot_sd_SMIAug

# # ggsave(paste("./figures/figures_exploratory/Proj/MeteoVar/","plot_sd_SMIAug_", "total",".pdf", sep=""), plot=plot_sd_SMIAug , width=21, height=8)


##############################################################################################################################################################################
#### Plot Differences in SD ####
##############################################################################################################################################################################
' Plots of Difference in SD , climate periods (2021-2050, 2070-2099), compared to reference period (1971-2000)
- Tav in July
- Pre in July
- SMI in June
- SMI in July
- SMI in August
'
#####################################################################
#### Plot difference in Sd of climate periods of Tav in July #####
###################################################################

#### Define colorRamp for July Temperature ####
summary(MeteoMonth_df_tidy_summaries_total_diff2070_sf$Tav_Jul_Sd)
summary(MeteoMonth_df_tidy_summaries_total_diff2021_sf$Tav_Jul_Sd)

myPalette <- colorRampPalette((brewer.pal(9, "PRGn")))
sc <- scale_fill_gradientn("July Temp.", colours = myPalette(100), limits=c(-0.7, 0.7))

#### Plot Sd of Sd (2070-2099) - (1971-2000) ####
plot_sd_total_diff2070_TavJul <-
  ggplot(MeteoMonth_df_tidy_summaries_total_diff2070_sf) +
  geom_sf(aes(fill = Tav_Jul_Sd)) +
  ggtitle("SD: (2070-2099) - (1971-2000) ") + sc +
  theme_bw()
plot_sd_total_diff2070_TavJul

#### Plot Sdof Sd (2021-2050) - (1971-2000) ####
plot_sd_total_diff2021_TavJul <-
  ggplot(MeteoMonth_df_tidy_summaries_total_diff2021_sf) +
  geom_sf(aes(fill = Tav_Jul_Sd)) +
  ggtitle("SD: (2021-2050) - (1971-2000)")  + sc +
  theme_bw()
plot_sd_total_diff2021_TavJul

plot_sd_total_diff_TavJul <-grid.arrange(plot_sd_total_diff2021_TavJul, plot_sd_total_diff2070_TavJul, ncol=2, top=title) 
plot_sd_total_diff_TavJul

# # ggsave(paste("./figures/figures_exploratory/Proj/MeteoVar/","plot_sd_diff_TavJul_", "total", ".pdf", sep=""), plot=plot_sd_total_diff_TavJul, "pdf",width=14, height=8)



#####################################################################
#### Plot difference in Sd of climate periods of Pre in July #####
###################################################################

#### Define colorRamp for July Temperature ####
summary(MeteoMonth_df_tidy_summaries_total_diff2070_sf$Pre_Jul_Sd)
summary(MeteoMonth_df_tidy_summaries_total_diff2021_sf$Pre_Jul_Sd)

myPalette <- colorRampPalette((brewer.pal(11, "PRGn")))
sc <- scale_fill_gradientn("July Prec.",colours = myPalette(100), limits=c(-25, 25))

#### Plot Difference of Sd (2070-2099) - (1971-2000) ####
plot_sd_total_diff2070_PreJul <-
  ggplot(MeteoMonth_df_tidy_summaries_total_diff2070_sf) +
  geom_sf(aes(fill = Pre_Jul_Sd)) +
  ggtitle("SD: (2070-2099) - (1971-2000) ") + sc +
  theme_bw()
plot_sd_total_diff2070_PreJul

#### Plot Difference of Sd (2021-2050) - (1971-2000) ####
plot_sd_total_diff2021_PreJul <-
  ggplot(MeteoMonth_df_tidy_summaries_total_diff2021_sf) +
  geom_sf(aes(fill = Pre_Jul_Sd)) +
  ggtitle("SD: (2021-2050) - (1971-2000)")  + sc +
  theme_bw()
plot_sd_total_diff2021_PreJul

plot_sd_total_diff_PreJul <-grid.arrange(plot_sd_total_diff2021_PreJul, plot_sd_total_diff2070_PreJul, ncol=2, top=title) 

# # ggsave(paste("./figures/figures_exploratory/Proj/MeteoVar/","plot_sd_diff_PreJul_", "total",".pdf", sep=""), plot=plot_sd_total_diff_PreJul, width=14, height=8)


#####################################################################
#### Plot difference in Sd of climate periods of SMI in June #####
###################################################################

#### Define colorRamp for June SMI ####
summary(MeteoMonth_df_tidy_summaries_total_diff2070_sf$SMI_Jun_Sd)
summary(MeteoMonth_df_tidy_summaries_total_diff2021_sf$SMI_Jun_Sd)

myPalette <- colorRampPalette((brewer.pal(11, "PRGn")))
sc <- scale_fill_gradientn("June SMI", colours = myPalette(100), limits=c(-0.25, 0.25))

#### Plot Difference of Sd (2070-2099) - (1971-2000) ####
plot_sd_total_diff2070_SMIJun <-
  ggplot(MeteoMonth_df_tidy_summaries_total_diff2070_sf) +
  geom_sf(aes(fill = SMI_Jun_Sd)) +
  ggtitle( "SD: (2070-2099) - (1971-2000)") + sc +
  theme_bw()
plot_sd_total_diff2070_SMIJun

#### Plot Difference of Sd (2021-2050) - (1971-2000) ####
plot_sd_total_diff2021_SMIJun <-
  ggplot(MeteoMonth_df_tidy_summaries_total_diff2021_sf) +
  geom_sf(aes(fill = SMI_Jun_Sd)) +
  ggtitle( "SD: (2021-2050) - (1971-2000)")  + sc +
  theme_bw()
plot_sd_total_diff2021_SMIJun

plot_sd_total_diff_SMIJun <- grid.arrange(plot_sd_total_diff2021_SMIJun, plot_sd_total_diff2070_SMIJun, ncol=2, top=title) 

# # ggsave(paste("./figures/figures_exploratory/Proj/MeteoVar/", "plot_sd_diff_SMIJun_", "total",".pdf", sep=""), plot=plot_sd_total_diff_SMIJun, width=14, height=8)

###################################################################
#### Plot difference in Sd of climate periods of SMI in July #####
#################################################################

#### Define colorRamp for July SMI ####
summary(MeteoMonth_df_tidy_summaries_total_diff2070_sf$SMI_Jul_Sd)
summary(MeteoMonth_df_tidy_summaries_total_diff2021_sf$SMI_Jul_Sd)

myPalette <- colorRampPalette((brewer.pal(11, "PRGn")))
sc <- scale_fill_gradientn("July SMI", colours = myPalette(100), limits=c(-0.25, 0.25))

#### Plot Difference of Sd (2070-2099) - (1971-2000) ####
plot_sd_total_diff2070_SMIJul <-
  ggplot(MeteoMonth_df_tidy_summaries_total_diff2070_sf) +
  geom_sf(aes(fill = SMI_Jul_Sd)) +
  ggtitle( "SD: (2070-2099) - (1971-2000)") + sc +
  theme_bw()
plot_sd_total_diff2070_SMIJul

#### Plot Difference of Sd (2021-2050) - (1971-2000) ####
plot_sd_total_diff2021_SMIJul <-
  ggplot(MeteoMonth_df_tidy_summaries_total_diff2021_sf) +
  geom_sf(aes(fill = SMI_Jul_Sd)) +
  ggtitle( "SD: (2021-2050) - (1971-2000)")  + sc +
  theme_bw()
plot_sd_total_diff2021_SMIJul

plot_sd_total_diff_SMIJul <- grid.arrange(plot_sd_total_diff2021_SMIJul, plot_sd_total_diff2070_SMIJul, ncol=2, top=title) 

# # ggsave(paste("./figures/figures_exploratory/Proj/MeteoVar/", "plot_sd_diff_SMIJul_", "total",".pdf", sep=""), plot=plot_sd_total_diff_SMIJul, width=14, height=8)

#######################################################################
#### Plot difference in Sd of climate periods of SMI in August #####
#####################################################################

#### Define colorRamp for August SMI ####
summary(MeteoMonth_df_tidy_summaries_total_diff2070_sf$SMI_Aug_Sd)
summary(MeteoMonth_df_tidy_summaries_total_diff2021_sf$SMI_Aug_Sd)

myPalette <- colorRampPalette((brewer.pal(11, "PRGn")))
sc <- scale_fill_gradientn("August SMI", colours = myPalette(100), limits=c(-0.25, 0.25))


#### Plot Difference of Sd (2070-2099) - (1971-2000) ####
plot_sd_total_diff2070_SMIAug <-
  ggplot(MeteoMonth_df_tidy_summaries_total_diff2070_sf) +
  geom_sf(aes(fill = SMI_Aug_Sd)) +
  ggtitle("SD: (2070-2099) - (1971-2000)") + sc +
  theme_bw()
plot_sd_total_diff2070_SMIAug

#### Plot Difference of Sd (2021-2050) - (1971-2000) ####
plot_sd_total_diff2021_SMIAug <-
  ggplot(MeteoMonth_df_tidy_summaries_total_diff2021_sf) +
  geom_sf(aes(fill = SMI_Aug_Sd)) +
  ggtitle("SD: (2021-2050) - (1971-2000)")  + sc +
  theme_bw()
plot_sd_total_diff2021_SMIAug

plot_sd_total_diff_SMIAug <-grid.arrange(plot_sd_total_diff2021_SMIAug, plot_sd_total_diff2070_SMIAug, ncol=2, top=title) 

# # ggsave(paste("./figures/figures_exploratory/Proj/MeteoVar/", "plot_sd_diff_SMIAug_","total",".pdf", sep=""), plot=plot_sd_total_diff_SMIAug, width=14, height=8)


################################################################################################################################################################################################
#### Make combined Plots of SDs ####
################################################################################################################################################################################################
'- Combined Plots
- TavJul, PreJul, SMIJun, SMIAug
- TavJul, PreJul, SMIJun, SMIAug, Yield
- TavJul, PreJul, SMIJul
- TavJul, PreJul, SMIJul, Yield'

'For the yield plots it is necessary to load plot_sd_diff...list via the BasePrediction_Plots Script. '


#######################################################
#### Sd plots for TavJul, PreJul, SMIJun, SMIAug ####

plot_sd_total_SMI_6_Jun_Aug <- grid.arrange(plot_sd_total_1970_TavJul , plot_sd_total_1970_PreJul,  plot_sd_total_1970_SMIJun, plot_sd_total_1970_SMIAug,
                                      plot_sd_total_diff2021_TavJul, plot_sd_total_diff2021_PreJul, plot_sd_total_diff2021_SMIJun, plot_sd_total_diff2021_SMIAug,
                                      plot_sd_total_diff2070_TavJul, plot_sd_total_diff2070_PreJul, plot_sd_total_diff2070_SMIJun, plot_sd_total_diff2070_SMIAug,
                                      ncol=4, top=title) 
plot_sd_total_SMI_6_Jun_Aug
# # ggsave(paste("./figures/figures_exploratory/Proj/MeteoVar/","plot_sd_SMI_6_Jun_Aug_", "total",".pdf", sep=""), plot=plot_sd_total_SMI_6_Jun_Aug , width=28, height=24)

# ##############################################################
# #### Sd plots for TavJul, PreJul, SMIJun, SMIAug, Yield ####
# plot_sd_yield_SMI_6_Jun_Aug <- grid.arrange(plot_sd_total_1970_TavJul , plot_sd_total_1970_PreJul,  plot_sd_total_1970_SMIJun, plot_sd_total_1970_SMIAug,plot_sd_total_1971_list[[1]][[l]],
#                                             plot_sd_total_diff2021_TavJul, plot_sd_total_diff2021_PreJul, plot_sd_total_diff2021_SMIJun, plot_sd_total_diff2021_SMIAug, plot_sd_total_diff2021_list[[1]][[l]],
#                                             plot_sd_total_diff2070_TavJul, plot_sd_total_diff2070_PreJul, plot_sd_total_diff2070_SMIJun, plot_sd_total_diff2070_SMIAug, plot_sd_total_diff2070_list[[1]][[l]],
#                                             ncol = 5, top=textGrob("All RCM"),gp=gpar(fontsize=25)))
# plot_sd_yield_SMI_6_Jun_Aug
# 
# # # ggsave(paste("./figures/figures_exploratory/Proj/Combined/","plot_sd_yield_SMI_6_Jun_Aug_", "total",".pdf", sep=""), plot=plot_sd_yield_SMI_6_Jun_Aug , width=35, height=24)

###############################################
#### Sd plots for TavJul, PreJul, SMIJul ####
plot_sd_total_SMI_6_Jul <- grid.arrange(plot_sd_total_1970_TavJul , plot_sd_total_1970_PreJul,  plot_sd_total_1970_SMIJul,
                                  plot_sd_total_diff2021_TavJul, plot_sd_total_diff2021_PreJul, plot_sd_total_diff2021_SMIJul,
                                  plot_sd_total_diff2070_TavJul, plot_sd_total_diff2070_PreJul, plot_sd_total_diff2070_SMIJul,
                                  ncol=3, top=title) 
plot_sd_total_SMI_6_Jul
# # ggsave(paste("./figures/figures_exploratory/Proj/MeteoVar/","plot_sd_SMI_6_Jul_", "total",".pdf", sep=""), plot=plot_sd_total_SMI_6_Jul , width=28, height=24)

# ######################################################
# #### Sd plots for TavJul, PreJul, SMIJul, Yield ####
# plot_sd_yield_SMI_6_Jul <- grid.arrange(plot_sd_total_1970_TavJul , plot_sd_total_1970_PreJul,  plot_sd_total_1970_SMIJul, plot_sd_total_1971_list[[2]][[l]],
#                                         plot_sd_total_diff2021_TavJul, plot_sd_total_diff2021_PreJul, plot_sd_total_diff2021_SMIJul,  plot_sd_total_diff2021_list[[2]][[l]],
#                                         plot_sd_total_diff2070_TavJul, plot_sd_total_diff2070_PreJul, plot_sd_total_diff2070_SMIJul, plot_sd_total_diff2070_list[[2]][[l]],
#                                         ncol = 4, top=textGrob("All RCM"),gp=gpar(fontsize=20)))
# plot_sd_yield_SMI_6_Jul
# 
# # # ggsave(paste("./figures/figures_exploratory/Proj/Combined/","plot_sd_yield_SMI_6_Jul_", "total",".pdf", sep=""), plot=plot_sd_yield_SMI_6_Jul , width=28, height=24)
