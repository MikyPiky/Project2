#######################################################################
## Base Model for Predictions of Soil Moisture Effects on Crop Yield ##
#######################################################################
#### Description of Script ####
' - Prepare data.frame on yield and meteo vars for exploring models and fitting / train final model
    - Load Maize_meteo -> filter for relevant data (siloMais, etc)
    - Generate Stepwise functions of SMI with six anomalies
    - Remove comIds with less than nine observations to avoid leverage issues (comparable to first paper - another cut would also delete the 
      the important regions for silage maize production at Rheinebene)
    - Remove log trend of indepedent variable -> no trend observed, i.e. no deleting, but relevant to detect outliers
    - Issue with Outliers - Correct Typo in Data
    - Change Variable Names: Prec -> P, Tavg -> T 
  - Explore Models
      - many results, please go to section
  -  Explore differences on extimation procedure which use plm, demeaned data, or LSDV - in particular for nonlinearities 
      - many results, please go to section
  - Use of Caret Package: I started to work with the CARET Package, in particular to implement cross-validation that only takes into 
    account one fold for each year. The implementation can be found in BasePrediction_long
  - Retrieve anomaly correction: comId specific mean siloMaize 
    The bias correction is used later to correct the predicted yield for the mean of siloMaize for each comId. 
    This is particular important for  the plotting acticities (corrected for average expected silage maize). 
  - Predict on data which were used to train the model (one prediction for each year) to allow comparision 
    - Fit combined Model and best model from Paper 1
    - Loop through those models to make prediction for each year




'
#### Output ####
## Files
'
  - Maize_meteo (data.frame after preparation scheme described above) -> /Proj2/data/data_processed/
'
## Plots
'
- Maps with missing values (different cutoffs) <-/figures_exploratory/Train/
- Map with average yield for each comId: AverageYield.pdf  <-/figures_exploratory/Train/
- Map of sum of yield_anomalies for each comId: Yield_predict_sumsComId.pdf <- /figures_exploratory/Train/", modelListNames[[i]]

'

## Descriptive Statistics of MeteoVar
''

#### Dependencies and Input ####
' -  yieldData_meteo -> /Proj2/data/data_processed/

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


##############################################################################################################################################################################
#### Prepare data.frame on yield and meteo vars for exploring models and fitting final model #####
##############################################################################################################################################################################

##################################################################################################################
#### Create data frame with siloMaize as dependent and variables of the month above as independent variables ####
################################################################################################################

## Read in tidy Dataframe for Maize ##
Maize_meteo <- read.csv("./data/data_processed/yieldData_meteo")
Maize_meteo$X <- NULL
str(Maize_meteo)

####################################################
#### Delete dependent Variables but silo Maize ####
names <- c("winterWheat", "rye", "winterBarley", "summerBarley", "oats", "triticale", "potatoes", "sugarBeet", "winterRape")
Maize_meteo <- Maize_meteo[,  !names(Maize_meteo)%in%names]
head(Maize_meteo)

################################################
#### Load shape of administrative districts ####
################################################
vg2500_krs <- read_sf("./../Proj1/data/data_spatial/", "vg2500_krs")
str(vg2500_krs, 2)

#### Change RS to five digits #####
vg2500_krs$RS <- as.integer(str_sub(vg2500_krs$RS, 1,5))
vg2500_krs$RS

##########################################
#### Create stepwise function of SMI ####
########################################

#################################
## Stepwise with six anomalies ##

# June
Maize_meteo$SMI_Jun6 <- relevel(cut(Maize_meteo$SMI_Jun, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1), 
                                 labels = c("svr drght","mdrt drght","abnrml dry", "nrml",
                                            "abnrml wt" ,"abndnt wt", "svr wt")), ref= "nrml") 
table(Maize_meteo$SMI_Jun6, Maize_meteo$year  )

# July
Maize_meteo$SMI_Jul6 <- relevel(cut(Maize_meteo$SMI_Jul, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1), 
                                         labels = c("svr drght","mdrt drght","abnrml dry", "nrml",
                                                    "abnrml wt" ,"abndnt wt", "svr wt")), ref= "nrml") 
table(Maize_meteo$SMI_Jul6, Maize_meteo$year  )


# Aug
Maize_meteo$SMI_Aug6 <- relevel(cut(Maize_meteo$SMI_Aug, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1),
                                 labels = c("svr drght","mdrt drght","abnrml dry", "nrml",
                                            "abnrml wt" ,"abndnt wt", "svr wt")), ref= "nrml") 
table(Maize_meteo$SMI_Aug6,Maize_meteo$year  )


#########################################################################################
#### Remove comIds with more than one observations missing to avoid leverage issues #### 
#######################################################################################

############################################
## First delete all observations with NAs ##
Maize_meteo <- na.omit(Maize_meteo)
length(unique(Maize_meteo$comId)) # 365
'45 comIds have no observation in general '

######################################################
## Delete all comIds with less than 9 observations ##
missing_distribution <- as.data.frame(table(Maize_meteo$comId))
# str(missing_distribution)
# sum(table(Maize_meteo$comId) < 9) # 103 comIds have missing independent data when only considering full data sets, 31 with a cutoff of nine
# table(Maize_meteo$comId) < 9 

####################################################
#### Make map of distribution of missing values ####
missing_distribution_sf <- merge(vg2500_krs, missing_distribution, by.x="RS", by.y="Var1")
# dim(missing_distribution_sf)
str(missing_distribution_sf)
missing_distribution_sf$Freq <- as.numeric(missing_distribution_sf$Freq)

missing_distribution_sf_plot_09 <-
  ggplot(missing_distribution_sf) + 
    # geom_sf(data = vg2500_krs, fill="black") + 
    geom_sf(aes(fill = cut(Freq, c(0,9,17)) )) + 
  guides(fill=guide_legend(title="Number of Observations - Cut 9"))

missing_distribution_sf_plot_10 <-
  ggplot(missing_distribution_sf) + 
  # geom_sf(data = vg2500_krs, fill="black") + 
  geom_sf(aes(fill = cut(Freq, c(0,10,17)) ))+ 
  guides(fill=guide_legend(title="Number of Observations - Cut 10"))


missing_distribution_sf_plot_9_10_17 <-
  ggplot(missing_distribution_sf) + 
  # geom_sf(data = vg2500_krs, fill="black") + 
  geom_sf(aes(fill = cut(Freq, c(0,9,10,17)) )) + 
  guides(fill=guide_legend(title="Number of Observations"))

missing_distribution_sf_plot_16_17 <-
  ggplot(missing_distribution_sf) + 
  # geom_sf(data = vg2500_krs, fill="black") + 
  geom_sf(aes(fill = cut(Freq, c(0,14,17)) )) + 
  guides(fill=guide_legend(title="Number of Observations"))

ggsave(paste("./figures/figures_exploratory/Train/MissingValues/", "MissingValues_cut9.pdf", sep="") , missing_distribution_sf_plot_09, width=16, height=9) 
ggsave(paste("./figures/figures_exploratory/Train/MissingValues/", "MissingValues_cut10.pdf", sep="") , missing_distribution_sf_plot_10, width=16, height=9) 
ggsave(paste("./figures/figures_exploratory/Train/MissingValues/", "MissingValues_cut_9_10_17.pdf", sep="") , missing_distribution_sf_plot_9_10_17, width=16, height=9) 
ggsave(paste("./figures/figures_exploratory/Train/MissingValues/", "MissingValues_cut_16_17.pdf", sep="") , missing_distribution_sf_plot_16_17, width=16, height=9) 

# 
# ## comIds with at least one observation missing ##
# list_delete <- c( 3101, 3102, 3103, 3153, 3154, 3157, 3158,  
#            3402, 3404, 
#            5111, 5112, 5113, 5114, 5117, 5119, 5124, 5314, 5315, 5316, 5334, 5378,  5512, 5515 ,
#            5711, 5911, 5913, 5914, 5916, 5954,  6411, 6412, 6413, 
#            6438, 6611, 7131, 7132, 7133, 7134, 7135, 7137, 7138, 7140, 7141, 7143, 
#            7233,  7331, 7332, 7333, 7334, 7335, 7337, 7338, 7339, 7340, 8111, 8115, 8117, 8135,  8215, 8216, 
#            8235,  8315, 8316, 8317,  8326, 8327,  8336, 8415, 8416,  8435 ,
#            9461,  
#            12052, 12053, 13051, 13052, 13053, 13054, 13055, 13056, 13057, 13058, 13059, 13060, 13061, 
#            13062, 14511, 14612, 14628, 14713, 15001, 15082, 15083, 15084, 15085, 15086, 15087, 15088, 15089, 15091, 16051, 16052, 16056, 
#            16072)
# length(list_delete)
# list_delete[[1]]

## comIds with less than nine observation: ##
list_delete <- c(3101, 3102, 3402, 5111 , 5112 , 5113 , 5114, 5117, 5124, 5314,
          5315, 5334,5378,  5512, 5911,   5916,  7131,  7133, 7135, 7233, 
          7331, 7332,7334, 7335, 7337,    7338, 7339,   8111,12052, 14612, 16052 )

length(list_delete)
list_delete[[1]]


## Look at comIds with missing dependent data ##
k <- NULL
for (k in 1:length(list_delete)){
print(Maize_meteo[Maize_meteo$comId == list_delete[[k]],1:6])  
}
' Das Fehlen der Dateneinträge schein meistens systematisch zu sein, da es meisten Blöcke sind, welche fehlen'

## Delete comIds with at least one observation missing ##
temp <- Maize_meteo
for (k in 1:length(list_delete))
{
  print(Maize_meteo[Maize_meteo$comId==list_delete[k],])
  temp <- (temp[!temp$comId==list_delete[k],])
}


## Number of deleted rows ##
dim(temp) - dim(Maize_meteo)

## Further use old name for data.frame
Maize_meteo <- temp

length(unique(Maize_meteo$comId)) # 334 /262 (only full comIds)

##################################
## Clean up data after deleting ##
rownames(Maize_meteo) <- NULL
str(Maize_meteo)
# Maize_meteo <- plm.data(Maize_meteo, index=c("comId", "year")) 
Maize_meteo[,c("comId","year")] <- lapply(Maize_meteo[,c("comId","year")], factor )


# ##################################################
# #### Remove log trend of indepedent variable ####
# ################################################
trend <- lm(siloMaize ~ as.integer(year), data = Maize_meteo)
summary(trend)

'Da es keinen Trend gibt, detrenden wir die Daten nicht mehr. Dennoch lass ich weiterhin siloMaize_logtrend im Datensatz '

# # ###############################################
# # #### Compare different silage Maize Versions ##
# summary(log(Maize_meteo$siloMaize))
# summary(Maize_meteo$siloMaize)
# summary(Maize_meteo$siloMaize_logtrend)
# 
# hist(log(Maize_meteo$siloMaize))
# hist(Maize_meteo$siloMaize)
# hist(Maize_meteo$siloMaize_logtrend)
# ' Auch die Verteilung gibt keinen wirklichen Anlass zu detrenden, oder den logaithmus zu benutzen.'

#####################################################
#### Issue with Outliers - Correct Typo in Data ####
###################################################
' based on outlier statistic in regression on trend'
par(mfrow = c(2,2))
plot(trend) 

## Look at Outliers Values ##
Maize_meteo[c(1785, 934, 1383),] # comId 6532, 5116, 5319

## Look at other values of outliers com #
Maize_meteo[Maize_meteo$comId == "6532",] # 2008 hier scheint nur ein Jahr ein Messfehler zu sein: diesen korrigiere ich 86.4 <- 486.4
Maize_meteo[Maize_meteo$comId == "5116",] # 2006 verändere ich nicht 
Maize_meteo[Maize_meteo$comId == "5913",] # 2003 verändere ich nicht 

## Correct Typo in Data ##
Maize_meteo[c(Maize_meteo$comId == "6532" & Maize_meteo$year == "2008"), 6] <- 486.4

##########################################################
#### Change Variable Names: Prec -> P, Tavg -> T ####
########################################################
names(Maize_meteo) <- gsub("Prec", "P", names(Maize_meteo))
names(Maize_meteo) <- gsub("Tavg", "T", names(Maize_meteo))

######################################################################
#### Retrieve anomaly correction: comId specific mean siloMaize) ####
####################################################################
' The bias correction is used later to correct the predicted yield for the mean of siloMaize for each comId. 
This is particular important for  the plotting acticities (corrected for average expected silage maize (log). '
dim(Maize_meteo)
anomaly_correction <-Maize_meteo %>% group_by(comId) %>% summarise(average_yield = mean(siloMaize))
str(anomaly_correction)
summary(anomaly_correction$average_yield)

###################################
#### Export anomaly_correction ####
write.csv(anomaly_correction, file="./data/data_processed/AnomalyCorrection.csv")

#######################################
#### Make a map of average values ####
#####################################
anomaly_correction_sf <- merge(vg2500_krs, anomaly_correction, by.x="RS", by.y="comId")
dim(anomaly_correction)
dim(anomaly_correction_sf)
str(anomaly_correction_sf)

## Anomaly
anomaly_correction_sf_plot <- 
  ggplot(anomaly_correction_sf) + 
  geom_sf(data=vg2500_krs,fill="white") + 
  geom_sf(aes(fill = average_yield)) +  
  guides(fill = guide_legend(title = "Av. Yield ")) + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) 


ggsave(paste("./figures/figures_exploratory/Train/", "AverageYield.pdf", sep=""), anomaly_correction_sf_plot, device="pdf", width=8, height= 8) 

#######################################################
#### Append anomaly correction on Maize_meteo #####
#######################################################

## Make data.frame with values of anomaly correction ##
anomaly_correction_df <- anomaly_correction[rep(seq_len(17*334)),]
anomaly_correction_df$average_yield <- as.factor(anomaly_correction_df$average_yield)

anomaly_correction_df <- do.call("rbind", replicate(17, anomaly_correction, simplify = FALSE))
anomaly_correction_df$year <- as.factor(sort(rep(seq(1999,2015),334)))
# str(anomaly_correction_df)
# View(anomaly_correction_df)


#### Merge with Maize_meteo #####
Maize_meteo_anomaly <- merge(Maize_meteo, anomaly_correction_df, by=c("comId","year"))
# str(Maize_meteo_anomaly)
# str(Maize_meteo)

#### Further use standard name of data.frame ####
Maize_meteo <- Maize_meteo_anomaly

############################################################
##### Save newly created data.frame Maize_meteo extern ####
##########################################################
write.csv(Maize_meteo, file="./data/data_processed/Maize_meteo_step_cut9_anomaly.csv")

###########################
#### Clear Environment ####
rm(list=ls()[! ls() %in% c("Maize_meteo", "vg2500_krs")])

# ##################################################################################################################################################################
# ################################### Explore Models ###############################################################################################################
# ##################################################################################################################################################################
 
# ' 
# Caveat: Die Explorative Analyse war für den Datensatz welcher nur comIds mit vollständigen Report berücksichtigt hat
# Ich habe hier eine explorative Analyse verschiedener Modelle gemacht. Der Code zu der Analyse befindet sich in BasePrediction_long.R .
#  Vor allem habe ich mit stepwise in 4 Stufen und Polynomen von SMI gespielt.
# Ergebnisse sind
# - Interessant ist, dass wenn man für die Meteorologie kontrolliert (P, T im July), die SMI wet im August an Significanz verlieren 
#   (Adj. R-Squared: 0.34778 fpr diese Modell (SMIJun6, Pol3TPJun, SMIAug6)
# - Wenn mann eine stepwise function mit nur 4 steps nimmt, dann sind die Ergebnisse vergleichbar (Adj. R-Squared: 0.34353)
# - Interactionsterme zwischen SMIJun4 und SMIJun4 liefern schlechtere Ergebnisse als mit Meteoroligie
# (Adj. R-Squared: 0.23835 ohne T und P July): Es scheint nicht möglich zu sein für
# die Meteorologie in July via SMI-Intaction Jun und Ug zu kontrollieren. 
# - Wenn ich neben Interaction SMI auch für Meteorologie im July reagieren habe ich ein Adj. R-Squared: 0.34989 für Polynome 3. Grades 
#   und 0.35036 für Polynome 4. Grades in P and T 
# - Polynime statt stepwise functions für den SMI verbessern die Ergebnnise auch nicht :
# # # Adj. R-Squared: 0.34989, Polynome 3. Grades in P and T and SMI
# # # Adj. R-Squared: 0.35036, Polynome 4. Grades in P and T , SMi 3. Grad
# # # Adj. R-Squared: 0.35595, Polynome 4. Grades in P and T and SMI
# -  Meteorologie im July verbessert die vorhersagekraft im sample sehr. 
# '
# #############################################################################################################################################################################
# #############################################################################################################################################################################
# 
# ################################################################################################################################
# #### Explore differences on extimation procedure which use plm, demeaned data, or LSDV - in particular for nonlinearities #####
# ##############################################################################################################################
# 
# ##############################
# #### (Time) - Demean Data ####
# str(Maize_meteo)
# ## Scale all the data within a comId ##
# Maize_meteo_demean1  <- ddply(Maize_meteo,  ~ comId,  numcolwise(scale, scale=FALSE))
# 
# summary(Maize_meteo_demean1)#
# str(Maize_meteo_demean1)
# names(Maize_meteo_demean1)
# 
# #########################################################################
# #### Compare the data demeaned by ddplyr with manually demeand Data ####
# 
# ## ddplyr
# head(Maize_meteo_demean1,18)[5]
# ## manually
# # head(Maize_meteo)[8]
# Maize_meteo[1:18,8] - mean(Maize_meteo[1:17,8]) 
# ' Da der 18. Wert nicht mehr übereinstimmt scheint dass demeanen für die einzelnen comIds zu funktionieren.'
# 
# #######################################################################################################
# #### Comprise full demeaned data.frame  also including temporal information and stepwise functions ####
# ## Append Year ##
# year <- Maize_meteo$year
# Maize_meteo_demean  <- cbind(year, Maize_meteo_demean1)
# summary(Maize_meteo_demean)
# names(Maize_meteo_demean)
# 
# ## Change Order that comId is first column ##
# Maize_meteo_demean  <- Maize_meteo_demean[,c(2,1,3:length(names(Maize_meteo_demean)))]
# names(Maize_meteo_demean )
# 
# ## Append stepwise functions ##
# names(Maize_meteo)[(length(names(Maize_meteo))-2):(length(names(Maize_meteo)))]
# names(cbind(Maize_meteo_demean, Maize_meteo[(length(names(Maize_meteo))-2):(length(names(Maize_meteo)))]))
# 
# Maize_meteo_demean <- cbind(Maize_meteo_demean, Maize_meteo[(length(names(Maize_meteo))-2):(length(names(Maize_meteo)))])
# str(Maize_meteo_demean )
# 
# ############################################################
# #### Compare results of plm, demean, and lm with dummy ####
# ##########################################################
# 
# #### linear ####
# ## plm
# plm.fit_lin <- plm(siloMaize ~ SMI_Jun6 + SMI_Aug6 + I(T_Jul)  ,
#                data = Maize_meteo,  effect="individual", model=("within"), index = c("comId","year"))
# summary(plm.fit_lin)
# 
# ## demean
# lm.fit_demean_lin <- lm(siloMaize ~  SMI_Jun6 + SMI_Aug6 + I(T_Jul) , data = Maize_meteo_demean)
# summary(lm.fit_demean_lin)  
# 
# ## LSDV 
# lm.fit_dummy_lin <- lm(siloMaize ~ SMI_Jun6 + SMI_Aug6  + I(T_Jul) + comId, data = Maize_meteo)
# summary(lm.fit_dummy_lin) 
# 
# ## Comparision of Coefficients ##
# coefficients(lm.fit_dummy_lin)[2:14] # I(T_Jul): -0.019542 (siloMaize_logtrend); -0.02019190 log(siloMaize);  -8.423623 (silomaize);
# coefficients(plm.fit_lin)[1:13] # I(T_Jul):  -0.0195418, (siloMaize_logtrend);-0.02019190 log(siloMaize); -8.423623 (silomaize);
# coefficients(lm.fit_demean_lin)[2:14] # I(T_Jul): -0.020220 (siloMaize_logtrend); -0.02019190 log(siloMaize); -8.714845 (silomaize);
# ' Die Coefficienten ändern sich kaum wenn man mit cutoff 9 statt mit cuttoff 17 arbeitet'
# 
# #### nonlinear ####
# ## plm
# plm.fit_nonlin <- plm(siloMaize ~ SMI_Jun6 + SMI_Aug6  + I(T_Jul)  + I(T_Jul^2) ,
#                    data = Maize_meteo,  effect="individual", model=("within"), index = c("comId","year"))
# summary(plm.fit_nonlin) # Adj.  R-sq  0.33496 (siloMaize), 0.31927 (siloMaize_logtrend)
# 
# ## demean
# lm.fit_demean_nonlin <- lm(siloMaize ~  SMI_Jun6 + SMI_Aug6 + I(T_Jul)  + I(T_Jul^2) , data = Maize_meteo_demean)
# summary(lm.fit_demean_nonlin) # Adj.  R-sq  0.3611  (siloMaize),  0.3454  (siloMaize_logtrend)
# 
# ## LSDV 
# lm.fit_dummy_nonlin <- lm(siloMaize ~ SMI_Jun6 + SMI_Aug6  + I(T_Jul)  + I(T_Jul^2) + factor(comId), data = Maize_meteo)
# summary(lm.fit_dummy_nonlin) # Adj.  R-sq  0.6801 (siloMaize),  0.6779  (siloMaize_logtrend)
# 
# ## Comparision of Coefficients ##
# coefficients(lm.fit_dummy_nonlin)[2:15 ] # I(T_Jul) 0.146906158, I(T_Jul^2) -0.004449528 (siloMaize_logtrend);  
# # I(T_Jul)55.492156  , I(T_Jul^2) -1.708612 (siloMaize)
# coefficients(plm.fit_nonlin)[1:14]  # I(T_Jul) 0.146906158, I(T_Jul^2) -0.004449528 (siloMaize_logtrend)
# # I(T_Jul)55.49215 , I(T_Jul^2) -1.708612 (siloMaize)
# coefficients(lm.fit_demean_nonlin)[2:15] # I(T_Jul) -0.017895938 , I(T_Jul^2) -0.004448825(siloMaize_logtrend)
# # I(T_Jul)-7.806 , I(T_Jul^2) -1.740 (siloMaize)
# 
# 
# ' Die Coefficienten ändern sich kaum wenn man mit cutoff 9 statt mit cuttoff 17 arbeitet
# Bei linearer Konfiguration: Für die Coefficienten bekomme ich die gleichen Werte. 
# Das adjusted R-square unterscheidet sich aber zwischen demean und plm.
# Dummy liefert natürlich ein größeres R2, da dort die Fixed Effects explicit mit eingehen. 
# Bei linearen Modellen kann man also Problemlos demeanen. Des weiteren ist der SMI 
# wohl so wie er definiert ist nicht durch demeanen betroffen, da das demeaning framework nicht für die SMIs angewendet wurde 
# (numcolwise berücksichtige alle Faktoren nicht), 
# die Ergebnisse bei linearen Konfigurationen aber gleich sind.
# Bei nichtlinearitäten: lm.fit_demean liefert andere cofficienten für die Polynome als die drei anderen Modelle. 
# D.h. durch das demeanen werden die Poylnomberechnungen entscheidend verändert.
# The function poly does not work with demeanded data, even not with degree 1 '
# 
# '
# Ich glaube, es wäre schlecht, wenn die Daten gefitted werden, ohne dass jede räumliche Einheit betrachtet werden. In so einem Fall würde es nämlich keinen Fixed Effect
# für die Daten in einem Ort geben. Das heißt, dort würde kein demeaning stattfinden. Daher sollte ich darauf achten, dass ich stratified samples für cross-validation nehme ()
# '
#############################################################################################################################################################################
###############################################################################################################################################################################
# ############################
# #### use caret package ####
# ##########################
###############################################################################################################################################################################
' I started to work with the CARET Package, in particular to implement cross-validation that only takes into 
account one fold for each year. The implementation can be found in BasePrediction_long'


###########################################################################################################################################################
################ Predict on data which were used to train the model (one prediction for each year) to allow comparision ##################################
#########################################################################################################################################################

#########################################
#### Fit model used for Predictions ####
#######################################

####################################
#### Fit combined model - yield ####
lm.fit_SMI_6_Jun_Aug_comId <- lm(siloMaize ~ T_Jul + P_Jul  + I(T_Jul^2) +  I(T_Jul^3) + I(P_Jul^2) +  I(P_Jul^3) + SMI_Jun6 + SMI_Aug6 + comId,
                                       data = Maize_meteo)
summary(lm.fit_SMI_6_Jun_Aug_comId )  # Adjusted R-squared:  0.6939 (silomaize_logtrend) , Adjusted R-squared:  0.6964 (silomaize - only full observations),
                                      # Adjusted R-squared:  0.6857  (silomaize - keep comIds with at least nine observations)


# #### Compare to plm.fit ####
# plm.fit_SMI_6_Jun_Aug_comId <- plm(siloMaize ~ T_Jul + P_Jul  + I(T_Jul^2) +  I(T_Jul^3) + I(P_Jul^2) +  I(P_Jul^3) + SMI_Jun6 + SMI_Aug6,
#                                  data = Maize_meteo, effect="individual", model=("within"), index = c("comId","year"))
# summary(plm.fit_SMI_6_Jun_Aug_comId ) # Adjusted R-squared:  0.35306 (silomaize_logtrend) , 0.36876 (silomaize - only full observations)
#                                       # Adjusted R-squared:  0.33616  (silomaize - keep comIds with at least nine observations)
# 

#############################################
#### Fit best model from paper 1 - yield ####
lm.fit_SMI_6_Jul_comId <- lm(siloMaize ~ T_Jul + P_Jul  + I(T_Jul^2) +  I(T_Jul^3) + I(P_Jul^2) +  I(P_Jul^3) + SMI_Jul6 + comId,
                                 data = Maize_meteo)
summary(lm.fit_SMI_6_Jul_comId )  # Adjusted R-squared:  0.6694 (silomaize_logtrend) , Adjusted R-squared:  0.669 (silomaize- only full observations),
                                  # Adjusted R-squared:  0.661  (silomaize - keep comIds with at least nine observations)


# #### Compare to plm.fit ####
# plm.fit_SMI_6_Jul_comId <- plm(siloMaize ~ T_Jul + P_Jul  + I(T_Jul^2) +  I(T_Jul^3) + I(P_Jul^2) +  I(P_Jul^3) + SMI_Jul6,
#                                    data = Maize_meteo, effect="individual", model=("within"), index = c("comId","year"))
# summary(plm.fit_SMI_6_Jul_comId ) # Adjusted R-squared:  0.30122 (silomaize_logtrend) , 0.31181(silomaize- only full observations),
#                                   # Adjusted R-squared:  0.28411  (silomaize - keep comIds with at least nine observations)
# 
# 
# #  
# #########################################
# #### Fit combined model - log(yield) ####
# lm.fit_SMI_6_Jun_Aug_comId_log <- lm(log(siloMaize) ~ T_Jul + P_Jul  + I(T_Jul^2) +  I(T_Jul^3) + I(P_Jul^2) +  I(P_Jul^3) + SMI_Jun6 + SMI_Aug6 + comId,
#                                  data = Maize_meteo)
# summary(lm.fit_SMI_6_Jun_Aug_comId_log ) 
# # Adjusted R-squared:  0.6939 (silomaize_logtrend) , Adjusted R-squared:  0.6964 (silomaize - only full observations),
# # Adjusted R-squared:  0.6857  (silomaize - keep comIds with at least nine observations)
# # Adjusted R-squared:  0.6869  (log(silomaize) - keep comIds with at least nine observations)
# 
# 
# #### Compare to plm.fit ####
# plm.fit_SMI_6_Jun_Aug_comId_log <- plm(log(siloMaize) ~ T_Jul + P_Jul  + I(T_Jul^2) +  I(T_Jul^3) + I(P_Jul^2) +  I(P_Jul^3) + SMI_Jun6 + SMI_Aug6,
#                                    data = Maize_meteo, effect="individual", model=("within"), index = c("comId","year"))
# summary(plm.fit_SMI_6_Jun_Aug_comId_log ) 
# # Adjusted R-squared:  0.35306 (silomaize_logtrend) , 0.36876 (silomaize - only full observations)
# # Adjusted R-squared:  0.33616  (silomaize - keep comIds with at least nine observations)
# # Adjusted R-squared:  0.33104  (log(silomaize) - keep comIds with at least nine observations)
# 

# ##################################################
# #### Fit best model from paper 1 - log(yield) ####
# lm.fit_SMI_6_Jul_comId_log <- lm(log(siloMaize)~ T_Jul + P_Jul  + I(T_Jul^2) +  I(T_Jul^3) + I(P_Jul^2) +  I(P_Jul^3) + SMI_Jul6 + comId,
#                              data = Maize_meteo)
# summary(lm.fit_SMI_6_Jul_comId_log )  
# # Adjusted R-squared:  0.6694 (silomaize_logtrend) , Adjusted R-squared:  0.669 (silomaize- only full observations),
# # Adjusted R-squared:  0.661  (silomaize - keep comIds with at least nine observations)
# # Adjusted R-squared:   0.6859  (log(silomaize) - keep comIds with at least nine observations)
# 
# #### Compare to plm.fit ####
# plm.fit_SMI_6_Jul_comId_log <- plm(log(siloMaize) ~ T_Jul + P_Jul  + I(T_Jul^2) +  I(T_Jul^3) + I(P_Jul^2) +  I(P_Jul^3) + SMI_Jul6,
#                                data = Maize_meteo, effect="individual", model=("within"), index = c("comId","year"))
# summary(plm.fit_SMI_6_Jul_comId_log )
# # Adjusted R-squared:  0.30122 (silomaize_logtrend) , 0.31181(silomaize- only full observations),
# # Adjusted R-squared:  0.28411  (silomaize - keep comIds with at least nine observations)
# # Adjusted R-squared: 0.28289,   (log(silomaize) - keep comIds with at least nine observations)

'
## Combined Model ##
## lsdv
# Adjusted R-squared:  0.6939  (silomaize_logtrend  - only full observations) 
# Adjusted R-squared:  0.6964  (silomaize - only full observations),
# Adjusted R-squared:  0.6857  (silomaize - keep comIds with at least nine observations)
# Adjusted R-squared:  0.6869  (log(silomaize) - keep comIds with at least nine observations)

## plm
# Adjusted R-squared:  0.35306 (silomaize_logtrend   - only full observations) 
# Adjusted R-squared:  0.36876 (silomaize - only full observations)
# Adjusted R-squared:  0.33616 (silomaize - keep comIds with at least nine observations)
# Adjusted R-squared:  0.33104 (log(silomaize) - keep comIds with at least nine observations)

## plm / lsdv
# Adjusted R-squared:  0.5088053 (silomaize_logtrend   - only full observations) 
# Adjusted R-squared:  0.5295233(silomaize - only full observations)
# Adjusted R-squared:  0.4902435 (silomaize - keep comIds with at least nine observations)
# Adjusted R-squared:  0.4819333 (log(silomaize) - keep comIds with at least nine observations)


## Model Paper 1 ##

## lsdv
# Adjusted R-squared:  0.6694 (silomaize_logtrend  - keep comIds with at least nine observations) 
# Adjusted R-squared:  0.669  (silomaize- only full observations),
# Adjusted R-squared:  0.661  (silomaize - keep comIds with at least nine observations)
# Adjusted R-squared:  0.6859 (log(silomaize) - keep comIds with at least nine observations)

## plm
# Adjusted R-squared:  0.30122 (silomaize_logtrend  - keep comIds with at least nine observations) 
# Adjusted R-squared:  0.31181 (silomaize- only full observations),
# Adjusted R-squared:  0.28411 (silomaize - keep comIds with at least nine observations)
# Adjusted R-squared:  0.28289 (log(silomaize) - keep comIds with at least nine observations)

## plm /lsdv
# Adjusted R-squared:  0.4499851 (silomaize_logtrend  - keep comIds with at least nine observations) 
# Adjusted R-squared:  0.4660837 (silomaize- only full observations),
# Adjusted R-squared:  0.4298185 (silomaize - keep comIds with at least nine observations)
# Adjusted R-squared:  0.4124362 (log(silomaize) - keep comIds with at least nine observations)

## Conclusions ##
# Detrendenden has basically no effect - makes sense since no trend was found
# Combined models always have a better adjusted R-square (smalles difference for log(silomaize) - keep comIds with at least nine observations))
# plm /lsdv of adjusted R-square always larger for combined models -> more variation in combined model is explained by meteorology and smi comapred to the fixed effects
# when dropping all comIds with missing observation, the R-square is larger compared to when using a cut of nine. 
# no model improvement when using log yield instead of yield
'


######################################################################
#### Loop through those models to make predictions for each year ####
## Prepare loop ##
modelList <- list(lm.fit_SMI_6_Jun_Aug_comId, lm.fit_SMI_6_Jul_comId)
modelListNames <- list("lm.fit_SMI_6_Jun_Aug_comId", "lm.fit_SMI_6_Jul_comId")
str(modelList,1 )
str(lm.fit_SMI_6_Jun_Aug_comId)

## Start loop ##
for (i in 1:2){
    dir.create(paste("./figures/figures_exploratory/Train/", modelListNames[[i]], sep=""), showWarnings = F)

  ######################################################################################################################################
  #### Loop thorugh 1999 to 2015 to make predictions for each year (stored in predictData_train and predictData_train_anomaly) ####
  
  ## Prepare loop ##
  listyear <- seq(1999, 2015)
  
  ## Create containers 
  predictData_train <- NULL
  predictData_train_anomaly <- NULL
  predictData_train_anomaly_allyears  <-NULL 
  
  predictData_train <-  data.frame()
  predictData_train_anomaly  <-  data.frame()
  predictData_train_anomaly_allyears   <- data.frame()

  #### Choose color Setting ####
  ## Absolute
  # summary(predictData_train_sf)
  myPalette <- colorRampPalette((brewer.pal(11, "BrBG")))
  sc_abs <- scale_fill_gradientn("Yield Deviation", colours = myPalette(100), limits=c(200,700))
  
  ## Anomaly
  myPalette_anomaly <- colorRampPalette((brewer.pal(11, "BrBG")))
  sc_anomaly <- scale_fill_gradientn("Yield Deviation", colours = myPalette_anomaly(100), limits=c(-150, 150))
  
  
  ## Start loop ##
    for(m in 1:17){
      ## Create data.frame for the year m ##
      Maize_meteo_year <- Maize_meteo %>% filter(year == listyear[[m]] )
   
      ## Predict model on that data.frame ##
      predict_year <- as.data.frame(predict(modelList[[i]], newdata = Maize_meteo_year ))
  
      ## Correct for the average yield ##
      predict_year_anomaly  <- predict_year - Maize_meteo_year$average_yield
  
      ## Change names to year m ##
      ## Jun_Jul_Aug
      names(predict_year) <-  paste(listyear[[m]]) 
      names(predict_year_anomaly ) <- paste(listyear[[m]],"anomaly",sep="_")
      
      ## Append to create a large data.frame
      predictData_train <- cbind(Maize_meteo_year[,1:2], predict_year)
      predictData_train_anomaly <- cbind(predictData_train , predict_year_anomaly )
      str(predictData_train_anomaly)
      
      #############################
      #### Plot predicted data ####
      predictData_train_sf <- merge(vg2500_krs, predictData_train_anomaly, by.x ="RS", by.y="comId")
      names(predictData_train_sf) <- gsub("X","", names(predictData_train_sf))
      str(predictData_train_sf )
      
      ## Anomaly
      predictData_train_sf_anomaly_plot  <- 
        ggplot(   predictData_train_sf) + 
        geom_sf(data=vg2500_krs,fill="gray", color="white") + 
        geom_sf(aes(fill =  predictData_train_sf[[8]]) )  +  
        guides(fill = guide_legend(title = "Predicted Yield Anomaly")) + 
        sc_anomaly + 
        ggtitle(paste(  names(predict_year) )) + 
        theme_bw() + 
        theme(plot.title = element_text(hjust = 0.5)) 
      
      ## Absolut
      predictData_train_sf_absolut_plot  <- 
        ggplot(   predictData_train_sf) + 
        geom_sf(data=vg2500_krs,fill="gray", color="white") + 
        geom_sf(aes(fill =  predictData_train_sf[[7]]) )  +  
        guides(fill = guide_legend(title = "Predicted Yield")) + 
        sc_abs + 
        ggtitle(paste(  names(predict_year) ))  + 
        theme_bw() + 
        theme(plot.title = element_text(hjust = 0.5)) 
  
      
      #### Save the plots ####
      ggsave(paste("./figures/figures_exploratory/Train/", modelListNames[[i]],  "/Yield_predict_anomaly_", listyear[m],".pdf", sep=""),predictData_train_sf_anomaly_plot , device="pdf", width=8, height= 8) 
      ggsave(paste("./figures/figures_exploratory/Train/", modelListNames[[i]],  "/Yield_predict_", listyear[m],".pdf", sep=""),     predictData_train_sf_absolut_plot, device="pdf", width=8, height= 8) 
   
      ########################################################
      #### Create on large data frame including all years ####
      names(predictData_train_anomaly) <- c("comId", "year", "yield_predicted", "yield_anomaly_predicted")
      predictData_train_anomaly_allyears <- rbind(predictData_train_anomaly_allyears ,  predictData_train_anomaly  )
      dim(predictData_train_anomaly_allyears)
      dim(predictData_train_anomaly)
    }
  
  ###########################################################################################################################
  #### Export the data.frame including the maize yield and maize yield deviations (anomalies) of the period 1999 - 2015 ####
  #########################################################################################################################
  write.csv(predictData_train_anomaly_allyears, paste("./data/data_processed/Train/", modelListNames[[i]], "/Yield_predict_allYears.pdf", sep="" ))


  ##########################################################
  #### Calculate sums of the predictions for each comId ####
  predictData_train_sums <- predictData_train_anomaly_allyears %>% group_by(comId)  %>% summarise(comIdsums = sum(yield_anomaly_predicted))
  
  #### Merge with Sf.data.frame ####
  predictData_train_sums_sf <- merge(vg2500_krs, predictData_train_sums, by.x = "RS", by.y = "comId") 
  str(predictData_train_sums_sf )
  
    ##########################################################################
  #### Plot sums of Predictions over years of each comId in train Data ####
  ########################################################################
  myPalette <- colorRampPalette((brewer.pal(11, "BrBG")))
  sc <- scale_fill_gradientn(colours = colorRampPalette((brewer.pal(11, "BrBG")))(100))
  
  predictData_train_sums_plot <- 
      ggplot(predictData_train_sums_sf) + 
      geom_sf(aes(fill = comIdsums)) + 
      ggtitle("Sums of annual predictions")  + theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
   sc
    
  ggsave(paste("./figures/figures_exploratory/Train/", modelListNames[[i]],  "/Yield_predict_sumsComId.csv", sep=""), predictData_train_sums_plot , device="pdf", width=8, height= 8) 


} ## END OF LOOP WHICH LOOPS THROUGH THE DIFFERENT MODELS 


rm(list=ls())
