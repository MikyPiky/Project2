#######################################################################
## Base Model for Predictions of Soil Moisture Effects on Crop Yield ##
#######################################################################
#### Description of Script ####
' - Prepare data.frame on yield and meteo vars for exploring models and fitting / train final model
    - Load yieldData_meteo -> filter for relevant data (siloMais, etc)
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
  - Retrieve anomaly correction: comId specific mean siloMaize)
    The bias correction is used later to correct the predicted yield for the mean of siloMaize for each comId. 
    This is particular important for  the plotting acticities (corrected for average expected silage maize). 
  - Predict on data which were used to train the model (one prediction for each year) to allow comparision 
    - Fit combined Model and best model from Paper 1
    - Loop through those models to make prediction for each year




'
#### Output ####
## Files
'
  - yieldData_meteo (data.frame after preparation scheme described above) -> /Proj2/data/data_processed/
'
## Plots
''

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
library(reshape)
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
##############################################################################################################################################################################


##############################################################################################################################################################################
#### Prepare data.frame on yield and meteo vars for exploring models and fitting final model #####
##############################################################################################################################################################################

##################################################################################################################
#### Create data frame with siloMaize as dependent and variables of the month above as independent variables ####
################################################################################################################

## Read in tidy Dataframe for Maize ##
yieldData_meteo <- read.csv("./data/data_processed/yieldData_meteo")
yieldData_meteo$X <- NULL
str(yieldData_meteo)

####################################################
#### Delete dependent Variables but silo Maize ####
names <- c("winterWheat", "rye", "winterBarley", "summerBarley", "oats", "triticale", "potatoes", "sugarBeet", "winterRape")
yieldData_meteo <- yieldData_meteo[,  !names(yieldData_meteo)%in%names]
head(yieldData_meteo)

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
yieldData_meteo$SMI_Jun6 <- relevel(cut(yieldData_meteo$SMI_Jun, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1), 
                                 labels = c("svr drght","mdrt drght","abnrml dry", "nrml",
                                            "abnrml wt" ,"abndnt wt", "svr wt")), ref= "nrml") 
table(yieldData_meteo$SMI_Jun6, yieldData_meteo$year  )

# July
yieldData_meteo$SMI_Jul6 <- relevel(cut(yieldData_meteo$SMI_Jul, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1), 
                                         labels = c("svr drght","mdrt drght","abnrml dry", "nrml",
                                                    "abnrml wt" ,"abndnt wt", "svr wt")), ref= "nrml") 
table(yieldData_meteo$SMI_Jul6, yieldData_meteo$year  )


# Aug
yieldData_meteo$SMI_Aug6 <- relevel(cut(yieldData_meteo$SMI_Aug, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1),
                                 labels = c("svr drght","mdrt drght","abnrml dry", "nrml",
                                            "abnrml wt" ,"abndnt wt", "svr wt")), ref= "nrml") 
table(yieldData_meteo$SMI_Aug6,yieldData_meteo$year  )


#########################################################################################
#### Remove comIds with more than one observations missing to avoid leverage issues #### 
#######################################################################################

############################################
## First delete all observations with NAs ##
yieldData_meteo <- na.omit(yieldData_meteo)
length(unique(yieldData_meteo$comId)) # 365
'45 comIds have no observation in general '

######################################################
## Delete all comIds with less than 9 observations ##
missing_distribution <- as.data.frame(table(yieldData_meteo$comId))
# str(missing_distribution)
# sum(table(yieldData_meteo$comId) < 9) # 103 comIds have missing independent data when only considering full data sets, 31 with a cutoff of nine
# table(yieldData_meteo$comId) < 9 

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


missing_distribution_sf_plot <-
  ggplot(missing_distribution_sf) + 
  # geom_sf(data = vg2500_krs, fill="black") + 
  geom_sf(aes(fill = Freq) ) + 
  guides(fill=guide_legend(title="Number of Observations"))

ggsave(paste("./figures/figures_exploratory/Train/MissingValues/", "MissingValues_cut9.pdf", sep="") , missing_distribution_sf_plot_09, width=16, height=9) 
ggsave(paste("./figures/figures_exploratory/Train/MissingValues/", "MissingValues_cut10.pdf", sep="") , missing_distribution_sf_plot_10, width=16, height=9) 
ggsave(paste("./figures/figures_exploratory/Train/MissingValues/", "MissingValues_cut.pdf", sep="") , missing_distribution_sf_plot, width=16, height=9) 

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
print(yieldData_meteo[yieldData_meteo$comId == list_delete[[k]],1:6]) # 2008 hier scheint nur ein Jahr ein Messfehler zu sein: diesen Lösche ich 
}
' Das Fehlen der Dateneinträge schein meistens systematisch zu sein, da es meisten Blöcke sind, welche fehlen'

## Delete comIds with at least one observation missing ##
temp <- yieldData_meteo
for (k in 1:length(list_delete))
{
  print(yieldData_meteo[yieldData_meteo$comId==list_delete[k],])
  temp <- (temp[!temp$comId==list_delete[k],])
}


## Number of deleted rows ##
dim(temp) - dim(yieldData_meteo)

## Further use old name for data.frame
yieldData_meteo <- temp

length(unique(yieldData_meteo$comId)) # 334 /262 (only full comIds)

##################################
## Clean up data after deleting ##
rownames(yieldData_meteo) <- NULL
str(yieldData_meteo)
# yieldData_meteo <- plm.data(yieldData_meteo, index=c("comId", "year")) 
yieldData_meteo[,c("comId","year")] <- lapply(yieldData_meteo[,c("comId","year")], factor )


# ##################################################
# #### Remove log trend of indepedent variable ####
# ################################################
trend <- lm(siloMaize ~ as.integer(year), data = yieldData_meteo)
summary(trend)

'Da es keinen Trend gibt, detrenden wir die Daten nicht mehr. Dennoch lass ich weiterhin siloMaize_logtrend im Datensatz '

# # ###############################################
# # #### Compare different silage Maize Versions ##
# summary(log(yieldData_meteo$siloMaize))
# summary(yieldData_meteo$siloMaize)
# summary(yieldData_meteo$siloMaize_logtrend)
# 
# hist(log(yieldData_meteo$siloMaize))
# hist(yieldData_meteo$siloMaize)
# hist(yieldData_meteo$siloMaize_logtrend)
# ' Auch die Verteilung gibt keinen wirklichen Anlass zu detrenden, oder den logaithmus zu benutzen.'

#####################################################
#### Issue with Outliers - Correct Typo in Data ####
###################################################
' based on outlier statistic in regression on trend'
par(mfrow = c(2,2))
plot(trend) 

## Look at Outliers Values ##
yieldData_meteo[c(1785, 934, 1383),] # comId 6532, 5116, 5319

## Look at other values of outliers com #
yieldData_meteo[yieldData_meteo$comId == "6532",] # 2008 hier scheint nur ein Jahr ein Messfehler zu sein: diesen korrigiere ich 86.4 <- 486.4
yieldData_meteo[yieldData_meteo$comId == "5116",] # 2006 verändere ich nicht 
yieldData_meteo[yieldData_meteo$comId == "5913",] # 2003 verändere ich nicht 

## Correct Typo in Data ##
yieldData_meteo[c(yieldData_meteo$comId == "6532" & yieldData_meteo$year == "2008"), 6] <- 486.4

##########################################################
#### Change Variable Names: Prec -> P, Tavg -> T ####
########################################################
names(yieldData_meteo) <- gsub("Prec", "P", names(yieldData_meteo))
names(yieldData_meteo) <- gsub("Tavg", "T", names(yieldData_meteo))

######################################################################
#### Retrieve anomaly correction: comId specific mean siloMaize) ####
####################################################################
' The bias correction is used later to correct the predicted yield for the mean of siloMaize for each comId. 
This is particular important for  the plotting acticities (corrected for average expected silage maize (log). '
dim(yieldData_meteo)
anomaly_correction <-yieldData_meteo %>% group_by(comId) %>% summarise(average_yield = mean(siloMaize))
str(anomaly_correction)
summary(anomaly_correction$average_yield)

#### Make a map of average values ####
anomaly_correction_sf <- merge(vg2500_krs, anomaly_correction, by.x="RS", by.y="comId")
dim(anomaly_correction)
dim(anomaly_correction_sf)

## Anomaly
anomaly_correction_sf_plot <- 
  ggplot(anomaly_correction_sf) + 
  geom_sf(data=vg2500_krs,fill="white") + 
  geom_sf(aes(fill = average_yield)) +  
  guides(fill = guide_legend(title = "Av. Yield "))


ggsave(paste("./figures/figures_exploratory/Train/", "AverageYield.pdf", sep=""), anomaly_correction_sf_plot, device="pdf", width=8, height= 8) 

#######################################################
#### Append anomaly correction on yieldData_meteo #####
#######################################################

## Make data.frame with values of anomaly correction ##
anomaly_correction_df <- anomaly_correction[rep(seq_len(17*334)),]
anomaly_correction_df$average_yield <- as.factor(anomaly_correction_df$average_yield)

anomaly_correction_df <- do.call("rbind", replicate(17, anomaly_correction, simplify = FALSE))
anomaly_correction_df$year <- as.factor(sort(rep(seq(1999,2015),334)))
str(anomaly_correction_df)
View(anomaly_correction_df)


#### Merge with yieldData_meteo #####
yieldData_meteo_anomaly <- merge(yieldData_meteo, anomaly_correction_df, by=c("comId","year"))
str(yieldData_meteo_anomaly)
str(yieldData_meteo)


################################################################
##### Save newly created data.frane yieldData_meteo extern ####
################################################################
write.csv(yieldData_meteo_anomaly, file="./data/data_processed/yieldData_meteo_step_cut9_anomaly.csv")


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
# str(yieldData_meteo)
# ## Scale all the data within a comId ##
# yieldData_meteo_demean1  <- ddply(yieldData_meteo,  ~ comId,  numcolwise(scale, scale=FALSE))
# 
# summary(yieldData_meteo_demean1)#
# str(yieldData_meteo_demean1)
# names(yieldData_meteo_demean1)
# 
# #########################################################################
# #### Compare the data demeaned by ddplyr with manually demeand Data ####
# 
# ## ddplyr
# head(yieldData_meteo_demean1,18)[5]
# ## manually
# # head(yieldData_meteo)[8]
# yieldData_meteo[1:18,8] - mean(yieldData_meteo[1:17,8]) 
# ' Da der 18. Wert nicht mehr übereinstimmt scheint dass demeanen für die einzelnen comIds zu funktionieren.'
# 
# #######################################################################################################
# #### Comprise full demeaned data.frame  also including temporal information and stepwise functions ####
# ## Append Year ##
# year <- yieldData_meteo$year
# yieldData_meteo_demean  <- cbind(year, yieldData_meteo_demean1)
# summary(yieldData_meteo_demean)
# names(yieldData_meteo_demean)
# 
# ## Change Order that comId is first column ##
# yieldData_meteo_demean  <- yieldData_meteo_demean[,c(2,1,3:length(names(yieldData_meteo_demean)))]
# names(yieldData_meteo_demean )
# 
# ## Append stepwise functions ##
# names(yieldData_meteo)[(length(names(yieldData_meteo))-2):(length(names(yieldData_meteo)))]
# names(cbind(yieldData_meteo_demean, yieldData_meteo[(length(names(yieldData_meteo))-2):(length(names(yieldData_meteo)))]))
# 
# yieldData_meteo_demean <- cbind(yieldData_meteo_demean, yieldData_meteo[(length(names(yieldData_meteo))-2):(length(names(yieldData_meteo)))])
# str(yieldData_meteo_demean )
# 
# ############################################################
# #### Compare results of plm, demean, and lm with dummy ####
# ##########################################################
# 
# #### linear ####
# ## plm
# plm.fit_lin <- plm(siloMaize ~ SMI_Jun6 + SMI_Aug6 + I(T_Jul)  ,
#                data = yieldData_meteo,  effect="individual", model=("within"), index = c("comId","year"))
# summary(plm.fit_lin)
# 
# ## demean
# lm.fit_demean_lin <- lm(siloMaize ~  SMI_Jun6 + SMI_Aug6 + I(T_Jul) , data = yieldData_meteo_demean)
# summary(lm.fit_demean_lin)  
# 
# ## LSDV 
# lm.fit_dummy_lin <- lm(siloMaize ~ SMI_Jun6 + SMI_Aug6  + I(T_Jul) + comId, data = yieldData_meteo)
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
#                    data = yieldData_meteo,  effect="individual", model=("within"), index = c("comId","year"))
# summary(plm.fit_nonlin) # Adj.  R-sq  0.33496 (siloMaize), 0.31927 (siloMaize_logtrend)
# 
# ## demean
# lm.fit_demean_nonlin <- lm(siloMaize ~  SMI_Jun6 + SMI_Aug6 + I(T_Jul)  + I(T_Jul^2) , data = yieldData_meteo_demean)
# summary(lm.fit_demean_nonlin) # Adj.  R-sq  0.3611  (siloMaize),  0.3454  (siloMaize_logtrend)
# 
# ## LSDV 
# lm.fit_dummy_nonlin <- lm(siloMaize ~ SMI_Jun6 + SMI_Aug6  + I(T_Jul)  + I(T_Jul^2) + factor(comId), data = yieldData_meteo)
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
                                       data = yieldData_meteo)
summary(lm.fit_SMI_6_Jun_Aug_comId )  # Adjusted R-squared:  0.6939 (silomaize_logtrend) , Adjusted R-squared:  0.6964 (silomaize - only full observations),
                                      # Adjusted R-squared:  0.6857  (silomaize - keep comIds with at least nine observations)


#### Compare to plm.fit ####
plm.fit_SMI_6_Jun_Aug_comId <- plm(siloMaize ~ T_Jul + P_Jul  + I(T_Jul^2) +  I(T_Jul^3) + I(P_Jul^2) +  I(P_Jul^3) + SMI_Jun6 + SMI_Aug6,
                                 data = yieldData_meteo, effect="individual", model=("within"), index = c("comId","year"))
summary(plm.fit_SMI_6_Jun_Aug_comId ) # Adjusted R-squared:  0.35306 (silomaize_logtrend) , 0.36876 (silomaize - only full observations)
                                      # Adjusted R-squared:  0.33616  (silomaize - keep comIds with at least nine observations)


#############################################
#### Fit best model from paper 1 - yield ####
lm.fit_SMI_6_Jul_comId <- lm(siloMaize ~ T_Jul + P_Jul  + I(T_Jul^2) +  I(T_Jul^3) + I(P_Jul^2) +  I(P_Jul^3) + SMI_Jul6 + comId,
                                 data = yieldData_meteo)
summary(lm.fit_SMI_6_Jul_comId )  # Adjusted R-squared:  0.6694 (silomaize_logtrend) , Adjusted R-squared:  0.669 (silomaize- only full observations),
                                  # Adjusted R-squared:  0.661  (silomaize - keep comIds with at least nine observations)


#### Compare to plm.fit ####
plm.fit_SMI_6_Jul_comId <- plm(siloMaize ~ T_Jul + P_Jul  + I(T_Jul^2) +  I(T_Jul^3) + I(P_Jul^2) +  I(P_Jul^3) + SMI_Jul6,
                                   data = yieldData_meteo, effect="individual", model=("within"), index = c("comId","year"))
summary(plm.fit_SMI_6_Jul_comId ) # Adjusted R-squared:  0.30122 (silomaize_logtrend) , 0.31181(silomaize- only full observations),
                                  # Adjusted R-squared:  0.28411  (silomaize - keep comIds with at least nine observations)


 
#########################################
#### Fit combined model - log(yield) ####
lm.fit_SMI_6_Jun_Aug_comId_log <- lm(log(siloMaize) ~ T_Jul + P_Jul  + I(T_Jul^2) +  I(T_Jul^3) + I(P_Jul^2) +  I(P_Jul^3) + SMI_Jun6 + SMI_Aug6 + comId,
                                 data = yieldData_meteo)
summary(lm.fit_SMI_6_Jun_Aug_comId_log )  # Adjusted R-squared:  0.6939 (silomaize_logtrend) , Adjusted R-squared:  0.6964 (silomaize - only full observations),
# Adjusted R-squared:  0.6857  (silomaize - keep comIds with at least nine observations)
# Adjusted R-squared:  0.6869  (log(silomaize) - keep comIds with at least nine observations)


#### Compare to plm.fit ####
plm.fit_SMI_6_Jun_Aug_comId_log <- plm(log(siloMaize) ~ T_Jul + P_Jul  + I(T_Jul^2) +  I(T_Jul^3) + I(P_Jul^2) +  I(P_Jul^3) + SMI_Jun6 + SMI_Aug6,
                                   data = yieldData_meteo, effect="individual", model=("within"), index = c("comId","year"))
summary(plm.fit_SMI_6_Jun_Aug_comId_log ) # Adjusted R-squared:  0.35306 (silomaize_logtrend) , 0.36876 (silomaize - only full observations)
# Adjusted R-squared:  0.33616  (silomaize - keep comIds with at least nine observations)
# Adjusted R-squared:  0.33104  (log(silomaize) - keep comIds with at least nine observations)


##################################################
#### Fit best model from paper 1 - log(yield) ####
lm.fit_SMI_6_Jul_comId_log <- lm(log(siloMaize)~ T_Jul + P_Jul  + I(T_Jul^2) +  I(T_Jul^3) + I(P_Jul^2) +  I(P_Jul^3) + SMI_Jul6 + comId,
                             data = yieldData_meteo)
summary(lm.fit_SMI_6_Jul_comId_log )  # Adjusted R-squared:  0.6694 (silomaize_logtrend) , Adjusted R-squared:  0.669 (silomaize- only full observations),
# Adjusted R-squared:  0.661  (silomaize - keep comIds with at least nine observations)
# Adjusted R-squared:   0.6859  (log(silomaize) - keep comIds with at least nine observations)

#### Compare to plm.fit ####
plm.fit_SMI_6_Jul_comId_log <- plm(log(siloMaize) ~ T_Jul + P_Jul  + I(T_Jul^2) +  I(T_Jul^3) + I(P_Jul^2) +  I(P_Jul^3) + SMI_Jul6,
                               data = yieldData_meteo, effect="individual", model=("within"), index = c("comId","year"))
summary(plm.fit_SMI_6_Jul_comId_log ) # Adjusted R-squared:  0.30122 (silomaize_logtrend) , 0.31181(silomaize- only full observations),
# Adjusted R-squared:  0.28411  (silomaize - keep comIds with at least nine observations)
# Adjusted R-squared: 0.28289,   (log(silomaize) - keep comIds with at least nine observations)


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
  predictData_train <- list(1:262)
  predictData_train_anomaly  <- list(1:262)
  
  ## Start loop ##
    for(m in 1:17){
    ## Create data.frame for the year m ##
    yieldData_meteo_year <- yieldData_meteo[yieldData_meteo$year == listyear[[m]], ]
    dim(yieldData_meteo_year)
    length(predict(lm.fit_SMI_6_Jun_Aug_comId, newdata = yieldData_meteo_year ))
    
    ## Predict model on that data.frame ##
    ## Jun_Jul_Aug
    predict_year <- as.data.frame(predict(modelList[[i]], newdata = yieldData_meteo_year ))
    # dim(predict_year )
    
    ## Correct for the mean(detrend(log(silagmaize yield)))
    predict_year_anomaly  <- predict_year - anomaly_correction$mean
    # predict_year_anomaly_july  <- predict_year_july - anomaly_correction$mean
    
    ## Change names to year m ##
    ## Jun_Jul_Aug
    names(predict_year) <-  listyear[[m]] 
    names(predict_year_anomaly ) <- paste(listyear[[m]],"anomaly",sep="_")
    
   
    ## Append to create a large data.frame
    ## Jun_Jul_Aug
    predictData_train <- cbind(predictData_train, predict_year)
    predictData_train_anomaly <- cbind(predictData_train_anomaly, predict_year_anomaly )
  }
  
  ## Check created data.frames ##
  # dim(predictData_train)
  # dim(predictData_train_anomaly)
  
  # str(predictData_train)
  # str(predictData_train_anomaly)
  
  
  
  ##############################################
  #### Include Spatial Information (comIds) ####
  # dim(yieldData_meteo_year[1])
  
  ##########################
  ## Prediction (absolut) ##
  
  ## Absolute
  predictData_train <- cbind(yieldData_meteo_year[1], predictData_train)
  
  rownames(predictData_train) <- NULL
  predictData_train$`1:262` <- NULL
  # head(predictData_train, 15)
  
  
  ##########################
  ## Prediction (anomaly) ##
  predictData_train_anomaly <- cbind(yieldData_meteo_year[1], predictData_train_anomaly)
  
  rownames(predictData_train_anomaly) <- NULL
  predictData_train_anomaly$`1:262` <- NULL
  
  
  ##############################################################################################
  #### Load Shape with SpatialInformation for the Polygones of the Administrative Districts ####
  ' necesarry to make plots '
  vg2500_krs <- read_sf("/Storage/ownCloud/Home/Klimabuero/Proj2/data/data_proj/Input/CLC/", "vg2500_krs")
  str(vg2500_krs, 2)
  vg2500_krs$RS
  
  #### Change RS to five digits #####
  vg2500_krs$RS <- as.integer(str_sub(vg2500_krs$RS, 1,5))
  vg2500_krs$RS
  
  plot(vg2500_krs$geometry)
  
  #################################################################
  #### Convert predictData_train - data.frame to sf.data.frame ####
  
  #### Merge sf.data.frame with normal data.frame to allow plots ####
  
  ## Absolute 
  predictData_train_sf <- merge(vg2500_krs, predictData_train, by.x = "RS", by.y = "comId", all=T, sort=T) 
  # str(predictData_train_sf )
  
  ## Anomaly 
  predictData_train_anomaly_sf <- merge(vg2500_krs, predictData_train_anomaly, by.x = "RS", by.y = "comId", all=T, sort=T) 
  # str(predictData_train_sf )
  
  
  #### Change names of predictData_train_sf ####
  names(predictData_train_sf) <- gsub("X","", names(predictData_train_sf))
  names(predictData_train_anomaly_sf) <- gsub("X","", names(predictData_train_anomaly_sf))
  
  
  
  #############################################
  #### Plot Yield Deviations for each year ####
  
  #### Choose color Setting ####
  ## Absolute
  summary(predictData_train_sf)
  myPalette <- colorRampPalette((brewer.pal(11, "BrBG")))
  sc <- scale_fill_gradientn("Yield Deviation", colours = myPalette(100), limits=c(250, 650))
  
  ## Anomaly
  summary(predictData_train_anomaly_sf)
  myPalette_anomaly <- colorRampPalette((brewer.pal(11, "BrBG")))
  sc_anomaly <- scale_fill_gradientn("Yield Deviation", colours = myPalette(100), limits=c(-100, 100))
  
  #### Plot Predictions for each of the train Data ####
  for(r in 1:17){
    
    ## Absolute 
    predictData_train_sf_plot <- 
      ggplot(predictData_train_sf) + 
      geom_sf(aes(fill = predictData_train_sf[[r+5]])) + 
      ggtitle(paste(names(predictData_train_sf)[[r+5]])) + sc + 
      theme_bw()
    
   ggsave(paste("./figures/figures_exploratory/Train/", modelListNames[[i]],  "/Yield_predict_", listyear[r],".pdf", sep=""), predictData_train_sf_plot, device="pdf", width=8, height= 8) 
  
    ## Anomaly
   predictData_train_anomaly_sf_plot <- 
     ggplot(predictData_train_anomaly_sf) + 
     geom_sf(aes(fill = predictData_train_anomaly_sf[[r+5]])) + 
     ggtitle(paste(names(predictData_train_anomaly_sf)[[r+5]])) + sc_anomaly + 
     theme_bw()
   
   ggsave(paste("./figures/figures_exploratory/Train/", modelListNames[[i]],  "/Yield_predict_anomaly_", listyear[r],".pdf", sep=""), predictData_train_anomaly_sf_plot, device="pdf", width=8, height= 8) 
 
 
}


' 
Achtung, die Reihenfolge ist nun anders. Ich denke aber, dass die sf richtig sind, und damit die sp Resultate falsch. 
Die Landkreise Stendal und Altmarkkreis Salzwedel sind bei sp nicht dargestellt, bei sf schon. Diese LK haben aber durchgehende Daten und sollten 
daher berücksichtigt sein. 
Des Weiteren is mir aufgefallen, dass die Veränderung der Reihenfolge bei sf keine Auswirkung auf das plotten hat. Dahr arbeite ich weiterhin mit den sortierten Daten,
i.e. merge(,sort=T)
Generell sind die Ergebnisse zwischen sp und sf output ähnlich. 
'



##########################################################
#### Calculate sums of the predictions for each comId ####
predictData_train_sums <- predictData_train_anomaly[,-1]
predictData_train_rowsums <- as.data.frame(rowSums(predictData_train_sums))
summary(predictData_train_rowsums) 
' Die berechneten rowsums sind alle sehr nahe an Null'
#### Add comIds ####
predictData_train_rowsums <- cbind(predictData_train[,1], predictData_train_rowsums )
names(predictData_train_rowsums) <- c("comId", "PredictSums")

str(predictData_train_rowsums )


#### Merge with Sf.data.frame ####
predictData_train_rowsums_sf <- merge(vg2500_krs, predictData_train_rowsums, by.x = "RS", by.y = "comId", all.x=T, sort=F) 
str(predictData_train_rowsums_sf )

#### Plot sums of deviation for each comId ####

#### Choose color Setting ####
summary(predictData_train_rowsums_sf )
myPalette <- colorRampPalette((brewer.pal(11, "BrBG")))
sc <- scale_fill_gradientn("Yield Deviation (%)", colours = myPalette(100), limits=c(-0.1 , 0.1))

#########################################################################
#### Plot sums of Predictions over years of each comId in train Data ####
predictData_train_rowsums_plot <- 
    ggplot(predictData_train_rowsums_sf) + 
    geom_sf(aes(fill = PredictSums)) + 
    ggtitle("Sums of prediction for each year") + sc + 
    theme_bw()
  
ggsave(paste("./figures/figures_exploratory/Train/", modelListNames[[i]],  "/Yield_predict_sumsComId.pdf", sep=""), predictData_train_rowsums_plot , device="pdf", width=8, height= 8) 


} ## END OF LOOP WHICH LOOPS THROUGH THE DIFFERENT MODELS 


##############################################################################################################################################################################
##############################################################################################################################################################################
#### Climate Projections #####
##############################


#########################################################################################
#### Preparations to make Predictions for the data derived from five climate models ####
#######################################################################################
' Since I am not using the same data on which the model is trained I need to implement some changes. 
  In particular I cannot work with factor(comId) but need to derive the model matrix to be able to use predict().'


#######################################################
#### Read in Climate Data with Stepwise Funcktions ####

yieldData_meteo <- read.csv(file="./data/data_raw/yieldData_meteo.csv")
yieldData_meteo$X <- NULL

###################################################################
#### Create data.frame including variables used for estimation ####
yieldData_meteo_short <- as.data.frame(yieldData_meteo[,names(yieldData_meteo)%in%c("year","comId", "siloMaize", 
                                                                                       "SMI_Jun6","SMI_Jul6", "SMI_Aug6", "T_Jul",
                                                                                       "Pre_Jul")])
names(yieldData_meteo_short)

#########################################
#### Append model.matrix explicitly ####
#######################################
'Here I include the model.matrix for the comID exclusively since the  predict command had issues to deal with those. 
Also, only the data used in the models are considered further.'

## Create model.matrix ##
modelmatrix <- model.matrix(~ yieldData_meteo_short$comId)
dim(modelmatrix)

## Convert model.matrix to data.frame ##
modelmatrix_Df <-as.data.frame((modelmatrix))
str(modelmatrix_Df)
modelmatrix_Df$`yieldData_meteo_short$comId1002` # There is a one for each year in the data when comId == 1002 is true

# ## Convert all numeric to factor ##
# modelmatrix_Df <- lapply(modelmatrix_Df, factor )

## Delte modelmatrix ##
rm(modelmatrix)

## Cbind modelmatrix with short data.frame ##
yieldData_meteo_modelmatrix <- cbind(yieldData_meteo_short, modelmatrix_Df)
yieldData_meteo_modelmatrix$`(Intercept)` <- NULL

## Clean up names  ##
x <- make.names(names(yieldData_meteo_modelmatrix))
str(x)
x
colnames <- gsub("yieldData_meteo_short.", "", x)
colnames(yieldData_meteo_modelmatrix) <- colnames
head(yieldData_meteo_modelmatrix)


########################################
#### Fit model used for prediction ####
######################################

## Delete columns year and comId ##
names(yieldData_meteo_modelmatrix)
yieldData_meteo_modelmatrix$year <- NULL
yieldData_meteo_modelmatrix$comId <- NULL

## fit model on data.frame with explizit model.matrix for comId ##
str(yieldData_meteo_modelmatrix)
summary(yieldData_meteo_modelmatrix$siloMaize)

## lm.fit ##
## June to August
head(yieldData_meteo_modelmatrix)[, c(-4, -6)]
lm.fit_SMI_6_Jun_Aug_modelmatrix <- 
  lm(siloMaize ~ I(T_Jul^2) + I(T_Jul^3)  +I(Pre_Jul^2) +  I(Pre_Jul^3) + .   ,
     data = yieldData_meteo_modelmatrix[, -5])
summary(lm.fit_SMI_6_Jun_Aug_modelmatrix) # Adjusted R-squared:  0.6964 

## July
lm.fit_SMI_6_Jul_modelmatrix <- 
  lm(siloMaize ~ I(T_Jul^2) + I(T_Jul^3)  +I(Pre_Jul^2) +  I(Pre_Jul^3) + .   ,
     data = yieldData_meteo_modelmatrix[, c(-4, -6)])
summary(lm.fit_SMI_6_Jul_modelmatrix) # Adjusted R-squared:   0.669 

###############################################
#### Generate frame of comIds to merge on ####
#############################################
' This data.frame only includes those comIds which have complete data for silage maize. Needed for projection data.'
ComIdMerge <- as.data.frame(unique(yieldData_meteo$comId))
ComIdMerge 
colnames(ComIdMerge ) <- "comId"


########################################################################################################
#############################################################
#### Loop for different models used to make predictions ####
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
  namelist_models <- c("MPI","DMI","KNMI","ICTP","SMHI")
  
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
    head(predictData)
    
    predictData_anomaly  <- NULL
    predictData_anomaly  <- ComIdMerge 
    head(predictData_anomaly )
    
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
                                                                            "SMI_Jun6","SMI_Jul6", "SMI_Aug6", "T_Jul", "Pre_Jul")])
    str(NewValues_short)
    
    ##############################################################################################
    #### Merge with Yield Data Frame to get same comIds, i.e. those which only have full data ####
    NewValues_merge <- merge(NewValues_short, ComIdMerge ,  by="comId") #
    str(NewValues_merge) # 39038/149 = 262 : passt also, da wir nur noch 262 comIds übrig haben
    
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
      NewValuesyear <- NewValues_modelmatrix[NewValues_modelmatrix$year == listyear[[l]], ]
      rownames(NewValuesyear) <- NULL
      str(NewValuesyear)
      dim(NewValues)
      
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
      #### Clear Prediction for mean yield ####
      dim(predictlm)
      dim(anomaly_correction)
      predictlm_anomaly  <- predictlm - anomaly_correction[,2]
      summary(predictlm_anomaly )
      # summary(lm.fit_SMI_6_Jun_Aug_modelmatrix)
      
      #### Change name of predictlm_anomaly  data.frame ####
      names(predictlm_anomaly ) <- paste(listyear[l])
      
      
      #### Manual Prediction for com1001 ####
      ' The calculation of the manual prediction can be found in BasePrediction_long. However, it made sense.'
      
      names(predictlm_anomaly ) <- paste(listyear[l], sep="")
      
      #### Combine to one large data.frame including all the years ####
      predictData           <- cbind(predictData , predictlm )
      predictData_anomaly   <- cbind(predictData_anomaly  , predictlm_anomaly  )
      
      #### End of loop through all the years of one climate model ####
    }
    
    #########################################################
    #### Save one wide data.frame for each climate model ####
    str(predictData)
    str(predictData_anomaly )
    
    names(predictData)
    names(predictData_anomaly )
    
    # write.csv(predictData,          paste("./data/data_proj/output/",modelListMatrixNames[[s]],"/Yield_predict_1951-2099_wide_",namelist_models[[r]], ".csv", sep=""))
    # write.csv(predictData_anomaly , paste("./data/data_proj/output/",modelListMatrixNames[[s]],"/Yield_predict_anomaly_1951-2099_wide_",namelist_models[[r]], ".csv", sep="") )
    # 
    #########################
    #### Convert to tidy ####
    
    ## Data.frane with absolut predictions
    predictData_tidy <- predictData %>% gather(year, Y_absolut, 2:150, factor_key = T)
    str(predictData_tidy)
    levels(predictData_tidy$year)
    
    ## Data.frane with anomaly predictions
    predictData_anomaly_tidy <- predictData_anomaly  %>% gather(year, Y_anomaly, 2:150, factor_key = T)
    str(predictData_anomaly_tidy )
    
    #### Create data.frame defining the model names ####
    model <- as.data.frame(rep(namelist_models[[r]], dim(predictData_tidy)[1]))
    names(model) <- "model"
    
    #### Append Model Name to tidy data.frame ####
    predictData_tidy <- cbind(model, predictData_tidy)
    predictData_anomaly_tidy <- cbind(model, predictData_anomaly_tidy)
    
    # #########################################################
    # #### Save one tidy data.frame for each climate model ####
    # write.csv(predictData_tidy, paste("./data/data_proj/output/",modelListMatrixNames[[s]],"/Yield_predict_1951-2099_tidy_",namelist_models[[r]], ".csv", sep=""))
    # write.csv(predictData_anomaly_tidy,paste("./data/data_proj/output/",modelListMatrixNames[[s]],"/Yield_predict_anomaly_1951-2099_tidy_",namelist_models[[r]], ".csv", sep="") )
    # 
    # 
  
    #### Combine to one large data.frame including all models ####
    predictData_tidy_all <- rbind(predictData_tidy_all, predictData_tidy)
    predictData_anomaly_tidy_all <- rbind(predictData_anomaly_tidy_all, predictData_anomaly_tidy)
    str(predictData_tidy_all )
    
  
  } ## End of loop which goes through different climate models

########################################################################################################
#### Save one tidy data.frame for each absolute and anomaly predictions including all climate model ####
# dim(predictData_tidy_all)
# write.csv(predictData_tidy_all, paste("./data/data_proj/output/",modelListMatrixNames[[s]],"/Yield_predict_1951-2099_tidy_all.csv", sep=""))
# write.csv(predictData_anomaly_tidy_all,paste("./data/data_proj/output/",modelListMatrixNames[[s]],"/Yield_predict_anomaly_1951-2099_tidy_all.csv", sep="") )

#####################################################################################
#### Combine absolute predictions and anomaly predictions to one tidy data.frame ####
str(predictData_tidy_all)
str(predictData_anomaly_tidy_all)

names(predictData_tidy_all)[4] <- "Y"

predictData_tidy_complete <- merge(predictData_tidy_all, predictData_anomaly_tidy_all)


write.csv(predictData_tidy_complete, paste("./data/data_proj/output/",modelListMatrixNames[[s]],"/Yield_predict_complete_1951-2099_tidy.csv", sep="") )

} ## End of loop which uses different models to make predictions

str(predictData_tidy_complete )
