#### File Description ####
'
# Ziel ist es die SMI und Daten der Regionalstatistik zu mergen --> tidy data
  - Mergen: Anpassen der ComIds und Abgleichen notwendig, da es hier unterschiede gibt
  - Der finale Datensatz hat nur 410 räumliche Einheiten pro Jahr: Gründe dafür
  - Nur Kreis und Kreisfreie Städte berücksichtigt, also jede ComID mit 2 oder 3 Ziffern ist nicht berücksichtigt.

# Anschließend wird der Datensatz manipuliert, um diesen für weitere Arbeiten nutzen zu können. 
  - Hinzufügen von IDs und Namen der Bundesländer
  - Löschen der Daten, welche redundant sind
  - Ab dem Jahr 1995 
  - Output: Yield_SMI_long

# Anschließend erstelle ich lagged Daten des SMI für die Monate August bis December (Wachstumsyklus von Wintersorten)
  - Hier habe ich auch die yields==0 gelöscht und in NAs unbenannt
  - Output: Yield_SMI_arr (ab 1995) <- basierend auf Yield_SMI_long

# Anschließen kürze ich den Datensatz von 1995 auf 1999
  - Output: Yield_SMI basierend auf Yield_SMI_arr

# Anschließend Manipulation der Yield Daten
  - Datensatz basierend auf Yield_SMI 
  - Standardisierung der Yield Daten -> detrended z-score  (vor allem für Luis Fuzzy Rule und andere Data Mining Ansätze)
  - Output: Yield_SMI_detrend (detrended z-score of all crops an 1999)

# steht noch aus  
  - Detrending der absoluten Werte (für Panelansatz)
    - detrend() scheint nicht wirklich zu funktionieren, daher muss das manuel gemacht werden.
  



'
#### Dependencies and Input ####
'
# yieldData (Regionalstatistik) # Achtung, habe ich in in  yieldData.csv in data_processed Ordner umbennant
# KreisSMI_spdf_1995 (4km_SMI)
'

#### Vorgehen beim bearbeiten der SMI Daten ####
'Ziel: Ich habe 412 Beobachtungen pro Zeitschritt, also jeweils ein Monat in einem Jahr. 
Da mein Ziel-Datensatz tidy ist, also die Jahre untereinander fortlaufend angeordnet sind, müssen meine Variablen entsprechend angepasst werden.
Soll heißen aus den SMI Variablen mit Monats und Jahres zuordnung wird nun ein SMI mit Monats Zuordnung und die Jahre verschmelzen alle in 
eine Spalte. 
Damit habe ich 12 Spalten für den SMI, also eine pro Monat, und 16 (Jahre) mal 412 Beobachtungen, also 6592 Beobachtungen, je Spalte.
Für dieses Vorgehen reshape ich den Kreis_SMI_dataframe von wide nach long mit Hilfe der stats::reshape function. 
Dazu muss ich eine Liste mit den SMI mit Monat und Jahres Zuordnung erstellen, welche dann jeweils in die SMI mit Monats zuordnung
im long Format übergeht. -> listMonthYearNames[[i]]
Anschließend erstelle ich die Namen der SMI mit reiner Monats Zurdnung -> listMonthNames
Mit Hilfe der Year Variablen werden dann die Beeobachtungen innerhalb der SMI mit Monats Zuordnungen nach den Jahren differenziert.
'

#### Anzahl der Communities ####
'
- yieldData 525
- Kreis_SMI_spdf_1995: 410
    * Nur fünstellige ComIDs berücksichtigt: 55 ComIds weniger
    * Landkreis Aachen:  Der Kreis Aachen wurde gemäß Aachen-Gesetz mit Ablauf des 20. Oktober 2009 aufgelöst, und aus den neun Gemeinden des Kreises Aachen und 
                         der Stadt Aachen wurde mit Wirkung vom 21. Oktober 2009 als neuer Gemeindeverband die Gebietskörperschaft Städteregion Aachen gebildet
    * Kreisreformen: Sachsen(2008: 35 ComIds weniger) und Sachsen-Anhalt(2007: 24 ComIds weniger)
'

####  Validäts Check des Merge Prozesses ####
'
  - Die Daten machen nach den Validätschecks einen soliden Eindruck

'

#### Ouput ####
'
  - Yield_SMI_long (Daten ab 1995)
  - Yield_SMI (Daten ab 1999)
  - Yield_norm_1999 (Daten für cdf kernel smoother)
  - Yield_SMI_detrend ( this data are used further to combine it with other explanatory variables)
  - 
  
'

#### Hintergrund ####
### Hadely Wickham: Tidy Data ####
'
Tidy Data als Ziel, da diese auch als Panel Daten Format bezeichnet werden
Begriff tidy data, wie er von Hadley Wickham benutzt wird:
Jede Variable hat eine Spalte, jede Beobachtung ist eine Zeile, ?jede Observational unit bildet einen table?

id year var1 var2 var3 etc.
'

#### Pakete ####
library(sp)
library(rgdal)
library(raster)
library(rasterVis)
library(maptools)
library(plyr)
library(ncdf4)
library(reshape)
library(stringr)
library(eeptools)
library("ggplot2")
library("foreign")
library("plm")
library("car")
library("lmtest")
library("lattice")
library("zoo")
library("scales")
library("nlme")
library("lme4")
library("mgcv")
library("apsrtable")
library("texreg")
library("DataCombine")
library("reshape2")
library("gplots")
library("pracma")


#############################################################################################################################################################################################
#############################################################################################################################################################################################

#### Load large table with all the variables of regionalstatistik ####
  yieldData <- read.table("/Storage/ownCloud/Home/Klimabuero/Proj1/data/data_raw/Yield2015.csv", sep=";", dec=",", quote="\"", na.strings=(c("-","/", ".")),
                        col.names=c("year","comId","com","winterWheat","rye","winterBarley","summerBarley","oats","triticale","potatoes","sugarBeet","winterRape",
                                    "siloMaize"),   nrows=8925, skip=7, colClasses=c("integer","factor", "factor", rep("numeric", 10)))
# View(yieldData)
str(yieldData)
head(yieldData)
tail(yieldData)

### Explore yieldData ####
head(yieldData)
tail(yieldData)
names(yieldData)
table(yieldData$year) # hier stimmt noch die Anzahl der Kreis pro Jahr
table(yieldData$comId) 
summary(yieldData)

#########################################################################
#### Load SpatialPolygonesDataframe mit meteorological Daten ab 1995 #### 
PET_months <- read.csv("data/data_processed/PET_months.csv", colClasses=c("factor", "factor", "factor", "factor", "factor", "factor",rep("numeric", 17)))
PET_months$X <- NULL
head(PET_months)
dim(PET_months)
unique(PET_months$year)

Prec_months <- read.csv("data/data_processed/Prec_months.csv", colClasses=c("factor", "factor", "factor", "factor", "factor", "factor",rep("numeric", 17)))
Prec_months$X <- NULL
head(Prec_months)
dim(Prec_months)

Tavg_months <- read.csv("data/data_processed/Tavg_months.csv", colClasses=c("factor", "factor", "factor", "factor", "factor", "factor",rep("numeric", 17)))
Tavg_months$X <- NULL
head(Tavg_months)
dim(Tavg_months)

SMI_months <- read.csv("data/data_processed/SMI_months.csv", colClasses=c("factor", "factor", "factor", "factor", "factor", "factor",rep("numeric", 17)))
SMI_months$X <- NULL
head(SMI_months)
dim(SMI_months)

#######################################
## Mergen der Meteorologischen Daten ##
#######################################
names(SMI_months)
names(yieldData)
unique(SMI_months$year)
unique(yieldData$year) 

head(yieldData)
head(SMI_months)

tail(yieldData)
tail(SMI_months)

mergeNames <- names(SMI_months)[names(SMI_months)%in%names(Tavg_months)]
mergeNames

Meteo_long <- merge(Tavg_months,SMI_months, by=mergeNames)
Meteo_long <- merge(Prec_months,Meteo_long, by=mergeNames)
Meteo_long <- merge(PET_months,Meteo_long, by=mergeNames)
head(Meteo_long)
dim(Meteo_long)

table(Meteo_long$year) # 412 Beobachtungen in den 21 Jahren, posst
table(Meteo_long$comId)
###################################################################################################################################################################################################
################################################################################# Manipulation of Yield Data ######################################################################################
###################################################################################################################################################################################################


##########################################
#### Delete zero values in yield Data ####
## Hier lösche ich alle Yield mit Wert 0, da dies nicht nachzuvollziehen ist
## Darünerhinaus sollen die Variablen Anomalien erklären, wenn angepflanzt wurde.
yieldData$winterWheat[yieldData$winterWheat==0] <- NA
yieldData$rye[yieldData$rye==0] <- NA
yieldData$winterBarley[yieldData$winterBarley==0] <- NA
yieldData$oats[yieldData$oats==0] <- NA
yieldData$triticale[yieldData$triticale==0] <- NA
yieldData$potatoes[yieldData$potatoes==0] <- NA
yieldData$sugarBeet[yieldData$sugarBeet==0] <- NA
yieldData$winterRape[yieldData$winterRape==0] <- NA
yieldData$siloMaize[yieldData$siloMaize==0] <- NA



#################################################################################################################################################################################
#################################################################################################################################################################################
###########################################################
#### Mergen von SMI_months mit yieldData anhand von RS ####
###########################################################

names(Meteo_long)
names(yieldData)


head(Meteo_long$comId)
head(yieldData$comId)

str(Meteo_long)
str(yieldData)

unique(Meteo_long$year)
unique(yieldData$year) 

head(yieldData)
head(Meteo_long)

tail(yieldData)
tail(Meteo_long)

mergeNames <- names(Meteo_long)[names(Meteo_long)%in%names(yieldData)]
mergeNames
yieldData_meteo <- merge(yieldData, Meteo_long, by=mergeNames)
head(yieldData_meteo)
dim(yieldData_meteo)

yieldData[yieldData$year==1999,][1:15,]
yieldData[yieldData$year==1999,][1:15,]

table(yieldData_meteo$year) # statt 525 nur noch 410 Beobachtungen
table(yieldData$year)
table(Meteo_long$year)

table(yieldData_meteo$comId)
table(yieldData$comId)
table(Meteo_long$comId)

###################################################
#### Neuen Datensatz auf Richigkeit überprüfen ####
head(yieldData_meteo) # geordnet nach comId
head(yieldData) # geordnet nach Jahr
head(Meteo_long)

Meteo_long[Meteo_long$comId==412, ][1:12]
yieldData[yieldData$comId==412, ][1:12]

head(yieldData[order(yieldData$comId, yieldData$year),], 34)[1:12]
head(yieldData_meteo[order(yieldData_meteo$comId, yieldData_meteo$year),])[1:12]

tail(Meteo_long[order(Meteo_long$comId, Meteo_long$year),])[c(3,5,(length(names(Meteo_long))-16):length(names(Meteo_long)))]
tail(yieldData_meteo[order(yieldData_meteo$comId, yieldData_meteo$year),])[c(1,2,(length(names(yieldData_meteo))-16):length(names(yieldData_meteo)))]

tail(Meteo_long[order(Meteo_long$comId, Meteo_long$year),])[c(3,5,(length(names(Meteo_long))-16):length(names(Meteo_long)))]%in%tail(yieldData_meteo[order(yieldData_meteo$comId, yieldData_meteo$year),])[c(1,2,(length(names(yieldData_meteo))-16):length(names(yieldData_meteo)))]
'scheint zu passen'

### Nur noch ComId mit der Länge 5 ###
yieldData_red <- yieldData[str_length(yieldData$comId)==5,]
unique(yieldData_red$comId)
length(unique(yieldData_red$comId)) # 470

##  Vergleichen der Beobachtungen pro Jahr
table(yieldData$year) # 525
table(yieldData_red$year) # hier sind es nur noch 470 Beobachtungen, also 55 weniger
# Durch das weglassen der IDs kleiner als 5 fallen 55 Werte weg; Jedoch hat 
table(year) # Nur 410 Werte

### Der Wegfall der anderen 60 ComIDs lässt sich wie folgt erklären ####
all(yieldData_red[yieldData_red$year==1999,][1:60,1:12]%in%yieldData[yieldData$year==1999,][1:60,1:12])

yieldData_red[yieldData_red$year==1999,][1:60,1:12]==yieldData[yieldData$year==1999,][1:60,1:12]

yieldData_red[yieldData_red$year==2005,][c(1:82),1:12]%in%yieldData[yieldData$year==2005,][1:82,1:12]

# Zumindest für die ersten 80 Beobachtungen scheint es übereinstimmend zu sein, 
# aber was passiert danach
yieldData_red[yieldData_red$year==2005,][80:90,1:12]
yieldData[yieldData$year==2005,][80:90,1:12]

# Bei yieldData fehlt der Kreis Aachen mit der comId 05354
## Fehlt der Kreis in den vorgänger Daten schon?

### KreisSMI_df_1995
any(levels(KreisSMI_df_1995$RS)=="05354")

# Das scheint der Fall zu sein

### Laden des ursprünglichen SpatialPolygonDataFrames
KreisPolOGR <- readOGR("./data/data_raw/4_michael/adminitrative_borders_Ger/","vg2500_krs")
dim(KreisPolOGR)

KreisPolOGR$RS=="053540000000"
# scheint es auch hier schon nicht zu geben
# Erklärung: Der Kreis Aachen wurde gemäß Aachen-Gesetz mit Ablauf des 20. Oktober 2009 aufgelöst, und aus den neun Gemeinden des Kreises Aachen und 
# der Stadt Aachen wurde mit Wirkung vom 21. Oktober 2009 als neuer Gemeindeverband die Gebietskörperschaft Städteregion Aachen gebildet
# Damit fällt eine Beobachtung weg

#### Weitere Inspektion der Daten ####
21*410
any(yieldData$comId == 01)
length(unique(yieldData$comId)) # es gibt pro Jahr 410 Beobachtungen
21*410/410 #= 16, so viele Jahre sind im Datensatz

# Welche weiteren IDs gibt es nicht?
yieldData_red[yieldData_red$year==2005,][c(1:82,84:361),1:12]%in%yieldData[yieldData$year==2005,][1:360,1:12]
# bis hierhin stimmmen die Datensätze überein

yieldData_red[yieldData_red$year==2005,][362:396,1:12]# comIds 13071 -14389 sind nicht im Datensatz yieldData zu finden, Grund Kreisreform Sachsen 2008
dim(yieldData_red[yieldData_red$year==2005,][362:396,1:12]) # damit fallen 35 Beobachtungen weg

yieldData[yieldData$year==2005,][361:398,1:12]
# das sind Landkreis aus dem Osten

# Diese Abschnitte stimmen wieder überein
yieldData_red[yieldData_red$year==2005,][397:423,1:12]%in%yieldData[yieldData$year==2005,][361:387,1:12]
yieldData_red[yieldData_red$year==2005,][c(1:82,84:361, 397:423),1:12]%in%yieldData[yieldData$year==2005,][1:387,1:12]
# Ab Wittenberg stimmen die Daten wieder nicht überein
yieldData_red[yieldData_red$year==2005,][c(1:82,84:361, 397:424),1:12]%in%yieldData[yieldData$year==2005,][1:388,1:12]
yieldData[yieldData$year==2005,][386:410,1:12] # 15091 ist noch in yieldData, 15101 
yieldData_red[yieldData_red$year==2005,][423:448 ,1:12] # 15091 ist noch in yieldData, 15101 bis 15370  fehlt, Grund Kreisgebietsreform 2007 in Sachsen Anhalt
dim(yieldData_red[yieldData_red$year==2005,][423:448 ,1:12]) # hier fallen damit 24 Beobachtungen weg

# Vollständiger Abgleich der beiden Datensätze
yieldData_red[yieldData_red$year==2005,][c(1:82,84:361,397:423,448:470),1:12]%in%yieldData[yieldData$year==2005,][1:410,1:12] 
# check, Unterschiede yieldData und yieldData_red sind erklärt

# Nun stellt sich die Frage, welche Landkreise aus dem orginalen SpatialPolygonesDataFrame fehlen und warum, da diese ursprünglich 412 statt jetzt 410 sind
yieldData_comID<-unique(yieldData$comId)[str_length(unique(yieldData$comId))==5]
KreisSMI_df_1995_RS<-unique(KreisSMI_df_1995$RS)


length(yieldData_comID)
length(unique(KreisSMI_df_1995_RS))

all(KreisSMI_df_1995_RS[c(1:14, 15,16:411)]%in%yieldData_comID[1:410])
KreisSMI_df_1995_RS[15] # 02000 wurde nicht übernommen: Hamburg

all(KreisSMI_df_1995_RS[c(1:14,16:325,327:412)]%in%yieldData_comID[c(1:410)])
KreisSMI_df_1995_RS[326] # 11000 wurde nicht übernommen: Berlin

KreisSMI_df_1995[KreisSMI_df_1995$RS==02000,]
unique(yieldData$comId)
yieldData[yieldData$year=="2000", 1:10]
yieldData[yieldData$year=="2001", 1:10]

############################################################################################################################################################################################
###################################################################### Manipulation des Datensatzes ########################################################################################
############################################################################################################################################################################################
##############################
#### Make state variables ####
##############################

## comIdState
# i.e. if comId starts with 1 give it a 01 and when it starts with 1 give it a 16 

class(unique(yieldData_meteo$comId))
yieldData_meteo$comId

  x <- yieldData_meteo$comId

str_sub(x, -3, -1) <- "" ;x
table(x)
yieldData_meteo$comIdState<-x
head(yieldData_meteo)
tail(yieldData_meteo, 20)
unique(yieldData_meteo$comIdState)

####################################
## make names for state variables ##
names(yieldData_meteo)

# make list of comStateIds und Namen
csIds_list<-list("comStateIds"=unique(yieldData_meteo$comIdState), "comStateNames"= c("Schleswig-Holstein","Lower Saxony", "Bremen","NRW", "Hesse","Rhineland-Palatinate","Baden-Württemberg","Bavaria","Saarland","Brandenburg","Mecklenburg-Vorpommern","Saxony","Saxony-Anhalt","Thuringia" )) ; csIds_list
csIds_list[[1]][1]
csIds_list[[2]][1]
seq_along(csIds_list$comStateIds)
seq_along(csIds_list$comStateNames) #beide sind gleich lang, die Stadtstaten Hamburg und Berlin sind raus

## Loop durch die Liste um  comStateNames den entsprechende comStateIds zuzuordnen
for (i in seq_along(csIds_list$comStateIds))
{
  yieldData_meteo$comState[yieldData_meteo$comIdState==csIds_list[[1]][i]] <- csIds_list[[2]][i]
  
}
## Check, whether adding names to comIdState worked
table(yieldData_meteo$comState=="Lower Saxony")
table(yieldData_meteo$comIdState=="03")
# looks good
unique(yieldData_meteo$comState)
unique(yieldData_meteo$comIdState)
head(yieldData_meteo, 15)
tail(yieldData_meteo, 15)
# View(yieldData)


###############################################################
## Löschen der Reduntanten Variablen test, SP_ID, USE, GEN ###
yieldData_meteo$test <- yieldData_meteo$SP_ID <- yieldData_meteo$USE <- yieldData_meteo$GEN <- NULL


##################
## Reorder data ##
length(yieldData_meteo)
names(yieldData_meteo[, c(1:3,82,83, 4:81 )])
x <- yieldData_meteo[, c(1:3,82,83, 4:81 )]
head(x)
yieldData_meteo <- x

##################################################
#### Schreiben des yieldData Datensatzes #### 
head(yieldData_meteo)
table(yieldData_meteo$year)
# write.table(yieldData,"./data/data_processed/yieldData", row.names= FALSE)
write.csv(yieldData_meteo,"./data/data_processed/yieldData_meteo", row.names= FALSE )




# #############################################################################################################################################################################################
# #################################################################  For further analysis only use data starting 1999 #########################################################################
# #############################################################################################################################################################################################
# Yield_SMI_arr <- read.csv("data/data_processed/Yield_SMI_arr.csv")
# 
# 
# 
# ## Delete years 1995 - 1998 ##
# sapply(Yield_SMI_arr, class)
# Yield_SMI<-Yield_SMI_arr[Yield_SMI_arr$year >= 1999,] 
# 
# 
# ##################################################
# #### Schreiben des Yield_SMI Datensatzes #### 
# write.csv(Yield_SMI,"data/data_processed/Yield_SMI", row.names= FALSE )
# 
# 
# 
# # ###################################################################################################################################################################################################
# # ############################################################################################## Detrended Z-score ##################################################################################
# # ###################################################################################################################################################################################################
# # 
# # ######################
# # ####  winterWheat ####
# # 'First I consider winterWheat as a training set on which I do the calculations'
# # 
# # WW_SMI_1999 <- read.csv("data/data_processed/Yield_SMI")
# # names(WW_SMI_1999)
# # head(WW_SMI_1999)
# # dim(WW_SMI_1999)
# # 
# # ## Hier berechne ich den mean von Winterwheat der einzelnen Kreise über die Jahre 1999-2011
# # WW_SMI_1999<-ddply(WW_SMI_1999, .(comId), transform, meanWW_Kreise = mean(winterWheat) )
# # length(WW_SMI_1999$meanWW_Kreise) ## newly created vector has the appropriate length
# # 
# # ## Hier berechne ich die Standardabweichung von Winterwheat der einzelenn Kreise 
# # WW_SMI_1999<-ddply(WW_SMI_1999, .(comId), transform, sdWW_Kreise= sd(winterWheat) )
# # length(WW_SMI_1999$sdWW_Kreise)
# # 
# # ## Hie berechne ich den standard score der winterWheat Daten
# # WW_SMI_1999 <- transform(WW_SMI_1999, scoreWW_Kreise = (winterWheat - meanWW_Kreise)/sdWW_Kreise)
# # length(WW_SMI_1999$scoreWW_Kreise)
# # 
# # ## Detrend WinterWheat z-score Daten der einzelnen Kreise
# # WW_SMI_1999<-ddply(WW_SMI_1999, .(comId), transform, detrendScoreWW_Kreise= detrend(scoreWW_Kreise, "linear") )
# # length(WW_SMI_1999$detrendScoreWW_Kreise)
# 
# # ##############################################################
# # ## Compare the scoreWW_Kreise and the detrendScoreWW_Kreise ##
# # par(mfrow=c(2,1))
# # plot(WW_SMI_1999$scoreWW_Kreise~ WW_SMI_1999$year)
# # fit <- lm(WW_SMI_1999$scoreWW_Kreise~ WW_SMI_1999$year)
# # abline(fit,col=2,lwd=2)
# # abline(a=0, b=0)
# # plot(WW_SMI_1999$detrendScoreWW_Kreise~ WW_SMI_1999$year)
# # fit2 <- lm(WW_SMI_1999$detrendScoreWW_Kreise~ WW_SMI_1999$year)
# # abline(fit2,col=2,lwd=2)
# # abline(a=0, b=0)
# # summary(WW_SMI_1999$detrendScoreWW_Kreise)
# # ## Interpretation ##
# # '
# #   Looks like there is not trend left after detrending the data
# # '
# 
# # ##############################
# # ## Check out new data.frame ##
# # head(WW_SMI_1999, 15) # Order: First all years in comID 1001, then all year in comId 1002, and so on
# # tail(WW_SMI_1999, 15)
# # names(WW_SMI_1999)
# # dim(WW_SMI_1999)
# # ## Interpretation
# # '
# # Looks like the data are okay.
# # '
# 
# #############################################################################################################################################################################################
# ############################################################ Detrending and standardization for all yield Data ##############################################################################
# #############################################################################################################################################################################################
# 'Now I am doing the same procedure for each crop'
# 
# ## Load data.frame ##
# Yield_SMI <- read.csv("data/data_processed/Yield_SMI")
# head(Yield_SMI) # order: First all years of comID 1001, then all of comId 1002, and so on
# 
# #################################################################################
# ## Calculate mean of crops for the communities with help of ddply and colwise  ##
# #################################################################################
# mean<-ddply(Yield_SMI , .(comId), colwise(mean, c("winterWheat", "rye", "winterBarley" , "summerBarley",  "oats" , "triticale", "potatoes" , "sugarBeet", "winterRape", "siloMaize"  )))
# 
# names(mean)<-c("comId", "winterWheat_mean", "rye_mean", "winterBarley_mean" , "summerBarley_mean",  "oats_mean" , "triticale_mean", "potatoes_mean" , "sugarBeet_mean", "winterRape_mean", "siloMaize_mean"  )
# 
# head(mean,  20) 
# dim(mean) # only 410 observation per attribute, because mean aggregates data over the years
# 
# #####################################################################################################################
# ## Because data are now aggregated across the years, I need to repeat new data.frame 12 times and append it rowise ##
# mean2<-rbind(mean, mean, mean, mean, mean, mean, mean, mean, mean, mean, mean, mean)
# head(mean2)
# tail(mean2)
# 
# ########################################################################################
# ## Reorder data.frame for combining it with the year column gained from the Yield_SMI ##
# mean2<-mean2[order(mean2$comId),] # First, all years of comId 1001, that all years of comId 1002, and so on
# head(mean2, 25)
# dim(mean2)
# ## Interpretation
# 'This is the same order now as the Yield_SMI and WW_SMI_1999 data.frames'
# 
# ###############################################################################
# ## Cbind years from Yield_SMI with mean2 data.frame to get the time dimension ##
# head(Yield_SMI , 25) ## First, all years of comId 1001, than all years of comId 1002, and so on
# 
# ## Extract year vector ##
# year<-Yield_SMI$year
# head(year,24)
# tail(year, 24)
# dim(as.data.frame(year))
# 
# #####################################################
# ## Cbind the year vector with the mean2-data.frame ##
# mean3<-cbind(year, mean2)
# dim(mean3)
# head(mean3)
# 
# ########################################################################
# ## Compare created mean3$winterWheat_mean with WW_SMI_1999$meanWW_Kreise
# all(mean3$winterWheat_mean%in%WW_SMI_1999$meanWW_Kreise)
# # Intepretation: both vectors are the same
# 
# ##################################
# ## Calculate standard deviation ##
# ##################################
# sd<-ddply(Yield_SMI , .(comId), colwise(sd, c("winterWheat", "rye", "winterBarley" , "summerBarley",  "oats" , "triticale", "potatoes" , "sugarBeet", "winterRape", "siloMaize"  )))
# head(sd,  20)
# tail(sd)
# names(sd)<-c("comId", "winterWheat_sd", "rye_sd", "winterBarley_sd" , "summerBarley_sd",  "oats_sd" , "triticale_sd", "potatoes_sd" , "sugarBeet_sd", "winterRape_sd", "siloMaize_sd"  )
# dim(sd)
# 
# #####################################################################################################################
# ## Because data are now aggregated across the years, I need to repeat new data.frame 12 times and append it rowise ## 
# sd2<-rbind(sd, sd, sd, sd, sd, sd, sd, sd, sd, sd, sd, sd)
# head(sd2)
# tail(sd2)
# 
# #######################################################################
# ## Bring new data.frame in right order and cbind it with year vector ##
# sd2<-sd2[order(sd2$comId),]
# head(sd2)
# dim(sd2)
# sd3 <- cbind(year, sd2 )
# dim(sd3)
# head(sd3)
# 
# 
# #######################
# ## Calculate z-score ##
# #######################
# 
# ############################################
# ## Extract matrix of original crop values ##
# head(Yield_SMI )
# values<-Yield_SMI [c( "year","comId","winterWheat", "rye", "winterBarley" , "summerBarley",  "oats" , "triticale", "potatoes" , "sugarBeet", "winterRape", "siloMaize"  )]
# head(values)
# dim(values)
# 
# #################################################################
# ## do some simple matrix calculations to calculate the z-score ##
# dim(mean3)%in%dim(sd3)
# dim(sd3)%in%dim(values)
# 
# score = (values -mean3)/sd3 
# names(score)<-c("year","comId", "winterWheat_zscore", "rye_zscore", "winterBarley_zscore" , "summerBarley_zscore",  "oats_zscore" , "triticale_zscore", "potatoes_zscore" , "sugarBeet_zscore", "winterRape_zscore", "siloMaize_zscore"  )
# 
# dim(score)
# head(score)
# 
# ##############################################################################
# ## Compare newlycreate score of winterWheat with training winterWheat score ## 
# all(Yield_SMI $scoreWW_Kreise%in%score$winterWheat_zscore)
# ## Interpretation
# '
# Procedure was succesfull, both scores are the same
# '
# 
# ###################################
# ## Cbind z-score with data.frame ##
# 
# score$year<-NULL
# score$comId<- NULL
# 
# head(score)
# head(Yield_SMI)
# 
# scoreAll_1999<-cbind(Yield_SMI, score)
# dim(scoreAll_1999)
# head(scoreAll_1999, 13)
# names(scoreAll_1999)
# 
# ##################################
# ## Detrend z-score of all crops ##
# ##################################
# 
# detrend_zscore<-ddply(scoreAll_1999, .(comId), colwise(detrend, c("winterWheat_zscore", "rye_zscore", "winterBarley_zscore" , "summerBarley_zscore",  "oats_zscore" , "triticale_zscore", "potatoes_zscore" , "sugarBeet_zscore", "winterRape_zscore", "siloMaize_zscore"  )))
# dim(detrend_zscore)
# head(detrend_zscore)
# names(detrend_zscore)<-c("comId", "winterWheat_deZscore", "rye_deZscore", "winterBarley_deZscore" , "summerBarley_deZscore",  "oats_deZscore" , "triticale_deZscore", "potatoes_deZscore" , "sugarBeet_deZscore", "winterRape_deZscore", "siloMaize_deZscore"  )
# 
# #############################
# ## Cbind detrended z-score ##
# names(detrend_zscore)
# detrend_zscore$comId<-NULL
# 
# scoreAllDe_1999<-cbind(Yield_SMI, detrend_zscore)
# 
# dim(scoreAllDe_1999)
# head(scoreAllDe_1999)
# 
# 
# #############################################################################
# ## Compare results of procedure with only wheat and procedure of all crops ##
# all(WW_SMI_1999$detrendScoreWW_Kreise%in%scoreAllDe_1999$winterWheat_deZscore)
# ## Intepretation
# 'Results are the same!'
# 
# ##########################################################
# ## Compare not detrended z-score with detrended z-score ##
# par(mfrow=c(2,1))
# plot(scoreAll_1999$winterBarley_zscore~scoreAllDe_1999$year)
# fit <- lm(scoreAll_1999$winterBarley_zscore ~ scoreAllDe_1999$year)
# abline(fit,col=2,lwd=2)
# abline(0,0)
# plot(scoreAllDe_1999$winterBarley_deZscore~scoreAllDe_1999$year)
# fit2 <- lm(scoreAllDe_1999$winterBarley_deZscore~scoreAllDe_1999$year)
# abline(fit2,col=2,lwd=2)
# abline(0,0)
# 
# #######################################################
# ## Complete newly created data.frame scoreAllDe_1999 ##
# head(scoreAllDe_1999)
# names(scoreAllDe_1999)
# dim(scoreAllDe_1999)
# scoreAllDe_1999$X.1<-NULL
# scoreAllDe_1999$X <- NULL
# 
# #######################                     
# ## Reorder dataframe ##
# 
# #### Soft gecoded ####
# 
# length(scoreAllDe_1999)
# names(scoreAllDe_1999[c(1:5,16,6:15, 17:43)])
# scoreAllDe_1999 <- scoreAllDe_1999[c(1:5,16,6:15, 17:43)]
# names(scoreAllDe_1999[c(1:16, 34:43,17:33)])
# scoreAllDe_1999 <- scoreAllDe_1999[c(1:16, 34:43,17:33)]
# names(scoreAllDe_1999)
# 
# #####################################################################
# ## Write data.frame with the detrended z-scores of the crop yields ##
# write.csv(scoreAllDe_1999, "data//data_processed/Yield_SMI_detrend")
# 
# 
# #############################################################################################################################################################################################
# ############################################################################# Detrending of absolute crop yield values ######################################################################
# #############################################################################################################################################################################################
# 
# # Detrend WinterWheat Daten der einzelnen Kreise ##
# # Comment: Da detrend mit linear nicht funktioniert, demeane ich die Daten zuerst. Anschließend detrende ich diese dann. 
# # Attention: die detrend funtion scheint nicht wirklich gut für große Daten zu funktionieren. Daher sollte ich es mal per Hand versuchen.
# # Dies ist mir bisher auch nicht gelungen, da es nicht so einfach ist lm innerhalb von ddply zu benutzen
# # Das ist erst relevant, wenn ich den Panelansatz verfolge
# 
# summary(Yield_SMI$winterWheat)
# mean(Yield_SMI$winterWheat, na.rm = T)
# 
# ##########################
# ## Plot absolute values ##
# par(mfrow=c(3,1))
# plot(Yield_SMI$winterWheat~ Yield_SMI $year)
# fit0 <- lm(Yield_SMI$winterWheat~ Yield_SMI $year)
# abline(fit0, col=3)
# abline(mean(Yield_SMI$winterWheat, na.rm= T), 0, col=2)
# # Interpretation
# 'One can see a trend in the data'
# 
# ############################################
# ## Detrend absolute values of winterWheat ##
# Yield_SMI <-ddply(Yield_SMI , .(comId), transform, detrendWW_Kreise = detrend(winterWheat, "linear") )
# summary(Yield_SMI$detrendWW_Kreise) # Das der Mean hier nicht 0 ist, ist ein Zeichen dafür, dass nicht alles Daten durch die detrend funktion 
# plot(Yield_SMI $detrendWW_Kreise ~ Yield_SMI $year)
# fit1 <- lm(Yield_SMI$detrendWW_Kreise ~ Yield_SMI $year)
# abline(fit1)
# abline(0,0, col=2)
# 
# ## Interpretation
# '
# Offensichtlich werden nicht alle Daten detrended. 
# '
# 
# #######################################################
# ## Demean data, i.e. detrend data with constand mean ##
# Yield_SMI <-ddply(Yield_SMI , .(comId), transform, demeanWW_Kreise = detrend(winterWheat, "constant") )
# summary(Yield_SMI $demeanWW_Kreise)
# plot(Yield_SMI $demeanWW_Kreise ~ Yield_SMI $year)
# fit2 <- lm(Yield_SMI$demeanWW_Kreise ~ Yield_SMI $year)
# abline(fit2)
# abline(0,0, col=2)
# ## Interpretation
# '
# Es ist ein leichter Trend zu erkennen
# '
# 
# ###############################
# ## Detrend the demeaned data ##
# Yield_SMI <-ddply(Yield_SMI , .(comId), transform, detrendWW_Kreise= detrend(demeanWW_Kreise, "linear") )
# summary(Yield_SMI $detrendWW_Kreise)
# plot(Yield_SMI $detrendWW_Kreise ~ Yield_SMI $year)#
# fit3 <- lm(Yield_SMI$detrendWW_Kreise ~ Yield_SMI $year)
# abline(fit3)
# abline(0,0, col=2)
# 
# ##################################################
# ## Referenz: detrend unabhängig von den Kreisen ##
# # Comment: Does not work
# Yield_SMI  <- transform(Yield_SMI , demeanWW = detrend(winterWheat, "linear"))
# summary(Yield_SMI$demeanWW)
# plot(Yield_SMI$demeanWW) # no detrending apparent
# fit4 <- lm(Yield_SMI$demeanWW ~ Yield_SMI$year)
# abline(fit4)
# head(Yield_SMI )
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
