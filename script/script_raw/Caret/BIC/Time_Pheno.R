Klimadaten_agri_annual <- read.csv("~/Downloads/PH_Jahresmelder_Landwirtschaft_Kulturpflanze_Mais_1951_2014_hist.txt", sep=";", dec=",")
Klimadaten_agri_imit <- read.csv("~/Downloads/PH_Sofortmelder_Landwirtschaft_Kulturpflanze_Mais_ohne_Sortenangabe_1979_2014_hist.txt", sep=";", dec=",")

## Merge both data.frames
Klimadaten_agri <- merge(Klimadaten_agri_annual,Klimadaten_agri_imit, all=T )

dim(Klimadaten_agri)
dim(Klimadaten_agri_annual)
dim(Klimadaten_agri_imit)
dim(Klimadaten_agri_annual)[1]+ dim(Klimadaten_agri_imit)[1]


head(Klimadaten_agri, 15)
head(Klimadaten_agri_annual, 15)
head(Klimadaten_agri_imit, 15)

str(Klimadaten_agri )

names(Klimadaten_agri)
names_sub <- c("Stations_id","Referenzjahr","Phase_id", "Eintrittsdatum" )
Klimadaten_agri_sub1 <- Klimadaten_agri[names_sub]

remove(Klimadaten_agri_annual)
remove(Klimadaten_agri_imit)
remove(Klimadaten_agri)

str(Klimadaten_agri_sub1)

library(plyr)
library(stringr)

names(Klimadaten_agri_sub1)

Eintrittsdatum <- str_sub(Klimadaten_agri_sub1$Eintrittsdatum, 6,8)
str(Eintrittsdatum)

Klimadaten_agri_sub1$Eintrittsdatum <- as.numeric(Eintrittsdatum)

# Nur Daten ab 1999
Klimadaten_agri_sub2 <- subset(Klimadaten_agri_sub1, Referenzjahr > 1998)
head(Klimadaten_agri_sub2)
tail(Klimadaten_agri_sub2)
table(Klimadaten_agri_sub2$Phase_id)
table(Klimadaten_agri_sub2$Referenzjahr)

# Blüte
Klimadaten_agri_sub5 <- subset(Klimadaten_agri_sub2, Phase_id == c("5")) # Blüte 
Klimadaten_agri_sub5 <- subset(Klimadaten_agri_sub5, Eintrittsdatum < "900") # Blüte im July
Klimadaten_agri_sub5 <- subset(Klimadaten_agri_sub5, Eintrittsdatum > "700") # Blüte im July

Klimadaten_agri_sub5_7 <- subset(Klimadaten_agri_sub5, Eintrittsdatum < "800") # Blüte im July
# Klimadaten_agri_sub5_7 <- subset(Klimadaten_agri_sub5_7, Eintrittsdatum > "700") # Blüte im July
Klimadaten_agri_sub5_8 <- subset(Klimadaten_agri_sub5, Eintrittsdatum > "800") # Blüte im august
# Klimadaten_agri_sub5_8 <- subset(Klimadaten_agri_sub5_8, Eintrittsdatum < "900") # Blüte im august

summary(Klimadaten_agri_sub5) # Durschnittlich 733, also Anfang August
summary(Klimadaten_agri_sub5_7)
summary(Klimadaten_agri_sub5_8)
par(mfrow = c(1,2))
hist(Klimadaten_agri_sub5_7$Eintrittsdatum)
hist(Klimadaten_agri_sub5_8$Eintrittsdatum)
hist(Klimadaten_agri_sub5$Eintrittsdatum)

# Milchreife
Klimadaten_agri_sub19 <- subset(Klimadaten_agri_sub2, Phase_id == c("19")) # Blüte 
# Klimadaten_agri_sub19 <- subset(Klimadaten_agri_sub19, Eintrittsdatum < "1000") # Blüte im July
Klimadaten_agri_sub19 <- subset(Klimadaten_agri_sub19, Eintrittsdatum > 800) # Blüte im July

Klimadaten_agri_sub19_8 <- subset(Klimadaten_agri_sub19, Eintrittsdatum < 900) # Blüte im July
# Klimadaten_agri_sub19_7 <- subset(Klimadaten_agri_sub19_7, Eintrittsdatum > "700") # Blüte im July
Klimadaten_agri_sub19_9 <- subset(Klimadaten_agri_sub19, Eintrittsdatum > 900) # Blüte im august
# Klimadaten_agri_sub19_8 <- subset(Klimadaten_agri_sub19_8, Eintrittsdatum < "900") # Blüte im august

summary(Klimadaten_agri_sub19) # Durschnittlich 733, also Anfang August
summary(Klimadaten_agri_sub19_8)
summary(Klimadaten_agri_sub19_9)
par(mfrow = c(1,2))
hist(Klimadaten_agri_sub19_8$Eintrittsdatum)
hist(Klimadaten_agri_sub19_9$Eintrittsdatum)
hist(Klimadaten_agri_sub19$Eintrittsdatum)

# Teigreife
Klimadaten_agri_sub20 <- subset(Klimadaten_agri_sub2, Phase_id == c("20")) # Teigreife 
# Klimadaten_agri_sub20 <- subset(Klimadaten_agri_sub20, Eintrittsdatum < "1000") # Blüte im July
Klimadaten_agri_sub20 <- subset(Klimadaten_agri_sub20, Eintrittsdatum > 800) # Blüte im July

Klimadaten_agri_sub20_8 <- subset(Klimadaten_agri_sub20, Eintrittsdatum < 900) # Blüte im July
# Klimadaten_agri_sub20_7 <- subset(Klimadaten_agri_sub20_7, Eintrittsdatum > "700") # Blüte im July
Klimadaten_agri_sub20_9 <- subset(Klimadaten_agri_sub20, Eintrittsdatum > 900) # Blüte im august
# Klimadaten_agri_sub20_8 <- subset(Klimadaten_agri_sub20_8, Eintrittsdatum < "900") # Blüte im august

summary(Klimadaten_agri_sub20) # Durschnittlich 733, also Anfang August
summary(Klimadaten_agri_sub20_8)
summary(Klimadaten_agri_sub20_9)
par(mfrow = c(1,2))
hist(Klimadaten_agri_sub20_8$Eintrittsdatum)
hist(Klimadaten_agri_sub20_9$Eintrittsdatum)
hist(Klimadaten_agri_sub20$Eintrittsdatum)




# Klimadaten_agri_sub6 <- subset(Klimadaten_agri_sub2, Phase_id == c("6")) # Vollblüte

# Beginn Bestellung
Klimadaten_agri_sub10 <- subset(Klimadaten_agri_sub2, Phase_id == c("10")) # stellung Beginn 
summary(Klimadaten_agri_sub10)

Klimadaten_agri_sub10 <- subset(Klimadaten_agri_sub10, Eintrittsdatum > "400") # Bestellung zwishen April und May
Klimadaten_agri_sub10 <- subset(Klimadaten_agri_sub10, Eintrittsdatum < "600") # Bestellung zwishen April und May
Klimadaten_agri_sub10_4 <- subset(Klimadaten_agri_sub10, Eintrittsdatum < "500") # Bestellung im April
Klimadaten_agri_sub10_5 <- subset(Klimadaten_agri_sub10, Eintrittsdatum > "500") # Bestellung im May

summary(Klimadaten_agri_sub10_4)
summary(Klimadaten_agri_sub10_5)

hist(Klimadaten_agri_sub10_4$Eintrittsdatum)
hist(Klimadaten_agri_sub10_5$Eintrittsdatum)

# Beginn Ernte
Klimadaten_agri_sub24 <- subset(Klimadaten_agri_sub2, Phase_id == c("24")) # Ernte)
summary(Klimadaten_agri_sub24)
str(Klimadaten_agri_sub24)

Klimadaten_agri_sub24 <- subset(Klimadaten_agri_sub24, Eintrittsdatum > 800) # Ernte zwischen August und September
any(Klimadaten_agri_sub24$Eintrittsdatum==9)
Klimadaten_agri_sub24_8 <- subset(Klimadaten_agri_sub24, Eintrittsdatum < 900) # Ernte im August
Klimadaten_agri_sub24_9 <- subset(Klimadaten_agri_sub24, Eintrittsdatum > 900) # Ernte im October

summary(Klimadaten_agri_sub24_8)
summary(Klimadaten_agri_sub24_9)

par(mfrow=c(1,2))
hist(Klimadaten_agri_sub24_8$Eintrittsdatum)
hist(Klimadaten_agri_sub24_9$Eintrittsdatum)

' Ich denke, ich sollte October rausnehmen'

	5	Blüte Beginn                            
	6	Vollblüte                               
	10	Bestellung Beginn                       
	12	Auflaufen Beginn                        
 	19	Milchreife Beginn                       
	20	Teigreife Beginn                        
	21	Gelbreife Beginn                        
	24	Ernte                                   
	65	Fahnenschieben Beginn                   
	67	Längenwachstum Beginn                   



dim(Klimadaten_agri_sub1)
dim(Klimadaten_agri_sub2)
dim(Klimadaten_agri_sub5)
dim(Klimadaten_agri_sub6)
dim(Klimadaten_agri_sub10)
dim(Klimadaten_agri_sub_561024)

dim(Klimadaten_agri_sub5)[1] + dim(Klimadaten_agri_sub6)[1] + dim(Klimadaten_agri_sub10)[1] + dim(Klimadaten_agri_sub24)[1]





str(Klimadaten_agri_sub1)
unique()

Klimadaten_agri_sub3 <-na.omit(Klimadaten_agri_sub2)



head(Klimadaten_agri_sub1)

aggregate(Klimadaten_agri_sub1)

unique(Klimadaten_agri_sub3$Referenzjahr)

# Example
set.seed(1)
d <- data.frame(year = rep(2000:2002, each = 3), count = round(runif(9, 0, 20)))
d
str(d)
ddply(d, "year", function(x) {
   mean.count <- mean(x$count)
#    sd.count <- sd(x$count)
#    cv <- sd.count/mean.count
#    data.frame(cv.count = cv)
   })

ddply(Klimadaten_agri_sub2, "Referenzjahr",    mean =mean(Klimadaten_agri_sub2$Eintrittsdatum) )

unique(Klimadaten_agri_sub3$)