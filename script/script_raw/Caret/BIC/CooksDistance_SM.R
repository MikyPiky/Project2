## Cooks Distance Silage Maize ##

# Description
' This script serves to make statistics and maps of the cooks distance
- First a make a barplot of the number of the Cooks Distance above a certain threshold (4/(n-k-1) conditional on the years.
- Than a make a map of the distribution of Cooks Distance above that threshold)
'

# Libraries
library("stargazer")
library(ggplot2)
library(ggthemes)
library(scales)


###############################################################
## Plot Cooks Distribution with Barplots conditional on year ##
CooksDistribution <- read.csv("./figures/figures_exploratory/BIC/Silomaize/CooksDistribution.csv", sep=",")
head(CooksDistribution)
str(CooksDistribution)
## Only consider relevant rows and columns
CooksDistribution <- CooksDistribution[1:17,1:18]

## Check data.frame
str(CooksDistribution)
names(CooksDistribution)
levels(as.factor(CooksDistribution$Year))
levels(CooksDistribution$Group)

## Transform year variable to factor for use in ggplot
CooksDistribution$Year <- factor(CooksDistribution$Year)
is.na(CooksDistribution)

## Generate basic aestetics for ggplot
g_Cooks<- ggplot(CooksDistribution, aes(x = Year, y=Share, fill=Group))

## Add geom_bars and other information
CooksDistribution_plot <-
g_Cooks + geom_bar(stat = "identity", position="dodge") + theme_few() + scale_fill_grey() + 
  theme(axis.text.x = element_text(angle=90, vjust=0, size=25),
        axis.text.y = element_text(angle=0, vjust=0, size=25),
        axis.title.y =element_text(angle=90, vjust=0, size=25),
        axis.title.x = element_blank(),  
        legend.title=element_blank(),
        legend.text = element_text(size=25))  +    scale_y_continuous(labels=percent) + ylab("% above threshold")
  CooksDistribution_plot
ggsave("CooksDistribution.png", plot = CooksDistribution_plot, device = "png", path = "/Storage/ownCloud/Home/Klimabuero/Proj1/figures/figures_exploratory/BIC/Silomaize/",
       width = 16, height = 8,     dpi = 300)

###############################################################
## Take a look at the spatial distribution of Cooks Distance ##

#########
## May ##

## Read Data ##
## Read in Cooks Distance of May with 2003
Yield_Covariates_May_CD <- read.csv( file = "./figures/figures_exploratory/BIC/Silomaize/May/best_SM_May_cooksDistance.csv")
Yield_Covariates_May_CD$X <- NULL
dim(Yield_Covariates_May_CD) # 3868


## Plot the Distribution of Cooks Distance for May
par(mfrow=c(1,2))
plot(Yield_Covariates_May_CD$comId, Yield_Covariates_May_CD$CooksDistance) # hier scheint es ja schon eine Art röumlich Verteilung zu geben. 

## Read in Table of Distribution of Cooks Distance larger than small cutoff  ##
comId_cooks_SM_May_small <- read.csv(file = "./figures/figures_exploratory/BIC/Silomaize/May/best_SM_May_cooks_comId.csv")
comId_cooks_SM_May_small$X <- NULL
comId_cooks_SM_May_small

plot(comId_cooks_SM_May_small$comId,comId_cooks_SM_May_small$Freq)
comId_cooks_SM_May_small[order(comId_cooks_SM_May_small$Freq),]

## Export both figures as pdf
pdf(".//figures/figures_exploratory/CooksDistance/Silomaize/May.pdf", width=16, height=4)
par(mfrow=c(1,2))
plot(Yield_Covariates_May_CD$comId, Yield_Covariates_May_CD$CooksDistance) 
plot(comId_cooks_SM_May_small$comId,comId_cooks_SM_May_small$Freq)
dev.off()

#########
## Jun ##

## Read Data ##
## Read in Cooks Distance of Jun with 2003
Yield_Covariates_Jun_CD <- read.csv( file = "./figures/figures_exploratory/BIC/Silomaize/Jun/best_SM_Jun_cooksDistance.csv")
Yield_Covariates_Jun_CD$X <- NULL
dim(Yield_Covariates_Jun_CD) # 3868


## Plot the Distribution of Cooks Distance for Jun
par(mfrow=c(1,2))
plot(Yield_Covariates_Jun_CD$comId, Yield_Covariates_Jun_CD$CooksDistance) # hier scheint es ja schon eine Art röumlich Verteilung zu geben. 

## Read in Table of Distribution of Cooks Distance larger than small cutoff  ##
comId_cooks_SM_Jun_small <- read.csv(file = "./figures/figures_exploratory/BIC/Silomaize/Jun/best_SM_Jun_cooks_comId.csv")
comId_cooks_SM_Jun_small$X <- NULL
comId_cooks_SM_Jun_small

plot(comId_cooks_SM_Jun_small$comId,comId_cooks_SM_Jun_small$Freq)
comId_cooks_SM_Jun_small[order(comId_cooks_SM_Jun_small$Freq),]

## Export both figures as pdf
pdf(".//figures/figures_exploratory/CooksDistance/Silomaize/Jun.pdf", width=16, height=4)
par(mfrow=c(1,2))
plot(Yield_Covariates_Jun_CD$comId, Yield_Covariates_Jun_CD$CooksDistance) 
plot(comId_cooks_SM_Jun_small$comId,comId_cooks_SM_Jun_small$Freq)
dev.off()


#########
## Jul ##

## Read Data ##
## Read in Cooks Distance of Jul with 2003
Yield_Covariates_Jul_CD <- read.csv( file = "./figures/figures_exploratory/BIC/Silomaize/Jul/best_SM_Jul_cooksDistance.csv")
Yield_Covariates_Jul_CD$X <- NULL
dim(Yield_Covariates_Jul_CD) # 3868


## Plot the Distribution of Cooks Distance for Jul
par(mfrow=c(1,2))
plot(Yield_Covariates_Jul_CD$comId, Yield_Covariates_Jul_CD$CooksDistance) # hier scheint es ja schon eine Art röumlich Verteilung zu geben. 

## Read in Table of Distribution of Cooks Distance larger than small cutoff  ##
comId_cooks_SM_Jul_small <- read.csv(file = "./figures/figures_exploratory/BIC/Silomaize/Jul/best_SM_Jul_cooks_comId.csv")
comId_cooks_SM_Jul_small$X <- NULL
comId_cooks_SM_Jul_small

plot(comId_cooks_SM_Jul_small$comId,comId_cooks_SM_Jul_small$Freq)
comId_cooks_SM_Jul_small[order(comId_cooks_SM_Jul_small$Freq),]

## Export both figures as pdf
pdf(".//figures/figures_exploratory/CooksDistance/Silomaize/Jul.pdf", width=16, height=4)
par(mfrow=c(1,2))
plot(Yield_Covariates_Jul_CD$comId, Yield_Covariates_Jul_CD$CooksDistance) 
plot(comId_cooks_SM_Jul_small$comId,comId_cooks_SM_Jul_small$Freq)
dev.off()

#########
## Aug ##

## Read Data ##
## Read in Cooks Distance of Aug with 2003
Yield_Covariates_Aug_CD <- read.csv( file = "./figures/figures_exploratory/BIC/Silomaize/Aug/best_SM_Aug_cooksDistance.csv")
Yield_Covariates_Aug_CD$X <- NULL
dim(Yield_Covariates_Aug_CD) # 3868


## Plot the Distribution of Cooks Distance for Aug
par(mfrow=c(1,2))
plot(Yield_Covariates_Aug_CD$comId, Yield_Covariates_Aug_CD$CooksDistance) # hier scheint es ja schon eine Art röumlich Verteilung zu geben. 

## Read in Table of Distribution of Cooks Distance larger than small cutoff  ##
comId_cooks_SM_Aug_small <- read.csv(file = "./figures/figures_exploratory/BIC/Silomaize/Aug/best_SM_Aug_cooks_comId.csv")
comId_cooks_SM_Aug_small$X <- NULL
comId_cooks_SM_Aug_small

plot(comId_cooks_SM_Aug_small$comId,comId_cooks_SM_Aug_small$Freq)
comId_cooks_SM_Aug_small[order(comId_cooks_SM_Aug_small$Freq),]

## Export both figures as pdf
pdf(".//figures/figures_exploratory/CooksDistance/Silomaize/Aug.pdf", width=16, height=4)
par(mfrow=c(1,2))
plot(Yield_Covariates_Aug_CD$comId, Yield_Covariates_Aug_CD$CooksDistance) 
plot(comId_cooks_SM_Aug_small$comId,comId_cooks_SM_Aug_small$Freq)
dev.off()

#########
## Sep ##

## Read Data ##
## Read in Cooks Distance of Sep with 2003
Yield_Covariates_Sep_CD <- read.csv( file = "./figures/figures_exploratory/BIC/Silomaize/Sep/best_SM_Sep_cooksDistance.csv")
Yield_Covariates_Sep_CD$X <- NULL
dim(Yield_Covariates_Sep_CD) # 3868


## Plot the Distribution of Cooks Distance for Sep
par(mfrow=c(1,2))
plot(Yield_Covariates_Sep_CD$comId, Yield_Covariates_Sep_CD$CooksDistance) # hier scheint es ja schon eine Art röumlich Verteilung zu geben. 

## Read in Table of Distribution of Cooks Distance larger than small cutoff  ##
comId_cooks_SM_Sep_small <- read.csv(file = "./figures/figures_exploratory/BIC/Silomaize/Sep/best_SM_Sep_cooks_comId.csv")
comId_cooks_SM_Sep_small$X <- NULL
comId_cooks_SM_Sep_small

plot(comId_cooks_SM_Sep_small$comId,comId_cooks_SM_Sep_small$Freq)
comId_cooks_SM_Sep_small[order(comId_cooks_SM_Sep_small$Freq),]

## Export both figures as pdf
pdf(".//figures/figures_exploratory/CooksDistance/Silomaize/Sep.pdf", width=16, height=4)
par(mfrow=c(1,2))
plot(Yield_Covariates_Sep_CD$comId, Yield_Covariates_Sep_CD$CooksDistance) 
plot(comId_cooks_SM_Sep_small$comId,comId_cooks_SM_Sep_small$Freq)
dev.off()

#########
## Oct ##

## Read Data ##
## Read in Cooks Distance of Oct with 2003
Yield_Covariates_Oct_CD <- read.csv( file = "./figures/figures_exploratory/BIC/Silomaize/Oct/best_SM_Oct_cooksDistance.csv")
Yield_Covariates_Oct_CD$X <- NULL
dim(Yield_Covariates_Oct_CD) # 3868


## Plot the Distribution of Cooks Distance for Oct
par(mfrow=c(1,2))
plot(Yield_Covariates_Oct_CD$comId, Yield_Covariates_Oct_CD$CooksDistance) # hier scheint es ja schon eine Art röumlich Verteilung zu geben. 

## Read in Table of Distribution of Cooks Distance larger than small cutoff  ##
comId_cooks_SM_Oct_small <- read.csv(file = "./figures/figures_exploratory/BIC/Silomaize/Oct/best_SM_Oct_cooks_comId.csv")
comId_cooks_SM_Oct_small$X <- NULL
comId_cooks_SM_Oct_small

plot(comId_cooks_SM_Oct_small$comId,comId_cooks_SM_Oct_small$Freq)
comId_cooks_SM_Oct_small[order(comId_cooks_SM_Oct_small$Freq),]

## Export both figures as pdf
pdf(".//figures/figures_exploratory/CooksDistance/Silomaize/Oct.pdf", width=16, height=4)
par(mfrow=c(1,2))
plot(Yield_Covariates_Oct_CD$comId, Yield_Covariates_Oct_CD$CooksDistance) 
plot(comId_cooks_SM_Oct_small$comId,comId_cooks_SM_Oct_small$Freq)
dev.off()

