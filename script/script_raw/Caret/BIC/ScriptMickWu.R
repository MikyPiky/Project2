############################
####  SiloMaize in Jul ####
############################
'
- Correct Standard Errors with either Driscoll Kray or Cameron et al /Thompson estimator

The --vcovHC– function estimates three heteroskedasticity-consistent covariance estimators:
• "white1" - for general heteroskedasticity but no serial correlation. Recommended for random effects.
• "white2" - is "white1" restricted to a common variance within groups. Recommended for random effects.
• "arellano" - both heteroskedasticity and serial correlation. Recommended for fixed effects.
The following options apply*:
• HC0 - heteroskedasticity consistent. The default.
• HC1,HC2, HC3 – Recommended for small samples. HC3 gives less weight to influential
observations.
• HC4 - small samples with influential observations
• HAC - heteroskedasticity and autocorrelation consistent (type ?vcovHAC for more
details)
Solution for serial correlation: Cluster by groups.
Solution for cross sectional correlation: Cluster by time

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
library(ggplot2)
library(ggthemes)
library(reshape)
library(ggthemes)
library(cowplot)
library(scales)

####################################################################################################################################################################


###################################################
##### Save Yield_Covariates_SM_Jul extern ####
Yield_Covariates_SM_Jul <- read.csv( file="./data/data_raw/Yield_Covariates_SM_Jul.csv")
str(Yield_Covariates_SM_Jul)

##############
## Cleaning ##
rownames(Yield_Covariates_SM_Jul) <- NULL

head(Yield_Covariates_SM_Jul)
Yield_Covariates_SM_Jul <- plm.data(Yield_Covariates_SM_Jul, index=c("comId", "year"))
Yield_Covariates_SM_Jul[,c("comId","stateId")] <- lapply(Yield_Covariates_SM_Jul[,c("comId","stateId")], factor )

## create a matrix which contains all possible degree combinations, here for three variables ##
degree <- permutations(n=3,r=2,v=c(1:3),repeats.allowed=T)
degree



###################################################################################################################################################################
################################################################
## Generate output of Standard Model with highest Polynomials ##


r <- 9
formula_Jul_sm_detrendlog_SMIPrecTavg <- siloMaize_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  poly(Tavg, degree[r, 2], raw = T) + 
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormally dry", "abnormally wet" ,"abundantly wet", "severely wet")) 

## PLM ##
plm.fit_SM_BestStdFull_Jul <- plm(formula_Jul_sm_detrendlog_SMIPrecTavg,  data = Yield_Covariates_SM_Jul,  effect="individual", model=("within"), index = c("comId","year"))
summary(plm.fit_SM_BestStdFull_Jul)

## Correction og Standard errors
cov2_SM_BestStdFull_Jul     <- vcovSCC(plm.fit_SM_BestStdFull_Jul,method = "arellano",type = "HC0")
DK.se_SM_BestStdFull_Jul    <- sqrt(diag(cov2_SM_BestStdFull_Jul ))

# LM ##
lm.fit_SM_BestStdFull_Jul <- lm(update(formula_Jul_sm_detrendlog_SMIPrecTavg , .~. + dummy(comId)),  data = Yield_Covariates_SM_Jul)
summary(lm.fit_SM_BestStdFull_Jul)



##########################
## Table of the Models ##
se_results_Jul <- list(DK.se_SM_BestStdFull_Jul)

stargazer(plm.fit_SM_BestStdFull_Jul,
          se = se_results_Jul,   
          title  = "Results of Jul",
          dep.var.caption = "Dependent Variable: log(Silomaize)",
          #           align=TRUE, 
          model.numbers = FALSE,
          covariate.labels = c("Precipitation$^{1}$","Precipitation$^{2}$", "Precipitation$^{3}$", "AverageTemperature$^{1}$", "AverageTemperature$^{2}$",
                               "AverageTemperature$^{3}$"    ,  
                               "SMI:severe drought", "SMI:moderate drought",  "SMI:abnormal dry", "SMI:abnormal wet", 
                               "SMI:abundant wet", "SMI:severe wet" ),
#           type="text", out="./figures/figures_exploratory/BIC/Silomaize/Jul/SM_BestStandard_Jul.txt",
                   out="./figures/figures_exploratory/BIC/Silomaize/Jul/SM_BestStandard_Jul",
          no.space=TRUE ,
          df = FALSE)

################################################
#### Plotting the results of all the modell ####
################################################

df1 <- data.frame(x = seq(-2.575829,2.575829,0.0001))

ylim_min = -0.4
ylim_max = 0.1
rect_xmas = 2.575829

######################################################################
#### Results for Best_Standard Model with polynomials of degree 3 ####

################# 
# Precipitation #
Std_Prec1=function(x){0.004*x - 0.023*x^2 + 0.004*x^3}
Std_Prec2=function(x){0.036*x - 0.014*x^2 + 0.001*x^3}
Std_Prec3=function(x){0.039*x - 0.023*x^2 + 0.005*x^3}
Std_Prec4=function(x){-0.014*x - 0.019*x^2 + 0.004*x^3}
Std_Prec5=function(x){-0.011*x - 0.005*x^2 + 0.002*x^3}
Std_Prec6=function(x){-0.003*x + 0.002*x^2 - 0.0001*x^3}

## Make Predictions for Values similiar to SMI Anomaly Categories
x1 <-  c(2.575829, 1.644854, 1.281552,1.036433, 0.8416212,0.6744898, 0.5244005,-2.575829, -1.644854, -1.281552, -1.036433,- 0.8416212,-0.6744898, -0.5244005 )
round(Std_Prec1(x1), 3)
round(Std_Prec2(x1), 3)
round(Std_Prec3(x1), 3)
round(Std_Prec4(x1), 3)
round(Std_Prec5(x1), 3)
round(Std_Prec6(x1), 3)

Std_Prec  <- ggplot(df1, aes(x)) +
  theme_minimal()  +
  ylim(ylim_min, ylim_max) +
  geom_rect(aes(xmin = 1.281552, xmax = rect_xmas, ymin = ylim_min, ymax = ylim_max), fill="gray70", alpha=0.2) +
  geom_rect(aes(xmin = -1.281552, xmax = -rect_xmas, ymin = ylim_min, ymax = ylim_max), fill="gray70", alpha=0.2) +
  geom_rect(aes(xmin = 0.8416212, xmax =  1.281552, ymin = ylim_min, ymax = ylim_max), fill="gray80", alpha=0.2) +
  geom_rect(aes(xmin = - 0.8416212, xmax =  - 1.281552, ymin = ylim_min, ymax = ylim_max), fill="gray80", alpha=0.2) +
  geom_rect(aes(xmin = 0.5244005 , xmax =  0.8416212, ymin = ylim_min, ymax = ylim_max), fill="gray90", alpha=0.2) +
  geom_rect(aes(xmin = -0.5244005 , xmax =  -0.8416212, ymin = ylim_min, ymax = ylim_max), fill="gray90", alpha=0.2) +
  geom_hline(yintercept = c(0, -0.1, -0.2) ,  colour="gray100", linetype = "solid", size=2) +
  geom_vline(xintercept = c(1,-1,2,-2) ,  colour="gray100", linetype = "solid", size=2) +
  stat_function(fun = Std_Prec1,    aes(colour="1"), size=4, linetype="dashed")  +
  stat_function(fun = Std_Prec2 ,  aes(colour="2"), size=4) +
  stat_function(fun = Std_Prec3 ,  aes(colour="3"), size=4)  +
  stat_function(fun = Std_Prec4 ,  aes(colour="4"), size=4, linetype="dashed")  +
  stat_function(fun = Std_Prec5,    aes(colour="5"), size=0)  +
  stat_function(fun = Std_Prec6,    aes(colour="6"), size=0)  +  
  theme(axis.text.x = element_text(angle=00, vjust=0, size=30),
        axis.text.y = element_text(angle=0, vjust=0, size=30),
        axis.title.y = element_text(angle=90, size=30),
        axis.title.x = element_text(size=30),
        strip.text.x = element_text(size=30),
        legend.text = element_text(angle=0, vjust=0, size=40),
        legend.title = element_text(angle=0, vjust=0, size=40), 
        legend.key.size = unit(2.5, 'lines')) +
  #   geom_vline(xintercept = 0,  colour="grey", linetype = "solid", size=2) +
  #   geom_hline(yintercept = 0,  colour="grey", linetype = "solid", size=2) +
  ylab("Change of Silage Maize") + 
  xlab("Precipitation Standard Deviation") +
  scale_y_continuous(labels=percent) +
  #   xlab("Standard Deviations") +
  scale_colour_manual("Months", values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a","#1f78b4","#e6ab02"), 
                      labels=c("May","June","July","Aug.", "Sept.", "Oct.")) 


# ggsave("Std_Prec.png", plot = Std_Prec, device = "png", path = "./figures/figures_exploratory/BIC/Silomaize/",
#        width = 8, height = 8 ,      dpi = 300)

#################
## Temperature ##
Std_Tavg1 <- function(x){0.024*x - 0.005*x^2 + 0.0004*x^3}
Std_Tavg2 <- function(x){-0.006*x - 0.006*x^2 - 0.002*x^3}
Std_Tavg3 <- function(x){-0.036*x - 0.007*x^2 + 0.004*x^3}
Std_Tavg4 <- function(x){-0.003*x - 0.008*x^2 - 0.002*x^3}
Std_Tavg5 <- function(x){0.038*x - 0.009*x^2 - 0.013*x^3}
Std_Tavg6 <- function(x){-0.002*x - 0.016*x^2 + 0.005*x^3}

round(Std_Tavg1(x1), 3)
round(Std_Tavg2(x1), 3)
round(Std_Tavg3(x1), 3)
round(Std_Tavg4(x1), 3)
round(Std_Tavg5(x1), 3)
round(Std_Tavg6(x1), 3)

Std_Tavg  <- ggplot(df1, aes(x)) +
  theme_minimal() +
  ylim(ylim_min, ylim_max) +
  geom_rect(aes(xmin = 1.281552, xmax = rect_xmas, ymin = ylim_min, ymax = ylim_max), fill="gray70", alpha=0.2) +
  geom_rect(aes(xmin = -1.281552, xmax = -rect_xmas, ymin = ylim_min, ymax = ylim_max), fill="gray70", alpha=0.2) +
  geom_rect(aes(xmin = 0.8416212, xmax =  1.281552, ymin = ylim_min, ymax = ylim_max), fill="gray80", alpha=0.2) +
  geom_rect(aes(xmin = - 0.8416212, xmax =  - 1.281552, ymin = ylim_min, ymax = ylim_max), fill="gray80", alpha=0.2) +
  geom_rect(aes(xmin = 0.5244005 , xmax =  0.8416212, ymin = ylim_min, ymax = ylim_max), fill="gray90", alpha=0.2) +
  geom_rect(aes(xmin = -0.5244005 , xmax =  -0.8416212, ymin = ylim_min, ymax = ylim_max), fill="gray90", alpha=0.2) +
  geom_hline(yintercept = c(0, -0.1, -0.2) ,  colour="gray100", linetype = "solid", size=2) +
  geom_vline(xintercept = c(1,-1,2,-2) ,  colour="gray100", linetype = "solid", size=2) +
  stat_function(fun = Std_Tavg1,    aes(colour="1"), size=0)  +
  stat_function(fun = Std_Tavg2 ,  aes(colour="2"), size=0) +
  stat_function(fun = Std_Tavg3 ,  aes(colour="3"), size=4)  +
  stat_function(fun = Std_Tavg4 ,  aes(colour="4"), size=4, linetype="dashed")  +
  stat_function(fun = Std_Tavg5,    aes(colour="5"), size=4, linetype="dashed")  +
  stat_function(fun = Std_Tavg6,    aes(colour="6"), size=4, linetype="dashed")  +  
  theme(axis.text.x = element_text(angle=00, vjust=0, size=30),
        axis.text.y = element_text(angle=0, vjust=0, size=30),
        axis.title.y = element_text(angle=90, size=30),
        axis.title.x = element_text(size=30),
        strip.text.x = element_text(size=30),
        legend.text = element_text(angle=0, vjust=0, size=40),
        legend.title = element_text(angle=0, vjust=0, size=40), 
        legend.key.size = unit(2.5, 'lines')) +
  #   geom_vline(xintercept = 0,  colour="grey", linetype = "solid", size=2) +
  #   geom_hline(yintercept = 0,  colour="grey", linetype = "solid", size=2) +
  ylab("Change of Silage Maize") + 
  xlab("Temperature Standard Deviation") +
  scale_y_continuous(labels=percent) +
  #   xlab("Standard Deviations") +
  scale_colour_manual("Months", values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a","#1f78b4","#e6ab02"), 
                      labels=c("May","June","July","Aug.", "Sept.", "Oct.")) 

Std_Tavg

