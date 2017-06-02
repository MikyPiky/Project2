library(ggplot2)
library(reshape)
library(ggthemes)
library(cowplot)
library(scales)
df1 <- data.frame(x = seq(-2.575829,2.575829,0.0001))

ylim_min = -0.25
ylim_max = 0.1
rect_xmas = 2.575829

## Results for Best_BIC Models

# Precipitation
Prec1=function(x){0.019*x-0.005*x^2}
Prec2=function(x){0.036*x - 0.017*x^2 +0.002*x^3}
Prec3=function(x){0.031*x - 0.014*x^2}
Prec4=function(x){-0.012*x}
Prec5=function(x){0}
Prec6=function(x){0}

x1 <-  c(2.575829, 1.644854, 1.281552,1.036433, 0.8416212,0.6744898, 0.5244005,-2.575829, -1.644854, -1.281552, -1.036433,- 0.8416212,-0.6744898, -0.5244005 )
round(Prec1(x1), 3)
round(Prec2(x1), 3)
round(Prec3(x1), 3)
round(Prec4(x1), 3)
round(Prec5(x1), 3)
round(Prec6(x1), 3)


Prec  <- ggplot(df1, aes(x)) +
  theme_minimal()  +
  ylim(ylim_min, ylim_max) +
  geom_rect(aes(xmin = 1.281552, xmax = rect_xmas, ymin = ylim_min, ymax = ylim_max), fill="gray70", alpha=0.2) +
  geom_rect(aes(xmin = -1.281552, xmax = -rect_xmas, ymin = ylim_min, ymax = ylim_max), fill="gray70", alpha=0.2) +
  geom_rect(aes(xmin = 0.8416212, xmax =  1.281552, ymin = ylim_min, ymax = ylim_max), fill="gray80", alpha=0.2) +
  geom_rect(aes(xmin = - 0.8416212, xmax =  - 1.281552, ymin = ylim_min, ymax = ylim_max), fill="gray80", alpha=0.2) +
  geom_rect(aes(xmin = 0.5244005 , xmax =  0.8416212, ymin = ylim_min, ymax = ylim_max), fill="gray90", alpha=0.2) +
  geom_rect(aes(xmin = -0.5244005 , xmax =  -0.8416212, ymin = ylim_min, ymax = ylim_max), fill="gray90", alpha=0.2) +
  geom_hline(yintercept = 0,  colour="grey", linetype = "solid", size=1) +
  stat_function(fun = Prec1,    aes(colour="1"), size=2)  +
  stat_function(fun = Prec2 ,  aes(colour="2"), size=2) +
  stat_function(fun = Prec3 ,  aes(colour="3"), size=2)  +
  stat_function(fun = Prec4 ,  aes(colour="4"), size=2)  +
  stat_function(fun = Prec5,    aes(colour="5"), size=0)  +
  stat_function(fun = Prec6,    aes(colour="6"), size=0)  +  
  theme(axis.text.x = element_text(angle=00, vjust=0, size=30),
        axis.text.y = element_text(angle=0, vjust=0, size=30),
        axis.title.y = element_text(angle=90, size=30),
        axis.title.x = element_text(size=30),
        strip.text.x = element_text(size=30),
        legend.text = element_text(angle=0, vjust=0, size=20),
        legend.title = element_text(angle=0, vjust=0, size=30)) +
#   geom_vline(xintercept = 0,  colour="grey", linetype = "solid", size=1) +
#   geom_hline(yintercept = 0,  colour="grey", linetype = "solid", size=1) +
  ylab("Change of Silage Maize") + 
  xlab("Precipitation SD") +
  
#   xlab("Standard Deviations") +
  scale_colour_manual("Months", values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a","#66a61e","#e6ab02"), 
                      labels=c("May","June","July","August", "September", "October")) 

Prec

# PET
PET1 <- function(x){0.033*x}
PET2 <- function(x){-0.007*x - 0.016*x^2 - 0.004*x^3}
PET3 <- function(x){0}
PET4 <- function(x){0}
PET5 <- function(x){0}
PET6 <- function(x){-0.025*x - 0.002*x^2 +0.002*x^3}

round(PET1(x1), 3)
round(PET2(x1), 3)
round(PET3(x1), 3)
round(PET4(x1), 3)
round(PET5(x1), 3)
round(PET6(x1), 3)

PET  <- ggplot(df1, aes(x)) +
  theme_minimal()  +
  ylim(ylim_min, 0.1) +
  geom_rect(aes(xmin = 1.281552, xmax = rect_xmas, ymin = ylim_min, ymax = 0.1), fill="gray70", alpha=0.2) +
  geom_rect(aes(xmin = -1.281552, xmax = -rect_xmas, ymin = ylim_min, ymax = 0.1), fill="gray70", alpha=0.2) +
  geom_rect(aes(xmin = 0.8416212, xmax =  1.281552, ymin = ylim_min, ymax = 0.1), fill="gray80", alpha=0.2) +
  geom_rect(aes(xmin = - 0.8416212, xmax =  - 1.281552, ymin = ylim_min, ymax = 0.1), fill="gray80", alpha=0.2) +
  geom_rect(aes(xmin = 0.5244005 , xmax =  0.8416212, ymin = ylim_min, ymax = 0.1), fill="gray90", alpha=0.2) +
  geom_rect(aes(xmin = -0.5244005 , xmax =  -0.8416212, ymin = ylim_min, ymax = 0.1), fill="gray90", alpha=0.2) +
  geom_hline(yintercept = 0,  colour="grey", linetype = "solid", size=1) +
  stat_function(fun = PET1,    aes(colour="1"), size=2)  +
  stat_function(fun = PET2 ,  aes(colour="2"), size=2) +
  stat_function(fun = PET3 ,  aes(colour="3"), size=0)  +
  stat_function(fun = PET4 ,  aes(colour="4"), size=0)  +
  stat_function(fun = PET5,    aes(colour="5"), size=0)  +
  stat_function(fun = PET6,    aes(colour="6"), size=2)  +  
  theme(axis.text.x = element_text(angle=00, vjust=0, size=30),
        axis.text.y = element_blank(),
        axis.title.y =element_blank(),
        axis.title.x = element_text(size=30),
        strip.text.x = element_text(size=30),
        legend.text = element_text(angle=0, vjust=0, size=20),
        legend.title = element_text(angle=0, vjust=0, size=30)) +
  #   geom_vline(xintercept = 0,  colour="grey", linetype = "solid", size=1) +
  #   geom_hline(yintercept = 0,  colour="grey", linetype = "solid", size=1) +
#   ylab("Change of Silage Maize") + 
  xlab("Potential Evapotranspiration SD") +
  
  #   xlab("Standard Deviations") +
  scale_colour_manual("Months", values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a","#66a61e","#e6ab02"), 
                      labels=c("May","June","July","August", "September", "October")) 

PET

# Tavg
TAVG1 <- function(x){0}
TAVG2 <- function(x){0}
TAVG3 <- function(x){-0.025*x - 0.008*x^2 +0.004*x^3}
TAVG4 <- function(x){-0.027*x - 0.020*x^2 +0.006*x^3}
TAVG5 <- function(x){0}
TAVG6 <- function(x){0}

round(TAVG1(x1), 3)
round(TAVG2(x1), 3)
round(TAVG3(x1), 3)
round(TAVG4(x1), 3)
round(TAVG5(x1), 3)
round(TAVG6(x1), 3)

TAVG  <- ggplot(df1, aes(x)) +
  theme_minimal() +
  ylim(ylim_min, ylim_max) +
  geom_rect(aes(xmin = 1.281552, xmax = rect_xmas, ymin = ylim_min, ymax = ylim_max), fill="gray70", alpha=0.2) +
  geom_rect(aes(xmin = -1.281552, xmax = -rect_xmas, ymin = ylim_min, ymax = ylim_max), fill="gray70", alpha=0.2) +
  geom_rect(aes(xmin = 0.8416212, xmax =  1.281552, ymin = ylim_min, ymax = ylim_max), fill="gray80", alpha=0.2) +
  geom_rect(aes(xmin = - 0.8416212, xmax =  - 1.281552, ymin = ylim_min, ymax = ylim_max), fill="gray80", alpha=0.2) +
  geom_rect(aes(xmin = 0.5244005 , xmax =  0.8416212, ymin = ylim_min, ymax = ylim_max), fill="gray90", alpha=0.2) +
  geom_rect(aes(xmin = -0.5244005 , xmax =  -0.8416212, ymin = ylim_min, ymax = ylim_max), fill="gray90", alpha=0.2) +
  geom_hline(yintercept = 0,  colour="grey", linetype = "solid", size=1) +
  stat_function(fun = TAVG1,    aes(colour="1"), size=0)  +
  stat_function(fun = TAVG2 ,  aes(colour="2"), size=0) +
  stat_function(fun = TAVG3 ,  aes(colour="3"), size=2)  +
  stat_function(fun = TAVG4 ,  aes(colour="4"), size=2)  +
  stat_function(fun = TAVG5,    aes(colour="5"), size=0)  +
  stat_function(fun = TAVG6,    aes(colour="6"), size=0)  +  
  theme(axis.text.x = element_text(angle=00, vjust=0, size=30),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size=30),
        strip.text.x = element_text(size=30),
        legend.text = element_text(angle=0, vjust=0, size=20),
        legend.title = element_text(angle=0, vjust=0, size=30)) +
  #   geom_vline(xintercept = 0,  colour="grey", linetype = "solid", size=1) +
  #   geom_hline(yintercept = 0,  colour="grey", linetype = "solid", size=1) +
#   ylab("Change of Silage Maize") + 
  xlab("Temperature SD") +
  
  #   xlab("Standard Deviations") +
  scale_colour_manual("Months", values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a","#66a61e","#e6ab02"), 
                      labels=c("May","June","July","August", "September", "October")) 

TAVG

grobs <- ggplotGrob(TAVG)$grobs
legend <- grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]]

prow <- plot_grid( Prec + theme(legend.position="none"),
                   PET + theme(legend.position="none"),
                   TAVG + theme(legend.position="none"),
                   align = 'vh',
                   labels = c('a)', 'b)', "c)"), label_size = 30,
                   hjust = -3.8,  vjust= 3.1,
                   nrow = 1
)
prow

p <- plot_grid( prow, legend, rel_widths = c(3, .3))
p


######################################################################
#### Results for Best_Standard Model with polynomials of degree 3 ####
######################################################################
# Precipitation
Std_Prec1=function(x){0.007*x - 0.003*x^2 - 0.001*x^3}
Std_Prec2=function(x){0.042*x - 0.018*x^2 + 0.002*x^3}
Std_Prec3=function(x){0.026*x - 0.017*x^2 + 0.002*x^3}
Std_Prec4=function(x){-0.024*x - 0.017*x^2 +0.005*x^3}
Std_Prec5=function(x){-0.001*x - 0.004*x^2 + 0.001*x^3}
Std_Prec6=function(x){0.003*x - 0.008*x^2 - 0.002*x^3}

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
  geom_hline(yintercept = c(0, -0.1, -0.2) ,  colour="gray100", linetype = "solid", size=1) +
  geom_vline(xintercept = c(1,-1,2,-2) ,  colour="gray100", linetype = "solid", size=1) +
  stat_function(fun = Std_Prec1,    aes(colour="1"), size=1,  linetype="dotted")  +
  stat_function(fun = Std_Prec2 ,  aes(colour="2"), size=4) +
  stat_function(fun = Std_Prec3 ,  aes(colour="3"), size=4)  +
  stat_function(fun = Std_Prec4 ,  aes(colour="4"), size=2,  linetype="dashed")  +
  stat_function(fun = Std_Prec5,    aes(colour="5"), size=1,  linetype="dotted")  +
  stat_function(fun = Std_Prec6,    aes(colour="6"), size=1,  linetype="dotted")  +  
  theme(axis.text.x = element_text(angle=00, vjust=0, size=30),
        axis.text.y = element_text(angle=0, vjust=0, size=30),
        axis.title.y = element_text(angle=90, size=30),
        axis.title.x = element_text(size=30),
        strip.text.x = element_text(size=30),
        legend.text = element_text(angle=0, vjust=0, size=25),
        legend.title = element_text(angle=0, vjust=0, size=30)) +
  #   geom_vline(xintercept = 0,  colour="grey", linetype = "solid", size=1) +
  #   geom_hline(yintercept = 0,  colour="grey", linetype = "solid", size=1) +
  ylab("Change of Silage Maize") + 
  xlab("Precipitation Standard Deviation") +
  scale_y_continuous(labels=percent) +
  #   xlab("Standard Deviations") +
  scale_colour_manual("Months", values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a","#66a61e","#e6ab02"), 
                      labels=c("May","June","July","August", "September", "October")) 

Std_Prec


# Tavg
Std_Tavg1 <- function(x){0.003*x - 0.004*x^2 + 0.003*x^3}
Std_Tavg2 <- function(x){-0.008*x - 0.014*x^2 - 0.001*x^3}
Std_Tavg3 <- function(x){-0.025*x - 0.007*x^2 + 0.004*x^3}
Std_Tavg4 <- function(x){0.006*x - 0.007*x^2 - 0.002*x^3}
Std_Tavg5 <- function(x){0.008*x - 0.014*x^2 - 0.005*x^3}
Std_Tavg6 <- function(x){-0.0003*x - 0.023*x^2 + 0.005*x^3}

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
  geom_hline(yintercept = c(0, -0.1, -0.2) ,  colour="gray100", linetype = "solid", size=1) +
  geom_vline(xintercept = c(1,-1,2,-2) ,  colour="gray100", linetype = "solid", size=1) +
  stat_function(fun = Std_Tavg1,    aes(colour="1"), size=1,  linetype="dotted")  +
  stat_function(fun = Std_Tavg2 ,  aes(colour="2"), size=2,  linetype="dashed") +
  stat_function(fun = Std_Tavg3 ,  aes(colour="3"), size=4)  +
  stat_function(fun = Std_Tavg4 ,  aes(colour="4"), size=2,  linetype="dashed")  +
  stat_function(fun = Std_Tavg5,    aes(colour="5"), size=2,  linetype="dashed"))  +
  stat_function(fun = Std_Tavg6,    aes(colour="6"), size=2,  linetype="dashed"))  +  
 
  theme(axis.text.x = element_text(angle=00, vjust=0, size=30),
        axis.text.y = element_text(angle=0, vjust=0, size=30),
        axis.title.y = element_text(angle=90, size=30),
        axis.title.x = element_text(size=30),
        strip.text.x = element_text(size=30),
        legend.text = element_text(angle=0, vjust=0, size=25),
        legend.title = element_text(angle=0, vjust=0, size=30)) +
  #   geom_vline(xintercept = 0,  colour="grey", linetype = "solid", size=1) +
  #   geom_hline(yintercept = 0,  colour="grey", linetype = "solid", size=1) +
  ylab("Change of Silage Maize") + 
  xlab("Precipitation Standard Deviation") +
  scale_y_continuous(labels=percent) +
  #   xlab("Standard Deviations") +
  scale_colour_manual("Months", values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a","#66a61e","#e6ab02"), 
                      labels=c("May","June","July","August", "September", "October")) 

Std_Tavg

grobs <- ggplotGrob(Std_Tavg)$grobs
legend <- grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]]

prow <- plot_grid( Std_Prec + theme(legend.position="none"),
                   Std_Tavg + theme(legend.position="none"),
                   align = 'vh',
                   labels = c('a)', 'b)'), label_size = 30,
                   hjust = -5.2,  vjust= 3.1,
                   nrow = 1
)
prow

p <- plot_grid( prow, legend, rel_widths = c(3, .3))
p

###############################################################################
#### Results for Best_Standard Model with polynomials of degree 3 - no SMI ####
###############################################################################
# Precipitation
Std_Prec_noSMI1=function(x){0.008*x - 0.002*x^2 - 0.0003*x^3}
Std_Prec_noSMI2=function(x){0.034*x - 0.020*x^2 + 0.002*x^3}
Std_Prec_noSMI3=function(x){0.032*x - 0.017*x^2 + 0.002*x^3}
Std_Prec_noSMI4=function(x){-0.010*x - 0.027*x^2 + 0.007*x^3}
Std_Prec_noSMI5=function(x){0.012*x - 0.009*x^2 + 0.002*x^3}
Std_Prec_noSMI6=function(x){0.003*x - 0.012*x^2 - 0.003*x^3}

## Make Predictions for Values similiar to SMI Anomaly Categories
x1 <-  c(2.575829, 1.644854, 1.281552,1.036433, 0.8416212,0.6744898, 0.5244005,-2.575829, -1.644854, -1.281552, -1.036433,- 0.8416212,-0.6744898, -0.5244005 )
round(Std_Prec_noSMI1(x1), 3)
round(Std_Prec_noSMI2(x1), 3)
round(Std_Prec_noSMI3(x1), 3)
round(Std_Prec_noSMI4(x1), 3)
round(Std_Prec_noSMI5(x1), 3)
round(Std_Prec_noSMI6(x1), 3)

Std_Prec_noSMI  <- ggplot(df1, aes(x)) +
  theme_minimal()  +
  ylim(ylim_min, ylim_max) +
  geom_rect(aes(xmin = 1.281552, xmax = rect_xmas, ymin = ylim_min, ymax = ylim_max), fill="gray70", alpha=0.2) +
  geom_rect(aes(xmin = -1.281552, xmax = -rect_xmas, ymin = ylim_min, ymax = ylim_max), fill="gray70", alpha=0.2) +
  geom_rect(aes(xmin = 0.8416212, xmax =  1.281552, ymin = ylim_min, ymax = ylim_max), fill="gray80", alpha=0.2) +
  geom_rect(aes(xmin = - 0.8416212, xmax =  - 1.281552, ymin = ylim_min, ymax = ylim_max), fill="gray80", alpha=0.2) +
  geom_rect(aes(xmin = 0.5244005 , xmax =  0.8416212, ymin = ylim_min, ymax = ylim_max), fill="gray90", alpha=0.2) +
  geom_rect(aes(xmin = -0.5244005 , xmax =  -0.8416212, ymin = ylim_min, ymax = ylim_max), fill="gray90", alpha=0.2) +
  geom_hline(yintercept = c(0, -0.1, -0.2) ,  colour="gray100", linetype = "solid", size=1) +
  geom_vline(xintercept = c(1,-1,2,-2) ,  colour="gray100", linetype = "solid", size=1) +
  stat_function(fun = Std_Prec_noSMI1,    aes(colour="1"), size=1)  +
  stat_function(fun = Std_Prec_noSMI2 ,  aes(colour="2"), size=3) +
  stat_function(fun = Std_Prec_noSMI3 ,  aes(colour="3"), size=3)  +
  stat_function(fun = Std_Prec_noSMI4 ,  aes(colour="4"), size=3)  +
  stat_function(fun = Std_Prec_noSMI5,    aes(colour="5"), size=2)  +
  stat_function(fun = Std_Prec_noSMI6,    aes(colour="6"), size=1)  +  
  theme(axis.text.x = element_text(angle=00, vjust=0, size=30),
        axis.text.y = element_text(angle=0, vjust=0, size=30),
        axis.title.y = element_text(angle=90, size=30),
        axis.title.x = element_text(size=30),
        strip.text.x = element_text(size=30),
        legend.text = element_text(angle=0, vjust=0, size=25),
        legend.title = element_text(angle=0, vjust=0, size=30)) +
  #   geom_vline(xintercept = 0,  colour="grey", linetype = "solid", size=1) +
  #   geom_hline(yintercept = 0,  colour="grey", linetype = "solid", size=1) +
  ylab("Change of Silage Maize") + 
  xlab("Precipitation Standard Deviation") +
  scale_y_continuous(labels=percent) +
  #   xlab("Standard Deviations") +
  scale_colour_manual("Months", values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a","#66a61e","#e6ab02"), 
                      labels=c("May","June","July","August", "September", "October")) 

Std_Prec_noSMI


# Tavg
Std_Tavg_noSMI1 <- function(x){0.005*x - 0.006*x^2 + 0.002*x^3}
Std_Tavg_noSMI2 <- function(x){-0.006*x - 0.012*x^2 - 0.001*x^3}
Std_Tavg_noSMI3 <- function(x){-0.03*x - 0.008*x^2 + 0.004*x^3}
Std_Tavg_noSMI4 <- function(x){0.001*x - 0.012*x^2 - 0.003*x^3}
Std_Tavg_noSMI5 <- function(x){0.008*x - 0.009*x^2 - 0.005*x^3}
Std_Tavg_noSMI6 <- function(x){-0.00003*x - 0.027*x^2 + 0.006*x^3}

round(Std_Tavg_noSMI1(x1), 3)
round(Std_Tavg_noSMI2(x1), 3)
round(Std_Tavg_noSMI3(x1), 3)
round(Std_Tavg_noSMI4(x1), 3)
round(Std_Tavg_noSMI5(x1), 3)
round(Std_Tavg_noSMI6(x1), 3)

Std_Tavg_noSMI  <- ggplot(df1, aes(x)) +
  theme_minimal() +
  ylim(ylim_min, ylim_max) +
  geom_rect(aes(xmin = 1.281552, xmax = rect_xmas, ymin = ylim_min, ymax = ylim_max), fill="gray70", alpha=0.2) +
  geom_rect(aes(xmin = -1.281552, xmax = -rect_xmas, ymin = ylim_min, ymax = ylim_max), fill="gray70", alpha=0.2) +
  geom_rect(aes(xmin = 0.8416212, xmax =  1.281552, ymin = ylim_min, ymax = ylim_max), fill="gray80", alpha=0.2) +
  geom_rect(aes(xmin = - 0.8416212, xmax =  - 1.281552, ymin = ylim_min, ymax = ylim_max), fill="gray80", alpha=0.2) +
  geom_rect(aes(xmin = 0.5244005 , xmax =  0.8416212, ymin = ylim_min, ymax = ylim_max), fill="gray90", alpha=0.2) +
  geom_rect(aes(xmin = -0.5244005 , xmax =  -0.8416212, ymin = ylim_min, ymax = ylim_max), fill="gray90", alpha=0.2) +
  geom_hline(yintercept = c(0, -0.1, -0.2) ,  colour="gray100", linetype = "solid", size=1) +
  geom_vline(xintercept = c(1,-1,2,-2) ,  colour="gray100", linetype = "solid", size=1) +
  stat_function(fun = Std_Tavg_noSMI1,    aes(colour="1"), size=1)  +
  stat_function(fun = Std_Tavg_noSMI2 ,  aes(colour="2"), size=2) +
  stat_function(fun = Std_Tavg_noSMI3 ,  aes(colour="3"), size=3)  +
  stat_function(fun = Std_Tavg_noSMI4 ,  aes(colour="4"), size=2)  +
  stat_function(fun = Std_Tavg_noSMI5,    aes(colour="5"), size=1)  +
  stat_function(fun = Std_Tavg_noSMI6,    aes(colour="6"), size=2)  +  
  theme(axis.text.x = element_text(angle=00, vjust=0, size=30),
        axis.text.y = element_text(angle=0, vjust=0, size=30),
        axis.title.y = element_text(angle=90, size=30),
        axis.title.x = element_text(size=30),
        strip.text.x = element_text(size=30),
        legend.text = element_text(angle=0, vjust=0, size=25),
        legend.title = element_text(angle=0, vjust=0, size=30)) +
  #   geom_vline(xintercept = 0,  colour="grey", linetype = "solid", size=1) +
  #   geom_hline(yintercept = 0,  colour="grey", linetype = "solid", size=1) +
  ylab("Change of Silage Maize") + 
  xlab("Precipitation Standard Deviation") +
  scale_y_continuous(labels=percent) +
  #   xlab("Standard Deviations") +
  scale_colour_manual("Months", values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a","#66a61e","#e6ab02"), 
                      labels=c("May","June","July","August", "September", "October")) 

Std_Tavg_noSMI

grobs <- ggplotGrob(Std_Tavg_noSMI)$grobs
legend <- grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]]

prow <- plot_grid( Std_Prec_noSMI + theme(legend.position="none"),
                   Std_Tavg_noSMI + theme(legend.position="none"),
                   align = 'vh',
                   labels = c('a)', 'b)'), label_size = 30,
                   hjust = -5.2,  vjust= 3.1,
                   nrow = 1
)
prow

p <- plot_grid( prow, legend, rel_widths = c(3, .3))
p
