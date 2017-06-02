library(ggplot2)
library(reshape)
library(ggthemes)
library(cowplot)
library(scales)

df1 <- data.frame(x = seq(-2,2,0.005))

ylim_min = -0.25
ylim_max = 0.1
rect_xmas = 2

######################################################################
#### Results for Best_Standard Model with polynomials of degree 3 ####
######################################################################
# Precipitation
Std_Prec1=function(x){0.004*x - 0.023*x^2 + 0.004*x^3}
Std_Prec2=function(x){0.036*x - 0.014*x^2 + 0.001*x^3}
Std_Prec3=function(x){0.039*x - 0.023*x^2 + 0.005*x^3}
Std_Prec4=function(x){-0.014*x - 0.019*x^2 + 0.004*x^3}
Std_Prec5=function(x){-0.011*x - 0.005*x^2 + 0.002*x^3}
Std_Prec6=function(x){-0.003*x + 0.002*x^2 - 0.0001*x^3}

## Make Predictions for Values similiar to SMI Anomaly Categories
x1 <-  c(2, 1.644854, 1.281552,1.036433, 0.8416212,0.6744898, 0.5244005,-2, -1.644854, -1.281552, -1.036433,- 0.8416212,-0.6744898, -0.5244005 )
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
  geom_hline(yintercept = c(0.1, 0, -0.1, -0.2) ,  colour="gray100", linetype = "solid", size=2) +
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

ggsave("Std_Prec.png", plot = Std_Prec, device = "png", path = "./figures/figures_exploratory/BIC/Silomaize/",
       width = 10, height = 8 ,      dpi = 300)

# Std_Prec

# Tavg
# Tavg
Std_Tavg1 <- function(x){0.024*x - 0.005*x^2 + 0.0004*x^3}
Std_Tavg2 <- function(x){-0.006*x - 0.006*x^2 - 0.002*x^3}
Std_Tavg3 <- function(x){-0.036*x - 0.007*x^2 + 0.004*x^3}
Std_Tavg4 <- function(x){-0.003*x - 0.008*x^2 - 0.002*x^3}
Std_Tavg5 <- function(x){0.038*x - 0.009*x^2 - 0.013*x^3}
Std_Tavg6 <- function(x){-0.002*x - 0.016*x^2 + 0.005*x^3}
Std_Tavg3_nowater <- function(x){-0.066*x - 0.012*x^2 + 0.007*x^3}

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
  geom_hline(yintercept = c(0.1, 0, -0.1, -0.2) ,  colour="gray100", linetype = "solid", size=2) +
  geom_vline(xintercept = c(1,-1,2,-2) ,  colour="gray100", linetype = "solid", size=2) +
  stat_function(fun = Std_Tavg1,    aes(colour="1"), size=0)  +
  stat_function(fun = Std_Tavg2 ,  aes(colour="2"), size=0) +
  stat_function(fun = Std_Tavg3 ,  aes(colour="3"), size=4)  +
  stat_function(fun = Std_Tavg4 ,  aes(colour="4"), size=4, linetype="dashed")  +
  stat_function(fun = Std_Tavg5,    aes(colour="5"), size=4, linetype="dashed")  +
  stat_function(fun = Std_Tavg6,    aes(colour="6"), size=4, linetype="dashed")  +  
#   stat_function(fun = Std_Tavg3_nowater,    aes(colour="7"), size=4)  +  
  
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


# Std_Tavg

# Std_Tavg
ggsave("Std_Tavg.png", plot = Std_Tavg, device = "png", path = "./figures/figures_exploratory/BIC/Silomaize/",
width = 10, height = 8 ,      dpi = 300)# 

## Make a grid of the two plots 
grobs <- ggplotGrob(Std_Tavg)$grobs
legend <- grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]]

prow <- plot_grid( Std_Prec + theme(legend.position="none"),
                   Std_Tavg + theme(legend.position="none", axis.title.y = element_blank(), axis.text.y = element_blank()),
                   align = 'vh',
                   labels = c('a)', 'b)'), label_size = 30,
                   hjust = -5.2,  vjust= 3.1,
                   nrow = 1
)
# prow

PrecTavg <- plot_grid( prow, legend, rel_widths = c(3, 0.6))
# PrecTavg


ggsave("PrecTavg.png", plot = PrecTavg, device = "png", path = "./figures/figures_exploratory/BIC/Silomaize/",
       width = 20, height = 8 ,      dpi = 300)# 






###############################################################################
#### Results for Best_Standard Model with polynomials of degree 3 - no SMI ####
###############################################################################
# Precipitation
Std_Prec_noSMI1=function(x){-0.002*x -0.021*x^2 + 0.003*x^3}
Std_Prec_noSMI2=function(x){0.021*x -0.020*x^2 + 0.002*x^3}
Std_Prec_noSMI3=function(x){0.040*x -0.024*x^2 + 0.004*x^3}
Std_Prec_noSMI4=function(x){-0.004*x -0.023*x^2 + 0.005*x^3}
Std_Prec_noSMI5=function(x){0.009*x -0.011*x^2 +  0.002*x^3}
Std_Prec_noSMI6=function(x){0.001*x +0.006*x^2 -0.001 *x^3}

## Make Predictions for Values similiar to SMI Anomaly Categories
x1 <-  c(2, 1.644854, 1.281552,1.036433, 0.8416212,0.6744898, 0.5244005,-2, -1.644854, -1.281552, -1.036433,- 0.8416212,-0.6744898, -0.5244005 )
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
  geom_hline(yintercept = c(0.1, 0, -0.1, -0.2) ,  colour="gray100", linetype = "solid", size=2) +
  geom_vline(xintercept = c(1,-1,2,-2) ,  colour="gray100", linetype = "solid", size=2) +
  stat_function(fun = Std_Prec_noSMI1,    aes(colour="1"), size=0)  +
  stat_function(fun = Std_Prec_noSMI2 ,  aes(colour="2"), size=4,  linetype="dashed") +
  stat_function(fun = Std_Prec_noSMI3 ,  aes(colour="3"), size=4)  +
  stat_function(fun = Std_Prec_noSMI4 ,  aes(colour="4"), size=4,  linetype="dashed")  +
  stat_function(fun = Std_Prec_noSMI5,    aes(colour="5"), size=4, linetype="dashed")  +
  stat_function(fun = Std_Prec_noSMI6,    aes(colour="6"), size=0)  +   
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

# Std_Prec_noSMI

ggsave("Std_Prec_noSMI.png", plot = Std_Prec_noSMI, device = "png", path = "./figures/figures_exploratory/BIC/Silomaize/",
       width = 10, height = 8 ,      dpi = 300)# 


# Tavg
Std_Tavg_noSMI1 <- function(x){0.024*x -0.007*x^2 -0.0001*x^3}
Std_Tavg_noSMI2 <- function(x){-0.001*x -0.005*x^2 -0.003*x^3}
Std_Tavg_noSMI3 <- function(x){-0.039*x -0.007*x^2+0.004 *x^3}
Std_Tavg_noSMI4 <- function(x){-0.013*x -0.014*x^2 -0.002*x^3}
Std_Tavg_noSMI5 <- function(x){0.047*x -0.003*x^2 -0.015*x^3}
Std_Tavg_noSMI6 <- function(x){0.004*x -0.020*x^2 + 0.005*x^3}
Std_Tavg_noSMI3_nowater <- function(x){-0.073*x - 0.013*x^2 + 0.007*x^3}

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
  geom_hline(yintercept = c(0.1, 0, -0.1, -0.2) ,  colour="gray100", linetype = "solid", size=2) +
  geom_vline(xintercept = c(1,-1,2,-2) ,  colour="gray100", linetype = "solid", size=2) +
  stat_function(fun = Std_Tavg_noSMI1,    aes(colour="1"), size=0,  linetype="dashed")  +
  stat_function(fun = Std_Tavg_noSMI2 ,  aes(colour="2"), size=0,  linetype="dashed") +
  stat_function(fun = Std_Tavg_noSMI3 ,  aes(colour="3"), size=4)  +
  stat_function(fun = Std_Tavg_noSMI4 ,  aes(colour="4"), size=4,  linetype="dashed")  +
  stat_function(fun = Std_Tavg_noSMI5,    aes(colour="5"), size=4,  linetype="dashed")  +
  stat_function(fun = Std_Tavg_noSMI6,    aes(colour="6"), size=4,  linetype="dashed")  +  
#   stat_function(fun = Std_Tavg_noSMI3_nowater ,  aes(colour="7"), size=4)  +
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

# Std_Tavg_noSMI 
ggsave("Std_Tavg_noSMI.png", plot = Std_Tavg_noSMI, device = "png", path = "./figures/figures_exploratory/BIC/Silomaize/",
       width = 10, height = 8, dpi = 300)# 

################
grobs <- ggplotGrob(Std_Tavg_noSMI)$grobs
legend <- grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]]

prow <- plot_grid( Std_Prec_noSMI + theme(legend.position="none"),
                   Std_Tavg_noSMI + theme(legend.position="none", axis.title.y = element_blank(), axis.text.y = element_blank()),
                   align = 'vh',
                   labels = c('(a)', '(b)'), label_size = 30,
                   hjust = -5.2,  vjust= 3.1,
                   nrow = 1
)
# prow

PrecTavg_noSMI <- plot_grid(prow, legend, rel_widths = c(3, 0.6))



ggsave("PrecTavg_noSMI.png", plot = PrecTavg_noSMI, device = "png", path = "./figures/figures_exploratory/BIC/Silomaize/",
       width = 20, height = 8 ,      dpi = 300)# 

## Plot of Temperature and Precipitation with and without SMI ##
grobs <- ggplotGrob(Std_Tavg)$grobs
legend <- grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]]

prow_4 <- plot_grid(  Std_Prec + theme(legend.position="none", axis.title.x = element_blank(), axis.text.x = element_blank()) ,
                      Std_Tavg + theme(legend.position="none", axis.title.x = element_blank(), axis.text.x = element_blank(),
                                       axis.title.y = element_blank(), axis.text.y = element_blank()),
                      Std_Prec_noSMI + theme(legend.position="none"),
                      Std_Tavg_noSMI + theme(legend.position="none", axis.title.y = element_blank(), axis.text.y = element_blank()),
                   align = 'vh',
                   labels = c('(a)', '(c)', '(b)', '(d)'), label_size = 30,
                   hjust = -4,  vjust= 3.1,
                   nrow = 2
)
# prow_4

PrecTavg_SMI_vs_noSMI <- plot_grid(prow_4, legend, rel_widths = c(3, 0.6))
# p_4

ggsave("PrecTavg_SMI_vs_noSMI.png", plot = PrecTavg_SMI_vs_noSMI, device = "png", path = "./figures/figures_exploratory/BIC/Silomaize/",
       width = 20, height = 16 ,      dpi = 300)# 

ggsave("PrecTavg_SMI_vs_noSMI.pdf", plot = PrecTavg_SMI_vs_noSMI, device = "pdf", path = "./figures/figures_exploratory/BIC/Silomaize/",
       width = 20, height = 16 ,      dpi = 300)# 

#####################################
#### Temperature Sensitivity July ####
ylim_max <- 0.15
Std_Tavg3 
Std_Tavg_noSMI3 
Std_Tavg3_noPrec <- function(x){-0.066*x - 0.012*x^2 + 0.007*x^3}
Std_Tavg_noSMI3_noPrec <- function(x){-0.073*x - 0.013*x^2 + 0.007*x^3}

Std_Tavg_sensitivity  <- ggplot(df1, aes(x)) +
  theme_minimal() +
  ylim(ylim_min, ylim_max) +
  geom_rect(aes(xmin = 1.281552, xmax = rect_xmas, ymin = ylim_min, ymax = ylim_max), fill="gray70", alpha=0.2) +
  geom_rect(aes(xmin = -1.281552, xmax = -rect_xmas, ymin = ylim_min, ymax = ylim_max), fill="gray70", alpha=0.2) +
  geom_rect(aes(xmin = 0.8416212, xmax =  1.281552, ymin = ylim_min, ymax = ylim_max), fill="gray80", alpha=0.2) +
  geom_rect(aes(xmin = - 0.8416212, xmax =  - 1.281552, ymin = ylim_min, ymax = ylim_max), fill="gray80", alpha=0.2) +
  geom_rect(aes(xmin = 0.5244005 , xmax =  0.8416212, ymin = ylim_min, ymax = ylim_max), fill="gray90", alpha=0.2) +
  geom_rect(aes(xmin = -0.5244005 , xmax =  -0.8416212, ymin = ylim_min, ymax = ylim_max), fill="gray90", alpha=0.2) +
  geom_hline(yintercept = c(0.1, 0, -0.1, -0.2) ,  colour="gray100", linetype = "solid", size=2) +
  geom_vline(xintercept = c(1,-1,2,-2) ,  colour="gray100", linetype = "solid", size=2) +
  stat_function(fun = Std_Tavg3,    aes(colour="1"), size=4)  +
  stat_function(fun = Std_Tavg_noSMI3 ,  aes(colour="2"), size=4) +
  stat_function(fun = Std_Tavg3_noPrec ,  aes(colour="3"), size=4)  +
  stat_function(fun = Std_Tavg_noSMI3_noPrec ,  aes(colour="4"), size=4)  +
  #   stat_function(fun = Std_Tavg_noSMI3_nowater ,  aes(colour="7"), size=4)  +
  theme(axis.text.x = element_text(angle=00, vjust=0, size=30),
        axis.text.y = element_text(angle=0, vjust=0, size=30),
        axis.title.y = element_text(angle=90, size=30),
        axis.title.x = element_text(size=30),
        strip.text.x = element_text(size=30),
        legend.text = element_text(angle=0, vjust=0, size=30),
        legend.title = element_text(angle=0, vjust=0, size=30), 
        legend.key.size = unit(2.5, 'lines'),
        plot.title = element_text(size = 40, face = "bold")) +
  #   geom_vline(xintercept = 0,  colour="grey", linetype = "solid", size=2) +
  #   geom_hline(yintercept = 0,  colour="grey", linetype = "solid", size=2) +
  ylab("Change of Silage Maize") + 
  xlab("Temperature Standard Deviation") +
  ggtitle("July") +
  scale_y_continuous(labels=percent) +
  #   xlab("Standard Deviations") +
  scale_colour_manual("Adj. R² / Model", values = c("#7570b3","#635e9b","#4e4a82","#383566"), 
                      labels=c("0.305 / 09_SMIPrecTavg ","0.296 / 08_PrecTavg ","0.219 / 05_SMITavg ","0.204 / 04_Tavg")) 
# labels=c("0.168 / 09_SMIPrecTavg "," 0.113 / 08_PrecTavg ","0.144 / 05_SMITavg ","0.204 / 04_Tavg")) 
Std_Tavg_sensitivity
ggsave("Std_Tavg_sensitivity.png", plot = Std_Tavg_sensitivity, device = "png", path = "./figures/figures_exploratory/BIC/Silomaize/",
       width = 16, height = 8 ,      dpi = 300)

#####################################
#### Temperature Sensitivity AUGUST ####

Std_Tavg4 
Std_Tavg_noSMI4 
Std_Tavg4_noPrec <- function(x){0.001*x - -0.011*x^2  -0.002*x^3}
Std_Tavg_noSMI4_noPrec <- function(x){ -0.01441384*x - -0.0168253*x^2 -0.00221030*x^3} # Adj. R-Squared: 0.09181

Std_Tavg4_sensitivity  <- ggplot(df1, aes(x)) +
  theme_minimal() +
  ylim(ylim_min, ylim_max) +
  geom_rect(aes(xmin = 1.281552, xmax = rect_xmas, ymin = ylim_min, ymax = ylim_max), fill="gray70", alpha=0.2) +
  geom_rect(aes(xmin = -1.281552, xmax = -rect_xmas, ymin = ylim_min, ymax = ylim_max), fill="gray70", alpha=0.2) +
  geom_rect(aes(xmin = 0.8416212, xmax =  1.281552, ymin = ylim_min, ymax = ylim_max), fill="gray80", alpha=0.2) +
  geom_rect(aes(xmin = - 0.8416212, xmax =  - 1.281552, ymin = ylim_min, ymax = ylim_max), fill="gray80", alpha=0.2) +
  geom_rect(aes(xmin = 0.5244005 , xmax =  0.8416212, ymin = ylim_min, ymax = ylim_max), fill="gray90", alpha=0.2) +
  geom_rect(aes(xmin = -0.5244005 , xmax =  -0.8416212, ymin = ylim_min, ymax = ylim_max), fill="gray90", alpha=0.2) +
  geom_hline(yintercept = c(0.1, 0, -0.1, -0.2) ,  colour="gray100", linetype = "solid", size=2) +
  geom_vline(xintercept = c(1,-1,2,-2) ,  colour="gray100", linetype = "solid", size=2) +
  stat_function(fun = Std_Tavg4,    aes(colour="1"), size=4)  +
  stat_function(fun = Std_Tavg_noSMI4 ,  aes(colour="2"), size=4) +
  stat_function(fun = Std_Tavg4_noPrec ,  aes(colour="3"), size=4)  +
  stat_function(fun = Std_Tavg_noSMI4_noPrec ,  aes(colour="4"), size=4)  +
#     stat_function(fun = Std_Tavg_noSMI4_nowater ,  aes(colour="7"), size=4)  +
  theme(axis.text.x = element_text(angle=00, vjust=0, size=30),
        axis.text.y = element_text(angle=0, vjust=0, size=30),
        axis.title.y = element_text(angle=90, size=30),
        axis.title.x = element_text(size=30),
        strip.text.x = element_text(size=30),
        legend.text = element_text(angle=0, vjust=0, size=30),
        legend.title = element_text(angle=0, vjust=0, size=30), 
        legend.key.size = unit(2.5, 'lines'),
        plot.title = element_text(size = 40, face = "bold")) +
  #   geom_vline(xintercept = 0,  colour="grey", linetype = "solid", size=2) +
  #   geom_hline(yintercept = 0,  colour="grey", linetype = "solid", size=2) +
  ylab("Change of Silage Maize") + 
  xlab("Temperature Standard Deviation") +
  ggtitle("August") +
  scale_y_continuous(labels=percent) +
  #   xlab("Standard Deviations") +
  scale_colour_manual("Adj. R² / Model", values = c("#e7298a","#b21e69","#7c164a","#560f33"), 
                      labels=c("0.168 / 09_SMIPrecTavg","0.113 / 08_PrecTavg","0.144 / 05_SMITavg","0.092 / 04_Tavg")) 
# 
#   scale_colour_manual("Model / Adj. R²", values = c("#e7298a","#b21e69","#7c164a","#560f33"), 
#                       labels=c("Aug / 0.168","Aug_noSMI / 0.113","Aug_noPrec / 0.144","Aug_noSMI_noPrec / 0.092")) 
Std_Tavg4_sensitivity
ggsave("Std_Tavg_sensitivity_Aug.png", plot = Std_Tavg4_sensitivity, device = "png", path = "./figures/figures_exploratory/BIC/Silomaize/",
       width = 12, height = 8 ,      dpi = 300)

###############################################
## Make a grid of the two SENSITIVITY  plots ##  
# grobs <- ggplotGrob(Std_Tavg)$grobs
# legend <- grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]]

Std_Tavg_sensitivity_comb <- plot_grid( Std_Tavg_sensitivity + theme(axis.title.x = element_blank(), axis.text.x = element_blank()),
                   Std_Tavg4_sensitivity ,
                   align = 'v',
                   labels = c('(a)','(b)'), label_size = 30,
                   hjust = - 4,  
                   vjust= 4.5,
                   nrow = 2
)
# prow

Std_Tavg_sensitivity_comb 
# PrecTavg


ggsave("Std_Tavg_sensitivity_comb.pdf", plot = Std_Tavg_sensitivity_comb, device = "pdf", path = "./figures/figures_exploratory/BIC/Silomaize/",
       width = 12, height = 16 ,      dpi = 300)# 


# ###############################################################################
# #### Results for Best_Standard Model with polynomials of degree 3 - no2003 ####
# ###############################################################################
# # Precipitation
# Std_Prec_no2003_1=function(x){0.003*x - 0.009*x^2 + 0.001*x^3}
# Std_Prec_no2003_2=function(x){0.041*x - 0.019*x^2 + 0.002*x^3}
# Std_Prec_no2003_3=function(x){0.018*x - 0.015*x^2 + 0.003*x^3}
# Std_Prec_no2003_4=function(x){-0.028*x - 0.004*x^2 +0.002*x^3}
# Std_Prec_no2003_5=function(x){-0.0002*x - 0.003*x^2 + 0.001*x^3}
# Std_Prec_no2003_6=function(x){0.002*x + 0.002*x^2 - 0.0004*x^3}
# 
# ## Make Predictions for Values similiar to SMI Anomaly Categories
# x1 <-  c(2, 1.644854, 1.281552,1.036433, 0.8416212,0.6744898, 0.5244005,-2, -1.644854, -1.281552, -1.036433,- 0.8416212,-0.6744898, -0.5244005 )
# round(Std_Prec_no2003_1(x1), 3)
# round(Std_Prec_no2003_2(x1), 3)
# round(Std_Prec_no2003_3(x1), 3)
# round(Std_Prec_no2003_4(x1), 3)
# round(Std_Prec_no2003_5(x1), 3)
# round(Std_Prec_no2003_6(x1), 3)
# 
# Std_Prec_no2003_  <- ggplot(df1, aes(x)) +
#   theme_minimal()  +
#   ylim(ylim_min, ylim_max) +
#   geom_rect(aes(xmin = 1.281552, xmax = rect_xmas, ymin = ylim_min, ymax = ylim_max), fill="gray70", alpha=0.2) +
#   geom_rect(aes(xmin = -1.281552, xmax = -rect_xmas, ymin = ylim_min, ymax = ylim_max), fill="gray70", alpha=0.2) +
#   geom_rect(aes(xmin = 0.8416212, xmax =  1.281552, ymin = ylim_min, ymax = ylim_max), fill="gray80", alpha=0.2) +
#   geom_rect(aes(xmin = - 0.8416212, xmax =  - 1.281552, ymin = ylim_min, ymax = ylim_max), fill="gray80", alpha=0.2) +
#   geom_rect(aes(xmin = 0.5244005 , xmax =  0.8416212, ymin = ylim_min, ymax = ylim_max), fill="gray90", alpha=0.2) +
#   geom_rect(aes(xmin = -0.5244005 , xmax =  -0.8416212, ymin = ylim_min, ymax = ylim_max), fill="gray90", alpha=0.2) +
#   geom_hline(yintercept = c(0.1, 0, -0.1, -0.2) ,  colour="gray100", linetype = "solid", size=2) +
#   geom_vline(xintercept = c(1,-1,2,-2) ,  colour="gray100", linetype = "solid", size=2) +
#   stat_function(fun = Std_Prec_no2003_1,    aes(colour="1"), size=0, linetype="dashed")  +
#   stat_function(fun = Std_Prec_no2003_2 ,  aes(colour="2"), size=4) +
#   stat_function(fun = Std_Prec_no2003_3 ,  aes(colour="3"), size=4)  +
#   stat_function(fun = Std_Prec_no2003_4 ,  aes(colour="4"), size=4, linetype="dashed")  +
#   stat_function(fun = Std_Prec_no2003_5,    aes(colour="5"), size=0, linetype="dashed")  +
#   stat_function(fun = Std_Prec_no2003_6,    aes(colour="6"), size=0, linetype="dashed")  +  
#   theme(axis.text.x = element_text(angle=00, vjust=0, size=30),
#         axis.text.y = element_text(angle=0, vjust=0, size=30),
#         axis.title.y = element_text(angle=90, size=30),
#         axis.title.x = element_text(size=30),
#         strip.text.x = element_text(size=30),
#         legend.text = element_text(angle=0, vjust=0, size=40),
#         legend.title = element_text(angle=0, vjust=0, size=40), 
#         legend.key.size = unit(2.5, 'lines')) +
#   #   geom_vline(xintercept = 0,  colour="grey", linetype = "solid", size=2) +
#   #   geom_hline(yintercept = 0,  colour="grey", linetype = "solid", size=2) +
#   ylab("Change of Silage Maize") + 
#   xlab("Precipitation Standard Deviation") +
#   scale_y_continuous(labels=percent) +
#   #   xlab("Standard Deviations") +
#   scale_colour_manual("Months", values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a","#1f78b4","#e6ab02"), 
#                       labels=c("May","June","July","Aug.", "Sept.", "Oct.")) 
# 
# Std_Prec_no2003_
# 
# # Tavg -  no2003
# Std_Tavg_no2003_1 <- function(x){0.021*x - 0.005*x^2 + 0.001*x^3}
# Std_Tavg_no2003_2 <- function(x){0.013*x + 0.006*x^2 + 0.001*x^3}
# Std_Tavg_no2003_3 <- function(x){-0.017*x - 0.011*x^2 + 0.001*x^3}
# Std_Tavg_no2003_4 <- function(x){0.010*x + 0.001*x^2 - 0.00005*x^3}
# Std_Tavg_no2003_5 <- function(x){0.003*x - 0.019*x^2 - 0.005*x^3}
# Std_Tavg_no2003_6 <- function(x){-0.016*x - 0.011*x^2 + 0.005*x^3}
# 
# round(Std_Tavg_no2003_1(x1), 3)
# round(Std_Tavg_no2003_2(x1), 3)
# round(Std_Tavg_no2003_3(x1), 3)
# round(Std_Tavg_no2003_4(x1), 3)
# round(Std_Tavg_no2003_5(x1), 3)
# round(Std_Tavg_no2003_6(x1), 3)
# 
# Std_Tavg_no2003_  <- ggplot(df1, aes(x)) +
#   theme_minimal() +
#   ylim(ylim_min, ylim_max) +
#   geom_rect(aes(xmin = 1.281552, xmax = rect_xmas, ymin = ylim_min, ymax = ylim_max), fill="gray70", alpha=0.2) +
#   geom_rect(aes(xmin = -1.281552, xmax = -rect_xmas, ymin = ylim_min, ymax = ylim_max), fill="gray70", alpha=0.2) +
#   geom_rect(aes(xmin = 0.8416212, xmax =  1.281552, ymin = ylim_min, ymax = ylim_max), fill="gray80", alpha=0.2) +
#   geom_rect(aes(xmin = - 0.8416212, xmax =  - 1.281552, ymin = ylim_min, ymax = ylim_max), fill="gray80", alpha=0.2) +
#   geom_rect(aes(xmin = 0.5244005 , xmax =  0.8416212, ymin = ylim_min, ymax = ylim_max), fill="gray90", alpha=0.2) +
#   geom_rect(aes(xmin = -0.5244005 , xmax =  -0.8416212, ymin = ylim_min, ymax = ylim_max), fill="gray90", alpha=0.2) +
#   geom_hline(yintercept = c(0.1, 0, -0.1, -0.2) ,  colour="gray100", linetype = "solid", size=2) +
#   geom_vline(xintercept = c(1,-1,2,-2) ,  colour="gray100", linetype = "solid", size=2) +
#   stat_function(fun = Std_Tavg_no2003_1,    aes(colour="1"), size=0, linetype="dashed")  +
#   stat_function(fun = Std_Tavg_no2003_2 ,  aes(colour="2"), size=4, linetype="dashed") +
#   stat_function(fun = Std_Tavg_no2003_3 ,  aes(colour="3"), size=4)  +
#   stat_function(fun = Std_Tavg_no2003_4 ,  aes(colour="4"), size=4, linetype="dashed")  +
#   stat_function(fun = Std_Tavg_no2003_5,    aes(colour="5"), size=4, linetype="dashed")  +
#   stat_function(fun = Std_Tavg_no2003_6,    aes(colour="6"), size=4, linetype="dashed")  +  
#   theme(axis.text.x = element_text(angle=00, vjust=0, size=30),
#         axis.text.y = element_text(angle=0, vjust=0, size=30),
#         axis.title.y = element_text(angle=90, size=30),
#         axis.title.x = element_text(size=30),
#         strip.text.x = element_text(size=30),
#         legend.text = element_text(angle=0, vjust=0, size=40),
#         legend.title = element_text(angle=0, vjust=0, size=40), 
#         legend.key.size = unit(2.5, 'lines')) +
#   #   geom_vline(xintercept = 0,  colour="grey", linetype = "solid", size=2) +
#   #   geom_hline(yintercept = 0,  colour="grey", linetype = "solid", size=2) +
#   ylab("Change of Silage Maize") + 
#   xlab("Temperature Standard Deviation") +
#   scale_y_continuous(labels=percent) +
#   #   xlab("Standard Deviations") +
#   scale_colour_manual("Months", values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a","#1f78b4","#e6ab02"),
#                       labels=c("May","June","July","Aug.", "Sept.", "Oct.")) 
# 
# 
# Std_Tavg_no2003_
# 
# 
