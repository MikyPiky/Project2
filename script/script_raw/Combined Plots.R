#### Description ####
'
Make combine plots derived from plots in KlimaMeteo_model_plot.R, KlimaMeteo_total_plot.R and Base Prediction_Plots.R.
Each specific plotused in the combined plots needs to be created first in those scripts.  

'

#### Dependencies ###
'
KlimaMeteo_model_plot.R, KlimaMeteo_total_plot.R and Base Prediction_Plots.R.

'

#### Output ####


################################################################################################################################################################################################
#### Make combined Plots of Means ####
################################################################################################################################################################################################


'Combined Plots
- TavJul, PreJul, SMIJun, SMIAug
- TavJul, PreJul, SMIJun, SMIAug, Yield
- TavJul, PreJul, SMIJul
- TavJul, PreJul, SMIJul, Yield'

'For the yield plots it is necessary to load plot_sd_diff...list via the BasePrediction_Plots Script. '

#######################################################
#### Mean plots for TavJul, PreJul, SMIJun, SMIAug ####
plot_mean_SMI_6_Jun_Aug <- grid.arrange(plot_mean_1970_TavJul , plot_mean_1970_PreJul,  plot_mean_1970_SMIJun, plot_mean_1970_SMIAug,
                                        plot_mean_diff2021_TavJul, plot_mean_diff2021_PreJul, plot_mean_diff2021_SMIJun, plot_mean_diff2021_SMIAug,
                                        plot_mean_diff2070_TavJul, plot_mean_diff2070_PreJul, plot_mean_diff2070_SMIJun, plot_mean_diff2070_SMIAug,
                                        ncol=4, top=textGrob(paste(namelist_models[[l]]),gp=gpar(fontsize=30)))
plot_mean_SMI_6_Jun_Aug
# ggsave(paste("./figures/figures_exploratory/Proj/MeteoVar/","plot_mean_SMI_6_Jun_Aug_", namelist_models[[l]],".pdf", sep=""), plot=plot_mean_SMI_6_Jun_Aug , width=28, height=24)

# ##############################################################
# #### Mean plots for TavJul, PreJul, SMIJun, SMIAug, Yield ####
# plot_mean_yield_SMI_6_Jun_Aug <- grid.arrange(plot_mean_1970_TavJul , plot_mean_1970_PreJul,  plot_mean_1970_SMIJun, plot_mean_1970_SMIAug,plot_mean_1971_list[[1]][[l]],
#                                               plot_mean_diff2021_TavJul, plot_mean_diff2021_PreJul, plot_mean_diff2021_SMIJun, plot_mean_diff2021_SMIAug, plot_mean_diff2021_list[[1]][[l]],
#                                               plot_mean_diff2070_TavJul, plot_mean_diff2070_PreJul, plot_mean_diff2070_SMIJun, plot_mean_diff2070_SMIAug, plot_mean_diff2070_list[[1]][[l]],
#                                               ncol = 5, top=textGrob(paste(namelist_models[[l]]),gp=gpar(fontsize=25)))
# plot_mean_yield_SMI_6_Jun_Aug
# 
# # ggsave(paste("./figures/figures_exploratory/Proj/Combined/","plot_mean_yield_SMI_6_Jun_Aug_", namelist_models[[l]],".pdf", sep=""), plot=plot_mean_yield_SMI_6_Jun_Aug , width=35, height=24)
# 
###############################################
#### Mean plots for TavJul, PreJul, SMIJul ####
plot_mean_SMI_6_Jul <- grid.arrange(plot_mean_1970_TavJul , plot_mean_1970_PreJul,  plot_mean_1970_SMIJul,
                                    plot_mean_diff2021_TavJul, plot_mean_diff2021_PreJul, plot_mean_diff2021_SMIJul,
                                    plot_mean_diff2070_TavJul, plot_mean_diff2070_PreJul, plot_mean_diff2070_SMIJul,
                                    ncol=3, top=textGrob(paste(namelist_models[[l]]),gp=gpar(fontsize=30)))
plot_mean_SMI_6_Jul
# ggsave(paste("./figures/figures_exploratory/Proj/MeteoVar/","plot_mean_SMI_6_Jul_", namelist_models[[l]],".pdf", sep=""), plot=plot_mean_SMI_6_Jul , width=28, height=24)

# ######################################################
# #### Mean plots for TavJul, PreJul, SMIJul, Yield ####
# plot_mean_yield_SMI_6_Jul <- grid.arrange(plot_mean_1970_TavJul , plot_mean_1970_PreJul,  plot_mean_1970_SMIJul, plot_mean_1971_list[[2]][[l]],
#                                           plot_mean_diff2021_TavJul, plot_mean_diff2021_PreJul, plot_mean_diff2021_SMIJul,  plot_mean_diff2021_list[[2]][[l]],
#                                           plot_mean_diff2070_TavJul, plot_mean_diff2070_PreJul, plot_mean_diff2070_SMIJul, plot_mean_diff2070_list[[2]][[l]],
#                                           ncol = 4, top=textGrob(paste(namelist_models[[l]]),gp=gpar(fontsize=20)))
# plot_mean_yield_SMI_6_Jul
# 
# # ggsave(paste("./figures/figures_exploratory/Proj/Combined/","plot_mean_yield_SMI_6_Jul_", namelist_models[[l]],".pdf", sep=""), plot=plot_mean_yield_SMI_6_Jul , width=28, height=24)
# 
# 







































# ################################################################################################################################################################################################
# ################################################################################################################################################################################################

# ################################################################################################################################################################################################
# #### Make combined Plots of SDs ####
# ################################################################################################################################################################################################
# '- Combined Plots
# - TavJul, PreJul, SMIJun, SMIAug
# - TavJul, PreJul, SMIJun, SMIAug, Yield
# - TavJul, PreJul, SMIJul
# - TavJul, PreJul, SMIJul, Yield'
# 
# 'For the yield plots it is necessary to load plot_sd_diff...list via the BasePrediction_Plots Script. '
# 
# 
# #######################################################
# #### Sd plots for TavJul, PreJul, SMIJun, SMIAug ####
# 
# plot_sd_SMI_6_Jun_Aug <- grid.arrange(plot_sd_1970_TavJul , plot_sd_1970_PreJul,  plot_sd_1970_SMIJun, plot_sd_1970_SMIAug,
#                                       plot_sd_diff2021_TavJul, plot_sd_diff2021_PreJul, plot_sd_diff2021_SMIJun, plot_sd_diff2021_SMIAug,
#                                       plot_sd_diff2070_TavJul, plot_sd_diff2070_PreJul, plot_sd_diff2070_SMIJun, plot_sd_diff2070_SMIAug,
#                                       ncol=4, top=textGrob(paste(namelist_models[[l]]),gp=gpar(fontsize=30)))
# # plot_sd_SMI_6_Jun_Aug
# # ggsave(paste("./figures/figures_exploratory/Proj/MeteoVar/","plot_sd_SMI_6_Jun_Aug_", namelist_models[[l]],".pdf", sep=""), plot=plot_sd_SMI_6_Jun_Aug , width=28, height=24)
# 
# # ##############################################################
# # #### Sd plots for TavJul, PreJul, SMIJun, SMIAug, Yield ####
# # plot_sd_yield_SMI_6_Jun_Aug <- grid.arrange(plot_sd_1970_TavJul , plot_sd_1970_PreJul,  plot_sd_1970_SMIJun, plot_sd_1970_SMIAug,plot_sd_1971_list[[1]][[l]],
# #                                             plot_sd_diff2021_TavJul, plot_sd_diff2021_PreJul, plot_sd_diff2021_SMIJun, plot_sd_diff2021_SMIAug, plot_sd_diff2021_list[[1]][[l]],
# #                                             plot_sd_diff2070_TavJul, plot_sd_diff2070_PreJul, plot_sd_diff2070_SMIJun, plot_sd_diff2070_SMIAug, plot_sd_diff2070_list[[1]][[l]],
# #                                             ncol = 5, top=textGrob(paste(namelist_models[[l]]),gp=gpar(fontsize=25)))
# # plot_sd_yield_SMI_6_Jun_Aug
# # 
# # # ggsave(paste("./figures/figures_exploratory/Proj/Combined/","plot_sd_yield_SMI_6_Jun_Aug_", namelist_models[[l]],".pdf", sep=""), plot=plot_sd_yield_SMI_6_Jun_Aug , width=35, height=24)
# # 
# ###############################################
# #### Sd plots for TavJul, PreJul, SMIJul ####
# plot_sd_SMI_6_Jul <- grid.arrange(plot_sd_1970_TavJul , plot_sd_1970_PreJul,  plot_sd_1970_SMIJul,
#                                   plot_sd_diff2021_TavJul, plot_sd_diff2021_PreJul, plot_sd_diff2021_SMIJul,
#                                   plot_sd_diff2070_TavJul, plot_sd_diff2070_PreJul, plot_sd_diff2070_SMIJul,
#                                   ncol=3, top=textGrob(paste(namelist_models[[l]]),gp=gpar(fontsize=30)))
# # plot_sd_SMI_6_Jul
# # ggsave(paste("./figures/figures_exploratory/Proj/MeteoVar/","plot_sd_SMI_6_Jul_", namelist_models[[l]],".pdf", sep=""), plot=plot_sd_SMI_6_Jul , width=28, height=24)
# 
# # ######################################################
# # #### Sd plots for TavJul, PreJul, SMIJul, Yield ####
# # plot_sd_yield_SMI_6_Jul <- grid.arrange(plot_sd_1970_TavJul , plot_sd_1970_PreJul,  plot_sd_1970_SMIJul, plot_sd_1971_list[[2]][[l]],
# #                                         plot_sd_diff2021_TavJul, plot_sd_diff2021_PreJul, plot_sd_diff2021_SMIJul,  plot_sd_diff2021_list[[2]][[l]],
# #                                         plot_sd_diff2070_TavJul, plot_sd_diff2070_PreJul, plot_sd_diff2070_SMIJul, plot_sd_diff2070_list[[2]][[l]],
# #                                         ncol = 4, top=textGrob(paste(namelist_models[[l]]),gp=gpar(fontsize=20)))
# # plot_sd_yield_SMI_6_Jul
# # 
# # # ggsave(paste("./figures/figures_exploratory/Proj/Combined/","plot_sd_yield_SMI_6_Jul_", namelist_models[[l]],".pdf", sep=""), plot=plot_sd_yield_SMI_6_Jul , width=28, height=24)

