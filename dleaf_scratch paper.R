##############################################################################
#Name: Lauren Azevedo Schmidt
#R version: 3.6.1
#Purpose: dLeaf with Monte Carlo error
#date started: 04.24.20
#############################################################################
#wd
getwd()
#packages
require(ggplot2)
require(vegan)
require(paletteer)
require(tidyverse)
require(wesanderson)
require(cowplot)
library(ggridges)
##############################################################################
#Monte Carlo script must be run in tandum with this script!!!!!!!!
##############################################################################
#adding SD to each point
dleaf_data$Dleaf_C29_Low <- dleaf_data$Dleaf_nC29_MC_mean - dleaf_data$Dleaf_nC29_MC_1sd
dleaf_data$Dleaf_C29_High <- dleaf_data$Dleaf_nC29_MC_mean + dleaf_data$Dleaf_nC29_MC_1sd
#same for C31
dleaf_data$Dleaf_C31_Low <- dleaf_data$Dleaf_nC31_MC_mean - dleaf_data$Dleaf_nC31_MC_1sd
dleaf_data$Dleaf_C31_High <- dleaf_data$Dleaf_nC31_MC_mean + dleaf_data$Dleaf_nC31_MC_1sd
###############################################################################
  ggplot(dleaf_data, aes(x=Basin,y=Dleaf_nC29_MC_mean, color=Epoch, fill=Epoch)) +
#   geom_bar(stat="identity", position=position_dodge(), width = 0.5)+
#   geom_errorbar(aes(ymin = Dleaf_C29_Low, ymax = Dleaf_C29_High),width=.2,
#                 position=position_dodge(.9))+
#   labs(x="Basins", y= "Leaf Fractionation (dleaf)") +
#   theme_minimal()+
#   theme(text = element_text(size = 12))+
#   scale_color_manual(values=wes_palette(n=2, name="Moonrise2"))+
#   scale_fill_manual(values=wes_palette(n=2, name="Moonrise2"),
#                     guide = guide_legend(order = 2)) 
# 
# dleaf_data %>%
#   ggplot(aes(x = Basin, 
#              color = Epoch)) +
#   geom_errorbar(aes(ymax = Dleaf_nC29_MC_mean + Dleaf_nC29_MC_1sd,
#                     ymin = Dleaf_nC29_MC_mean - Dleaf_nC29_MC_1sd),
#                 position = "dodge")+
#   geom_boxplot(aes(fill = Epoch), alpha=0.75)+
#   scale_fill_manual(values=wes_palette(n=2, name="Moonrise2"),
#                     guide = guide_legend(order = 2)) +
#   scale_color_manual(values=wes_palette(n=2, name="Moonrise2")) +
#   labs(x="Basin", y="Leaf Franctionation (dleaf)")+
#   theme_light()
