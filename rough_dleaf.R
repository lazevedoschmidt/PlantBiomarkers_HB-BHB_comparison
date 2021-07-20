##############################################################################
#Name: Lauren Azevedo Schmidt
#R version: 3.6.1
#Purpose: Rough graphing to visualizing pre,PETM,post data
#date started: 02.12.20
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
require(stats)
require(ggsignif)
require(ggpubr)
require(ggpmisc)
require(dplyr)
require(stats)
require(lme4)
require(lmerTest)
require(ggnewscale)

#load data file
dleaf_dat <- read.table("BHB_HB_dleaf.csv", 
                         header=T, sep=",", 
                         na.strings=  "NA", dec=".", strip.white=TRUE) 

dleaf_dat<- subset(dleaf_dat,Epoch!="UNK") 
dleaf_HB<- subset(dleaf_dat,Basin!="BHB") #subsetting data to only sample HB
dleaf_BHB<- subset(dleaf_dat,Basin!="HB") #subsetting data to only sample BHB
##############################################################################
#all data
dleaf_dat <- dleaf_dat %>%
  dplyr::select(Basin, Epoch, Δleaf)%>%
  drop_na()
dleaf_dat<- subset(dleaf_dat,Epoch!="") #subsetting to make sure there are no blanks 
dleaf_dat <- subset(dleaf_dat,Epoch!="PETM")

#HB
dleaf_HB <- dleaf_HB %>%
  dplyr::select(Basin, Epoch,Δleaf)%>%
  drop_na()
dleaf_HB<- subset(dleaf_HB,Epoch!="") 

#BHB
dleaf_BHB <- dleaf_BHB %>%
  dplyr::select(Basin, Epoch,Δleaf)%>%
  drop_na()
dleaf_BHB<- subset(dleaf_BHB,Epoch!="") 

##############################################################################
palette <- c("#9986A5", "#CCBA72","#79402E", "#0F0D0E","#D9D0D3")
# Basic box plot all data
dleaf_plot <- dleaf_dat %>%
  mutate(name = fct_relevel(Epoch, 
                            "Paleocene", "Eocene")) %>%
  ggplot(aes(x = Epoch, y = Δleaf, 
                           shape = Basin)) + 
  geom_boxplot(aes(fill = Basin), alpha=0.75, outlier.shape = NA)+
  geom_jitter(position=position_jitterdodge(jitter.width = NULL,
                                            jitter.height = 0,
                                            dodge.width = 0.75,
                                            seed = NA), 
              size=3, 
              aes(color = Basin))+
  scale_fill_manual(values=wes_palette(n=2, name="IsleofDogs1"), 
                    guide = guide_legend(order = 2)) +
  scale_color_manual(values=wes_palette(n=2, name="IsleofDogs1")) +
  scale_x_discrete(limits=c("Paleocene","Eocene"))+
  labs(x="Epoch", y="dleaf")+
  #ggtitle("Leaf Fractionation across the Paleocene-Eocene boundary")+
  theme_light()
  #guides(color=FALSE, fill=FALSE, shape=FALSE) #use this to delete legend
  
#No difference between fractionation from Paleocene and Eocene but there is a
#difference with Paleocene to PETM, and PETM to Eocene
dleaf_plot

#extracting legend
legend_dleaf <- get_legend(dleaf_plot)

# #runing again without legend
# dleaf_plot <- dleaf_dat %>%
#   mutate(name = fct_relevel(Epoch, 
#                             "Paleocene", "PETM", "Eocene")) %>%
#   ggplot(aes(x = Epoch, y = Δleaf, 
#              shape = Basin)) + 
#   geom_boxplot(aes(fill = Basin), alpha=0.75, outlier.shape = NA)+
#   geom_jitter(position=position_jitterdodge(jitter.width = NULL,
#                                             jitter.height = 0,
#                                             dodge.width = 0.75,
#                                             seed = NA), 
#               size=3, 
#               aes(color = Basin))+
#   scale_fill_manual(values=wes_palette(n=2, name="IsleofDogs1"), 
#                     guide = guide_legend(order = 2)) +
#   scale_color_manual(values=wes_palette(n=2, name="IsleofDogs1")) +
#   scale_x_discrete(limits=c("Paleocene","PETM","Eocene"))+
#   labs(x="Epoch", y="dleaf")+
#   theme_light()+
#   guides(color=FALSE, fill=FALSE, shape=FALSE) #use this to delete legend
# #No difference between fractionation from Paleocene and Eocene but there is a
# #difference with Paleocene to PETM, and PETM to Eocene
# dleaf_plot


#Adding pvalues 
df_pair_dleaf <- pairwiseComparisons::pairwise_comparisons(
  data = dleaf_dat,
  x = Epoch,
  y = Δleaf)

# adding a geom for pairwise comparisons
dleaf_plot<- ggstatsplot:::ggsignif_adder(
  plot = dleaf_plot,
  data =dleaf_dat,
  x = Epoch,
  y = Δleaf,
  df_pairwise = df_pair_dleaf)
dleaf_plot
dleaf_plot <- dleaf_plot + stat_compare_means(aes(group = Basin), label.y = 26, 
                                              method = "t.test")
dleaf_plot
#############################################################################
# # Basic box plot HB
# dleaf_plot_HB <- dleaf_HB %>%
#   mutate(name = fct_relevel(Epoch, 
#                             "Paleocene", "PETM", "Eocene")) %>%
#   ggplot(aes(x = Epoch, y = Δleaf, 
#              shape = Basin)) + 
#   geom_boxplot(aes(fill = Basin), alpha=0.75, outlier.shape = NA)+
#   geom_jitter(position=position_jitterdodge(jitter.width = NULL,
#                                             jitter.height = 0,
#                                             dodge.width = 0.75,
#                                             seed = NA), 
#               size=3, 
#               aes(color = Basin))+
#   scale_fill_manual(values="#C27D38",
#                     guide = guide_legend(order = 2)) +
#   scale_color_manual(values="#C27D38") +
#   scale_x_discrete(limits=c("Paleocene","PETM","Eocene"))+
#   labs(x="Epoch", y="dleaf")+
#   theme_light()+
#   guides(color=FALSE, fill=FALSE, shape=FALSE) #use this to delete legend
# #No difference between fractionation from Paleocene and Eocene but there is a
# #difference with Paleocene to PETM, and PETM to Eocene
# dleaf_plot_HB
# 
# #Adding pvalues 
# df_pair_dleaf_HB <- pairwiseComparisons::pairwise_comparisons(
#   data = dleaf_HB,
#   x = Epoch,
#   y = Δleaf)
# 
# # adding a geom for pairwise comparisons
# dleaf_plot_HB<- ggstatsplot:::ggsignif_adder(
#   plot = dleaf_plot_HB,
#   data =dleaf_HB,
#   x = Epoch,
#   y = Δleaf,
#   df_pairwise = df_pair_dleaf_HB)
# dleaf_plot_HB
# #############################################################################
# # Basic box plot BHB
# dleaf_plot_BHB <- dleaf_BHB %>%
#   mutate(name = fct_relevel(Epoch, 
#                             "Paleocene", "PETM", "Eocene")) %>%
#   ggplot(aes(x = Epoch, y = Δleaf, 
#              shape = Basin)) + 
#   geom_boxplot(aes(fill = Basin), alpha=0.75, outlier.shape = NA)+
#   geom_jitter(position=position_jitterdodge(jitter.width = NULL,
#                                             jitter.height = 0,
#                                             dodge.width = 0.75,
#                                             seed = NA), 
#               size=3, 
#               aes(color = Basin))+
#   scale_fill_manual(values="#798E87",
#                     guide = guide_legend(order = 2)) +
#   scale_color_manual(values="#798E87") +
#   scale_x_discrete(limits=c("Paleocene","PETM","Eocene"))+
#   labs(x="Epoch", y="dleaf")+
#   theme_light()+
#   guides(color=FALSE, fill=FALSE, shape=FALSE) #use this to delete legend
# #No difference between fractionation from Paleocene and Eocene but there is a
# #difference with Paleocene to PETM, and PETM to Eocene
# dleaf_plot_BHB
# 
# #Adding pvalues 
# df_pair_dleaf_BHB <- pairwiseComparisons::pairwise_comparisons(
#   data = dleaf_BHB,
#   x = Epoch,
#   y = Δleaf)
# 
# # adding a geom for pairwise comparisons
# dleaf_plot_BHB<- ggstatsplot:::ggsignif_adder(
#   plot = dleaf_plot_BHB,
#   data =dleaf_BHB,
#   x = Epoch,
#   y = Δleaf,
#   df_pairwise = df_pair_dleaf_BHB)
# dleaf_plot_BHB
# 
# #combining all plots into one super plot
# 
# super_dleaf<- plot_grid(dleaf_plot_HB,
#                       dleaf_plot,
#                       dleaf_plot_BHB,
#                       legend_dleaf,
#                       nrow = 2,
#                       ncol=2,
#                       labels = c("A", "C", "B", NULL))
# super_dleaf

#############################################################################
#saving figure
ggsave("Figures/dleaf_basin_boxplot.pdf", dleaf_plot)
# ggsave("Figures/dleaf_basin_boxplot_HB.pdf", dleaf_plot_HB, units = "in", 
#        width = 6.5, scale = 1)
# ggsave("Figures/dleaf_basin_boxplot_BHB.pdf", dleaf_plot_BHB, units = "in", 
#        width = 6.5, scale = 1)
# ggsave("Figures/dleaf_basin_boxplot_super.pdf", super_dleaf, units = "in")

