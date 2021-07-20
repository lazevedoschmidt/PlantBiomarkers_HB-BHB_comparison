##############################################################################
#Name: Lauren Azevedo Schmidt
#R version: 3.6.1
#Purpose: dD C29 against modern WY rainwater dD
#date started: 02.19.20
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
dD_data <- read.table("BHB_HB_dD.csv", 
                   header=T, sep=",", 
                   na.strings=  "NA", dec=".", strip.white=TRUE)
dD_data<- subset(dD_data,Epoch!="UNK") 
#cleaning dataframes
dD_data<- dD_data %>%
  rename(
    dD_C29 = dD.n.C29.alkane.mean,
    dD_C31 = dD.n.C31.alkane.mean
  )
dD_HB<- subset(dD_data,Basin!="Bighorn Basin") #subsetting data to only sample HB
dD_BHB<- subset(dD_data,Basin!="Hanna Basin") #subsetting data to only sample BHB
##############################################################################
#all data
dD_data <- dD_data %>%
  dplyr::select(Basin, Epoch, dD_C29, dD_C31)%>%
  drop_na()
dD_data<- subset(dD_data,Epoch!="") #subsetting to make sure there are no blanks 
#HB
dD_HB <- dD_HB %>%
  dplyr::select(Basin, Epoch,dD_C29, dD_C31)%>%
  drop_na()
dD_HB<- subset(dD_HB,Epoch!="") 

#BHB
dD_BHB <- dD_BHB %>%
  dplyr::select(Basin, Epoch,dD_C29, dD_C31)%>%
  drop_na()
dD_BHB<- subset(dD_BHB,Epoch!="") 

#counting samples
table(dD_BHB$Epoch)
table(dD_HB$Epoch)
##############################################################################
# Basic box plot
dD_29_plot <- dD_data %>%
  mutate(name = fct_relevel(Epoch, 
                            "Paleocene", "PETM", "Eocene")) %>%
  ggplot(aes(x = Epoch, y = dD_C29, shape=Basin)) +
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
  scale_x_discrete(limits=c("Paleocene","PETM","Eocene"))+
  ylim(-225,-140)+
  labs(x="", y=expression(delta^2*'H C'[29]))+
  theme_light()+
  theme(text = element_text(size = 20))+
  guides(color=FALSE, fill=FALSE, shape=FALSE) #use this to delete legend
  
dD_29_plot

#Adding pvalues 
df_pair <- pairwiseComparisons::pairwise_comparisons(
  data = dD_data,
  x = Epoch,
  y = dD_C29,
  type = "parametric",
  var.equal = TRUE,
  paired = FALSE,
  p.adjust.method = "bonferroni")

# adding a geom for pairwise comparisons
dD_29_plot <-ggstatsplot:::ggsignif_adder(
  plot = dD_29_plot,
  data =dD_data ,
  x = Epoch,
  y = dD_C29,
  df_pairwise = df_pair)
dD_29_plot

dD_29_plot <- dD_29_plot + stat_compare_means(aes(group = Basin), 
                                              hide.ns = TRUE, label.y = -140, 
                                              method = "anova",
                                              )
#adding p values to basin comparisions 
dD_29_plot

#dD 31
dD_31_plot <- dD_data %>%
  mutate(name = fct_relevel(Epoch, 
                            "Paleocene", "PETM", "Eocene")) %>%
  ggplot(aes(x = Epoch, y = dD_C31, shape=Basin)) +
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
  scale_x_discrete(limits=c("Paleocene","PETM","Eocene"))+
  ylim(-225,-140)+
  labs(x="", y=expression(delta^2*'H C'[31]))+
  theme_light()+
  theme(text = element_text(size = 20))+
  theme(legend.justification = c(1, 0), legend.position = c(0.95, 0.05))
#same is true here.  
dD_31_plot

#extracting legend
#legend1 <- get_legend(dD_31_plot)

#rerun without legend
# dD_31_plot <- dD_data %>%
#   mutate(name = fct_relevel(Epoch, 
#                             "Paleocene", "PETM", "Eocene")) %>%
#   ggplot(aes(x = Epoch, y = dD_C31, shape=Basin)) +
#   geom_boxplot(aes(fill = Basin), alpha=0.75, outlier.shape = NA)+
#   geom_jitter(position=position_jitterdodge(jitter.width = NULL,
#                                             jitter.height = 0,
#                                             dodge.width = 0.75,
#                                             seed = NA), 
#               size=3, 
#               aes(color = Basin))+
#   # theme(legend.position = "none")+ #For some reason this is not deleting the legend
#   scale_fill_manual(values=wes_palette(n=2, name="IsleofDogs1"), 
#                     guide = guide_legend(order = 2)) +
#   scale_color_manual(values=wes_palette(n=2, name="IsleofDogs1")) +
#   scale_x_discrete(limits=c("Paleocene","PETM","Eocene"))+
#   labs(x="", y=expression(delta^2*'H C'[31]))+
#   theme_light()+
#   guides(color=FALSE, fill=FALSE, shape=FALSE) #use this to delete legend
# #same is true here.  
# dD_31_plot

#Adding pvalues 
df_pair_31 <- pairwiseComparisons::pairwise_comparisons(
  data = dD_data,
  x = Epoch,
  y = dD_C31,
  type = "parametric",
  var.equal = TRUE,
  paired = FALSE,
  p.adjust.method = "bonferroni")

# adding a geom for pairwise comparisons
dD_31_plot<- ggstatsplot:::ggsignif_adder(
  plot = dD_31_plot,
  data =dD_data ,
  x = Epoch,
  y = dD_C31,
  df_pairwise = df_pair_31)

dD_31_plot <- dD_31_plot + stat_compare_means(aes(group = Basin), label.y = -140, 
                                              method = "anova") 
                                               
dD_31_plot
###############################################################################
# # Basic box plot HB
# #dD 29 HANNA BASIN ONLY 
# dD29_HB_plot <- dD_HB %>%
#   mutate(name = fct_relevel(Epoch, 
#                             "Paleocene","PETM", "Eocene")) %>%
#   ggplot(aes(x = Epoch, y = dD_C29, 
#              color = Basin, shape=Basin)) + 
#   geom_boxplot(aes(fill = Basin), alpha=0.75, outlier.shape = NA)+
#   geom_jitter(position=position_jitterdodge(jitter.width = NULL,
#                                             jitter.height = 0,
#                                             dodge.width = 0.75,
#                                             seed = NA), 
#               size=3, 
#               aes(color = Basin))+
#   scale_color_manual(values="#79402E") +
#   scale_fill_manual(values="#79402E", guide = guide_legend(order = 2)) +
#   scale_x_discrete(limits=c("Paleocene","PETM","Eocene"))+
#   labs(x="Epoch", y=expression(delta~"2H"))+
#   #ggtitle("Hanna Basin")+
#   #theme(legend.position = "none")+ #not working?? not sure why.  
#   theme_light()+
#   guides(color=FALSE, fill=FALSE, shape=FALSE) #use this to delete legend
# #same is true here.  
# dD29_HB_plot
# #Do not need code below because no time slice is significant
# 
# #Adding pvalues
# df_pair_29_HB <- pairwiseComparisons::pairwise_comparisons(
#   data = dD_HB,
#   x = Epoch,
#   y = dD_C29,
#   type = "parametric",
#   var.equal = TRUE,
#   paired = FALSE,
#   p.adjust.method = "bonferroni")
# 
# # adding a geom for pairwise comparisons
# dD29_HB_plot<- ggstatsplot:::ggsignif_adder(
#   plot = dD29_HB_plot,
#   data =dD_HB,
#   x = Epoch,
#   y = dD_C29,
#   df_pairwise = df_pair_29_HB)
# dD29_HB_plot #nothing is significant so it wont print 
# 
# #dD 31 HANNA BASIN ONLY 
# dD31_HB_plot <- dD_HB %>%
#   mutate(name = fct_relevel(Epoch, 
#                             "Paleocene","PETM", "Eocene")) %>%
#   ggplot(aes(x = Epoch, y = dD_C31, 
#              color = Basin, shape=Basin)) + 
#   geom_boxplot(aes(fill = Basin), alpha=0.75, outlier.shape = NA)+
#   geom_jitter(position=position_jitterdodge(jitter.width = NULL,
#                                             jitter.height = 0,
#                                             dodge.width = 0.75,
#                                             seed = NA), 
#               size=3, 
#               aes(color = Basin))+
#   scale_color_manual(values="#79402E") +
#   scale_fill_manual(values="#79402E", guide = guide_legend(order = 2)) +
#   scale_x_discrete(limits=c("Paleocene","PETM","Eocene"))+
#   labs(x="Epoch", y=expression(delta~"2H"))+
#   #ggtitle("Hanna Basin")+
#   #theme(legend.position = "none")+ #not working?? not sure why.  
#   theme_light()+
#   guides(color=FALSE, fill=FALSE, shape=FALSE) #use this to delete legend
# #same is true here.  
# dD31_HB_plot
# 
# #Adding pvalues
# df_pair_31_HB <- pairwiseComparisons::pairwise_comparisons(
#   data = dD_HB,
#   x = Epoch,
#   y = dD_C31,
#   type = "parametric",
#   var.equal = TRUE,
#   paired = FALSE,
#   p.adjust.method = "bonferroni")
# 
# # adding a geom for pairwise comparisons
# dD31_HB_plot<- ggstatsplot:::ggsignif_adder(
#   plot = dD31_HB_plot,
#   data =dD_HB,
#   x = Epoch,
#   y = dD_C31,
#   df_pairwise = df_pair_31_HB)
# dD31_HB_plot
# #############################################################################
# # Basic box plot BHB
# #dD 29 BIGHORN BASIN ONLY 
# dD29_BHB_plot <- dD_BHB %>%
#   mutate(name = fct_relevel(Epoch, 
#                             "Paleocene","PETM", "Eocene")) %>%
#   ggplot(aes(x = Epoch, y = dD_C29, 
#              color = Basin, shape=Basin)) + 
#   geom_boxplot(aes(fill = Basin), alpha=0.75, outlier.shape = NA)+
#   geom_jitter(position=position_jitterdodge(jitter.width = NULL,
#                                             jitter.height = 0,
#                                             dodge.width = 0.75,
#                                             seed = NA), 
#               size=3, 
#               aes(color = Basin))+
#   scale_color_manual(values="#9986A5") +
#   scale_fill_manual(values="#9986A5", guide = guide_legend(order = 2)) +
#   scale_x_discrete(limits=c("Paleocene","PETM","Eocene"))+
#   labs(x="Epoch", y="dD C29")+
#   #ggtitle("Hanna Basin")+
#   #theme(legend.position = "none")+ #not working?? not sure why.  
#   theme_light()+
#   guides(color=FALSE, fill=FALSE, shape=FALSE) #use this to delete legend
# #same is true here.  
# dD29_BHB_plot
# 
# #Adding pvalues
# df_pair_29_BHB <- pairwiseComparisons::pairwise_comparisons(
#   data = dD_BHB,
#   x = Epoch,
#   y = dD_C29,
#   type = "parametric",
#   var.equal = TRUE,
#   paired = FALSE,
#   p.adjust.method = "bonferroni")
# 
# # adding a geom for pairwise comparisons
# dD29_BHB_plot<- ggstatsplot:::ggsignif_adder(
#   plot = dD29_BHB_plot,
#   data =dD_BHB,
#   x = Epoch,
#   y = dD_C29,
#   df_pairwise = df_pair_29_BHB)
# dD29_BHB_plot
# 
# #dD 31 HANNA BASIN ONLY 
# dD31_BHB_plot <- dD_BHB %>%
#   mutate(name = fct_relevel(Epoch, 
#                             "Paleocene","PETM", "Eocene")) %>%
#   ggplot(aes(x = Epoch, y = dD_C31, 
#              color = Basin, shape=Basin)) + 
#   geom_boxplot(aes(fill = Basin), alpha=0.75, outlier.shape = NA)+
#   geom_jitter(position=position_jitterdodge(jitter.width = NULL,
#                                             jitter.height = 0,
#                                             dodge.width = 0.75,
#                                             seed = NA), 
#               size=3, 
#               aes(color = Basin))+
#   scale_color_manual(values="#9986A5") +
#   scale_fill_manual(values="#9986A5", guide = guide_legend(order = 2)) +
#   scale_x_discrete(limits=c("Paleocene","PETM","Eocene"))+
#   labs(x="Epoch", y="dD C31")+
#   #ggtitle("Hanna Basin")+
#   #theme(legend.position = "none")+ #not working?? not sure why.  
#   theme_light()+
#   guides(color=FALSE, fill=FALSE, shape=FALSE) #use this to delete legend
# #same is true here.  
# dD31_BHB_plot
# 
# #Adding pvalues
# df_pair_31_BHB <- pairwiseComparisons::pairwise_comparisons(
#   data = dD_BHB,
#   x = Epoch,
#   y = dD_C31,
#   type = "parametric",
#   var.equal = TRUE,
#   paired = FALSE,
#   p.adjust.method = "bonferroni")
# 
# # adding a geom for pairwise comparisons
# dD31_BHB_plot<- ggstatsplot:::ggsignif_adder(
#   plot = dD31_BHB_plot,
#   data =dD_BHB,
#   x = Epoch,
#   y = dD_C31,
#   df_pairwise = df_pair_31_BHB)
# dD31_BHB_plot

##############################################################################
# #Plotting the four plots tracking basin through time together 
# dD_basin_super<- plot_grid( dD29_HB_plot,
#                             dD29_BHB_plot,
#                             legend1,
#                             dD31_HB_plot,
#                             dD31_BHB_plot,
#                             nrow = 2,
#                             ncol=3, 
#                             labels = c("AUTO"))
# dD_basin_super
# ##############################################################################
# #making palette
# palette <- c("#9986A5", "#CCBA72","#79402E", "#0F0D0E","#D9D0D3")
# #Correlation between dD C29 and C31
#  dD_29_31_plot <- ggplot(dD_data, aes(x=dD_C29, y=dD_C31, shape=Basin, color=Basin))+
#       geom_point(size=3.5, aes(colour=Epoch))+
#       geom_smooth(method = lm, se=FALSE, fullrange=TRUE,aes(fill=Basin))+
#       scale_color_manual(values=palette)+
#       #scale_x_discrete(limits=c("Paleocene","PETM","Eocene"))+
#       labs(x=expression("C"[29]), y=expression("C"[31]), color="Basin")+
#       #theme(legend.position = "none")+
#       stat_fit_glance(method = "lm",
#                   label.y = "top",
#                   label.x = "left",
#                   method.args = list(formula=y~x),
#                   mapping = aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2g',
#                                                 stat(r.squared), stat(p.value))),
#                   parse = TRUE)+
#       theme_light()+
#    theme(legend.justification = c(1, 0), legend.position = c(0.95, 0.05))
# #guides(color=FALSE, fill=FALSE, shape=FALSE) #use this to delete legend
# dD_29_31_plot
# 
# #correlation test
# #BHB
# cor.test(dD_BHB$dD_C29, dD_BHB$dD_C31)
# #HB
# cor.test(dD_HB$dD_C29, dD_HB$dD_C31)
##############################################################################
#Plotting the two significant plots together 
dD_super<- plot_grid( dD_29_plot,
                      dD_31_plot,
                      nrow = 1,
                      ncol=2,
                      labels = c("A", "B"))
dD_super

#############################################################################
#saving figure
# ggsave("Figures/dD29_basin.pdf", dD_29_plot, units = "in", width = 6.5,scale =1)
# ggsave("Figures/dD31_basin.pdf", dD_31_plot, units = "in", width = 6.5,scale =1)
ggsave("Figures/dD_super.pdf",dD_super, width = 15, height = 13)
#ggsave("Figures/dD_super_basin.pdf",dD_basin_super, width = 6.5, scale = 1)
#fix parameters.  THey dont fit in this figure and thigns are getting cut off
