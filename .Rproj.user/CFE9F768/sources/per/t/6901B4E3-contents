##############################################################################
#Name: Lauren Azevedo Schmidt
#R version: 3.6.1
#Purpose: d13C alkane values across basins
#date started: 03.11.2020
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
library(gridExtra)

#load data file
d13C_alk <- read.table("BHB_HB_d13Calkane.csv", 
                   header=T, sep=",", 
                   na.strings=  "NA", dec=".", strip.white=TRUE)
d13C_alk<- subset(d13C_alk,Epoch!="UNK") 

#cleaning dataframes
d13C_alk$d13C.TOC<-NULL
d13C_alk<- d13C_alk %>%
  rename(
    C29 = d13C.C29..mean..VPDB.,
    C31 = d13C.C31..mean..VPDB.
  )
#HB only dataset
d13C_alkHB<- subset(d13C_alk,Basin!="Bighorn Basin")
#BHB only dataset
d13C_alkBHB<- subset(d13C_alk,Basin!="Hanna Basin")

##############################################################################
#all data
d13C_alk <- d13C_alk %>%
  dplyr::select(C29,C31, Basin, Epoch)%>%
  drop_na()
d13C_alk<- subset(d13C_alk,Epoch!="") 
#HB
d13C_alkHB <- d13C_alkHB %>%
  dplyr::select(C29,C31, Basin, Epoch)%>%
  drop_na()
d13C_alkHB<- subset(d13C_alkHB,Epoch!="") 

#BHB
d13C_alkBHB <- d13C_alkBHB %>%
  dplyr::select(C29,C31, Basin, Epoch)%>%
  drop_na()
d13C_alkBHB<- subset(d13C_alkBHB,Epoch!="") 

#counting samples
table(d13C_alkBHB$Epoch)
table(d13C_alkHB$Epoch)
##############################################################################
#d13C 29 Alkane
d13C29_plot <- d13C_alk %>%
  mutate(name = fct_relevel(Epoch, 
                            "Paleocene", "PETM", "Eocene")) %>%
  ggplot(aes(x = Epoch, y = C29, 
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
  scale_x_discrete(limits=c("Paleocene","PETM","Eocene"))+
  ylim(-37.5,-27.5)+
  labs(x="", y=expression(delta^13*'C C'[29]))+
  #theme(legend.position = "none")+
  theme_light()+
  theme(text = element_text(size = 20))+  
  guides(color=FALSE, fill=FALSE, shape=FALSE) #use this to delete legend

d13C29_plot 

#Adding pvalues
df_pair_C29 <- pairwiseComparisons::pairwise_comparisons(
  data = d13C_alk,
  x = Epoch,
  y = C29,
  type = "nonparametric",
  var.equal = TRUE,
  paired = FALSE,
  p.adjust.method = "bonferroni")

# adding a geom for pairwise comparisons
d13C29_plot<- ggstatsplot:::ggsignif_adder(
  plot = d13C29_plot,
  data =d13C_alk,
  x = Epoch,
  y = C29,
  df_pairwise = df_pair_C29)

d13C29_plot <- d13C29_plot +stat_compare_means(aes(group = Basin), label.y = -27.5,
                                               method = "anova")
d13C29_plot

#d13C 31 Alkane
d13C31_plot <- d13C_alk %>%
  mutate(name = fct_relevel(Epoch, 
                            "Paleocene", "PETM", "Eocene")) %>%
  ggplot(aes(x = Epoch, y = C31, 
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
  scale_x_discrete(limits=c("Paleocene","PETM","Eocene"))+
  ylim(-37.5,-27.5)+
  labs(x="", y=expression(delta^13*'C C'[31]))+
  #theme(legend.position = "none")+
  theme_light()+
  theme(text = element_text(size = 20))
  #guides(color=FALSE, fill=FALSE, shape=FALSE) #use this to delete legend

d13C31_plot

#extracting legend
legend <- get_legend(d13C31_plot)

#Rerun this to delete legend
d13C31_plot <-  d13C_alk %>%
  mutate(name = fct_relevel(Epoch, 
                            "Paleocene", "PETM", "Eocene")) %>%
  ggplot(aes(x = Epoch, y = C31, 
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
  scale_x_discrete(limits=c("Paleocene","PETM","Eocene"))+
  ylim(-37.5,-27.5)+
  labs(x="", y=expression(delta^13*'C C'[31]))+
  #theme(legend.position = "none")+
  theme_light()+
  theme(text = element_text(size = 20))+
  guides(color=FALSE, fill=FALSE, shape=FALSE) #use this to delete legend
d13C31_plot
#Adding pvalues
df_pair_C31 <- pairwiseComparisons::pairwise_comparisons(
  data = d13C_alk,
  x = Epoch,
  y = C31,
  type = "nonparametric",
  var.equal = TRUE,
  paired = FALSE,
  p.adjust.method = "bonferroni")

# adding a geom for pairwise comparisons
d13C31_plot<- ggstatsplot:::ggsignif_adder(
  plot = d13C31_plot,
  data =d13C_alk,
  x = Epoch,
  y = C31,
  df_pairwise = df_pair_C31)
d13C31_plot <- d13C31_plot +stat_compare_means(aes(group = Basin), label.y = -27.5, 
                                               method = "anova") 
d13C31_plot
###############################################################################
#plotting HB c29 vs c31
####using ggplot
#making palette
palette <- c("#9986A5", "#CCBA72","#79402E", "#0F0D0E","#D9D0D3")
m1 <- lm(C31 ~ C29, data = d13C_alk)
summary(m1)

# c29vsc31_plot_SE <- ggplot(d13C_alk, aes(x=C29, 
#                                          y=C31, 
#                                          shape=Basin, color=Basin))+
#   geom_point(size=3.5, aes(colour=Epoch))+
#   geom_smooth(method = lm, se=TRUE, fullrange=TRUE,aes(fill=Basin))+
#   scale_color_manual(values=palette) +
#   #scale_x_discrete(limits=c("Paleocene","PETM","Eocene"))+
#   labs(x=expression("C"[29]), y=expression("C"[31]), color="Basin")+
#   ylim(-37.5,-27.5)+
#   #theme(legend.position = "none")+
#   stat_fit_glance(method = "lm",
#                   label.y = "top",
#                   label.x = "left",
#                   method.args = list(formula=y~x),
#                   mapping = aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2g',
#                                                 stat(r.squared), stat(p.value))),
#                   parse = TRUE)+
#   theme_light()+
#   theme(text = element_text(size = 20))
# c29vsc31_plot_SE #Standard error and legend  

#no SE
c29vsc31_plot<- ggplot(d13C_alk, aes(x=C29, 
                     y=C31, 
                     shape=Basin, color=Basin))+
  geom_point(size=3.5, aes(colour=Epoch))+
  geom_smooth(method = lm, se=FALSE, fullrange=TRUE,aes(fill=Basin))+
  scale_color_manual(values=palette) +
  #scale_x_discrete(limits=c("Paleocene","PETM","Eocene"))+
  labs(x=expression("C"[29]), y=expression("C"[31]), color="Basin")+
  ylim(-37.5,-27.5)+
  stat_fit_glance(method = "lm",
                  label.y = "top",
                  label.x = "left",
                  method.args = list(formula=y~x),
                  mapping = aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2g',
                                                stat(r.squared), stat(p.value))),
                  parse = TRUE)+
  theme_light()+
  theme(text = element_text(size = 20))+
  theme(legend.justification = c(1, 0), legend.position = c(0.95, 0.05))
  #theme(legend.position = "none")
  #guides(color=FALSE, fill=FALSE, shape=FALSE) #use this to delete legend
c29vsc31_plot

#extracting legend
#legend1 <- get_legend(c29vsc31_plot)

# #HB only
# HB_C29vs31 <-ggplot(d13C_alkHB, aes(x=C29, 
#                         y=C31, 
#                         shape=Basin, color=Basin))+
#   geom_point(size=3.5)+
#   geom_smooth(method = lm, se=TRUE, fullrange=TRUE,aes(fill=Basin))+
#   scale_color_manual(values="#79402E") +
#   #scale_x_discrete(limits=c("Paleocene","PETM","Eocene"))+
#   labs(x="C29", y="C31", color="Basin")+
#   theme(legend.position = "none")+
#   stat_fit_glance(method = "lm",
#                   label.y = "top",
#                   label.x = "left",
#                   method.args = list(formula=y~x),
#                   mapping = aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2g',
#                                                 stat(r.squared), stat(p.value))),
#                   parse = TRUE)+
#   theme_light()
# HB_C29vs31
# 
# #BHB only
# BHB_C29vs31 <-ggplot(d13C_alkBHB, aes(x=C29, 
#                                     y=C31, 
#                                     shape=Basin, color=Basin))+
#   geom_point(size=3.5)+
#   geom_smooth(method = lm, se=TRUE, fullrange=TRUE,aes(fill=Basin))+
#   scale_color_manual(values="#9986A5") +
#   #scale_x_discrete(limits=c("Paleocene","PETM","Eocene"))+
#   labs(x="C29", y="C31", color="Basin")+
#   theme(legend.position = "none")+
#   stat_fit_glance(method = "lm",
#                   label.y = "top",
#                   label.x = "left",
#                   method.args = list(formula=y~x),
#                   mapping = aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2g',
#                                                 stat(r.squared), stat(p.value))),
#                   parse = TRUE)+
#   theme_light()
# BHB_C29vs31
###############################################################################
#testing correlation
#BHB
cor.test(d13C_alkBHB$C29,d13C_alkBHB$C31)
#HB
cor.test(d13C_alkHB$C29,d13C_alkHB$C31)

###############################################################################
#Plotting all plots together
#creating plot 
carbon_super<- plot_grid( d13C29_plot,
                          d13C31_plot,
                          c29vsc31_plot,
                          #legend1,
                          nrow = 1,
                          ncol=3,
                          labels = c("A", "B", "C"))
carbon_super
##############################################################################
#saving figure
ggsave("Figures/carbon_super.pdf", carbon_super, width=21, height = 12, units = "in")
ggsave("Figures/C29vsC31.pdf", c29vsc31_plot)
ggsave("Figures/C29vsC31.pdf", c29vsc31_plot)
ggsave("Figures/C31plot.pdf", d13C31_plot)
