##############################################################################
#Name: Lauren Azevedo Schmidt
#R version: 3.6.1
#Purpose: TOC across basins
#date started: 04.08.2020
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


#load data file
TOC_data <- read.csv("TOC data.csv", header=T, sep=",", 
                  na.strings=  "NA", dec=".", strip.white=TRUE)

TOC_data<- subset(TOC_data,Epoch!="?") #subsetting the data so that any Epoch that is 
#unknown or denoted by a "?", is deleted from the data
TOC_data$Sample.ID <- NULL #removing sample ID column
#cleaning dataframes
TOC_data<- TOC_data %>%
  rename(TOC = Wt....TOC)

#Ordering legend properly
TOC_data$Epoch <- factor(TOC_data$Epoch, levels = c("Paleocene", "PETM", "Eocene"))

##############################################################################
#all data
TOC_data <- TOC_data %>%
  dplyr::select(Basin, Epoch, TOC)%>%
  drop_na()  
TOC_data<- subset(TOC_data,Epoch!="") #subsetting to make sure there are no blanks

#counting data length()
BHB_TOC <- subset(TOC_data,Basin!="Hanna Basin")
HB_TOC <- subset(TOC_data,Basin!="Bighorn Basin")

table(HB_TOC$Epoch) #counts each group
table(BHB_TOC$Epoch)
##############################################################################
# Basic box plot
#d13C TOC
TOC.p <- TOC_data %>%
  mutate(name = fct_relevel(Epoch, 
                            "Paleocene", "PETM", "Eocene")) %>%
  ggplot(aes(x = Epoch, y = TOC, shape=Basin)) + 
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
  labs(x="", y="TOC")+
  ylim(0,60)+
  ggtitle("Total Organic Carbon Changes across Basins")+
  theme_light()
  # theme(
  #   legend.position = c(.95, .95),
  #   legend.justification = c("right", "top"),
  #   legend.box.just = "right",
  #   legend.margin = margin(6, 6, 6, 6)
  # ) #Legend sits within the plot using the above code. delete if not desired
  #guides(color=FALSE, fill=FALSE, shape=FALSE) #use this to delete legend
  
TOC.p

#Adding pvalues 
df_pair_TOC_total <- pairwiseComparisons::pairwise_comparisons(
  data = TOC_data,
  x = Epoch,
  y = TOC,
  type = "parametric",
  var.equal = TRUE,
  paired = FALSE,
  p.adjust.method = "bonferroni")

# adding a geom for pairwise comparisons
TOC.p <-ggstatsplot:::ggsignif_adder(
  plot = TOC.p,
  data =TOC_data,
  x = Epoch,
  y = TOC,
  df_pairwise = df_pair_TOC_total)
TOC.p
TOC.p <- TOC.p +stat_compare_means(aes(group = Basin), label.y = 60, 
                                         method = "anova") 
TOC.p
############################################################################
#TOC x axis as basin
TOC_plot_basinx<- TOC_data %>%
  mutate(name = fct_relevel(Epoch, 
                            "Paleocene", "PETM", "Eocene")) %>%
  ggplot(aes(x = Basin, y = TOC, shape=Epoch)) + 
  guides(fill = guide_legend(reverse = TRUE))+ #flips legend for geologic order
  geom_boxplot(aes(fill = Epoch), alpha=0.75, outlier.shape = NA)+
  geom_jitter(position=position_jitterdodge(jitter.width = NULL,
                                            jitter.height = 0,
                                            dodge.width = 0.75,
                                            seed = NA), 
              size=3, 
              aes(color = Epoch))+
  scale_fill_manual(values=wes_palette(n=3, name="IsleofDogs1"),
                    guide = guide_legend(order = 2)) +
  scale_color_manual(values=wes_palette(n=3, name="IsleofDogs1")) +
  #scale_x_discrete(limits=c("Paleocene","PETM","Eocene"))+
  labs(x="", y="TOC")+
  ylim(-5,60)+
  ggtitle("Total Organic Carbon Changes across Basins")+
  theme_light()
TOC_plot_basinx
##############################################################################
#bar graphs
#creating sd to add to barplot
palette2 <- c("#0F0D0E","#D9D0D3","#CCBA72")
df <- TOC_data %>% 
  group_by(Basin,Epoch) %>%
  summarize(Mean=mean(TOC,na.rm=T),SD=sd(TOC,na.rm=T))

TOC_bar <- ggplot(df, aes(x=Basin,y=Mean,fill=Epoch)) +
  geom_bar(position="dodge",stat="identity",color="black") +
  geom_errorbar(aes(ymin=Mean-SD,ymax=Mean+SD),width=0.1, size=0.3,
                position=position_dodge(0.9))+
  scale_fill_manual(values=palette2,
                    guide = guide_legend(order = 2), drop=FALSE)+
  ggtitle("Total Organic Carbon Changes across Basins")+
  labs(y="TOC (mean)")+
  guides(fill = guide_legend(reverse = TRUE))+ #flips legend for geologic order
  theme_light()
TOC_bar

#############################################################################
stat.test.basin <- compare_means(
  TOC ~ Epoch, data = TOC_data, group.by = "Basin",
  method = "t.test")
stat.test.basin
write_csv(stat.test.basin,"stat.test.basin.csv")

stat.test.epoch <- compare_means(
  TOC ~ Basin, data = TOC_data, group.by = "Epoch",
  method = "t.test")
stat.test.epoch
write_csv(stat.test.epoch,"stat.test.epoch.csv")

# bp <- ggbarplot(TOC_data, x = "Basin", y = "TOC",
#                 fill = "Epoch", palette = c("#9986A5","#79402E","#CCBA72"),
#                 add = "mean_sd", add.params = list(group = "Epoch"),
#                 position = position_dodge(0.8))+
#       guides(fill = guide_legend(reverse = TRUE))
# bp
#not sure how to add stat.test to bp
#############################################################################
#saving figure
ggsave("Figures/TOC.pdf", TOC.p, units = "in", width = 6.5, 
       scale = 1)
ggsave("Figures/TOC_basinx.pdf", TOC_plot_basinx, units = "in", width = 6.5, 
       scale = 1)
ggsave("Figures/TOC_bar.pdf", TOC_bar, units = "in", width = 6.5, 
       scale = 1)




