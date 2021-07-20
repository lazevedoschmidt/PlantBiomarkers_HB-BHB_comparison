##############################################################################
#Name: Lauren Azevedo Schmidt
#R version: 3.6.1
#Purpose: Normalized alkanes and hopanes
#date started: 04.16.2020
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
library(agricolae)
library(EnvStats)

#load data file----
avg_data <- read.csv("AVG_normalized_alkane_hopane.csv", header=T, sep=",", 
                     na.strings=  "NA", dec=".", strip.white=TRUE)

avg_data<- subset(avg_data,Epoch!="?") 
#subsetting the data so that any Epoch that is 
#unknown or denoted by a "?", is deleted from the data
avg_data$Sample.ID <- NULL #removing sample ID column

#Ordering legend properly
avg_data$Epoch <- factor(avg_data$Epoch, levels = c("Paleocene", "PETM", 
                                                          "Eocene"))
alk_data <- read.csv("Normalized_alkane_Hopane.csv", header=T, sep=",", 
         na.strings=  "NA", dec=".", strip.white=TRUE)
alk_data<- subset(alk_data,Epoch!="?") 
#subsetting the data so that any Epoch that is 
#unknown or denoted by a "?", is deleted from the data
alk_data$Sample.ID <- NULL #removing sample ID column

#Ordering legend properly
alk_data$Epoch <- factor(alk_data$Epoch, levels = c("Paleocene", "PETM", 
                                                    "Eocene"))

#data manipulation----
#all data
# alkhop_data <- alkhop_data %>%
#   dplyr::select(Basin, Epoch, Sum.C27.35.ODD, SUM.C27.35,
#                 Total.Hopanes, Sum.Hopanes.Sum.C27.C35alkanes)%>%
#   drop_na()

avg_data <- avg_data %>%
  dplyr::select(Basin, Epoch, AVG.C27.35.ODD, AVG.Hopanes,
                AVG.Hopanes.AVG.n.alk)%>%
  drop_na()

alk_data.summary <- alk_data %>%
  dplyr::select(Basin, Epoch, Sum.C27.35.ODD, Total.Hopanes,
                Sum.Hopanes.Sum.C27.C35alkanes)%>%
  drop_na()%>%
  group_by(Basin, Epoch)%>%
  summarise(mean.odd=mean(Sum.C27.35.ODD),
            sd.odd = sd(Sum.C27.35.ODD),
            mean.hop=mean(Total.Hopanes),
            sd.hop = sd(Total.Hopanes),
            mean.alk=mean(Sum.Hopanes.Sum.C27.C35alkanes),
            sd.alk = sd(Sum.Hopanes.Sum.C27.C35alkanes),
            count = n()) 
alk_data.summary$Epoch <- factor(alk_data.summary$Epoch, levels = c("Paleocene", "PETM", 
                                                    "Eocene"))

#odd box plots----
alk_data2 <- read.csv("Normalized_alkane_Hopanev2.csv", header=T, sep=",", 
                      na.strings=  "NA", dec=".", strip.white=TRUE)
alk_data2<- subset(alk_data2,Epoch!="?") 
#subsetting the data so that any Epoch that is 
#unknown or denoted by a "?", is deleted from the data
alk_data2$Sample.ID <- NULL #removing sample ID column
alk_data2$Epoch <- factor(alk_data2$Epoch, levels = c("Paleocene", "PETM", 
                                                    "Eocene"))

test <- rosnerTest(alk_data2$Sum.C27.35.ODD,
                   k = 10
)
test
alk_data2 <- alk_data2[c(-50,-47,-48,-43,-45,-44,-46,-42),]
alk_data2 %>%
  group_by(Basin, Epoch) %>% 
  summarise(COUNT = n())

bhbdata <- filter(alk_data2, Basin=='Bighorn Basin')
hbdata <- filter(alk_data2, Basin=='Hanna Basin')

#Tukey's test n-alkanes----
hblm <- lm(Sum.C27.35.ODD ~ Epoch, data = hbdata)
av <- aov(hblm)
summary(av)
test <- HSD.test(av, 'Epoch')
test
TukeyHSD(av) #pairwise tukey test

#Tukey's test nalkanes----
bhblm <- lm(Sum.C27.35.ODD ~ Epoch, data = bhbdata)
av <- aov(bhblm)
summary(av)
test <- HSD.test(av, 'Epoch')
test
TukeyHSD(av)

#comparing across basins----
paldata <- filter(alk_data2, Epoch=='Paleocene')
eocdata <- filter(alk_data2, Epoch=='Eocene')

baslm <- lm(Sum.C27.35.ODD ~ Basin, data=paldata)
basav <- aov(baslm)
summary(basav)
bastest <- HSD.test(basav, 'Basin')
bastest
TukeyHSD(basav)

baslm <- lm(Sum.C27.35.ODD ~ Basin, data=eocdata)
basav <- aov(baslm)
summary(basav)
bastest <- HSD.test(basav, 'Basin')
bastest
TukeyHSD(basav)

oddplot <- ggplot(alk_data2, aes(x = Basin, y =Sum.C27.35.ODD, fill = Epoch)) +
  geom_boxplot(alpha=0.75, outlier.shape = NA)+
  geom_jitter(position=position_jitterdodge(jitter.width = NULL,
                                            jitter.height = 0,
                                            dodge.width = 0.75,
                                            seed = NA), 
              size=1)+
  scale_x_discrete(drop=FALSE)+
  scale_fill_manual(values=palette2)+
                    # guide = guide_legend(order = 2)
  #ggtitle("Alkane changes across basins \nby Epoch")+
  labs(y=expression("Sum nC"[27-35](odd, µg/gC)), x=(""))+
  guides(fill = guide_legend(reverse = TRUE), drop=FALSE)+ #flips legend for geologic order
  theme_light()+
  theme(legend.position = c(.95, .95),
  legend.justification = c("right", "top"),
  legend.box.just = "right",
  legend.margin = margin(6, 6, 6, 6)) 
 
oddplot

#hopane plot----
alk_data2 <- read.csv("Normalized_alkane_Hopanev2.csv", header=T, sep=",", 
                      na.strings=  "NA", dec=".", strip.white=TRUE)
alk_data2<- subset(alk_data2,Epoch!="?") 
#subsetting the data so that any Epoch that is 
#unknown or denoted by a "?", is deleted from the data
alk_data2$Sample.ID <- NULL #removing sample ID column

test <- rosnerTest(alk_data2$Total.Hopanes,
                   k = 10
)
test
alk_data2 <-alk_data2[c(-21,-20,-51,-50,-19,-17,-53),]
alk_data2 %>%
  group_by(Basin, Epoch) %>% 
  summarise(COUNT = n())

bhbdata <- filter(alk_data2, Basin=='Bighorn Basin')
hbdata <- filter(alk_data2, Basin=='Hanna Basin')

#Tukey's test hopanes----
hblm <- lm(Total.Hopanes ~ Epoch, data = hbdata)
av <- aov(hblm)
summary(av)
test <- HSD.test(av, 'Epoch')
test
TukeyHSD(av)

#Tukey's test hopanes----
bhblm <- lm(Total.Hopanes ~ Epoch, data = bhbdata)
av <- aov(bhblm)
summary(av)
test <- HSD.test(av, 'Epoch')
test
TukeyHSD(av)

#comparing across basins----
paldata <- filter(alk_data2, Epoch=='Paleocene')
eocdata <- filter(alk_data2, Epoch=='Eocene')

baslm <- lm(Total.Hopanes ~ Basin, data=paldata)
basav <- aov(baslm)
summary(basav)
bastest <- HSD.test(basav, 'Basin')
bastest
TukeyHSD(basav)

baslm <- lm(Total.Hopanes ~ Basin, data=eocdata)
basav <- aov(baslm)
summary(basav)
bastest <- HSD.test(basav, 'Basin')
bastest
TukeyHSD(basav)

# alk_data2 <- read.csv("Normalized_alkane_Hopanev2.csv", header=T, sep=",", 
#                      na.strings=  "NA", dec=".", strip.white=TRUE)
# alk_data2<- subset(alk_data2,Epoch!="?") 
# #subsetting the data so that any Epoch that is 
# #unknown or denoted by a "?", is deleted from the data
# alk_data2$Sample.ID <- NULL #removing sample ID column
# #alk_data2 <-alk_data2[c(-21,-20,-51,-50,-19,-17,-53),]
# #Ordering legend properly
alk_data2$Epoch <- factor(alk_data2$Epoch, levels = c("Paleocene", "PETM", 
                                                    "Eocene"))

hopplot <- ggplot(alk_data2, aes(x = Basin, y =Total.Hopanes, fill = Epoch)) +
  geom_boxplot(alpha=0.75, outlier.shape = NA)+
  geom_jitter(position=position_jitterdodge(jitter.width = NULL,
                                            jitter.height = 0,
                                            dodge.width = 0.75,
                                            seed = NA), 
              size=1)+
scale_fill_manual(values=palette2)+
  # guide = guide_legend(order = 2)
  ggtitle("Hopane changes across basins \nby Epoch")+
  labs(y=expression("Sum Hopanes (µg/gC) "), x=(""))+
  guides(fill = guide_legend(reverse = TRUE), drop=FALSE)+ #flips legend for geologic order
  theme_light()+
  theme(legend.position = "none")+
  scale_x_discrete(drop=FALSE)

hopplot

#bacteria figures----
alk_data2 <- read.csv("Normalized_alkane_Hopanev2.csv", header=T, sep=",", 
                      na.strings=  "NA", dec=".", strip.white=TRUE)
alk_data2<- subset(alk_data2,Epoch!="?") 
#subsetting the data so that any Epoch that is 
#unknown or denoted by a "?", is deleted from the data
alk_data2$Sample.ID <- NULL #removing sample ID column

test <- rosnerTest(alk_data2$Sum.Hopanes.Sum.C27.C35alkanes,
                   k = 10
)
test
alk_data <-alk_data[c(-33,-21,-6,-20),]

alk_data2 %>%
  group_by(Basin, Epoch) %>% 
  summarise(COUNT = n())

# alk_data2 <- read.csv("Normalized_alkane_Hopanev2.csv", header=T, sep=",", 
#                       na.strings=  "NA", dec=".", strip.white=TRUE)
# alk_data2<- subset(alk_data2,Epoch!="?") 
# #subsetting the data so that any Epoch that is 
# #unknown or denoted by a "?", is deleted from the data
# alk_data2$Sample.ID <- NULL #removing sample ID column
# #alk_data2 <-alk_data2[c(-33,-21,-6,-20),]
alk_data2$Epoch <- factor(alk_data2$Epoch, levels = c("Paleocene", "PETM", 
                                                      "Eocene"))

bhbdata <- filter(alk_data2, Basin=='Bighorn Basin')
hbdata <- filter(alk_data2, Basin=='Hanna Basin')

#Tukey's test bacteria----
hblm <- lm(Sum.Hopanes.Sum.C27.C35alkanes ~ Epoch, data = hbdata)
av <- aov(hblm)
summary(av)
test <- HSD.test(av, 'Epoch')
test
TukeyHSD(av)

#Tukey's test bacteria----
bhblm <- lm(Sum.Hopanes.Sum.C27.C35alkanes ~ Epoch, data = bhbdata)
av <- aov(bhblm)
summary(av)
test <- HSD.test(av, 'Epoch')
test
TukeyHSD(av)

#comparing across basins----
paldata <- filter(alk_data2, Epoch=='Paleocene')
eocdata <- filter(alk_data2, Epoch=='Eocene')

baslm <- lm(Sum.Hopanes.Sum.C27.C35alkanes ~ Basin, data=paldata)
basav <- aov(baslm)
summary(basav)
bastest <- HSD.test(basav, 'Basin')
bastest
TukeyHSD(basav)

baslm <- lm(Sum.Hopanes.Sum.C27.C35alkanes ~ Basin, data=eocdata)
basav <- aov(baslm)
summary(basav)
bastest <- HSD.test(basav, 'Basin')
bastest
TukeyHSD(basav)

bacplot <-ggplot(alk_data2, aes(x = Basin, y =Sum.Hopanes.Sum.C27.C35alkanes, 
                               fill = Epoch)) +
  geom_boxplot(alpha=0.75, outlier.shape = NA)+
  geom_jitter(position=position_jitterdodge(jitter.width = NULL,
                                            jitter.height = 0,
                                            dodge.width = 0.75,
                                            seed = NA), 
              size=1)+
  scale_x_discrete(drop=FALSE)+
  scale_fill_manual(values=palette2)+
  # guide = guide_legend(order = 2)
  ggtitle("Bacterial input relative \nto alkanes")+
  labs(y=expression("Sum Hopanes/Sum nC"[27-35] (odd)), x=(""))+
  guides(fill = guide_legend(reverse = TRUE), drop=FALSE)+ #flips legend for geologic order
  theme_light()+
  theme(legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))+
  theme(legend.position = "none")
  
bacplot

#plotting all together----
super_plot <- plot_grid(oddplot,
              # allplot,
              hopplot,
              bacplot,
              #legend.alk,
              nrow = 1,
              ncol=3,
              labels = c("A", "B", "C"))
super_plot

#saving figure----
ggsave("Figures/nalkane.pdf", super_plot)
ggsave("Figures/0ddnalkane.pdf", oddplot, units = "in", width = 6.5, scale = 1)
