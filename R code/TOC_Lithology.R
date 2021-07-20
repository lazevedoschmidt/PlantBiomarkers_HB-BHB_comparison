#Lauren Azevedo Schmidt
#Purpose: TOC data of HB and BHB based on lithology
#date: 07.30.2020
#R: 3.6.1
##############################################################################
library(ggplot2)
library(hrbrthemes)
library(tidyverse)
library(ggalluvial)
library(viridis)
library(colorRamps)
library(RColorBrewer)
library(wesanderson)
library(agricolae)
library(egg)
library(EnvStats)

#load data file
TOC_data <- read.csv("TOC data_updatedV2.csv", header=T, sep=",", 
                     na.strings=  "NA", dec=".", strip.white=TRUE)
TOC_data<- subset(TOC_data,Epoch!="?") #subsetting the data so that any Epoch that is 
#unknown or denoted by a "?", is deleted from the data
TOC_data$Sample.ID <- NULL #removing sample ID column
#cleaning dataframes
TOC_data<- TOC_data %>%
  rename(TOC = Wt....TOC)

#Ordering legend properly
TOC_data$Epoch <- factor(TOC_data$Epoch, levels = c("Paleocene", "PETM", "Eocene"))

#Reordering lithology so we can alter colors later on
TOC_data$Lithology <- factor(TOC_data$Lithology,
                             levels = c("claystone", "carbonaceous facies",
                                        "siltstone", "sandstone"))

test <- rosnerTest(TOC_data$TOC,
                   k = 10
)
test

#remove outliers
TOC_data <- TOC_data[c(-308,-77,-66,-80,-75,-265,-294,-64,-306,-73),]
#rerun lines 36-39
TOC_data <- TOC_data[c(-79,-100,-149,-259,-8,-148,-87,-317,-257,-258),]
#rerun lines 36-39
TOC_data <- TOC_data[c(-280,-283,-288,-250,-86,-285,-247,-83,-308,-67),]
#rerun lines 36-39
TOC_data <- TOC_data[c(-85,-243,-278,-80,-99,-57,-6,-481,-78,-274),]
#rerun lines 36-39
TOC_data <- TOC_data[c(-10,-101,-4,-95,-54,-269,-271,-289,-310,-8),]
#rerun lines 36-39
TOC_data <- TOC_data[c(-91,-414,-300,-59,-251,-426,-249,-89,-6,-262),]
#rerun lines 36-39
TOC_data <- TOC_data[c(-452,-229,-100,-329,-404,-407),]

TOC_data %>%
  group_by(Basin, Epoch) %>% 
  summarise(COUNT = n())

#tukey tests
paldata <- filter(TOC_data, Epoch=="Paleocene")
petmdata <- filter(TOC_data, Epoch=="PETM")
eocdata <- filter(TOC_data, Epoch=="Eocene")

#all data test
lm <- lm(TOC ~Lithology + Epoch, data=TOC_data)
av <- aov(lm)
summary(av)

pallm <- lm(TOC~Lithology + Basin, data = paldata)
palav <- aov(pallm)
summary(palav)
TukeyHSD(palav)

#Lm without +Basin
pallm <- lm(TOC ~ Lithology, data = paldata)
av <- aov(pallm)
summary(av)
test <- HSD.test(av, 'Lithology')
test
TukeyHSD(av)

petmlm <- lm(TOC~Lithology+Basin, data = petmdata)
petmav <- aov(petmlm)
summary(petmav)

#Tukey's test PETM----
petmlm <- lm(TOC ~ Lithology, data = petmdata)
av <- aov(petmlm)
summary(av)
test <- HSD.test(av, 'Lithology')
test
TukeyHSD(av)

#Tukey test Eocene----
eoclm <- lm(TOC~Lithology, data = eocdata)
eocav <- aov(eoclm)
summary(eocav)
test <- HSD.test(eocav, 'Lithology')
test
TukeyHSD(eocav)

#carb facies only
carb <- filter(TOC_data, Lithology=="carbonaceous facies")
carblm <- lm(TOC~Epoch+Basin, data = carb)
carbav <- aov(carblm)
summary(carbav)
test <- HSD.test(carbav, 'Epoch')
test
TukeyHSD(carbav)

#use for count data. 
lith <- TOC_data %>%
  group_by(Basin,Epoch,Lithology)%>%
  summarise(mean=mean(TOC),
            sd.value = sd(TOC),
            count = n(),
            se.mean = sd.value/sqrt(count))

#Boxplot by time, lithology and basin----
TOC_data <- read.csv("TOC data_updated.csv", header=T, sep=",", 
                     na.strings=  "NA", dec=".", strip.white=TRUE)
TOC_data<- subset(TOC_data,Epoch!="?") #subsetting the data so that any Epoch that is 
#unknown or denoted by a "?", is deleted from the data
TOC_data$Sample.ID <- NULL #removing sample ID column
#cleaning dataframes
TOC_data<- TOC_data %>%
  rename(TOC = Wt....TOC)
#removing outliers
test <- rosnerTest(TOC_data$TOC,
                   k = 10)
test

#remove outliers
TOC_data <- TOC_data[c(-315,-84,-73,-87,-82,-272,-301,-71,-311,-80),]
TOC_data <- TOC_data[c(-304,-86,-107,-156,-266,-8,-155,-94,-324,-264),]
TOC_data <- TOC_data[c(-258,-288,-291,-295,-257,-93,-293,-254,-90,-315),]
TOC_data <- TOC_data[c(-74,-93,-251,-286,-88,-107,-64,-6,-488,-86),]
TOC_data <- TOC_data[c(-274,-10,-108,-4,-102,-61,-277,-278,-296,-317),]
TOC_data <- TOC_data[c(-7,-99,-421,-307,-67,-259,-433,-257,-97,-6),]
TOC_data <- TOC_data[c(-263,-459,-236,-107,-366,-411,-414,-117),]
TOC_data <- TOC_data[-332,]
#Ordering legend properly
TOC_data$Epoch <- factor(TOC_data$Epoch, levels = c("Paleocene", "PETM", "Eocene"))

#Reordering lithology so we can alter colors later on
TOC_data$Lithology <- factor(TOC_data$Lithology,
                             levels = c("claystone", "carbonaceous facies",
                                        "siltstone", "sandstone"))
Lith.TOC <-ggplot(TOC_data, aes(fill=Lithology, y=TOC, x=Basin)) +
  geom_boxplot(alpha=0.75, outlier.shape = NA)+
  geom_jitter(position=position_jitterdodge(jitter.width = NULL,
                                            jitter.height = 0,
                                            dodge.width = 0.75,
                                            seed = NA), 
              size=1)+
  facet_wrap(~Epoch, drop=FALSE) +
  scale_fill_manual(values=updatedcolors, drop=FALSE) +
  #ggtitle("Total Organic Carbon by Lithology") +
  xlab("")+
  ylab("Total Organic Carbon (TOC)")+
  theme_light()+
  theme(legend.position="bottom",plot.title = element_text(hjust = 0.5), 
        legend.title = element_blank()) +
  guides(fill=guide_legend(nrow=1), drop=FALSE)
Lith.TOC


#Save figure----
ggsave("Figures/TOC.Lithology.pdf", Lith.TOC)  
