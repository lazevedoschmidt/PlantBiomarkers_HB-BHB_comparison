#PAQ Data analysis
require(ggplot2)
require(tidyverse)
require(stats)
require(ggsignif)
require(ggpubr)
require(ggpmisc)
require(dplyr)
require(scales)
library(agricolae)
library(EnvStats)


#load data file
paq <- read.table("PAQ_data.csv", header=T, sep=",", 
                        na.strings= "NA", dec=".", strip.white=TRUE)
paq <- paq[c(-66:-138),] #removing random empty rows

#testing for outliers
test <- rosnerTest(paq$PAQ,
                   k = 10
)
test #no outliers

paq %>%
  group_by(Basin, Epoch) %>% 
  summarise(COUNT = n())

#making basins specific dataframe for PAQ comparison across time
bhbpaq <- filter(paq, Basin=='Bighorn Basin')
hbpaq <- filter(paq, Basin=='Hanna Basin')

#Tukey test
bhblm <- lm(PAQ ~ Epoch, data = bhbpaq)
bhbav <- aov(bhblm)
summary(bhbav)
test <- HSD.test(bhbav, 'Epoch')
test
TukeyHSD(bhbav) #pairwise tukey test

hblm <- lm(PAQ ~ Epoch, data = hbpaq)
hbav <- aov(hblm)
summary(hbav)
test <- HSD.test(hbav, 'Epoch')
test
TukeyHSD(hbav) #pairwise tukey test
#Tukey shows HB Paleocene-PETM (a), Eocene (b)

#reloading data with BHB PETM NAs
paq <- read.table("PAQ_datav2.csv", header=T, sep=",", 
                  na.strings= "NA", dec=".", strip.white=TRUE)
paq <- paq[c(-66:-138),] #removing random empty rows
#Ordering legend properly
paq$Epoch <- factor(paq$Epoch, 
                          levels = c("Paleocene", "PETM", "Eocene"))
# Scatterplot with Basin as shape
paq_plot <- ggplot(paq, aes(x=Basin, y=PAQ, fill= Epoch)) +
  geom_boxplot(alpha=0.75, outlier.shape = NA)+
  geom_jitter(position=position_jitterdodge(jitter.width = NULL,
                                            jitter.height = 0,
                                            dodge.width = 0.75,
                                            seed = NA), size=1)+
  scale_x_discrete(drop=FALSE)+
  ylab("PAQ")+
  xlab("")+
  scale_fill_manual(values=palette2, guide = guide_legend(reverse = TRUE))+
  geom_hline(yintercept=0.4, linetype="dashed")+ #under 1 is anoxic
  theme_light()+
  theme(legend.position="none") #removes legend
paq_plot

#saving figure----
ggsave("Figures/PAQ.pdf", paq_plot)
