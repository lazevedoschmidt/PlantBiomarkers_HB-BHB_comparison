require(tidyverse)
require(ggplot2)
library(EnvStats)

#load data file
data <- read.csv("terp_lgfrm_data.csv", header=TRUE)
#Ordering legend properly
data$Epoch <- factor(data$Epoch, levels = c("Paleocene", "PETM", "Eocene"))
data<- subset(data,Epoch!="?")

data2 <- read.csv("terp_lgfrm_data_lean.csv", header=TRUE)
#Ordering legend properly
data2$Epoch <- factor(data2$Epoch, levels = c("Paleocene", "PETM", "Eocene"))
data2<- subset(data2,Epoch!="?")

terp.summary <- data %>%
  group_by(Basin, Epoch, ID) %>%
  summarise(mean.value= mean(values),
            sd.value = sd(values), 
            count = n(),
            se.mean = sd.value/sqrt(count)) 
terp.summary

wrap.labs <- c("Diterpenoids"= "Sum Diterpenoids", 
               "Triterpenoids" = "Sum Triterpenoids")

#Keeping outliers!!! 
# outlier.terp <- ggplot(data, aes(x=Basin, y=values, fill=Epoch)) +
#   geom_boxplot(alpha=0.75, outlier.shape = NA)+
#   geom_jitter(position=position_jitterdodge(jitter.width = NULL,
#                                             jitter.height = 0,
#                                             dodge.width = 0.75,
#                                             seed = NA), 
#               size=1)+
#   xlab("")+
#   ylab("Average Terpenoid")+
#   scale_fill_manual(values=palette2,
#                     guide = guide_legend(order = 2))+
#   guides(fill = guide_legend(reverse = TRUE))+
#   theme_light()+
#   theme(legend.position = "none")+ #had to remove legend for new figure
#   facet_wrap(~ID)
# #,nrow=1,labeller = labeller(ID = wrap.labs)
# #above was added to facet_wrap but not really sure why I had them in there. 
# outlier.terp

# x <- subset(test.summary,ID=="Total Diterp" | ID=="Total Triterp")
#dealing with outliers
test <- rosnerTest(data$values,
                   k = 10
)
test
data <- data[c(-44,-92,-5, -103, -1, -47,-89,-30,-87,-2),] #rerun test
data <- data[c(-82,-18,-92,-80,-63,-81,-66,-85,-22,-16),] #rerun test
data <- data[c(-16,-23,-63,-37,-26),] #rerun test
data <- data[c(-70,-40,-55,-11,-20,-59,-61,-77,-56,-39),] #rerun test
data <- data[c(-3,-52,-7,-55,-19),] #rerun test

data %>%
  group_by(Basin, Epoch, ID) %>% 
  summarise(COUNT = n())

avg_terp_error <- ggplot(data, aes(x=Basin, y=values, fill=Epoch)) +
  geom_boxplot(alpha=0.75, outlier.shape = NA)+
  geom_jitter(position=position_jitterdodge(jitter.width = NULL,
                                            jitter.height = 0,
                                            dodge.width = 0.75,
                                            seed = NA), 
              size=1)+
  xlab("")+
  ylab("Sum Terpenoid (µg/gC )")+
  scale_fill_manual(values=palette2,
                    guide = guide_legend(order = 2))+
  guides(fill = guide_legend(reverse = TRUE))+
  theme_light()+
  theme(legend.position = "none")+ #had to remove legend for new figure
  facet_wrap(~ID)
avg_terp_error

#statistical tests----
#testing within basin----
bhbdatadi <- filter(data2, Basin=='Bighorn Basin', ID=="Diterpenoids")
bhbdatatri <- filter(data2, Basin=='Bighorn Basin', ID=="Triterpenoids")
hbdatadi <- filter(data2, Basin=='Hanna Basin',ID=="Diterpenoids")
hbdatatri <- filter(data2, Basin=='Hanna Basin',ID=="Triterpenoids")

#Tukey's test diterp----
hblm <- lm(values ~ Epoch, data = hbdatadi)
av <- aov(hblm)
summary(av)
test <- HSD.test(av, 'Epoch')
test
TukeyHSD(av)

hblm <- lm(values ~ Epoch, data = hbdatatri)
av <- aov(hblm)
summary(av)
test <- HSD.test(av, 'Epoch')
test
TukeyHSD(av)

bhblm <- lm(values ~ Epoch, data = bhbdatadi)
av <- aov(bhblm)
summary(av)
test <- HSD.test(av, 'Epoch')
test
TukeyHSD(av)

bhblm <- lm(values ~ Epoch, data = bhbdatatri)
av <- aov(bhblm)
summary(av)
test <- HSD.test(av, 'Epoch')
test
TukeyHSD(av)

#comparing across basins----
#paleo diterp
paldatadi <- filter(data2, Epoch=='Paleocene', ID=="Diterpenoids")
test <- rosnerTest(paldatadi$values,
                   k = 10
)
test
paldatadi <- paldatadi[c(-10,-3,-5,-1,-2,-9,-13,-7),] #rerun test
#paleo triterp
paldatatri <- filter(data2, Epoch=='Paleocene', ID=="Triterpenoids")
test <- rosnerTest(paldatatri$values,
                   k = 10
)
test
paldatatri <- paldatatri[c(-7,-18,-4,-2,-5,-16,-1,-3,-9,-6),] #rerun test
paldatatri <- paldatatri[c(-7,-2),]
#eocene diterp
eocdatadi <- filter(data2, Epoch=='Eocene', ID=="Diterpenoids")
test <- rosnerTest(eocdatadi$values,
                   k = 10
)
test
eocdatadi <- eocdatadi[c(-18,-5,-1,-20,-2,-19,-26,-13,-25,-6),]
eocdatadi <- eocdatadi[c(-6,-7,-5,-12,-11,-2,-10),]
#eocene tri
eocdatatri <- filter(data2, Epoch=='Eocene', ID=="Triterpenoids")
test <- rosnerTest(eocdatatri$values,
                   k = 10
)
test
eocdatatri <- eocdatatri[c(-2,-5,-7,-1,-8,-10,-3,-6,-11),]

#Tukey's test paleo diterp ----
paldilm <- lm(values ~ Basin, data = paldatadi)
av <- aov(paldilm)
summary(av)
test <- HSD.test(av, 'Basin')
test
TukeyHSD(av)

paltrilm <- lm(values ~ Basin, data = paldatatri)
av <- aov(paltrilm)
summary(av)
test <- HSD.test(av, 'Basin')
test
TukeyHSD(av)

eocdilm <- lm(values ~ Basin, data = eocdatadi)
av <- aov(eocdilm)
summary(av)
test <- HSD.test(av, 'Basin')
test
TukeyHSD(av)

eoctrilm <- lm(values ~ Basin, data = eocdatatri)
av <- aov(eoctrilm)
summary(av)
test <- HSD.test(av, 'Basin')
test
TukeyHSD(av)

#Saving figure----
ggsave("Figures/terp_bar_avg_error.pdf", avg_terp_error)
