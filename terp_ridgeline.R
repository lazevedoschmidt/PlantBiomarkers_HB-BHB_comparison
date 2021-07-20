#ridgeplots----
# library
library(ggridges)
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(ggnewscale)

#load data with all data. 
ridgedata <- read.csv("terp_lgfrm_data.csv", header=TRUE)
#Ordering legend properly
ridgedata$Epoch <- factor(ridgedata$Epoch, levels = c("Paleocene", "PETM", "Eocene"))

ridgedata<- subset(ridgedata,Epoch!="?")
ridgedata <- subset(ridgedata, values!=0.00)
diridge <- subset(ridgedata, ID!="Total Triterp")

hbdata <- subset(diridge, Basin=="Hanna Basin")
bhbdata <- subset(diridge, Basin=="Bighorn Basin")

# Plot
ridge <- ggplot(ridgedata, aes(x = values, y = Basin, fill = ..x..)) +
  geom_density_ridges_gradient(scale=1.5, rel_min_height =0.01) +
  theme_ridges()+
  ylab("")+
  theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(~ID)
ridge

#only diterp
HBdiridge <- ggplot(hbdata, aes(x = values, y = Epoch, fill = Epoch)) +
  geom_density_ridges_gradient(scale=0.9, rel_min_height =0.01) +
  theme_ridges()+
  ylab("")+
  theme(plot.title = element_text(hjust = 0.5))
HBdiridge
