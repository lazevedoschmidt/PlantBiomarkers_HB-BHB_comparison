##############################################################################
#Name: Lauren Azevedo Schmidt
#R version: 3.6.1
#Purpose: diterp data
#date started: 04.29.20
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
require(EnvStats)
require(ggforce)
require(ggrepel)

#load data file
terp_data <- read.csv("terp_alk_lgfrm_data.csv", header=TRUE)
#Ordering legend properly
terp_data$Epoch <- factor(terp_data$Epoch, levels = c("Paleocene", "PETM", "Eocene"))
#removing unneeded variables
terp_data <- terp_data[!(terp_data$ID=="ACL" | terp_data$ID=="Sum C27:35 ODD" | terp_data$ID=="SUM C27:36"),]
#pie colors
piecolors <- c("#15616D","#78290F","#FFA047")

#sum and percentage by Basin and ID 
piehb <- subset(terp_data,Basin=="Hanna Basin")
piehb <- piehb %>%
  group_by(Basin,ID) %>%
  summarise(total=sum(values, na.rm = T),
            count = n())%>%
  mutate(pct_tot = total/sum(total))
piehb$labels <- scales::percent(piehb$pct_tot)

piebhb <- subset(terp_data,Basin=="Bighorn Basin")
piebhb <- piebhb %>%
  group_by(Basin,ID) %>%
  summarise(total=sum(values, na.rm = T),
            count = n())%>%
  mutate(pct_tot = total/sum(total))
piebhb$labels <- scales::percent(piebhb$pct_tot)

# HB piechart
HB_pie <- ggplot(piehb, aes(x="", y=total, fill=ID)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0)+
  geom_text_repel(aes(x = 1.4, y = total, label = labels), 
                  nudge_x = .3, 
                  segment.size = .7, 
                  show.legend = FALSE) +
  scale_fill_manual(values=piecolors)+
  # scale_fill_manual(values=wes_palette(n=3, name="Chevalier1"),
  #                   guide = guide_legend(order = 2))+
  theme_void()+ # remove background, grid, numeric labels
  theme(legend.position="none")+
  ggtitle("Hanna Basin")+
  theme(plot.title = element_text(hjust = 0.5))
HB_pie

BHB_pie <- ggplot(piebhb, aes(x="", y=total, fill=ID)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0)+
  geom_text_repel(aes(x = 1.4, y = total, label = labels), 
                  nudge_x = .3, 
                  segment.size = .7, 
                  show.legend = FALSE) +
  scale_fill_manual(values=piecolors)+
  # scale_fill_manual(values=wes_palette(n=3, name="Chevalier1"),
  #                   guide = guide_legend(order = 2))+
  theme_void()+ # remove background, grid, numeric labels
  #theme(legend.position="none")+
  guides(fill=guide_legend(title=""))+
  ggtitle("Bighorn Basin")+
  theme(legend.position="bottom")+
  theme(plot.title = element_text(hjust = 0.5))
#############################################################################
#plotting pie charts together 
super_pie <- plot_grid(HB_pie,
                       BHB_pie, 
                       nrow=2, 
                       ncol=1)
super_pie
###############################################################################
#saving
ggsave("Figures/piechart.pdf", super_pie)

