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
diterp_dat <- read.table("diterp_data.csv", 
                        header=T, sep=",", 
                        na.strings=  "NA", dec=".", strip.white=TRUE) 

diterp_dat<- subset(diterp_dat,Epoch!="?") 

#dealing with NAs
diterp_dat <- diterp_dat %>%
  drop_na()

#Ordering legend properly
diterp_dat$Epoch <- factor(diterp_dat$Epoch, levels = c("Paleocene", "PETM", "Eocene"))

terp_data <- read.csv("terp_alk_lgfrm_data.csv", header=TRUE)
#Ordering legend properly
terp_data$Epoch <- factor(terp_data$Epoch, levels = c("Paleocene", "PETM", "Eocene"))

###############################################################################
#bargraph using facet_wrap
facet_bar <- ggplot(terp_data[terp_data$ID %in% c("Total Diterp", "Total Triterp"),]) +
  geom_col(aes(x = Basin, y = values, fill = Epoch), position = "dodge") +
  scale_x_discrete(drop=FALSE)+
  scale_fill_manual(values=palette2,
                    guide = guide_legend(order = 2))+
  labs(x="", y="Total Terpenoids")+
  guides(fill = guide_legend(reverse = TRUE))+ #flips legend for geologic order
  theme_light()+
  facet_wrap(~ID) #tell it to keep PETM in BHB
facet_bar

###############################################################################
#bar graph
tot_diterp <- ggplot(diterp_dat) +
  geom_col(aes(x = Basin, y = Total.diterpenoids, fill = Epoch), position = "dodge") +
  scale_fill_manual(values=palette2)+
                    #guide = guide_legend(order = 2))+
  labs(x="", y="Total Diterpenoids")+
  guides(fill = guide_legend(reverse = TRUE))+ #flips legend for geologic order
  theme_light()
  #theme(legend.position = "none")
tot_diterp

ex_diterp <- ggplot(diterp_dat) +
  geom_col(aes(x = Basin, y = Total.diterpenoids..excluding.tetracylic.compounds., 
               fill = Epoch), position = "dodge") +
  scale_fill_manual(values=palette2)+
                    #guide = guide_legend(order = 2))+
  labs(x="", y="Total Diterpenoids \n(excluding tetracyclic compounds)")+
  guides(fill = guide_legend(reverse = TRUE))+ #flips legend for geologic order
  theme_light()

#extracting legend
legend_diterp <- get_legend(ex_diterp)

ex_diterp <- ggplot(diterp_dat) +
  geom_col(aes(x = Basin, y = Total.diterpenoids..excluding.tetracylic.compounds., 
               fill = Epoch), position = "dodge") +
  scale_fill_manual(values=palette2)+
                      #guide = guide_legend(order = 2))+
  labs(x="", y="Total Diterpenoids \n(excluding tetracyclic compounds)")+
  guides(fill = guide_legend(reverse = TRUE))+ #flips legend for geologic order
  theme_light()+
  theme(legend.position = "none")
ex_diterp
############################################################################
#putting both together
diterp_plot <- plot_grid(tot_diterp,
          legend_diterp,
          ex_diterp,
          nrow = 2,
          ncol=2)
diterp_plot
##############################################################################
#pie
#colors
piecolors <- c("#15616D","#78290F","#FFA047")


#sum and percentage by Basin and ID 
pie <- terp_data %>%
  group_by(Basin,ID) %>%
  summarise(total=sum(values, na.rm = T),
            count = n())%>%
  mutate(pct_tot = total/sum(total))
pie$labels <- scales::percent(pie$pct_tot)

#removing unused rows
pie <- pie[!(pie$ID=="ACL" | pie$ID=="Sum C27:35 ODD" | pie$ID=="SUM C27:36"),]
pieBHB<- subset(pie,Basin!="Hanna Basin")
pieHB<- subset(pie,Basin!="Bighorn Basin")

# HB piechart
HB_pie <- ggplot(pieHB, aes(x="", y=total, fill=ID)) +
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

BHB_pie <- ggplot(pieBHB, aes(x="", y=total, fill=ID)) +
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
  ggtitle("Bighorn Basin")+
  theme(plot.title = element_text(hjust = 0.5))

#extract legend
pie_legend <- get_legend(BHB_pie)

BHB_pie <-ggplot(pieBHB, aes(x="", y=total, fill=ID)) +
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
  ggtitle("Bighorn Basin")+
  theme(plot.title = element_text(hjust = 0.5)) 
BHB_pie
#############################################################################
#plotting pie charts together 
super_pie <- plot_grid(HB_pie,
                       pie_legend,
                       BHB_pie, 
                       nrow=2, 
                       ncol=2)
super_pie
###############################################################################
#saving
ggsave("Figures/diterp.pdf", diterp_plot, units = "in", width = 6.5, 
       scale = 1)
ggsave("Figures/piechart.pdf", super_pie)
ggsave("Figures/terp_bar.pdf", facet_bar)

