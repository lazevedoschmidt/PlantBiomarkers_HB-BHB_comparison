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

###############################################################################
diterp_dat %>%
  mutate(name = fct_relevel(Epoch, 
                            "Paleocene", "Eocene")) %>%
  ggplot(aes(x = Epoch, y = Total.diterpenoids, 
             shape = Basin)) + 
  geom_boxplot(aes(fill = Basin), alpha=0.75, outlier.shape = NA)+
  geom_jitter(position=position_jitterdodge(jitter.width = NULL,
                                            jitter.height = 0,
                                            dodge.width = 0.75,
                                            seed = NA), 
              size=3, 
              aes(color = Basin))+
  scale_fill_manual(values=wes_palette(n=2, name="Moonrise2"),
                    guide = guide_legend(order = 2)) +
  scale_color_manual(values=wes_palette(n=2, name="Moonrise2")) +
  scale_x_discrete(limits=c("Paleocene","Eocene"))+
  labs(x="Epoch", y="")+
  theme_light()
###############################################################################
#bar graph
tot_diterp <- ggplot(diterp_dat) +
  geom_col(aes(x = Basin, y = Total.diterpenoids, fill = Epoch), position = "dodge") +
  scale_fill_manual(values=wes_palette(n=3, name="Chevalier1"),
                    guide = guide_legend(order = 2))+
  labs(x="", y="Total Diterpenoids")+
  guides(fill = guide_legend(reverse = TRUE))+ #flips legend for geologic order
  theme_light()+
  theme(legend.position = "none")
tot_diterp

ex_diterp <- ggplot(diterp_dat) +
  geom_col(aes(x = Basin, y = Total.diterpenoids..excluding.tetracylic.compounds., 
               fill = Epoch), position = "dodge") +
  scale_fill_manual(values=wes_palette(n=3, name="Chevalier1"),
                    guide = guide_legend(order = 2))+
  labs(x="", y="Total Diterpenoids \n(excluding tetracyclic compounds)")+
  guides(fill = guide_legend(reverse = TRUE))+ #flips legend for geologic order
  theme_light()

#extracting legend
legend_diterp <- get_legend(ex_diterp)

ex_diterp <- ggplot(diterp_dat) +
  geom_col(aes(x = Basin, y = Total.diterpenoids..excluding.tetracylic.compounds., 
               fill = Epoch), position = "dodge") +
  scale_fill_manual(values=wes_palette(n=3, name="Chevalier1"),
                    guide = guide_legend(order = 2))+
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
###############################################################################
#saving
ggsave("Figures/diterp.pdf", diterp_plot, units = "in", width = 6.5, 
       scale = 1)
