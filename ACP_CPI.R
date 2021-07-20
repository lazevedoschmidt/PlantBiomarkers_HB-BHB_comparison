##############################################################################
#Name: Lauren Azevedo Schmidt
#R version: 3.6.1
#Purpose: ACL and CPI 
#date started: 02.12.20
#############################################################################
#wd
getwd()
#packages
require(ggplot2)
require(paletteer)
require(ggpubr)
install.packages("wesanderson")
# Load
library(wesanderson)
library(cowplot)

#load data file
carb_data <- read.table("TOC_ACL_CPI.csv", 
                   header=T, sep=",", 
                   na.strings=  "NA", dec=".", strip.white=TRUE)
carb_data <-na.exclude(carb_data)

# Plotting TOC against all Sample IDs
TOCplot <- ggplot(carb_data, aes(x = Sample.ID, y = d13C.TOC, 
                           color = Epoch, shape=Basin)) + 
  geom_point(size=4)+
  scale_color_manual(values=wes_palette(n=3, name="Darjeeling1")) +
  theme_classic()+
  theme(legend.position = "none", 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  #theme(plot.title = element_text(hjust = 0.5))+
  labs(y="TOC")
  #ggpubr::rotate_x_text()
TOCplot


#Plotting ACL against all Sample ID
ACLplot <- ggplot(carb_data, aes(x = Sample.ID, y = ACL, 
                                 color = Epoch, shape=Basin)) + 
  geom_point(size=4)+
  scale_color_manual(values=wes_palette(n=3, name="Darjeeling1")) +
  theme_classic()+
  theme(legend.position = "none", 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  #theme(plot.title = element_text(hjust = 0.5))+
  labs(y="ACL")
  #ggpubr::rotate_x_text()
ACLplot

#Plotting ACL against all Sample ID
CPIplot <- ggplot(carb_data, aes(x = Sample.ID, y = CPI, 
                                 color = Epoch, shape=Basin)) + 
  geom_point(size=4)+
  scale_color_manual(values=wes_palette(n=3, name="Darjeeling1")) +
  theme_classic()+
  theme(axis.title.x=element_blank(), #removes the x axis labels 
        axis.text=element_text(size=6),
        axis.ticks.x=element_blank())+
  #theme(plot.title = element_text(hjust = 0.5))+
  labs(y="CPI") +
  ggpubr::rotate_x_text()  #rotates the sample IDs
CPIplot

# extract the legend from one of the plots
# (clearly the whole thing only makes sense if all plots
# have the same legend, so we can arbitrarily pick one.)
legend <- get_legend(CPIplot)

#Stacking all three plots together
sup_carb <- plot_grid( TOCplot + theme(legend.position="none"),
           ACLplot + theme(legend.position="none"),
           CPIplot + theme(legend.position="none"),
           align = 'v',
           rel_widths = c(0.8,0.2),
           nrow = 3)

# add the legend to the row we made earlier. Give it one-third of the width
# of one plot (via rel_widths).
sup_carb <- plot_grid( sup_carb, legend, rel_widths = c(0.85,0.15),scale = 1)
sup_carb
#############################################################################
#saving figure
ggsave("Figures/TOC.pdf", TOCplot, units = "in", scale = 1)
ggsave("Figures/ACL.pdf", ACLplot, units = "in", scale = 1)
ggsave("Figures/CPI.pdf", CPIplot, units = "in", scale = 1)
ggsave("Figures/sup_carb.pdf", sup_carb, units = "in", scale = 1)
