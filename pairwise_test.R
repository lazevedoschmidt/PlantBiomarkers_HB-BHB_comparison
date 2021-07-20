#packages
require(ggplot2)
require(pairwiseComparisons)
require(ggplot2)
require(vegan)
require(paletteer)
require(tidyverse)
require(wesanderson)
require(cowplot)
require(stats)
require(ggstatsplot)
require(ggsignif)
require(DescTools)

#load data file
dD_data <- read.table("BHB_HB_dD.csv", 
                      header=T, sep=",", 
                      na.strings=  "NA", dec=".", strip.white=TRUE)
dD_data<- subset(dD_data,Epoch!="UNK") 

# Basic box plot
dD_29_plot <- dD_data %>%
  mutate(name = fct_relevel(Epoch, 
                            "Paleocene", "PETM", "Eocene")) %>%
  ggplot(aes(x = Epoch, y = dD_data$dD.n.C29.alkane.mean, 
             fill = Basin)) + 
  geom_boxplot()+
  geom_jitter(width = 0.2)+
  scale_fill_manual(values=wes_palette(n=2, name="GrandBudapest1")) +
  scale_x_discrete(limits=c("Paleocene","PETM","Eocene"))+
  labs(x="", y="dD C29")+
  theme(legend.position = "none")+
  #theme(plot.title = element_text(hjust = 0.5))+
  #ggtitle("dD values across basins\n")+
  #geom_signif(comparisons = list(c("Paleocene","PETM")), 
  #map_signif_level=TRUE)
  #geom_signif(comparisons = list(c("PETM","Eocene")), 
  #map_signif_level=TRUE)
  #geom_signif(comparisons = list(c("Paleocene","Eocene")), 
              #map_signif_level=TRUE)+
  scale_y_continuous(expand = expansion(mult = c(0.05,0.1)))  
#The difference between the Paleocene and 
#Eocene is significant but the difference between Paleo to PETM, and PETM to 
#Eocene are not significantly different.  
dD_29_plot

#Trying to add pvalues 
df_pair <- pairwiseComparisons::pairwise_comparisons(
  data = dD_data,
  x = Epoch,
  y = dD.n.C29.alkane.mean)

# adding a geom for pairwise comparisons
ggstatsplot:::ggsignif_adder(
  plot = dD_29_plot,
  data =dD_data ,
  x = Epoch,
  y = dD.n.C29.alkane.mean,
  df_pairwise = df_pair)
