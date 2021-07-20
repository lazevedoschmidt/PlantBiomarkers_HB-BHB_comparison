#Pr/PH:TAR analysis
# load ggplot2
require(ggplot2)
require(tidyverse)
require(stats)
require(ggsignif)
require(ggpubr)
require(ggpmisc)
require(dplyr)
require(scales)


#load data file
pr.ph.tar <- read.table("Pr:Ph_TAR.csv", 
                       header=T, sep=",", 
                       na.strings=  "NA", dec=".", strip.white=TRUE)
pr.ph.tar<- subset(pr.ph.tar,Epoch!="UNK") 
pr.ph.tar<- subset(pr.ph.tar,Epoch!="")
pr.ph.tar<- subset(pr.ph.tar,Epoch!="?")

test <- rosnerTest(pr.ph.tar$Pr.Ph,
                   k = 10
)
test
pr.ph.tar <- pr.ph.tar[-39,]

test <- rosnerTest(pr.ph.tar$TAR,
                   k = 10
)
test
pr.ph.tar <- pr.ph.tar[-53,]

pr.ph.tar %>%
  group_by(Basin, Epoch) %>%
  drop_na()%>%
  summarise(COUNT = n())

pr.ph.tar <- pr.ph.tar %>%
  dplyr::select(Basin, Epoch, Lithology, Pr.Ph, TAR)%>%
  drop_na()
#Ordering legend properly
pr.ph.tar$Epoch <- factor(pr.ph.tar$Epoch, 
                          levels = c("Paleocene", "PETM", "Eocene"))

# Scatterplot with Basin as shape
Pr.PhvsTAR <- pr.ph.tar %>%
  mutate(name = fct_relevel(Epoch, 
                            "Paleocene", "PETM", "Eocene")) %>%
  ggplot(aes(x=TAR, y=Pr.Ph, color=Epoch, shape=Basin)) +
  geom_point(size=3.5, aes(colour=Epoch))+
  ggtitle("Pr/Ph vs.TAR")+
  ylab("Pr/Ph")+
  scale_fill_manual(values=palette2, guide = guide_legend(reverse = TRUE))+
  scale_colour_manual(values=palette2, guide = guide_legend(reverse = TRUE))+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  geom_hline(yintercept=1, linetype="dashed")+ #under 1 is anoxic
  geom_hline(yintercept=3, linetype="dashed")+ #grey area
  #but above 3 is oxic environment
  geom_vline(xintercept = 1, linetype="dotdash", color="grey")+
  theme_light()+
  theme(plot.title = element_text(hjust=0.5)) #centers title must be under
#the theme_light call or it will be overridden

Pr.PhvsTAR 
#Scatter plot with facet-wrap ~Basin
lith_Pr.PhvsTAR <- pr.ph.tar %>%
  mutate(name = fct_relevel(Epoch, 
                            "Paleocene", "PETM", "Eocene")) %>%
  ggplot(aes(x=TAR, y=Pr.Ph, color=Epoch, shape=Lithology)) +
  geom_point(size=3.5, aes(colour=Epoch))+
  facet_wrap(~Basin)+
  ggtitle("Pr/Ph vs.TAR")+
  ylab("Pr/Ph")+
  scale_fill_manual(values=palette2, guide = guide_legend(reverse = TRUE))+
  scale_colour_manual(values=palette2, guide = guide_legend(reverse = TRUE))+
  scale_shape_manual(values=c(15,16,17,18,8,3,5,6))+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  geom_hline(yintercept=1, linetype="dashed")+ #under 1 is anoxic
  geom_hline(yintercept=3, linetype="dashed")+ #grey area
  #but above 3 is oxic environment
  geom_vline(xintercept = 1, linetype="dotdash", color="grey")+
  theme_light()+
  theme(plot.title = element_text(hjust=0.5))
lith_Pr.PhvsTAR 

#Save figure----
ggsave("Figures/Pr.Phvs.TAR.pdf",Pr.PhvsTAR)
ggsave("Figures/Pr.Phvs.TAR_lith.pdf",lith_Pr.PhvsTAR)
