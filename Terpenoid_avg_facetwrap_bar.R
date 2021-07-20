require(ggplot2)
#load data file
diterp_avg <- read.table("AVG.terp.bar.data.csv", 
                         header=T, sep=",", 
                         na.strings=  "NA", dec=".", strip.white=TRUE) 

#Ordering legend properly
diterp_avg$Epoch <- factor(diterp_avg$Epoch, levels = c("Paleocene", "PETM", "Eocene"))
wrap.labs <- c("Total Diterp"= "Average Diterpenoids", 
               "Total Triterp" = "Average Triterpenoids")

#bargraph using facet_wrap
facet_avg <- ggplot(diterp_avg) +
  geom_col(aes(x = Basin, y = AVG.values, fill = Epoch),position="dodge") +
  geom_errorbar( aes(x=Basin, y=AVG.values,
                     ymin=AVG.values-SD, 
                     ymax=AVG.values+SD),
                    position=position_dodge(0.9),
                    colour="black", width=0.1, alpha=0.2, size=.5)+
  scale_x_discrete(drop=FALSE)+
  scale_fill_manual(values=palette2,
                    guide = guide_legend(order = 2))+
  labs(x="", y="Average Terpenoids")+
  guides(fill = guide_legend(reverse = TRUE))+ #flips legend for geologic order
  theme_light()+
  facet_wrap(~ID, labeller = labeller(ID = wrap.labs))  #tell it to keep PETM in BHB
  
facet_avg
ggsave("Figures/terp_bar_avg.pdf", facet_avg)
