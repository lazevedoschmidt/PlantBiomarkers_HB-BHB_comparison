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

my_comparisons_Epoch <- list( c("Paleocene", "PETM"), c("Paleocene", "Eocene"), 
                        c("PETM", "Eocene") )
my_comparisons_Epoch <- list(c("Paleocene", "Eocene")) #only keeping significant
                             

my_comparisons_Basin <- list( c("Bighorn Basin", "Hanna Basin"), c("Bighorn Basin", "Hanna Basin"), 
                              c("Bighorn Basin", "Hanna Basin") )

my_comparisons_Basin <- list( c("Bighorn Basin", "Hanna Basin"),  
                              c("Bighorn Basin", "Hanna Basin") ) #only keeping significant

p <- dD_29_plot + stat_compare_means(comparisons = my_comparisons_Basin)+
  stat_compare_means(comparisons = my_comparisons_Epoch)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = -130)  # Add global p-value
p

#dD 31
dD_31_plot <- dD_data %>%
  mutate(name = fct_relevel(Epoch, 
                            "Paleocene", "PETM", "Eocene")) %>%
  ggplot(aes(x = Epoch, y = dD_data$dD.n.C31.alkane.mean, 
             fill = Basin)) + 
  geom_boxplot()+
  geom_jitter(width = 0.2)+
  scale_fill_manual(values=wes_palette(n=2, name="GrandBudapest1")) +
  scale_x_discrete(limits=c("Paleocene","PETM","Eocene"))+
  labs(x="Epoch", y="dD C31")+
  theme(legend.position = "none")+
  #geom_signif(comparisons = list(c("Paleocene","PETM")), 
  #map_signif_level=TRUE)+
  #geom_signif(comparisons = list(c("PETM","Eocene")), 
  #map_signif_level=TRUE)+
  #geom_signif(comparisons = list(c("Paleocene","Eocene")), 
  #map_signif_level=TRUE)+
  scale_y_continuous(expand = expansion(mult = c(0.05,0.1)))
#same is true here.  
dD_31_plot

my_comparisons_Epoch31 <- list( c("Paleocene", "PETM"), c("Paleocene", "Eocene"), 
                              c("PETM", "Eocene") )
my_comparisons_Epoch31 <- list(c("Paleocene", "Eocene")) #only keeping significant


my_comparisons_Basin31 <- list( c("Bighorn Basin", "Hanna Basin"), c("Bighorn Basin", "Hanna Basin"), 
                              c("Bighorn Basin", "Hanna Basin") )

 p2 <- dD_31_plot + stat_compare_means(comparisons = my_comparisons_Basin31)+
  stat_compare_means(comparisons = my_comparisons_Epoch31)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = -140)  # Add global p-value

 #Plotting the two plots together 
 plot_grid( p,
                       legend1,
                       p2,
                       nrow = 2,
                       ncol=2)
 





