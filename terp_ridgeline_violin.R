#ridgeplots----
# library
library(ggridges)
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(ggnewscale)
library(cowplot)

#load data with all data. 
ridgedata <- read.csv("terp_lgfrm_data_lean.csv", header=TRUE)
#Ordering legend properly
ridgedata$Epoch <- factor(ridgedata$Epoch, levels = c("Paleocene", "PETM", "Eocene"))
ridgedata<- subset(ridgedata,Epoch!="?")

diridge <- subset(ridgedata, ID!="Total Triterp")

hbdata <- subset(diridge, Basin=="Hanna Basin")
hbdata$Epoch <- factor(hbdata$Epoch, levels = c("Paleocene", "PETM", "Eocene"))
hbdata <- arrange(hbdata, Epoch)
sample_size <- factor(sample_size$Epoch, levels = c("Paleocene", "PETM", "Eocene"))

bhbdata <- subset(diridge, Basin=="Bighorn Basin")
bhbdata$Epoch <- factor(bhbdata$Epoch, levels = c("Paleocene", "PETM", "Eocene"))

# # Plot
# ridge <- ggplot(ridgedata, aes(x = values, y = ID, fill = Epoch)) +
#   geom_density_ridges_gradient(scale=0.9, rel_min_height =0.01) +
#   scale_fill_manual(values= c("#0F0D0E","#CCBA72"))+ 
#   #scale_x_discrete(drop=FALSE)+
#   theme_ipsum() +
#   ylab("")+
#   theme(plot.title = element_text(hjust = 0.5))+
#   facet_wrap(~Basin, scales = "free_x")
# ridge

#violin plots
bhbviolin <- ggplot(bhbdata, aes(x=Epoch, y=values, fill=Epoch, color=Epoch)) +
  geom_violin(width=0.8, size=0.2) +
  scale_fill_manual(values= c("#0F0D0E","#CCBA72")) +
  scale_color_manual(values= c("#0F0D0E","#CCBA72")) +
  scale_x_discrete(drop=FALSE)+
  theme_ipsum() +
  theme(
    legend.position="none"
  ) +
  coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  xlab("") +
  ylab("")+
  facet_wrap(~Basin, scales = "free_x")
bhbviolin


# sample size
hbsample_size = hbdata %>% 
  group_by(Epoch) %>% 
  summarize(num=n())

# Plot
hbviolin <- hbdata %>%
  left_join(hbsample_size) %>%
  mutate(myaxis = paste0(Epoch, "\n", "n=", num)) %>%
  ggplot( aes(x=myaxis, y=values, fill=Epoch)) +
  geom_violin(width=1.4) +
  geom_boxplot(width=0.1, color="grey", alpha=0.2) +
  scale_fill_manual(values= c("#CCBA72","#0F0D0E","#D9D0D3")) +
  scale_color_manual(values= c("#CCBA72","#0F0D0E","#D9D0D3")) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  coord_flip() + 
  ggtitle("Hanna Basin Diterpenoid Variability") +
  xlab("")
hbviolin

bhbsample_size = bhbdata %>% 
  group_by(Epoch) %>% 
  summarize(num=n())

bhbviolin <- bhbdata %>%
  left_join(bhbsample_size) %>%
  mutate(myaxis = paste0(Epoch, "\n", "n=", num)) %>%
  ggplot( aes(x=myaxis, y=values, fill=Epoch)) +
  geom_violin(width=0.8) +
  geom_boxplot(width=0.1, color="grey", alpha=0.5) +
  scale_fill_manual(values= c("#0F0D0E","#CCBA72")) +
  scale_color_manual(values= c("#0F0D0E","#CCBA72")) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  coord_flip() +
  ggtitle("Bighorn Basin Diterpenoid Variability") +
  xlab("")+
  scale_x_discrete(drop=FALSE)
bhbviolin

#putting them together
diterpvar.plot <- plot_grid(hbviolin, 
                            bhbviolin, 
                            nrow = 2, 
                            ncol = 1)
diterpvar.plot

#saving plots----
ggsave("Figures/bhbviolin.pdf", bhbviolin)
ggsave("Figures/diterpvar.plot.pdf", diterpvar.plot, height = 12, width = 8, units = "in")
