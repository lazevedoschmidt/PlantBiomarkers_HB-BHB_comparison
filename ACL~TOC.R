##############################################################################
#Name: Lauren Azevedo Schmidt
#R version: 3.6.1
#Purpose: ACL and TOC
#date started: 02.12.20
#############################################################################
#wd
getwd()

#load packages
require(ggplot2)
require(wesanderson)
require(ggpmisc)

#load data file
carb_data <- read.table("TOC_ACL_CPI.csv", 
                        header=T, sep=",", 
                        na.strings=  "NA", dec=".", strip.white=TRUE)
carb_data <-na.exclude(carb_data)

dialkane_data <- read.table("ACL_di:di+alkanes.csv", 
                        header=T, sep=",", 
                        na.strings=  "NA", dec=".", strip.white=TRUE)
dialkane_data$CPI<- NULL
dialkane_data <-na.exclude(dialkane_data)


#plotting ACL and TOC correlation 
ACL <- carb_data$ACL
TOC <- carb_data$d13C.TOC
#plot(ACL~TOC, col=carb_data$Epoch, cex=1, pch=3)
summary(m1 <- lm(ACL~TOC))
abline(m1)
plot(m1)

#ggplot of TOC and ACL
# Scatter plot with regression line fitted
ACL_TOC <- ggplot(carb_data, aes(x=d13C.TOC, y=ACL, color = Epoch, shape=Basin)) +
  geom_point(size=4)+
  geom_smooth(method=lm, se=TRUE, linetype="dashed",
              color="darkred", fullrange=TRUE)+
  scale_color_manual(values=wes_palette(n=3, name="GrandBudapest1")) +
  stat_fit_glance(method = "lm",
                  label.y = "top",
                  label.x = "right",
                  method.args = list(formula=y~x),
                  mapping = aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2g',
                                                stat(r.squared), stat(p.value))),
                  parse = TRUE)+
  theme_get()+
  ggtitle("TOC~ACL")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x="TOC", y="ACL")
ACL_TOC

#ACL by di/(di+alkanes)
ACL_dialk <- ggplot(dialkane_data, aes(x=ACL, y=dialkane_data$di..di.alkanes., 
                          color = Epoch, shape=Basin)) +
  geom_point(size=4)+
  geom_smooth(method=lm, se=TRUE, linetype="dashed",
              color="darkred", fullrange=TRUE)+
  scale_color_manual(values=wes_palette(n=3, name="BottleRocket1")) +
  stat_fit_glance(method = "lm",
                  label.y = "top",
                  label.x = "left",
                  method.args = list(formula=y~x),
                  mapping = aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2g',
                                                stat(r.squared), stat(p.value))),
                  parse = TRUE)+
  theme_get()+
  ggtitle("ACL~di/(di+alkanes) plot")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x="ACL", y="di/(di+alkanes)")
ACL_dialk

#Plotting the two plots together 
compfig2<- plot_grid( ACL_TOC,
             ACL_dialk,
             nrow = 2,
             ncol=1,
           labels = c("A", "B"))
compfig2

#############################################################################
#saving figure
ggsave("Figures/ACL~TOC.pdf", ACL_TOC, units = "in", scale = 1)
ggsave("Figures/ACL~dialk.pdf", ACL_dialk, units = "in", scale = 1)
ggsave("Figures/TOC_ACL_alkane.pdf", compfig2, units = "in", scale = 1)





