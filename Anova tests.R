##############################################################################
#Name: Lauren Azevedo Schmidt
#R version: 3.6.1
#Purpose: Anova and pairwise comparison
#date started: 03.10.2020
#############################################################################
#wd
getwd()

#packages
require(curl)
require(car)
require(dplyr)
install_github("willemsleegers/tidystats")
require(tidystats)
library(dplyr)
library(knitr)
require(devtools)
require(rstatix)

#load data file
simp_data <- read.table("BHB_HB_simple.csv", 
                        header=T, sep=",", 
                        na.strings=  "NA", dec=".", strip.white=TRUE)

#response as a function of epoch*basin.  This is the interaction we need to 
#look at

#d13C. TOC Anova
aov_d13C.TOC <- aov(d13C.TOC ~ as.factor(Epoch)*as.factor(Basin), data = simp_data)
aov_d13C.TOC
#plotting data
res_d13C <- aov_d13C.TOC$residuals
hist(res_d13C, main = "Histogram of residuals \nAOV d13C TOC", xlab = "residuals")
leveneTest(d13C.TOC ~ Epoch*Basin, data = simp_data)
summary(aov_d13C.TOC)
#Tukey test
TukeyHSD(aov_d13C.TOC)
#making a table
kable(aov_d13C.TOC, digits = 3, format = "pandoc", caption = "ANOVA table")

#dD Anova
aov_dD.29 <- aov(dD.n.C29.alkane.mean ~ as.factor(Epoch)*as.factor(Basin), 
                 data = simp_data)
aov_dD.29
#plotting data
res_dD.29 <- aov_dD.29$residuals
hist(res_dD.29, main = "Histogram of residuals \nAOV dD 29", xlab = "residuals")
leveneTest(dD.n.C29.alkane.mean ~ Epoch*Basin, data = simp_data)
summary(aov_dD.29)
#Tukey test
TukeyHSD(aov_dD.29)

#dLeaf Anova
aov_dLeaf <- aov(Dleaf..ECO2.leaf. ~ as.factor(Epoch)*as.factor(Basin), 
                 data = simp_data)
aov_dLeaf
#plotting data
res_dLeaf <- aov_dLeaf$residuals
hist(res_dLeaf, main = "Histogram of residuals \nAOV dLeaf", xlab = "residuals")
leveneTest(Dleaf..ECO2.leaf. ~ as.factor(Epoch)*as.factor(Basin),
           data = simp_data)
summary(aov_dLeaf)
#Tukey test
TukeyHSD(aov_dLeaf)

#ACL Anova
aov_ACL <-aov(ACL ~ as.factor(Epoch)*as.factor(Basin), data = simp_data)
aov_ACL
#plotting data
res_dACL <- aov_ACL$residuals
hist(res_dACL, main = "Histogram of residuals \nAOV ACL", xlab = "residuals")
leveneTest(ACL ~ as.factor(Epoch)*as.factor(Basin),
           data = simp_data)
summary(aov_ACL)
#Tukey test
TukeyHSD(aov_ACL)

#CPI Anova
aov_CPI <- aov(CPI ~ as.factor(Epoch)*as.factor(Basin), data = simp_data)
aov_CPI
#plotting data
res_CPI <- aov_CPI$residuals
hist(res_CPI, main = "Histogram of residuals \nAOV CPI", xlab = "residuals")
leveneTest(CPI ~ as.factor(Epoch)*as.factor(Basin),
           data = simp_data)
summary(aov_CPI)

#Tukey test
TukeyHSD(aov_CPI)

#plotting all histograms together
par(mfrow=c(3,2))
hist(res_d13C, main = "Histogram of residuals \nAOV d13C TOC", xlab = "residuals")
hist(res_dD.29, main = "Histogram of residuals \nAOV dD 29", xlab = "residuals")
hist(res_dLeaf, main = "Histogram of residuals \nAOV dLeaf", xlab = "residuals")
hist(res_dACL, main = "Histogram of residuals \nAOV ACL", xlab = "residuals")
hist(res_CPI, main = "Histogram of residuals \nAOV CPI", xlab = "residuals")

#using tidystats to make tables
# Add results
results <- list() #making an empty list to fill in later 

results <- results %>%
  add_stats(aov_ACL) %>%
  add_stats(aov_CPI) %>%
  add_stats(aov_d13C.TOC) %>%
  add_stats(aov_dD.29) %>%
  add_stats(aov_dLeaf)
anova_summary(aov_ACL, aov_CPI, aov_d13C.TOC, aov_dD.29, aov_dLeaf)

#To write the results to a file, use write_stats() with the results list as the first argument.
write_stats(results, "results/results.csv")

options(knitr.kable.NA = '-')
results %>%
  stats_list_to_df() %>%
  select(-notes) %>%
  kable()

#reporting functions
results <- read_stats("results/results.csv")
options(tidystats_list = results)

##############################################################################


