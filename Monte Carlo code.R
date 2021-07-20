##############################################################################
#Name: Lauren Azevedo Schmidt
#R version: 3.6.1
#Purpose: Monte Carlo
#date started: 04.23.2020
#############################################################################
#wd
getwd()
require(tidyverse)

#load in data
dleaf_data <- read.csv("BHB_HB_dleaf_data.csv", header=TRUE)
dleaf_data_avg <- read.csv("BHB_HB_dleaf_data_timeaverage.csv", header=TRUE)
##############################################################################
#Cleaning up NA from data frame
dleaf_data <- dleaf_data %>%
  drop_na() 

#same as above just for time averaged data
dleaf_data_avg <- dleaf_data_avg %>%
  drop_na() 
##############################################################################
#All data
nrun <- 10000 #number of runs

dleaf_data$d13CnC29leaf <- (dleaf_data$d13CnC29_alkane+1000)/(dleaf_data$E_nC29_alkane/1000+1)-1000
dleaf_data$d13CnC31leaf <- (dleaf_data$d13CnC31_alkane+1000)/(dleaf_data$E_nC31_alkane/1000+1)-1000
dleaf_data$Dleaf_nC29 <- (dleaf_data$d13Catm-dleaf_data$d13CnC29leaf)/(1+(dleaf_data$d13CnC29leaf/1000))
dleaf_data$Dleaf_nC31<-(dleaf_data$d13Catm-dleaf_data$d13CnC31leaf)/(1+(dleaf_data$d13CnC31leaf/1000))

for (x in 1:nrow(dleaf_data)) {
  EnC29_MC <- rnorm(nrun,mean = dleaf_data$E_nC29_alkane[x], 
                    sd=dleaf_data$E_nC29_alkane_1sd[x])
  d13Catm_MC <- rnorm(nrun,mean = dleaf_data$d13Catm[x], 
                    sd=dleaf_data$d13Catm_1sd[x])
  d13CnC29_MC <- rnorm(nrun,mean = dleaf_data$d13CnC29_alkane[x], 
                      sd=dleaf_data$d13CnC29_alkane_1sd[x])
  d13CnC29leaf_MC <-(d13CnC29_MC+1000)/(EnC29_MC/1000+1)-1000
  Dleaf_nC29_MC <- (d13Catm_MC-d13CnC29leaf_MC)/(1+(d13CnC29leaf_MC/1000))
  dleaf_data$Dleaf_nC29_MC_mean[x] <- mean(Dleaf_nC29_MC)
  dleaf_data$Dleaf_nC29_MC_1sd[x] <- sd(Dleaf_nC29_MC)
  
  EnC31_MC <- rnorm(nrun,mean = dleaf_data$E_nC31_alkane[x], 
                    sd=dleaf_data$E_nC31_alkane_1sd[x])
  d13CnC31_MC <- rnorm(nrun,mean = dleaf_data$d13CnC31_alkane[x], 
                      sd=dleaf_data$d13CnC31_alkane_1sd[x])
  d13CnC31leaf_MC <-(d13CnC31_MC+1000)/(EnC31_MC/1000+1)-1000
  Dleaf_nC31_MC <- (d13Catm_MC-d13CnC31leaf_MC)/(1+(d13CnC31leaf_MC/1000))
  dleaf_data$Dleaf_nC31_MC_mean[x] <- mean(Dleaf_nC31_MC)
  dleaf_data$Dleaf_nC31_MC_1sd[x] <- sd(Dleaf_nC31_MC)
}

mean(dleaf_data$Dleaf_nC29_MC_mean)
mean(dleaf_data$Dleaf_nC29_MC_1sd)

#Dleaf MC errors
dleaf_data$Dleaf_nC29_MC_offset <- dleaf_data$Dleaf_nC29 - dleaf_data$Dleaf_nC29_MC_mean
dleaf_data$Dleaf_nC31_MC_offset <- dleaf_data$Dleaf_nC31 - dleaf_data$Dleaf_nC31_MC_mean

###############################################################################
#same as above but for time averaged data
nrun <- 10000 #number of runs

dleaf_data_avg$d13CnC29leaf <- (dleaf_data_avg$d13CnC29_alkane+1000)/(dleaf_data_avg$E_nC29_alkane/1000+1)-1000
dleaf_data_avg$d13CnC31leaf <- (dleaf_data_avg$d13CnC31_alkane+1000)/(dleaf_data_avg$E_nC31_alkane/1000+1)-1000
dleaf_data_avg$Dleaf_nC29 <- (dleaf_data_avg$d13Catm-dleaf_data_avg$d13CnC29leaf)/(1+(dleaf_data_avg$d13CnC29leaf/1000))
dleaf_data_avg$Dleaf_nC31<-(dleaf_data_avg$d13Catm-dleaf_data_avg$d13CnC31leaf)/(1+(dleaf_data_avg$d13CnC31leaf/1000))

for (x in 1:nrow(dleaf_data_avg)) {
  EnC29_MC <- rnorm(nrun,mean = dleaf_data_avg$E_nC29_alkane[x], 
                    sd=dleaf_data_avg$E_nC29_alkane_1sd[x])
  d13Catm_MC <- rnorm(nrun,mean = dleaf_data_avg$d13Catm[x], 
                      sd=dleaf_data_avg$d13Catm_1sd[x])
  d13CnC29_MC <- rnorm(nrun,mean = dleaf_data_avg$d13CnC29_alkane[x], 
                       sd=dleaf_data_avg$d13CnC29_alkane_1sd[x])
  d13CnC29leaf_MC <-(d13CnC29_MC+1000)/(EnC29_MC/1000+1)-1000
  Dleaf_nC29_MC <- (d13Catm_MC-d13CnC29leaf_MC)/(1+(d13CnC29leaf_MC/1000))
  dleaf_data_avg$Dleaf_nC29_MC_mean[x] <- mean(Dleaf_nC29_MC)
  dleaf_data_avg$Dleaf_nC29_MC_1sd[x] <- sd(Dleaf_nC29_MC)
  
  EnC31_MC <- rnorm(nrun,mean = dleaf_data_avg$E_nC31_alkane[x], 
                    sd=dleaf_data_avg$E_nC31_alkane_1sd[x])
  d13CnC31_MC <- rnorm(nrun,mean = dleaf_data_avg$d13CnC31_alkane[x], 
                       sd=dleaf_data_avg$d13CnC31_alkane_1sd[x])
  d13CnC31leaf_MC <-(d13CnC31_MC+1000)/(EnC31_MC/1000+1)-1000
  Dleaf_nC31_MC <- (d13Catm_MC-d13CnC31leaf_MC)/(1+(d13CnC31leaf_MC/1000))
  dleaf_data_avg$Dleaf_nC31_MC_mean[x] <- mean(Dleaf_nC31_MC)
  dleaf_data_avg$Dleaf_nC31_MC_1sd[x] <- sd(Dleaf_nC31_MC)
}

#Dleaf MC errors
dleaf_data_avg$Dleaf_nC29_MC_offset <- dleaf_data_avg$Dleaf_nC29 - dleaf_data_avg$Dleaf_nC29_MC_mean
dleaf_data_avg$Dleaf_nC31_MC_offset <- dleaf_data_avg$Dleaf_nC31 - dleaf_data_avg$Dleaf_nC31_MC_mean
###############################################################################


