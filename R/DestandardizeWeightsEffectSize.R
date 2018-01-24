#destandardize coefficients for use in toy model of if climate changes x then limno changes x

setwd("~/Dropbox/Sarah_Work/Manuscripts/2017_LAGOSClimateWaterqual/LAGOSclimate")

#Standardized coefficients
n.std<-readRDS(file="Data/n_l3_03.rds")
p.std<-readRDS(file="Data/p_l3_03.rds")
sec.std<-readRDS(file="Data/sec_l3_03.rds")
chla.std<-readRDS(file="Data/chl_l3_03.rds")

#data to get means and stdevs of input data. Shuai did variable-mean/stdev to standardize prior to analysis

inputdata<-readRDS(file="Data/LimnoandClimateforMTLanalysis.rds")

library(dplyr)

#just want to look at focal variables for winter precip and summer temp -- so:
#nov precip, dec precip, jan precip, feb precip, winter precip
#may temp, june temp, summer temp

## shuai standardized each variable by subtracting the mean and dividing by standard deviation, so need to multiply by SD and add mean to destandardize effect sizes.  use initial data to find means and SDs first, then modify effect sizes

relvars<-inputdata[,c(1)]