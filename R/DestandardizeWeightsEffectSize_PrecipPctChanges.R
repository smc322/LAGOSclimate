#destandardize coefficients for use in toy model of if climate changes x then limno changes x

setwd("~/Dropbox/Sarah_Work/Manuscripts/2017_LAGOSClimateWaterqual/LAGOSclimate")

#Standardized coefficients
n.std<-readRDS(file="Data/n_l3_03.rds")
p.std<-readRDS(file="Data/p_l3_03.rds")
sec.std<-readRDS(file="Data/sec_l3_03.rds")
chla.std<-readRDS(file="Data/chl_l3_03.rds")

#data to get means and stdevs of input data. Shuai did variable-mean/stdev to standardize prior to analysis

inputdata<-readRDS(file="Data/MTLinputdata_feb2017.rds")

library(dplyr)

#just want to look at focal variables for winter precip and summer temp -- so:
#nov precip, jan precip, winter precip


## shuai standardized each predictor and response variable by subtracting the mean and dividing by standard deviation.  So, to figure out what the difference between current temp and projected temp is on TP, we would standardize median temp and median plus projected increase (sam created these) based on mean and sd temp of the input that shuai used, then mult by std coef for that variable, then destandardize the result based on the input TP data mean and sd.  Relevant variables are may, june and summer for temp, and nov, jan and winter for precip

relvars<-inputdata[,c(1:3,25, 34, 49)]

##winter precip info for standardizing medians and medians plus clim change
#mean nov precip = 6468
mean(relvars$November.ppt.x1)
#sd nov precip = 4625
sd(relvars$November.ppt.x1)

#mean jan precip = 4385
mean(relvars$January.ppt)
#sd jan precip = 3747
sd(relvars$January.ppt)

#mean winter precip = 4596
mean(relvars$ppt.winter)
#sd winter precip = 3134
sd(relvars$ppt.winter)

#read in sam's summer medians and summer medians plus 1.7deg C projection, then subtract means and divide by sd to sdandardize like shuai did

winterprecip<-readRDS(file="Data/hu12_ppt_median.rds")

winterprecip$NovStd<-(winterprecip$November-6468)/4625
winterprecip$JanStd<-(winterprecip$January-4385)/3747
winterprecip$winterStd<-(winterprecip$winter-4596)/3134

stdmedianwp<-winterprecip[,c(1,5:7)]
names(stdmedianwp)<-c("hu12_zoneid", "Novmedianst", "Janmedianst", "wintermedianst")

relvars.ids<-unique(relvars[,c(1,3)])

winterprecipstdmeds<-merge(relvars.ids, stdmedianwp, by="hu12_zoneid", all.x=T, all.y=F)

winterprecipstdmeds.lake<-winterprecipstdmeds[,2:5]

#pull out p standardized coeffs for winter temp to multiply by those vals

p.st.coefs<-p.std[,c(1, 18,27,42)]

p.coefs.meds<-merge(p.st.coefs, winterprecipstdmeds.lake, by="lagoslakeid", all.x=T, all.y=T)

p.coefs.meds$pmednov<-p.coefs.meds$November.ppt.x1*p.coefs.meds$Novmedianst
p.coefs.meds$pmedjan<-p.coefs.meds$January.ppt*p.coefs.meds$Janmedianst
p.coefs.meds$pmedwinter<-p.coefs.meds$ppt.winter*p.coefs.meds$wintermedianst

p.coefs.meds$ptotalwinter<-p.coefs.meds$pmednov+p.coefs.meds$pmedjan+p.coefs.meds$pmedwinter

##get mean and sd of P to destandardize ptotalsummer to a value of median P for each lake

#mean P = 33.15
mean(na.omit(relvars$annual.tp))
#SD P = 59.08
sd(na.omit(relvars$annual.tp))

##make col of destandardized median P
p.coefs.meds$destdPmed<-(p.coefs.meds$ptotalwinter*59.08)+33.15

##now do the same for summer temp plus 1.7degc data
winterprecip.plus<-readRDS(file="Data/hu12_ppt_plus15percent.rds")

winterprecip.plus$NovStd<-(winterprecip.plus$November-6468)/4625
winterprecip.plus$JanStd<-(winterprecip.plus$January-4385)/3747
winterprecip.plus$winterStd<-(winterprecip.plus$winter-4596)/3134

stdplusst<-winterprecip.plus[,c(1,5:7)]
names(stdplusst)<-c("hu12_zoneid", "novplusst", "janplusst", "winterplusst")

winterprecipstdplus<-merge(relvars.ids, stdplusst, by="hu12_zoneid", all.x=T, all.y=F)
winterprecipstdplus.lake<-winterprecipstdplus[,2:5]

p.coefs.meds.plus<-merge(p.coefs.meds, winterprecipstdplus.lake, by="lagoslakeid", all.x=T, all.y=T)

#mult coef by median plus change 

p.coefs.meds.plus$pplusnov<-p.coefs.meds.plus$November.ppt.x1*p.coefs.meds.plus$novplusst
p.coefs.meds.plus$pplusjan<-p.coefs.meds.plus$January.ppt*p.coefs.meds.plus$janplusst
p.coefs.meds.plus$ppluswinter<-p.coefs.meds.plus$ppt.winter*p.coefs.meds.plus$winterplusst

p.coefs.meds.plus$ptotalwinterplus<-p.coefs.meds.plus$pplusnov+p.coefs.meds.plus$pplusjan+p.coefs.meds.plus$ppluswinter

##make col of destandardized plus 1.7 P
p.coefs.meds.plus$destdPplus<-(p.coefs.meds.plus$ptotalwinterplus*59.08)+33.15

#average % change in P
p.coefs.meds.plus$pctchangeinP<-(p.coefs.meds.plus$destdPplus-p.coefs.meds.plus$destdPmed)/p.coefs.meds.plus$destdPmed*100

hist(p.coefs.meds.plus$pctchangeinP)

# precip a lot lower than temp there - closer around zero, mostly neg.



####make %change projections for temp for other three response variables
#use summertempstdmeds.lake and summertemstdplus.lake for the standardized median and plus climate change scenario temps, then load in the coefs for the other variables and proceed as with P above.  should have written a function for this but I'm lazy.

c.st.coefs<-chla.std[,c(1, 18, 27, 42)]

c.coefs.meds<-merge(c.st.coefs, winterprecipstdmeds.lake, by="lagoslakeid", all.x=T, all.y=T)

c.coefs.meds$cmednov<-c.coefs.meds$November.ppt.x1*c.coefs.meds$Novmedianst
c.coefs.meds$cmedjan<-c.coefs.meds$January.ppt*c.coefs.meds$Janmedianst
c.coefs.meds$cmedwinter<-c.coefs.meds$ppt.winter*c.coefs.meds$wintermedianst

c.coefs.meds$ctotalwinter<-c.coefs.meds$cmednov+c.coefs.meds$cmedjan+c.coefs.meds$cmedwinter

##get mean and sd of chla to destandardize ctotalsummer to a value of median chla for each lake

#mean chla = 17.18
mean(na.omit(inputdata$annual.chla))
#SD P = 33.25
sd(na.omit(inputdata$annual.chla))

##make col of destandardized median chla
c.coefs.meds$destdCmed<-(c.coefs.meds$ctotalwinter*33.25)+17.18

##add plus 1.7 c data and compute difference
c.coefs.meds.plus<-merge(c.coefs.meds, winterprecipstdplus.lake, by="lagoslakeid", all.x=T, all.y=T)

#mult coef by median plus change 

c.coefs.meds.plus$cplusnov<-c.coefs.meds.plus$November.ppt.x1*c.coefs.meds.plus$novplusst
c.coefs.meds.plus$cplusjan<-c.coefs.meds.plus$January.ppt*c.coefs.meds.plus$janplusst
c.coefs.meds.plus$cpluswinter<-c.coefs.meds.plus$ppt.winter*c.coefs.meds.plus$winterplusst

c.coefs.meds.plus$ctotalwinterplus<-c.coefs.meds.plus$cplusnov+c.coefs.meds.plus$cplusjan+c.coefs.meds.plus$cpluswinter

##make col of destandardized plus 1.7 chla
c.coefs.meds.plus$destdCplus<-(c.coefs.meds.plus$ctotalwinterplus*33.25)+17.18

#average % change in chla
c.coefs.meds.plus$pctchangeinC<-(c.coefs.meds.plus$destdCplus-c.coefs.meds.plus$destdCmed)/c.coefs.meds.plus$destdCmed*100

hist(c.coefs.meds.plus$pctchangeinC)


#and same thing for N
#pull out n standardized coeffs for summer temp to multiply by those vals

n.st.coefs<-n.std[,c(1, 18, 27, 42)]

n.coefs.meds<-merge(n.st.coefs, winterprecipstdmeds.lake, by="lagoslakeid", all.x=T, all.y=T)

n.coefs.meds$nmednov<-n.coefs.meds$November.ppt.x1*n.coefs.meds$Novmedianst
n.coefs.meds$nmedjan<-n.coefs.meds$January.ppt*n.coefs.meds$Janmedianst
n.coefs.meds$nmedwinter<-n.coefs.meds$ppt.winter*n.coefs.meds$wintermedianst

n.coefs.meds$ntotalwinter<-n.coefs.meds$nmednov+n.coefs.meds$nmedjan+n.coefs.meds$nmedwinter

##get mean and sd of N to destandardize ntotalsummer to a value of median N for each lake

#mean N = 819
mean(na.omit(inputdata$annual.tn))
#SD N = 1150
sd(na.omit(inputdata$annual.tn))

##make col of destandardized median N
n.coefs.meds$destdNmed<-(n.coefs.meds$ntotalwinter*1150)+819

##add plus 1.7 c data and compute difference
n.coefs.meds.plus<-merge(n.coefs.meds, winterprecipstdplus.lake, by="lagoslakeid", all.x=T, all.y=T)

#mult coef by median plus change 

n.coefs.meds.plus$nplusnov<-n.coefs.meds.plus$November.ppt.x1*n.coefs.meds.plus$novplusst
n.coefs.meds.plus$nplusjan<-n.coefs.meds.plus$January.ppt*n.coefs.meds.plus$janplusst
n.coefs.meds.plus$npluswinter<-n.coefs.meds.plus$ppt.winter*n.coefs.meds.plus$winterplusst

n.coefs.meds.plus$ntotalwinterplus<-n.coefs.meds.plus$nplusnov+n.coefs.meds.plus$nplusjan+n.coefs.meds.plus$npluswinter

##make col of destandardized plus 1.7 N
n.coefs.meds.plus$destdNplus<-(n.coefs.meds.plus$ntotalwinterplus*1150)+819

#average % change in N
n.coefs.meds.plus$pctchangeinN<-(n.coefs.meds.plus$destdNplus-n.coefs.meds.plus$destdNmed)/n.coefs.meds.plus$destdNmed*100

hist(n.coefs.meds.plus$pctchangeinN)


#and same thing for Secchi
#pull out Sec standardized coeffs for summer temp to multiply by those vals

s.st.coefs<-sec.std[,c(1, 18, 27, 42)]

s.coefs.meds<-merge(s.st.coefs, winterprecipstdmeds.lake, by="lagoslakeid", all.x=T, all.y=T)

s.coefs.meds$smednov<-s.coefs.meds$November.ppt.x1*s.coefs.meds$Novmedianst
s.coefs.meds$smedjan<-s.coefs.meds$January.ppt*s.coefs.meds$Janmedianst
s.coefs.meds$smedwinter<-s.coefs.meds$ppt.winter*s.coefs.meds$wintermedianst

s.coefs.meds$stotalwinter<-s.coefs.meds$smednov+s.coefs.meds$smedjan+s.coefs.meds$smedwinter

##get mean and sd of secchi to destandardize stotalsummer to a value of median secchi for each lake

#mean sec = 3.06
mean(na.omit(inputdata$annual.secchi))
#SD sec = 2.03
sd(na.omit(inputdata$annual.secchi))

##make col of destandardized median P
s.coefs.meds$destdSmed<-(s.coefs.meds$stotalwinter*2.03)+3.06

##add plus 1.7 c data and compute difference
s.coefs.meds.plus<-merge(s.coefs.meds, winterprecipstdplus.lake, by="lagoslakeid", all.x=T, all.y=T)

#mult coef by median plus change 

s.coefs.meds.plus$splusnov<-s.coefs.meds.plus$November.ppt.x1*s.coefs.meds.plus$novplusst
s.coefs.meds.plus$splusjan<-s.coefs.meds.plus$January.ppt*s.coefs.meds.plus$janplusst
s.coefs.meds.plus$spluswinter<-s.coefs.meds.plus$ppt.winter*s.coefs.meds.plus$winterplusst

s.coefs.meds.plus$stotalwinterplus<-s.coefs.meds.plus$splusnov+s.coefs.meds.plus$splusjan+s.coefs.meds.plus$spluswinter

##make col of destandardized plus 1.7 S
s.coefs.meds.plus$destdSplus<-(s.coefs.meds.plus$stotalwinterplus*2.03)+3.06

#average % change in S
s.coefs.meds.plus$pctchangeinS<-(s.coefs.meds.plus$destdSplus-s.coefs.meds.plus$destdSmed)/s.coefs.meds.plus$destdSmed*100

hist(s.coefs.meds.plus$pctchangeinS)

#saveto merge together w temp

p.chgs<-p.coefs.meds.plus[,c(1,21)]
names(p.chgs)<-c("lagoslakeid", "pctPchgppt")
saveRDS(p.chgs, file="Data/ppptchange.rds")
c.chgs<-c.coefs.meds.plus[,c(1,21)]
names(c.chgs)<-c("lagoslakeid", "pctCchgppt")
saveRDS(c.chgs, file="Data/cpptchange.rds")
n.chgs<-n.coefs.meds.plus[,c(1,21)]
names(n.chgs)<-c("lagoslakeid", "pctNchgppt")
saveRDS(n.chgs, file="Data/npptchange.rds")
s.chgs<-s.coefs.meds.plus[,c(1,21)]
names(s.chgs)<-c("lagoslakeid", "pctSchgppt")
saveRDS(s.chgs, file="Data/spptchange.rds")

##use these to merge w/ precip change data later to make a second panel for the effect size boxplot.  do precip in a separate script.