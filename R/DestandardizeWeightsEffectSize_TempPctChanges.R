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
#nov precip, dec precip, jan precip, feb precip, winter precip
#may temp, june temp, summer temp

## shuai standardized each predictor and response variable by subtracting the mean and dividing by standard deviation.  So, to figure out what the difference between current temp and projected temp is on TP, we would standardize median temp and median plus projected increase (sam created these) based on mean and sd temp of the input that shuai used, then mult by std coef for that variable, then destandardize the result based on the input TP data mean and sd.  Relevant variables are may, june and summer for temp, and nov, jan and winter for precip

relvars<-inputdata[,c(1:4, 15,16,24, 25, 34, 49)]

##first for summer temp example
#mean may temp = 1287
mean(relvars$May.tmean)
#sd may temp = 237
sd(relvars$May.tmean)

#mean june temp = 1807
mean(relvars$June.tmean)
#sd june temp = 218
sd(relvars$June.tmean)

#mean summer temp = 1949
mean(relvars$tmean.summer)
#sd summer temp = 198
sd(relvars$tmean.summer)

#read in sam's summer medians and summer medians plus 1.7deg C projection, then subtract means and divide by sd to sdandardize like shuai did

summertemp<-readRDS(file="Data/hu12_temp_median.rds")

summertemp$MayStd<-(summertemp$May-1287)/237
summertemp$JuneStd<-(summertemp$June-1807)/218
summertemp$summerStd<-(summertemp$summer-1949)/198

stdmedianst<-summertemp[,c(1,5:7)]
names(stdmedianst)<-c("hu12_zoneid", "maymedianst", "junemedianst", "summermedianst")

relvars.ids<-unique(relvars[,c(1,3)])

summertempstdmeds<-merge(relvars.ids, stdmedianst, by="hu12_zoneid", all.x=T, all.y=F)

summertempstdmeds.lake<-summertempstdmeds[,2:5]

#pull out p standardized coeffs for summer temp to multiply by those vals

p.st.coefs<-p.std[,c(1, 8, 9,17)]

p.coefs.meds<-merge(p.st.coefs, summertempstdmeds.lake, by="lagoslakeid", all.x=T, all.y=T)

p.coefs.meds$pmedmay<-p.coefs.meds$May.tmean*p.coefs.meds$maymedianst
p.coefs.meds$pmedjune<-p.coefs.meds$June.tmean*p.coefs.meds$junemedianst
p.coefs.meds$pmedsummer<-p.coefs.meds$tmean.summer*p.coefs.meds$summermedianst

p.coefs.meds$ptotalsummer<-p.coefs.meds$pmedmay+p.coefs.meds$pmedjune+p.coefs.meds$pmedsummer

##get mean and sd of P to destandardize ptotalsummer to a value of median P for each lake

#mean P = 33.15
mean(na.omit(relvars$annual.tp))
#SD P = 59.08
sd(na.omit(relvars$annual.tp))

##make col of destandardized median P
p.coefs.meds$destdPmed<-(p.coefs.meds$ptotalsummer*59.08)+33.15

##now do the same for summer temp plus 1.7degc data
summertemp.plus<-readRDS(file="Data/hu12_temp_plus1point7degC.rds")

summertemp.plus$MayStd<-(summertemp.plus$May-1287)/237
summertemp.plus$JuneStd<-(summertemp.plus$June-1807)/218
summertemp.plus$summerStd<-(summertemp.plus$summer-1949)/198

stdplusst<-summertemp.plus[,c(1,5:7)]
names(stdplusst)<-c("hu12_zoneid", "mayplusst", "juneplusst", "summerplusst")

summertempstdplus<-merge(relvars.ids, stdplusst, by="hu12_zoneid", all.x=T, all.y=F)
summertempstdplus.lake<-summertempstdplus[,2:5]

p.coefs.meds.plus<-merge(p.coefs.meds, summertempstdplus.lake, by="lagoslakeid", all.x=T, all.y=T)

#mult coef by median plus change 

p.coefs.meds.plus$pplusmay<-p.coefs.meds.plus$May.tmean*p.coefs.meds.plus$mayplusst
p.coefs.meds.plus$pplusjune<-p.coefs.meds.plus$June.tmean*p.coefs.meds.plus$juneplusst
p.coefs.meds.plus$pplussummer<-p.coefs.meds.plus$tmean.summer*p.coefs.meds.plus$summerplusst

p.coefs.meds.plus$ptotalsummerplus<-p.coefs.meds.plus$pplusmay+p.coefs.meds.plus$pplusjune+p.coefs.meds.plus$pplussummer

##make col of destandardized plus 1.7 P
p.coefs.meds.plus$destdPplus<-(p.coefs.meds.plus$ptotalsummerplus*59.08)+33.15

#average % change in P
p.coefs.meds.plus$pctchangeinP<-(p.coefs.meds.plus$destdPplus-p.coefs.meds.plus$destdPmed)/p.coefs.meds.plus$destdPmed*100

hist(p.coefs.meds.plus$pctchangeinP)

# cool- this is actually pretty high!  what if we map where the values are?
# map very autocorrelated - not surprising given use of autocorrelation in the model and autocorrelation of the climate data.

cords<-unique(inputdata.nona[,c(1,8,9)])
pchg.latlong<-merge(p.coefs.meds.plus,cords, by="lagoslakeid", all.x=T, all.y=F)

Q1<-quantile(pchg.latlong$pctchangeinP, 0.25, na.rm=TRUE)
Q2<-quantile(pchg.latlong$pctchangeinP, 0.50, na.rm=TRUE)
Q3<-quantile(pchg.latlong$pctchangeinP, 0.75, na.rm=TRUE)
pchg_25<-pchg.latlong[pchg.latlong$pctchangeinP<=Q1,]
pchg_50<-pchg.latlong[pchg.latlong$pctchangeinP>Q1&pchg.latlong$pctchangeinP<=Q2,]
pchg_75<-pchg.latlong[pchg.latlong$pctchangeinP>Q2&pchg.latlong$pctchangeinP<=Q3,]
pchg_100<-pchg.latlong[pchg.latlong$pctchangeinP>Q3,]


library(maps)
library(mapdata)

map("worldHires", "Canada", xlim=c(min(pchg.latlong$nhd_long, na.rm=TRUE)-1,max(pchg.latlong$nhd_long, na.rm=TRUE)+1), ylim=c(min(pchg.latlong$nhd_lat, na.rm=TRUE)-1,max(pchg.latlong$nhd_lat, na.rm=TRUE)+1), fill=TRUE, col="khaki", lwd=2, bg="lightblue1")
map("worldHires", "USa", add=TRUE,  fill=TRUE, col="khaki", lwd=2)
map("state", boundary = FALSE, add = TRUE)
points(pchg_25$nhd_long, pchg_25$nhd_lat, pch=19, col="red", cex=0.5)
points(pchg_50$nhd_long, pchg_50$nhd_lat, pch=19, col="green", cex=.5) 
points(pchg_75$nhd_long, pchg_75$nhd_lat, pch=19, col="blue", cex=0.5)
points(pchg_100$nhd_long, pchg_100$nhd_lat, pch=19, col="black", cex=0.5)
legend(-72, 40, c("<25", "25-50", "50-75", ">75"), pch=19, pt.cex=0.8, cex=.8, col=c("red", "green", "blue", "black"), title="Percentiles", bty="n")


####make %change projections for temp for other three response variables
#use summertempstdmeds.lake and summertemstdplus.lake for the standardized median and plus climate change scenario temps, then load in the coefs for the other variables and proceed as with P above.  should have written a function for this but I'm lazy.

c.st.coefs<-chla.std[,c(1, 8, 9,17)]

c.coefs.meds<-merge(c.st.coefs, summertempstdmeds.lake, by="lagoslakeid", all.x=T, all.y=T)

c.coefs.meds$cmedmay<-c.coefs.meds$May.tmean*c.coefs.meds$maymedianst
c.coefs.meds$cmedjune<-c.coefs.meds$June.tmean*c.coefs.meds$junemedianst
c.coefs.meds$cmedsummer<-c.coefs.meds$tmean.summer*c.coefs.meds$summermedianst

c.coefs.meds$ctotalsummer<-c.coefs.meds$cmedmay+c.coefs.meds$cmedjune+c.coefs.meds$cmedsummer

##get mean and sd of chla to destandardize ctotalsummer to a value of median chla for each lake

#mean chla = 17.18
mean(na.omit(inputdata$annual.chla))
#SD P = 33.25
sd(na.omit(inputdata$annual.chla))

##make col of destandardized median chla
c.coefs.meds$destdCmed<-(c.coefs.meds$ctotalsummer*33.25)+17.18

##add plus 1.7 c data and compute difference
c.coefs.meds.plus<-merge(c.coefs.meds, summertempstdplus.lake, by="lagoslakeid", all.x=T, all.y=T)

#mult coef by median plus change 

c.coefs.meds.plus$cplusmay<-c.coefs.meds.plus$May.tmean*c.coefs.meds.plus$mayplusst
c.coefs.meds.plus$cplusjune<-c.coefs.meds.plus$June.tmean*c.coefs.meds.plus$juneplusst
c.coefs.meds.plus$cplussummer<-c.coefs.meds.plus$tmean.summer*c.coefs.meds.plus$summerplusst

c.coefs.meds.plus$ctotalsummerplus<-c.coefs.meds.plus$cplusmay+c.coefs.meds.plus$cplusjune+c.coefs.meds.plus$cplussummer

##make col of destandardized plus 1.7 chla
c.coefs.meds.plus$destdCplus<-(c.coefs.meds.plus$ctotalsummerplus*33.25)+17.18

#average % change in chla
c.coefs.meds.plus$pctchangeinC<-(c.coefs.meds.plus$destdCplus-c.coefs.meds.plus$destdCmed)/c.coefs.meds.plus$destdCmed*100

hist(c.coefs.meds.plus$pctchangeinC)


#and same thing for N
#pull out n standardized coeffs for summer temp to multiply by those vals

n.st.coefs<-n.std[,c(1, 8, 9,17)]

n.coefs.meds<-merge(n.st.coefs, summertempstdmeds.lake, by="lagoslakeid", all.x=T, all.y=T)

n.coefs.meds$nmedmay<-n.coefs.meds$May.tmean*n.coefs.meds$maymedianst
n.coefs.meds$nmedjune<-n.coefs.meds$June.tmean*n.coefs.meds$junemedianst
n.coefs.meds$nmedsummer<-n.coefs.meds$tmean.summer*n.coefs.meds$summermedianst

n.coefs.meds$ntotalsummer<-n.coefs.meds$nmedmay+n.coefs.meds$nmedjune+n.coefs.meds$nmedsummer

##get mean and sd of N to destandardize ntotalsummer to a value of median N for each lake

#mean N = 819
mean(na.omit(inputdata$annual.tn))
#SD N = 1150
sd(na.omit(inputdata$annual.tn))

##make col of destandardized median N
n.coefs.meds$destdNmed<-(n.coefs.meds$ntotalsummer*1150)+819

##add plus 1.7 c data and compute difference
n.coefs.meds.plus<-merge(n.coefs.meds, summertempstdplus.lake, by="lagoslakeid", all.x=T, all.y=T)

#mult coef by median plus change 

n.coefs.meds.plus$nplusmay<-n.coefs.meds.plus$May.tmean*n.coefs.meds.plus$mayplusst
n.coefs.meds.plus$nplusjune<-n.coefs.meds.plus$June.tmean*n.coefs.meds.plus$juneplusst
n.coefs.meds.plus$nplussummer<-n.coefs.meds.plus$tmean.summer*n.coefs.meds.plus$summerplusst

n.coefs.meds.plus$ntotalsummerplus<-n.coefs.meds.plus$nplusmay+n.coefs.meds.plus$nplusjune+n.coefs.meds.plus$nplussummer

##make col of destandardized plus 1.7 N
n.coefs.meds.plus$destdNplus<-(n.coefs.meds.plus$ntotalsummerplus*1150)+819

#average % change in N
n.coefs.meds.plus$pctchangeinN<-(n.coefs.meds.plus$destdNplus-n.coefs.meds.plus$destdNmed)/n.coefs.meds.plus$destdNmed*100

hist(n.coefs.meds.plus$pctchangeinN)


#and same thing for Secchi
#pull out Sec standardized coeffs for summer temp to multiply by those vals

s.st.coefs<-sec.std[,c(1, 8, 9,17)]

s.coefs.meds<-merge(s.st.coefs, summertempstdmeds.lake, by="lagoslakeid", all.x=T, all.y=T)

s.coefs.meds$smedmay<-s.coefs.meds$May.tmean*s.coefs.meds$maymedianst
s.coefs.meds$smedjune<-s.coefs.meds$June.tmean*s.coefs.meds$junemedianst
s.coefs.meds$smedsummer<-s.coefs.meds$tmean.summer*s.coefs.meds$summermedianst

s.coefs.meds$stotalsummer<-s.coefs.meds$smedmay+s.coefs.meds$smedjune+s.coefs.meds$smedsummer

##get mean and sd of secchi to destandardize stotalsummer to a value of median secchi for each lake

#mean sec = 3.06
mean(na.omit(inputdata$annual.secchi))
#SD sec = 2.03
sd(na.omit(inputdata$annual.secchi))

##make col of destandardized median P
s.coefs.meds$destdSmed<-(s.coefs.meds$stotalsummer*2.03)+3.06

##add plus 1.7 c data and compute difference
s.coefs.meds.plus<-merge(s.coefs.meds, summertempstdplus.lake, by="lagoslakeid", all.x=T, all.y=T)

#mult coef by median plus change 

s.coefs.meds.plus$splusmay<-s.coefs.meds.plus$May.tmean*s.coefs.meds.plus$mayplusst
s.coefs.meds.plus$splusjune<-s.coefs.meds.plus$June.tmean*s.coefs.meds.plus$juneplusst
s.coefs.meds.plus$splussummer<-s.coefs.meds.plus$tmean.summer*s.coefs.meds.plus$summerplusst

s.coefs.meds.plus$stotalsummerplus<-s.coefs.meds.plus$splusmay+s.coefs.meds.plus$splusjune+s.coefs.meds.plus$splussummer

##make col of destandardized plus 1.7 S
s.coefs.meds.plus$destdSplus<-(s.coefs.meds.plus$stotalsummerplus*2.03)+3.06

#average % change in S
s.coefs.meds.plus$pctchangeinS<-(s.coefs.meds.plus$destdSplus-s.coefs.meds.plus$destdSmed)/s.coefs.meds.plus$destdSmed*100

hist(s.coefs.meds.plus$pctchangeinS)

#merge all temp pct changes together

p.chgs<-p.coefs.meds.plus[,c(1,21)]
names(p.chgs)<-c("lagoslakeid", "pctPchgtemp")
saveRDS(p.chgs, file="Data/ptempchange.rds")
c.chgs<-c.coefs.meds.plus[,c(1,21)]
names(c.chgs)<-c("lagoslakeid", "pctCchgtemp")
saveRDS(c.chgs, file="Data/ctempchange.rds")
n.chgs<-n.coefs.meds.plus[,c(1,21)]
names(n.chgs)<-c("lagoslakeid", "pctNchgtemp")
saveRDS(n.chgs, file="Data/ntempchange.rds")
s.chgs<-s.coefs.meds.plus[,c(1,21)]
names(s.chgs)<-c("lagoslakeid", "pctSchgtemp")
saveRDS(s.chgs, file="Data/stempchange.rds")

##use these to merge w/ precip change data later to make a second panel for the effect size boxplot.  do precip in a separate script.