#save number of obeservations for each variable for use in other analyses, and calculate 5 and 15 year data lakes for table of limno data

setwd("~/Dropbox/Sarah_Work/Manuscripts/2017_LAGOSClimateWaterqual/LAGOSclimate")
limnodata<-readRDS(file="Data/LimnoData.rds")

pdata<-na.omit(limnodata[,c(1:3,7:10)])
chladata<-na.omit(limnodata[,c(1:2,4,7:10 )])
secdata<-na.omit(limnodata[,c(1:2, 5, 7:10)])
ndata<-na.omit(limnodata[,c(1:2, 6, 7:10)])

library(dplyr)
pnobs<-count(pdata, lagoslakeid)
names(pnobs)<-c("lagoslakeid", "nobs_p")
p.5yr<-pnobs[pnobs$nobs_p>4,]
p.15yr<-pnobs[pnobs$nobs_p>14,]


chlanobs<-count(chladata, lagoslakeid)
names(chlanobs)<-c("lagoslakeid", "nobs_chla")
chla.5yr<-chlanobs[chlanobs$nobs_chla>4,]
chla.15yr<-chlanobs[chlanobs$nobs_chla>14,]

secnobs<-count(secdata, lagoslakeid)
names(secnobs)<-c("lagoslakeid", "nobs_sec")
sec.5yr<-secnobs[secnobs$nobs_sec>4,]
sec.15yr<-secnobs[secnobs$nobs_sec>14,]

nnobs<-count(ndata, lagoslakeid)
names(nnobs)<-c("lagoslakeid", "nobs_n")
n.5yr<-nnobs[nnobs$nobs_n>4,]
n.15yr<-nnobs[nnobs$nobs_n>14,]

#save NOBS with lagoslakeid for each variable for use elsewhere
setwd("~/Dropbox/Sarah_Work/Manuscripts/2017_LAGOSClimateWaterqual/LAGOSclimate")
saveRDS(nnobs, file="Data/nobs_n.rds")
saveRDS(pnobs, file="Data/nobs_p.rds")
saveRDS(chlanobs, file="Data/nobs_chla.rds")
saveRDS(secnobs, file="Data/nobs_sec.rds")
