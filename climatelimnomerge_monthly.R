#merging limno data and climate data into a single table for Shuai to use.  horizontal format here, but could switch to vertical if needed?

#merge by hu12, include all limno data but only include climate data for years where we have limno data for years and lakes where we have data for at least one limno variable (secchi tp or chla)

#from limno script, need dataframe lakes.limno (limno data, lakeid, year, hu12, hu4 identifiers, and lat long data in case we need it)
#from climate script, need 
#add a second year column to the limno data that is called "yearx1" to match the one with the modified yar in climate data, but it shoudl stay the same year so we can match them...

setwd("~/Dropbox/Sarah_Work/Manuscripts/2016_climate_waterqual/Data/Annual_monthly_calculated")


hu12.tmean.annual<-read.csv("hu12_tmean_annual.csv", header=T)
hu12.tmin.annual<-read.csv("hu12_tmin_annual.csv", header=T)
hu12.tmax.annual<-read.csv("hu12_tmax_annual.csv", header=T)
hu12.ppt.annual<-read.csv("hu12_ppt_annual.csv", header=T)
hu12.tmean.season<-read.csv("hu12_tmean_seasonal.csv", header=T)
hu12.ppt.season<-read.csv("hu12_ppt_seasonal.csv", header=T)


#separate out relevant columns and merge, add a second year column for x+1 that can be merged with the other year info to get previous years climate data with limno data (this seems like cheating but I was too lazy to figure out how to tell it to add each variable for year x-1 and this should work) Will just need to create a duplicate year column in the limno dataset that's called the same thing as the adjusted one here.

tmean.ann<-hu12.tmean.annual[,c(2:16)]
names(tmean.ann)<-c("hu12_zoneid", "year",  "January.tmean", "February.tmean", "March.tmean", "April.tmean",  "May.tmean", "June.tmean", "July.tmean", "August.tmean", "September.tmean", "October.tmean", "November.tmean","December.tmean", "tmean.annual")
tmin.ann<-hu12.tmin.annual[,c(2, 3, 16)]
names(tmin.ann)<-c("hu12_zoneid", "year", "tmin.annual")
tmax.ann<-hu12.tmax.annual[,c(2, 3, 16)]
names(tmax.ann)<-c("hu12_zoneid", "year", "tmax.annual")
tmean.seas<-hu12.tmean.season[,c(2,3,16:19)]
names(tmean.seas)<-c("hu12_zoneid", "year", "tmean.winter", "tmean.spring", "tmean.summer", "tmean.fall")
precip.ann<-hu12.ppt.annual[,c(2:16)]
names(precip.ann)<-c("hu12_zoneid", "year",  "January.ppt", "February.ppt", "March.ppt", "April.ppt",  "May.ppt", "June.ppt", "July.ppt", "August.ppt", "September.ppt", "October.ppt", "November.ppt","December.ppt", "precip.annual")
precip.seas<-hu12.ppt.season[,c(2,3,16:19)]
names(precip.seas)<-c("hu12_zoneid", "year", "ppt.winter", "ppt.spring", "ppt.summer", "ppt.fall")

annmeanmax<-merge(tmean.ann, tmax.ann, by=c("hu12_zoneid", "year"))
annmeanmaxmin<-merge(annmeanmax, tmin.ann, by=c("hu12_zoneid", "year"))
annseasont<-merge(annmeanmaxmin, tmean.seas, by=c("hu12_zoneid", "year"))
annseasontp<-merge(annseasont, precip.ann, by=c("hu12_zoneid", "year"))
annseasontpsp<-merge(annseasontp, precip.seas, by=c("hu12_zoneid", "year"))


annseasontpsp$yrx1<-annseasontpsp$year+1


#add indicies here too
setwd("~/Dropbox/Sarah_Work/Manuscripts/2016_climate_waterqual/Data/Climate_raw")


nao<-read.csv("NAO.csv", header=T)
pdsi<-read.csv("PDSI 1970-2013 Annual Values by State.csv", header=T)
enso<-read.csv("ENSO_INDEX.csv", header=T)

##ENSO and NAO have monthly data, but for now, just aggregate by year.  
##PDSI is by state, tried to get that to work but it was a pain in the ass, figure out later...

annual.enso = aggregate(enso[,c("ESPI")], by=list(enso$Year), FUN="mean")
names(annual.enso)<-c("year", "enso.espi")

climate.enso<-merge(annseasontpsp, annual.enso, by="year")

annual.nao = aggregate(nao[,c("NAO_INDEX")], by=list(nao$YEAR), FUN="mean")
names(annual.nao)<-c("year", "nao.index")

climate.enso.nao<-merge(climate.enso, annual.nao, by="year")

hu12.state<-data.lake.specific[,c(20, 26)]
hu12.state<-unique(hu12.state)
climate.enso.nao.state<-merge(climate.enso.nao, hu12.state, by="hu12_zoneid")
names(pdsi)<-c("state_name", "year", "PDSI.value", "PDSI.anomaly")
climate.all<-merge(climate.enso.nao.state, pdsi, by=c("state_name", "year"))

head(lakes.limno)
lakes.limno$yrx1<-lakes.limno$year
climate.yearx<-climate.all[,c(2:11,16:21,23:30,35:38,41:44 )]
climate.yearx1<-climate.all[,c(3, 12:18, 22,31:35,39:44)]
names(climate.yearx1)<-c("hu12_zoneid", "September.tmean.x1","October.tmean.x1", "November.tmean.x1", "December.tmean.x1", "tmean.annual.x1", "tmax.annual.x1", "tmin.annual.x1", "tmean.fall.x1", "September.ppt.x1", "October.ppt.x1", "November.ppt.x1", "December.ppt.x1", "precip.annual.x1", "ppt.fall.x1", "yrx1", "enso.espi.x1", "nao.index.x1", "PDSI.value.x1", "PDSI.anomaly.x1")

limno.climatex<-merge(lakes.limno, climate.yearx, by=c("hu12_zoneid", "year"), all.x=TRUE, all.y=FALSE)
limno.allclimate<-merge(limno.climatex, climate.yearx1, by=c("hu12_zoneid", "yrx1"), all.x=TRUE, all.y=FALSE)
limno.allclimate$yrx1=NULL


setwd("~/Dropbox/Sarah_Work/Manuscripts/2016_climate_waterqual/Data/SummaryforShuai")
write.csv(limno.allclimate, "ClimateCompiled_1.054_initialclim_Oct2016.csv")
