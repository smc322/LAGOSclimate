###SMC 27 September 2017
##want to add a bunch of RData files to project for things that are only stored as CSVs on SMC dropbox/harddrive

#climate data
setwd("~/Dropbox/Sarah_Work/Manuscripts/2017_LAGOSClimateWaterqual/Data/Annual_monthly_calculated")
hu12.tmean.annual<-read.csv("hu12_tmean_annual.csv", header=T)
hu12.tmin.annual<-read.csv("hu12_tmin_annual.csv", header=T)
hu12.tmax.annual<-read.csv("hu12_tmax_annual.csv", header=T)
hu12.ppt.annual<-read.csv("hu12_ppt_annual.csv", header=T)
hu12.tmean.season<-read.csv("hu12_tmean_seasonal_updated.csv", header=T)
hu12.ppt.season<-read.csv("hu12_ppt_seasonal_updated.csv", header=T)

setwd("~/Dropbox/Sarah_Work/Manuscripts/2017_LAGOSClimateWaterqual/LAGOSclimate")
saveRDS(hu12.tmean.annual, file="Data/hu12.tmean.annual.rds")
saveRDS(hu12.tmin.annual, file="Data/hu12.tmin.annual.rds")
saveRDS(hu12.tmax.annual, file="Data/hu12.tmax.annual.rds")
saveRDS(hu12.ppt.annual, file="Data/hu12.ppt.annual.rds")
saveRDS(hu12.tmean.season, file="Data/hu12.tmean.season.rds")
saveRDS(hu12.ppt.season, file="Data/hu12.ppt.season.rds")


#limno data - run script to get lakes.limno

saveRDS(lakes.limno, file="Data/LimnoData.rds")


#combined file I sent to SY for MTL analysis from climatelimnomerge.R

saveRDS(limno.all.climate.palmer, file="Data/LimnoandClimateforMTLanalysis.rds")
