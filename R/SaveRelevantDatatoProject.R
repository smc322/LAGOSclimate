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


#limno data - input file that Shuai used for FEb 2017 analysis
setwd("~/Dropbox/CSI&CL/CSI_LIMNO_Manuscripts-presentations/CSI_ClimateWaterQual/Data/updated_feb2017")

feb2017inputdata<-read.csv("ClimateCompiled_1.087_updated_Feb2017_summer.csv", header=T)

  setwd("~/Dropbox/Sarah_Work/Manuscripts/2017_LAGOSClimateWaterqual/LAGOSclimate")

saveRDS(feb2017inputdata, file="Data/MTLInputData_feb2017.rds")


#combined file I sent to SY for MTL analysis from climatelimnomerge.R

saveRDS(limno.all.climate.palmer, file="Data/LimnoandClimateforMTLanalysis.rds")

#and original palmer, nao and enso from climatelimnomerge.R

phdi.seasons.nona<-na.omit(phdi.seasons)
saveRDS(phdi.seasons.nona, file="Data/PHDI.rds")

saveRDS(annual.enso, file="Data/ENSO.rds")
saveRDS(annual.nao, file="Data/NAO.rds")


#lambda 3 = .03 weights for April 2017 results for each of four response variables. File info in weight summary

saveRDS(n.weights, file="Data/n_l3_03.rds")
saveRDS(p.weights, file="Data/p_l3_03.rds")
saveRDS(secchi.weights, file="Data/sec_l3_03.rds")
saveRDS(chla.weights, file="Data/chl_l3_03.rds")

###RMSE data for maps

setwd("~/Dropbox/Sarah_Work/Manuscripts/2017_LAGOSClimateWaterqual/Data/RMSE_April2017")

n.l3.003<-read.csv("TN_lambda3_003.csv", header=T)
p.l3.003<-read.csv("TP_lambda3_003.csv", header=T)
s.l3.003<-read.csv("Secchi_lambda3_003.csv", header=T)
c.l3.003<-read.csv("Chla_lambda3_003.csv", header=T)

n.l3.005<-read.csv("TN_lambda3_005.csv", header=T)
p.l3.005<-read.csv("TP_lambda3_005.csv", header=T)
s.l3.005<-read.csv("Secchi_lambda3_005.csv", header=T)
c.l3.005<-read.csv("Chla_lambda3_005.csv", header=T)

setwd("~/Dropbox/Sarah_Work/Manuscripts/2017_LAGOSClimateWaterqual/LAGOSclimate")

saveRDS(n.l3.003, file="Data/n_l3_03_rmse.rds")
saveRDS(p.l3.003, file="Data/p_l3_03_rmse.rds")
saveRDS(s.l3.003, file="Data/sec_l3_03_rmse.rds")
saveRDS(c.l3.003, file="Data/chl_l3_03_rmse.rds")

saveRDS(n.l3.005, file="Data/n_l3_05_rmse.rds")
saveRDS(p.l3.005, file="Data/p_l3_05_rmse.rds")
saveRDS(s.l3.005, file="Data/sec_l3_05_rmse.rds")
saveRDS(c.l3.005, file="Data/chl_l3_05_rmse.rds")