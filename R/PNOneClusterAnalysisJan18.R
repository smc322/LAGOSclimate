##Pang ning looked at distance from the center of each lake for the single cluster that they form.  Some are closely related to centroid, others very different.  He outputted distances - want to look at whether the outliers from the cluster are just lakes with very little/no data, or whether they're in certian geographic locations or anything else that we wanted to learn if we'd found more than one cluster.

##read in files from PNT and save to data folder in R project

setwd("~/Dropbox/Sarah_Work/Manuscripts/2017_LAGOSClimateWaterqual/ResultsSummaryStats/SingleClusterPNTJan2018")
chla.cluster.cor<-read.csv("chlaweights-corr.csv", header=T)
sec.cluster.cor<-read.csv("secchiweights-corr.csv", header=T)
n.cluster.cor<-read.csv("nweights-corr.csv", header=T)
p.cluster.cor<-read.csv("pweights-corr.csv", header=T)

chla.cluster.cor$ChlCorrelation<-1-chla.cluster.cor$dist_to_clusterCenter
sec.cluster.cor$SecCorrelation<-1-sec.cluster.cor$dist_to_clusterCenter
p.cluster.cor$PCorrelation<-1-p.cluster.cor$dist_to_clusterCenter
n.cluster.cor$NCorrelation<-1-n.cluster.cor$dist_to_clusterCenter

setwd("~/Dropbox/Sarah_Work/Manuscripts/2017_LAGOSClimateWaterqual/LAGOSclimate")
saveRDS(chla.cluster.cor, file="Data/ChlaClusterCorr.rds")
saveRDS(sec.cluster.cor, file="Data/SecchiClusterCorr.rds")
saveRDS(n.cluster.cor, file="Data/NClusterCorr.rds")
saveRDS(p.cluster.cor, file="Data/PClusterCorr.rds")



