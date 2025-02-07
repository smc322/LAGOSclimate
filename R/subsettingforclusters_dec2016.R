##shuai produced weights for all lakes regardless of whether they have data or not.  want to subset that to initial list of lakes that we had data for each variable for clustering so we are only working with weights that resulted from lakes with observations for a given response variable.

lakes.with.n<-lakes.limno[complete.cases(lakes.limno[,6]),]
lakes.with.p<-lakes.limno[complete.cases(lakes.limno[,3]),]
lakes.with.secchi<-lakes.limno[complete.cases(lakes.limno[,5]),]
lakes.with.chla<-lakes.limno[complete.cases(lakes.limno[,4]),]

n.idlist<-data.frame(unique(lakes.with.n$lagoslakeid))
names(n.idlist)<-"lagoslakeid"
p.idlist<-data.frame(unique(lakes.with.p$lagoslakeid))
names(p.idlist)<-"lagoslakeid"
secchi.idlist<-data.frame(unique(lakes.with.secchi$lagoslakeid))
names(secchi.idlist)<-"lagoslakeid"
chla.idlist<-data.frame(unique(lakes.with.chla$lagoslakeid))
names(chla.idlist)<-"lagoslakeid"

#read in weights tables for trimming

setwd("~/Dropbox/Sarah_Work/Manuscripts/2017_LAGOSClimateWaterqual/Data/RegressionWeights_Dec2016")

n.weights<-read.csv("nweights.csv", header=T)
n.weights.data<-merge(n.idlist, n.weights, by="lagoslakeid", all.x=T, all.y=F)
write.csv(n.weights.data, "nweights_datalakes.csv")

p.weights<-read.csv("pweights.csv", header=T)
p.weights.data<-merge(p.idlist, p.weights, by="lagoslakeid", all.x=T, all.y=F)
write.csv(p.weights.data, "pweights_datalakes.csv")

secchi.weights<-read.csv("secchiweights.csv", header=T)
secchi.weights.data<-merge(secchi.idlist, secchi.weights, by="lagoslakeid", all.x=T, all.y=F)
write.csv(secchi.weights.data, "secchiweights_datalakes.csv")

chla.weights<-read.csv("chlaweights.csv", header=T)
chla.weights.data<-merge(chla.idlist, chla.weights, by="lagoslakeid", all.x=T, all.y=F)
write.csv(chla.weights.data, "chlaweights_datalakes.csv")


##or updated here for april 2017 data, lambda .003

setwd("~/Dropbox/Sarah_Work/Manuscripts/2017_LAGOSClimateWaterqual/Data/RegressionWeights_April2017")

n.weights<-read.csv("TN_lambda3_003.csv", header=T)
n.weights.data<-merge(n.idlist, n.weights, by="lagoslakeid", all.x=T, all.y=F)
n.weights.data<-na.omit(n.weights.data)
write.csv(n.weights.data, "TN_lambda3_003_datalakes.csv")

p.weights<-read.csv("TP_lambda3_003.csv", header=T)
p.weights.data<-merge(p.idlist, p.weights, by="lagoslakeid", all.x=T, all.y=F)
p.weights.data<-na.omit(p.weights.data)
write.csv(p.weights.data, "TP_lambda3_003_datalakes.csv")

secchi.weights<-read.csv("Secchi_lambda3_003.csv", header=T)
secchi.weights.data<-merge(secchi.idlist, secchi.weights, by="lagoslakeid", all.x=T, all.y=F)
secchi.weights.data<-na.omit(secchi.weights.data)
write.csv(secchi.weights.data, "Secchi_lambda3_003_datalakes.csv")

chla.weights<-read.csv("Chla_lambda3_003.csv", header=T)
chla.weights.data<-merge(chla.idlist, chla.weights, by="lagoslakeid", all.x=T, all.y=F)
chla.weights.data<-na.omit(chla.weights.data)
write.csv(chla.weights.data, "Chla_lambda3_003_datalakes.csv")

