##want to sum coefs for lambda 3, then compare the sum of all weights to the amount of input data for that lake.  aka, are lakes w/ strong climate relationship due to having enough data to make a strong relationship?


setwd("~/Dropbox/CSI&CL/CSI_LIMNO_Manuscripts-presentations/CSI_ClimateWaterQual/Results/Clustering/WeightsToCluster_April2017/Lambda3_008_AllLakes")

chla.008.w<-read.csv("chla_008_weights.csv", header=T)
tn.008.w<-read.csv("TN_008_weights.csv", header=T)
tp.008.w<-read.csv("TP_008_weights.csv", header=T)
sec.008.w<-read.csv("secc_008_weights.csv", header=T)

chla.rel<-chla.008.w[,c(4:51)]
chla.flip<-abs(data.frame(t(chla.rel)))

chlasummary<- chla.flip %>% summarise_all(sum)
chla.sums<-data.frame(t(chlasummary))
names(chla.sums)<- "chlasum"

lakes.cords<-chla.008.w[,c(1:3)]

chla<-cbind(lakes.cords, chla.sums)


tp.rel<-tp.008.w[,c(4:51)]
tp.flip<-abs(data.frame(t(tp.rel)))

tpsummary<- tp.flip %>% summarise_all(sum)
tp.sums<-data.frame(t(tpsummary))
names(tp.sums)<-"tpsum"

lakes.cords<-tp.008.w[,c(1:3)]

tp<-cbind(lakes.cords, tp.sums)


tn.rel<-tn.008.w[,c(4:51)]
tn.flip<-abs(data.frame(t(tn.rel)))

tnsummary<- tn.flip %>% summarise_all(sum)
tn.sums<-data.frame(t(tnsummary))
names(tn.sums)<-"tnsum"


lakes.cords<-tn.008.w[,c(1:3)]

tn<-cbind(lakes.cords, tn.sums)


sec.rel<-sec.008.w[,c(4:51)]
sec.flip<-abs(data.frame(t(sec.rel)))

secsummary<- sec.flip %>% summarise_all(sum)
sec.sums<-data.frame(t(secsummary))
names(sec.sums)<-"secsum"


lakes.cords<-sec.008.w[,c(1:3)]

sec<-cbind(lakes.cords, sec.sums)


#run limnodata.R to get lakes.limno table... summarize that by number of obs per year per variable

lakes.chla<-lakes.limno[,c(1,2,4)]
lakes.chla<-na.omit(lakes.chla)
chlanobs = count(lakes.chla, "lagoslakeid")
names(chlanobs)<- c("lagoslakeid", "freq.chla")
chla.coefs.nobs<-merge(chlanobs, chla, by="lagoslakeid")
chla.coefs.nobs$freq.chla[is.na(chla.coefs.nobs$freq.chla)] <- 0
chla.coefs.nobs<-na.omit(chla.coefs.nobs)
plot(freq.chla~chlasum, data=chla.coefs.nobs)


lakes.sec<-lakes.limno[,c(1,2,5)]
lakes.sec<-na.omit(lakes.sec)
secnobs = count(lakes.sec, "lagoslakeid")
names(secnobs)<- c("lagoslakeid", "freq.sec")
sec.coefs.nobs<-merge(secnobs, sec, by="lagoslakeid")
sec.coefs.nobs$freq.sec[is.na(sec.coefs.nobs$freq.sec)] <- 0
sec.coefs.nobs<-na.omit(sec.coefs.nobs)
plot(freq.sec~secsum, data=sec.coefs.nobs)


lakes.tp<-lakes.limno[,c(1,2,3)]
lakes.tp<-na.omit(lakes.tp)
tpnobs = count(lakes.tp, "lagoslakeid")
names(tpnobs)<- c("lagoslakeid", "freq.tp")
tp.coefs.nobs<-merge(tpnobs, tp, by="lagoslakeid")
tp.coefs.nobs$freq.tp[is.na(tp.coefs.nobs$freq.tp)] <- 0
tp.coefs.nobs<-na.omit(tp.coefs.nobs)
plot(freq.tp~tpsum, data=tp.coefs.nobs)


lakes.tn<-lakes.limno[,c(1,2,6)]
lakes.tn<-na.omit(lakes.tn)
tnnobs = count(lakes.tn, "lagoslakeid")
names(tnnobs)<- c("lagoslakeid", "freq.tn")
tn.coefs.nobs<-merge(tnnobs, tn, by="lagoslakeid")
tn.coefs.nobs$freq.tn[is.na(tn.coefs.nobs$freq.tn)] <- 0
tn.coefs.nobs<-na.omit(tn.coefs.nobs)
plot(freq.tn~tnsum, data=tn.coefs.nobs)
