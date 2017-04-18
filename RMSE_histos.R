###want to confirm that there are no major changes in distribution, mean, sd, etc of RMSE when we change lambda 3.  We know that there are small changes in overall RMSE, but need to check out lake specific RMSE to make sure there aren't certain lakes that get huge.  Also generally interested inw hether some lakes have very high error, how many of them, etc for making cutoffs for clusters.

setwd("~/Dropbox/Sarah_Work/Manuscripts/2017_LAGOSClimateWaterqual/Data/RMSE_April2017")

n.l3.003<-read.csv("TN_lambda3_003.csv", header=T)
p.l3.003<-read.csv("TP_lambda3_003.csv", header=T)
s.l3.003<-read.csv("Secchi_lambda3_003.csv", header=T)
c.l3.003<-read.csv("Chla_lambda3_003.csv", header=T)

n.l3.005<-read.csv("TN_lambda3_005.csv", header=T)
p.l3.005<-read.csv("TP_lambda3_005.csv", header=T)
s.l3.005<-read.csv("Secchi_lambda3_005.csv", header=T)
c.l3.005<-read.csv("Chla_lambda3_005.csv", header=T)

s.l3.008<-read.csv("Secchi_lambda3_008.csv", header=T)

n.l3.003<-na.omit(n.l3.003)
p.l3.003<-na.omit(p.l3.003)
s.l3.003<-na.omit(s.l3.003)
c.l3.003<-na.omit(c.l3.003)

n.l3.005<-na.omit(n.l3.005)
p.l3.005<-na.omit(p.l3.005)
s.l3.005<-na.omit(s.l3.005)
c.l3.005<-na.omit(c.l3.005)

s.l3.008<-na.omit(s.l3.008)




hist(s.l3.003$RMSE)
hist(s.l3.005$RMSE, add=T, col="green")


#looks pretty similar regardless of lambda3 choice (.003 vs .005 for everything but secchi, also .008 for that because it seemed relevant based on Shuai's summary), totally overlapping distributions and more or less same mean and sd for rmse.  Perhaps do a cutoff of one SD RMSE for the subset of lakes with data for JF?

##make cutoff of RMSE plus one SD for eliminating lakes with very high error from clustering analysis


##dataframes of regression weights for lakes that had data (from subsettingforclusters_dec2016.R)
setwd("~/Dropbox/Sarah_Work/Manuscripts/2017_LAGOSClimateWaterqual/Data/RegressionWeights_April2017")

n.datalakes<-read.csv("TN_lambda3_003_datalakes.csv", header=T)
n.rmse.cutoff<-mean(n.l3.003$RMSE)+sd(n.l3.003$RMSE)
n.rmse.trimmed<-n.l3.003[n.l3.003$RMSE<n.rmse.cutoff,]
n.lakelist<-data.frame(n.rmse.trimmed[,1])
names(n.lakelist)<-"lagoslakeid"
n.weights.rmse<-merge(n.datalakes, n.lakelist, by="lagoslakeid", all.x=F, all.y=T)
write.csv(n.weights.rmse, "TN_lambda3_003_rmselakes.csv")

p.datalakes<-read.csv("TP_lambda3_003_datalakes.csv", header=T)
p.rmse.cutoff<-mean(p.l3.003$RMSE)+sd(p.l3.003$RMSE)
p.rmse.trimmed<-p.l3.003[p.l3.003$RMSE<p.rmse.cutoff,]
p.lakelist<-data.frame(p.rmse.trimmed[,1])
names(p.lakelist)<-"lagoslakeid"
p.weights.rmse<-merge(p.datalakes, p.lakelist, by="lagoslakeid", all.x=F, all.y=T)
write.csv(p.weights.rmse, "TP_lambda3_003_rmselakes.csv")

chla.datalakes<-read.csv("Chla_lambda3_003_datalakes.csv", header=T)
chla.rmse.cutoff<-mean(c.l3.003$RMSE)+sd(c.l3.003$RMSE)
chla.rmse.trimmed<-c.l3.003[c.l3.003$RMSE<chla.rmse.cutoff,]
chla.lakelist<-data.frame(chla.rmse.trimmed[,1])
names(chla.lakelist)<-"lagoslakeid"
chla.weights.rmse<-merge(chla.datalakes, chla.lakelist, by="lagoslakeid", all.x=F, all.y=T)
write.csv(chla.weights.rmse, "Chla_lambda3_003_rmselakes.csv")


sec.datalakes<-read.csv("Secchi_lambda3_003_datalakes.csv", header=T)
sec.rmse.cutoff<-mean(s.l3.003$RMSE)+sd(s.l3.003$RMSE)
sec.rmse.trimmed<-s.l3.003[s.l3.003$RMSE<sec.rmse.cutoff,]
sec.lakelist<-data.frame(sec.rmse.trimmed[,1])
names(sec.lakelist)<-"lagoslakeid"
sec.weights.rmse<-merge(sec.datalakes, sec.lakelist, by="lagoslakeid", all.x=F, all.y=T)
write.csv(sec.weights.rmse, "Secchi_lambda3_003_rmselakes.csv")
