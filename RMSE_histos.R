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


#Aug 2017: merge together RMSE and lakes that have data files to see how many overlap with zero.  start with the predictors we already identified as important in barplots of winter precip and summer temp, and just for secchi b/c lots of data and error estimates for that

sec.imptpreds<-sec.datalakes[,c(1,8,9,17,18,27,42,44)]
names(s.l3.003) <- c("lagoslakeid", "lat", "lon", "RMSE")
sec.imptpreds.rmse<-merge(sec.imptpreds, s.l3.003, by="lagoslakeid", all.x=FALSE, all.y=TRUE)

#hhhmmmmm, but how do we deal with one RMSE for all variables?  then we don't know if they overlap with zero?  need to revisit this post ESA but don't have time now... but can map RMSE and show where error is high to correspond with where cluster membership occurs?

library(maps)
library(mapdata)

Q1<-quantile(sec.imptpreds.rmse$RMSE, 0.25, na.rm=TRUE)
Q2<-quantile(sec.imptpreds.rmse$RMSE, 0.50, na.rm=TRUE)
Q3<-quantile(sec.imptpreds.rmse$RMSE, 0.75, na.rm=TRUE)
rmse_25<-sec.imptpreds.rmse[sec.imptpreds.rmse$RMSE<=Q1,]
rmse_50<-sec.imptpreds.rmse[sec.imptpreds.rmse$RMSE>Q1&sec.imptpreds.rmse$RMSE<=Q2,]
rmse_75<-sec.imptpreds.rmse[sec.imptpreds.rmse$RMSE>Q2&sec.imptpreds.rmse$RMSE<=Q3,]
rmse_100<-sec.imptpreds.rmse[sec.imptpreds.rmse$RMSE>Q3,]

map("worldHires", "Canada", xlim=c(min(sec.imptpreds.rmse$lon, na.rm=TRUE)-1,max(sec.imptpreds.rmse$lon, na.rm=TRUE)+1), ylim=c(min(sec.imptpreds.rmse$lat, na.rm=TRUE)-1,max(sec.imptpreds.rmse$lat, na.rm=TRUE)+1), fill=TRUE, col="khaki", lwd=2, bg="lightblue1")
map("worldHires", "USa", add=TRUE,  fill=TRUE, col="khaki", lwd=2)
map("state", boundary = FALSE, add = TRUE)
text(-78, 49, "LAGOS GEO HUC12 Groundwater Baseflow", cex=.75)
points(rmse_25$lon, rmse_25$lat, pch=19, col="red", cex=0.5)  
points(rmse_50$lon, rmse_50$lat, pch=19, col="green", cex=0.5) 
points(rmse_75$lon, rmse_75$lat, pch=19, col="blue", cex=0.5)
points(rmse_100$lon, rmse_100$lat, pch=19, col="black", cex=0.5)
legend(-72, 40, c("<25", "25-50", "50-75", ">75"), pch=19, pt.cex=0.8, cex=.8, col=c("red", "green", "blue", "black"), title="Baseflow\nPercentiles", bty="n")


hist(sec.imptpreds.rmse$RMSE)

setwd("~/Dropbox/Sarah_Work/Manuscripts/2017_LAGOSClimateWaterqual/ResultsSummaryStats")
jfclusters<-read.csv("secchiclusters_aug2017.csv", header=T)

plot(jfclusters$Prin1~jfclusters$Prin2)
summary(jfclusters)

cluster1<-jfclusters[jfclusters$Cluster_all.variables==1,]
cluster2<-jfclusters[jfclusters$Cluster_all.variables==2,]
cluster3<-jfclusters[jfclusters$Cluster_all.variables==3,]
cluster4<-jfclusters[jfclusters$Cluster_all.variables==4,]
cluster5<-jfclusters[jfclusters$Cluster_all.variables==5,]


map("worldHires", "Canada", xlim=c(min(jfclusters$Lon, na.rm=TRUE)-1,max(jfclusters$Lon, na.rm=TRUE)+1), ylim=c(min(jfclusters$Lat, na.rm=TRUE)-1,max(jfclusters$Lat, na.rm=TRUE)+1), fill=TRUE, col="white", lwd=2, bg="aliceblue")
map("worldHires", "USa", add=TRUE,  fill=TRUE, col="white", lwd=2)
map("state", boundary = FALSE, add = TRUE)
points(cluster1$Lon, cluster1$Lat, pch=19, col="grey30", cex=0.4)  
points(cluster5$Lon, cluster5$Lat, pch=10, col="lightgoldenrod2", cex=0.4)  
points(cluster4$Lon, cluster4$Lat, pch=19, col="#2c7bb6", cex=0.4)  
points(cluster2$Lon, cluster2$Lat, pch=19, col="#fd8d3c", cex=0.4)  
points(cluster3$Lon, cluster3$Lat, pch=19, col="#d7191c", cex=0.4)  
legend(-72, 40, c("1", "2", "3", "4", "5"), pch=19, pt.cex=1, cex=1, col=c("grey30", "#fd8d3c", "#d7191c", "#2c7bb6", "lightgoldenrod2"), title="Clusters", bty="n")

