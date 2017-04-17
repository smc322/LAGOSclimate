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

