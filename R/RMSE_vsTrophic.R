##in response to WRR reviewers, want to see if error is different for oligotrophic vs eutrophic lakes or other issues. Get RMSE for each of the response variables, and compare to median and range of each variable

chlrmse<-na.omit(readRDS("Data/chl_l3_03_rmse.rds"))
names(chlrmse)=c("lagoslakeid", "lat", "lon", "rmse")
nrmse<-na.omit(readRDS("Data/n_l3_03_rmse.rds"))
names(nrmse)=c("lagoslakeid", "lat", "lon", "rmse")
prmse<-na.omit(readRDS("Data/p_l3_03_rmse.rds"))
names(prmse)=c("lagoslakeid", "lat", "lon", "rmse")
secrmse<-na.omit(readRDS("Data/sec_l3_03_rmse.rds"))
names(secrmse)=c("lagoslakeid", "lat", "lon", "rmse")


limnoinput<-readRDS("Data/MTLInputData_feb2017.rds")

chldat<-na.omit(limnoinput[,c("lagoslakeid", "annual.chla")])
tpdat<-na.omit(limnoinput[,c("lagoslakeid", "annual.tp")])
tndat<-na.omit(limnoinput[,c("lagoslakeid", "annual.tn")])
secdat<-na.omit(limnoinput[,c("lagoslakeid", "annual.secchi")])

#summarize median and cv of each of those by lakeid to compare median and variance to rmse
library(dplyr)
cv <- function(x) {return(sd(x)/mean(x))}
chlmeds<- na.omit(chldat %>% group_by(lagoslakeid) %>% summarise(med=median(annual.chla), cv=cv(annual.chla)))

chlall<-merge(chlrmse, chlmeds, by="lagoslakeid")


tpmeds<- na.omit(tpdat %>% group_by(lagoslakeid) %>% summarise(med=median(annual.tp), cv=cv(annual.tp)))

tpall<-merge(prmse, tpmeds, by="lagoslakeid")

tnmeds<- na.omit(tndat %>% group_by(lagoslakeid) %>% summarise(med=median(annual.tn), cv=cv(annual.tn)))

tnall<-merge(nrmse, tnmeds, by="lagoslakeid")

secmeds<- na.omit(secdat %>% group_by(lagoslakeid) %>% summarise(med=median(annual.secchi), cv=cv(annual.secchi)))

secall<-merge(secrmse, secmeds, by="lagoslakeid")


# 4 panel plot of rmse vs median for each?

png("Figures/4panelrmsemed.png",width = 10,height = 10,units = 'in',res=300)
par(mfrow=c(2,2), oma=c(3,4.5,1,1), mar=c(2.5,0,0,0))
plot(log(chlall$rmse)~log(chlall$med), xaxt='n', yaxt='n', ylim=c(-8.2, 2.3), xlim=c(-2.5, 7), pch=21, bg=rgb(192, 192, 192, 150, max=255), col=rgb(105, 105, 105, 255, max=255))
axis(2, at=c(-6.91,-4.61, -2.3,0, 2.3), labels=c("0.001", "0.01","0.1", "1", "10"), cex.axis=1.5)
axis(1, at=c(-2.3, 0, 2.3, 4.6, 6.91), labels=c("0.1", "1", "10", "100", "1000"), cex.axis=1.5)
text(-0.9, 2, "Chlorophyll (ug/L)", cex=1.5)
mtext("RMSE", side=2, line=2.8, cex=1.5, at=-9)
plot(log(secall$rmse)~log(secall$med), xaxt='n', yaxt='n', ylim=c(-8.2, 2.3), xlim=c(-2.5, 2.5), pch=21, bg=rgb(192, 192, 192, 150, max=255), col=rgb(105, 105, 105, 255, max=255))
axis(1, at=c(-2.3, 0, 2.3), labels=c("0.1", "1", "10"), cex.axis=1.5)
text(-2, 2, "Secchi (m)", cex=1.5)
plot(log(tnall$rmse)~log(tnall$med), xaxt='n', yaxt='n', ylim=c(-8.2, 2.4), xlim=c(4.5, 9.5), pch=21, bg=rgb(192, 192, 192, 150, max=255), col=rgb(105, 105, 105, 255, max=255))
axis(2, at=c(-6.91,-4.61, -2.3,0, 2.3), labels=c("0.001", "0.01","0.1", "1", "10"), cex.axis=1.5)
axis(1, at=c(4.61, 6.91, 9.21), labels=c("100", "1,000", "10,000"), cex.axis=1.5)
text(5, 2, "TN (ug/L)", cex=1.5)
mtext("Median", side=1, line=3, cex=1.5, at=9.5)
plot(log(tpall$rmse)~log(tpall$med), xaxt='n', yaxt='n', ylim=c(-8.2, 2.4), xlim=c(0, 6.92), pch=21, bg=rgb(192, 192, 192, 150, max=255), col=rgb(105, 105, 105, 255, max=255))
axis(1, at=c(0, 2.30, 4.61, 6.91), labels=c("1", "10", "100", "1000"), cex.axis=1.5)
text(.6, 2, "TP (ug/L)", cex=1.5)

dev.off()