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
par(mfrow=c(2,2), oma=c(4,4,1,1), mar=c(0,0,0,0))
plot(log(chlall$rmse)~log(chlall$med))
mtext("log(RMSE)", side=2, line=2.5, cex=1.5, at=-9)
plot(log(secall$rmse)~log(secall$med))
plot(log(tnall$rmse)~log(tnall$med))
mtext("log(Median)", side=1, line=2.5, cex=1.5, at=9.5)
plot(log(tpall$rmse)~log(tpall$med))

dev.off()