##want to sum impt coefs for winter precip and summer temp and make boxplots of each response variable for each sum

setwd("~/Dropbox/Sarah_Work/Manuscripts/2017_LAGOSClimateWaterqual/LAGOSclimate")

#Standardized coefficients
n.std<-readRDS(file="Data/n_l3_03.rds")
p.std<-readRDS(file="Data/p_l3_03.rds")
sec.std<-readRDS(file="Data/sec_l3_03.rds")
chla.std<-readRDS(file="Data/chl_l3_03.rds")

n.std$winterprecipsum.n<-n.std$ppt.winter+n.std$November.ppt.x1+n.std$January.ppt

p.std$winterprecipsum.p<-p.std$ppt.winter+p.std$November.ppt.x1+p.std$January.ppt

sec.std$winteprecipsum.sec<-sec.std$ppt.winter+sec.std$November.ppt.x1+sec.std$January.ppt

chla.std$winteprecipsum.chla<-chla.std$ppt.winter+chla.std$November.ppt.x1+chla.std$January.ppt

sec.std$summertempsum.sec<-sec.std$May.tmean+sec.std$June.tmean+sec.std$tmean.summer

chla.std$summertempsum.chla<-chla.std$May.tmean+chla.std$June.tmean+chla.std$tmean.summer

p.std$summertempsum.p<-p.std$May.tmean+p.std$June.tmean+p.std$tmean.summer

n.std$summertempsum.n<-n.std$May.tmean+n.std$June.tmean+n.std$tmean.summer

n.sums<-n.std[,c(1,52,53)]
p.sums<-p.std[,c(1,52,53)]
chla.sums<-chla.std[,c(1,52,53)]
sec.sums<-sec.std[,c(1,52,53)]

chla.secchi.sums<-merge(chla.sums, sec.sums, by="lagoslakeid", all.x=T, all.y=T)
chla.secchi.p.sums<-merge(chla.secchi.sums, p.sums, by="lagoslakeid", all.x=T, all.y=T)
chla.secchi.p.n.sums<-merge(chla.secchi.p.sums, n.sums, by="lagoslakeid", all.x=T, all.y=T)

##read in data for second panel
c.ppt<-readRDS(file="Data/cpptchange.rds")
c.temp<-readRDS(file="Data/ctempchange.rds")
s.ppt<-readRDS(file="Data/spptchange.rds")
s.temp<-readRDS(file="Data/stempchange.rds")
p.ppt<-readRDS(file="Data/ppptchange.rds")
p.temp<-readRDS(file="Data/ptempchange.rds")
n.ppt<-readRDS(file="Data/npptchange.rds")
n.temp<-readRDS(file="Data/ntempchange.rds")

c<-merge(c.ppt, c.temp, by="lagoslakeid")
cs<-merge(c, s.ppt, by="lagoslakeid")
cs2<-merge(cs, s.temp, by="lagoslakeid")
csp<-merge(cs2, p.ppt, by="lagoslakeid")
csp2<-merge(csp, p.temp, by="lagoslakeid")
cspn<-merge(csp2, n.ppt, by="lagoslakeid")
all.pctchg<-merge(cspn, n.temp, by="lagoslakeid")


#can make boxplot from cols of a matrix with package sfsmisc and boxplot.matrix function. merge winterprecip sums and early summer temp sums into a matrix
library(sfsmisc)
matrix.sums<-data.matrix(chla.secchi.p.n.sums[2:9])
matrix.pctchg<-data.matrix(all.pctchg[2:9])

png("Figures/TempPrecipBoxplot.png",width = 6,height = 11,units = 'in',res=300)
par(mfrow=c(2,1),oma=c(1,4,1,1),mar=c(1,0,0,0))
boxplot(matrix.sums, ylim=c(-.6,.6), names=FALSE, axes=F, lwd.axis=2, cex.axis=1.4, pch=16, outcol=c(rgb(.129,.4,.675,.15), rgb(0.839,.376,.302,.15)), col=c(rgb(.129,.4,.675,.85), rgb(0.839,.376,.302,.85)))
box(lwd=2)
axis(side=2, lwd=2, labels=T, cex.axis=1.4)
abline(h=0, lwd=2, lty=2)
abline(v=2.5, lty=3)
abline(v=4.5, lty=3)
abline(v=6.5, lty=3)
mtext("Standardized Regression Coefficient", side=2, cex=1.4, line=2.5)
legend(7, -.4, legend=c("WP", "ST"), fill=c(rgb(.129,.4,.675,.85), rgb(0.839,.376,.302,.85)), bty="n", cex=1.4)


#use blue and orange colors from other fig - 
#November.ppt.x1.col= rgb(33,102,172, max=255)
#May.tmean.col= rgb(214,96,77, max=255)


#add second panel for the pct changes under global change estimates
boxplot(matrix.pctchg, ylim=c(-60,60), names=FALSE, axes=F, lwd.axis=2, cex.axis=1.4, pch=16, outcol=c(rgb(.129,.4,.675,.15), rgb(0.839,.376,.302,.15)), col=c(rgb(.129,.4,.675,.85), rgb(0.839,.376,.302,.85)))
box(lwd=2)
axis(side=2, lwd=2, labels=T, cex.axis=1.4)
abline(h=0, lwd=2, lty=2)
abline(v=2.5, lty=3)
abline(v=4.5, lty=3)
abline(v=6.5, lty=3)
mtext("Percent Change", side=2, cex=1.4, line=2.5)
mtext(c("Chlorophyll", "Secchi", "TP", "TN"), at=c(1.3, 3.5, 5.5, 7.5), side=1, cex=1.4, line=.5)
dev.off()
