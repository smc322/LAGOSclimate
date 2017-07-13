#originally did this with Dec 2016 data, updated here in July 2017 for April 2017 data from Shuai
#setwd("~/Dropbox/Sarah_Work/Manuscripts/2017_LAGOSClimateWaterqual/Data/RegressionWeights_Dec2016")
setwd("~/Dropbox/Sarah_Work/Manuscripts/2017_LAGOSClimateWaterqual/Data/RegressionWeights_April2017")


n.weights<-read.csv("nweights.csv", header=T)
p.weights<-read.csv("pweights.csv", header=T)
secchi.weights<-read.csv("secchiweights.csv", header=T)
chla.weights<-read.csv("chlaweights.csv", header=T)

n.weights.1<-abs(n.weights[,c(4:51)])
n.weights.2<-abs(n.weights[,c(4:51)])
n.weights.3<-abs(n.weights[,c(4:51)])

Largest.n<-data.frame(colnames(n.weights.1)[max.col(n.weights.1, ties.method="random")])
SecondLargest.n <- data.frame(colnames(n.weights.2)[apply(n.weights.2,1,function(x)which(x==sort(x,partial=47)[47])[1])])
ThirdLargest.n <- data.frame(colnames(n.weights.3)[apply(n.weights.3,1,function(x)which(x==sort(x,partial=46)[46])[1])])

names(Largest.n)<-"Largest"
names(SecondLargest.n)<-"SecondLargest"
names(ThirdLargest.n)<-"ThirdLargest"

coef.ranks.n<-data.frame(Largest.n=Largest.n$Largest,Second.n = SecondLargest.n$SecondLargest,Third.n=ThirdLargest.n$ThirdLargest)
summary(coef.ranks.n)


p.weights.1<-abs(p.weights[,c(4:51)])
p.weights.2<-abs(p.weights[,c(4:51)])
p.weights.3<-abs(p.weights[,c(4:51)])

Largest.p<-data.frame(colnames(p.weights.1)[max.col(p.weights.1, ties.method="random")])
SecondLargest.p <- data.frame(colnames(p.weights.2)[apply(p.weights.2,1,function(x)which(x==sort(x,partial=47)[47])[1])])
ThirdLargest.p <- data.frame(colnames(p.weights.3)[apply(p.weights.3,1,function(x)which(x==sort(x,partial=46)[46])[1])])

names(Largest.p)<-"Largest"
names(SecondLargest.p)<-"SecondLargest"
names(ThirdLargest.p)<-"ThirdLargest"

coef.ranks.p<-data.frame(Largest.p=Largest.p$Largest,Second.p = SecondLargest.p$SecondLargest,Third.p=ThirdLargest.p$ThirdLargest)
summary(coef.ranks.p)


secchi.weights.1<-abs(secchi.weights[,c(4:51)])
secchi.weights.2<-abs(secchi.weights[,c(4:51)])
secchi.weights.3<-abs(secchi.weights[,c(4:51)])

Largest.secchi<-data.frame(colnames(secchi.weights.1)[max.col(secchi.weights.1, ties.method="random")])
SecondLargest.secchi <- data.frame(colnames(secchi.weights.2)[apply(secchi.weights.2,1,function(x)which(x==sort(x,partial=47)[47])[1])])
ThirdLargest.secchi <- data.frame(colnames(secchi.weights.3)[apply(secchi.weights.3,1,function(x)which(x==sort(x,partial=46)[46])[1])])

names(Largest.secchi)<-"Largest"
names(SecondLargest.secchi)<-"SecondLargest"
names(ThirdLargest.secchi)<-"ThirdLargest"

coef.ranks.secchi<-data.frame(Largest.secchi=Largest.secchi$Largest,Second.secchi = SecondLargest.secchi$SecondLargest,Third.secchi=ThirdLargest.secchi$ThirdLargest)
summary(coef.ranks.secchi)


chla.weights.1<-abs(chla.weights[,c(4:51)])
chla.weights.2<-abs(chla.weights[,c(4:51)])
chla.weights.3<-abs(chla.weights[,c(4:51)])

Largest.chla<-data.frame(colnames(chla.weights.1)[max.col(chla.weights.1, ties.method="random")])
SecondLargest.chla <- data.frame(colnames(chla.weights.2)[apply(chla.weights.2,1,function(x)which(x==sort(x,partial=47)[47])[1])])
ThirdLargest.chla <- data.frame(colnames(chla.weights.3)[apply(chla.weights.3,1,function(x)which(x==sort(x,partial=46)[46])[1])])

names(Largest.chla)<-"Largest"
names(SecondLargest.chla)<-"SecondLargest"
names(ThirdLargest.chla)<-"ThirdLargest"

coef.ranks.chla<-data.frame(Largest.chla=Largest.chla$Largest,Second.chla = SecondLargest.chla$SecondLargest,Third.chla=ThirdLargest.chla$ThirdLargest)
summary(coef.ranks.chla)

##create summary of counts for everything
library(plyr)


largest.counts.chla<-count(coef.ranks.chla, 'Largest.chla')
names(largest.counts.chla)<-c("variable", "freq.largest.chla")
second.largest.counts.chla<-count(coef.ranks.chla, 'Second.chla')
names(second.largest.counts.chla)<-c("variable", "freq.second.chla")
third.largest.counts.chla<-count(coef.ranks.chla, 'Third.chla')
names(third.largest.counts.chla)<-c("variable", "freq.third.chla")

chla.12<-merge(largest.counts.chla, second.largest.counts.chla, by="variable", all.x=T, all.y=T)
chla.123<-merge(chla.12, third.largest.counts.chla, by="variable", all.x=T, all.y=T)
chla.123$sum.chla.123=rowSums(chla.123[,c(2:4)], na.rm=TRUE)

largest.counts.secchi<-count(coef.ranks.secchi, 'Largest.secchi')
names(largest.counts.secchi)<-c("variable", "freq.largest.secchi")
second.largest.counts.secchi<-count(coef.ranks.secchi, 'Second.secchi')
names(second.largest.counts.secchi)<-c("variable", "freq.second.secchi")
third.largest.counts.secchi<-count(coef.ranks.secchi, 'Third.secchi')
names(third.largest.counts.secchi)<-c("variable", "freq.third.secchi")

chla.secchi<-merge(chla.123, largest.counts.secchi, by="variable", all.x=T, all.y=T)
chla.secchi.12<-merge(chla.secchi, second.largest.counts.secchi, by="variable", all.x=T, all.y=T)
chla.secchi.123<-merge(chla.secchi.12, third.largest.counts.secchi, by="variable", all.x=T, all.y=T)
chla.secchi.123$sum.secchi.123=rowSums(chla.secchi.123[,c(6:8)], na.rm=TRUE)


largest.counts.p<-count(coef.ranks.p, 'Largest.p')
names(largest.counts.p)<-c("variable", "freq.largest.p")
second.largest.counts.p<-count(coef.ranks.p, 'Second.p')
names(second.largest.counts.p)<-c("variable", "freq.second.p")
third.largest.counts.p<-count(coef.ranks.p, 'Third.p')
names(third.largest.counts.p)<-c("variable", "freq.third.p")

chla.secchi.p<-merge(chla.secchi.123, largest.counts.p, by="variable", all.x=T, all.y=T)
chla.secchi.p.12<-merge(chla.secchi.p, second.largest.counts.p, by="variable", all.x=T, all.y=T)
chla.secchi.p.123<-merge(chla.secchi.p.12, third.largest.counts.p, by="variable", all.x=T, all.y=T)
chla.secchi.p.123$sum.p.123=rowSums(chla.secchi.p.123[,c(10:12)], na.rm=TRUE)

largest.counts.n<-count(coef.ranks.n, 'Largest.n')
names(largest.counts.n)<-c("variable", "freq.largest.n")
second.largest.counts.n<-count(coef.ranks.n, 'Second.n')
names(second.largest.counts.n)<-c("variable", "freq.second.n")
third.largest.counts.n<-count(coef.ranks.n, 'Third.n')
names(third.largest.counts.n)<-c("variable", "freq.third.n")

chla.secchi.p.n<-merge(chla.secchi.p.123, largest.counts.n, by="variable", all.x=T, all.y=T)
chla.secchi.p.n.12<-merge(chla.secchi.p.n, second.largest.counts.n, by="variable", all.x=T, all.y=T)
chla.secchi.p.n.123<-merge(chla.secchi.p.n.12, third.largest.counts.n, by="variable", all.x=T, all.y=T)
chla.secchi.p.n.123$sum.n.123=rowSums(chla.secchi.p.n.123[,c(14:16)], na.rm=TRUE)

all.coef.summary<-chla.secchi.p.n.123
all.coef.summary.sums<-all.coef.summary[,c(1, 5, 9, 13, 17)]

setwd("~/Dropbox/Sarah_Work/Manuscripts/2017_LAGOSClimateWaterqual/ResultsSummaryStats")

write.csv(all.coef.summary, "Coefficient_Counts_Sums_July2017.csv")
write.csv(all.coef.summary.sums, "Coefficient_Counts_Sums_Top3Combined_July2017.csv")

##pick out anything with sums over 1,000 for the top 3 combined, make stacked bar
###leave this here for the Dec 2016 data, start new version for July 2017 update below!

## names all with colors for each
#December.ppt.x1.col= rgb(103,0,31, max=255)
#January.ppt.col= rgb(178,24,43, max=255)
#November.ppt.x1.col= rgb(214,96,77, max=255)
#ppt.winter.col= rgb(244,165,130, max=255)
  
#precip.annual.x1.col= rgb(253,219,199, max=255)

#May.ppt.col= rgb(247,247,247, max=255)
#palmer.spring.col= rgb(209,229,240, max=255)

#June.tmean.col= rgb(146,197,222, max=255)
#May.tmean.col= rgb(67,147,195, max=255)

#tmean.annual.x1.col= rgb(33,102,172, max=255)
#tmin.annual.x1.col= rgb(5,48,97, max=255)

#Other.col= "grey80"


#vars<-c("December.ppt.x1", "January.ppt", "November.ppt.x1", "ppt.winter", "precip.annual.x1", "May.ppt", "palmer.spring", "June.tmean", "May.tmean", "tmean.annual.x1", "tmin.annual.x1", "Other")

#var.coef.vals.chla<-c(0, 7382, 1425,7368, 0, 0, 0, 6766, 0, 0, 0, (35604-7382-6766-1425-7368))

#var.coef.vals.secchi<-c(0, 7650, 0, 8567, 8671, 0, 1267, 0, 0, 1651, 2078, (35604-7650-1267-8567-8671-1651-2078))

#var.coef.vals.p<-c(3742, 7280, 0, 8361, 1391, 0, 0, 4199, 0, 0, 0, (35604-3742-7280-8361-1391-4199))

#var.coef.vals.n<-c(7649, 5703, 0, 8576, 0, 2041, 0, 3088, 1005, 0, 0, (35604-7649-5703-8576-2041-3088-1005))


#coefs.matrix=cbind(var.coef.vals.chla, var.coef.vals.secchi, var.coef.vals.p, var.coef.vals.n)
#colors.vars<-c(December.ppt.x1.col, January.ppt.col, November.ppt.x1.col, ppt.winter.col, precip.annual.x1.col, May.ppt.col, palmer.spring.col, June.tmean.col, May.tmean.col, tmean.annual.x1.col, tmin.annual.x1.col, Other.col)
#par(xpd=NA)
#barplot(coefs.matrix, col=colors.vars, names=c("Chlorophyll", "Secchi", "TP", "TN"), ylim=c(0, 36000), xlim=c(0, 7), cex.names=1.5, cex.axis=1.3)
#legend(5.2, 32000, legend=c("December", "January", "November", "Winter", "Annual"), fill=c(December.ppt.x1.col, January.ppt.col, November.ppt.x1.col, ppt.winter.col, precip.annual.x1.col), bg=rgb(1,1,1,.2), cex=1.3, bty="n", title="Prev. Winter/Year Precip")
#legend(5.2, 24000, legend=c("May PPT", "Spring Palmer"), fill=c(May.ppt.col, palmer.spring.col), bg=rgb(1,1,1,.2), cex=1.3, bty="n", title="Spring Precip/Palmer")
#legend(5.2, 19000, legend=c("May", "June"), fill=c(May.tmean.col, June.tmean.col), bg=rgb(1,1,1,.2), cex=1.3, bty="n", title="Early Summer Temp")
#legend(5.2, 14000, legend=c("Temp Mean", "Temp Min"), fill=c(tmean.annual.x1.col, tmin.annual.x1.col), bg=rgb(1,1,1,.2), cex=1.3, bty="n", title="Prev. Year Temperature")
#legend(5.2, 7000, legend=c("Other"), fill=c(Other.col), bg=rgb(1,1,1,.2), cex=1.3, bty="n")

#July 2017 version!
## names all with colors for each
##variables with at least 1000?  That's fewer than before (as we'd hope!)
##chla: June.tmean, May.tmean, November.ppt.x1, ppt.winter, precip.annual.x1, tmean.summer
##secchi: January.ppt, June.tmean, May.tmean, November.ppt.x1, ppt.winter, precip.annual.x1
##tp: June.tmean, May.tmean, November.ppt.x1, ppt.winter, precip.annual.x1, tmean.summer
##tn: January.ppt, June.tmean, May.tmean, November.ppt.x1, ppt.winter, precip.annual.x1, tmean.summer

#December.ppt.x1.col= rgb(103,0,31, max=255)
November.ppt.x1.col= rgb(178,24,43, max=255)
January.ppt.col= rgb(214,96,77, max=255)
ppt.winter.col= rgb(244,165,130, max=255)
precip.annual.x1.col= rgb(253,219,199, max=255)

#May.ppt.col= rgb(247,247,247, max=255)
#palmer.spring.col= rgb(209,229,240, max=255)

June.tmean.col= rgb(146,197,222, max=255)
May.tmean.col= rgb(67,147,195, max=255)
summer.tmean.col= rgb(33,102,172, max=255)

#tmean.annual.x1.col= rgb(33,102,172, max=255)
#tmin.annual.x1.col= rgb(5,48,97, max=255)

Other.col= "grey80"


vars<-c("November.ppt.x1", "January.ppt", "ppt.winter", "precip.annual.x1", "May.tmean", "June.tmean", "tmean.summer", "Other")

var.coef.vals.chla<-c(3455, 0, 4371, 3955, 5307, 5690, 3781, (35646-3455-4371-3955-5307-5690-3781))

var.coef.vals.secchi<-c(3529, 4031, 6858, 3508, 1971, 4127, 0, (35646-3529-4031-6858-3508-1971-4127))

var.coef.vals.p<-c(3226, 0, 6653, 3861, 5880, 6262, 2602, (35646-3226-6653-3861-5880-6262-2602))

var.coef.vals.n<-c(3336, 1587, 8332, 3974, 6872, 7198, 1243, (35646-3336-1587-8332-3974-6872-7198-1243))


coefs.matrix=cbind(var.coef.vals.chla, var.coef.vals.secchi, var.coef.vals.p, var.coef.vals.n)
colors.vars<-c(November.ppt.x1.col, January.ppt.col, ppt.winter.col, precip.annual.x1.col, May.tmean.col, June.tmean.col, summer.tmean.col, Other.col)
par(xpd=NA)
barplot(coefs.matrix, col=colors.vars, names=c("Chla", "Secchi", "TP", "TN"), ylim=c(0, 36000), xlim=c(0, 7), cex.names=1.5, cex.axis=1.3)
legend(5.2, 32000, legend=c("November PPT", "January PPT", "Winter PPT", "PrevYear PPT"), fill=c(November.ppt.x1.col, January.ppt.col, ppt.winter.col, precip.annual.x1.col), bg=rgb(1,1,1,.2), cex=1.3, bty="n")
legend(5.2, 24000, legend=c("May Tmean", "June Tmean", "Summer Tmean"), fill=c(May.tmean.col, June.tmean.col, summer.tmean.col), bg=rgb(1,1,1,.2), cex=1.3, bty="n")
#legend(5.2, 19000, legend=c("May", "June"), fill=c(May.tmean.col, June.tmean.col), bg=rgb(1,1,1,.2), cex=1.3, bty="n", title="Early Summer Temp")
#legend(5.2, 14000, legend=c("Temp Mean", "Temp Min"), fill=c(tmean.annual.x1.col, tmin.annual.x1.col), bg=rgb(1,1,1,.2), cex=1.3, bty="n", title="Prev. Year Temperature")
legend(5.2, 18000, legend=c("Other"), fill=c(Other.col), bg=rgb(1,1,1,.2), cex=1.3, bty="n")




##create maps - color top preds that made it into those bar graphs and map over space
##zillion maps below are not updated for July 2017 analysis, still Dec 2016 data.  I don't think we 
#particularly need them... hopefully we'll be able to map clusters if that works.  but might want to take
#a look to visualize some stuff if the clustering doesn't work or if we happen to want to focus on a particular
#predictor variable?  so leaving this here for now just in case

secchi.with.ranks<-cbind(secchi.weights, coef.ranks.secchi)
chla.with.ranks<-cbind(chla.weights, coef.ranks.chla)
p.with.ranks<-cbind(p.weights, coef.ranks.p)
n.with.ranks<-cbind(n.weights, coef.ranks.n)

secchi.with.ranks$ppt.winter.lakes<-apply(secchi.with.ranks, 1, function(r) any(r %in% c("ppt.winter")))
winter.ppt.subset<-subset(secchi.with.ranks, ppt.winter.lakes=="TRUE")
secchi.with.ranks$ppt.jan.lakes<-apply(secchi.with.ranks, 1, function(r) any(r %in% c("January.ppt")))
jan.ppt.subset<-subset(secchi.with.ranks, ppt.jan.lakes=="TRUE")
secchi.with.ranks$ppt.annualx1.lakes<-apply(secchi.with.ranks, 1, function(r) any(r %in% c("precip.annual.x1")))
annx1.ppt.subset<-subset(secchi.with.ranks, ppt.annualx1.lakes=="TRUE")
secchi.with.ranks$palmer.spring.lakes<-apply(secchi.with.ranks, 1, function(r) any(r %in% c("palmer.spring")))
palmer.sp.subset<-subset(secchi.with.ranks, palmer.spring.lakes=="TRUE")
secchi.with.ranks$tmean.annualx1.lakes<-apply(secchi.with.ranks, 1, function(r) any(r %in% c("tmean.annual.x1")))
annx1.tmean.subset<-subset(secchi.with.ranks, tmean.annualx1.lakes=="TRUE")
secchi.with.ranks$tmin.annualx1.lakes<-apply(secchi.with.ranks, 1, function(r) any(r %in% c("tmin.annual.x1")))
annx1.tmin.subset<-subset(secchi.with.ranks, tmin.annualx1.lakes=="TRUE")

chla.with.ranks$ppt.winter.lakes<-apply(chla.with.ranks, 1, function(r) any(r %in% c("ppt.winter")))
winter.ppt.subset.chla<-subset(chla.with.ranks, ppt.winter.lakes=="TRUE")
chla.with.ranks$ppt.jan.lakes<-apply(chla.with.ranks, 1, function(r) any(r %in% c("January.ppt")))
jan.ppt.subset.chla<-subset(chla.with.ranks, ppt.jan.lakes=="TRUE")
chla.with.ranks$ppt.novx1.lakes<-apply(chla.with.ranks, 1, function(r) any(r %in% c("November.ppt.x1")))
novx1.ppt.subset.chla<-subset(chla.with.ranks, ppt.novx1.lakes=="TRUE")
chla.with.ranks$temp.june.lakes<-apply(chla.with.ranks, 1, function(r) any(r %in% c("June.tmean")))
june.temp.subset.chla<-subset(chla.with.ranks, temp.june.lakes=="TRUE")

#check p and n dims on desktop - same as other above?  doesn't work on laptop but didn't run everything...
p.with.ranks$ppt.winter.lakes<-apply(p.with.ranks, 1, function(r) any(r %in% c("ppt.winter")))
winter.ppt.subset.p<-subset(p.with.ranks, ppt.winter.lakes=="TRUE")
p.with.ranks$ppt.decx1.lakes<-apply(p.with.ranks, 1, function(r) any(r %in% c("December.ppt.x1")))
decx1.ppt.subset.p<-subset(p.with.ranks, ppt.decx1.lakes=="TRUE")
p.with.ranks$ppt.jan.lakes<-apply(p.with.ranks, 1, function(r) any(r %in% c("January.ppt")))
jan.ppt.subset.p<-subset(p.with.ranks, ppt.jan.lakes=="TRUE")
p.with.ranks$ppt.annualx1.lakes<-apply(p.with.ranks, 1, function(r) any(r %in% c("precip.annual.x1")))
annx1.ppt.subset.p<-subset(p.with.ranks, ppt.annualx1.lakes=="TRUE")
p.with.ranks$temp.june.lakes<-apply(p.with.ranks, 1, function(r) any(r %in% c("June.tmean")))
june.temp.subset.p<-subset(p.with.ranks, temp.june.lakes=="TRUE")

n.with.ranks$ppt.winter.lakes<-apply(n.with.ranks, 1, function(r) any(r %in% c("ppt.winter")))
winter.ppt.subset.n<-subset(n.with.ranks, ppt.winter.lakes=="TRUE")
n.with.ranks$ppt.decx1.lakes<-apply(n.with.ranks, 1, function(r) any(r %in% c("December.ppt.x1")))
decx1.ppt.subset.n<-subset(n.with.ranks, ppt.decx1.lakes=="TRUE")
n.with.ranks$ppt.jan.lakes<-apply(n.with.ranks, 1, function(r) any(r %in% c("January.ppt")))
jan.ppt.subset.n<-subset(n.with.ranks, ppt.jan.lakes=="TRUE")
n.with.ranks$temp.june.lakes<-apply(n.with.ranks, 1, function(r) any(r %in% c("June.tmean")))
june.temp.subset.n<-subset(n.with.ranks, temp.june.lakes=="TRUE")
n.with.ranks$temp.may.lakes<-apply(n.with.ranks, 1, function(r) any(r %in% c("May.tmean")))
may.temp.subset.n<-subset(n.with.ranks, temp.may.lakes=="TRUE")
n.with.ranks$ppt.may.lakes<-apply(n.with.ranks, 1, function(r) any(r %in% c("May.ppt")))
may.ppt.subset.n<-subset(n.with.ranks, ppt.may.lakes=="TRUE")


library(maps)
library(mapdata)
map("worldHires", "Canada", xlim=c(min(secchi.with.ranks$Lon, na.rm=TRUE)-3,max(secchi.with.ranks$Lon, na.rm=TRUE)+3), ylim=c(min(secchi.with.ranks$Lat, na.rm=TRUE)-3,max(secchi.with.ranks$Lat, na.rm=TRUE)+3), fill=TRUE, col="khaki", lwd=2, bg="lightblue1")
map("worldHires", "USa", add=TRUE,  fill=TRUE, col="khaki", lwd=2)
map("state", boundary = FALSE, add = TRUE)

points(secchi.with.ranks$Lon, secchi.with.ranks$Lat, pch=19, col="black", cex=0.5)  
points(winter.ppt.subset$Lon, winter.ppt.subset$Lat, pch=19, col="blue", cex=0.5)  
points(jan.ppt.subset$Lon, jan.ppt.subset$Lat, pch=19, col="blue", cex=0.5)  
points(annx1.ppt.subset$Lon, annx1.ppt.subset$Lat, pch=19, col="blue", cex=0.5)  
points(palmer.sp.subset$Lon, palmer.sp.subset$Lat, pch=19, col="blue", cex=0.5)  
points(annx1.tmean.subset$Lon, annx1.tmean.subset$Lat, pch=19, col="blue", cex=0.5)  
points(annx1.tmin.subset$Lon, annx1.tmin.subset$Lat, pch=19, col="blue", cex=0.5)  

points(winter.ppt.subset.chla$Lon, winter.ppt.subset.chla$Lat, pch=19, col="blue", cex=0.5)  
points(jan.ppt.subset.chla$Lon, jan.ppt.subset.chla$Lat, pch=19, col="blue", cex=0.5)  
points(novx1.ppt.subset.chla$Lon, novx1.ppt.subset.chla$Lat, pch=19, col="blue", cex=0.5)  
points(june.temp.subset.chla$Lon, june.temp.subset.chla$Lat, pch=19, col="blue", cex=0.5)  

points(winter.ppt.subset.p$Lon, winter.ppt.subset.p$Lat, pch=19, col="blue", cex=0.5)  
points(jan.ppt.subset.p$Lon, jan.ppt.subset.p$Lat, pch=19, col="blue", cex=0.5)  
points(decx1.ppt.subset.p$Lon, decx1.ppt.subset.p$Lat, pch=19, col="blue", cex=0.5)  
points(june.temp.subset.p$Lon, june.temp.subset.p$Lat, pch=19, col="blue", cex=0.5)  
points(annx1.ppt.subset.p$Lon, annx1.ppt.subset.p$Lat, pch=19, col="blue", cex=0.5)  

points(winter.ppt.subset.n$Lon, winter.ppt.subset.n$Lat, pch=19, col="blue", cex=0.5)  
points(jan.ppt.subset.n$Lon, jan.ppt.subset.n$Lat, pch=19, col="blue", cex=0.5)  
points(decx1.ppt.subset.n$Lon, decx1.ppt.subset.n$Lat, pch=19, col="blue", cex=0.5)  
points(june.temp.subset.n$Lon, june.temp.subset.n$Lat, pch=19, col="blue", cex=0.5)
points(may.temp.subset.n$Lon, may.temp.subset.n$Lat, pch=19, col="blue", cex=0.5)
points(may.ppt.subset.n$Lon, may.ppt.subset.n$Lat, pch=19, col="blue", cex=0.5)



#add an r2 figure so everyone can see diff btwn our method and global model. goes chl-secchi-tp-tn, each global then ours
r2.vals<-c(.133, .397, .280, .739, .114, .401, .156, .755)
par(xpd=NA, mar=c(6,6,2,1))
barplot(r2.vals, names=c("GL", "OM", "GL", "OM", "GL", "OM","GL", "OM"), ylim=c(0, .8), col=c("grey40", "grey40", "darkorange3", "darkorange3", "palegreen4", "palegreen4", "steelblue4", "steelblue4"), cex.names=1.4, space=c(.2, .2, 1, .2, 1, .2, 1, .2), axes=F)
axis(2, col.axis="black", lwd.ticks=2, lwd=2, cex.axis=1.4)
segments(-.3, 0, 12.4, 0, lwd=2)
text(-1.5, .4, expression(paste("R"^"2")), cex=2, srt=90)
text(1.4, -.1, "Chlorophyll", cex=1.5, font=2)
text(4.5, -.1, "Secchi", cex=1.5, font=2)
text(7.75, -.1, "TP", cex=1.5, font=2)
text(11, -.1, "TN", cex=1.5, font=2)
