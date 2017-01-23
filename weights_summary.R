setwd("~/Dropbox/Sarah_Work/Manuscripts/2016_climate_waterqual/Data/RegressionWeights_Dec2016")

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
