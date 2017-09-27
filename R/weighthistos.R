
setwd("~/Dropbox/Sarah_Work/Manuscripts/2016_climate_waterqual/Data/RegressionWeights_Dec2016")

n.weights<-read.csv("nweights.csv", header=T)
p.weights<-read.csv("pweights.csv", header=T)
secchi.weights<-read.csv("secchiweights.csv", header=T)
chla.weights<-read.csv("chlaweights.csv", header=T)


for (col in 4:ncol(n.weights)) {
  hist(log(.7+n.weights[,col]), breaks=20, main=colnames(n.weights)[col])
}

