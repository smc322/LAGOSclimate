##SMC 23Aug2017: want to make heatmaps like shuai generated for the weights, but with other values of lambda 3 besides .03 (higher ones might work better for clustering according to PN).  Try first with .08 data then want to also add .5 data.

setwd("~/Dropbox/CSI&CL/CSI_LIMNO_Manuscripts-presentations/CSI_ClimateWaterQual/Results/Clustering/WeightsToCluster_April2017/Lambda3_05_AllLakes")

chla.008.w<-read.csv("chla_05_weights.csv", header=T)
rownames(chla.008.w)<-chla.008.w$lagoslakeid
chla.008.w.m<-data.matrix(chla.008.w)
chla.008.w.m<-chla.008.w.m[,-c(1,2,3)]
chla.008.w.m<-t(chla.008.w.m)

hmcols <- brewer.pal(11,"RdBu")
heatmap(chla.008.w.m, col=hmcols, Rowv=NA, Colv=NA, scale="none", breaks=c(-1, -.8, -.6, -.4, -.2, -.01, .01, .2, .4, .6, .8, 1), main="Chla Lambda3=.08")

chla.008.w.c<-chla.008.w[,-c(1,2,3)]
chla.008.w.c.d<-dist(chla.008.w.c)
test<-hclust(chla.008.w.c.d)



sec.05.w<-read.csv("secc_05_weights.csv", header=T)
rownames(sec.05.w)<-sec.05.w$lagoslakeid
sec.05.w.m<-data.matrix(sec.05.w)
sec.05.w.m<-sec.05.w.m[,-c(1,2,3)]
sec.05.w.m<-t(sec.05.w.m)

hmcols <- brewer.pal(11,"RdBu")
heatmap(sec.05.w.m, col=hmcols, Rowv=NA, Colv=NA, scale="none", breaks=c(-1, -.8, -.6, -.4, -.2, -.01, .01, .2, .4, .6, .8, 1), main="Sec Lambda3=.5")

sec.05.w.c<-sec.05.w[,-c(1,2,3)]
sec.05.w.c.d<-dist(sec.05.w.c)
test<-hclust(sec.05.w.c.d)



chla.05.w<-read.csv("chla_05_weights.csv", header=T)
rownames(chla.05.w)<-chla.05.w$lagoslakeid
chla.05.w.m<-data.matrix(chla.05.w)
chla.05.w.m<-chla.05.w.m[,-c(1,2,3)]
chla.05.w.m<-t(chla.05.w.m)

hmcols <- brewer.pal(11,"RdBu")
heatmap(chla.05.w.m, col=hmcols, Rowv=NA, Colv=NA, scale="none", breaks=c(-1, -.8, -.6, -.4, -.2, -.01, .01, .2, .4, .6, .8, 1), main="Chla Lambda3=.5")

chla.05.w.c<-chla.05.w[,-c(1,2,3)]
chla.05.w.c.d<-dist(chla.05.w.c)
test<-hclust(chla.05.w.c.d)


tp.05.w<-read.csv("TP_05_weights.csv", header=T)
rownames(tp.05.w)<-tp.05.w$lagoslakeid
tp.05.w.m<-data.matrix(tp.05.w)
tp.05.w.m<-tp.05.w.m[,-c(1,2,3)]
tp.05.w.m<-t(tp.05.w.m)

hmcols <- brewer.pal(11,"RdBu")
heatmap(tp.05.w.m, col=hmcols, Rowv=NA, Colv=NA, scale="none", breaks=c(-1, -.8, -.6, -.4, -.2, -.01, .01, .2, .4, .6, .8, 1), main="TP Lambda3=.5", barScale=1)

tp.05.w.c<-tp.05.w[,-c(1,2,3)]
tp.05.w.c.d<-dist(tp.05.w.c)
test<-hclust(tp.05.w.c.d)


tn.05.w<-read.csv("TN_05_weights.csv", header=T)
rownames(tn.05.w)<-tn.05.w$lagoslakeid
tn.05.w.m<-data.matrix(tn.05.w)
tn.05.w.m<-tn.05.w.m[,-c(1,2,3)]
tn.05.w.m<-t(tn.05.w.m)

hmcols <- brewer.pal(11,"RdBu")
heatmap(tn.05.w.m, col=hmcols, Rowv=NA, Colv=NA, scale="none", breaks=c(-1, -.8, -.6, -.4, -.2, -.01, .01, .2, .4, .6, .8, 1), main="TN Lambda3=.5", barScale=1)

tn.05.w.c<-tn.05.w[,-c(1,2,3)]
tn.05.w.c.d<-dist(tn.05.w.c)
test<-hclust(tn.05.w.c.d)