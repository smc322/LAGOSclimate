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


##make a violin plot instead as suggested by a reviewer for WRR revision. use color function from online
library(sm)
vioplot <- function(x,...,range=1.5,h=NULL,ylim=NULL,names=NULL, horizontal=FALSE,
                    col="magenta", border="black", lty=1, lwd=1, rectCol="black", colMed="white", pchMed=19, at, add=FALSE, wex=1,
                    drawRect=TRUE)
{
  # process multiple datas
  datas <- list(x,...)
  n <- length(datas)
  
  if(missing(at)) at <- 1:n
  
  # pass 1
  #
  # - calculate base range
  # - estimate density
  #
  
  # setup parameters for density estimation
  upper  <- vector(mode="numeric",length=n)
  lower  <- vector(mode="numeric",length=n)
  q1     <- vector(mode="numeric",length=n)
  q3     <- vector(mode="numeric",length=n)
  med    <- vector(mode="numeric",length=n)
  base   <- vector(mode="list",length=n)
  height <- vector(mode="list",length=n)
  baserange <- c(Inf,-Inf)
  
  # global args for sm.density function-call
  args <- list(display="none")
  
  if (!(is.null(h)))
    args <- c(args, h=h)
  
  for(i in 1:n) {
    data<-datas[[i]]
    
    # calculate plot parameters
    #   1- and 3-quantile, median, IQR, upper- and lower-adjacent
    data.min <- min(data)
    data.max <- max(data)
    q1[i]<-quantile(data,0.25)
    q3[i]<-quantile(data,0.75)
    med[i]<-median(data)
    iqd <- q3[i]-q1[i]
    upper[i] <- min( q3[i] + range*iqd, data.max )
    lower[i] <- max( q1[i] - range*iqd, data.min )
    
    #   strategy:
    #       xmin = min(lower, data.min))
    #       ymax = max(upper, data.max))
    #
    
    est.xlim <- c( min(lower[i], data.min), max(upper[i], data.max) )
    
    # estimate density curve
    smout <- do.call("sm.density", c( list(data, xlim=est.xlim), args ) )
    
    # calculate stretch factor
    #
    #  the plots density heights is defined in range 0.0 ... 0.5
    #  we scale maximum estimated point to 0.4 per data
    #
    hscale <- 0.4/max(smout$estimate) * wex
    
    # add density curve x,y pair to lists
    base[[i]]   <- smout$eval.points
    height[[i]] <- smout$estimate * hscale
    
    # calculate min,max base ranges
    t <- range(base[[i]])
    baserange[1] <- min(baserange[1],t[1])
    baserange[2] <- max(baserange[2],t[2])
    
  }
  
  # pass 2
  #
  # - plot graphics
  
  # setup parameters for plot
  if(!add){
    xlim <- if(n==1)
      at + c(-.5, .5)
    else
      range(at) + min(diff(at))/2 * c(-1,1)
    
    if (is.null(ylim)) {
      ylim <- baserange
    }
  }
  if (is.null(names)) {
    label <- 1:n
  } else {
    label <- names
  }
  
  boxwidth <- 0.05 * wex
  
  # setup plot
  if(!add)
    plot.new()
  if(!horizontal) {
    if(!add){
      plot.window(xlim = xlim, ylim = ylim)
      axis(2, cex.axis=1.4)
      axis(1,at = at, label=FALSE, col="white" )
    }
    
    box()
    for(i in 1:n) {
      # plot left/right density curve
      polygon( c(at[i]-height[[i]], rev(at[i]+height[[i]])),
               c(base[[i]], rev(base[[i]])),
               col = col[i %% length(col) + 1], border=border, lty=lty, lwd=lwd)
      
      if(drawRect){
        # plot IQR
        lines( at[c( i, i)], c(lower[i], upper[i]) ,lwd=lwd, lty=lty)
        
        # plot 50% KI box
        rect( at[i]-boxwidth/2, q1[i], at[i]+boxwidth/2, q3[i], col=rectCol)
        
        # plot median point
        points( at[i], med[i], pch=pchMed, col=colMed )
      }
    }
    
  }
  else {
    if(!add){
      plot.window(xlim = ylim, ylim = xlim)
      axis(1, cex.axis=1.4)
      axis(2,at = at, label=FALSE, col="white" )
    }
    
    box()
    for(i in 1:n) {
      # plot left/right density curve
      polygon( c(base[[i]], rev(base[[i]])),
               c(at[i]-height[[i]], rev(at[i]+height[[i]])),
               col = col[i %% length(col) + 1], border=border, lty=lty, lwd=lwd)
      
      if(drawRect){
        # plot IQR
        lines( c(lower[i], upper[i]), at[c(i,i)] ,lwd=lwd, lty=lty)
        
        # plot 50% KI box
        rect( q1[i], at[i]-boxwidth/2, q3[i], at[i]+boxwidth/2,  col=rectCol)
        
        # plot median point
        points( med[i], at[i], pch=pchMed, col=colMed )
      }
    }
  }
  invisible (list( upper=upper, lower=lower, median=med, q1=q1, q3=q3))
}

png("Figures/TempPrecipVioplot.png",width = 6,height = 9,units = 'in',res=300)
par(mfrow=c(2,1),oma=c(1,4,1,1),mar=c(1,0,0,0))
vioplot(chla.secchi.p.n.sums$winteprecipsum.chla, chla.secchi.p.n.sums$summertempsum.chla, chla.secchi.p.n.sums$winteprecipsum.sec, chla.secchi.p.n.sums$summertempsum.sec, chla.secchi.p.n.sums$winterprecipsum.p, chla.secchi.p.n.sums$summertempsum.p, chla.secchi.p.n.sums$winterprecipsum.n, chla.secchi.p.n.sums$summertempsum.n, col=c( rgb(0.839,.376,.302,.85), rgb(.129,.4,.675,.85)))
box(lwd=2)
#axis(side=2, lwd=2, labels=T, cex.axis=1.4)
abline(h=0, lwd=2, lty=2)
abline(v=2.5, lty=3)
abline(v=4.5, lty=3)
abline(v=6.5, lty=3)
mtext("Standardized Regression Coefficient", side=2, cex=1.4, line=2.5)
legend(7, 2.3, legend=c("WP", "ST"), fill=c(rgb(.129,.4,.675,.85), rgb(0.839,.376,.302,.85)), bty="n", cex=1.4)



#use blue and orange colors from other fig - 
#November.ppt.x1.col= rgb(33,102,172, max=255)
#May.tmean.col= rgb(214,96,77, max=255)


#add second panel for the pct changes under global change estimates
vioplot(all.pctchg$pctCchgppt, all.pctchg$pctCchgtemp, all.pctchg$pctSchgppt, all.pctchg$pctSchgtemp, all.pctchg$pctPchgppt, all.pctchg$pctPchgtemp, all.pctchg$pctNchgppt, all.pctchg$pctNchgtemp, col=c(rgb(0.839,.376,.302,.85), rgb(.129,.4,.675,.85)))
box(lwd=2)
#axis(side=2, lwd=2, labels=T, cex.axis=1.4)
abline(h=0, lwd=2, lty=2)
abline(v=2.5, lty=3)
abline(v=4.5, lty=3)
abline(v=6.5, lty=3)
mtext("Percent Change", side=2, cex=1.4, line=2.5)
mtext(c("Chlorophyll", "Secchi", "TP", "TN"), at=c(1.3, 3.5, 5.5, 7.5), side=1, cex=1.4, line=.5)
dev.off()

###single panel version of pct change for talks
png("Figures/TempPrecipBoxplot.png",width = 6,height = 5,units = 'in',res=300)
+par(mar=c(2,4,1,1))
+boxplot(matrix.sums, ylim=c(-.6,.6), names=FALSE, axes=F, lwd.axis=2, cex.axis=1.4, pch=16, outcol=c(rgb(.129,.4,.675,.15), rgb(0.839,.376,.302,.15)), col=c(rgb(.129,.4,.675,.85), rgb(0.839,.376,.302,.85)))
+box(lwd=2)
+axis(side=2, lwd=2, labels=T, cex.axis=1.4)
+abline(h=0, lwd=2, lty=2)
+abline(v=2.5, lty=3)
+abline(v=4.5, lty=3)
+abline(v=6.5, lty=3)
+mtext("Standardized Regression Coefficient", side=2, cex=1.4, line=2.5)
+mtext(c("Chlorophyll", "Secchi", "TP", "TN"), at=c(1.3, 3.5, 5.5, 7.5), side=1, cex=1.4, line=.5)
+legend(7.25, -.4, legend=c("WP", "ST"), fill=c(rgb(.129,.4,.675,.85), rgb(0.839,.376,.302,.85)), bty="n", cex=1.2)
+dev.off()


#map of pct change over space
library(LAGOSNE)
library(maps)
lagos <-lagosne_load(version = "1.087.1")
locus<-lagos$locus
cordsids<-unique(locus[,c("lagoslakeid", "nhd_lat", "nhd_long")])

color.gradient <- function(x, colors=c(rgb(0, 102,204, 150, max=255),rgb(255,255 ,255 ,150, max=255),rgb( 153,0 ,0 ,150, max=255)), colsteps=10) {
  return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps)) ] )
}

pctchgcords<-merge(all.pctchg, cordsids, by="lagoslakeid", all.x=T, all.y=F)

pdf("Figures/Pctchgmaps.pdf", width=12, height=14)
par(mfrow=c(2,1), oma=c(0,0,0,0), mar=c(0,0,0,0))


map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois", "Indiana", "Ohio", "Michigan","Pennsylvania","New York", "Missouri",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = TRUE, col="white", fg="grey30", lwd=1)
points(pctchgcords$nhd_long, pctchgcords$nhd_lat, pch=21, col="black", bg=color.gradient(pctchgcords$pctCchgtemp), cex=1.1)

map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois", "Indiana", "Ohio", "Michigan","Pennsylvania","New York", "Missouri",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = TRUE, col="white", fg="grey30", lwd=1)
points(pctchgcords$nhd_long, pctchgcords$nhd_lat, pch=21, col="black", bg=color.gradient(pctchgcords$pctCchgppt), cex=1.1)

dev.off()
