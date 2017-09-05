#script to create summer medians for desired LAGOS limno variables for each year of data for Shuai to use in climate multi task learning analysis.  originally doing this with LAGOS 1.054 for getting the analysis set up but will shift to 1.087 when it is available

#read LAGOS limno data, lake identifier data, and separate secchi dataset

#update summer seasonal data to 1.087 for new dataset for shuai (10 feb 2017)  for some reason the "lakes" file equivalent doesn't exist in the 1.087.1 folder so used the 1.087.0 version that should be the same data


setwd("/Users/SarahiMac/Dropbox/CSI&CL/CSI_LAGOS-exports/LAGOS-NE-EDI/LAGOS-NE-LIMNO-EXPORT")
data = read.table("LAGOSNE_epinutr10871.csv", 
                  header = TRUE, 
                  sep = ",", 
                  quote = "", 
                  dec = ".", 
                  strip.white = TRUE, 
                  comment.char = "" )

secchi = read.table("LAGOSNE_secchi10871.csv", 
                           header = TRUE, 
                           sep = ",", 
                           quote = "", 
                           dec = ".", 
                           strip.white = TRUE, 
                           comment.char = "" )

setwd("/Users/SarahiMac/Dropbox/CSI&CL/CSI_LAGOS-exports/LAGOS-LIMNO/Version1.087.0")
data.lake.specific = read.table("lagos_lakes_10870.txt", 
                                header = TRUE, 
                                sep = "\t", 
                                quote = "", 
                                dec = ".", 
                                strip.white = TRUE,
                                comment.char = "")

#we are interested in secchi, TP and chla for now.  first separate out limno data and secchi so they only include summer values (june 15-sept 15)

##separate sample date into Y, M, D so we have a date of month for summer cutoffs
data$year = as.numeric(format.POSIXct(x=data$sampledate, format="%Y"))
data$month = as.numeric(format.POSIXct(x=data$sampledate, format="%m"))
data$DoM = as.numeric(format.POSIXct(x=data$sampledate, format="%d"))

#create a data subset which only includes stoich samples that were 
#collected in summer (June 15-Sept 15)
data.summer.1 = data[which(data$samplemonth>6 & data$samplemonth<9),]
data.summer.2 = data[which(data$samplemonth==6 & data$DoM>=15),]
data.summer.3 = data[which(data$samplemonth==9 & data$DoM<=15),]
data.summer = rbind(data.summer.1, data.summer.2, data.summer.3)

##do the same for the secchi data
secchi$year = as.numeric(format.POSIXct(x=secchi$sampledate, format="%Y"))
secchi$month = as.numeric(format.POSIXct(x=secchi$sampledate, format="%m"))
secchi$DoM = as.numeric(format.POSIXct(x=secchi$sampledate, format="%d"))

secchi.summer.1 = secchi[which(secchi$samplemonth>6 & secchi$samplemonth<9),]
secchi.summer.2 = secchi[which(secchi$samplemonth==6 & secchi$DoM>=15),]
secchi.summer.3 = secchi[which(secchi$samplemonth==9 & secchi$DoM<=15),]
secchi.summer = rbind(secchi.summer.1, secchi.summer.2, secchi.summer.3)

#wanted to cut off any samples before 1970 (that's when monthly/annual climate samples start), but would need to use 1971 or 1972 because we need year x-1 cliamte data.  But for some reason ENSO data don't start until 1979 even though everything else is 1970?  So for now went with 1980 (to have ENSO for year x and year x-1) but should look into this.  We only lose a few hundred lakes by eliminating the 1970's.  Monthly/annual climate end in 2011 so stopped there even though there are some limno data from 2012 and 2013.  That eliminates only a handful of lakes though.
data.summer.current = data.summer[which(data.summer$sampleyear %in% c(1980:2011)), ]
secchi.summer.current = secchi.summer[which(secchi.summer$sampleyear %in% c(1980:2011)), ]

#take yearly average of each limno variable

tp.data = data.summer.current[!is.na(data.summer.current$tp),]
annual.tp = aggregate(tp.data[,c("tp")], by=list(tp.data$lagoslakeid, tp.data$sampleyear), FUN="median")
names(annual.tp)<-c("lagoslakeid", "year", "annual.tp")
annual.tp = annual.tp[order(annual.tp$lagoslakeid, annual.tp$year),]

chla.data = data.summer.current[!is.na(data.summer.current$chla),]
annual.chla = aggregate(chla.data[,c("chla")], by=list(chla.data$lagoslakeid, chla.data$sampleyear), FUN="median")
names(annual.chla)<-c("lagoslakeid", "year", "annual.chla")
annual.chla = annual.chla[order(annual.chla$lagoslakeid, annual.chla$year),]

secchi.data = secchi.summer.current[!is.na(secchi.summer.current$secchi),]
annual.secchi = aggregate(secchi.data[,c("secchi")], by=list(secchi.data$lagoslakeid, secchi.data$sampleyear), FUN="median")
names(annual.secchi)<-c("lagoslakeid", "year", "annual.secchi")
annual.secchi = annual.secchi[order(annual.secchi$lagoslakeid, annual.secchi$year),]

tn.data = data.summer.current[!is.na(data.summer.current$tn),]
annual.tn = aggregate(tn.data[,c("tn")], by=list(tn.data$lagoslakeid, tn.data$sampleyear), FUN="median")
names(annual.tn)<-c("lagoslakeid", "year", "annual.tn")
annual.tn = annual.tn[order(annual.tn$lagoslakeid, annual.tn$year),]

#trim lake characteristics data frame to what we need (lakeid, hu12id, lat and long to plot location of sampled lakes)
lake.chars = data.lake.specific[,c(1, 3, 4, 14, 20)]

#merge three limno variables into lake characteristics trimmed
tp.chla=merge(annual.tp, annual.chla, by=c("lagoslakeid", "year"), all.x=TRUE, all.y=TRUE)
tp.chla.secchi=merge(tp.chla, annual.secchi, by=c("lagoslakeid", "year"), all.x=TRUE, all.y=TRUE)
tp.chla.secchi.tn=merge(tp.chla.secchi, annual.tn, by=c("lagoslakeid", "year"), all.x=TRUE, all.y=TRUE)
lakes.limno=merge(tp.chla.secchi.tn, lake.chars, by="lagoslakeid", all.x=TRUE, all.y=FALSE)

#data.for.nick.pdsi<-lakes.limno[!duplicated(lakes.limno[,'lagoslakeid']),]
#data.for.nick.pdsi<-data.for.nick.pdsi[,c(1, 6,7)]
#setwd("/Users/SarahiMac/Dropbox/Sarah_Work/Manuscripts/2016_climate_waterqual/Data")
#write.csv(data.for.nick.pdsi, "climate.lake.coords.for.pdsi.csv")

#how many lakes do we have?
dim(lakes.limno)
length(unique(lakes.limno$lagoslakeid))

length(unique(annual.tp$lagoslakeid))
(82808-sum(is.na(lakes.limno$annual.tp)))/length(unique(annual.tp$lagoslakeid))
length(unique(annual.chla$lagoslakeid))
(82808-sum(is.na(lakes.limno$annual.chla)))/length(unique(annual.chla$lagoslakeid))
length(unique(annual.secchi$lagoslakeid))
(82808-sum(is.na(lakes.limno$annual.secchi)))/length(unique(annual.secchi$lagoslakeid))


#where are the lakes with data?
library(maps)
library(mapdata)

map("worldHires", "Canada", xlim=c(min(lakes.limno$nhd_long, na.rm=TRUE)-1,max(lakes.limno$nhd_long, na.rm=TRUE)+1), ylim=c(min(lakes.limno$nhd_lat, na.rm=TRUE)-1,max(lakes.limno$nhd_lat, na.rm=TRUE)+1), fill=TRUE, col="khaki", lwd=2, bg="lightblue1")
map("worldHires", "USa", add=TRUE,  fill=TRUE, col="khaki", lwd=2)
map("state", boundary = FALSE, add = TRUE)
text(-75, 46, "lake locations", cex=1.5)
points(lakes.limno$nhd_long, lakes.limno$nhd_lat, pch=19, col="blue", cex=0.5)
