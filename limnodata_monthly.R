#script to create summer medians for desired LAGOS limno variables for each year of data for Shuai to use in climate multi task learning analysis.  originally doing this with LAGOS 1.054 for getting the analysis set up but will shift to 1.087 when it is available

#read LAGOS limno data, lake identifier data, and separate secchi dataset

setwd("/Users/SarahiMac/Dropbox/CSI/CSI-LIMNO_DATA/LAGOSData/Version1.054.2")
data = read.table("lagos_epi_nutr_10542.txt", 
                  header = TRUE, 
                  sep = "\t", 
                  quote = "", 
                  dec = ".", 
                  strip.white = TRUE, 
                  comment.char = "", 
                  colClasses=c(sampledate = "POSIXct"))
data.lake.specific = read.table("lagos_lakes_10542.txt", 
                                header = TRUE, 
                                sep = "\t", 
                                quote = "", 
                                dec = ".", 
                                strip.white = TRUE,
                                comment.char = "")
secchi = read.table("lagos_secchi_10542.txt", 
                           header = TRUE, 
                           sep = "\t", 
                           quote = "", 
                           dec = ".", 
                           strip.white = TRUE, 
                           comment.char = "", 
                           colClasses=c(sampledate = "POSIXct"))

#we are interested in secchi, TP and chla for now.  first separate out limno data and secchi so they only include summer values (june 15-sept 15)

##separate sample date into Y, M, D so we have a date of month for summer cutoffs
data$year = as.numeric(format.POSIXct(x=data$sampledate, format="%Y"))
data$month = as.numeric(format.POSIXct(x=data$sampledate, format="%m"))
data$DoM = as.numeric(format.POSIXct(x=data$sampledate, format="%d"))

#create a data subset which only includes stoich samples that were 
#collected in summer (June 15-Sept 15)
data.june.1 = data[which(data$samplemonth==6 & data$DoM>=15),]
data.june.2 = data[which(data$samplemonth==7 & data$DoM<15),]
data.june = rbind(data.june.1, data.june.2)

data.july.1 = data[which(data$samplemonth==7 & data$DoM>=15),]
data.july.2 = data[which(data$samplemonth==8 & data$DoM<15),]
data.july = rbind(data.july.1, data.july.2)

data.aug.1 = data[which(data$samplemonth==8 & data$DoM>=15),]
data.aug.2 = data[which(data$samplemonth==9 & data$DoM<15),]
data.aug = rbind(data.aug.1, data.aug.2)


##do the same for the secchi data
secchi$year = as.numeric(format.POSIXct(x=secchi$sampledate, format="%Y"))
secchi$month = as.numeric(format.POSIXct(x=secchi$sampledate, format="%m"))
secchi$DoM = as.numeric(format.POSIXct(x=secchi$sampledate, format="%d"))

secchi.june.1 = secchi[which(secchi$samplemonth==6 & secchi$DoM>=15),]
secchi.june.2 = secchi[which(secchi$samplemonth==7 & secchi$DoM<15),]
secchi.june = rbind(secchi.june.1, secchi.june.2)

secchi.july.1 = secchi[which(secchi$samplemonth==7 & secchi$DoM>=15),]
secchi.july.2 = secchi[which(secchi$samplemonth==8 & secchi$DoM<15),]
secchi.july = rbind(secchi.july.1, secchi.july.2)

secchi.aug.1 = secchi[which(secchi$samplemonth==8 & secchi$DoM>=15),]
secchi.aug.2 = secchi[which(secchi$samplemonth==9 & secchi$DoM<15),]
secchi.aug = rbind(secchi.aug.1, secchi.aug.2)


#wanted to cut off any samples before 1970 (that's when monthly/annual climate samples start), but would need to use 1971 or 1972 because we need year x-1 cliamte data.  But for some reason ENSO data don't start until 1979 even though everything else is 1970?  So for now went with 1980 (to have ENSO for year x and year x-1) but should look into this.  We only lose a few hundred lakes by eliminating the 1970's.  Monthly/annual climate end in 2011 so stopped there even though there are some limno data from 2012 and 2013.  That eliminates only a handful of lakes though.
data.june.current = data.june[which(data.june$sampleyear %in% c(1980:2011)), ]
secchi.june.current = secchi.june[which(secchi.june$sampleyear %in% c(1980:2011)), ]

data.july.current = data.july[which(data.july$sampleyear %in% c(1980:2011)), ]
secchi.july.current = secchi.july[which(secchi.july$sampleyear %in% c(1980:2011)), ]

data.aug.current = data.aug[which(data.aug$sampleyear %in% c(1980:2011)), ]
secchi.aug.current = secchi.aug[which(secchi.aug$sampleyear %in% c(1980:2011)), ]

#take june, july and aug averages of each limno variable

tp.data.june = data.june.current[!is.na(data.june.current$tp),]
annual.tp.june = aggregate(tp.data.june[,c("tp")], by=list(tp.data.june$lagoslakeid, tp.data.june$sampleyear), FUN="median")
names(annual.tp.june)<-c("lagoslakeid", "year", "june.tp")
annual.tp.june = annual.tp.june[order(annual.tp.june$lagoslakeid, annual.tp.june$year),]

chla.data.june = data.june.current[!is.na(data.june.current$chla),]
annual.chla.june = aggregate(chla.data.june[,c("chla")], by=list(chla.data.june$lagoslakeid, chla.data.june$sampleyear), FUN="median")
names(annual.chla.june)<-c("lagoslakeid", "year", "june.chla")
annual.chla.june = annual.chla.june[order(annual.chla.june$lagoslakeid, annual.chla.june$year),]

secchi.data.june = secchi.june.current[!is.na(secchi.june.current$secchi),]
annual.secchi.june = aggregate(secchi.data.june[,c("secchi")], by=list(secchi.data.june$lagoslakeid, secchi.data.june$sampleyear), FUN="median")
names(annual.secchi.june)<-c("lagoslakeid", "year", "june.secchi")
annual.secchi.june = annual.secchi.june[order(annual.secchi.june$lagoslakeid, annual.secchi.june$year),]

tn.data.june = data.june.current[!is.na(data.june.current$tn),]
annual.tn.june = aggregate(tn.data.june[,c("tn")], by=list(tn.data.june$lagoslakeid, tn.data.june$sampleyear), FUN="median")
names(annual.tn.june)<-c("lagoslakeid", "year", "june.tn")
annual.tn.june = annual.tn.june[order(annual.tn.june$lagoslakeid, annual.tn.june$year),]

##july data

tp.data.july = data.july.current[!is.na(data.july.current$tp),]
annual.tp.july = aggregate(tp.data.july[,c("tp")], by=list(tp.data.july$lagoslakeid, tp.data.july$sampleyear), FUN="median")
names(annual.tp.july)<-c("lagoslakeid", "year", "july.tp")
annual.tp.july = annual.tp.july[order(annual.tp.july$lagoslakeid, annual.tp.july$year),]

chla.data.july = data.july.current[!is.na(data.july.current$chla),]
annual.chla.july = aggregate(chla.data.july[,c("chla")], by=list(chla.data.july$lagoslakeid, chla.data.july$sampleyear), FUN="median")
names(annual.chla.july)<-c("lagoslakeid", "year", "july.chla")
annual.chla.july = annual.chla.july[order(annual.chla.july$lagoslakeid, annual.chla.july$year),]

secchi.data.july = secchi.july.current[!is.na(secchi.july.current$secchi),]
annual.secchi.july = aggregate(secchi.data.july[,c("secchi")], by=list(secchi.data.july$lagoslakeid, secchi.data.july$sampleyear), FUN="median")
names(annual.secchi.july)<-c("lagoslakeid", "year", "july.secchi")
annual.secchi.july = annual.secchi.july[order(annual.secchi.july$lagoslakeid, annual.secchi.july$year),]

tn.data.july = data.july.current[!is.na(data.july.current$tn),]
annual.tn.july = aggregate(tn.data.july[,c("tn")], by=list(tn.data.july$lagoslakeid, tn.data.july$sampleyear), FUN="median")
names(annual.tn.july)<-c("lagoslakeid", "year", "july.tn")
annual.tn.july = annual.tn.july[order(annual.tn.july$lagoslakeid, annual.tn.july$year),]

##august data

tp.data.aug = data.aug.current[!is.na(data.aug.current$tp),]
annual.tp.aug = aggregate(tp.data.aug[,c("tp")], by=list(tp.data.aug$lagoslakeid, tp.data.aug$sampleyear), FUN="median")
names(annual.tp.aug)<-c("lagoslakeid", "year", "aug.tp")
annual.tp.aug = annual.tp.aug[order(annual.tp.aug$lagoslakeid, annual.tp.aug$year),]

chla.data.aug = data.aug.current[!is.na(data.aug.current$chla),]
annual.chla.aug = aggregate(chla.data.aug[,c("chla")], by=list(chla.data.aug$lagoslakeid, chla.data.aug$sampleyear), FUN="median")
names(annual.chla.aug)<-c("lagoslakeid", "year", "aug.chla")
annual.chla.aug = annual.chla.aug[order(annual.chla.aug$lagoslakeid, annual.chla.aug$year),]

secchi.data.aug = secchi.aug.current[!is.na(secchi.aug.current$secchi),]
annual.secchi.aug = aggregate(secchi.data.aug[,c("secchi")], by=list(secchi.data.aug$lagoslakeid, secchi.data.aug$sampleyear), FUN="median")
names(annual.secchi.aug)<-c("lagoslakeid", "year", "aug.secchi")
annual.secchi.aug = annual.secchi.aug[order(annual.secchi.aug$lagoslakeid, annual.secchi.aug$year),]

tn.data.aug = data.aug.current[!is.na(data.aug.current$tn),]
annual.tn.aug = aggregate(tn.data.aug[,c("tn")], by=list(tn.data.aug$lagoslakeid, tn.data.aug$sampleyear), FUN="median")
names(annual.tn.aug)<-c("lagoslakeid", "year", "aug.tn")
annual.tn.aug = annual.tn.aug[order(annual.tn.aug$lagoslakeid, annual.tn.aug$year),]

#trim lake characteristics data frame to what we need (lakeid, hu12id, lat and long to plot location of sampled lakes)
lake.chars = data.lake.specific[,c(1, 3, 4, 14, 20)]

#merge four limno variables into lake characteristics trimmed for each month
tp.chla.june=merge(annual.tp.june, annual.chla.june, by=c("lagoslakeid", "year"), all.x=TRUE, all.y=TRUE)
tp.chla.secchi.june=merge(tp.chla.june, annual.secchi.june, by=c("lagoslakeid", "year"), all.x=TRUE, all.y=TRUE)
tp.chla.secchi.tn.june=merge(tp.chla.secchi.june, annual.tn.june, by=c("lagoslakeid", "year"), all.x=TRUE, all.y=FALSE)
lakes.limno.june=merge(tp.chla.secchi.tn.june, lake.chars, by="lagoslakeid", all.x=TRUE, all.y=FALSE)

tp.chla.july=merge(annual.tp.july, annual.chla.july, by=c("lagoslakeid", "year"), all.x=TRUE, all.y=TRUE)
tp.chla.secchi.july=merge(tp.chla.july, annual.secchi.july, by=c("lagoslakeid", "year"), all.x=TRUE, all.y=TRUE)
tp.chla.secchi.tn.july=merge(tp.chla.secchi.july, annual.tn.july, by=c("lagoslakeid", "year"), all.x=TRUE, all.y=TRUE)
lakes.limno.july=merge(tp.chla.secchi.tn.july, lake.chars, by="lagoslakeid", all.x=TRUE, all.y=FALSE)

tp.chla.aug=merge(annual.tp.aug, annual.chla.aug, by=c("lagoslakeid", "year"), all.x=TRUE, all.y=TRUE)
tp.chla.secchi.aug=merge(tp.chla.aug, annual.secchi.aug, by=c("lagoslakeid", "year"), all.x=TRUE, all.y=TRUE)
tp.chla.secchi.tn.aug=merge(tp.chla.secchi.aug, annual.tn.aug, by=c("lagoslakeid", "year"), all.x=TRUE, all.y=TRUE)
lakes.limno.aug=merge(tp.chla.secchi.tn.aug, lake.chars, by="lagoslakeid", all.x=TRUE, all.y=FALSE)


#how many lakes do we have?
dim(lakes.limno.june)
length(unique(lakes.limno.june$lagoslakeid))

length(unique(annual.tp.june$lagoslakeid))
(59968-sum(is.na(lakes.limno.june$june.tp)))/length(unique(annual.tp.june$lagoslakeid))
length(unique(annual.chla.june$lagoslakeid))
(59968-sum(is.na(lakes.limno.june$june.chla)))/length(unique(annual.chla.june$lagoslakeid))
length(unique(annual.secchi.june$lagoslakeid))
(59968-sum(is.na(lakes.limno.june$june.secchi)))/length(unique(annual.secchi.june$lagoslakeid))


#where are the lakes with data?
library(maps)
library(mapdata)

map("worldHires", "Canada", xlim=c(min(lakes.limno$nhd_long, na.rm=TRUE)-1,max(lakes.limno$nhd_long, na.rm=TRUE)+1), ylim=c(min(lakes.limno$nhd_lat, na.rm=TRUE)-1,max(lakes.limno$nhd_lat, na.rm=TRUE)+1), fill=TRUE, col="khaki", lwd=2, bg="lightblue1")
map("worldHires", "USa", add=TRUE,  fill=TRUE, col="khaki", lwd=2)
map("state", boundary = FALSE, add = TRUE)
text(-75, 46, "lake locations", cex=1.5)
points(lakes.limno.aug$nhd_long, lakes.limno.aug$nhd_lat, pch=19, col="blue", cex=0.5)
