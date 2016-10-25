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

#cut off any samples before 1970 (that's when monthly/annual climate samples start)
data.summer.current = data.summer[which(data.summer$sampleyear %in% c(1970:2013)), ]
secchi.summer.current = secchi.summer[which(secchi.summer$sampleyear %in% c(1970:2013)), ]

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

#trim lake characteristics data frame to what we need (lakeid, hu12id, lat and long to plot location of sampled lakes)
lake.chars = data.lake.specific[,c(1, 3, 4, 14, 20)]

#merge three limno variables into lake characteristics trimmed
tp.chla=merge(annual.tp, annual.chla, by=c("lagoslakeid", "year"), all.x=TRUE, all.y=TRUE)
tp.chla.secchi=merge(tp.chla, annual.secchi, by=c("lagoslakeid", "year"), all.x=TRUE, all.y=TRUE)
lakes.limno=merge(tp.chla.secchi, lake.chars, by="lagoslakeid", all.x=TRUE, all.y=FALSE)

#how many lakes do we have?
dim(lakes.limno)
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
