#climate data for EDI prep - want to get together annual/seasonal/monthly files for each PRISM variable.  We only have seasonal data calc'ed for tmean and ppt, so caluclate for tmin and tmax too.

library(dplyr)
##make each variable into a single csv with monthly, annual, seasonal data


#still need seasonal data for tmax and tmin
setwd("~/Dropbox/Sarah_Work/Manuscripts/2017_LAGOSClimateWaterQual/Data/Climate_raw")

climate.month.to.season <- function(variable, scale) {
  temp = read.table(paste(scale, "_", variable, "_merge.txt", sep = ""), header = TRUE)
  for (i in 1:(nrow(temp))) {
    current_year = temp$year[i]
    current_id = temp[i,1]
    if (temp$year[i] > 1970) {
      if (temp$year[i] %in% seq(1972,2011,4) == TRUE) {
        temp$winter[i] = temp$December[temp$year == (current_year-1)&temp[,1] == current_id]
        temp$winter[i] = ((temp$winter[i]*31)  + (temp$January[i]*31) + (temp$February[i]*29))/91
        temp$spring[i] = ((temp$March[i]*31) + (temp$April[i]*30) + (temp$May[i]*31))/92
        temp$summer[i] = ((temp$June[i]*30) + (temp$July[i]*31) + (temp$August[i]*31))/92
        temp$fall[i] = ((temp$September[i]*30) + (temp$October[i]*31) + (temp$November[i]*30))/91
        
      } else {
        temp$winter[i] = temp$December[temp$year == (current_year-1)&temp[,1] == current_id]
        temp$winter[i] = ((temp$winter[i]*31) + (temp$January[i]*31) + (temp$February[i]*28))/90
        temp$spring[i] = ((temp$March[i]*31) + (temp$April[i]*30) + (temp$May[i]*31))/92
        temp$summer[i] = ((temp$June[i]*30) + (temp$July[i]*31) + (temp$August[i]*31))/92
        temp$fall[i] = ((temp$September[i]*30) + (temp$October[i]*31) + (temp$November[i]*30))/91
        
      }
    } else {
      temp$winter[i] = NA
      temp$winter[i] = ((temp$winter[i]*31) + (temp$January[i]*31) + (temp$February[i]*28))/90
      temp$spring[i] = ((temp$March[i]*31) + (temp$April[i]*30) + (temp$May[i]*31))/92
      temp$summer[i] = ((temp$June[i]*30) + (temp$July[i]*31) + (temp$August[i]*31))/92
      temp$fall[i] = ((temp$September[i]*30) + (temp$October[i]*31) + (temp$November[i]*30))/91
      
    }
  }
  return(temp)
}


hu12.tmin.season = climate.month.to.season("tmin", "HU12")
hu12.tmax.season = climate.month.to.season("tmax", "HU12")

setwd("~/Dropbox/CSI&CL/MSB-2-2015/ClimateData/Data/AnnualMonthlySeasonal_HU12")

write.csv(hu12.tmin.season, "hu12_tmin_seasonal.csv")
write.csv(hu12.tmax.season, "hu12_tmax_seasonal.csv")

#load in all csv files and merge to make a single csv for each variable with all scales of data
tmean.a<-read.csv("hu12_tmean_annual.csv", header=T)
tmean.s<-read.csv("hu12_tmean_seasonal.csv", header=T)
ppt.a<-read.csv("hu12_ppt_annual.csv", header=T)
ppt.s<-read.csv("hu12_ppt_seasonal.csv", header=T)
tmin.a<-read.csv("hu12_tmin_annual.csv", header=T)
tmin.s<-read.csv("hu12_tmin_seasonal.csv", header=T)
tmax.a<-read.csv("hu12_tmax_annual.csv", header=T)
tmax.s<-read.csv("hu12_tmax_seasonal.csv", header=T)

#make single file for each variable with monthly, seasonal, annual
#change hu12 zone id col names to match LAGOS
meanann<-na.omit(tmean.a[,c(2,3,16)])
names(meanann)<-c("hu12_zoneid", "year", "tmean_annual")
tmean<-na.omit(tmean.s[,c(2:19)])
colnames(tmean)<-paste("tmean", colnames(tmean), sep = "_")
names(tmean)<-c("hu12_zoneid", "year", "tmean_January", "tmean_February", "tmean_March", "tmean_April", "tmean_May", "tmean_June", "tmean_July", "tmean_August", "tmean_September", "tmean_October", "tmean_November", "tmean_December", "tmean_winter", "tmean_spring", "tmean_summer", "tmean_fall")

tmean.msa<-merge(tmean, meanann, by=c("hu12_zoneid", "year"))


pptann<-na.omit(ppt.a[,c(2,3,16)])
names(pptann)<-c("hu12_zoneid", "year", "ppt_annual")
ppt<-na.omit(ppt.s[,c(2:19)])
names(ppt)<-c("hu12_zoneid", "year", "ppt_January", "ppt_February", "ppt_March", "ppt_April", "ppt_May", "ppt_June", "ppt_July", "ppt_August", "ppt_September", "ppt_October", "ppt_November", "ppt_December", "ppt_winter", "ppt_spring", "ppt_summer", "ppt_fall")

ppt.msa<-merge(ppt, pptann, by=c("hu12_zoneid", "year"))

maxann<-na.omit(tmax.a[,c(2,3,16)])
names(maxann)<-c("hu12_zoneid", "year", "tmax_annual")
tmax<-na.omit(tmax.s[,c(2:19)])
names(tmax)<-c("hu12_zoneid", "year", "tmax_January", "tmax_February", "tmax_March", "tmax_April", "tmax_May", "tmax_June", "tmax_July", "tmax_August", "tmax_September", "tmax_October", "tmax_November", "tmax_December", "tmax_winter", "tmax_spring", "tmax_summer", "tmax_fall")

tmax.msa<-merge(tmax, maxann, by=c("hu12_zoneid", "year"))


minann<-na.omit(tmin.a[,c(2,3,16)])
names(minann)<-c("hu12_zoneid", "year", "tmin_annual")
tmin<-na.omit(tmin.s[,c(2:19)])
names(tmin)<-c("hu12_zoneid", "year", "tmin_January", "tmin_February", "tmin_March", "tmin_April", "tmin_May", "tmin_June", "tmin_July", "tmin_August", "tmin_September", "tmin_October", "tmin_November", "tmin_December", "tmin_winter", "tmin_spring", "tmin_summer", "tmin_fall")

tmin.msa<-merge(tmin, minann, by=c("hu12_zoneid", "year"))

#write files to EDI submission folder
setwd("~/Dropbox/CSI&CL/MSB-2-2015/ClimateData/Data/EDISubmission")
write.csv(tmean.msa, "hu12_tmean.csv")
write.csv(ppt.msa, "hu12_ppt.csv")
write.csv(tmax.msa, "hu12_tmax.csv")
write.csv(tmin.msa, "hu12_tmin.csv")

##calculate seasonal data for PHDI from Nick's file with all lagoslakeid>4ha
##cut off date 1970 to match other data

setwd("~/Dropbox/CSI&CL/MSB-2-2015/ClimateData/Data/PhDI")

phdinick<-read.csv("phdi_by_clim_div_4_12_18.csv", header=T)

phdi<-phdinick[phdinick$year>1969,c(3:15,17)]

rm(phdinick)

names(phdi)[1:12]<-c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

phdi<-na.omit(phdi)

#calc annual and seasonal from monthly data

#climate.month.to.season <- function(scale, variable, scale_col_name) {
  #temp = read.table(paste(scale, "_", variable, "_merge.txt", sep = ""), header = TRUE)
  
  temp <- phdi
  scale_col_name<-"lagoslakeid"
 
  temp_prev <- select_(temp, scale_col_name, 'year', 'September', 'October', 'November', 'December') %>%
    rename(prev_september = September, prev_october=October, prev_november=November, prev_december=December) %>%
    mutate(year = year + 1)
  
  temp_all <- left_join(temp, temp_prev, by = c(scale_col_name, 'year'))
  
  
  # get other data
  temp.leap <- filter(temp_all, year %in% seq(1972, 2011, 4))
  temp.notleap <- filter(temp_all, !(year %in% seq(1972, 2011, 4)))
  
  # calc leap year vals
  temp.leap$winter = ((temp.leap$prev_december*31)  + (temp.leap$January*31) + (temp.leap$February*29))/91
  temp.leap$spring = ((temp.leap$March*31) + (temp.leap$April*30) + (temp.leap$May*31))/92
  temp.leap$summer = ((temp.leap$June*30) + (temp.leap$July*31) + (temp.leap$August*31))/92
  temp.leap$fall = ((temp.leap$September*30) + (temp.leap$October*31) + (temp.leap$November*30))/91
  temp.leap$annual = ((temp.leap$prev_september*30) + 
                      (temp.leap$prev_october*31) +
                      (temp.leap$prev_november*30) + 
                      (temp.leap$prev_december*31) + 
                      (temp.leap$January*31) +
                      (temp.leap$February*29) + 
                      (temp.leap$March*31) + 
                      (temp.leap$April*30) +
                      (temp.leap$May*31) + 
                      (temp.leap$June*30) + 
                      (temp.leap$July*31) +
                      (temp.leap$August*31))/366
 
  
  # calc non-leap year vals
  temp.notleap$winter = ((temp.notleap$prev_december*31) + (temp.notleap$January*31) + (temp.notleap$February*28))/90
  temp.notleap$spring = ((temp.notleap$March*31) + (temp.notleap$April*30) + (temp.notleap$May*31))/92
  temp.notleap$summer = ((temp.notleap$June*30) + (temp.notleap$July*31) + (temp.notleap$August*31))/92
  temp.notleap$fall = ((temp.notleap$September*30) + (temp.notleap$October*31) + (temp.notleap$November*30))/91
  temp.notleap$annual = ((temp.notleap$prev_september*30) + 
                      (temp.notleap$prev_october*31) +
                      (temp.notleap$prev_november*30) + 
                      (temp.notleap$prev_december*31) + 
                      (temp.notleap$January*31) +
                      (temp.notleap$February*28) + 
                      (temp.notleap$March*31) + 
                      (temp.notleap$April*30) +
                      (temp.notleap$May*31) + 
                      (temp.notleap$June*30) + 
                      (temp.notleap$July*31) +
                      (temp.notleap$August*31))/365
  
  phdi.msa <- bind_rows(temp.leap, temp.notleap) %>%
    select(-prev_december, -prev_november, -prev_october, -prev_september) %>%
    arrange(lagoslakeid, year)
  
#  return(out)
#}
  
phdi.msa.sort<-phdi.msa[,c(14,13,1:12,15:19)]
colnames(phdi.msa.sort)[3:19]<-paste("PHDI", colnames(phdi.msa.sort[,c(3:19)]), sep = "_")
phdi.all<-na.omit(phdi.msa.sort)

write.csv(phdi.all, "phdi_mo_seas_ann.csv")



