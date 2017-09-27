#climate metrics (at hu12 scale, smallest available) to use in multitask learning analysis.  mix of monthly, seasonal, and annual data

## Modified version of Sam's code to convert monthly climate variables into yearly climate variables and convert monthly climate vars to seasonal climate vars


#copied merged HU12 and index raw data to a local folder
setwd("~/Dropbox/Sarah_Work/Manuscripts/2016_climate_waterqual/Data/Climate_raw")

#list of vars to include:
#annuals: temp and precip from year x, and x-1 (4)
#seasonals: fall, winter, spring, summer for temp and precip (8)
#monthlys: 12 mo temp and precip (sept x-1 to aug x) (24)
#indicies: PDSI, ENSO, NAO for year x and year x-1.  PDSI is specific by state, others are same for everything (6)
#total is 42 variables

# convert montly vals to annual vals
# note that this code accounts for number of days per month 
# and leap years
# changed sam's standard code to include a Sept-Aug year to be the 12 months before sampling (limno samples are summer medians so went with August becuase we don't knwo which month data are actually from)

climate.month.to.year <- function(variable, scale) {
  temp = read.table(paste(scale, "_", variable, "_merge.txt", sep = ""), header = TRUE)
  for (i in 1:nrow(temp)){
    if (temp$year[i] %in% seq(1972,2011,4) == TRUE) {
      temp$annual[i] = ((temp$September[i-1]*30) + 
                          (temp$October[i-1]*31) +
                          (temp$November[i-1]*30) + 
                          (temp$December[i-1]*31) + 
                          (temp$January[i]*31) +
                          (temp$February[i]*29) + 
                          (temp$March[i]*31) + 
                          (temp$April[i]*30) +
                          (temp$May[i]*31) + 
                          (temp$June[i]*30) + 
                          (temp$July[i]*31) +
                          (temp$August[i]*31))/366
      
      
    } else {
      temp$annual[i] = ((temp$September[i-1]*30) + 
                          (temp$October[i-1]*31) +
                          (temp$November[i-1]*30) + 
                          (temp$December[i-1]*31) + 
                          (temp$January[i]*31) +
                          (temp$February[i]*28) + 
                          (temp$March[i]*31) + 
                          (temp$April[i]*30) +
                          (temp$May[i]*31) + 
                          (temp$June[i]*30) + 
                          (temp$July[i]*31) +
                          (temp$August[i]*31))/365
    }
  }
  return(temp)
}


# convert monthly vals to seasonal vals,
# where winter = Dec, Jan, Feb
# spring = March, April, May
# summer = June, Jul, Aug
# fall = Sep, Oct, Nov
# note that the first year in record (1970) has NA values because 
# winter is calculated with previous years' December value, and there is no
# 1969 data

###Sam and Eric and I identified a fix for this because the indexing would use the previous hu12
##rather than the previous year for the first year of each unit. and it would be messed up
##if we didn't have a well sorted file by spatial unit and year.  Hence, we define current year and
##current spatial unit so we can make sure the december value from the previous year and the rest of the
##vals in a given column match well.  Identified this while doing the same thing with PDSI data. Sam's 
##original version is ok because it assigns NAs to the first year of each HU12

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




hu12.tmean.annual = climate.month.to.year("tmean", "HU12")
hu12.ppt.annual = climate.month.to.year("ppt", "HU12")
hu12.tmean.season = climate.month.to.season("tmean", "HU12")
hu12.tmin.annual = climate.month.to.year("tmin", "HU12")
hu12.tmax.annual = climate.month.to.year("tmax", "HU12")
hu12.ppt.season = climate.month.to.season("ppt", "HU12")


#these took freaking forever (hours each) to calculate so save them as .csv now for any future needs
setwd("~/Dropbox/Sarah_Work/Manuscripts/2016_climate_waterqual/Data/Annual_monthly_calculated")

write.csv(hu12.tmean.annual, "hu12_tmean_annual.csv")
write.csv(hu12.tmin.annual, "hu12_tmin_annual.csv")
write.csv(hu12.tmax.annual, "hu12_tmax_annual.csv")
write.csv(hu12.ppt.annual, "hu12_ppt_annual.csv")
write.csv(hu12.tmean.season, "hu12_tmean_seasonal_updated.csv")
write.csv(hu12.ppt.season, "hu12_ppt_seasonal_updated.csv")

