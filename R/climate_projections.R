library(dplyr)

# calculate median values of May, June, Summer (June, July, August) temp
# calculate median values of Nov, Jan, Winter (Dec, Jan, Feb)

# read in monthly and seasonal precip

ppt <- readRDS('Data/hu12.ppt.season.rds')
head(ppt)

med.ppt <- group_by(ppt, ZoneID) %>%
  summarize_at(vars(November, January, winter), funs(median), na.rm = T)

med.ppt.future <- med.ppt %>%
  mutate(November = November*1.15, 
         January = January*1.15,
         winter = winter*1.15)


temp <- readRDS('Data/hu12.tmean.season.rds')

med.temp <- group_by(temp, ZoneID) %>%
  summarize_at(vars(May, June, summer), funs(median), na.rm = T)

med.temp.future <- med.temp %>%
  mutate(May = May+(1.67*100),
         June = June+(1.67*100),
         summer = summer + (1.67*100))

slope = function(x,y){
  coefficients(lm(y~x))[[2]]*10
}

chng.ppt <- group_by(ppt, ZoneID) %>%
  mutate(yrly_change = slope(year, winter))

saveRDS(med.ppt, 'Data/hu12_ppt_median.rds')
saveRDS(med.temp, 'Data/hu12_temp_median.rds')
saveRDS(med.ppt.future, 'Data/hu12_ppt_plus15percent.rds')
saveRDS(med.temp.future, 'Data/hu12_temp_plus1point7degC.rds')


chng.temp <- group_by(temp, ZoneID) %>%
  mutate(yrly_change = slope(year, summer))

head(med.ppt)


