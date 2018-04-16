##nick provided palmer data and suggested using PHDI instead of PDSI because it's more explicitly related to lakes.  data are aggregated from climate divisions and he calculated them for each lagoslakeid in the climate dataset.  here, i change the data from monthly to seasonal using the same loop that sam created for the monthly climate data

setwd("~/Dropbox/Sarah_Work/Manuscripts/2016_climate_waterqual/Data/PDSI from Nick")


phdi = read.csv("phdi_by_clim_div.csv", header = TRUE)


for (i in 1:(nrow(phdi))) {
  current_year = phdi$year[i]
  current_id = phdi$lagoslakeid[i]
  if (phdi$year[i] > 1970) {
    if (phdi$year[i] %in% seq(1972,2011,4) == TRUE) {
      phdi$winter[i] = phdi$Dec[phdi$year==(current_year-1)&phdi$lagoslakeid==current_id]
      phdi$winter[i] = ((phdi$winter[i]*31) + (phdi$Jan[i]*31) + (phdi$Feb[i]*29))/91
      phdi$spring[i] = ((phdi$Mar[i]*31) + (phdi$Apr[i]*30) + (phdi$May[i]*31))/92
      phdi$summer[i] = ((phdi$Jun[i]*30) + (phdi$Jul[i]*31) + (phdi$Aug[i]*31))/92
      phdi$fall[i] = ((phdi$Sep[i]*30) + (phdi$Oct[i]*31) + (phdi$Nov[i]*30))/91
      
    } else {
      phdi$winter[i] = phdi$Dec[phdi$year==(current_year-1)&phdi$lagoslakeid==current_id]
      phdi$winter[i] = ((phdi$winter[i]*31) + (phdi$Jan[i]*31) + (phdi$Feb[i]*28))/90
      phdi$spring[i] = ((phdi$Mar[i]*31) + (phdi$Apr[i]*30) + (phdi$May[i]*31))/92
      phdi$summer[i] = ((phdi$Jun[i]*30) + (phdi$Jul[i]*31) + (phdi$Aug[i]*31))/92
      phdi$fall[i] = ((phdi$Sep[i]*30) + (phdi$Oct[i]*31) + (phdi$Nov[i]*30))/91
      
    }
  } else {
    phdi$winter[i] = NA
    phdi$spring[i] = NA
    phdi$summer[i] = NA
    phdi$fall[i] = NA
  }
}

  for (i in 1:nrow(phdi)){
    if (phdi$year[i] %in% seq(1972,2011,4) == TRUE) {
      phdi$annual[i] = ((phdi$PHDI_Sep[i-1]*30) + 
                          (phdi$PHDI_Oct[i-1]*31) +
                          (phdi$PHDI_Nov[i-1]*30) + 
                          (phdi$PHDI_Dec[i-1]*31) + 
                          (phdi$PHDI_Jan[i]*31) +
                          (phdi$PHDI_Feb[i]*29) + 
                          (phdi$PHDI_Mar[i]*31) + 
                          (phdi$PHDI_Apr[i]*30) +
                          (phdi$PHDI_May[i]*31) + 
                          (phdi$PHDI_Jun[i]*30) + 
                          (phdi$PHDI_Jul[i]*31) +
                          (phdi$PHDI_Aug[i]*31))/366
      
      
    } else {
      phdi$annual[i] = ((phdi$PHDI_Sep[i-1]*30) + 
                          (phdi$PHDI_Oct[i-1]*31) +
                          (phdi$PHDI_Nov[i-1]*30) + 
                          (phdi$PHDI_Dec[i-1]*31) + 
                          (phdi$PHDI_Jan[i]*31) +
                          (phdi$PHDI_Feb[i]*28) + 
                          (phdi$PHDI_Mar[i]*31) + 
                          (phdi$PHDI_Apr[i]*30) +
                          (phdi$PHDI_May[i]*31) + 
                          (phdi$PHDI_Jun[i]*30) + 
                          (phdi$PHDI_Jul[i]*31) +
                          (phdi$PHDI_Aug[i]*31))/365
    }
  }

#this took freaking forever (hours) to calculate so save it as .csv now for any future needs
setwd("~/Dropbox/Sarah_Work/Manuscripts/2016_climate_waterqual/Data/Annual_monthly_calculated")

write.csv(phdi, "phdi_with_seasonal.csv")

