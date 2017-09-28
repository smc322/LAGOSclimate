# LAGOSclimate
#SMC 2017 LAGOS climate water quality paper

#includes summary of climate data, limnology data and an input data file with both limno and climate for the multi-task learning analysis.
#this work was done prior to the release of LAGOS-NE 1.087.1 and the LAGOS R package in October 2017.  Limnology data for future work should be sourced and summarized from there.
#PRSIM climate data are not included in the version of LAGOS-NE that was released in October 2017, so Rdata files are included here for huc12 zones, including mean, min, max temperature at the annual scale for the 17 state area, mean seasonal temps, seasonal palmer index (PHDI) data by lagoslakeid, and ENSO/NAO index data by year (ENSO and NAO are not spatially specific)

#data folder also includes output from Shuai's April MTL results (for lambda 3 = .03 but we tried many values of lambda 3). These can be used for clustering, making heat maps, etc. There are mtricies of regression coefficients for each response variable (TP, TN, Secchi, chla) that have columns for each of the 48 predictors and rows for each of the ~12,000 lakes.

