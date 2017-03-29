# 2/1/17 
# generate de-trended FAO data with auto span parameters


# setwd("~/R/LandHindcastPaper")
# 
# fileLocation <- getwd()
# 
# library(fANCOVA)
# library(dplyr)

##############################################################################################################

### read in GCAM and FAO land allocation data set
data <- read.csv(file="Data/HindcastDataCrops.csv", head = TRUE, sep = ",")
data[is.na(data)] <- 0 #replace NAs in csv with 0 so that region is still included in averaging.
data <- data[,1:6]

### Some useful vectors
scenarios <- unique(data$scenario)
regions <- unique(data$region)
crops <- unique(data$crop)


### pull off just the fao data:
dataFAO <- data[,colnames(data) %in% c("region", "crop","Year", "fao")]
### there are four copies of each row for each of the 4 GCAM scenarios, eliminate:
dataFAO <- unique(dataFAO)
### add column to hold detrended data:
dataFAO$fao.detrend <-0

for (i in 1:length(crops)){ # for each crop
  for (j in 1:length(regions)){ #for each region


    ### subset to crop-region
    data1 <- subset(dataFAO, dataFAO$region==regions[j] & dataFAO$crop==crops[i])

    ### fit using aicc for span
    fit.fao <- loess.as(data1$Year, data1$fao,degree = 2, criterion = c("aicc", "gcv")[1], user.span = NULL)

    ### save the detrended data
    dataFAO[dataFAO$region==regions[j] & dataFAO$crop==crops[i],]$fao.detrend <- resid(fit.fao)

  } # end regions loop
} # end crop loop


### add a copy back to the main dataframe for each gcam scenario
dataNew <- left_join(data, dataFAO, by=c("region", "crop", "Year", "fao"))



write.csv(dataNew, paste0(fileLocation,"/Data/HindcastDataCrops_detrended.csv"))