



setwd("~/R/LandHindcastPaper")

fileLocation <- getwd()

library(fANCOVA)
library(dplyr)
library(ggplot2)
library(reshape2)



### update the land allocation data frame with detrended FAO data
source(paste0(fileLocation,"/Scripts/DetrendData.R"))

### Use the updated data from produced above to calculate the measures comparing GCAM and FAO data
source(paste0(fileLocation,"/Scripts/CoreStats.R"))

### make figures
source(paste0(fileLocation,"/Scripts/Figures.R"))