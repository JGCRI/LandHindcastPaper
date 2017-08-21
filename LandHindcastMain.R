



setwd("~/R/LandHindcastPaper")

fileLocation <- getwd()

library(fANCOVA)
library(dplyr)
library(ggplot2)
library(reshape2)



### update the land allocation data frame with detrended FAO data
source(paste0(fileLocation,"/Scripts/DetrendData_1.R"))

### Use the updated data from produced above to calculate the measures comparing GCAM and FAO data
source(paste0(fileLocation,"/Scripts/CoreStats_2.R"))

### make figures that appear in the paper
source(paste0(fileLocation,"/Scripts/Figures_paper_3.R"))

### make all figures#source(paste0(fileLocation,"/Scripts/Figures_full_4.R"))
