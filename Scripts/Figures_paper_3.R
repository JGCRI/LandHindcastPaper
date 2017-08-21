#3/23/2017
#figures for paper only

######################################################################################################################
######################################################################################################################
# library(ggplot2)
# library(dplyr)
# library(tidyr)
# 
# 
# 
# setwd("~/R/LandHindcastPaper")
# fileLocation <- getwd()


figurePath <- paste0(fileLocation,"/Outputs/PaperFigures")


regions <- c( "Africa", "Australia_NZ", "Canada", "China", "Eastern Europe", "Former Soviet Union", 
              "India", "Japan", "Korea", "Latin America", "Middle East", "Southeast Asia", "USA", "Western Europe")

######################################################################################################################
######################################################################################################################

# Global Bias and Absolute Bias

# load table of global statistics for each scenario
fore <-  read.csv(file=paste0(fileLocation,"/Outputs/GCAM_Hist_Forecast/GlobalStats_GCAM_Hist_Forecast.csv"), 
                  head = TRUE, sep = ",") %>%
  mutate(scenario = "FY") %>%
  rename(stat = X)


foreBio <-  read.csv(file=paste0(fileLocation,"/Outputs/GCAM_Hist_Forecast_Bio/GlobalStats_GCAM_Hist_Forecast_Bio.csv"), 
                     head = TRUE, sep = ",") %>%
  mutate(scenario = "FYB") %>%
  rename(stat = X)


history <-  read.csv(file=paste0(fileLocation,"/Outputs/GCAM_History/GlobalStats_GCAM_History.csv"), 
                     head = TRUE, sep = ",") %>%
  mutate(scenario = "AY") %>%
  rename(stat = X)


historyBio <-  read.csv(file=paste0(fileLocation,"/Outputs/GCAM_History_Bio/GlobalStats_GCAM_History_Bio.csv"), 
                        head = TRUE, sep = ",") %>%
  mutate(scenario = "AYB") %>%
  rename(stat = X)


# Combine scenarios, transition from wide to long format:
bind_rows(fore,
          foreBio,
          history, 
          historyBio) %>%
  gather(crop, value, -stat, -scenario) %>%
  replace_na(list(value = 0)) -> 
  data1


# plot  
data1 %>%
  filter(stat == "bias") ->
  unifBias 
p1 <- ggplot(unifBias,aes(x=crop,y=value,color=scenario,group=scenario))+geom_polygon(fill=NA)+coord_polar() +
  geom_rect(aes(xmin=0,ymin=0,xmax=3*pi,ymax=0), fill=NA, color="black", linetype="dotted", size=0.25) + #ref circle for bias=0
  theme_bw()+ theme(axis.title.x=element_blank()) + ylab(bquote('thousand ' ~km^2)) + scale_y_continuous(limits=c(-30,30)) +
  ggtitle("Global Bias") 
ggsave(paste0(figurePath,"/GlobalBias.pdf"),p1, height=7, width=8, units="in")


data1 %>%
  filter(stat == "absbias") ->
  unifAbsBias
p3 <- ggplot(unifAbsBias,aes(x=crop,y=value,color=scenario,group=scenario))+geom_polygon(fill=NA)+coord_polar() +
  geom_rect(aes(xmin=0,ymin=0,xmax=3*pi,ymax=0), fill=NA, color="black", linetype="dotted", size=0.25) + #ref circle for bias=0
  theme_bw()+ theme(axis.title.x=element_blank() ) + ylab(bquote('thousand ' ~km^2))+scale_y_continuous(limits=c(0,38)) +
  ggtitle("Global Absolute Bias")
ggsave(paste0(figurePath,"/GlobalAbsBias.pdf"),p3, height=7, width=8, units="in")


######################################################################################################################
######################################################################################################################
  
# Normalized RMSE - regional for wheat and all crops for USA

fore <-  read.csv(file=paste0(fileLocation,"/Outputs/GCAM_Hist_Forecast/NRMSE_GCAM_Hist_Forecast.csv"), 
                  head = TRUE, sep = ",") %>%
  mutate(scenario = "FY") %>%
  rename(region = X)


foreBio <-  read.csv(file=paste0(fileLocation,"/Outputs/GCAM_Hist_Forecast_Bio/NRMSE_GCAM_Hist_Forecast_Bio.csv"), 
                     head = TRUE, sep = ",") %>%
  mutate(scenario = "FYB") %>%
  rename(region = X)


history <-  read.csv(file=paste0(fileLocation,"/Outputs/GCAM_History/NRMSE_GCAM_History.csv"), 
                     head = TRUE, sep = ",") %>%
  mutate(scenario = "AY") %>%
  rename(region = X)


historyBio <- read.csv(file=paste0(fileLocation,"/Outputs/GCAM_History_Bio/NRMSE_GCAM_History_Bio.csv"), 
                       head = TRUE, sep = ",") %>%
  mutate(scenario = "AYB") %>%
  rename(region = X)


# Combine scenarios, transition from wide to long format:
bind_rows(fore,
          foreBio,
          history, 
          historyBio) %>%
  gather(crop, value, -region, -scenario) %>%
  replace_na(list(value = 0)) -> 
  data1


# Plot All regions for wheat
data1 %>%
  filter(crop == "Wheat") ->
  dat

crp <- unique(dat$crop)
ymax <- max(dat$value)

p1 <- ggplot(dat,aes(x=region,y=value,color=scenario,group=scenario))+coord_polar()+geom_polygon(fill=NA)+ 
  geom_rect(aes(xmin=0,ymin=1,xmax=4.6*pi,ymax=1), fill=NA, color="black", linetype="dotted", size=0.25) + #ref circle for bias=0
  theme_bw()+ theme(axis.title.x=element_blank(), axis.title.y=element_blank() )+scale_y_continuous(limits=c(0,ymax)) +
  ggtitle(paste("Normalized RMSE,", crp)) 
ggsave(paste0(figurePath,"/RegionalNRMS_", crp,".pdf"),p1, height=7, width=8, units="in")  



# Plot All crops for USA
data1 %>%
  filter(region == "USA") ->
  dat

rgn <- unique(dat$region)
ymax <- max(dat$value)

p1 <- ggplot(dat,aes(x=crop,y=value,color=scenario,group=scenario))+geom_polygon(fill=NA)+coord_polar() +
  geom_rect(aes(xmin=0,ymin=1,xmax=3*pi,ymax=1), fill=NA, color="black", linetype="dotted", size=0.25) + #ref circle for bias=0
  theme_bw()+ theme(axis.title.x=element_blank(), axis.title.y=element_blank() )+scale_y_continuous(limits=c(0,ymax)) +
  ggtitle(paste("Normalized RMSE,", rgn)) 
ggsave(paste0(figurePath,"/NRMS_", rgn,".pdf"),p1, height=7, width=8, units="in")  

    
######################################################################################################################
######################################################################################################################
    
# centered Normalized RMSE  -  all crops for USA
    
    fore <-  read.csv(file=paste0(fileLocation,"/Outputs/GCAM_Hist_Forecast/cNRMSE_GCAM_Hist_Forecast.csv"),
                      head = TRUE, sep = ",")
    fore$scenario <- "FY"
    fore$region <- regions
    fore <- fore[,2:ncol(fore)]
    
    
    
    foreBio <-  read.csv(file=paste0(fileLocation,"/Outputs/GCAM_Hist_Forecast_Bio/cNRMSE_GCAM_Hist_Forecast_Bio.csv"),
                         head = TRUE, sep = ",")
    foreBio$scenario <- "FYB"
    foreBio$region <- regions
    foreBio <- foreBio[,2:ncol(foreBio)]
    
    
    
    history <-  read.csv(file=paste0(fileLocation,"/Outputs/GCAM_History/cNRMSE_GCAM_History.csv"),
                         head = TRUE, sep = ",")
    history$scenario <- "AY"
    history$region <- regions
    history <- history[,2:ncol(history)]
    
    
    
    historyBio <-  read.csv(file=paste0(fileLocation,"/Outputs/GCAM_History_Bio/cNRMSE_GCAM_History_Bio.csv"),
                            head = TRUE, sep = ",")
    historyBio$scenario <- "AYB"
    historyBio$region <- regions
    historyBio <- historyBio[,2:ncol(historyBio)]
    
    
    #Combine scenarios, transition from wide to long format:
    data <- rbind(fore, foreBio, history,historyBio) #wide format
    data[is.na(data)] <- 0 #replace NAs in csv with 0
    data1 <- melt(data, id.vars=c("scenario","region"), variable.name="crop") #long format
    ind <- which(colnames(data1) == "variable")
    colnames(data1)[ind] <- "crop"

      
    #all crops in USA
      rgn <- "USA"
      dat <- subset(data1, data1$region==rgn)
      ymax <- max(dat$value)
      
      p1 <- ggplot(dat,aes(x=crop,y=value,color=scenario,group=scenario))+geom_polygon(fill=NA)+coord_polar()
      p1 <- p1 + geom_rect(aes(xmin=0,ymin=1,xmax=3*pi,ymax=1), fill=NA, color="black", linetype="dotted", size=0.25) #ref circle for bias=0
      p1 <- p1 + theme_bw()+ theme(axis.title.x=element_blank(), axis.title.y=element_blank() )+scale_y_continuous(limits=c(0,ymax))
      p1 <- p1 + ggtitle(paste("Normalized centered RMSE,", rgn)) 
      
      ggsave(paste0(figurePath,"/cNRMS_", rgn,".pdf"),p1, height=7, width=8, units="in")
      
      
######################################################################################################################
######################################################################################################################
      
# revised Normalized RMSE -  all crops for USA  
    fore <-  read.csv(file=paste0(fileLocation,"/Outputs/GCAM_Hist_Forecast/NRMSErevised_GCAM_Hist_Forecast.csv"), 
                        head = TRUE, sep = ",")
    fore$scenario <- "FY"
    fore$region <- regions
    fore <- fore[,2:ncol(fore)]
      
      
      
    foreBio <-  read.csv(file=paste0(fileLocation,"/Outputs/GCAM_Hist_Forecast_Bio/NRMSErevised_GCAM_Hist_Forecast_Bio.csv"), 
                           head = TRUE, sep = ",")
    foreBio$scenario <- "FYB"
    foreBio$region <- regions
    foreBio <- foreBio[,2:ncol(foreBio)]
      
      
      
    history <-  read.csv(file=paste0(fileLocation,"/Outputs/GCAM_History/NRMSErevised_GCAM_History.csv"), 
                           head = TRUE, sep = ",")
    history$scenario <- "AY"
    history$region <- regions
    history <- history[,2:ncol(history)]
      
      
      
    historyBio <-  read.csv(file=paste0(fileLocation,"/Outputs/GCAM_History_Bio/NRMSErevised_GCAM_History_Bio.csv"), 
                              head = TRUE, sep = ",")
    historyBio$scenario <- "AYB"
    historyBio$region <- regions
    historyBio <- historyBio[,2:ncol(historyBio)]
      
      
    #Combine scenarios, transition from wide to long format:
    data <- rbind(fore, foreBio, history,historyBio) #wide format
    data[is.na(data)] <- 0 #replace NAs in csv with 0
    data1 <- melt(data, id.vars=c("scenario","region"), variable.name="crop") #long format
    ind <- which(colnames(data1) == "variable")
    colnames(data1)[ind] <- "crop"
      
    #all crops in USA
      rgn <- "USA"
      dat <- subset(data1, data1$region==rgn)
      ymax <- max(dat$value)
        
      p1 <- ggplot(dat,aes(x=crop,y=value,color=scenario,group=scenario))+geom_polygon(fill=NA)+coord_polar()
      p1 <- p1 + geom_rect(aes(xmin=0,ymin=1,xmax=3*pi,ymax=1), fill=NA, color="black", linetype="dotted", size=0.25) #ref circle for bias=0
      p1 <- p1 + theme_bw()+ theme(axis.title.x=element_blank(), axis.title.y=element_blank() )+scale_y_continuous(limits=c(0,ymax))
      p1 <- p1 + ggtitle(paste("Revised normalized RMSE,", rgn)) 
        
      ggsave(paste0(figurePath,"/NRMSrevised_", rgn,".pdf"),p1, height=7, width=8, units="in")  
      
      
######################################################################################################################
######################################################################################################################
      
# Time series - wheat and corn, selected regions      

    #read in full GCAM data set
    data <- read.csv(file="Data/HindcastDataCrops_detrended.csv", head = TRUE, sep = ",")
    data <- data[2:ncol(data)]
      
    #relabel scenarios with paper abbreviations  
    data$scenario <- as.character(data$scenario)
    data[data$scenario=="GCAM_Hist_Forecast",]$scenario <- "FY"
    data[data$scenario=="GCAM_Hist_Forecast_Bio",]$scenario <- "FYB"
    data[data$scenario=="GCAM_History",]$scenario <- "AY"
    data[data$scenario=="GCAM_History_Bio",]$scenario <- "AYB"
      
      
    #useful vectors for looping 
    regions <- unique(data$region) #list of regions
    crops <- unique(data$crop) # list of crops
    scenarios <- unique(data$scenario) #list of scenarios
      
      
    #get FAO data  
    faoData <- read.csv(file="Data/fao_harvested_area.csv", head = TRUE, sep = ",")
    
    
    #convert to long data
    faoData <- melt(faoData, id.vars = c( "region", "crop"))  
    ind <- which(colnames(faoData) == "value")
    colnames(faoData)[ind] <- "fao"
    ind <- which(colnames(faoData) == "variable")
    colnames(faoData)[ind] <- "Year"
    
    #drop the X from the year labels and just treat years as numbers
    df2 <- mutate(faoData, Year = as.character(Year))
    df2<-mutate(df2,Year=sapply(strsplit(df2$Year, split='X', fixed = TRUE),function(x) (x[2]))) #use exact string matching
    df2 <- mutate(df2, Year = as.numeric(Year))
    faoData <- df2
    #adjust units to thousand km2
    faoData$fao <- 0.00001*faoData$fao
      

      
      
    crops <- c("Wheat","Corn")
    for (i in 1:length(crops)){

      Crop <- crops[i]
      
      #GCAM data for crop and regions of interest  
      CropData <- subset(data, data$crop == Crop & (data$region=="USA" | data$region=="Former Soviet Union" | data$region=="Africa" 
                                                      | data$region=="China" ))
      
      #FAO data for crop and regions of interest    
      CropFAO <- subset(faoData, faoData$crop == Crop & (faoData$region=="USA" | faoData$region=="Former Soviet Union" | faoData$region=="Africa"
                                                           | faoData$region=="China"))
        
      #plot  
      
      pCrop <- ggplot(data=CropData, aes(x=Year, y=gcam, color=scenario)) + geom_line()
      pCrop <- pCrop + facet_wrap(~region, nrow=5, scale = "free_y") + expand_limits(y=0)
      pCrop <- pCrop + geom_line(data=CropFAO, aes(x=Year, y=fao), color="black") 
      pCrop <- pCrop + ggtitle(paste("Time series,",unique(CropData$crop)))
      pCrop <- pCrop + ylab(bquote('land allocation, thousand ' ~km^2))
      ggsave(paste0(figurePath,"/timeSeries", unique(CropData$crop),".pdf"),pCrop, height=12, width=9, units="in") 
      
    }
    