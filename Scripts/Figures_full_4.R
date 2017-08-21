#8/21/2017
#all figures for all regions, statistics, and variables. 

######################################################################################################################
######################################################################################################################
# 
# library(ggplot2)
# library(dplyr)
# library(tidyr)
# 
# 
# setwd("~/R/LandHindcastPaper")
# fileLocation <- getwd()


figurePath <- paste0(fileLocation,"/Outputs/FullFigures")


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

  
  # Plot All regions for each crop
  data1 %>%
    group_by(crop) %>%
    mutate(y_max = max(value)) %>%
    ungroup -> 
    dat
  
  p1 <- ggplot(dat, aes(x = region, y = value, color = scenario, group = scenario)) + 
        facet_wrap(~ crop, nrow = 3) +
        coord_polar() + 
        geom_polygon(fill=NA) + 
        geom_rect(aes(xmin=0,ymin=1,xmax=4.6*pi,ymax=1), fill=NA, color="black", linetype="dotted", size=0.25) + #ref circle for bias=0
        theme_bw() +
        theme(axis.title.x=element_blank(), axis.title.y=element_blank() ) +
        scale_y_continuous(limits=c(0,ymax)) +
        ggtitle("Normalized RMSE, all regions for each crop")
  ggsave(paste0(figurePath,"/RegionalNRMS_eachCrop.pdf"),p1, height=22, width=22, units="in")  
  
  
  # Plot All crops for each Region
  data1  -> dat
    
  p1 <- ggplot(dat,aes(x = crop, y = value, color = scenario, group = scenario)) +
        facet_wrap(~ region, nrow = 3) + 
        geom_polygon(fill=NA)+coord_polar() +
        geom_rect(aes(xmin=0,ymin=1,xmax=3*pi,ymax=1), fill=NA, color="black", linetype="dotted", size=0.25) + #ref circle for bias=0
        theme_bw() +
        theme(axis.title.x=element_blank(), axis.title.y=element_blank() ) +
        ggtitle("Normalized RMSE, all crops for each region") 
  ggsave(paste0(figurePath,"/NRMS_eachRegion.pdf"),p1, height= 22, width= 34, units="in")  
    
  # somewhat zoomed in
  p1 <- ggplot(dat,aes(x = crop, y = value, color = scenario, group = scenario)) +
    facet_wrap(~ region, nrow = 3) + 
    geom_polygon(fill=NA)+coord_polar() +
    geom_rect(aes(xmin=0,ymin=1,xmax=3*pi,ymax=1), fill=NA, color="black", linetype="dotted", size=0.25) + #ref circle for bias=0
    theme_bw() +
    theme(axis.title.x=element_blank(), axis.title.y=element_blank() ) +
    scale_y_continuous(limits=c(0, 5)) +
    ggtitle("Normalized RMSE, all crops for each region") 
  ggsave(paste0(figurePath,"/NRMS_eachRegion_zoomed.pdf"),p1, height= 22, width=34, units="in")  
    
    
######################################################################################################################
######################################################################################################################
    
# centered Normalized RMSE  -  all crops for USA
    
    fore <-  read.csv(file=paste0(fileLocation,"/Outputs/GCAM_Hist_Forecast/cNRMSE_GCAM_Hist_Forecast.csv"),
                      head = TRUE, sep = ",") %>%
      mutate(scenario = "FY") %>%
      rename(region = X)
    
    
    foreBio <-  read.csv(file=paste0(fileLocation,"/Outputs/GCAM_Hist_Forecast_Bio/cNRMSE_GCAM_Hist_Forecast_Bio.csv"),
                         head = TRUE, sep = ",") %>%
      mutate(scenario = "FYB") %>%
      rename(region = X)
    
    
    history <-  read.csv(file=paste0(fileLocation,"/Outputs/GCAM_History/cNRMSE_GCAM_History.csv"),
                         head = TRUE, sep = ",") %>%
      mutate(scenario = "AY") %>%
      rename(region = X)
    
    
    historyBio <-  read.csv(file=paste0(fileLocation,"/Outputs/GCAM_History_Bio/cNRMSE_GCAM_History_Bio.csv"),
                            head = TRUE, sep = ",") %>%
      mutate(scenario = "AYB") %>%
      rename(region = X)
    
    
    #Combine scenarios, transition from wide to long format:
    bind_rows(fore,
              foreBio,
              history, 
              historyBio) %>%
      gather(crop, value, -region, -scenario) %>%
      replace_na(list(value = 0)) -> 
      data1
      
    
    # Plot All crops for each Region
    data1  -> dat
      
    p1 <- ggplot(dat,aes(x = crop, y = value, color = scenario, group = scenario)) +
          facet_wrap(~ region, nrow = 3) + 
          geom_polygon(fill=NA)+coord_polar() +
          geom_rect(aes(xmin=0,ymin=1,xmax=3*pi,ymax=1), fill=NA, color="black", linetype="dotted", size=0.25) + #ref circle for bias=0
          theme_bw() +
          theme(axis.title.x=element_blank(), axis.title.y=element_blank() ) +
          ggtitle("Normalized centered RMSE, all crops for each region") 
      ggsave(paste0(figurePath,"/cNRMS_eachRegion.pdf"),p1, height=22, width=34, units="in")
      
      
######################################################################################################################
######################################################################################################################
      
# revised Normalized RMSE -  all crops for USA  
    fore <-  read.csv(file=paste0(fileLocation,"/Outputs/GCAM_Hist_Forecast/NRMSErevised_GCAM_Hist_Forecast.csv"), 
                        head = TRUE, sep = ",")  %>%
      mutate(scenario = "FY") %>%
      rename(region = X)
    
      
    foreBio <-  read.csv(file=paste0(fileLocation,"/Outputs/GCAM_Hist_Forecast_Bio/NRMSErevised_GCAM_Hist_Forecast_Bio.csv"), 
                           head = TRUE, sep = ",")  %>%
      mutate(scenario = "FYB") %>%
      rename(region = X)
      
      
    history <-  read.csv(file=paste0(fileLocation,"/Outputs/GCAM_History/NRMSErevised_GCAM_History.csv"), 
                           head = TRUE, sep = ",")  %>%
      mutate(scenario = "AY") %>%
      rename(region = X)
      
      
    historyBio <-  read.csv(file=paste0(fileLocation,"/Outputs/GCAM_History_Bio/NRMSErevised_GCAM_History_Bio.csv"), 
                              head = TRUE, sep = ",")  %>%
      mutate(scenario = "AYB") %>%
      rename(region = X)
      
      
    #Combine scenarios, transition from wide to long format:
    bind_rows(fore,
              foreBio,
              history, 
              historyBio) %>%
      gather(crop, value, -region, -scenario) %>%
      replace_na(list(value = 0)) -> 
      data1
    
    
    
    # Plot All crops for each Region
    data1  -> dat
    
    p1 <- ggplot(dat,aes(x = crop, y = value, color = scenario, group = scenario)) +
      facet_wrap(~ region, nrow = 3) + 
      geom_polygon(fill=NA)+coord_polar() +
      geom_rect(aes(xmin=0,ymin=1,xmax=3*pi,ymax=1), fill=NA, color="black", linetype="dotted", size=0.25) + #ref circle for bias=0
      theme_bw() +
      theme(axis.title.x=element_blank(), axis.title.y=element_blank() ) +
      ggtitle("Revised normalized RMSE, all crops in each region") 
        
    ggsave(paste0(figurePath,"/NRMSrevised_eachRegion.pdf"),p1, height= 22, width= 34, units="in")  
      
      
######################################################################################################################
######################################################################################################################
      
# Time series - all regions for each crop

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
    faoData %>%
      gather(Year, fao, -region, -crop) %>%
      filter(crop %in% crops) %>%
      mutate(crop = as.character(crop),
             Year = as.integer(substr(Year, 2,5)),
             fao = 0.00001 * fao) -> 
      faoData
    
    
    # plot all regions for each crop.   
    for (i in 1:length(crops)){

      Crop <- crops[i]
      
      #GCAM data for crop and regions of interest  
      CropData <- subset(data, data$crop == Crop)
      
      #FAO data for crop and regions of interest    
      CropFAO <- subset(faoData, faoData$crop == Crop)
        
      #plot  
      
      pCrop <- ggplot(data=CropData, aes(x=Year, y=gcam, color=scenario)) + geom_line()
      pCrop <- pCrop + facet_wrap(~region, nrow=3, scale = "free_y") + expand_limits(y=0)
      pCrop <- pCrop + geom_line(data=CropFAO, aes(x=Year, y=fao), color="black") 
      pCrop <- pCrop + ggtitle(paste("Time series,",unique(CropData$crop)))
      pCrop <- pCrop + ylab(bquote('land allocation, thousand ' ~km^2))
      ggsave(paste0(figurePath,"/timeSeries", unique(CropData$crop),".pdf"),pCrop, height=22, width=35, units="in") 
      
    }
    