#3/2/17
# Hindcast metrics  
# first, scenario, crop, and region are fixed, and measures in each region are taken.
# then a global statistic is taken for each crop in each scenario.

# Must have folders with names identical to each gcam scenario tag set up, or files won't write



##############################################################################################################
# 
# setwd("~/R/LandHindcast")
# 
# fileLocation <- getwd()



#read in full data set with detrended fao information
data <- read.csv(file="Data/HindcastDataCrops_detrended.csv", head = TRUE, sep = ",")
data[is.na(data)] <- 0 #replace NAs in csv with 0 so that region is still included in averaging.


#useful vectors for looping 
regions <- unique(data$region) #list of regions
crops <- unique(data$crop) # list of crops
scenarios <- unique(data$scenario) #list of scenarios



##############################################################################################################

#allocate and label matrices for storing basic regional quantities

  #placeholder vector to store variance of observations for each region. Rewritten for each crop:
  obs.var <- matrix(0,length(regions),length(crops))
  rownames(obs.var) <- regions
  colnames(obs.var) <- crops
  
  obs.mean <- matrix(0,length(regions),length(crops))
  rownames(obs.mean) <- regions
  colnames(obs.mean) <- crops
  
  sim.var <- matrix(0,length(regions),length(crops))
  rownames(sim.var) <- regions
  colnames(sim.var) <- crops
  
  sim.mean <- matrix(0,length(regions),length(crops))
  rownames(sim.mean) <- regions
  colnames(sim.mean) <- crops
  
  
  obs.var.detrend <- matrix(0,length(regions),length(crops))
  rownames(obs.var.detrend) <- regions
  colnames(obs.var.detrend) <- crops
  
  obs.mean.detrend <- matrix(0,length(regions),length(crops))
  rownames(obs.mean.detrend) <- regions
  colnames(obs.mean.detrend) <- crops

##############################################################################################################

#allocate matrices to store Region-specific temporal statistics:

  #matrix to store regional RMSE
  RMS <- matrix(0, length(regions), length(crops))
  rownames(RMS) <- regions
  colnames(RMS) <- crops
      
  #matrix to hold centered RMSE
  cRMS <- matrix(0, length(regions), length(crops))
  rownames(cRMS) <- regions
  colnames(cRMS) <- crops
      
  #matrix to store regional bias 
  bias <- matrix(0, length(regions), length(crops))
  rownames(bias) <- regions
  colnames(bias) <- crops
      

  #matrix to store regional normalized RMSE, comparing to std dev of FAO data
  NRMS <- matrix(0, length(regions), length(crops))
  rownames(NRMS) <- regions
  colnames(NRMS) <- crops
      
      
  #matrix to store regional centered Normalized RMSE, comparing to std dev of FAO data
  cNRMS <- matrix(0, length(regions), length(crops))
  rownames(cNRMS) <- regions
  colnames(cNRMS) <- crops
      
  
  #revised normalized RMSE, comparing to std dev of FAO data about trend line
  NRMS.detrend <- matrix(0, length(regions), length(crops))
  rownames(NRMS.detrend) <- regions
  colnames(NRMS.detrend) <- crops
      
    

  #matrix to store global measures:
  globalstats <- matrix(0, 2, length(crops))
  rownames(globalstats) <- c( "bias", "absbias")
  colnames(globalstats) <- crops




##############################################################################################################
### Compute measures


for(scen in 1:length(scenarios)){ # for each scenario
  
  ### subset to a single scenario
  set <- subset(data, data$scenario == scenarios[scen]) 
  
 
  ### output results to the scenario specific folder
  scenarioFolderPath <- paste0(fileLocation,"/Outputs/",scenarios[scen])
  
  for(i in 1:length(crops)){ # for each crop
    
    #subset to a single crop
    set.crop <- subset(set, set$crop == crops[i]) 
    
    
    for(j in 1:length(regions)){ # for each region
      
      #subset to a single region
      set.region <- subset(set.crop, set.crop$region == regions[j]) 
      
      
      #######################################################################################################
      ### calculate some statistics on the simulation and observation from the data set, for i crop in j region

        obs.var[j,i] <- var(set.region$fao) # temporal variance of observation in region j
        obs.mean[j,i] <- mean(set.region$fao) # temporal mean of observation in region j
        sim.mean[j,i] <- mean(set.region$gcam) # temporal mean of simulation in region j
        sim.var[j,i] <- var(set.region$gcam) # temporal variance of simulation in region j

        obs.var.detrend[j,i] <- var(set.region$fao.detrend) # variance about the observed trend
        obs.mean.detrend[j,i] <- mean(set.region$fao.detrend) # should be 0

      #######################################################################################################      
      
 
        #RMSE:
        RMS[j,i] <- sqrt(mean((set.region$gcam - set.region$fao)^2)) #thous km2
        
        #Bias:
        bias[j,i] <- (sim.mean[j,i] - obs.mean[j,i]) #thous km2 
        
        #centered RMSE:
        cRMS[j,i] <- sqrt(mean(( (set.region$gcam-sim.mean[j,i]) - ( set.region$fao - obs.mean[j,i] ) )^2)) # thous km2
          
        #Normalized RMSE:
        NRMS[j,i] <- sqrt(mean((set.region$gcam-set.region$fao)^2 / obs.var[j,i])) #dimensionless
        
        #centered Normalized RMSE:
        cNRMS[j,i] <- sqrt(mean(( (set.region$gcam-sim.mean[j,i]) - ( set.region$fao - obs.mean[j,i] ) )^2) / obs.var[j,i]) #dimensionless   


        #Revised normalized RMSE - comparing to variance of FAO data about trend line:
        NRMS.detrend[j,i] <- sqrt(mean((set.region$gcam-set.region$fao)^2 / obs.var.detrend[j,i])) #dimensionless


    } # end region loop
    
    
    #######################################################################################################        
    #######################################################################################################        
    
#print out the tables of temporal statistics. Each entry is the temporal statistic calculated for a specific crop in a specific region:
    
    #core error stats:
    write.csv(RMS, file = paste0(scenarioFolderPath,"/RMSE_", scenarios[scen],".csv"))
    write.csv(bias, file = paste0(scenarioFolderPath,"/Bias_", scenarios[scen],".csv"))
    write.csv(cRMS, file = paste0(scenarioFolderPath,"/cRMSE_", scenarios[scen],".csv"))
    write.csv(NRMS, file = paste0(scenarioFolderPath,"/NRMSE_", scenarios[scen],".csv"))
    write.csv(cNRMS, file = paste0(scenarioFolderPath,"/cNRMSE_", scenarios[scen],".csv"))
    write.csv(NRMS.detrend, file = paste0(scenarioFolderPath,"/NRMSErevised_", scenarios[scen],".csv"))

    write.csv(obs.var, file = paste0(scenarioFolderPath,"/FAOvariance_", scenarios[scen],".csv"))
    write.csv(obs.mean, file = paste0(scenarioFolderPath,"/FAOmean_", scenarios[scen],".csv"))
    write.csv(obs.var.detrend, file = paste0(scenarioFolderPath,"/FAOdetrended_variance_", scenarios[scen],".csv"))
    write.csv(obs.mean.detrend, file = paste0(scenarioFolderPath,"/FAOdetrended_mean_", scenarios[scen],".csv"))
    
    #######################################################################################################    
    
    #global trend

    globalstats[1,i] <- mean(set.crop$gcam) - mean(set.crop$fao)
    globalstats[2,i] <- mean(abs(bias[,i]))
    

    
    

  } #end crops loop

  #export table of regional aggregate metrics and wt to unif ratios:
  write.csv(globalstats, file = paste0(scenarioFolderPath,"/GlobalStats_", scenarios[scen],".csv"))

  ##########################################################################################################
  

  
} # end Scenario Loop




