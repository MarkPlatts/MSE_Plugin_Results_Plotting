# INITIALISATION START ===============================================================================================

# rm(list = ls())

setwd("C:/Users/Mark/Desktop/MSE_Plugin_Results_Plotting")
source("share_tools.R")
library(profvis)


#root results path
# root.plot = "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/North Sea MultiAnnual Plan/ResultsType1-4_220117/Plots/"
# root.results = "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/North Sea MultiAnnual Plan/ResultsType1-4_220117/Results/"

# INITIALISATION END ===============================================================================================


# FUNCTION START ===============================================================================================

CountTimestepsFile = function(igroup, resultfolder, ncols_no_vals){
  file.name = GetFileName_ContainsStrings(FolderPath = paste(root.results,"CatchTrajectories/", sep=""), 
                                          Strings = c("AllFleets", igroup), WithPath=T)
  nTimeStepsInData = countColsWithVals(file.name = file.name, ncols_no_vals = 5, lines.to.skip = 7)
  return(nTimeStepsInData)
}

CreateCatchRatioTables = function(CatchType){
  
  folder.name = paste(CatchType,"Trajectories", sep="")
  
  #get a list of all the groups & fleets
  unique.groups = LoadUniqueGroups(root.results)
  unique.fleets = LoadUniqueFleets(root.results)
  
  #Create a data.table in which to compile all the tables from each file ready for saving to csv
  dt.all = data.table()
  
  #Find out how many years
  nTimeStepsInData = CountTimestepsFile(igroup = unique.groups[1], resultfolder = folder.name, ncols_no_vals = 5)
  
  #get list of files
  catch.files = list.files(path = paste(root.results, folder.name, "/", sep=""), full.names = T)
  
  for(iFile.catch in catch.files){
    
    print(iFile.catch)
    
    #get file in CatchTrajectories folder that are for "AllFleets" and current igroup
    # catches.file = GetFileName_ContainsStrings(FolderPath = paste(root.results, "CatchTrajectories/", sep=""), 
    #                                            Strings = c(iFleet, igroup), WithPath=T)

    catches = fread(iFile.catch, skip=7, header=T)
    
    #Determine file is valid
    if(!isNotAll(dt = catches, col.data.starts = 6, val.to.check=0)) next
    if(!isNotAll(dt = catches, col.data.starts = 6, val.to.check = -9999)) next
    
    #load the files and sum
    catches_last5year = calcLast5Year(catches, "catch.last5yearsum", 5, function.type=2)
    
    #load the catches at the first and last timestep of the forecast
    catch.first.year = GetiYearCatch(catches, iYear=1, ncols.before.timeseries=5)
    
    dt = merge(catch.first.year, catches_last5year, by = names(catch.first.year)[c(3,4)])
    
    #merge the two together so that we can easily calculate the difference between two columns
    dt$Ratios = dt$catch.last5yearsum/dt$Year1
    
    #bind them all together ready to be saved to csv
    dt.all = rbind(dt.all, dt)
    
  }

  catch.ratio.summary.by.strategy = dt.all[,list(Min = min(Ratios), 
                                                 LQ = quantile(Ratios, .25, na.rm=TRUE), 
                                                 Median = median(Ratios),
                                                 UQ = quantile(Ratios, .75, na.rm=TRUE),
                                                 Max = max(Ratios),
                                                 Mean = mean(Ratios)), by=c("StrategyName", "GroupName", "FleetName")]

  #finally save the table to csv
  write.csv(catch.ratio.summary.by.strategy, paste(root.plot, "Tables/",CatchType,"_Ratios.csv", sep=""))
  
}

# FUNCTION END ===============================================================================================



# SCRIPT START  ===============================================================================================
# create_start_end_ratio_catch_tables = function(root.plot){
#   browser()
  CreateCatchRatioTables(CatchType = "Catch")
  CreateCatchRatioTables(CatchType = "Landings")
  CreateCatchRatioTables(CatchType = "Discards")
  CreateCatchRatioTables(CatchType = "Value")
# }

# SCRIPT END  ===============================================================================================