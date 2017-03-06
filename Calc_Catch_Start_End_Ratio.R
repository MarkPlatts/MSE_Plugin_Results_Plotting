# INITIALISATION START ===============================================================================================

rm(list = ls())

source("share_tools.R")

#root results path
root.plot = "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/North Sea MultiAnnual Plan/ResultsType1-4_220117/Plots/"
root.results = "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/North Sea MultiAnnual Plan/ResultsType1-4_220117/Results/"

# INITIALISATION END ===============================================================================================


# FUNCTION START ===============================================================================================

CountTimestepsFile = function(igroup, resultfolder, ncols_no_vals){
  file.name = GetFileName_ContainsStrings(FolderPath = paste(root.results,"CatchTrajectories/", sep=""), 
                                          Strings = c("AllFleets", igroup), WithPath=T)
  nTimeStepsInData = countColsWithVals(file.name = file.name, ncols_no_vals = 5, lines.to.skip = 7)
  return(nTimeStepsInData)
}

# FUNCTION END ===============================================================================================



# SCRIPT START  ===============================================================================================
if(TRUE){
  
  #get a list of all the groups
  unique.groups = LoadUniqueGroups(root.results)
  
  #Create a data.table in which to compile all the tables from each file ready for saving to csv
  dt.all = data.table()
  
  #Find out how many years
  nTimeStepsInData = CountTimestepsFile(igroup = unique.groups[1], resultfolder = "CatchTrajectories", ncols_no_vals = 5)

  for(igroup in unique.groups)
  {
    print(igroup)

    #get file in CatchTrajectories folder that are for "AllFleets" and current igroup
    catches.file = GetFileName_ContainsStrings(FolderPath = paste(root.results, "/CatchTrajectories/", sep=""), 
                                               Strings = c("AllFleets", igroup), WithPath=T)
    
    catches = fread(catches.file, skip=7, header=T)

    #Determine file is valid
    #if(!isNotAll(dt = catches, col.data.starts = 6, val.to.check=-9999)) next
    if(!isNotAll(dt = catches, col.data.starts = 6, val.to.check=0)) next
#if (igroup=="Spurdog") browser()
    #load the catches at the first and last timestep of the forecast
    catch.first.year = GetiYearCatch(catches, iYear=1, ncols.before.timeseries=5)
    catch.last.year = GetiYearCatch(catches, iYear=nTimeStepsInData, ncols.before.timeseries=5)
    dt = merge(catch.first.year, catch.last.year, by = names(catch.first.year)[1:5])

    #merge the two together so that we can easily calculate the difference between two columns
    dt$Ratios = dt$Year20/dt$Year1
    
    #bind them all together ready to be saved to csv
    dt.all = rbind(dt.all, dt)
    
  }
  
  catch.ratio.summary.by.strategy = dt.all[,list(Min = min(Ratios), 
                                             LQ = quantile(Ratios, .25, na.rm=TRUE), 
                                             Median = median(Ratios),
                                             UQ = quantile(Ratios, .75, na.rm=TRUE),
                                             Max = max(Ratios),
                                             Mean = mean(Ratios)), by=c("StrategyName", "GroupName")]

  #finally save the table to csv
  write.csv(catch.ratio.summary.by.strategy, paste(root.plot, "Tables/Catch_Ratios.csv", sep=""))
  
}


# SCRIPT END  ===============================================================================================