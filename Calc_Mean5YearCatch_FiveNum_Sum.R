# FUNCTION START ===============================================================================================

CreateCatchTables = function(CatchType, root.results, root.plot){
  #Tested by hand - correct MP 14/8/17
  
  folder.name = paste(CatchType,"Trajectories", sep="")
  
  #get a list of all the groups & fleets
  unique.groups = LoadUniqueGroups(root.results)
  #unique.fleets = LoadUniqueFleets(root.results)
  
  #Create a data.table in which to compile all the tables from each file ready for saving to csv
  dt.all = data.table()
  
  #Find out how many years
  # nTimeStepsInData = CountTimestepsFile(igroup = unique.groups[1], resultfolder = folder.name, ncols_no_vals = 5)
  
  #get list of files
  catch.files = list.files(path = paste(root.results, folder.name, "/", sep=""), full.names = T)
  
  for(iFile.catch in catch.files){
    
    #print(iFile.catch)
    
    catches = fread(iFile.catch, skip=7, header=T)
    
    #Determine file is valid
    if(!isNotAll(dt = catches, col.data.starts = 6, val.to.check=0)) next
    if(!isNotAll(dt = catches, col.data.starts = 6, val.to.check = -9999)) next
    
    #load the files and sum
    catches_last5year = calcLast5Year(catches, "catch.last5yearsum", 5, function.type="mean")
    
    dt = merge(catches[,1:5], catches_last5year, by = c("ModelID", "StrategyName"))
    
    #bind them all together ready to be saved to csv
    dt.all = rbind(dt.all, dt)

  }
  
  catch.summary.by.strategy = dt.all[,list(Min = min(catch.last5yearsum), 
                                                 LQ = quantile(catch.last5yearsum, .25, na.rm=TRUE), 
                                                 Median = median(catch.last5yearsum),
                                                 Mean = mean(catch.last5yearsum), 
                                                 UQ = quantile(catch.last5yearsum, .75, na.rm=TRUE),
                                                 Max = max(catch.last5yearsum)), by=c("StrategyName", "GroupName", "FleetName")]

  #finally save the table to csv
  write.csv(catch.summary.by.strategy, paste(root.plot, "Tables/",CatchType,"_5NumSum.csv", sep=""))
  
}

# FUNCTION END ===============================================================================================