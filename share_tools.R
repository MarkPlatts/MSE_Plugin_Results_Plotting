#library(dplyr)
library(reshape2)
library(dtplyr)
library(dplyr)
library(data.table)

countColsWithVals = function(file.name, ncols_no_vals, lines.to.skip){
  file.to.inspect = read.csv(file.name, skip=lines.to.skip)
  nCols = ncol(file.to.inspect)-ncols_no_vals
  return(nCols)
}


groupsWithHcr = function(hcr.folders, groups.for.f.or.biomass){
  #compiles a list of all the unique groups with a hcr in the location of folders  

  hcr.file.list = list.files(hcr.folders, full.names = TRUE)
  
  groups.list = vector()
  for(iFile in hcr.file.list){
    dt = fread(iFile, skip=1, header=T)
    if(groups.for.f.or.biomass=="f") groups.list = c(groups.list, dt$GroupNameForF)
    if(groups.for.f.or.biomass=="biomass") groups.list = c(groups.list, dt$GroupNameForBiomass)
  }
  
  groups.unique.in.hcrs = unique(groups.list)
  
  return(groups.unique.in.hcrs)
  
}


getStrategyTable = function(hcr.folders){
  #load up all the hcrs into memory so that we can access and use the values when comparing with realised F's

  #compile a vector with the filenames and associated paths for all hcr files
  hcr.file.list = list.files(hcr.folders, full.names = TRUE)
  
  strategies = data.table()
  
  for(iStrategy in hcr.file.list){
    #get strategy name and the hcrs it contains
    strategy.name = sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(iStrategy))
    strategy.name = gsub("_hcr", "", strategy.name)
    strategy.table = fread(iStrategy, skip=1, header=T)
    
    #create table from strategy name and hcrs and append to overall strategies data.table
    strategy.table = appendVariableToDataTable(strategy.table, strategy.name, variablename = "StrategyName", beg=TRUE, end=FALSE)
    strategies = rbind(strategies, strategy.table)
  }
  
  return (strategies)
  
}


appendVariableToDataTable = function(dt, variable, variablename, beg, end){
  #append a single value to a column either before the first column or after the last depending on whether beg or end is TRUE
  
  nrows.table = dim(dt)[1]
  if(beg) dt.appended = cbind(data.table(x = rep(variable, nrows.table)), dt)
  if(end) dt.appended = cbind(dt, data.table(x = rep(variable, nrows.table)))
  names(dt.appended)[names(dt.appended) == "x"] = variablename
  
  return(dt.appended)
}


isNotAll = function(dt, col.data.starts, val.to.check)
  #count how many values aren't NA and if there is at least one then return that file is valid
{

  data.only = dt[, col.data.starts:ncol(dt)]
  file.valid = FALSE
  if(sum(data.only!=val.to.check)>0) {file.valid = TRUE}
  return (file.valid)
}

GetiYearCatch = function(dt, iYear, ncols.before.timeseries){
  
  #load the quota
  dt.melted = melt(dt, id=names(dt)[1:ncols.before.timeseries])
  
  #change name of columns to be more relevant
  names(dt.melted)[names(dt.melted)=="variable"] = "TimeStep"
  names(dt.melted)[names(dt.melted)=="value"] = paste("Year", iYear, sep="")
  
  #Change type of column
  dt.melted$TimeStep = as.numeric(dt.melted$TimeStep)
  
  #find the maximum timestep
  #nTimeSteps = max(dt.melted$TimeStep)
  
  #filter for iYear
  dt.melted = dt.melted[TimeStep==iYear]
  # #calc sum by strategy
  # if(function.type==1){
  #   dt.Last5YearSum.byStrategy = dt.melted[,.(Last5YearSum=sum(value)), by=.(StrategyName,ModelID)]
  # } else if(function.type==2){
  #   dt.Last5YearSum.byStrategy = dt.melted[,.(Last5YearSum=mean(value)), by=.(StrategyName,ModelID)]
  # }
  dt.melted[, TimeStep:=NULL]
  
  #change the name of the column to one specified in params so that when we merge two tables
  #we have column names that refer to data that the last 5 year mean was calculated for
  #names(dt.Last5YearSum.byStrategy)[names(dt.Last5YearSum.byStrategy)=="Last5YearSum"] = val.col.name
  
  #return(dt.Last5YearSum.byStrategy)
  return(dt.melted)
  
}


calcLast5Year = function(dt, val.col.name, ncols.before.timeseries, function.type)
  #outputs as a table the total catch or landings for the last 5 years
  # ncols.before.timeseries is the number of columns with information such as group strategy modelID prior 
  #to the values in the time series
  #if function.type == 1 then sum, if function.type == 2 then mean
{

  #load the quota
  dt.melted = melt(dt, id=names(dt)[1:ncols.before.timeseries])
  
  #change name of timestep column to be more relevant
  names(dt.melted)[names(dt.melted)=="variable"] = "TimeStep"
  
  #Change type of column
  dt.melted$TimeStep = as.numeric(dt.melted$TimeStep)
  
  #find the maximum timestep
  nTimeSteps = max(dt.melted$TimeStep)
  
  #filter out the last 5 years
  dt.melted = dt.melted[TimeStep>nTimeSteps-5]
  #calc sum by strategy
  if(function.type==1){
    dt.Last5YearSum.byStrategy = dt.melted[,.(Last5YearSum=sum(value)), by=.(StrategyName,ModelID)]
  } else if(function.type==2){
    dt.Last5YearSum.byStrategy = dt.melted[,.(Last5YearSum=mean(value)), by=.(StrategyName,ModelID)]
  }

  #change the name of the column to one specified in params so that when we merge two tables
  #we have column names that refer to data that the last 5 year mean was calculated for
  names(dt.Last5YearSum.byStrategy)[names(dt.Last5YearSum.byStrategy)=="Last5YearSum"] = val.col.name
  
  return(dt.Last5YearSum.byStrategy)
  
}

initialise_plotting_params = function(folder.name, plot.each.timestep, start.run.year, end.run.year, root.path){
  #init plotting params
  plotting_params = list()
  
  #Create a vector of x vals at either yearly or monthly intervals
  plotting_params$TimeStepVals = get_timestep_vals(plot.each.timestep, start.run.year, end.run.year)
  
  #get a list of all the files in the Biomass folder
  plotting_params$g <- list.files(paste(params$RootPath,"\\",folder.name, sep=''))
  
  return(plotting_params)
}

CreateFolderIfDoesntExist = function(folder.name, path){
  dir2create = paste(path, folder.name, sep="")
  if(!dir.exists(dir2create)){
    dir.create(dir2create)
  }
}


# calculating the upper and lower confidence intervals and median ---------
calc_vals_for_plotting = function(params, plotting_params){
  
  plotting_params$MDNS<- plotting_params$LOWS<- plotting_params$UPPS<- plotting_params$MEANS<- data.frame(year=plotting_params$TimeStepVals,row.names =plotting_params$TimeStepVals)
  for(strat_i in 1:length(params$strats)){

    STRAT<-paste(params$strats[strat_i],sep=' ')
    
    #select subset of data
    data2plot<- plotting_params$dat[plotting_params$dat$Strategy %in% STRAT,5:ncol(plotting_params$dat)]
    
    #quantiles for polygon plot
    perc<-apply(data2plot,2, FUN=function(x){quantile(x,probs=c(0.025,0.5,0.975),na.rm=T)})
    perc<-rbind(perc, apply(data2plot,2, FUN=mean) )
    
    #save percs
    plotting_params$LOWS<- cbind(plotting_params$LOWS,perc[1,]);   names(plotting_params$LOWS)[ncol(plotting_params$LOWS)]<-STRAT
    plotting_params$MDNS<- cbind(plotting_params$MDNS,perc[2,]);   names(plotting_params$MDNS)[ncol(plotting_params$MDNS)]<-STRAT
    plotting_params$UPPS<- cbind(plotting_params$UPPS,perc[3,]);   names(plotting_params$UPPS)[ncol(plotting_params$UPPS)]<-STRAT
    plotting_params$MEANS<- cbind(plotting_params$MEANS,perc[4,]); names(plotting_params$MEANS)[ncol(plotting_params$MEANS)]<-STRAT

  }
  
  return(plotting_params)
  
}

#Create a list of user selected strategies ---------------------------------
create_list_strategies = function(Path2ResultsCSV)
{
  path_and_filename = paste(Path2ResultsCSV, "Results.csv", sep='')
  results<-read.table(path_and_filename,sep=',',skip=8,col.names=c("Model","Strategy","GroupID","GroupName","Variable","Value"), fill=T)
  results<-results[results$Strategy!="Z",]#odd one in _SR_final
  strats <- as.character(unique(results$Strategy))  # 10/15 strategies
  vector_of_strats = vector()
  repeat {
    print(strats)
    nstrats = length(strats)
    keypress = readline("Press index number for strategy to add to array or e to end")
    if(keypress == "e") break
    keypress = strtoi(keypress) # convert to integer so we can check whether in range of indices for strategies
    if(keypress >= 1 & keypress <= nstrats){
      vector_of_strats = c(vector_of_strats, strats[keypress])
    }
  }
  return(vector_of_strats)
  
}


# Calculate all the x-values for plotting ---------------------------------
get_timestep_vals = function(plotmonthly, start_year, end_year){
  if (plotmonthly){
    xvals=seq(start_year,end_year-1/12,1/12)
  } else {
    xvals = start_year:(end_year-1)
  }
}

LoadUniqueStrategies = function(path){
  
  file.path = paste(path,"/ValueTrajectories/ValueYearly_AllGroups_FleetNo8.csv", sep="")
  file.data = read.csv(file.path, skip=7, header = TRUE)
  UniqueStrategies = as.vector(as.matrix(unique(file.data$StrategyName)))
  return(UniqueStrategies)
  
}

LoadUniqueGroups = function(path)
{
  if(file.exists(paste(path,"UniqueGroups.csv",sep=""))){
    UniqueGroups = as.matrix(read.csv(paste(path,"UniqueGroups.csv", sep=""),header = T))
  } else {
    file.path = paste(path,"/Results.csv", sep="")
    file.data = read.csv(file.path, skip=7, header = TRUE)
    file.data = filter(file.data, ResultName == "BiomassMin")
    UniqueGroups = as.vector(as.matrix(unique(file.data$GroupName)))
    write.csv(UniqueGroups,paste(path,"UniqueGroups.csv",sep=""), row.names = F)
  }
  return(UniqueGroups)
}

SubsetVectorStrings_ContainingString = function(VectorStrings, String2Find)
{
  SubsetIndices = grep(String2Find, VectorStrings, fixed=TRUE)
  return(VectorStrings[SubsetIndices])
}

#Checks whether the filename given is incorrect given setting to either plot yearly or none yearly values
IsIncorrectFileType_YearlyMonthly = function(FileName, plot_yearly){
  
  
  if(length(grep("Yearly", FileName, fixed=TRUE))>0 & !plot_yearly) return(TRUE)
  if(length(grep("Yearly", FileName, fixed=TRUE))==0 & plot_yearly) return(TRUE)
  return(FALSE)

}


#Find a string within another string
#This can be done in a single line but it is fairly unreadable
StringContains = function(ContainingString, String2Check)
{
  return(length(grep(String2Check, ContainingString, fixed=TRUE))>0)
}


#Check that multiple strings all exist within another string
StringContains_AllStrings = function(ContainingString, MultipleStrings2Check)
{
  for(iString in MultipleStrings2Check){
    if(StringContains(ContainingString, iString)==FALSE) 
      return(FALSE)
  }
  return(TRUE)
}


Check_FileName_Contains_Strings = function(FileName, MultipleStrings2Check)
  #This just wraps around StringContains_AllStrings with a name more fitting for the code so easier to read
{
  StringContains_AllStrings(FileName, MultipleStrings2Check)
}


GetFileName_ContainsStrings = function(FolderPath, Strings, WithPath)
{
  #Create a list of files at path
  AllFileNames = list.files(FolderPath, full.names = WithPath)
  
  #cycle through list checking whether file contains the strings
  for(iFileName in AllFileNames)
  {
    if(Check_FileName_Contains_Strings(iFileName,Strings))
    {
      return(iFileName)
    }
  }
  
}


FileIsForACompareGroup = function(params, FILENAME){
  nNotGroups2Compare=0
  for (igroup in params$Groups2Plot){
    if(!length(grep(igroup,FILENAME, fixed=TRUE))>0) nNotGroups2Compare = nNotGroups2Compare+1
  }
  if(length(params$Groups2Plot)==nNotGroups2Compare) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

FileIsForACompareGroupFleet = function(params, FILENAME){
  FILENAME = paste(FILENAME,".csv", sep="")
  for (igroup in params$Groups2Plot){
    if(length(grep(igroup,FILENAME, fixed=TRUE))>0) {
      for (iFleet in params$Fleets2Plot){
        if(iFleet=="AllFleets" && length(grep("AllFleets",FILENAME, fixed=TRUE))>0) {
          #browser()
          return (TRUE)
        }
        if(length(grep(paste(iFleet,".csv", sep=''),FILENAME, fixed=TRUE))>0) {
          #browser()
          return (TRUE)
        }
      }
    }
  }
  return (FALSE)
}

NumberOfValsNotNA = function(object.to.check, STRAT){
  
  return(sum(object.to.check[object.to.check$Strategy %in% STRAT,6:ncol(object.to.check)]!=-9999))
  
}
