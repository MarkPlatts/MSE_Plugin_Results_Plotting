library(dplyr)

initialise_plotting_params = function(folder_name, params){
  #init plotting params
  plotting_params = list()
  
  #Create a vector of x vals at either yearly or monthly intervals
  plotting_params$TimeStepVals = get_timestep_vals(params$plot_each_timestep, params$StartRun_Year, params$EndRun_Year)
  
  #reset the director
  #setwd(paste(params$RootPath,"\\",folder_name, sep=''))
  
  #get a list of all the files in the Biomass folder
  plotting_params$g <- list.files(paste(params$RootPath,"\\",folder_name, sep=''))
  
  return(plotting_params)
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

