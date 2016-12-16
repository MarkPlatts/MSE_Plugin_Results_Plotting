library(dplyr)
library(reshape)
library(ggplot2)

setwd("C:/Users/Mark/Desktop/MSE_Plugin_Results_Plotting/")
source("share_tools.R")


#Loads the file in folder specified containing all the strings in vector of strings
LoadFile_ContainsListStrings = function(Dir.Path, StringsInFileName)
{
  #Get a list of all the files
  AllFiles <- list.files(Dir.Path)
  #Need to loop across all files so that we can extract
  for (iFile in AllFiles){
    #Find and load file that contains values for selected group and fleet
    FoundFile = StringContains_AllStrings(ContainingString=iFile, MultipleStrings2Check=StringsInFileName)
    if(FoundFile) {
      iFile.data <- read.csv(paste(Dir.Path,iFile, sep=''),skip=7, head=T)
      return (iFile.data)
    }
  }
  return(NA)
  
}


Average_Quota_Across_Models_And_RegTypes = function(Group, Fleet, RegulationType, ResultsPath)
{

  FileData = LoadFile_ContainsListStrings(Dir.Path = paste(ResultsPath,"/HCRQuota_Targ/",sep=''), StringsInFileName = c(Group, Fleet))
  
  UniqueStrategies = LoadUniqueStrategies(ResultsPath)
  
  Strategies_OfRegType = SubsetVectorStrings_ContainingString(UniqueStrategies, RegulationType)

  FileData = filter(FileData, StrategyName %in% Strategies_OfRegType)
  
  return(as.numeric(colMeans(FileData[,6:ncol(FileData)])))

}

Plot_Average_Quotas = function(Path, Groups, Fleet, TimeStep, RegulationTypes)
{
  Area_km2 = 570000
  #Get all the groups to plot
  #UniqueGroups = LoadUniqueGroups(Path)
  
  df_Average_Quota = data.frame()
  
  #Cycle across all groups to plot and extract the results putting them in a dataframe
  for(iGroup in Groups)
  {
  print(iGroup)
    for(iRegulation in RegulationTypes)
    {
      temp_mean_vals = Average_Quota_Across_Models_And_RegTypes(iGroup,Fleet,iRegulation,Path)
      temp_mean_vals = temp_mean_vals * Area_km2
      df_Average_Quota = rbind(df_Average_Quota, data.frame(TimeStep = TimeStep, GroupName = iGroup, Regulation = iRegulation, AverageQuota = temp_mean_vals))
    }
  }
  
  #use ggplot to plot results - specify legends to be species and a column of different regulation types
  print(qplot(TimeStep, AverageQuota, data=df_Average_Quota, geom=c("line"), color=GroupName, facets=Regulation~., main=Fleet))
}


