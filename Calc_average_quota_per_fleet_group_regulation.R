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
  
  if(isAll(FileData, col.data.starts=6, val.to.check=-9999)) return(NA)
  
  UniqueStrategies = LoadUniqueStrategies(ResultsPath)
  
  Strategies_OfRegType = SubsetVectorStrings_ContainingString(UniqueStrategies, RegulationType)

  FileData = filter(FileData, StrategyName %in% Strategies_OfRegType)
  
  return(as.numeric(colMeans(FileData[,6:ncol(FileData)])))

}

Plot_Average_Quotas = function(results.path, plot.path, Groups, Fleets, TimeSteps, RegulationTypes)
{
  Area_km2 = 570000
  #Get all the groups to plot
  #UniqueGroups = LoadUniqueGroups(Path)
  for(iFleet in Fleets)
  {
  
    df_Average_Quota = data.frame()
    
    #Cycle across all groups to plot and extract the results putting them in a dataframe
    for(iGroup in Groups)
    {
      print(paste("Group =", iGroup, "Fleet =",iFleet))
      for(iRegulation in RegulationTypes)
      {
        temp_mean_vals = Average_Quota_Across_Models_And_RegTypes(iGroup,iFleet,iRegulation,results.path)
        if(is.na(temp_mean_vals)) next
        temp_mean_vals = temp_mean_vals * Area_km2
        df_Average_Quota = rbind(df_Average_Quota, data.frame(TimeSteps = TimeSteps, GroupName = iGroup, Regulation = iRegulation, AverageQuota = temp_mean_vals))
      }
    }

    #use ggplot to plot results - specify legends to be species and a column of different regulation types  
    # Plot2Save = qplot(TimeSteps, AverageQuota, data=df_Average_Quota, geom=c("line"), color=GroupName, facets=Regulation~., main=iFleet)
    Plot2Save = ggplot(data=df_Average_Quota, aes(x = TimeSteps, y = AverageQuota, color = GroupName)) + 
      geom_line(aes(linetype=GroupName)) + 
      facet_grid(Regulation ~.) + 
      scale_linetype_manual(values=rep(c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"), 20))
    ggsave(Plot2Save, file=paste(plot.path,"/AverageQuota_EachFleet/AverageQuota_",iFleet,".png",sep=""), width=6, height=12, limitsize = FALSE)
    
  }
  
}


