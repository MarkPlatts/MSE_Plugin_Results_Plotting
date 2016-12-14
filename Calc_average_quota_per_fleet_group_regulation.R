library(dplyr)

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
  
  return(as.numeric(colMeans(FileData[,6:25])))

}

# ResultPath = "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/R Code/TestFolder_R_Plotting_MSE_Plugin/withBiomassForcing_Yearly_Results HCR type1 and 3/Results"
# Average_Quota_Across_Models_And_RegTypes("Cod (adult", "FleetNo2", "Highest value", ResultPath)

