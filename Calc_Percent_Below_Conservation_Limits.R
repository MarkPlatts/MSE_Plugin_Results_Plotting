# INITIALISATION START ===============================================================================================

#start with a clean sheet
rm(list = ls())

#load sources
# source.folder.location = dirname(sys.frame(1)$ofile)
# setwd(source.folder.location)
setwd("C:/Users/Mark/Desktop/MSE_Plugin_Results_Plotting")
source("share_tools.R")
#library(reshape)

#root results path
root.plot =     "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/North Sea MultiAnnual Plan/ResultsType1-4_220117/Plots/"
root.results =  "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/North Sea MultiAnnual Plan/ResultsType1-4_220117/Results/"

hcr.folders = c("C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/NorthSea Model/2015 FINAL Key Run/DATA/HCRs/Type1_BmsytoZero",
                "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/NorthSea Model/2015 FINAL Key Run/DATA/HCRs/Type2_BmsyBlimClifftoZero",
                "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/NorthSea Model/2015 FINAL Key Run/DATA/HCRs/Type3_BmsytoZeroatBlim",
                "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/NorthSea Model/2015 FINAL Key Run/DATA/HCRs/Type4_BmsyBlimClifftoFmin")

groups.for.f.or.biomass = "biomass"

# INITIALISATION END ===============================================================================================



# FUNCTION START  ===============================================================================================

CreatePercentBelowConservationLimits = function(BLimitType){


  #get a list of hcrs by listed by strategy and group name with only biomass limits
  strategies.table = getStrategyTable(hcr.folders)
  strategies.table = filter(strategies.table, Target_or_Conservation==1)
  strategies.table = strategies.table[,.(StrategyName, GroupName = GroupNameForBiomass, Limit = get(BLimitType))]#, StepBiomass, UpperLimit)]
  strategies.table = unique(strategies.table)

  #get a list of all the groups
  unique.groups = unique(strategies.table$GroupName)

  #Create a data.table in which to compile all the tables from each file ready for saving to csv
  dt.all = data.table()
  
  for(igroup in unique.groups)
  {
    
    #get a list of all the files in CatchTrajectories that are for "AllFleets"
    biomass.file = GetFileName_ContainsStrings(FolderPath = paste(root.results, "/Biomass/", sep=""),
                                               Strings = c(igroup), WithPath=T)
    
    #Load the file
    biomass = fread(biomass.file, skip=7, header=T)
    
    #Determine file is valid
    if(!isNotAll(dt = biomass, col.data.starts = 4, val.to.check = -9999)) next
    
    #Sum last 5 year
    biomass = calcLast5Year(biomass, "biomass.last5yearsum", 4, function.type = 2)

    #add a column with the group name - need this to merge with the strategy data.table
    biomass = appendVariableToDataTable(dt=biomass, variable=igroup, variablename="GroupName", beg=TRUE, end=FALSE)

    #merge the two together so that we can easily calculate the difference between two columns
    dt = merge(x=biomass, y=strategies.table, by = c("StrategyName", "GroupName"), all.x = TRUE)

    dt = dt[Limit!="NA"]
    dt = cbind(dt, data.table(avbiomass.hcr.limit.diff = dt[,biomass.last5yearsum] - dt[,Limit]))
    
    #Check where the catch is greater than or less than the quota
    dt[, "avbiomass.greaterthan.hcr.upperlimit.diff"] = dt$avbiomass.hcr.limit.diff>=0
    dt[, "avbiomass.lessthan.hcr.upperlimit.diff"] = dt$avbiomass.hcr.limit.diff<0
    
    #For each strategy count how many models with realisedFs above and below maxFs and merge them together
    NumberAbove = dt[,.(NumberAbove=sum(avbiomass.greaterthan.hcr.upperlimit.diff)), by=.(StrategyName)]
    NumberBelow = dt[,.(NumberBelow=sum(avbiomass.lessthan.hcr.upperlimit.diff)), by=.(StrategyName)]
    dt.counts.temp = merge(NumberAbove,NumberBelow, by = c("StrategyName"))
    
    #Create new column that turns the counts into percentages
    dt.counts.temp$PercentAbove = dt.counts.temp$NumberAbove/(dt.counts.temp$NumberAbove+dt.counts.temp$NumberBelow)*100
    dt.counts.temp$PercentBelow = dt.counts.temp$NumberBelow/(dt.counts.temp$NumberAbove+dt.counts.temp$NumberBelow)*100
    
    #add column with groupname to differentiate after binding
    dt.counts.temp = cbind(data.table(GroupName = rep(igroup, dim(dt.counts.temp)[1])), dt.counts.temp) 
    
    #join table to strategy table so we can output the limit
    dt.counts.temp = merge(dt.counts.temp, strategies.table[,c("StrategyName", "GroupName", "Limit")], by=c("GroupName", "StrategyName"))

    dt.counts.temp = rename(dt.counts.temp, c("Limit" = BLimitType))

    #bind them all together ready to be saved to csv
    dt.all = rbind(dt.all, dt.counts.temp)
    
  }
  
  #finally save the table to csv
  write.csv(dt.all, paste(root.plot, "Tables/Percentage_ConservationSpecies_Above_Below_B_",BLimitType ,".csv", sep=""))
}

# FUNCTION END  ===============================================================================================



# FUNCTION CALLS START ===============================================================================================

CreatePercentBelowConservationLimits("LowerLimit")
CreatePercentBelowConservationLimits("UpperLimit")

# FUNCTION CALLS END ===============================================================================================



# TESTING START ===================================================================================================

if(FALSE){
  
  print(getStrategyTable(hcr.folders))
  
}

# TESTING END ===================================================================================================