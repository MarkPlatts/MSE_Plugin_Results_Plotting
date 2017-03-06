# INITIALISATION START ===============================================================================================

#start with a clean sheet
rm(list = ls())

#load libraries
source("share_tools.R")

#root results path
root.plot =     "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/North Sea MultiAnnual Plan/ResultsType1-4_220117/Plots/"
root.results =  "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/North Sea MultiAnnual Plan/ResultsType1-4_220117/Results/"

hcr.folders = c("C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/NorthSea Model/2015 FINAL Key Run/DATA/HCRs/Type1_BmsytoZero",
                "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/NorthSea Model/2015 FINAL Key Run/DATA/HCRs/Type2_BmsyBlimClifftoZero",
                "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/NorthSea Model/2015 FINAL Key Run/DATA/HCRs/Type3_BmsytoZeroatBlim",
                "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/NorthSea Model/2015 FINAL Key Run/DATA/HCRs/Type4_BmsyBlimClifftoFmin")

groups.for.f.or.biomass = "f"

# INITIALISATION END ===============================================================================================


# SCRIPT START  ===============================================================================================

if(TRUE){
  #get a list of all the groups
  unique.groups = groupsWithHcr(hcr.folders, groups.for.f.or.biomass)
  
  #get a list of strategy.tables with strategy name
  strategies.table = getStrategyTable(hcr.folders)
  
  #remove all conservation hcrs from strategy table
  strategies.table = filter(strategies.table, Target_or_Conservation==0)
  
  #Create a data.table in which to compile all the tables from each file ready for saving to csv
  dt.all = data.table()
  
  for(igroup in unique.groups)
  {

    #get the file in HCRQuota_Targ folder that are for "AllFleets"
    realisedF.file = GetFileName_ContainsStrings(FolderPath = paste(root.results, "/RealisedF/", sep=""), 
                                                 Strings = c(igroup), WithPath=T)
    #get a list of all the files in CatchTrajectories that are for "AllFleets"
    # catches.file = GetFileName_ContainsStrings(FolderPath = paste(root.results, "/CatchTrajectories/", sep=""), 
    #                                            Strings = c("AllFleets", igroup), WithPath=T)
    #get the hcr
    
    realised.f = fread(realisedF.file, skip=7, header=T)
    
    #Determine file is valid
    if(!isNotAll(dt = realised.f, col.data.starts = 4, val.to.check = -9999)) next
    
    #load the files and sum
    realised.f = calcLast5Year(realised.f, "realised.f.last5yearsum", 5, function.type = 2)
    
    #add a column with the group name - need this to merge with the strategy data.table
    realised.f = appendVariableToDataTable(dt=realised.f, variable=igroup, variablename="GroupNameForF", beg=TRUE, end=FALSE)

    #merge the two together so that we can easily calculate the difference between two columns
    dt = merge(x=realised.f, y=strategies.table, by = c("StrategyName", "GroupNameForF"), all.x = TRUE)
    dt = cbind(dt,data.table(realf.maxf.diff = dt[,realised.f.last5yearsum] - dt[,MaxF]))

    #Check where the catch is greater than or less than the quota
    dt[, "RealisedF.greater.than.maxF"] = dt$realf.maxf.diff>=0
    dt[, "RealisedF.less.than.maxF"] = dt$realf.maxf.diff<0
    
    #For each strategy count how many models with realisedFs above and below maxFs and merge them together
    NumberAbove = dt[,.(NumberAbove=sum(RealisedF.greater.than.maxF)), by=.(StrategyName)]
    NumberBelow = dt[,.(NumberBelow=sum(RealisedF.less.than.maxF)), by=.(StrategyName)]
    dt.counts.temp = merge(NumberAbove,NumberBelow, by = c("StrategyName"))
    
    #Create new column that turns the counts into percentages
    dt.counts.temp$PercentAbove = dt.counts.temp$NumberAbove/(dt.counts.temp$NumberAbove+dt.counts.temp$NumberBelow)*100
    dt.counts.temp$PercentBelow = dt.counts.temp$NumberBelow/(dt.counts.temp$NumberAbove+dt.counts.temp$NumberBelow)*100
    
    #add column with groupname to differentiate after binding
    dt.counts.temp = cbind(data.table(GroupName = rep(igroup, dim(dt.counts.temp)[1])), dt.counts.temp) 
    
    #bind them all together ready to be saved to csv
    dt.all = rbind(dt.all, dt.counts.temp)
    
  }
  
  #finally save the table to csv
  write.csv(dt.all, paste(root.plot, "Tables/Percentage_RealisedF_Above_Below_maxF.csv", sep=""))
}

# SCRIPT END  ===============================================================================================



# TESTING START ===================================================================================================

if(FALSE){
  
  print(getStrategyTable(hcr.folders))
  
}

# TESTING END ===================================================================================================