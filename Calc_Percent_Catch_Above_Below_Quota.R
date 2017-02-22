# INITIALISATION START ===============================================================================================

rm(list = ls())

source("share_tools.R")

library(reshape2)
library(data.table)

#root results path
root.plot = "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/North Sea MultiAnnual Plan/ResultsType1-4_220117/Plots/"
root.results = "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/North Sea MultiAnnual Plan/ResultsType1-4_220117/Results/"

# INITIALISATION END ===============================================================================================


# FUNCTIONS START ===============================================================================================

calcLast5YearSum = function(folder.and.filename, val.col.name, path.to.results.folder)
#outputs as a table the total catch or landings for the last 5 years
{
  
  #load the quota
  dt = fread(paste(path.to.results.folder, folder.and.filename, sep=""), skip=7, header=T)
  
  #Create table for last 5 years
  dt.melted = melt(dt, id=names(dt)[1:5])
  
  #change name of timestep column to be more relevant
  names(dt.melted)[names(dt.melted)=="variable"] = "TimeStep"
  
  #Change type of column
  dt.melted$TimeStep = as.numeric(dt.melted$TimeStep)
  
  #filter out the last 5 years
  dt.melted = dt.melted[TimeStep>15]
  #calc sum by strategy
  dt.Last5YearSum.byStrategy = dt.melted[,.(Last5YearSum=sum(value)), by=.(StrategyName,ModelID)]
  
  #change the name of the column to one specified in params so that when we merge two tables
  #we have column names that refer to data that the last 5 year mean was calculated for
  names(dt.Last5YearSum.byStrategy)[names(dt.Last5YearSum.byStrategy)=="Last5YearSum"] = val.col.name
  
  return(dt.Last5YearSum.byStrategy)

}

isValidValuesInFile = function(file.name.with.path, col.data.starts)
#count how many values aren't NA and if there is at least one then return that file is valid
{
  dt = fread(file.name.with.path, skip=7, header=T)
  data.only = dt[1:nrow(dt), col.data.starts:ncol(dt)]
  file.valid = FALSE
  if(sum(data.only!=-9999)>0) {file.valid = TRUE}
  return (file.valid)
}

# FUNCTIONS END ===============================================================================================


# SCRIPT START  ===============================================================================================

#get a list of all the groups
unique.groups = LoadUniqueGroups(root.results)

#Create a data.table in which to compile all the tables from each file ready for saving to csv
dt.all = data.table()

for(igroup in unique.groups)
{
  
  #get a list of all the files in HCRQuota_Targ folder that are for "AllFleets"
  hcr.quota.targ.file = GetFileName_ContainsStrings(FolderPath = paste(root.results, "/HCRQuota_Targ/", sep=""), 
                                                    Strings = c("AllFleets", igroup), WithPath=T)
  #get a list of all the files in CatchTrajectories that are for "AllFleets"
  catches.file = GetFileName_ContainsStrings(FolderPath = paste(root.results, "/CatchTrajectories/", sep=""), 
                                             Strings = c("AllFleets", igroup), WithPath=T)
  
  #Determine file is valid
  if(!isValidValuesInFile(file.name.with.path = hcr.quota.targ.file, col.data.starts = 6)) next
  if(!isValidValuesInFile(file.name.with.path = catches.file, col.data.starts = 6)) next
  
  #load the files and sum
  quota = calcLast5YearSum("HCRQuota_Targ/HCR_Quota_Targ_Cod (adult)_GroupNo14_AllFleets.csv", "quota.last5yearsum", root.results)
  catch = calcLast5YearSum("CatchTrajectories/TotalCatchYearly_Cod (adult)_GroupNo14_AllFleets.csv", "catch.last5yearsum", root.results)
  
  #merge the two together so that we can easily calculate the difference between two columns
  dt = merge(quota, catch, by = c("StrategyName", "ModelID"))
  dt[, "diff"] = dt[,"catch.last5yearsum"] - dt[,"quota.last5yearsum"]
  
  #Check where the catch is greater than or less than the quota
  dt[, "Catch.greater.than.quota"] = dt$diff>=0
  dt[, "Catch.less.than.quota"] = dt$diff<0
  
  #For each strategy count how many models with catches above and below quota and merge them together
  NumberAbove = dt[,.(NumberAbove=sum(Catch.greater.than.quota)), by=.(StrategyName)]
  NumberBelow = dt[,.(NumberBelow=sum(Catch.less.than.quota)), by=.(StrategyName)]
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
write.csv(dt.all, paste(root.plot, "Tables/Percentage_Catches_Above_Below_Quota.csv", sep=""))

# SCRIPT END  ===============================================================================================
