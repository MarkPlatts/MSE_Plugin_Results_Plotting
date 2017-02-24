# INITIALISATION START ===============================================================================================

rm(list = ls())

source("share_tools.R")

#root results path
root.plot = "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/North Sea MultiAnnual Plan/ResultsType1-4_220117/Plots/"
root.results = "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/North Sea MultiAnnual Plan/ResultsType1-4_220117/Results/"

# INITIALISATION END ===============================================================================================


# SCRIPT START  ===============================================================================================

#get a list of all the groups
unique.groups = LoadUniqueGroups(root.results)

#Create a data.table in which to compile all the tables from each file ready for saving to csv
dt.all = data.table()

for(igroup in unique.groups)
{
  
  #get file in HCRQuota_Targ folder that are for "AllFleets" and current igroup
  hcr.quota.targ.file = GetFileName_ContainsStrings(FolderPath = paste(root.results, "/HCRQuota_Targ/", sep=""), 
                                                    Strings = c("AllFleets", igroup), WithPath=T)
  #get file in CatchTrajectories folder that are for "AllFleets" and current igroup
  catches.file = GetFileName_ContainsStrings(FolderPath = paste(root.results, "/CatchTrajectories/", sep=""), 
                                             Strings = c("AllFleets", igroup), WithPath=T)
  
  #Determine file is valid
  if(!isNotAllNeg9999(file.name.with.path = hcr.quota.targ.file, col.data.starts = 6)) next
  if(!isNotAllNeg9999(file.name.with.path = catches.file, col.data.starts = 6)) next
  
  #load the files and sum
  quota = calcLast5Year(hcr.quota.targ.file, "quota.last5yearsum", 5, function.type=1)
  catch = calcLast5Year(catches.file, "catch.last5yearsum", 5, function.type=1)
  
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