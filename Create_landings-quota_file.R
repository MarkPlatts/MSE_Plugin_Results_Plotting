library(stringr)
source("share_tools.R")

rm(list = ls())

data.folder.path <- "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/North Sea MultiAnnual Plan/ResultsType1-4_220117/"
#data.folder.path <- "C:/Users/Mark/Desktop/Test_BigEffort/"

script.dir <- dirname(sys.frame(1)$ofile)
library(data.table)
library(stringr)
source(paste(script.dir,"/share_tools.R", sep=""))

prepare_dt_merge = function(dt,  val.name, ncol.before.timesteps = 5){
  dt = data.table(dt)
  dt = rename_timesteps(dt = dt, colstart = ncol.before.timesteps + 1, yearstart = 1)
  dt = melt(data = dt, id.vars = 1:ncol.before.timesteps, variable.name = "TimeStep", value.name = val.name)
  dt = dt[, !c("ResultType"), with=FALSE]
  return(dt)
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

root.path.results = str_c(data.folder.path, "Results/")
write.path = str_c(data.folder.path, "Processed_Results/landings-quota/")

path.landings = paste(root.path.results, "LandingsTrajectories/", sep = "")
path.quota = paste(root.path.results, "HCRQuota_Targ/", sep = "")
path.choke = str_c(root.path.results, "ChokeGroup/")

# unique.groups = LoadUniqueGroups(path = paste(root.path.results, sep=""))
unique.groups = c("Cod (adult)", "Haddock (adult)", "Whiting (adult)", "Saithe (adult)", "Sole", "Plaice", "Nephrops")
unique.fleets = LoadUniqueFleets(path = paste(root.path.results, sep=""))

for(iFleet in c(str_c("FleetNo", 1:11))){
  
  dt.choke = LoadFile_ContainsListStrings(Dir.Path = path.choke,
                                          StringsInFileName = iFleet)
  
  if(isAllNA(dt.choke, col.data.starts = 5)) {
    print("Error: Choke all NAs!")
    next
  }

  dt.choke = prepare_dt_merge(dt = dt.choke, val.name = "choke", ncol.before.timesteps = 4)
  dt.choke = dt.choke[!is.na(choke)]

  dt.choke <- dt.choke[, TimeStep := as.numeric(TimeStep)]
  dt.choke = dt.choke[, TimeStep := 1 + floor((TimeStep-1)/12)]
  dt.choke = dt.choke[, choke := Mode(choke), by = .(ModelID, StrategyName, TimeStep)]
  dt.choke <- unique(dt.choke)
  
  for(iGroup in unique.groups){

    group_fleet = c(iGroup, iFleet)
    
    dt.landings = LoadFile_ContainsListStrings(Dir.Path = path.landings, 
                                               StringsInFileName = group_fleet)
    
    dt.quota = LoadFile_ContainsListStrings(Dir.Path = path.quota,
                                            StringsInFileName = group_fleet)
    
    if(typeof(dt.landings) != "list") {
      print("Error: No landings file")
      next
    }
    if(typeof(dt.quota) != "list") {
      print("Error: No quota file")
      next
    }
    
    if(isNotAll(dt.quota, col.data.starts = 6, val.to.check = -9999)){
      
      dt.landings = prepare_dt_merge(dt = dt.landings, val.name = "landings")
      dt.quota =    prepare_dt_merge(dt = dt.quota, val.name = "quota")

      dt = merge(dt.landings, dt.quota)
      dt[, TimeStep := as.numeric(TimeStep)]
      dt = merge(dt, dt.choke, by = c("FleetName", "ModelID", "StrategyName", "TimeStep"))
      dt[, landings.minus.quota := landings - quota]
      dt[, landings.minus.quota.modified := landings.minus.quota * (GroupName!=choke)]
      
      write.csv(x = dt, file = paste(write.path, "landings-quota_", iGroup, "_", iFleet, ".csv", sep=""))
      
      sum = dt[, .(sum.across.timeseries = sum(landings.minus.quota)), by = list(ModelID, StrategyName)]
      write.csv(x = sum, file = paste(write.path, "sumByModelID,StrategyName_", iGroup, "_", iFleet,  ".csv", sep=""))
      
      average = sum[, .(average.across.models = mean(sum.across.timeseries)), by = list(StrategyName)]
      write.csv(x = average, file = paste(write.path, "averageAcrossModels_", iGroup, "_", iFleet, ".csv", sep=""))
      
    } else {
      print("Error: quota contains all -9999")
    }
    
  }
  
}





