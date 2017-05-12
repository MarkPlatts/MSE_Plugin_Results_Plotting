rm(list = ls())

script.dir <- dirname(sys.frame(1)$ofile)
library(data.table)
source(paste(script.dir,"/share_tools.R", sep=""))

prepare.landings.quotas.merge = function(dt,  val.name){
  dt = data.table(dt)
  dt = rename_timesteps(dt = dt, colstart = 6, yearstart = 1)
  dt = melt(data = dt, id.vars = 1:5, variable.name = "TimeStep", value.name = val.name)
  dt = dt[, !c("ResultType"), with=FALSE]
}

root.path.results = "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/North Sea MultiAnnual Plan/ResultsType1-4_220117/Results/"

path.landings = paste(root.path.results, "LandingsTrajectories/", sep = "")
path.quota = paste(root.path.results, "HCRQuota_Targ/", sep = "")


unique.groups = LoadUniqueGroups(path = paste(root.path.results, sep=""))

for(iGroup in unique.groups){
  
  group_fleet = c(iGroup, "AllFleets")
  
  dt.landings = LoadFile_ContainsListStrings(Dir.Path = path.landings, 
                                             StringsInFileName = group_fleet)
  
  dt.quota = LoadFile_ContainsListStrings(Dir.Path = path.quota,
                                          StringsInFileName = group_fleet)
  
  if(isNotAll(dt.quota, col.data.starts = 6, val.to.check = -9999)){
    dt.landings = prepare.landings.quotas.merge(dt = dt.landings, val.name = "landings")
    dt.quota =    prepare.landings.quotas.merge(dt = dt.quota, val.name = "quota")
    
    dt = merge(dt.landings, dt.quota)
    dt$landings.minus.quota = dt$landings - dt$quota
    
    write.csv(x = dt, file = paste("C:/Users/Mark/Desktop/Test/", iGroup, ".csv", sep=""))
  }
  
}





