library(reshape)
library(dplyr)
#library(data.table)

source("C:/Users/Mark/Desktop/MSE_Plugin_Results_Plotting/share_tools.R")

### Generate tables for the risk-reward plots
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_riskreward_table <- function(){

  plot.path = "C://Users/Mark/Dropbox/GAP2_MSE Plugin2/North Sea MultiAnnual Plan/Yearly_Results HCR type1 and 3/Plots/"
  RootPath =  "C://Users/Mark/Dropbox/GAP2_MSE Plugin2/North Sea MultiAnnual Plan/Yearly_Results HCR type1 and 3/Results/"
  
  #Create folder to store the results in
  dir.create("C://Users/Mark/Dropbox/GAP2_MSE Plugin2/North Sea MultiAnnual Plan/Yearly_Results HCR type1 and 3/Plots/RISK_REWARD_TABLES", showWarnings = FALSE)
  dir.create("C://Users/Mark/Dropbox/GAP2_MSE Plugin2/North Sea MultiAnnual Plan/Yearly_Results HCR type1 and 3/Plots/RISK_REWARD_TABLES/byGroup", showWarnings = FALSE)
  
  #get a list of all the files in the Biomass folder
  g.biomass.filenames <- list.files(paste(RootPath,"\\Biomass", sep=''), full.names=TRUE)
  g.catch.filenames <- list.files(paste(RootPath,"\\CatchTrajectories", sep=''), full.names=TRUE)
  g.landings.filenames <- list.files(paste(RootPath,"\\LandingsTrajectories", sep=''), full.names=TRUE)
  g.value.filenames <- list.files(paste(RootPath, "\\ValueTrajectories", sep=''), full.names=TRUE)
  
  GroupsOnly_Vars2MeltBy = c("GroupName","ModelID","StrategyName","ResultType")
  GroupFleet_Vars2MeltBy = c("GroupName","FleetName","ModelID","StrategyName","ResultType")
  
  Biomass_StartAverageAtTimeStep = 39
  Biomass_StartTimeStep = 23
  CatchTrajectories_StartAverageAtTimeStep = 16
  
  total = data.table()
  
  UniqueGroups = LoadUniqueGroups(RootPath)
  
  for(iGroup in UniqueGroups){
    
    biomass_filename = GetFileName_ContainsStrings(paste(RootPath,"\\Biomass", sep=''), c(iGroup), WithPath=TRUE)
    dat_biomass = Calc_Last5YearMean(biomass_filename, "Biomass_Last5YearMean", GroupsOnly_Vars2MeltBy, Biomass_StartAverageAtTimeStep)
    dat_biomass_start = Get_StartBiomass(biomass_filename, "Biomass_Start", GroupsOnly_Vars2MeltBy, Biomass_StartTimeStep)
    
    catch_filename = GetFileName_ContainsStrings(paste(RootPath,"\\CatchTrajectories", sep=''), c(iGroup, "AllFleets"), WithPath=TRUE)
    dat_catch = Calc_Last5YearMean(catch_filename, "Catch_Last5YearMean", GroupFleet_Vars2MeltBy, CatchTrajectories_StartAverageAtTimeStep)
    
    landings_filename = GetFileName_ContainsStrings(paste(RootPath,"\\LandingsTrajectories", sep=''), c(iGroup, "AllFleets"), WithPath=TRUE)
    dat_landings = Calc_Last5YearMean(landings_filename, "Landings_Last5YearMean", GroupFleet_Vars2MeltBy, CatchTrajectories_StartAverageAtTimeStep)
    
    value_filename = GetFileName_ContainsStrings(paste(RootPath,"\\ValueTrajectories", sep=''), c(iGroup, "AllFleets"), WithPath=TRUE)
    dat_value = Calc_Last5YearMean(value_filename, "Value_Last5YearMean", GroupFleet_Vars2MeltBy, CatchTrajectories_StartAverageAtTimeStep)
    
    both <- merge(dat_biomass,dat_biomass_start,by=c("ModelID","GroupName","StrategyName"), sort=FALSE)
    both <- merge(both,dat_catch, by=c("ModelID","GroupName","StrategyName"), sort=FALSE)
    both <- merge(both,dat_landings, by=c("ModelID","GroupName","StrategyName"), sort=FALSE)
    both <- merge(both,dat_value, by=c("ModelID","GroupName","StrategyName"), sort=FALSE)

    total <- rbind(total,both)
    
    #Save this to risk_reward_tables
    write.csv(both,paste(plot.path,"RISK_REWARD_TABLES/byGroup/",iGroup,".csv",sep=""))
    
  }
  
  write.csv(total,paste(plot.path,"RISK_REWARD_TABLES/Total.csv",sep=""))
  
}


Calc_Last5YearMean = function(GroupFileName, DataName, vars2meltby, StartTimeStep)
{
  
  dat = Get_Data_And_Melt(GroupFileName, vars2meltby)
  
  dat = filter(dat, variable>=StartTimeStep)
  dat_Averaged = ddply(dat, .(GroupName,ModelID,StrategyName),summarise, Last5YearMean = mean(value))
  
  names(dat_Averaged)[4] = DataName
  
  return(dat_Averaged)
  
}

Get_StartBiomass = function(GroupFileName, DataName, vars2meltby, StartTimeStep)
{
  dat = Get_Data_And_Melt(GroupFileName, vars2meltby)
  
  dat = filter(dat, variable==StartTimeStep)
  dat = select(dat, GroupName, ModelID, StrategyName, value)
  
  names(dat)[4] = DataName

  return(dat)
}

Get_Data_And_Melt = function(GroupFileName, vars2meltby)
{
  
  dat <- fread(GroupFileName, skip=7, head=T)
  
  dat <- melt(dat, id=vars2meltby)
  dat$variable = as.numeric(dat$variable)
  
  return(dat)
  
}



