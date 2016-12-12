rm(list = ls())

library(reshape)
library(plyr)
library(data.table)

source("C:/Users/Mark/Desktop/MSE_Plugin_Results_Plotting/share_tools.R")

### Generate tables for the risk-reward plots
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_riskreward_table <- function(){

  plot.path = "C://Users/Mark/Dropbox/GAP2_MSE Plugin2/North Sea MultiAnnual Plan/Yearly_Results HCR type1 and 3/Plots/"
  RootPath =  "C://Users/Mark/Dropbox/GAP2_MSE Plugin2/North Sea MultiAnnual Plan/Yearly_Results HCR type1 and 3/Results"
  
  #Create folder to store the results in
  dir.create("C://Users/Mark/Dropbox/GAP2_MSE Plugin2/North Sea MultiAnnual Plan/Yearly_Results HCR type1 and 3/Plots/RISK_REWARD_TABLES")
  dir.create("C://Users/Mark/Dropbox/GAP2_MSE Plugin2/North Sea MultiAnnual Plan/Yearly_Results HCR type1 and 3/Plots/RISK_REWARD_TABLES/byGroup")
  
    
  #get a list of all the files in the Biomass folder
  g.biomass <- list.files(paste(RootPath,"\\Biomass", sep=''), full.names=TRUE)
  g.catch <- list.files(paste(RootPath,"\\CatchTrajectories", sep=''), full.names=TRUE)
  g.landings <- list.files(paste(RootPath,"\\LandingsTrajectories", sep=''), full.names=TRUE)
  
  total = data.table()
  
  for(G.biomass in g.biomass){

    #Get the filename to be used to check whether yearly in name, to name files of plots and to add text to plots
    FILENAME = substr(G.biomass,1,nchar(G.biomass)-4)
    
    #dat <- read.csv(G.biomass,skip=7, head=T)
    dat <- fread(G.biomass,skip=7, head=T)
    #names(dat) = c(names(dat)[1:4],1:43)
    
    dat <- melt(dat, id=c("GroupName","ModelID","StrategyName","ResultType"))
    dat$variable = as.numeric(dat$variable)
    GroupName = dat[1,GroupName]
    
    dat = filter(dat, variable>=39)
    dat_biomass = ddply(dat, .(GroupName,ModelID,StrategyName),summarise, Biomass5YearMean = mean(value))

    dat_catch = CalcMean_CatchLandings(g.catch, GroupName)
    dat_landings = CalcMean_CatchLandings(g.landings, GroupName)
    names(dat_landings)[4]="Landings5YearMean"

    both <- merge(dat_biomass,dat_catch,by=c("ModelID","GroupName","StrategyName"), sort=FALSE)
    both <- merge(both,dat_landings, by=c("ModelID","GroupName","StrategyName"), sort=FALSE)

    total <- rbind(total,both)
    
    #Save this to risk_reward_tables
    write.csv(both,paste(plot.path,"RISK_REWARD_TABLES/byGroup/",GroupName,".csv",sep=""))
    
  }
  
  write.csv(total,paste(plot.path,"RISK_REWARD_TABLES/Total.csv",sep=""))
  
}

CalcMean_CatchLandings = function(catchlandings, GroupName){
  
  #Find the matching catch trajectory
  for(G.catchlandings in catchlandings){
    if (length(grep(GroupName, G.catchlandings, fixed=TRUE))==1){
      if (length(grep("AllFleets", G.catchlandings, fixed=TRUE))==1){
        
        dat <- fread(G.catchlandings,skip=7, head=T)
        
        dat <- melt(dat, id=c("GroupName","FleetName","ModelID","StrategyName","ResultType"))
        dat$variable = as.numeric(dat$variable)
        
        dat = filter(dat, variable>=16)
        dat_catch = ddply(dat, .(GroupName,ModelID,StrategyName),summarise, Catch5YearMean = mean(value))
        break
      }
    }
  }
  return(dat_catch)
}

