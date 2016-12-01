rm(list = ls())

library(reshape)
#library(dplyr)
#library(plyr)
library(data.table)

source("C:/Users/Mark/Desktop/MSE_Plugin_Results_Plotting/share_tools.R")

### Generate tables for the risk-reward plots
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_riskreward_table <- function(){
  
  plot.path = "C://Users/Mark/Dropbox/GAP2_MSE Plugin2/North Sea MultiAnnual Plan/withBiomassForcing_Yearly_Results HCR type1 and 3/Plots/"
  RootPath =  "C://Users/Mark/Dropbox/GAP2_MSE Plugin2/North Sea MultiAnnual Plan/withBiomassForcing_Yearly_Results HCR type1 and 3/Results"
  
  #get a list of all the files in the Biomass folder
  g.biomass <- list.files(paste(RootPath,"\\Biomass", sep=''), full.names=TRUE)
  g.catch <- list.files(paste(RootPath,"\\CatchTrajectories", sep=''), full.names=TRUE)
  
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
    
    # unique_strats = unique(dat$StrategyName)
    # unique_ModelID = unique(dat$ModelID)
    
    dat = filter(dat, variable>=39)
    #print(ddply(dat, .(GroupName,ModelID,StrategyName,ResultType),summarise, mean_val = mean(value)))
    dat_biomass = ddply(dat, .(GroupName,ModelID,StrategyName),summarise, Biomass5YearMean = mean(value))

    #Find the matching catch trajectory
    for(G.catch in g.catch){
      if (length(grep(GroupName, G.catch, fixed=TRUE))==1){
        if (length(grep("AllFleets", G.catch, fixed=TRUE))==1){

          dat <- fread(G.catch,skip=7, head=T)
          #names(dat) = c(names(dat)[1:5],1:20)
          
          dat <- melt(dat, id=c("GroupName","FleetName","ModelID","StrategyName","ResultType"))
          dat$variable = as.numeric(dat$variable)
          
          # unique_strats = unique(dat$StrategyName)
          # unique_ModelID = unique(dat$ModelID)
          
          dat = filter(dat, variable>=16)
          dat_catch = ddply(dat, .(GroupName,ModelID,StrategyName),summarise, Catch5YearMean = mean(value))
          break
        }
      }
    }
    
    both <- merge(dat_biomass,dat_catch,by=c("ModelID","GroupName","StrategyName"), sort=FALSE) 

    total <- rbind(total,both)
    
    #browser()
    #Save this to risk_reward_tables
    #write.csv(both,paste(plot.path,"RISK_REWARD_TABLES/",GroupName,".csv",sep=""))
  }
  
  write.csv(total,paste(plot.path,"RISK_REWARD_TABLES/Total.csv",sep=""))
  
}