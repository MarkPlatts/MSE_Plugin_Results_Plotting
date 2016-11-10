source("B_Trajectories.R")
source("F_Trajectories.R")
source("Value_Trajectories.R")
source("Effort_Trajectories.R")
source("ChokePie.R")
source("HighestValuePie.R")
source("plot_tools.R")
source("share_tools.R")

plot_type = function(type2plot) {
  
  create.plot.dirs()
  
  params = initialise_params()
  
  #OBSELETE ================================================================================================
  #mods  <- (unique(results$Model))                  # 294 models
  #gnames<- as.character(unique(results$GroupName))# 77 groups: 1=Baleen whales, 65=Phytoplankton. 66-77 fleets, n-1 AllGroups, n AllFleets
  #gnames<- as.character(results$GroupName[c(14,18,16,29,31)])
  #gnames<- c(gnames, "AllGroups", "AllFleets")
  #vars  <- as.character(unique(results$Variable)) # 6 Variables: "BiomassMin" "BiomassEnd" "Landings" "DiscardMortalities" "DiscardSurvivals" "TotalEndValue"
  #OBSELETE ================================================================================================

  #OBSELETE ================================================================================================
  ###Trajectories by MODEL run
  #m<-mods[1]
  #read a model
  #setwd(paste(params$RootPath,"\\Biomass",sep=''))
  #biomass.filenames = list.files("Biomass")
  
  #path<-paste("Biomass\\",biomass.filenames[m],sep='')
  #mod<-read.csv(path,skip=6, head=T, fill=T)              #monthly time-series
  #mod<-read.table(path, skip=7, header = TRUE, fill = TRUE,sep=",",as.is =T)
  #nyrs <- (ncol(mod)-4)/12 # num yrs
  
  #take only january values
  #mod<-mod[,c(1:4,4+seq(1,nyrs*12,12))]          #dec values would be  mod[,3+seq(12,nyrs*12,12)]
  #mod[62:67,1:5] #inspect
  #mod[1:2,1:5] #info
  
  #### mortalities
  #setwd("Y:\\WP1 Tradeoffs in mixed fisheries\\NSea Multiannual Plan\\Evaluation\\STECF Results 1000_30 years_SR_final\\")
  #setwd("Y:\\WP1 Tradeoffs in mixed fisheries\\NSea Multiannual Plan\\Evaluation\\STECF Results 1000_30 years_SR\\")
  #setwd("params$RootPath"); g <- list.files("Trajectories2"); params$strats<- c("HCR_HighF_Highest value","HCR_LowF_Highest value")
  #OBSELETE ================================================================================================
  
  

  
  if(type2plot==1){
    params$MORT_HCRF_Cons <- T;  if(params$MORT_HCRF_Cons) {setwd("HCRF_Cons");params$YLAB<-"F";print("Plotting MORT_HCRF_Cons")} # what are these?
  }
  if(type2plot==2){
    params$MORT_HCRF_Targ<- T;   if(params$MORT_HCRF_Targ) {setwd("HCRF_Targ");params$YLAB<-"F";print("Plotting HCRF_Targ")} # what are these?
  }
  if(type2plot==3){
    params$QUOTA_HCRF_Cons<- T;  if(params$QUOTA_HCRF_Cons) {setwd("HCRQuota_Cons");params$YLAB<-"t/km2";print("Plotting HCRQuota_Cons")} # what are these?
  }
  if(type2plot==4){
    params$QUOTA_HCRF_Targ<- T;  if(params$QUOTA_HCRF_Targ) {setwd("HCRQuota_Targ");params$YLAB<-"t";print("Plotting HCRQuota_Targ")} # what are these?
  }
  if(type2plot==5){
    params$MORT_REAL_LandF <- T; if(params$MORT_REAL_LandF) {setwd("RealisedLandedF");params$YLAB<-"F";print("Plotting MORT_REAL_LandF")}
  }
  if(type2plot==6){
    params$MORT_REAL_DiscF <- T; if(params$MORT_REAL_DiscF) {setwd("RealisedDiscardedF");params$YLAB<-"F";print("Plotting MORT_REAL_DiscF")}
  }
  if(type2plot==7){
    params$MORT_REAL_F <- T;     if(params$MORT_REAL_F) {setwd("RealisedF");params$YLAB<-"F";print("Plotting MORT_REAL_F")}
  }
  #params$YLAB<-"catch (t)"
  if(type2plot==8){
    params$CATCH <-T;    if(params$CATCH) {setwd("CatchTrajectories"); params$YLAB<-"catch (t/year)";print("Plotting CATCH")}
  }
  if(type2plot==9){
    params$LANDING <- T; if(params$LANDING) {setwd("LandingsTrajectories"); params$YLAB<-"landings (t/year)";print("Plotting LANDING")}
  }
  if(type2plot==10){
    params$DISCARD <- T; if(params$DISCARD) {setwd("DiscardsTrajectories"); params$YLAB<-"discards (t/year)";print("Plotting DISCARD")}
  }
  
  if(type2plot==11){
    params$EFFORT <-T;print("Plotting EFFORT")
  }
  if(type2plot==12){
    params$VALUE <- T;print("Plotting VALUE")
  }
  if(type2plot==13){
    params$BIOMASS <-T;print("Plotting BIOMASS")
  }
  
  if(type2plot==14){
    params$HIGHEST_VALUE <- T;print("Plotting HIGHEST_VALUE")
  }
  if(type2plot==15){
    params$CHOKE_GROUPS <- T;print("Plotting CHOKE_GROUPS")
  }
  
  if (params$COMPARE_STRATEGIES){
    print(params$strats)
    print("")
    strat1 <- readline(prompt="Select by vector index the 1st Strategy you want to compare:")
    strat2 <- readline(prompt="Select by vector index the 2nd Strategy you want to compare:")
    strat1name = strat[strtoi(strat1)]
    strat2name = strat[strtoi(strat2)]
    print(strat1name)
    print(strat2name)
    #params$Groups2Plot = c(14,16,18,29, 78)
    #params$Groups2Plot = 14
    #params$Fleets2Plot = c(67:77, 79)
    #params$Fleets2Plot = c(67:68)
    print("Comparing for groups: ")
    print(params$Groups2Plot)
    print("Compare for fleets: ")
    print(params$Fleets2Plot)
  }
  
  ###F Trajectories
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  if(any(params$MORT_REAL_F,params$MORT_REAL_LandF,params$MORT_REAL_DiscF,params$MORT_HCRF_Cons,params$MORT_HCRF_Targ,params$QUOTA_HCRF_Cons,params$QUOTA_HCRF_Targ,params$CATCH,params$DISCARD,params$LANDING)) plot_fishing_trajectories(params)
  
  ###Biomass trajectoreis
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  if (params$BIOMASS) plot_biomass_trajectories(params)
  
  ###Value trajectories
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  if (params$VALUE) plot_value_trajectories(params)
  
  ###Effort trajectories
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  if (params$EFFORT) plot_effort_trajectories(params)

  ###HIGHEST_VALUE Pie chart
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if(params$HIGHEST_VALUE) plot_highestvalue_pies(params)
  
  ###CHOKE SPECIES Pie chart
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if(params$CHOKE_GROUPS) plot_choke_pies(params)

}  

for(iplot in c(1:15)){
  print(paste("Currently plotting type", iplot))
  plot_type(iplot)
}
