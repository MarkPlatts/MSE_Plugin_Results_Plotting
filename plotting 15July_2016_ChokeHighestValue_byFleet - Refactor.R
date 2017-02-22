plot_type = function(type2plot) {
  
  options(scipen = 10)
  
  setwd("C:/Users/Mark/Desktop/MSE_Plugin_Results_Plotting/")
  source("initialisation.R")
  source("B_Trajectories.R")
  source("F_Trajectories.R")
  source("Value_Trajectories.R")
  source("Effort_Trajectories.R")
  source("Pies.R")
  source("plot_tools.R")
  source("share_tools.R")
  source("Calc_average_quota_per_fleet_group_regulation.R")
  
  params = initialise_params()

  create.plot.dirs(params)
  
  ###HCR F Cons trajectories
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  if(type2plot==1){
    params$MORT_HCRF_Cons <- T;  if(params$MORT_HCRF_Cons) {setwd("HCRF_Cons");params$YLAB<-"F";print("Plotting MORT_HCRF_Cons")} # what are these?
    plot_fishing_trajectories(params, "HCRF_Cons")
  }
  
  ###HCR F Targ trajectories
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  if(type2plot==2){
    params$MORT_HCRF_Targ<- T;   if(params$MORT_HCRF_Targ) {setwd("HCRF_Targ");params$YLAB<-"F";print("Plotting HCRF_Targ")} # what are these?
    plot_fishing_trajectories(params, "HCRF_Targ")
  }
  
  ###HCR Quota Cons trajectories
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  if(type2plot==3){
    params$QUOTA_HCRF_Cons<- T;  if(params$QUOTA_HCRF_Cons) {setwd("HCRQuota_Cons");params$YLAB<-"Quota (t)";print("Plotting HCRQuota_Cons")} # what are these?
    plot_fishing_trajectories(params, "HCRQuota_Cons")
  }  
  
  ###HCR Quota Targ trajectories
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  if(type2plot==4){
    params$QUOTA_HCRF_Targ<- T;  if(params$QUOTA_HCRF_Targ) {setwd("HCRQuota_Targ");params$YLAB<-"Quota (t)";print("Plotting HCRQuota_Targ")} # what are these?
    plot_fishing_trajectories(params, "HCRQuota_Targ")
  }
  
  ###Real Landed F trajectories
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  if(type2plot==5){
    params$MORT_REAL_LandF <- T; if(params$MORT_REAL_LandF) {setwd("RealisedLandedF");params$YLAB<-"F";print("Plotting MORT_REAL_LandF")}
    plot_fishing_trajectories(params, "RealisedLandedF")
  }
  
  ###Real Discard F trajectories
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  if(type2plot==6){
    params$MORT_REAL_DiscF <- T; if(params$MORT_REAL_DiscF) {setwd("RealisedDiscardedF");params$YLAB<-"F";print("Plotting MORT_REAL_DiscF")}
    plot_fishing_trajectories(params, "RealisedDiscardedF")
  }
  
  ###Real F trajectories
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  if(type2plot==7){
    params$MORT_REAL_F <- T;     if(params$MORT_REAL_F) {setwd("RealisedF");params$YLAB<-"F";print("Plotting MORT_REAL_F")}
    plot_fishing_trajectories(params, "RealisedF")
  }
  
  ###Catch trajectories
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  if(type2plot==8){
    params$CATCH <-T;    if(params$CATCH) {setwd("CatchTrajectories"); params$YLAB<-"Catch (t/year)";print("Plotting CATCH")}
    plot_fishing_trajectories(params, "CatchTrajectories")
  }
  
  ###Landed trajectories
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  if(type2plot==9){
    params$LANDING <- T; if(params$LANDING) {setwd("LandingsTrajectories"); params$YLAB<-"Landings (t/year)";print("Plotting LANDING")}
    plot_fishing_trajectories(params, "LandingsTrajectories")
  }
  
  ###Discard trajectories
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  if(type2plot==10){
    params$DISCARD <- T; if(params$DISCARD) {setwd("DiscardsTrajectories"); params$YLAB<-"Discards (t/year)";print("Plotting DISCARD")}
    plot_fishing_trajectories(params, "DiscardsTrajectories")
  }
  
  ###Effort trajectories
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  if(type2plot==11){
    params$EFFORT <-T;print("Plotting EFFORT")
    plot_effort_trajectories(params)
  }
  
  ###Value trajectories
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  if(type2plot==12){
    params$VALUE <- T;print("Plotting VALUE")
    plot_value_trajectories(params)
  }
  
  ###Biomass trajectoreis
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  if(type2plot==13){
    params$BIOMASS <-T;print("Plotting BIOMASS")
    plot_biomass_trajectories(params)
  }
  
  ###HIGHEST_VALUE Pie chart
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if(type2plot==14){
    params$HIGHEST_VALUE <- T;print("Plotting HIGHEST_VALUE")
    run_plot_pies(params, parents.folder.for.plots = "/HIGHEST_VALUE/", 
                               results.folder.name = "HighestValueGroup", by.regulations = TRUE)
  }
  
  ###CHOKE SPECIES Pie chart
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if(type2plot==15){
    params$CHOKE_GROUPS <- T;print("Plotting CHOKE_GROUPS")
    run_plot_pies(params, parents.folder.for.plots = "/CHOKE_GROUPS/",
                        results.folder.name = "ChokeGroup", by.regulations = FALSE)
  }
  
  ###Plot average target quota across a regulation type and models
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if(type2plot==16){
    params$AverageQuota_EachFleet <- T; params$YLAB<-"t/km2";print("Plotting AverageQuota_EachFleet")
    Plot_Average_Quotas(results.path = params$RootPath, plot.path = params$plot.path, Groups = params$Groups2Plot, 
                        Fleets = params$Fleets2Plot, TimeSteps =c(1:20), 
                        RegulationTypes = c("Highest value", "Weakest stock", "Selective"))
    }
  
}

for(iplot in c(1:13)){
  print(paste("Currently plotting type", iplot))
  plot_type(iplot)
}
