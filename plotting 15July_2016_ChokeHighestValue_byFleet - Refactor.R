#' Calls the major plotting functions
#'
#' @param type2plot The number of the plot type to plot - see code to determine what the codes mean
#'
#' @examples
#' plot_type(type2plot = 1)
plot_type = function(type2plot) {
  
  options(scipen = 10)
  
  setwd("C:/Users/Mark/Desktop/Desktop etc/GAP/MSE_Plugin_Results_Plotting/")
  
  source("initialisation.R")
  source("B_Trajectories.R")
  source("F_Trajectories.R")
  source("Value_Trajectories.R")
  source("Effort_Trajectories.R")
  source("Pies2.R")
  source("share_tools.R")
  source("Calc_average_quota_per_fleet_group_regulation.R")
  source("Plot_Distribution_Last5YearsBiomassMean.R")
 
  params = initialise_params(batch)

  create.plot.dirs(params)
  
  ###HCR F Cons trajectories
  if(type2plot==1){
    plot_fishing_trajectories(params, folder.to.save.plot = "HCRF_Cons", y_label = "F", plot_type = "mort_hcrf_cons")
  }
  
  ###HCR F Targ trajectories
  if(type2plot==2){
    plot_fishing_trajectories(params, folder.to.save.plot = "HCRF_Targ", y_label = "F", plot_type = "mort_hcrf_targ")
  }
  
  ###HCR Quota Cons trajectories
  if(type2plot==3){
    plot_fishing_trajectories(params, folder.to.save.plot = "HCRQuota_Cons", y_label = "Quota (t)", plot_type = "quota_hcrf_cons")
  }  
  
  ###HCR Quota Targ trajectories
  if(type2plot==4){
    plot_fishing_trajectories(params, folder.to.save.plot = "HCRQuota_Targ", y_label = "Quota (t)", plot_type = "quota_hcrf_targ")
  }
  
  ###Real Landed F trajectories
  if(type2plot==5){
    plot_fishing_trajectories(params, folder.to.save.plot = "RealisedLandedF", y_label = "F", plot_type = "mort_real_landf")
  }
  
  ###Real Discard F trajectories
  if(type2plot==6){
    plot_fishing_trajectories(params, folder.to.save.plot = "RealisedDiscardedF", y_label = "F", plot_type = "mort_real_discf")
  }
  
  ###Real F trajectories
  if(type2plot==7){
    plot_fishing_trajectories(params, folder.to.save.plot = "RealisedF", y_label = "F", plot_type = "mort_real_f")    
  }
  
  ###Catch trajectories
  if(type2plot==8){
    plot_fishing_trajectories(params, folder.to.save.plot = "CatchTrajectories", y_label = "Catch (t/year)", plot_type = "catch")    
  }
  
  ###Landed trajectories
  if(type2plot==9){
    plot_fishing_trajectories(params, folder.to.save.plot = "LandingsTrajectories", y_label = "Landings (t/year)", plot_type = "landings")
  }
  
  ###Discard trajectories
  if(type2plot==10){
    plot_fishing_trajectories(params, folder.to.save.plot = "DiscardsTrajectories", y_label = "Discards (t/year)", plot_type = "discards")
  }
  
  ###Effort trajectories
  if(type2plot==11){
    params$EFFORT <-T;print("Plotting EFFORT")
    plot_effort_trajectories(params)
  }
  
  ###Value trajectories
  if(type2plot==12){
    params$VALUE <- T;print("Plotting VALUE")
    plot_value_trajectories(params)
  }
  
  ###Biomass trajectoreis
  if(type2plot==13){
    params$BIOMASS <-T;print("Plotting BIOMASS")
    plot_biomass_trajectories(params)
  }
  
  ###HIGHEST_VALUE Pie chart
  if(type2plot==14){
    params$HIGHEST_VALUE <- T;print("Plotting HIGHEST_VALUE")
    run_plot_pies(params, parents.folder.for.plots = "/HIGHEST_VALUE/", 
                  results.folder.name = "HighestValueGroup", by.regulations = TRUE,
                  pie.name = "Highest value: percentage of years across all models")
  }
  
  ###CHOKE SPECIES Pie chart
  if(type2plot==15){
    params$CHOKE_GROUPS <- T;print("Plotting CHOKE_GROUPS")
    run_plot_pies(params, parents.folder.for.plots = "/CHOKE_GROUPS/",
                  results.folder.name = "ChokeGroup", by.regulations = FALSE,
                  pie.name = "Choke groups: percentage of years across all years of all models")
  }
  
  ###Plot average target quota across a regulation type and models
  if(type2plot==16){
    params$AverageQuota_EachFleet <- T; params$YLAB<-"t/km2";print("Plotting AverageQuota_EachFleet")
    Plot_Average_Quotas(results.path = params$RootPath, plot.path = params$plot.path, Groups = params$Groups2Plot, 
                        Fleets = params$Fleets2Plot, TimeSteps =c(1:20), 
                        RegulationTypes = c("Highest value", "Weakest stock", "Selective"))
  }
  
  ###Plot the biomass for the last 5 year mean
  if(type2plot==17){
    #source("Plot_Distribution_Last5YearsBiomassMean.R")
    Plot_Distribution_Last5YearsBiomassMean(params)
  }
  
}

batch = "0"

# already done plot types: 11, 13
vector_indices_plot_types = 15

for(iplot in vector_indices_plot_types){
  #print(paste("Currently plotting type", iplot))
  plot_type(type2plot = iplot)
}
