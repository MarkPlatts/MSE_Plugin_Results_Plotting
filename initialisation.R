initialise_params = function(batch){
  # batch = 0 means plot all strategies in the folder "Plots"
  
  params = list()
  
  params$plot_each_timestep = FALSE;
  params$PLOT_CONFIDENCE_INTERVALS = F;
  params$Plot_yearly_files = TRUE
  
  path.data.folder = "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/North Sea MultiAnnual Plan/ResultsType1-4_220117/"
  
  switch (batch,
             "0" = {params$plot.path = paste(path.data.folder,"Plots/", sep="")}
             "1" = {params$plot.path = paste(path.data.folder,"Plots_strats_1-20/", sep="")}
             "2" = {params$plot.path = paste(path.data.folder,"Plots_strats_21-40/", sep="")},
             "3" = {params$plot.path = paste(path.data.folder,"Plots_strats_41-end/", sep="")}
  )
  params$RootPath =  paste(path.data.folder, "Results/", sep="")
  
  params$hcr.folders = c("C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/NorthSea Model/2015 FINAL Key Run/DATA/HCRs/Type1_BmsytoZero",
    "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/NorthSea Model/2015 FINAL Key Run/DATA/HCRs/Type2_BmsyBlimClifftoZero",
    "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/NorthSea Model/2015 FINAL Key Run/DATA/HCRs/Type3_BmsytoZeroatBlim",
    "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/NorthSea Model/2015 FINAL Key Run/DATA/HCRs/Type4_BmsyBlimClifftoFmin")
  
  
  params$Base_NYears = 23
  params$Projected_NYears = 20
  params$StartRun_Year = 1991
  
  params$nYrs = params$Base_NYears + params$Projected_NYears
  params$StartProjection_Year = params$StartRun_Year + params$nYrs - params$Projected_NYears
  params$EndRun_Year = params$StartRun_Year + params$nYrs
  
  params$Area = 570000
  
  #Select groups and fleets to plot
  params$Groups2Plot = paste("GroupNo", 1:66, sep="")
  # params$Groups2Plot = c("GroupNo14", "GroupNo16")
  # params$Groups2Plot = c("GroupNo16","GroupNo14","GroupNo18","GroupNo20","GroupNo21","GroupNo23","GroupNo29","GroupNo30",
  #                        "GroupNo31","GroupNo33","GroupNo34","GroupNo38","GroupNo42","GroupNo22","GroupNo26","GroupNo32",
  #                        "GroupNo35","GroupNo39","GroupNo41","GroupNo55")
  
  #params$Fleets2Plot = c("AllFleets", "FleetNo1")
  params$Fleets2Plot = c("AllFleets", "FleetNo1", "FleetNo2", "FleetNo3", "FleetNo4", "FleetNo5", "FleetNo6", "FleetNo7",
  "FleetNo8", "FleetNo9", "FleetNo10", "FleetNo11")

  #setwd(params$RootPath)
  
  ### Load results files to get unique strats (comment/uncomment to select method of choosing strategies to plot)
  #plot all strats
  params$strats = LoadUniqueStrategies(params$RootPath)
  params$strats <- params$strats[params$strats != "NONE"]
  
  #SPECIFY STRATEGIES TO PLOT ===================================================================
  #comment/uncomment according to method you want to use for specifying strategies
  
  # 1. specifying ranges directly ---------------------------------------------------------------
  #params$strats <- params$strats[41:length(params$strats)]
  #params$strats <- params$strats[21:40]
  
  # 2. running this code in batch mode ----------------------------------------------------------
  # switch (batch,
  #           "1" = {params$strats <- params$strats[1:20]},
  #           "2" = {params$strats <- params$strats[21:40]},
  #           "3" = {params$strats <- params$strats[41:length(params$strats)]})
  
  # 3. specifying a vector of the strategies names -----------
  params$strats <- c("14 NSMAP 2020_HighF_Highest value", "11 NSMAP 2020_HighF_Weakest stock")
  
  # 4. enabling the stategies to be chosen interactively-----------------------------------------
  # params$strats <- create_list_strategies(params$RootPath)
  # =============================================================================================
    
  params$reg.types = c("Weakest stock", "Highest value", "Selective")
    
  params$lineweight = 0.3
  params$legend_x_inset2 = -0.45
  params$COL = rep(1:8,10)[1:length(params$strat)]
  params$LTY<- c(rep(1,8),rep(2,8),rep(3,8),rep(4,8),rep(5,8),rep(6,8),rep(7,8),rep(8,8))[1:length(params$strats)]
  params$WRITE<-F
  params$SAVE<-T
  params$LEGEND<-T
  
  params$MORT_HCRF_Cons <- F
  params$MORT_HCRF_Targ<- F
  params$QUOTA_HCRF_Cons<- F
  params$QUOTA_HCRF_Targ<- F
  params$MORT_REAL_LandF <- F
  params$MORT_REAL_DiscF <- F
  params$MORT_REAL_F <- F
  
  params$CATCH <-F
  params$LANDING <- F
  params$DISCARD <- F
  
  params$EFFORT <-F
  params$VALUE <- F
  params$BIOMASS <-F
  
  params$HIGHEST_VALUE <- F
  params$CHOKE_GROUPS <- F
  
  params$AverageQuota_EachFleet <- F
  
  return(params)
}
