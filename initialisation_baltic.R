initialise_params = function(batch){
  # batch = 0 means plot all strategies in the folder "Plots"
  
  params = list()
  
  params$plot_each_timestep = FALSE;
  params$PLOT_CONFIDENCE_INTERVALS = F;
  params$Plot_yearly_files = TRUE
  
  path.data.folder = "C:/Users/Mark/Box Sync/Baltic - Stockholm/Results-and-plots-2017-09-08/"
  
  
  switch (batch,
          # "0" = {params$plot.path = "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/North Sea MultiAnnual Plan/ResultsType1-4_220117/Plots/"},
          #"0" = {params$plot.path = "C:/Users/Mark/Desktop/Plots for report/"}
          "0" = {params$plot.path = paste0(path.data.folder, "Plots/")}
          
          # "1" = {params$plot.path = "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/North Sea MultiAnnual Plan/ResultsType1-4_220117/Plots_strats_1-20/"},
          # "2" = {params$plot.path = "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/North Sea MultiAnnual Plan/ResultsType1-4_220117/Plots_strats_21-40/"},
          # "3" = {params$plot.path = "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/North Sea MultiAnnual Plan/ResultsType1-4_220117/Plots_strats_41-end/"}
  )
  #params$RootPath =  "C:/Users/Mark/Desktop/Testing more contrasting HCRs/Results/"
  params$RootPath =  paste0(path.data.folder, "Results/")
  params$hcr.folders = paste0(path.data.folder, "HCRs/")

  params$Base_NYears = 10
  params$Projected_NYears = 19
  params$StartRun_Year = 2004
  
  params$nYrs = params$Base_NYears + params$Projected_NYears
  params$StartProjection_Year = params$StartRun_Year + params$nYrs - params$Projected_NYears
  params$EndRun_Year = params$StartRun_Year + params$nYrs
  
  params$Area = 240000
  
  #Select groups and fleets to plot
  params$Groups2Plot = paste("GroupNo", 1:22, sep="")
  # params$Groups2Plot = c("GroupNo14", "GroupNo16")
  # params$Groups2Plot = c("GroupNo16","GroupNo14","GroupNo18","GroupNo20","GroupNo21","GroupNo23","GroupNo29","GroupNo30",
  #                        "GroupNo31","GroupNo33","GroupNo34","GroupNo38","GroupNo42","GroupNo22","GroupNo26","GroupNo32",
  #                        "GroupNo35","GroupNo39","GroupNo41","GroupNo55")
  #params$Fleets2Plot = c("AllFleets", "FleetNo1")
  params$Fleets2Plot = c("AllFleets", paste("FleetNo", 1:10, sep=""))
  # params$Fleets2Plot = c("AllFleets", "FleetNo1", "FleetNo2", "FleetNo3", "FleetNo4", "FleetNo5", "FleetNo6", "FleetNo7",
  #                        "FleetNo8", "FleetNo9", "FleetNo10", "FleetNo11")
  
  setwd(params$RootPath)
  
  ### Load results files to get unique strats (comment/uncomment to select method of choosing strategies to plot)
  #plot all strats
  # results<-read.table("Results.csv",sep=',',skip=8,col.names=c("Model","Strategy","GroupID","GroupName","Variable","Value"), fill=T)
  # results<-results[results$Strategy!="Z",]#odd one in _SR_final
  # params$strats <- as.character(unique(results$Strategy))  # 10/15 strategies
  params$strats = LoadUniqueStrategies(params$RootPath)
  params$strats <- params$strats[params$strats != "NONE"]
  
  
  #params$strats <- params$strats[41:length(params$strats)]
  #params$strats <- params$strats[21:40]
  switch (batch,
          "1" = {params$strats <- params$strats[1:20]},
          "2" = {params$strats <- params$strats[21:40]},
          "3" = {params$strats <- params$strats[41:length(params$strats)]})
  
  #plot strats in this vector
  #params$strats <- c("14 NSMAP 2020_HighF_Highest value", "11 NSMAP 2020_HighF_Weakest stock")
  #plot strats chosen interactively
  # params$strats <- create_list_strategies(params$RootPath)
  
  
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