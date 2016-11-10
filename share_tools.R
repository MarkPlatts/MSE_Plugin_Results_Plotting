get_timestep_vals = function(plotmonthly, start_year, end_year){
  if (plotmonthly){
    xvals=seq(start_year,end_year-1/12,1/12)
  } else {
    xvals = start_year:(end_year-1)
  }
}

IsIncorrectFileType_YearlyMonthly = function(FileName, plot_yearly){
  #Checks whether the filename given is incorrect given setting to either plot yearly or none yearly values
  
  if(length(grep("Yearly", FileName, fixed=TRUE))>0 & !plot_yearly) return(TRUE)
  if(length(grep("Yearly", FileName, fixed=TRUE))==0 & plot_yearly) return(TRUE)
  return(FALSE)

}

FileIsForACompareGroup = function(params, FILENAME){
  nNotGroups2Compare=0
  for (igroup in params$Groups2Plot){
    if(!length(grep(igroup,FILENAME, fixed=TRUE))>0) nNotGroups2Compare = nNotGroups2Compare+1
  }
  if(length(params$Groups2Plot)==nNotGroups2Compare) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

FileIsForACompareGroupFleet = function(params, FILENAME){
  FILENAME = paste(FILENAME,".csv", sep="")
  for (igroup in params$Groups2Plot){
    if(length(grep(igroup,FILENAME, fixed=TRUE))>0) {
      for (iFleet in params$Fleets2Plot){
        if(iFleet=="AllFleets" && length(grep("AllFleets",FILENAME, fixed=TRUE))>0) {
          #browser()
          return (TRUE)
        }
        if(length(grep(paste(iFleet,".csv", sep=''),FILENAME, fixed=TRUE))>0) {
          #browser()
          return (TRUE)
        }
      }
    }
  }
  return (FALSE)
}

initialise_params = function(params){
  params = list()
  
  params$plot_each_timestep = FALSE;
  params$COMPARE_STRATEGIES = F;
  params$PLOT_CONFIDENCE_INTERVALS = F;
  params$Plot_yearly_files = T
  
  params$plot.path = "C://Users/Mark/Dropbox/GAP2_MSE Plugin2/North Sea MultiAnnual Plan/Yearly_Results HCR type1 and 3/Plots/"
  params$RootPath = "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/North Sea MultiAnnual Plan/Yearly_Results HCR type1 and 3/Results"
  
  params$Base_NYears = 23
  params$Projected_NYears = 20
  params$StartRun_Year = 1991
  
  params$nYrs = params$Base_NYears + params$Projected_NYears
  params$StartProjection_Year = params$StartRun_Year + params$nYrs - params$Projected_NYears
  params$EndRun_Year = params$StartRun_Year + params$nYrs
  
  #Select groups and fleets to plot
  params$Groups2Plot = c("GroupNo16")
  # params$Groups2Plot = c("GroupNo16","GroupNo14","GroupNo18","GroupNo20","GroupNo21","GroupNo23","GroupNo29","GroupNo30",
  #                 "GroupNo31","GroupNo33","GroupNo34","GroupNo38","GroupNo42","GroupNo22","GroupNo26","GroupNo32",
  #                 "GroupNo35","GroupNo39","GroupNo41","GroupNo55")
  params$Fleets2Plot = c("FleetNo1")
  #params$Fleets2Plot = c("AllFleets", "FleetNo1", "FleetNo2", "FleetNo3", "FleetNo4", "FleetNo5", "FleetNo6", "FleetNo7", 
  #                "FleetNo8", "FleetNo9", "FleetNo10", "FleetNo11", "FleetNo12")
  
  setwd(params$Root)
  
  ### Load results files to get unique strats
  results<-read.table("Results.csv",sep=',',skip=8,col.names=c("Model","Strategy","GroupID","GroupName","Variable","Value"), fill=T)
  results<-results[results$Strategy!="Z",]#odd one in _SR_final
  params$strats <- as.character(unique(results$Strategy))  # 10/15 strategies
  params$strats <- params$strats[params$strats != "NONE"] 
  
  params$lineweight = 0.3
  params$legend_x_inset2 = -0.45
  params$COL = rep(1:8,10)[1:length(params$strat)]
  params$LTY<- c(rep(1,8),rep(2,8),rep(3,8),rep(4,8),rep(5,8),rep(6,8))[1:length(params$strats)]
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
  
  return(params)
}
