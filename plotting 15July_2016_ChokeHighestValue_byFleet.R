plot.path <<- "C://Users/Mark/Dropbox/GAP2_MSE Plugin2/North Sea MultiAnnual Plan/Yearly_Results HCR type1 and 3/Plots/"

create.plot.dirs = function(){
  FolderNames = c("OUTPUT_GEARSGROUPSbySTRATEGIES", "OUTPUT_GROUP_FIGS", "OUTPUT_COMPARE_STRATS", 
                  "OUTPUT_GEARSbySTRATEGIES", "OUTPUT_HIGHEST_CHOKE_FIGS", "OUTPUT_percentiles")
  for (iFolder in FolderNames){
    #browser()
    if (!dir.exists(paste(plot.path,iFolder,sep=""))){
      dir.create(paste(plot.path,iFolder,sep=""))
    }
  }
}


plot_type = function(type2plot) {
  
  RootPath = "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/North Sea MultiAnnual Plan/Yearly_Results HCR type1 and 3/Results"
  
  Base_NYears = 23
  Projected_NYears = 20
  StartRun_Year = 1991
  
  nYrs = Base_NYears + Projected_NYears
  StartProjection_Year = StartRun_Year + nYrs - Projected_NYears
  EndRun_Year = StartRun_Year + nYrs
  #Base_NYears = nYrs-Projected_NYears
  
  #Groups2Plot = c("GroupNo34")#, "GroupNo21", "GroupNo27")
  #Groups2Plot = c("GroupNo14", "GroupNo16", "GroupNo18", "GroupNo29", "AllGroups","GroupNo20", "GroupNo55", "GroupNo38", "GroupNo34", "GroupNo31")
  Groups2Plot = c("GroupNo14")
  # Groups2Plot = c("GroupNo16","GroupNo14","GroupNo18","GroupNo20","GroupNo21","GroupNo23","GroupNo29","GroupNo30",
  #                 "GroupNo31","GroupNo33","GroupNo34","GroupNo38","GroupNo42","GroupNo22","GroupNo26","GroupNo32",
  #                 "GroupNo35","GroupNo39","GroupNo41","GroupNo55")
  #Fleets2Plot = c("1","2","3","4","5","6","7","8","9","10","11")
  #Fleets2Plot = c("FleetNo1","FleetNo2","FleetNo4")
  Fleets2Plot = c("AllFleets", "FleetNo1", "FleetNo2", "FleetNo3", "FleetNo4", "FleetNo5", "FleetNo6", "FleetNo7", 
                  "FleetNo8", "FleetNo9", "FleetNo10", "FleetNo11", "FleetNo12")
  
  lineweight = 0.6
  
  setwd(RootPath)
  
  ### RESULTS FILES
  results<-read.table("Results.csv",sep=',',skip=8,col.names=c("Model","Strategy","GroupID","GroupName","Variable","Value"), fill=T)
  head(results)
  results<-results[results$Strategy!="Z",]#odd one in _SR_final
  strat <- as.character(unique(results$Strategy))  # 10/15 strategies
  strat <- strat[strat != "NONE"] 
  mods  <- (unique(results$Model))                  # 294 models
  gnames<- as.character(unique(results$GroupName))# 77 groups: 1=Baleen whales, 65=Phytoplankton. 66-77 fleets, n-1 AllGroups, n AllFleets
  #gnames<- as.character(results$GroupName[c(14,18,16,29,31)])
  gnames<- c(gnames, "AllGroups", "AllFleets")
  vars  <- as.character(unique(results$Variable)) # 6 Variables: "BiomassMin" "BiomassEnd" "Landings" "DiscardMortalities" "DiscardSurvivals" "TotalEndValue"
  
  STRATS<- strat
  
  ###Trajectories by MODEL run
  m<-mods[1]
  #read a model
  #setwd(paste(RootPath,"\\Biomass",sep=''))
  biomass.filenames = list.files("Biomass")
  
  path<-paste("Biomass\\",biomass.filenames[m],sep='')
  #mod<-read.csv(path,skip=6, head=T, fill=T)              #monthly time-series
  mod<-read.table(path, skip=7, header = TRUE, fill = TRUE,sep=",",as.is =T)
  nyrs <- (ncol(mod)-4)/12 # num yrs
  
  #take only january values
  mod<-mod[,c(1:4,4+seq(1,nyrs*12,12))]          #dec values would be  mod[,3+seq(12,nyrs*12,12)]
  mod[62:67,1:5] #inspect
  mod[1:2,1:5] #info
  
  #### mortalities
  #setwd("Y:\\WP1 Tradeoffs in mixed fisheries\\NSea Multiannual Plan\\Evaluation\\STECF Results 1000_30 years_SR_final\\")
  #setwd("Y:\\WP1 Tradeoffs in mixed fisheries\\NSea Multiannual Plan\\Evaluation\\STECF Results 1000_30 years_SR\\")
  #setwd("RootPath"); g <- list.files("Trajectories2"); STRATS<- c("HCR_HighF_Highest value","HCR_LowF_Highest value")
  MORT_HCRF_Cons <- F
  MORT_HCRF_Targ<- F
  QUOTA_HCRF_Cons<- F
  QUOTA_HCRF_Targ<- F
  MORT_REAL_LandF <- F
  MORT_REAL_DiscF <- F
  MORT_REAL_F <- F
  
  CATCH <-F
  LANDING <- F
  DISCARD <- F
  
  EFFORT <-F
  VALUE <- F
  BIOMASS <-F

  HIGHEST_VALUE <- F
  CHOKE_GROUPS <- F
  
  if(type2plot==1){
    MORT_HCRF_Cons <- T;  if(MORT_HCRF_Cons) {setwd("HCRF_Cons");YLAB<-"F";print("Plotting MORT_HCRF_Cons")} # what are these?
  }
  if(type2plot==2){
    MORT_HCRF_Targ<- T;   if(MORT_HCRF_Targ) {setwd("HCRF_Targ");YLAB<-"F";print("Plotting HCRF_Targ")} # what are these?
  }
  if(type2plot==3){
    QUOTA_HCRF_Cons<- T;  if(QUOTA_HCRF_Cons) {setwd("HCRQuota_Cons");YLAB<-"t/km2";print("Plotting HCRQuota_Cons")} # what are these?
  }
  if(type2plot==4){
    QUOTA_HCRF_Targ<- T;  if(QUOTA_HCRF_Targ) {setwd("HCRQuota_Targ");YLAB<-"t";print("Plotting HCRQuota_Targ")} # what are these?
  }
  if(type2plot==5){
    MORT_REAL_LandF <- T; if(MORT_REAL_LandF) {setwd("RealisedLandedF");YLAB<-"F";print("Plotting MORT_REAL_LandF")}
  }
  if(type2plot==6){
    MORT_REAL_DiscF <- T; if(MORT_REAL_DiscF) {setwd("RealisedDiscardedF");YLAB<-"F";print("Plotting MORT_REAL_DiscF")}
  }
  if(type2plot==7){
    MORT_REAL_F <- T;     if(MORT_REAL_F) {setwd("RealisedF");YLAB<-"F";print("Plotting MORT_REAL_F")}
  }
  #YLAB<-"catch (t)"
  if(type2plot==8){
    CATCH <-T;    if(CATCH) {setwd("CatchTrajectories"); YLAB<-"catch (t/year)";print("Plotting CATCH")}
  }
  if(type2plot==9){
    LANDING <- T; if(LANDING) {setwd("LandingsTrajectories"); YLAB<-"landings (t/year)";print("Plotting LANDING")}
  }
  if(type2plot==10){
    DISCARD <- T; if(DISCARD) {setwd("DiscardsTrajectories"); YLAB<-"discards (t/year)";print("Plotting DISCARD")}
  }
  
  if(type2plot==11){
    EFFORT <-T;print("Plotting EFFORT")
  }
  if(type2plot==12){
    VALUE <- T;print("Plotting VALUE")
  }
  if(type2plot==13){
    BIOMASS <-T;print("Plotting BIOMASS")
  }
  
  if(type2plot==14){
    HIGHEST_VALUE <- T;print("Plotting HIGHEST_VALUE")
  }
  if(type2plot==15){
    CHOKE_GROUPS <- T;print("Plotting CHOKE_GROUPS")
  }
  
  plot_each_timestep = FALSE;
  
  COMPARE_STRATEGIES = F;
  
  PLOT_CONFIDENCE_INTERVALS = F;
  
  Plot_yearly_files = T
  
  FileIsForACompareGroup = function(Groups2Plot, FILENAME){
    nNotGroups2Compare=0
    for (igroup in Groups2Plot){
      if(!length(grep(igroup,FILENAME, fixed=TRUE))>0) nNotGroups2Compare = nNotGroups2Compare+1
    }
    if(length(Groups2Plot)==nNotGroups2Compare) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  }
  
  FileIsForACompareGroupFleet = function(Groups2Plot, Fleets2Plot, FILENAME){
    FILENAME = paste(FILENAME,".csv", sep="")
    for (igroup in Groups2Plot){
      if(length(grep(igroup,FILENAME, fixed=TRUE))>0) {
        for (iFleet in Fleets2Plot){
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
  
  
  if (COMPARE_STRATEGIES){
    print(strat)
    print("")
    strat1 <- readline(prompt="Select by vector index the 1st Strategy you want to compare:")
    strat2 <- readline(prompt="Select by vector index the 2nd Strategy you want to compare:")
    strat1name = strat[strtoi(strat1)]
    strat2name = strat[strtoi(strat2)]
    print(strat1name)
    print(strat2name)
    #Groups2Plot = c(14,16,18,29, 78)
    #Groups2Plot = 14
    #Fleets2Plot = c(67:77, 79)
    #Fleets2Plot = c(67:68)
    print("Comparing for groups: ")
    print(Groups2Plot)
    print("Compare for fleets: ")
    print(Fleets2Plot)
  }
  
  ###F Trajectories
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  if(any(MORT_REAL_F,MORT_REAL_LandF,MORT_REAL_DiscF,MORT_HCRF_Cons,MORT_HCRF_Targ,QUOTA_HCRF_Cons,QUOTA_HCRF_Targ,CATCH,DISCARD,LANDING)){
    
    g <- list.files()     # which groups are there?
    # g[1:12] All groups 12 fleets
    #g <- g[-c(1:12)]
    #remove those with a comma in title as issues
    #substr(g)
    gnum <- 6; FILENAME <- substr(g[gnum],1,nchar(g[gnum])-4); FILENAME
    groupdat <- read.csv(g[gnum],skip=7, head=T)
    PlotStart_Year = StartProjection_Year
    nYrs = Projected_NYears
    names(groupdat)[names(groupdat)=="StrategyName"]  <- "Strategy"
    if(any(MORT_REAL_F,MORT_REAL_LandF,MORT_REAL_DiscF)){
      if(plot_each_timestep==F && !Plot_yearly_files){
        browser()
        groupdat<-groupdat[,c(1:4,4+seq(1,nYrs*12,12))]
      }
    }
    # if(any(CATCH,DISCARD,LANDING)){ 
    #   #WORK OUT AVE ANN CATCH
    #   if (!plot_each_timestep && !Plot_yearly_files)
    #     {
    #     AVEgroupdat <-   groupdat[,c(1,2,3,4,5,5+seq(1,(nYrs)*12,12))]
    #     for(i in 1:11) AVEgroupdat[,6:ncol(AVEgroupdat)] <- AVEgroupdat[,6:ncol(AVEgroupdat)] + groupdat[,c(5+seq(1,(nyrs-Base_NYears)*12,12))+i] #jan
    #     #take JANS
    #     }
    #   if (!plot_each_timestep && Plot_yearly_files){AVEgroupdat <-   groupdat[,c(1,2,3,4,5,5+seq(1,(nYrs),1))]}
    # 
    #   if (plot_each_timestep==F && !Plot_yearly_files) groupdat<-groupdat[,c(1,2,3,4,5,5+seq(1,Projected_NYears*12,12))]
    #   #overwrite with AVERAGE
    #   AVEgroupdat[,6:ncol(AVEgroupdat)] <- AVEgroupdat[,6:ncol(AVEgroupdat)]/12#average yr # rbind(3+seq(1,(nyrs-Base_NYears)*12,12) ,(2+seq(1,(nyrs-Base_NYears)*12,12)+12))  jan@dec
    # }
    
    if(any(MORT_HCRF_Cons,MORT_HCRF_Targ)){
      groupdat <- groupdat[,c(1,2,3,4,4+seq(1,Projected_NYears,1))]
    }
    if(any(QUOTA_HCRF_Cons,QUOTA_HCRF_Targ)){                               
      groupdat <- groupdat[,c(1:5,5+seq(1,Projected_NYears,1))]
    }
    
    #take jans   RealisedF is just forecast
    
    groupdat[62:66,1:7] #inspect
    groupdat[1:5,1:7] #info
    
    #plot group above for strategies
    strat_i <-6     #strat[1] is "1 CFP_2015 TargetF_Weakest stock"
    if(any(MORT_HCRF_Cons,MORT_HCRF_Targ)){
      data2plot<- groupdat[groupdat$Strategy %in% STRATS[strat_i],5:ncol(groupdat)]
    }
    if(any(CATCH,DISCARD,LANDING,QUOTA_HCRF_Cons,QUOTA_HCRF_Targ)){
      data2plot<- groupdat[groupdat$Strategy %in% STRATS[strat_i],6:ncol(groupdat)]
    }
    
    if(any(MORT_REAL_F,MORT_REAL_LandF,MORT_REAL_DiscF)){ 
      data2plot<- groupdat[groupdat$Strategy %in% STRATS[strat_i],5:ncol(groupdat)]
    }
    
    #set the x axis values depending on what result type and whether plotting yearly or montly
    if(any(QUOTA_HCRF_Cons,QUOTA_HCRF_Targ,MORT_HCRF_Cons,MORT_HCRF_Targ)){
      xvals=PlotStart_Year:(EndRun_Year-1)
    } else {
      if(plot_each_timestep==T) {xvals = seq(PlotStart_Year,EndRun_Year-1/12,1/12)} else {xvals=PlotStart_Year:(EndRun_Year-1)}
    }
    
    #colourful plot
    # plot mean line
    ymax = max(data2plot)
    #browser()
    plot(xvals,apply(data2plot,2,mean),type='l',lwd=2, ylim=c(0,ymax),
         main=paste(FILENAME,STRATS[strat_i],sep=' '),xlab='years',ylab="")
    #     if(MORT_REAL_F) TITLE="Realised F"
    #     if(MORT_REAL_LandF) TITLE="Landed F"
    #     if(MORT_REAL_DiscF) TITLE="Discarded F"
    #     if(CATCH) TITLE="Realised catch"
    #     if(LANDING) TITLE="Landed catch"
    #     if(DISCARD) TITLE="Discarded catch"
    TITLE = FILENAME
    title(TITLE,line=-1,cex.main=0.8)
    # add all sims
    
    for(s in 1:nrow(data2plot)) lines(xvals,data2plot[s,],lwd=1,col=s)
    
    
    #all trajectories        
    WRITE<-F
    SAVE<-T
    graphics.off()#par(mfrow=c(5,6),mar=c(2,2,4,2),oma=c(1,1,3,1))
    #if(length(strat)==10) COL<-c(1:6,9:12)     # for line types and colours #skip yellow 7
    #if(length(strat)==22) COL<-c(1:6,8:12,1:6,8:12) # have more now!
    COL = rep(seq(1,8,1),8)[1:length(strat)]
    LTY<- c(rep(1,8),rep(2,8),rep(3,8),rep(4,8),rep(5,8),rep(6,8))[1:length(strat)]; #LTY[1:6] <- COL[6:1]
    LEGEND<-T

    if(!SAVE) pdf(file =paste(plot.path,"\\OUTPUT_GROUP_FIGS\\",TITLE," plots by group and strategy.pdf",sep=""),width=14,height=7,paper="a4r")

    for(G in g){
      FILENAME <- substr(G,1,nchar(G)-4)
      print(FILENAME)
      
      #stopifnot(FILENAME!="HCR_F_Cons_Nephrops_GroupsNo55") 
      if (any(MORT_REAL_F,MORT_REAL_LandF,MORT_REAL_DiscF, MORT_HCRF_Cons,MORT_HCRF_Targ)){
        if(COMPARE_STRATEGIES && !FileIsForACompareGroup(Groups2Plot, FILENAME)) next
      } else if (any(CATCH,DISCARD,LANDING,QUOTA_HCRF_Cons,QUOTA_HCRF_Targ)) {
        if (COMPARE_STRATEGIES && !FileIsForACompareGroupFleet(Groups2Plot, Fleets2Plot, FILENAME)) next
      }
      
      if(length(grep("Yearly", FILENAME, fixed=TRUE))==1 && !Plot_yearly_files) next
      
      # GroupsNotPlot = c("Planktonic", "Seals_Grou","Small mobi", "Benthic mi", "Juvenile s","Juvenile W","Juvenile H","Juvenile S","Juvenile C","Herring (j")
      # for(iGroup in GroupsNotPlot){
      #   if (length(grep(iGroup, FILENAME, fixed=TRUE))==1) next
      # }
      #Checks whether the fleet and group of this file has been chosen at top of script to be plotted

      DontPlot = TRUE
      for(iGroup in Groups2Plot){
        if (length(grep(paste("_",iGroup, sep=''), G, fixed=TRUE))==1) {
          DontPlot=FALSE
          break
        }
      }
      if (DontPlot==TRUE) next
      
      if(any(QUOTA_HCRF_Cons,QUOTA_HCRF_Targ,CATCH,DISCARD,LANDING)){
        #Checks whether the fleet of this file has been chosen at top of script to be plotted
        DontPlot = TRUE
        for(iFleet in Fleets2Plot){
          if (length(grep(paste(iFleet,".csv", sep=''), G, fixed=TRUE))==1) {
            DontPlot=FALSE
            break
          }
        }
        if (DontPlot==TRUE) next
      }
      
      #if(substr(FILENAME,12,21) %in% c("Planktonic", "Seals_Grou","Small mobi", "Benthic mi", "Juvenile s","Juvenile W","Juvenile H","Juvenile S","Juvenile C","Herring (j")) next
      #c("Planktonic microflora (incl. Bacteria, protozoa)_GroupNo64","Juvenile sharks_GroupNo5", "Seals_GroupNo3",     "Small mobile epifauna (swarming crustaceans)_GroupNo59",                       "Benthic microflora (incl. Bacteria, protozoa))_GroupNo63", "Juvenile Whiting (0-1, 0-20cm)_GroupNo15",          "Juvenile Haddock (0-1, 0-20cm)_GroupNo17","Juvenile Saithe (0-3, 0-40cm)_GroupNo19","Juvenile Cod(0-2, 0-40cm)_GroupNo13","Herring (juvenile 0, 1)_GroupNo28")) next
      
      groupdat <- read.table(file=paste(G,sep=''),skip=7, header = TRUE, fill = TRUE,sep=",",as.is =T)
      #browser()
      #if all values in the file are -9999 then we need to skip plotting it
      if(any(MORT_HCRF_Cons,MORT_HCRF_Targ)){
        testvaliddata<- groupdat[, 5:ncol(groupdat)]
        if(length(as.matrix(testvaliddata))*-9999==sum(testvaliddata)) {next}
      }
      
      if(any(MORT_REAL_F,MORT_REAL_LandF,MORT_REAL_DiscF)){
        testvaliddata<- groupdat[, 5:ncol(groupdat)]
        if(sum(testvaliddata)==0) {next}
      }

      if(any(QUOTA_HCRF_Cons,QUOTA_HCRF_Targ)){
        testvaliddata<- groupdat[, 6:ncol(groupdat)]
        if(length(as.matrix(testvaliddata))*-9999==sum(testvaliddata)) {next}
      }
      if (SAVE) {
        if(!COMPARE_STRATEGIES) {
          if(any(QUOTA_HCRF_Cons,QUOTA_HCRF_Targ,CATCH,DISCARD,LANDING)){
            png(filename = paste(plot.path,"\\OUTPUT_GEARSGROUPSbySTRATEGIES\\",FILENAME,"_.png",sep=""), res=900, width=10, height=4, units='in')              
          } else {
            png(filename = paste(plot.path,"\\OUTPUT_GROUP_FIGS\\",FILENAME,"_.png",sep=""), res=900, width=10, height=4, units='in')              
          }
        } else {
          png(filename = paste(plot.path,"\\OUTPUT_COMPARE_STRATS\\",FILENAME,"_COMP.png",sep=""), res=900, width=10, height=4, units='in')
        }
      }

      
      print(paste("The number of open devices is",length(dev.list())))
      #if(length(dev.list())>1) browser()
      
      if(any(MORT_REAL_F,MORT_REAL_LandF,MORT_REAL_DiscF)){
        groupdat <- groupdat[,-which(names(groupdat)=="ResultType")]
        if(plot_each_timestep==F && !Plot_yearly_files){
          groupdat<-groupdat[,c(2:3,3+seq(1,Projected_NYears*12,12))] # TAKE JANS
        } else if(plot_each_timestep && !Plot_yearly_files) {
          groupdat<-groupdat[,c(2:(3+Projected_NYears*12))]
        } else {
          groupdat<-groupdat[,c(2:(3+Projected_NYears))]
        }
        
      }
      
      if(any(MORT_HCRF_Cons,MORT_HCRF_Targ)){
        groupdat <- groupdat[,-which(names(groupdat)=="ResultType")]
        groupdat<-groupdat[,2:ncol(groupdat)] # TAKE JANS
      }
      
      if(any(QUOTA_HCRF_Cons,QUOTA_HCRF_Targ)){
        groupdat <- groupdat[,-which(names(groupdat)=="ResultType")]
        groupdat<-groupdat[,2:ncol(groupdat)]
      }
      
      if(any(CATCH,DISCARD,LANDING)){
        #missing line data! arrgh
        #groupdat<-groupdat[-nrow(groupdat),]
        #for(i in 5:ncol(groupdat)) groupdat[,i] <- as.numeric(groupdat[,i])#
        #groupdat[,(5:ncol(groupdat))] <- as.matrix(groupdat[,(5:ncol(groupdat))])#use as.is instead
        
        #groupdat <- groupdat[,-which(names(groupdat)=="CatchType")]
        #if(any(names(groupdat)=="FleetName")) groupdat <- groupdat[,-which(names(groupdat)=="FleetName")]
        groupdat[,6:ncol(groupdat)] <- groupdat[,6:ncol(groupdat)]
        
        #WORK OUT AVE ANN CATCH
        # AVEgroupdat <-   groupdat[,c(1:5,5+seq(1,Projected_NYears*12,12))]
        # for(i in 1:11) AVEgroupdat[,6:ncol(AVEgroupdat)] <- AVEgroupdat[,6:ncol(AVEgroupdat)] + groupdat[,c(5+seq(1,Projected_NYears*12,12))+i] #jan
        # #take JANS
        # 
        # #overwrite with AVERAGE
        # AVEgroupdat[,6:ncol(AVEgroupdat)] <- AVEgroupdat[,6:ncol(AVEgroupdat)]/12 #average yr # rbind(3+seq(1,(nyrs-Base_NYears)*12,12) ,(2+seq(1,(nyrs-Base_NYears)*12,12)+12))  jan@dec
        # now t not t/km2
        if(plot_each_timestep==F && !Plot_yearly_files){
          groupdat<-groupdat[,c(1:5,5+seq(1,Projected_NYears*12,12))]
        } else if (plot_each_timestep && !Plot_yearly_files){
          groupdat<-groupdat[,c(1:(5+Projected_NYears*12))]
        } else {
          groupdat<-groupdat[,c(1:(5+Projected_NYears))]
        }
        
      }
      
      
      
      #This code converts the data into percentiles and the mean and only these values are plotted and saved
      PERCS<-MDNS<- LOWS<- UPPS<- MEANS<- data.frame(year=xvals,row.names =xvals)
      
      for(strat_i in 1:length(STRATS)){
        
        STRAT<-paste(STRATS[strat_i],sep=' ')
        
        if(COMPARE_STRATEGIES){
          if(strat1name!=STRAT && strat2name!=STRAT) {
            next
          }
        }

        
        #select subset of data
        if(any(CATCH,DISCARD,LANDING)){
          data2plot<- groupdat[groupdat$Strategy %in% STRAT,6:ncol(groupdat)] * 570000
        }
        
        if(any(MORT_HCRF_Cons,MORT_HCRF_Targ)){
          data2plot<- groupdat[groupdat$Strategy %in% STRAT, 3:ncol(groupdat)]
          if(length(data2plot)*-9999==sum(data2plot)) {
            next
          }
        }
        
        if(any(MORT_REAL_F,MORT_REAL_LandF,MORT_REAL_DiscF)){
          data2plot<- groupdat[groupdat$Strategy %in% STRAT, 3:ncol(groupdat)]
        }
        
        if(any(QUOTA_HCRF_Cons,QUOTA_HCRF_Targ)){
          data2plot<- groupdat[groupdat$Strategy %in% STRAT, 4:ncol(groupdat)] * 570000
        }
        
        if((nrow(data2plot)==0)) next
        #quantiles for polygon plot
        perc<-apply(data2plot,2, FUN=function(x){quantile(x,probs=c(0.025,0.5,0.975),na.rm=T)})
        perc<-rbind(perc, apply(data2plot,2, FUN=mean) )
        #save percs
        LOWS<- cbind(LOWS,perc[1,]);   names(LOWS)[ncol(LOWS)]<-STRAT
        MDNS<- cbind(MDNS,perc[2,]);   names(MDNS)[ncol(MDNS)]<-STRAT
        UPPS<- cbind(UPPS,perc[3,]);   names(UPPS)[ncol(UPPS)]<-STRAT
        MEANS<- cbind(MEANS,perc[4,]); names(MEANS)[ncol(MEANS)]<-STRAT
        
        PERC<-data.frame(t(perc))
        names(PERC) <- c(paste(STRAT,"LOW"),paste(STRAT,"MDN"),paste(STRAT,"UPP"),paste(STRAT,"MEAN"))
        PERCS<- cbind(PERCS, PERC)
      }
      
      
      #now summary plot
      if(ncol(MEANS)==1) {
        graphics.off()
        next
      }
      if(sum(MEANS[,-1],na.rm=T)==0 & !any(MORT_REAL_F,MORT_REAL_LandF,MORT_REAL_DiscF) ) {
        graphics.off()
        next
      }
      #if(plot_each_timestep==T) {xvals = seq(PlotStart_Year,EndRun_Year-1/12,1/12)} else {xvals=PlotStart_Year:(EndRun_Year-1)}
      par(mar=c(5.1, 4.1, 4.1, 15), xpd=TRUE)
      

      
      if(PLOT_CONFIDENCE_INTERVALS){
        plot(xvals,MEANS[,2],type='l',ylim=c(0,1.25*(max(MEANS[,-1],UPPS[,-1],na.rm=T))),lty=LTY[1],col=COL[1],ylab=YLAB,xlab="year",font=20,lwd=lineweight)
        for(i in 3:ncol(MEANS)) {
          lines(xvals,MEANS[,i],lty=LTY[(i-1)],col=COL[(i-1)],lwd=lineweight)
        }
        for(i in 2:ncol(LOWS)) {
          lines(xvals,LOWS[,i],lty=LTY[(i)],col=COL[(i-1)],lwd=lineweight*0.5)
          lines(xvals,UPPS[,i],lty=LTY[(i)],col=COL[(i-1)],lwd=lineweight*0.5)
        }
      } else {
        plot(xvals,MEANS[,2],type='l',ylim=c(0,1.25*(max(MEANS[,-1],na.rm = T))),lty=LTY[1],col=COL[1],ylab=YLAB,xlab="year",font=20,lwd=lineweight)
        for(i in 3:ncol(MEANS)) {
          lines(xvals,MEANS[,i],lty=LTY[(i-1)],col=COL[(i-1)],lwd=lineweight)
        }
      }
      
      #       plot(xvals,MEANS[,2],type='l',ylim=c(min(MEANS[,-1],na.rm=T)*.75,1.25*(max(MEANS[,-1],na.rm=T))),lty=LTY[1],col=COL[1],ylab=YLAB,xlab="year",font=20,lwd=lineweight)
      #       for(i in 3:ncol(MEANS)) lines(xvals,MEANS[,i],lty=LTY[(i-1)],col=COL[(i-1)],lwd=lineweight)
      
      if(any(MORT_REAL_F,MORT_REAL_LandF,MORT_REAL_DiscF)) title(c("F trajectory (mean) by strategy",FILENAME),font.main=20)#only individual plots   
      if(any(CATCH,DISCARD,LANDING))   title(c("Catch (mean) by strategy",FILENAME),font.main=20)
      if(any(MORT_HCRF_Cons,MORT_HCRF_Targ,QUOTA_HCRF_Cons,QUOTA_HCRF_Targ)) title(FILENAME,font.main=20)
      
      #title(TITLE,line=-1,cex.main=0.8)
      if(LEGEND){
        if(COMPARE_STRATEGIES){
          legend('bottomright',c(strat1name,strat2name),col = COL,lty =LTY,inset=c(-0.45,0),pt.cex = 1,cex=0.5,lwd=1,text.font=20)
        } else {
          legend('bottomright',STRATS,col = COL,lty =LTY,inset=c(-0.45,0),pt.cex = 1,cex=0.5,lwd=1,text.font=20)
        }
      }
      
      #if(SAVE) savePlot(paste(RootPath,"\\OUTPUT_GROUP_FIGS\\",FILENAME,TITLE,".pdf",sep=""),type='pdf')
      if (any(MORT_REAL_F,MORT_REAL_LandF,MORT_REAL_DiscF)){
        if(WRITE) write.csv(PERCS[,-1],paste(plot.path,"\\OUTPUT_FcatchBySTRATEGIES\\",FILENAME,".csv",sep=""))
      } else if (any(CATCH,DISCARD,LANDING)) {
        
      } else if (any(MORT_HCRF_Cons,MORT_HCRF_Targ, QUOTA_HCRF_Cons,QUOTA_HCRF_Targ)) {
        
      }
      
      graphics.off()
    }#next G
    if(!SAVE) dev.off()
    #setwd("Y:\\WP1 Tradeoffs in mixed fisheries\\NSea Multiannual Plan\\Evaluation\\STECF Results 1000_30 years_SR_final\\")
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  
  if (BIOMASS){
    #Load up biomass reference file
    biom_refs = read.csv(paste(plot.path,"/Biom_refs.csv",sep=''))
    
    ###Group Trajectories
    
    #setwd("P:\\WP1 Tradeoffs in mixed fisheries\\NSea Multiannual Plan\\Evaluation\\STECF Results 1000_30 years_SR\\")
    setwd("Biomass")
    #now read
    g <- list.files()     # which groups are there?
    gnum <- 37    #g[6] is "Cod (adult)_GroupNo14.csv"
    FILENAME <- substr(g[gnum],1,nchar(g[gnum])-4)
    #groupdat <- read.csv(g[gnum],skip=7, header=T)
    groupdat<-read.table(g[gnum], skip=7, header = TRUE, fill = TRUE,sep=",",as.is =T)
    setwd(RootPath)
    nyrs <- (ncol(groupdat)-4)/12 # num yrs
    Start <- StartRun_Year
    End <- EndRun_Year
    
    #setwd("Y:\\WP1 Tradeoffs in mixed fisheries\\NSea Multiannual Plan\\Evaluation\\STECF Results 1000_30 years_SR\\")
    # Model Strategy  X1...months
    #take jans   BIOMASS is 1991+forecast, RealisedF is just forecast
    if (!plot_each_timestep && !Plot_yearly_files) {groupdat<-groupdat[,c(1:4,4+seq(1,nyrs*12,12))]}          #dec values would be  groupdat[,3+seq(12,nyrs*12,12)]
    if (plot_each_timestep && !Plot_yearly_files) {groupdat<-groupdat[,c(1:4,4+seq(1,nyrs*12,1))]}
    groupdat[,-c(1:4)] <- groupdat[,-c(1:4)]*570000
    groupdat <- groupdat[,-which(names(groupdat)=="ResultType")]
    groupdat <- groupdat[,-which(names(groupdat)=="GroupName")]
    groupdat <- groupdat[,-which(names(groupdat)=="ModelID")]
    names(groupdat)[names(groupdat)=="StrategyName"]  <- "Strategy"
    
    groupdat[62:66,1:5] #inspect
    groupdat[1:5,1:5] #info
    
    #plot group above for strategies
    strat_i <-1     #strat[1] is "CFP_TargetF_Highest value"
    data2plot<- groupdat[groupdat$Strategy %in% strat[strat_i],2:ncol(groupdat)]
    ymax = max(data2plot)
    #colourful plot
    # plot mean line
    #browser()
    if (plot_each_timestep){
      xvals=seq(Start,End-1/12,1/12)
    } else {
      xvals = Start:(End-1)
    }
    plot(xvals,apply(data2plot,2,mean),type='l',lwd=2,ylim=c(0,ymax*1.2),
         main=paste(FILENAME,strat[strat_i],sep=' '),xlab='years',ylab="biomass (t)")
    # add all sims
    for(s in 1:nrow(data2plot)) lines(xvals,data2plot[s,],lwd=1,col=s)  
    
    #biomass trajectories
    graphics.off() #par(mfrow=c(5,6),mar=c(2,2,4,2),oma=c(1,1,3,1))
    
    ## Strategies to plot
    #3          CFP_FIXEDTargetF_Highest value
    #5          CFP_FIXEDTargetF_Weakest stock
    #8          HCR_HighF_Weakest stock
    #9          HCR_LowF_Weakest stock
    #10        HCR_TargetF_Highest value
    #12        HCR_TargetF_Weakest stock
    STRATS<- strat
    #STRATS<- strat[c(3,5,8,9,10,12)]
    #all biomass trajectories
    WRITE<-F
    SAVE<-T
    graphics.off()#par(mfrow=c(5,6),mar=c(2,2,4,2),oma=c(1,1,3,1))
    COL = rep(1:8,10)[1:length(strat)]
    LTY<- c(rep(1,8),rep(2,8),rep(3,8),rep(4,8),rep(5,8),rep(6,8))[1:length(strat)]; #LTY[1:6] <- COL[6:1]
    LEGEND<-T
    strat <- as.character(unique(results$Strategy))  # 10 strategies
    setwd(RootPath)
    #setwd("Y:\\WP1 Tradeoffs in mixed fisheries\\NSea Multiannual Plan\\Evaluation\\STECF Results 1000_30 years_SR_additionalruns\\"); g <- list.files("Trajectories2"); STRATS<- c("HCR_HighF_Highest value","HCR_LowF_Highest value")
    if(!SAVE) pdf(file =paste("OUTPUT_GROUP_FIGS//meanbio plots by group and strategy.pdf",sep=""),width=14,height=7,paper="a4r")
    for(G in g){
      
      FILENAME <- substr(G,1,nchar(G)-4)
      if(length(grep("Yearly", FILENAME, fixed=TRUE))==1 && !Plot_yearly_files) next
      
      #if (COMPARE_STRATEGIES && !FileIsForACompareGroup(Groups2Plot, FILENAME)) next
      
      #GroupsNotPlot = c("Planktonic", "Seals_Grou","Small mobi", "Benthic mi", "Juvenile s","Juvenile W","Juvenile H","Juvenile S","Juvenile C","Herring (j")
      
      #Checks whether the group has been listed at the top of the script to plot
      DontPlot = TRUE
      for(iGroup in Groups2Plot){
        if (length(grep(paste(iGroup,".csv", sep=''), G, fixed=TRUE))==1) {
          DontPlot=FALSE
          break
        }
      }
      if (DontPlot==TRUE) next
      
      groupdat <- read.csv(paste("Biomass\\",G,sep=''),skip=7, head=T)
      GroupName = groupdat[1,1]
      
      
      if (SAVE) {
        if(!COMPARE_STRATEGIES) {
          png(filename = paste(plot.path,"\\OUTPUT_GROUP_FIGS\\",FILENAME,"_PERCS.png",sep=""), res=900, width=8, height=4, units='in')
        } else {
          png(filename = paste(plot.path,"\\OUTPUT_COMPARE_STRATS\\",FILENAME,"_COMP.png",sep=""), res=900, width=8, height=4, units='in')
        }
      }
      
      if(!plot_each_timestep && !Plot_yearly_files){
        groupdat<-groupdat[,c(1:4,4+seq(1,nyrs*12,12))] 
      } else if (plot_each_timestep && !Plot_yearly_files){
        groupdat<-groupdat[,c(1:4,4+seq(1,nyrs*12,1))] 
      }
      #dec values would be  groupdat[,3+seq(12,nyrs*12,12)]
      groupdat[,-c(1:4)] <- groupdat[,-c(1:4)]*570000
      groupdat <- groupdat[,-which(names(groupdat)=="ResultType")]
      groupdat <- groupdat[,-which(names(groupdat)=="GroupName")]
      groupdat <- groupdat[,-which(names(groupdat)=="ModelID")]
      
      PERCS<-MDNS<- LOWS<- UPPS<- MEANS<- data.frame(year=xvals,row.names =xvals)
      for(strat_i in 1:length(STRATS)){
        
        STRAT<-paste(STRATS[strat_i],sep=' ')
        
        if(COMPARE_STRATEGIES){
          if(strat1name!=STRAT && strat2name!=STRAT) next
        }
        
        #select subset of data
        #browser()
        data2plot<- groupdat[groupdat$Strategy %in% STRAT,2:ncol(groupdat)]
        
        #quantiles for polygon plot
        perc<-apply(data2plot,2, FUN=function(x){quantile(x,probs=c(0.025,0.5,0.975),na.rm=T)})
        perc<-rbind(perc, apply(data2plot,2, FUN=mean) )
        
        #save percs
        LOWS<- cbind(LOWS,perc[1,]);   names(LOWS)[ncol(LOWS)]<-STRAT
        MDNS<- cbind(MDNS,perc[2,]);   names(MDNS)[ncol(MDNS)]<-STRAT
        UPPS<- cbind(UPPS,perc[3,]);   names(UPPS)[ncol(UPPS)]<-STRAT
        MEANS<- cbind(MEANS,perc[4,]);   names(MEANS)[ncol(MEANS)]<-STRAT
        
        PERC<-data.frame(t(perc))
        names(PERC) <- c(paste(STRAT,"LOW"),paste(STRAT,"MDN"),paste(STRAT,"UPP"),paste(STRAT,"MEAN"))
        PERCS<- cbind(PERCS, PERC)
      }

      #plot the reference points
      #read_biom_refs = function(biom_refs, group, ref_type)
      bpa = read_biom_refs(biom_refs, GroupName, "bpa") * 570
      blim = read_biom_refs(biom_refs, GroupName, "blim") * 570

      #now summary plot
      #par(mar=c(5.1, 4.1, 4.1, 20), xpd=TRUE)
      par(mar=c(5.1, 4.1, 4.1, 16), xpd=TRUE)
      
      #figure out what the highest value the y-axis needs to be
      if(!is.na(bpa)){
        y_upper = max(MEANS[,-1],UPPS[,-1],bpa)
      } else {
        y_upper = max(MEANS[,-1],UPPS[,-1])
      }
      
      if(PLOT_CONFIDENCE_INTERVALS){
        plot(xvals,MEANS[,2],type='l',ylim=c(0,1.25*y_upper),lty=LTY[1],col=COL[1],ylab="relative biomass (t)",xlab="year",font=20,lwd=lineweight)
        for(i in 3:ncol(MEANS)) {
          lines(xvals,MEANS[,i],lty=LTY[(i-1)],col=COL[(i-1)],lwd=lineweight)
        }
        for(i in 2:(ncol(LOWS)-1)) {
          #browser()
          lines(xvals,LOWS[,i],lty=LTY[(i)],col=COL[(i-1)],lwd=lineweight*0.5)
          lines(xvals,UPPS[,i],lty=LTY[(i)],col=COL[(i-1)],lwd=lineweight*0.5)
        }
      } else {
        plot(xvals,MEANS[,2],type='l',ylim=c(0,1.25*y_upper),lty=LTY[1],col=COL[1],ylab="relative biomass (t)",xlab="year",font=20,lwd=lineweight)
        for(i in 2:ncol(MEANS)) {
          lines(xvals,MEANS[,i],lty=LTY[(i-1)],col=COL[(i-1)],lwd=lineweight)
        }
      }

      #plot reference levels
      if(!is.na(bpa)){
        lines(c(xvals[1],xvals[length(xvals)]),c(bpa,bpa),col=1,lwd=0.5, lty=3)
        text(xvals[1]+1, bpa+0.05*y_upper, "Bpa", cex=0.5)
      }
      if(!is.na(blim)){
        lines(c(xvals[1],xvals[length(xvals)]),c(blim,blim),col=1,lwd=0.5, lty=3)
        text(xvals[1]+1, blim+0.05*y_upper, "Blim", cex=0.5)
      }

      title(c("Biomass trajectory (mean) by strategy",FILENAME),font.main=20)#only individual plots
      #       if(LEGEND){
      #         if(FILENAME %in% c("Blue whiting_GroupNo22","Seabirds_GroupNo4","Gurnards_GroupNo27")){
      #           legend('bottomleft',strat,col = COL,lty =LTY,inset=0,cex=0.65,lwd=1,text.font=20)
      #         } else { legend('bottomright',strat,col = COL,lty =LTY,inset=0,cex=0.65,lwd=1,text.font=20) }
      #       }
      if(LEGEND){
        if (COMPARE_STRATEGIES) {
          legend('topright',c(strat1name,strat2name),col = COL,lty =LTY,inset=c(-0.72,0),pt.cex = 1,cex=0.5,lwd=1,text.font=3)
        } else {
          legend('topright',STRATS,col = COL,lty =LTY,inset=c(-0.72,-0.2),lwd=1,text.font=3,pt.cex = 1,cex=0.5)
        }
        #legend("topright",legend=STRATS,inset=c(-0.2,0), pch=c(1,3), title="Group")
        #legend('bottomright',STRATS,col = COL,lty =LTY,inset=c(-0.4,0),cex=0.65,lwd=1,text.font=3)
      }
      
      
      #if(SAVE) savePlot(paste("OUTPUT_GROUP_FIGS//",FILENAME,"_PERCS.pdf",sep=""),type='pdf')
      graphics.off()
      
      if(WRITE) write.csv(PERCS[,-1],paste("OUTPUT_percentiles//",FILENAME,"_PERCS.csv",sep=""))
    }
    #if(!SAVE) dev.off()
  }
  graphics.off()
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  
  if (EFFORT){
    
    setwd(paste(RootPath,"\\Effort", sep=''))
    
    g <- list.files()     # which groups are there?
    gnum <- 2    #g[6] is "Cod (adult)_GroupNo14.csv"
    FILENAME <- substr(g[gnum],1,nchar(g[gnum])-4)
    groupdat <- read.csv(g[gnum],skip=6, head=T)
    
    nyrs <- (ncol(groupdat)-4)/12 # num yrs
    Start <- StartRun_Year
    End <- EndRun_Year
    
    COL = rep(1:8,10)[1:length(strat)]
    LTY<- c(rep(1,8),rep(2,8),rep(3,8),rep(4,8),rep(5,8),rep(6,8))[1:length(strat)]; #LTY[1:6] <- COL[6:1]
    
    WRITE<-F
    SUMMARYPLOT<-T
    SAVE<-T
    SAVE_ONLY_SUMMARY<-F
    
    LEGEND<-T
    
    ###Fleet Trajectories
    
    for (G in g){

      FILENAME = substr(G,1,nchar(G)-4)
      effortdat<-read.csv(G,skip=6, head=T)
      FLEET<- as.character(unique(effortdat$FleetName))
      if(length(grep("Yearly", FILENAME, fixed=TRUE))>0 & !Plot_yearly_files) {next}
      
#      if (COMPARE_STRATEGIES && !FileIsForACompareGroup(Fleets2Plot, FILENAME)) next
      #Checks whether the fleet of this file has been chosen at top of script to be plotted
      #browser()
      DontPlot = TRUE
      for(iFleet in Fleets2Plot){
        if (length(grep(paste(iFleet,".csv", sep=''), G, fixed=TRUE))==1) {
          DontPlot=FALSE
          break
        }
      }
      if (DontPlot==TRUE) next
      
      #timeseries of FLEET effort by FleetNumber 1:12 for the 10 strategies
      if(!plot_each_timestep & !Plot_yearly_files) effortdat<-effortdat[,c(1:3,4+seq(1,nyrs*12,12))] 
      names(effortdat)[names(effortdat)=="StrategyName"]  <- "Strategy"
      
      graphics.off()
      
      #par(mfrow=c(3,4),mar=c(2,2,4,2),oma=c(1,1,3,1))
      
      if(length(grep("Yearly", G, fixed=TRUE))==1 & !Plot_yearly_files) next
      if(length(grep("Shellfish",G, fixed=TRUE))==1 & !Plot_yearly_files) next
      
      #effortdat <- effort[effort$FleetName == GROUP,c(1,3:ncol(effort))]
      if (plot_each_timestep) {
        xvals = seq(StartRun_Year,EndRun_Year-1/12,1/12)
      } else {
        xvals = StartRun_Year:(EndRun_Year-1)
      } 
      PERCS<-MDNS<- LOWS<- UPPS<- MEANS<- data.frame(year=xvals,row.names =xvals)
      if(!SUMMARYPLOT) par(mfrow=c(3,4),mar=c(2,2,4,1),oma=c(1,1,3,1))
      if(SAVE_ONLY_SUMMARY) par(mfrow=c(2,1),mar=c(1,4,3,1),oma=c(1,1,3,1))
      
      if (SAVE) {
        if(!COMPARE_STRATEGIES) {
          png(filename = paste(plot.path, "\\OUTPUT_GEARSbySTRATEGIES\\",FILENAME,"_PERCS.png",sep=""), res=900, width=8, height=4, units='in')
        } else {
          png(filename = paste(plot.path,"\\OUTPUT_COMPARE_STRATS\\",FILENAME,"_COMP.png",sep=""), res=900, width=8, height=4, units='in')
        }
      }
      
      for(strat_i in 1:length(STRATS)){

        STRAT<-paste(STRATS[strat_i],sep=' ')
        
        if(COMPARE_STRATEGIES){
          if(strat1name!=STRAT && strat2name!=STRAT) next
        }
        
        #select subset of data
        data2plot<- effortdat[effortdat$Strategy %in% STRAT,5:ncol(effortdat)]
        
        #quantiles for polygon plot
        perc<-apply(data2plot,2, FUN=function(x){quantile(x,probs=c(0.025,0.5,0.975),na.rm=T)})
        #for(i in 1:2) lines(Start:End,perc[i,],lwd=4,col='dark blue',lty=1) 
        perc<-rbind(perc, apply(data2plot,2, FUN=mean) )
        
        #           if(!SUMMARYPLOT){#do all
        #             #grey poly
        #             if(strat_i==1) YMAX<- 1.25*(max(perc))
        #             plot(StartRun_Year:(EndRun_Year-1),perc[4,],type='l',lwd=2,ylim=c(0,YMAX),
        #                  main=STRAT,xlab='years',ylab="relative effort (mean)")
        #             polygon(c(StartRun_Year:(EndRun_Year-1),StartRun_Year:(EndRun_Year-1)), c(perc[1,],perc[3,ncol(perc):1]),
        #                     col=c("grey"), border=c("grey"), lwd=1, lty=c("solid"))
        #             # add heavy mean line
        #             lines(StartRun_Year:(EndRun_Year-1),perc[4,],lwd=2,col=COL[strat_i],lty=LTY[strat_i])
        #           }
        
        #save percs

        LOWS<- cbind(LOWS,perc[1,]);   names(LOWS)[ncol(LOWS)]<-STRAT
        MDNS<- cbind(MDNS,perc[2,]);   names(MDNS)[ncol(MDNS)]<-STRAT
        UPPS<- cbind(UPPS,perc[3,]);   names(UPPS)[ncol(UPPS)]<-STRAT
        MEANS<- cbind(MEANS,perc[4,]);   names(MEANS)[ncol(MEANS)]<-STRAT
        
        PERC<-data.frame(t(perc))
        names(PERC) <- c(paste(STRAT,"LOW"),paste(STRAT,"MDN"),paste(STRAT,"UPP"),paste(STRAT,"MEANS"))
        PERCS<- cbind(PERCS, PERC)
        
      } 
      if(!SUMMARYPLOT) mtext(FILENAME, outer=T,side=3,font=2)

      #summary plot
      par(mar=c(5.1, 4.1, 4.1, 12), xpd=TRUE)

      plot(xvals,MEANS[,2],type='l',ylim=c(0,1.25*(max(MEANS[,-1]))),col=COL[1],lty=LTY[1],ylab="relative effort",xlab="year",font=20)
      for(i in 3:ncol(MEANS)) {
        lines(xvals,MEANS[,i],lty=LTY[(i-1)],col=COL[(i-1)], lwd=1)
      }
      if(SAVE & SUMMARYPLOT)  title(FILENAME,font.main=20)

      if(PLOT_CONFIDENCE_INTERVALS){
        plot(xvals,MEANS[,2],type='l',ylim=c(0,1.25*(max(MEANS[,-1],UPPS[,-1]))),lty=LTY[1],col=COL[1],ylab="relative effort (t)",xlab="year",font=20,lwd=lineweight)
        for(i in 3:ncol(MEANS)) {
          lines(xvals,MEANS[,i],lty=LTY[(i-1)],col=COL[(i-1)],lwd=lineweight)
        }
        for(i in 2:ncol(LOWS)) {
          lines(xvals,LOWS[,i],lty=LTY[(i)],col=COL[(i-1)],lwd=lineweight*0.5)
          lines(xvals,UPPS[,i],lty=LTY[(i)],col=COL[(i-1)],lwd=lineweight*0.5)
        }
      } else {
        plot(xvals,MEANS[,2],type='l',ylim=c(0,1.25*(max(MEANS[,-1]))),lty=LTY[1],col=COL[1],ylab="relative effort (t)",xlab="year",font=20,lwd=lineweight)
        for(i in 3:ncol(MEANS)) {
          lines(xvals,MEANS[,i],lty=LTY[(i-1)],col=COL[(i-1)],lwd=lineweight)
        }
      }
      
      if(LEGEND){
        if (COMPARE_STRATEGIES) {
          legend('topright',c(strat1name,strat2name),col = COL,lty =LTY,inset=c(-0.5,0),lwd=1,text.font=3,pt.cex = 1,cex=0.5)
        } else {
          legend('topright',STRATS,col = COL,lty =LTY,inset=c(-0.5,0),lwd=1,text.font=3,pt.cex = 1,cex=0.5)
        }
        #legend("topright",legend=STRATS,inset=c(-0.2,0), pch=c(1,3), title="Group")
        #legend('bottomright',STRATS,col = COL,lty =LTY,inset=c(-0.4,0),cex=0.65,lwd=1,text.font=3)
      }
      
      if(SAVE & !SUMMARYPLOT){
        if(LEGEND) {plot(0,0,axes=F,col="white",ylab="",xlab="")
          legend('bottomright',STRATS,col = COL,lty =LTY,inset=0,lwd=1,text.font=20,pt.cex = 1,cex=0.5)}
        title("effort trajectory (mean) by strategy")
        #savePlot(paste("OUTPUT_GEARSbySTRATEGIES//",FILENAME,"_PERCS.png",sep=""),type='png')
      }
      
      if(SAVE_ONLY_SUMMARY){
        plot(0,0,axes=F,col="white",ylab="",xlab="")
        legend('topright',STRATS,col = COL,lty =LTY,inset=0,lwd=1,text.font=20,pt.cex = 1,cex=0.5)
        #savePlot(paste("OUTPUT_GEARSbySTRATEGIES//",FILENAME,"_SUMMARY.pdf",sep=""),type='pdf')
      }
      if(!SAVE & !SUMMARYPLOT) title(FILENAME,font.main=20)
      
      if(WRITE) write.csv(PERCS[,-1],paste("OUTPUT_GEARSbySTRATEGIES//",FILENAME,"_PERCS.csv",sep=""))
      
      #         if(SAVE & SUMMARYPLOT & !SAVE_ONLY_SUMMARY){  title(FILENAME,font.main=20)
      #           mtext("effort trajectory (median) by strategy",side=3,outer=T,font=20)
      #           plot(0,0,axes=F,col="white",ylab="",xlab="")
      #           legend('bottomright',STRATS,col = COL,lty =LTY,inset=0,cex=.6,lwd=1,text.font=20)
      #           #savePlot(paste("OUTPUT_GEARSbySTRATEGIES//FLEET_summary.pdf",sep=""),type='pdf')
      #         }
      
      graphics.off()
      
    }
    
  }
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  
  if (VALUE){
    
    setwd(paste(RootPath,"\\ValueTrajectories", sep=''))
    
    g <- list.files()     # which groups are there?
    gnum <- 2    #g[6] is "Cod (adult)_GroupNo14.csv"
    FILENAME <- substr(g[gnum],1,nchar(g[gnum])-4)
    
    groupdat <- read.csv(g[gnum],skip=6, head=T)
    
    nyrs <- (ncol(groupdat)-5)/12 # num yrs
    Start <- StartProjection_Year
    End <- EndRun_Year
    
    COL = rep(1:8,10)[1:length(strat)]
    LTY<- c(rep(1,8),rep(2,8),rep(3,8),rep(4,8),rep(5,8),rep(6,8))[1:length(strat)]; #LTY[1:6] <- COL[6:1]
    
    WRITE<-F
    SUMMARYPLOT<-T
    SAVE<-T
    SAVE_ONLY_SUMMARY<-F
    
    LEGEND<-T
    
    ###Fleet Trajectories
    
    for (G in g){
      
      FILENAME = substr(G,1,nchar(G)-4)
      if (COMPARE_STRATEGIES && !FileIsForACompareGroupFleet(Groups2Plot, Fleets2Plot, FILENAME)) next
      if(length(grep("Yearly", FILENAME, fixed=TRUE))>0 & !Plot_yearly_files) {next}
      
      #Checks whether the fleet and group of this file has been chosen at top of script to be plotted
      DontPlot = TRUE
      for(iFleet in Fleets2Plot){
        if (length(grep(paste(iFleet,".csv", sep=''), G, fixed=TRUE))==1) {
          DontPlot=FALSE
          break
        }
      }
      if (DontPlot==TRUE) next
      DontPlot = TRUE
      for(iGroup in Groups2Plot){
        if (length(grep(paste(iGroup,"_", sep=''), G, fixed=TRUE))==1) {
          DontPlot=FALSE
          break
        }
      }
      if (DontPlot==TRUE) next
      
      groupdat<-read.csv(G,skip=6, head=T)
      FLEET<- as.character(unique(groupdat$FleetName))
      
      #timeseries of FLEET effort by FleetNumber 1:12 for the 10 strategies
      if (!plot_each_timestep && !Plot_yearly_files) groupdat<-groupdat[,c(1:5,5+seq(1,nyrs*12,12))] 
      if (plot_each_timestep && !Plot_yearly_files) groupdat<-groupdat[,c(1:5,5+seq(1,nyrs*12,1))] 

      names(groupdat)[names(groupdat)=="StrategyName"]  <- "Strategy"
      
      graphics.off()
      
      #par(mfrow=c(3,4),mar=c(2,2,4,2),oma=c(1,1,3,1))
      
      if(length(grep("Yearly", G, fixed=TRUE))==1 & !Plot_yearly_files) next
      if(length(grep("Shellfish",G, fixed=TRUE))==1 & !Plot_yearly_files) next
      
      #groupdat <- effort[effort$FleetName == GROUP,c(1,3:ncol(effort))]
      if (plot_each_timestep){
        xvals = seq(Start,End-1/12,1/12)
      } else {
        xvals = Start:(End-1)
      }
      PERCS<-MDNS<- LOWS<- UPPS<- MEANS<- data.frame(year=xvals,row.names =xvals)
      if(!SUMMARYPLOT) par(mfrow=c(3,4),mar=c(2,2,4,1),oma=c(1,1,3,1))
      if(SAVE_ONLY_SUMMARY) par(mfrow=c(2,1),mar=c(1,4,3,1),oma=c(1,1,3,1))
      
      if (SAVE) {
        if(!COMPARE_STRATEGIES) {
          png(filename = paste(plot.path, "\\OUTPUT_GEARSGROUPSbySTRATEGIES\\",FILENAME,"_PERCS.png",sep=""), res=900, width=8, height=4, units='in')
        } else {
          png(filename = paste(plot.path,"\\OUTPUT_COMPARE_STRATS\\",FILENAME,"_COMP.png",sep=""), res=900, width=8, height=4, units='in')
        }
      }
      
      
      for(strat_i in 1:length(STRATS)){

        STRAT<-paste(STRATS[strat_i],sep=' ')
        
        #if comparing 2 strategies and this strategy is neither move skip to next strategy in loop
        if(COMPARE_STRATEGIES){
          if(strat1name!=STRAT && strat2name!=STRAT) next
        }
        
        #select subset of data
        data2plot<- groupdat[groupdat$Strategy %in% STRAT,6:ncol(groupdat)]*570000
        
        #quantiles for polygon plot
        perc<-apply(data2plot,2, FUN=function(x){quantile(x,probs=c(0.025,0.5,0.975),na.rm=T)})
        #for(i in 1:2) lines(Start:End,perc[i,],lwd=4,col='dark blue',lty=1) 
        perc<-rbind(perc, apply(data2plot,2, FUN=mean) )
        
        #           if(!SUMMARYPLOT){#do all
        #             #grey poly
        #             if(strat_i==1) YMAX<- 1.25*(max(perc))
        #             plot(StartRun_Year:(EndRun_Year-1),perc[4,],type='l',lwd=2,ylim=c(0,YMAX),
        #                  main=STRAT,xlab='years',ylab="relative effort (mean)")
        #             polygon(c(StartRun_Year:(EndRun_Year-1),StartRun_Year:(EndRun_Year-1)), c(perc[1,],perc[3,ncol(perc):1]),
        #                     col=c("grey"), border=c("grey"), lwd=1, lty=c("solid"))
        #             # add heavy mean line
        #             lines(StartRun_Year:(EndRun_Year-1),perc[4,],lwd=2,col=COL[strat_i],lty=LTY[strat_i])
        #           }
        
        #save percs
        LOWS<- cbind(LOWS,perc[1,]);   names(LOWS)[ncol(LOWS)]<-STRAT
        MDNS<- cbind(MDNS,perc[2,]);   names(MDNS)[ncol(MDNS)]<-STRAT
        UPPS<- cbind(UPPS,perc[3,]);   names(UPPS)[ncol(UPPS)]<-STRAT
        MEANS<- cbind(MEANS,perc[4,]);   names(MEANS)[ncol(MEANS)]<-STRAT
        
        PERC<-data.frame(t(perc))
        names(PERC) <- c(paste(STRAT,"LOW"),paste(STRAT,"MDN"),paste(STRAT,"UPP"),paste(STRAT,"MEANS"))
        PERCS<- cbind(PERCS, PERC)
        
      } 
      
      if(!SUMMARYPLOT) mtext(FILENAME, outer=T,side=3,font=2)
      
      #summary plot
      par(mar=c(5.1, 4.1, 4.1, 12), xpd=TRUE)
      
      if(PLOT_CONFIDENCE_INTERVALS){
        plot(xvals,MEANS[,2],type='l',ylim=c(0,1.25*(max(MEANS[,-1],UPPS[,-1]))),lty=LTY[1],col=COL[1],ylab="relative effort",xlab="year",font=20,lwd=lineweight)
        for(i in 3:ncol(MEANS)) {
          lines(xvals,MEANS[,i],lty=LTY[(i-1)],col=COL[(i-1)],lwd=lineweight)
        }
        for(i in 2:ncol(LOWS)) {
          lines(xvals,LOWS[,i],lty=LTY[(i)],col=COL[(i-1)],lwd=lineweight*0.5)
          lines(xvals,UPPS[,i],lty=LTY[(i)],col=COL[(i-1)],lwd=lineweight*0.5)
        }
      } else {
        plot(xvals,MEANS[,2],type='l',ylim=c(0,1.25*(max(MEANS[,-1]))),lty=LTY[1],col=COL[1],ylab="value (EUR/yr)",xlab="year",font=20,lwd=lineweight)
        for(i in 3:ncol(MEANS)) {
          lines(xvals,MEANS[,i],lty=LTY[(i-1)],col=COL[(i-1)],lwd=lineweight)
        }
      }
      #plot(xvals,MEANS[,2],type='l',ylim=c(min(MEANS[,-1])*.75,1.25*(max(MEANS[,-1]))),col=COL[1],lty=LTY[1],ylab="relative effort",xlab="year",font=20)
      #for(i in 3:ncol(MEANS)) lines(xvals,MEANS[,i],lty=LTY[(i-1)],col=COL[(i-1)], lwd=1)
      
      if(SAVE & SUMMARYPLOT)  title(FILENAME,font.main=20)
      
      #         if(LEGEND){
      #           legend('topright',STRATS,col = COL,lty =LTY,inset=c(-0.5,0),cex=0.65,lwd=1,text.font=3)
      #           #legend("topright",legend=STRATS,inset=c(-0.2,0), pch=c(1,3), title="Group")
      #           #legend('bottomright',STRATS,col = COL,lty =LTY,inset=c(-0.4,0),cex=0.65,lwd=1,text.font=3)
      #         }
      
      if(LEGEND){
        if (COMPARE_STRATEGIES) {
          legend('topright',c(strat1name,strat2name),col = COL,lty =LTY,inset=c(-0.5,0),lwd=1,text.font=3,pt.cex = 1,cex=0.5)
        } else {
          legend('topright',STRATS,col = COL,lty =LTY,inset=c(-0.5,0),lwd=1,text.font=3,pt.cex = 1,cex=0.5)
        }
        #legend("topright",legend=STRATS,inset=c(-0.2,0), pch=c(1,3), title="Group")
        #legend('bottomright',STRATS,col = COL,lty =LTY,inset=c(-0.4,0),cex=0.65,lwd=1,text.font=3)
      }
      
      #         if(SAVE & !SUMMARYPLOT){
      #           if(LEGEND) {plot(0,0,axes=F,col="white",ylab="",xlab="")
      #             legend('bottomright',STRATS,col = COL,lty =LTY,inset=0,cex=.65,lwd=1,text.font=20)}
      #           title("effort trajectory (mean) by strategy")
      #           #savePlot(paste("OUTPUT_GEARSbySTRATEGIES//",FILENAME,"_PERCS.png",sep=""),type='png')
      #         }
      #         
      #         if(SAVE_ONLY_SUMMARY){
      #           plot(0,0,axes=F,col="white",ylab="",xlab="")
      #           legend('topright',STRATS,col = COL,lty =LTY,inset=0,cex=.65,lwd=1,text.font=20)
      #           #savePlot(paste("OUTPUT_GEARSbySTRATEGIES//",FILENAME,"_SUMMARY.pdf",sep=""),type='pdf')
      #         }
      
      if(!SAVE & !SUMMARYPLOT) title(FILENAME,font.main=20)
      
      if(WRITE) write.csv(PERCS[,-1],paste("OUTPUT_GEARSbySTRATEGIES//",FILENAME,"_PERCS.csv",sep=""))
      
      #         if(SAVE & SUMMARYPLOT & !SAVE_ONLY_SUMMARY){  title(FILENAME,font.main=20)
      #           mtext("effort trajectory (median) by strategy",side=3,outer=T,font=20)
      #           plot(0,0,axes=F,col="white",ylab="",xlab="")
      #           legend('bottomright',STRATS,col = COL,lty =LTY,inset=0,cex=.6,lwd=1,text.font=20)
      #           #savePlot(paste("OUTPUT_GEARSbySTRATEGIES//FLEET_summary.pdf",sep=""),type='pdf')
      #         }
      
      graphics.off()
      
    }
    
  }
  
  
  ###HIGHEST_VALUE Pie chart
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  if(HIGHEST_VALUE){

    setwd(paste(RootPath,"\\HighestValueGroup", sep=''))
    
    PlotData = vector()
    
    g <- list.files()     # which groups are there?
    
    for (G in g){
      #browser()
      
      FILENAME <- substr(G,1,nchar(G)-4)
      
      FleetData = read.csv(G, skip=6, head=T)
      FleetData = as.vector(as.matrix(FleetData[!is.na(FleetData$X1),][,5:ncol(FleetData)]))
      if(length(FleetData)==0) next
      
      Groups = unique(FleetData)
      slices = vector()
      
      for(iGroup in Groups){
        slices = c(slices,length(FleetData[FleetData==iGroup]))
      }
      
      png(filename = paste(plot.path,"\\OUTPUT_HIGHEST_CHOKE_FIGS\\",FILENAME,".png",sep=""), res=900, width=9, height=8, units='in')
      
      pct <- round(slices/sum(slices)*100,1)
      
      Groups <- paste(Groups, " ", pct, "%", sep='') # add percents to labels
      
      pie(slices, labels = Groups, main = "Highest value: percentage of years across all models", col=rainbow(length(Groups)))
      mtext(FILENAME)
      
      graphics.off()
      
    }
    
  }
  
  
  ###CHOKE SPECIES Pie chart
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  if(CHOKE_GROUPS){
    
    setwd(paste(RootPath,"/ChokeGroup", sep=''))
    
    PlotData = vector()
    
    g <- list.files()     # which groups are there?
    
    for (G in g){
      #browser()
      
      FILENAME <- substr(G,1,nchar(G)-4)
      
      FleetData = read.csv(G, skip=6, head=T)
      FleetData = as.vector(as.matrix(FleetData[!is.na(FleetData$X1),][,5:ncol(FleetData)]))
      if(length(FleetData)==0) next
      
      Groups = unique(FleetData)
      slices = vector()
      
      for(iGroup in Groups){
        slices = c(slices,length(FleetData[FleetData==iGroup]))
      }
    
    # PlotData = vector()
    # 
    # g <- list.files()     # which groups are there?
    # 
    # for (G in g){
    #   
    #   FleetData = read.csv(G, skip=6, head=T)
    #   PlotData = c(PlotData, as.vector(as.matrix(FleetData[!is.na(FleetData$X1),][,5:ncol(FleetData)])))
    #   
    # }
    # 
    # Groups = unique(PlotData)
    # slices = vector()
    # 
    # for(iGroup in Groups){
    #   slices = c(slices,length(PlotData[PlotData==iGroup]))
    # }
    png(filename = paste(plot.path,"/OUTPUT_HIGHEST_CHOKE_FIGS/",FILENAME,".png",sep=""), res=900, width=9, height=8, units='in')
    
    pct <- round(slices/sum(slices)*100,1)
    
    Groups <- paste(Groups, " ", pct, "%", sep='') # add percents to labels
    
    pie(slices, labels = Groups, main = "Choke Groups: percentage of time across all models", col=rainbow(length(Groups)))
    mtext(G)
    
    graphics.off()
    
  }    
}

}  
  
read_biom_refs = function(biom_refs, group, ref_type)
{
  group = as.character(group)
  e_blim = 2
  e_bpa = 3
  #load up the file with the values in
  for(iRow in 1:(dim(biom_refs)[1])){
    if(group == biom_refs[iRow,1]){
      if (ref_type == "blim") return (biom_refs[iRow, e_blim])
      if (ref_type == "bpa") return (biom_refs[iRow, e_bpa])
      break
    }
    if (iRow == (dim(biom_refs)[1])){
      print ("Did not find the group specified")
      return (NA)
    }
  }
}

for(iplot in c(13)){
  print(paste("Currently plotting type", iplot))
  plot_type(iplot)
}
