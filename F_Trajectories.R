source("C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/R Code/Plotting Trajectories post Jan 2016/Plotting_Trajectores_Project_2016/share_tools.R")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot_fishing_trajectories <- function(params){
  
  #set the x axis values depending on what result type and whether plotting yearly or montly
  if(any(params$QUOTA_HCRF_Cons,params$QUOTA_HCRF_Targ,params$MORT_HCRF_Cons,params$MORT_HCRF_Targ)){
    TimeStepVals = get_timestep_vals(FALSE, params$StartProjection_Year, params$EndRun_Year)
  } else if (any(params$MORT_REAL_LandF, params$MORT_REAL_DiscF, params$MORT_REAL_F, params$CATCH, params$LANDING, 
                 params$DISCARD)) {
    TimeStepVals = get_timestep_vals(params$plot_each_timestep, params$StartProjection_Year, params$EndRun_Year)
  }
    g <- list.files()     # which groups are there?
    # g[1:12] All groups 12 fleets
    #g <- g[-c(1:12)]
    #remove those with a comma in title as issues
    #substr(g)
    # gnum <- 6; FILENAME <- substr(g[gnum],1,nchar(g[gnum])-4); FILENAME
    # groupdat <- read.csv(g[gnum],skip=7, head=T)
    # PlotStart_Year = params$StartProjection_Year
    # nYrs = params$Projected_NYears
    # names(groupdat)[names(groupdat)=="StrategyName"]  <- "Strategy"
    # if(any(params$MORT_REAL_F,params$MORT_REAL_LandF,params$MORT_REAL_DiscF)){
    #   if(params$plot_each_timestep==F && !params$Plot_yearly_files){
    #     groupdat<-groupdat[,c(1:4,4+seq(1,nYrs*12,12))]
    #   }
    # }
    # 
    # if(any(params$MORT_HCRF_Cons,params$MORT_HCRF_Targ)){
    #   groupdat <- groupdat[,c(1,2,3,4,4+seq(1,params$Projected_NYears,1))]
    # }
    # if(any(params$QUOTA_HCRF_Cons,params$params$QUOTA_HCRF_Targ)){                               
    #   groupdat <- groupdat[,c(1:5,5+seq(1,params$Projected_NYears,1))]
    # }
    # 
    # #take jans   RealisedF is just forecast
    # 
    # #plot group above for strategies
    # strat_i <-6     #strat[1] is "1 CFP_2015 TargetF_Weakest stock"
    # if(any(params$MORT_HCRF_Cons,params$MORT_HCRF_Targ)){
    #   data2plot<- groupdat[groupdat$Strategy %in% params$strats[strat_i],5:ncol(groupdat)]
    # }
    # if(any(params$CATCH,params$DISCARD,params$LANDING,params$QUOTA_HCRF_Cons,params$QUOTA_HCRF_Targ)){
    #   data2plot<- groupdat[groupdat$Strategy %in% params$strats[strat_i],6:ncol(groupdat)]
    # }
    # 
    # if(any(params$MORT_REAL_F,params$MORT_REAL_LandF,params$MORT_REAL_DiscF)){ 
    #   data2plot<- groupdat[groupdat$Strategy %in% params$strats[strat_i],5:ncol(groupdat)]
    # }
    # 
    # 
    # 
    # #colourful plot
    # # plot mean line
    # ymax = max(data2plot)
    # #browser()
    # plot(TimeStepVals,apply(data2plot,2,mean),type='l',lwd=2, ylim=c(0,ymax),
    #      main=paste(FILENAME,params$strats[strat_i],sep=' '),xlab='years',ylab="")
    # #     if(params$MORT_REAL_F) TITLE="Realised F"
    # #     if(params$MORT_REAL_LandF) TITLE="Landed F"
    # #     if(params$MORT_REAL_DiscF) TITLE="Discarded F"
    # #     if(params$CATCH) TITLE="Realised catch"
    # #     if(params$LANDING) TITLE="Landed catch"
    # #     if(params$DISCARD) TITLE="Discarded catch"
    # TITLE = FILENAME
    # title(TITLE,line=-1,cex.main=0.8)
    # # add all sims
    # 
    # for(s in 1:nrow(data2plot)) lines(TimeStepVals,data2plot[s,],lwd=1,col=s)
    # 
    # 
    # #all trajectories        
    # 
    # graphics.off()#par(mfrow=c(5,6),mar=c(2,2,4,2),oma=c(1,1,3,1))
    #if(length(strat)==10) params$COL<-c(1:6,9:12)     # for line types and colours #skip yellow 7
    #if(length(strat)==22) params$COL<-c(1:6,8:12,1:6,8:12) # have more now!
    # params$COL = rep(seq(1,8,1),8)[1:length(params$strats)]
    # LTY<- c(rep(1,8),rep(2,8),rep(3,8),rep(4,8),rep(5,8),rep(6,8))[1:length(params$strats)]; #LTY[1:6] <- params$COL[6:1]
    
    if(!params$SAVE) pdf(file =paste(plot.path,"\\OUTPUT_GROUP_FIGS\\",TITLE," plots by group and strategy.pdf",sep=""),width=14,height=7,paper="a4r")
    
    for(G in g){

      FILENAME <- substr(G,1,nchar(G)-4)
      print(FILENAME)
      
      #stopifnot(FILENAME!="HCR_F_Cons_Nephrops_GroupsNo55") 
      if (any(params$MORT_REAL_F,params$MORT_REAL_LandF,params$MORT_REAL_DiscF, params$MORT_HCRF_Cons,params$MORT_HCRF_Targ)){
        if(params$COMPARE_STRATEGIES && !FileIsForACompareGroup(params, FILENAME)) next
      } else if (any(params$CATCH,params$DISCARD,params$LANDING,params$QUOTA_HCRF_Cons,params$QUOTA_HCRF_Targ)) {
        if (params$COMPARE_STRATEGIES && !FileIsForACompareGroupFleet(params, FILENAME)) next
      }
      
      #Need to figure out what to do about types that are yearly but don't have yearly in them
      if(any(params$CATCH,params$DISCARD,params$LANDING,params$MORT_REAL_F,params$MORT_REAL_LandF,
             params$MORT_REAL_DiscF)){
        if(IsIncorrectFileType_YearlyMonthly(FILENAME, params$Plot_yearly_files)) next
      }
      
      #if(length(grep("Yearly", FILENAME, fixed=TRUE))==1 && !params$Plot_yearly_files) next
      
      # GroupsNotPlot = c("Planktonic", "Seals_Grou","Small mobi", "Benthic mi", "Juvenile s","Juvenile W","Juvenile H","Juvenile S","Juvenile C","Herring (j")
      # for(iGroup in GroupsNotPlot){
      #   if (length(grep(iGroup, FILENAME, fixed=TRUE))==1) next
      # }
      #Checks whether the fleet and group of this file has been chosen at top of script to be plotted
      
      DontPlot = TRUE
      for(iGroup in params$Groups2Plot){
        if (length(grep(paste("_",iGroup, sep=''), G, fixed=TRUE))==1) {
          DontPlot=FALSE
          break
        }
      }
      if (DontPlot==TRUE) next
      
      if(any(params$QUOTA_HCRF_Cons,params$QUOTA_HCRF_Targ,params$CATCH,params$DISCARD,params$LANDING)){
        #Checks whether the fleet of this file has been chosen at top of script to be plotted
        DontPlot = TRUE
        for(iFleet in params$Fleets2Plot){
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
      if(any(params$MORT_HCRF_Cons,params$MORT_HCRF_Targ)){
        testvaliddata<- groupdat[, 5:ncol(groupdat)]
        if(length(as.matrix(testvaliddata))*-9999==sum(testvaliddata)) {next}
      }
      
      if(any(params$MORT_REAL_F,params$MORT_REAL_LandF,params$MORT_REAL_DiscF)){
        testvaliddata<- groupdat[, 5:ncol(groupdat)]
        if(sum(testvaliddata)==0) {next}
      }
      
      if(any(params$QUOTA_HCRF_Cons,params$QUOTA_HCRF_Targ)){
        testvaliddata<- groupdat[, 6:ncol(groupdat)]
        if(length(as.matrix(testvaliddata))*-9999==sum(testvaliddata)) {next}
      }
      if (params$SAVE) {
        if(!params$COMPARE_STRATEGIES) {
          if(any(params$QUOTA_HCRF_Cons,params$QUOTA_HCRF_Targ,params$CATCH,params$DISCARD,params$LANDING)){
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
      
      if(any(params$MORT_REAL_F,params$MORT_REAL_LandF,params$MORT_REAL_DiscF)){
        groupdat <- groupdat[,-which(names(groupdat)=="ResultType")]
        if(params$plot_each_timestep==F && !params$Plot_yearly_files){
          groupdat<-groupdat[,c(2:3,3+seq(1,params$Projected_NYears*12,12))] # TAKE JANS
        } else if(params$plot_each_timestep && !params$Plot_yearly_files) {
          groupdat<-groupdat[,c(2:(3+params$Projected_NYears*12))]
        } else {
          groupdat<-groupdat[,c(2:(3+params$Projected_NYears))]
        }
        
      }
      
      if(any(params$MORT_HCRF_Cons,params$MORT_HCRF_Targ)){
        groupdat <- groupdat[,-which(names(groupdat)=="ResultType")]
        groupdat<-groupdat[,2:ncol(groupdat)] # TAKE JANS
      }
      
      if(any(params$QUOTA_HCRF_Cons,params$QUOTA_HCRF_Targ)){
        groupdat <- groupdat[,-which(names(groupdat)=="ResultType")]
        groupdat<-groupdat[,2:ncol(groupdat)]
      }
      
      if(any(params$CATCH,params$DISCARD,params$LANDING)){
        #missing line data! arrgh
        #groupdat<-groupdat[-nrow(groupdat),]
        #for(i in 5:ncol(groupdat)) groupdat[,i] <- as.numeric(groupdat[,i])#
        #groupdat[,(5:ncol(groupdat))] <- as.matrix(groupdat[,(5:ncol(groupdat))])#use as.is instead
        
        #groupdat <- groupdat[,-which(names(groupdat)=="CatchType")]
        #if(any(names(groupdat)=="FleetName")) groupdat <- groupdat[,-which(names(groupdat)=="FleetName")]
        groupdat[,6:ncol(groupdat)] <- groupdat[,6:ncol(groupdat)]
        
        #WORK OUT AVE ANN params$CATCH
        # AVEgroupdat <-   groupdat[,c(1:5,5+seq(1,params$Projected_NYears*12,12))]
        # for(i in 1:11) AVEgroupdat[,6:ncol(AVEgroupdat)] <- AVEgroupdat[,6:ncol(AVEgroupdat)] + groupdat[,c(5+seq(1,params$Projected_NYears*12,12))+i] #jan
        # #take JANS
        # 
        # #overwrite with AVERAGE
        # AVEgroupdat[,6:ncol(AVEgroupdat)] <- AVEgroupdat[,6:ncol(AVEgroupdat)]/12 #average yr # rbind(3+seq(1,(nyrs-Base_NYears)*12,12) ,(2+seq(1,(nyrs-Base_NYears)*12,12)+12))  jan@dec
        # now t not t/km2
        if(params$plot_each_timestep==F && !params$Plot_yearly_files){
          groupdat<-groupdat[,c(1:5,5+seq(1,params$Projected_NYears*12,12))]
        } else if (params$plot_each_timestep && !params$Plot_yearly_files){
          groupdat<-groupdat[,c(1:(5+params$Projected_NYears*12))]
        } else {
          groupdat<-groupdat[,c(1:(5+params$Projected_NYears))]
        }
        
      }
      
      
      
      #This code converts the data into percentiles and the mean and only these values are plotted and saved
      PERCS<-MDNS<- LOWS<- UPPS<- MEANS<- data.frame(year=TimeStepVals,row.names =TimeStepVals)
      
      for(strat_i in 1:length(params$strats)){
        
        STRAT<-paste(params$strats[strat_i],sep=' ')
        
        if(params$COMPARE_STRATEGIES){
          if(strat1name!=STRAT && strat2name!=STRAT) {
            next
          }
        }
        
        
        #select subset of data
        if(any(params$CATCH,params$DISCARD,params$LANDING)){
          data2plot<- groupdat[groupdat$Strategy %in% STRAT,6:ncol(groupdat)] * 570000
        }
        
        if(any(params$MORT_HCRF_Cons,params$MORT_HCRF_Targ)){
          data2plot<- groupdat[groupdat$Strategy %in% STRAT, 3:ncol(groupdat)]
          if(length(data2plot)*-9999==sum(data2plot)) {
            next
          }
        }
        
        if(any(params$MORT_REAL_F,params$MORT_REAL_LandF,params$MORT_REAL_DiscF)){
          data2plot<- groupdat[groupdat$Strategy %in% STRAT, 3:ncol(groupdat)]
        }
        
        if(any(params$QUOTA_HCRF_Cons,params$QUOTA_HCRF_Targ)){
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
      if(sum(MEANS[,-1],na.rm=T)==0 & !any(params$MORT_REAL_F,params$MORT_REAL_LandF,params$MORT_REAL_DiscF) ) {
        graphics.off()
        next
      }
      #if(params$plot_each_timestep==T) {TimeStepVals = seq(PlotStart_Year,params$EndRun_Year-1/12,1/12)} else {TimeStepVals=PlotStart_Year:(params$EndRun_Year-1)}
      par(mar=c(5.1, 4.1, 4.1, 15), xpd=TRUE)
      
      

      if(params$PLOT_CONFIDENCE_INTERVALS){
        plot(TimeStepVals,MEANS[,2],type='l',ylim=c(0,1.25*(max(MEANS[,-1],UPPS[,-1],na.rm=T))),lty=params$LTY[1],col=params$COL[1],ylab=params$YLAB,xlab="year",font=20,lwd=params$lineweight)
        for(i in 3:ncol(MEANS)) {
          lines(TimeStepVals,MEANS[,i],lty=params$LTY[(i-1)],col=params$COL[(i-1)],lwd=params$lineweight)
        }
        for(i in 2:ncol(LOWS)) {
          lines(TimeStepVals,LOWS[,i],lty=params$LTY[(i)],col=params$COL[(i-1)],lwd=params$lineweight*0.5)
          lines(TimeStepVals,UPPS[,i],lty=params$LTY[(i)],col=params$COL[(i-1)],lwd=params$lineweight*0.5)
        }
      } else {
        plot(TimeStepVals,MEANS[,2],type='l',ylim=c(0,1.25*(max(MEANS[,-1],na.rm = T))),lty=params$LTY[1],col=params$COL[1],ylab=params$YLAB,xlab="year",font=20,lwd=params$lineweight)
        for(i in 3:ncol(MEANS)) {
          lines(TimeStepVals,MEANS[,i],lty=params$LTY[(i-1)],col=params$COL[(i-1)],lwd=params$lineweight)
        }
      }
      
      #       plot(TimeStepVals,MEANS[,2],type='l',ylim=c(min(MEANS[,-1],na.rm=T)*.75,1.25*(max(MEANS[,-1],na.rm=T))),lty=params$LTY[1],col=params$COL[1],ylab=params$YLAB,xlab="year",font=20,lwd=params$lineweight)
      #       for(i in 3:ncol(MEANS)) lines(TimeStepVals,MEANS[,i],lty=params$LTY[(i-1)],col=params$COL[(i-1)],lwd=params$lineweight)
      
      if(any(params$MORT_REAL_F,params$MORT_REAL_LandF,params$MORT_REAL_DiscF)) title(c("F trajectory (mean) by strategy",FILENAME),font.main=20)#only individual plots   
      if(any(params$CATCH,params$DISCARD,params$LANDING))   title(c("Catch (mean) by strategy",FILENAME),font.main=20)
      if(any(params$MORT_HCRF_Cons,params$MORT_HCRF_Targ,params$QUOTA_HCRF_Cons,params$QUOTA_HCRF_Targ)) title(FILENAME,font.main=20)
      
      #title(TITLE,line=-1,cex.main=0.8)
      if(params$LEGEND){
        if(params$COMPARE_STRATEGIES){
          legend('bottomright',c(strat1name,strat2name),col = params$COL,lty =params$LTY,inset=c(-0.72,0),pt.cex = 1,cex=0.5,lwd=1,text.font=3)
        } else {
          #inset=c(x_coord,y_coord)
          #legend('bottomright',params$strats,col = params$COL,lty =params$LTY,inset=c(-0.4,-0.4),pt.cex = 1,cex=0.5,lwd=1,text.font=3)
          #legend('topright',params$strat,col = params$COL,lty =params$LTY,inset=c(-0.72,-0.2),lwd=1,text.font=3,pt.cex = 1,cex=0.5)
          legend('topright',params$strat,col = params$COL,lty =params$LTY,inset=c(params$legend_x_inset2,-0.2),lwd=1,text.font=3,pt.cex = 1,cex=0.5)
        }
      }
      
      #if(params$SAVE) savePlot(paste(params$RootPath,"\\OUTPUT_GROUP_FIGS\\",FILENAME,TITLE,".pdf",sep=""),type='pdf')
      if (any(params$MORT_REAL_F,params$MORT_REAL_LandF,params$MORT_REAL_DiscF)){
        if(params$WRITE) write.csv(PERCS[,-1],paste(plot.path,"\\OUTPUT_FcatchBySTRATEGIES\\",FILENAME,".csv",sep=""))
      } else if (any(params$CATCH,params$DISCARD,params$LANDING)) {
        
      } else if (any(params$MORT_HCRF_Cons,params$MORT_HCRF_Targ, params$QUOTA_HCRF_Cons,params$QUOTA_HCRF_Targ)) {
        
      }
      
      graphics.off()
    }#next G
    if(!params$SAVE) dev.off()
    #setwd("Y:\\WP1 Tradeoffs in mixed fisheries\\NSea Multiannual Plan\\Evaluation\\STECF Results 1000_30 years_SR_final\\")
  
}

