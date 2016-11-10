source("C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/R Code/Plotting Trajectories post Jan 2016/Plotting_Trajectores_Project_2016/share_tools.R")

###Biomass trajectories
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot_biomass_trajectories <- function(params){
  #Load up biomass reference file
  biom_refs = read.csv(paste(params$plot.path,"/Biom_refs.csv",sep=''))

  #get a list of all the files in the Biomass folder
  g <- list.files(paste(params$RootPath,"\\Biomass", sep=''))     # which groups are there?
  setwd(params$RootPath) #reset the director
  
  #Create a vector of x vals at either yearly or monthly intervals
  TimeStepVals = get_timestep_vals(params$plot_each_timestep, params$StartRun_Year, params$EndRun_Year)

  #This is used to figure out how many years to plot for
  #params$nYrs <- (ncol(groupdat)-4)/12 # num yrs
  
  #setwd("Y:\\WP1 Tradeoffs in mixed fisheries\\NSea Multiannual Plan\\Evaluation\\STECF Results 1000_30 years_SR\\")
  # Model Strategy  X1...months
  #take jans   BIOMASS is 1991+forecast, RealisedF is just forecast
  # if (!params$plot_each_timestep && !params$Plot_yearly_files) {groupdat<-groupdat[,c(1:4,4+seq(1,params$nYrs*12,12))]}          #dec values would be  groupdat[,3+seq(12,params$params$nYrs*12,12)]
  # if (params$plot_each_timestep && !params$Plot_yearly_files) {groupdat<-groupdat[,c(1:4,4+seq(1,params$nYrs*12,1))]}
  # groupdat[,-c(1:4)] <- groupdat[,-c(1:4)]*570000
  # groupdat <- groupdat[,-which(names(groupdat)=="ResultType")]
  # groupdat <- groupdat[,-which(names(groupdat)=="GroupName")]
  # groupdat <- groupdat[,-which(names(groupdat)=="ModelID")]
  #names(groupdat)[names(groupdat)=="StrategyName"]  <- "Strategy"
  
  #groupdat[62:66,1:5] #inspect
  #groupdat[1:5,1:5] #info
  
  #plot group above for strategies
  #strat_i <-1     #strat[1] is "CFP_TargetF_Highest value"
  #data2plot<- groupdat[groupdat$Strategy %in% params$strats[strat_i],2:ncol(groupdat)]
  #ymax = max(data2plot)
  #colourful plot
  # plot mean line
  #browser()

  # plot(TimeStepVals,apply(data2plot,2,mean),type='l',lwd=2,ylim=c(0,ymax*1.2),
  #      main=paste(FILENAME,params$strats[strat_i],sep=' '),xlab='years',ylab="biomass (t)")
  # # add all sims
  # for(s in 1:nrow(data2plot)) lines(TimeStepVals,data2plot[s,],lwd=1,col=s)  
  
  #biomass trajectories
  #graphics.off() #par(mfrow=c(5,6),mar=c(2,2,4,2),oma=c(1,1,3,1))
  
  ## Strategies to plot
  #3          CFP_FIXEDTargetF_Highest value
  #5          CFP_FIXEDTargetF_Weakest stock
  #8          HCR_HighF_Weakest stock
  #9          HCR_LowF_Weakest stock
  #10        HCR_TargetF_Highest value
  #12        HCR_TargetF_Weakest stock
  #params$strats<- params$strats
  #params$strats<- strat[c(3,5,8,9,10,12)]
  #all biomass trajectories
  #WRITE<-F
  #SAVE<-T
  #graphics.off()#par(mfrow=c(5,6),mar=c(2,2,4,2),oma=c(1,1,3,1))
  #COL = rep(1:8,10)[1:length(params$strat)]
  #LTY<- c(rep(1,8),rep(2,8),rep(3,8),rep(4,8),rep(5,8),rep(6,8))[1:length(params$strats)]; #params$LTY[1:6] <- params$COL[6:1]
  #params$LEGEND<-T
  #params$strats <- as.character(unique(params$strats))  # 10 strategies
  #setwd(params$RootPath)
  #setwd("Y:\\WP1 Tradeoffs in mixed fisheries\\NSea Multiannual Plan\\Evaluation\\STECF Results 1000_30 years_SR_additionalruns\\"); g <- list.files("Trajectories2"); params$strat<- c("HCR_HighF_Highest value","HCR_LowF_Highest value")
  
  for(G in g){
    
    #Get the filename to be used to check whether yearly in name, to name files of plots and to add text to plots
    FILENAME = substr(G,1,nchar(G)-4)
    
    #Check whether we should plot the yearly files or the monthly files
    if(IsIncorrectFileType_YearlyMonthly(FILENAME, params$Plot_yearly_files)) next
    
    #Checks whether the group is designated to be plotted
    DontPlot = TRUE
    for(iGroup in params$Groups2Plot){
      if (length(grep(paste(iGroup,".csv", sep=''), G, fixed=TRUE))==1) {
        DontPlot=FALSE
        break
      }
    }
    if (DontPlot==TRUE) next
    
    
    groupdat <- read.csv(paste("Biomass\\",G,sep=''),skip=7, head=T)
    GroupName = groupdat[1,1]
    
    
    if (params$SAVE) {
      if(!params$COMPARE_STRATEGIES) {
        png(filename = paste(params$plot.path,"\\OUTPUT_GROUP_FIGS\\",FILENAME,"_PERCS.png",sep=""), res=900, width=8, height=4, units='in')
      } else {
        png(filename = paste(params$plot.path,"\\OUTPUT_COMPARE_STRATS\\",FILENAME,"_COMP.png",sep=""), res=900, width=8, height=4, units='in')
      }
    }
    
    if(!params$plot_each_timestep && !params$Plot_yearly_files){
      groupdat<-groupdat[,c(1:4,4+seq(1,params$params$nYrs*12,12))] 
    } else if (params$plot_each_timestep && !params$Plot_yearly_files){
      groupdat<-groupdat[,c(1:4,4+seq(1,params$params$nYrs*12,1))] 
    }
    #dec values would be  groupdat[,3+seq(12,params$params$nYrs*12,12)]
    groupdat[,-c(1:4)] <- groupdat[,-c(1:4)]*570000
    groupdat <- groupdat[,-which(names(groupdat)=="ResultType")]
    groupdat <- groupdat[,-which(names(groupdat)=="GroupName")]
    groupdat <- groupdat[,-which(names(groupdat)=="ModelID")]
    
    PERCS<-MDNS<- LOWS<- UPPS<- MEANS<- data.frame(year=TimeStepVals,row.names =TimeStepVals)
    for(strat_i in 1:length(params$strats)){
      
      STRAT<-paste(params$strats[strat_i],sep=' ')
      
      if(params$COMPARE_STRATEGIES){
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

    if(params$PLOT_CONFIDENCE_INTERVALS){
      plot(TimeStepVals,MEANS[,2],type='l',ylim=c(0,1.25*y_upper),lty=params$LTY[1],col=params$COL[1],ylab="relative biomass (t)",xlab="year",font=20,lwd=params$lineweight)
      for(i in 3:ncol(MEANS)) {
        lines(TimeStepVals,MEANS[,i],lty=params$LTY[(i-1)],col=params$COL[(i-1)],lwd=params$lineweight)
      }
      for(i in 2:(ncol(LOWS)-1)) {
        #browser()
        lines(TimeStepVals,LOWS[,i],lty=params$LTY[(i)],col=params$COL[(i-1)],lwd=params$lineweight*0.5)
        lines(TimeStepVals,UPPS[,i],lty=params$LTY[(i)],col=params$COL[(i-1)],lwd=params$lineweight*0.5)
      }
    } else {
      plot(TimeStepVals,MEANS[,2],type='l',ylim=c(0,1.25*y_upper),lty=params$LTY[1],col=params$COL[1],ylab="relative biomass (t)",xlab="year",font=20,lwd=params$lineweight)
      for(i in 2:ncol(MEANS)) {
        lines(TimeStepVals,MEANS[,i],lty=params$LTY[(i-1)],col=params$COL[(i-1)],lwd=params$lineweight)
      }
    }
    
    #plot reference levels
    if(!is.na(bpa)){
      lines(c(TimeStepVals[1],TimeStepVals[length(TimeStepVals)]),c(bpa,bpa),col=1,lwd=0.5, lty=3)
      text(TimeStepVals[1]+1, bpa+0.05*y_upper, "Bpa", cex=0.5)
    }
    if(!is.na(blim)){
      lines(c(TimeStepVals[1],TimeStepVals[length(TimeStepVals)]),c(blim,blim),col=1,lwd=0.5, lty=3)
      text(TimeStepVals[1]+1, blim+0.05*y_upper, "Blim", cex=0.5)
    }
    
    title(c("Biomass trajectory (mean) by strategy",FILENAME),font.main=20)#only individual plots
    #       if(params$LEGEND){
    #         if(FILENAME %in% c("Blue whiting_GroupNo22","Seabirds_GroupNo4","Gurnards_GroupNo27")){
    #           legend('bottomleft',strat,col = params$COL,lty =params$LTY,inset=0,cex=0.65,lwd=1,text.font=20)
    #         } else { legend('bottomright',strat,col = params$COL,lty =params$LTY,inset=0,cex=0.65,lwd=1,text.font=20) }
    #       }
    if(params$LEGEND){
      if (params$COMPARE_STRATEGIES) {
        legend('topright',c(strat1name,strat2name),col = params$COL,lty =params$LTY,inset=c(-0.72,0),pt.cex = 1,cex=0.5,lwd=1,text.font=3)
      } else {
        legend('topright',params$strats,col = params$COL,lty =params$LTY,inset=c(-0.72,-0.2),lwd=1,text.font=3,pt.cex = 1,cex=0.5)
      }
      #legend("topright",legend=params$strats,inset=c(-0.2,0), pch=c(1,3), title="Group")
      #legend('bottomright',params$strats,col = params$COL,lty =params$LTY,inset=c(-0.4,0),cex=0.65,lwd=1,text.font=3)
    }
    
    
    #if(params$SAVE) savePlot(paste("OUTPUT_GROUP_FIGS//",FILENAME,"_PERCS.pdf",sep=""),type='pdf')
    graphics.off()
    
    if(params$WRITE) write.csv(PERCS[,-1],paste("OUTPUT_percentiles//",FILENAME,"_PERCS.csv",sep=""))
  }
  #if(!params$SAVE) dev.off()
  graphics.off()
}
