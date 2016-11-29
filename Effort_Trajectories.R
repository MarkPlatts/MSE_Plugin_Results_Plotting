source("C:/Users/Mark/Desktop/MSE_Plugin_Results_Plotting/share_tools.R")

###Value Trajectories
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot_effort_trajectories <- function(params){
  
  #reset the director
  setwd(paste(params$RootPath,"\\Effort", sep=''))
  
  #Create a vector of x vals at either yearly or monthly intervals
  TimeStepVals = get_timestep_vals(params$plot_each_timestep, params$StartRun_Year, params$EndRun_Year)
  
  #get a list of all the files in the Biomass folder
  g <- list.files()
  
  SUMMARYPLOT<-T
  SAVE_ONLY_SUMMARY<-F
  
  for (G in g){
    
    #Get the filename to be used to check whether yearly in name, to name files of plots and to add text to plots
    FILENAME = substr(G,1,nchar(G)-4)
    
    #Check whether we should plot the yearly files or the monthly files
    if(IsIncorrectFileType_YearlyMonthly(FILENAME, params$Plot_yearly_files)) next
    
    #Checks whether the fleet is designated to be plotted
    DontPlot = TRUE
    for(iFleet in params$Fleets2Plot){
      if (length(grep(paste(iFleet,".csv", sep=''), G, fixed=TRUE))==1) {
        DontPlot=FALSE
        break
      }
    }
    if (DontPlot==TRUE) next
    
    
    #Load the data from the file represented by G
    effortdat<-read.csv(G,skip=6, head=T)
    #Get the name of the fleet
    FLEET<- as.character(unique(effortdat$FleetName))
    
    #timeseries of FLEET effort by FleetNumber 1:12 for the 10 strategies
    if(!params$plot_each_timestep & !params$Plot_yearly_files) effortdat<-effortdat[,c(1:3,4+seq(1,params$nyrs*12,12))] 
    names(effortdat)[names(effortdat)=="StrategyName"]  <- "Strategy"
    
    graphics.off()

    PERCS<-MDNS<- LOWS<- UPPS<- MEANS<- data.frame(year=TimeStepVals,row.names =TimeStepVals)
    if(!SUMMARYPLOT) par(mfrow=c(3,4),mar=c(2,2,4,1),oma=c(1,1,3,1))
    if(SAVE_ONLY_SUMMARY) par(mfrow=c(2,1),mar=c(1,4,3,1),oma=c(1,1,3,1))
    
    if (params$SAVE) {
      if(!params$COMPARE_STRATEGIES) {
        png(filename = paste(plot.path, "\\OUTPUT_GEARSbySTRATEGIES\\",FILENAME,"_PERCS.png",sep=""), res=900, width=8, height=4, units='in')
      } else {
        png(filename = paste(plot.path,"\\OUTPUT_COMPARE_STRATS\\",FILENAME,"_COMP.png",sep=""), res=900, width=8, height=4, units='in')
      }
    }
    
    for(strat_i in 1:length(params$strats)){
      
      STRAT<-paste(params$strats[strat_i],sep=' ')
      
      if(params$COMPARE_STRATEGIES){
        if(strat1name!=STRAT && strat2name!=STRAT) next
      }
      
      #select subset of data
      data2plot<- effortdat[effortdat$Strategy %in% STRAT,5:ncol(effortdat)]
      
      #quantiles for polygon plot
      perc<-apply(data2plot,2, FUN=function(x){quantile(x,probs=c(0.025,0.5,0.975),na.rm=T)})
      #for(i in 1:2) lines(params$StartRun_Year:End,perc[i,],lwd=4,col='dark blue',lty=1) 
      perc<-rbind(perc, apply(data2plot,2, FUN=mean) )
      
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
    
    plot(TimeStepVals,MEANS[,2],type='l',ylim=c(0,1.25*(max(MEANS[,-1]))),col=params$COL[1],lty=params$LTY[1],ylab="relative effort",xlab="year",font=20)
    for(i in 3:ncol(MEANS)) {
      lines(TimeStepVals,MEANS[,i],lty=params$LTY[(i-1)],col=params$COL[(i-1)], lwd=1)
    }
    if(params$SAVE & SUMMARYPLOT)  title(FILENAME,font.main=20)
    
    if(params$PLOT_CONFIDENCE_INTERVALS){
      plot(TimeStepVals,MEANS[,2],type='l',ylim=c(0,1.25*(max(MEANS[,-1],UPPS[,-1]))),lty=params$LTY[1],col=params$COL[1],ylab="relative effort (t)",xlab="year",font=20,lwd=params$lineweight)
      for(i in 3:ncol(MEANS)) {
        lines(TimeStepVals,MEANS[,i],lty=params$LTY[(i-1)],col=params$COL[(i-1)],lwd=params$lineweight)
      }
      for(i in 2:ncol(LOWS)) {
        lines(TimeStepVals,LOWS[,i],lty=params$LTY[(i)],col=params$COL[(i-1)],lwd=params$lineweight*0.5)
        lines(TimeStepVals,UPPS[,i],lty=params$LTY[(i)],col=params$COL[(i-1)],lwd=params$lineweight*0.5)
      }
    } else {
      plot(TimeStepVals,MEANS[,2],type='l',ylim=c(0,1.25*(max(MEANS[,-1]))),lty=params$LTY[1],col=params$COL[1],ylab="relative effort (t)",xlab="year",font=20,lwd=params$lineweight)
      for(i in 3:ncol(MEANS)) {
        lines(TimeStepVals,MEANS[,i],lty=params$LTY[(i-1)],col=params$COL[(i-1)],lwd=params$lineweight)
      }
    }
    
    if(params$LEGEND){
      if (params$COMPARE_STRATEGIES) {
        legend('topright',c(strat1name,strat2name),col = params$COL,lty =params$LTY,inset=c(params$legend_x_inset2,0),lwd=1,text.font=3,pt.cex = 1,cex=0.5)
      } else {
        #legend('topright',params$strats,col = params$COL,lty =params$LTY,inset=c(-0.5,0),lwd=1,text.font=3,pt.cex = 1,cex=0.5)
        legend('topright',params$strats,col = params$COL,lty =params$LTY,inset=c(params$legend_x_inset2,-0.2),lwd=1,text.font=3,pt.cex = 1,cex=0.5)
      }
    }
    
    if(params$SAVE & !SUMMARYPLOT){
      if(params$LEGEND) {plot(0,0,axes=F,col="white",ylab="",xlab="")
        legend('bottomright',params$strats,col = params$COL,lty =params$LTY,inset=0,lwd=1,text.font=20,pt.cex = 1,cex=0.5)}
      title("effort trajectory (mean) by strategy")
      #savePlot(paste("OUTPUT_GEARSbySTRATEGIES//",FILENAME,"_PERCS.png",sep=""),type='png')
    }
    
    if(SAVE_ONLY_SUMMARY){
      plot(0,0,axes=F,col="white",ylab="",xlab="")
      legend('topright',params$strats,col = params$COL,lty =params$LTY,inset=0,lwd=1,text.font=20,pt.cex = 1,cex=0.5)
      #savePlot(paste("OUTPUT_GEARSbySTRATEGIES//",FILENAME,"_SUMMARY.pdf",sep=""),type='pdf')
    }
    if(!params$SAVE & !SUMMARYPLOT) title(FILENAME,font.main=20)
    
    if(params$WRITE) write.csv(PERCS[,-1],paste("OUTPUT_GEARSbySTRATEGIES//",FILENAME,"_PERCS.csv",sep=""))
    
    graphics.off()
    
  } 
}