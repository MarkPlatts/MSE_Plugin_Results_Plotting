source("C:/Users/Mark/Desktop/MSE_Plugin_Results_Plotting/share_tools.R")
###Value Trajectories
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot_value_trajectories <- function(params){
  
  #reset the director
  setwd(paste(params$RootPath,"\\ValueTrajectories", sep=''))
  
  #Create a vector of x vals at either yearly or monthly intervals
  TimeStepVals = get_timestep_vals(params$plot_each_timestep, params$StartProjection_Year, params$EndRun_Year)
  
  #get a list of all the files in the Biomass folder
  g <- list.files()
  
  
  gnum <- 2    #g[6] is "Cod (adult)_GroupNo14.csv"
  FILENAME <- substr(g[gnum],1,nchar(g[gnum])-4)
  
  dat <- read.csv(g[gnum],skip=6, head=T)
  
  COL = rep(1:8,10)[1:length(params$strat)]
  LTY<- c(rep(1,8),rep(2,8),rep(3,8),rep(4,8),rep(5,8),rep(6,8))[1:length(params$strat)] #LTY[1:6] <- COL[6:1]
  
  WRITE<-F
  SUMMARYPLOT<-T
  SAVE<-T
  SAVE_ONLY_SUMMARY<-F
  
  LEGEND<-T
  
  ###Fleet Trajectories
  
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
    
    #Checks whether the group is designated to be plotted
    DontPlot = TRUE
    for(iGroup in params$Groups2Plot){
      if (length(grep(paste(iGroup,"_", sep=''), G, fixed=TRUE))==1) {
        DontPlot=FALSE
        break
      }
    }
    if (DontPlot==TRUE) next
    
    dat<-read.csv(G,skip=6, head=T)
    FLEET<- as.character(unique(dat$FleetName))
    
    #timeseries of FLEET effort by FleetNumber 1:12 for the 10 strategies
    if (!params$plot_each_timestep && !params$Plot_yearly_files) dat<-dat[,c(1:5,5+seq(1,params$Projected_NYears*12,12))] 
    if (params$plot_each_timestep && !params$Plot_yearly_files) dat<-dat[,c(1:5,5+seq(1,params$Projected_NYears*12,1))] 
    
    names(dat)[names(dat)=="StrategyName"]  <- "Strategy"
    
    graphics.off()
    
    #par(mfrow=c(3,4),mar=c(2,2,4,2),oma=c(1,1,3,1))
    
    if(length(grep("Yearly", G, fixed=TRUE))==1 & !params$Plot_yearly_files) next
    if(length(grep("Shellfish",G, fixed=TRUE))==1 & !params$Plot_yearly_files) next
    

    PERCS<-MDNS<- LOWS<- UPPS<- MEANS<- data.frame(year=TimeStepVals,row.names =TimeStepVals)
    if(!SUMMARYPLOT) par(mfrow=c(3,4),mar=c(2,2,4,1),oma=c(1,1,3,1))
    if(SAVE_ONLY_SUMMARY) par(mfrow=c(2,1),mar=c(1,4,3,1),oma=c(1,1,3,1))
    
    if (SAVE) {
      if(!params$COMPARE_STRATEGIES) {
        png(filename = paste(params$plot.path, "\\OUTPUT_GEARSGROUPSbySTRATEGIES\\",FILENAME,"_PERCS.png",sep=""), res=900, width=8, height=4, units='in')
      } else {
        png(filename = paste(params$plot.path,"\\OUTPUT_COMPARE_STRATS\\",FILENAME,"_COMP.png",sep=""), res=900, width=8, height=4, units='in')
      }
    }
    
    
    for(strat_i in 1:length(params$strats)){
      
      STRAT<-paste(params$strats[strat_i],sep=' ')
      
      #if comparing 2 strategies and this strategy is neither move skip to next strategy in loop
      if(params$COMPARE_STRATEGIES){
        if(strat1name!=STRAT && strat2name!=STRAT) next
      }
      
      #select subset of data
      data2plot<- dat[dat$Strategy %in% STRAT,6:ncol(dat)]*570000
      
      #quantiles for polygon plot
      perc<-apply(data2plot,2, FUN=function(x){quantile(x,probs=c(0.025,0.5,0.975),na.rm=T)})
      #for(i in 1:2) lines(params$StartProjection_Year:End,perc[i,],lwd=4,col='dark blue',lty=1) 
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
    
    if(params$PLOT_CONFIDENCE_INTERVALS){
      plot(TimeStepVals,MEANS[,2],type='l',ylim=c(0,1.25*(max(MEANS[,-1],UPPS[,-1]))),lty=LTY[1],col=COL[1],ylab="relative effort",xlab="year",font=20,lwd=params$lineweight)
      for(i in 3:ncol(MEANS)) {
        lines(TimeStepVals,MEANS[,i],lty=LTY[(i-1)],col=COL[(i-1)],lwd=params$lineweight)
      }
      for(i in 2:ncol(LOWS)) {
        lines(TimeStepVals,LOWS[,i],lty=LTY[(i)],col=COL[(i-1)],lwd=params$lineweight*0.5)
        lines(TimeStepVals,UPPS[,i],lty=LTY[(i)],col=COL[(i-1)],lwd=params$lineweight*0.5)
      }
    } else {
      plot(TimeStepVals,MEANS[,2],type='l',ylim=c(0,1.25*(max(MEANS[,-1]))),lty=LTY[1],col=COL[1],ylab="Value (EUR/yr)",xlab="Year",font=20,lwd=params$lineweight)
      for(i in 3:ncol(MEANS)) {
        lines(TimeStepVals,MEANS[,i],lty=LTY[(i-1)],col=COL[(i-1)],lwd=params$lineweight)
      }
    }
    
    if(SAVE & SUMMARYPLOT)  title(FILENAME,font.main=20)
    
    if(LEGEND){
      if (params$COMPARE_STRATEGIES) {
        legend('topright',c(strat1name,strat2name),col = COL,lty =LTY,inset=c(-0.5,-0.2),lwd=1,text.font=3,pt.cex = 1,cex=0.5)
      } else {
        legend('topright',params$strats,col = COL,lty =LTY,inset=c(params$legend_x_inset2,-0.2),lwd=1,text.font=3,pt.cex = 1,cex=0.5)
      }
    }
    
    
    if(!SAVE & !SUMMARYPLOT) title(FILENAME,font.main=20)
    
    if(WRITE) write.csv(PERCS[,-1],paste("OUTPUT_GEARSbySTRATEGIES//",FILENAME,"_PERCS.csv",sep=""))
    
    graphics.off()
    
  }
  
}