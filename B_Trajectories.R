source("C:/Users/Mark/Desktop/MSE_Plugin_Results_Plotting/share_tools.R")

###Biomass trajectories
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot_biomass_trajectories <- function(params){
  
  #Load up biomass reference file
  biom_refs = read.csv(paste(params$plot.path,"/Biom_refs.csv",sep=''))
  #reset the director
  setwd(paste(params$RootPath,"\\Biomass", sep=''))
  
  #Create a vector of x vals at either yearly or monthly intervals
  TimeStepVals = get_timestep_vals(params$plot_each_timestep, params$StartRun_Year, params$EndRun_Year)
  
  #get a list of all the files in the Biomass folder
  g <- list.files()

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
    
    #browser()
    groupdat <- read.csv(paste(G,sep=''),skip=7, head=T)
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
    if(params$LEGEND){
      if (params$COMPARE_STRATEGIES) {
        legend('topright',c(strat1name,strat2name),col = params$COL,lty =params$LTY,inset=c(-0.72,0),pt.cex = 1,cex=0.5,lwd=1,text.font=3)
      } else {
        legend('topright',params$strats,col = params$COL,lty =params$LTY,inset=c(-0.72,-0.2),lwd=1,text.font=3,pt.cex = 1,cex=0.5)
      }
    }
    
    graphics.off()
    
    if(params$WRITE) write.csv(PERCS[,-1],paste("OUTPUT_percentiles//",FILENAME,"_PERCS.csv",sep=""))
  }
  graphics.off()
}
