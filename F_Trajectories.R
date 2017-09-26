source("share_tools.R")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot_fishing_trajectories <- function(params, folder.to.save.plot, y_label, plot_type){
  
  setwd(folder.to.save.plot)
  print(paste0("Plotting", folder.to.save.plot))

  #set the x axis values depending on what result type and whether plotting yearly or montly
  if(any(plot_type == "quota_hcrf_cons", plot_type == "quota_hcrf_targ", plot_type == "mort_hcrf_cons", plot_type == "mort_hcrf_targ")){
    TimeStepVals = get_timestep_vals(FALSE, params$StartProjection_Year, params$EndRun_Year)
  } else if (any(plot_type == "mort_real_landf", plot_type == "mort_real_discf", plot_type == "mort_real_f", plot_type == "catch", plot_type == "landings", 
                 plot_type == "discards")) {
    TimeStepVals = get_timestep_vals(params$plot_each_timestep, params$StartProjection_Year, params$EndRun_Year)
  }
    g <- list.files()     # which groups are there?

    if(!params$SAVE) pdf(file =paste(params$plot.path, "\\", folder.to.save.plot, "\\", TITLE," plots by group and strategy.pdf",sep=""),width=14,height=7,paper="a4r")
    
    for(G in g){
      
      #if(G =="HCR_Quota_Cons_Blue whiting_GroupNo22_FleetNo1.csv") browser()

      FILENAME <- substr(G,1,nchar(G)-4)
      #print(FILENAME)

      #stopifnot(FILENAME!="HCR_F_Cons_Nephrops_GroupsNo55") 
      if (any(plot_type == "mort_real_f",plot_type == "mort_real_landf",plot_type == "mort_real_discf", plot_type == "mort_hcrf_cons",plot_type == "mort_hcrf_targ")){
        if(!FileIsForACompareGroup(params, FILENAME)) next
      } else if (any(plot_type == "catch",plot_type == "discards",plot_type == "landings",plot_type == "quota_hcrf_cons",plot_type == "quota_hcrf_targ")) {
        if (!FileIsForACompareGroupFleet(params, FILENAME)) next
      }
      
      #Need to figure out what to do about types that are yearly but don't have yearly in them
      if(any(plot_type == "catch",plot_type == "discards",plot_type == "landings",plot_type == "mort_real_f",plot_type == "mort_real_landf",
             plot_type == "mort_real_discf")){
        if(IsIncorrectFileType_YearlyMonthly(FILENAME, params$Plot_yearly_files)) next
      }

      DontPlot = TRUE
      for(iGroup in params$Groups2Plot){
        if (length(grep(paste("_",iGroup, sep=''), G, fixed=TRUE))==1) {
          DontPlot=FALSE
          break
        }
      }
      if (DontPlot==TRUE) next
      
      if(any(plot_type == "quota_hcrf_cons",plot_type == "quota_hcrf_targ",plot_type == "catch",plot_type == "discards",plot_type == "landings")){
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

      groupdat <- read.table(file=paste(G,sep=''),skip=7, header = TRUE, fill = TRUE,sep=",",as.is =T)
      #if all values in the file are -9999 then we need to skip plotting it
      if(any(plot_type == "mort_hcrf_cons",plot_type == "mort_hcrf_targ")){
        if (isAll(dt = groupdat, col.data.starts = 5, val.to.check = -9999)) next
        # testvaliddata <- groupdat[, 5:ncol(groupdat)]
        # browser()
        # if(length(as.matrix(testvaliddata))*-9999==sum(testvaliddata)) {next}
        # browser()
      }
      
      if(any(plot_type == "mort_real_f",plot_type == "mort_real_landf",plot_type == "mort_real_discf")){
        if (isAll(dt = groupdat, col.data.starts = 5, val.to.check = 0)) next
        # testvaliddata<- groupdat[, 5:ncol(groupdat)]
        # if(sum(testvaliddata)==0) {next}
      }
      
      if(any(plot_type == "quota_hcrf_cons",plot_type == "quota_hcrf_targ")){
        testvaliddata<- groupdat[, 6:ncol(groupdat)]
        #if(length(as.matrix(testvaliddata))*-9999==sum(testvaliddata)) {next}
        if (sum(testvaliddata!=-9999 & testvaliddata!=0) == 0) next
      }

      # if (params$SAVE) {
      #   if(any(plot_type == "quota_hcrf_cons",plot_type == "quota_hcrf_targ",plot_type == "catch",plot_type == "discards",plot_type == "landings")){
      png(filename = paste(params$plot.path,folder.to.save.plot, "/", FILENAME,"_.png",sep=""), res=900, width=10, height=4, units='in')              
      #   } else {
      #     png(filename = paste(params$plot.path,folder.to.save.plot, FILENAME,"_.png",sep=""), res=900, width=10, height=4, units='in')              
      #   }
      # }
      
      
      #print(paste("The number of open devices is",length(dev.list())))
      #if(length(dev.list())>1) browser()
      
      if(any(plot_type == "mort_real_f",plot_type == "mort_real_landf",plot_type == "mort_real_discf")){
        groupdat <- groupdat[,-which(names(groupdat)=="ResultType")]
        if(params$plot_each_timestep==F && !params$Plot_yearly_files){
          groupdat<-groupdat[,c(2:3,3+seq(1,params$Projected_NYears*12,12))] # TAKE JANS
        } else if(params$plot_each_timestep && !params$Plot_yearly_files) {
          groupdat<-groupdat[,c(2:(3+params$Projected_NYears*12))]
        } else {
          groupdat<-groupdat[,c(2:(3+params$Projected_NYears))]
        }
        
      }
      
      if(any(plot_type == "mort_hcrf_cons",plot_type == "mort_hcrf_targ")){
        groupdat <- groupdat[,-which(names(groupdat)=="ResultType")]
        groupdat<-groupdat[,2:ncol(groupdat)] # TAKE JANS
      }
      
      if(any(plot_type == "quota_hcrf_cons",plot_type == "quota_hcrf_targ")){
        groupdat <- groupdat[,-which(names(groupdat)=="ResultType")]
        groupdat<-groupdat[,2:ncol(groupdat)]
      }
      
      if(any(plot_type == "catch",plot_type == "discards",plot_type == "landings")){

        groupdat[,6:ncol(groupdat)] <- groupdat[,6:ncol(groupdat)]
        

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

        #check if all the values for this strategy are -9999
        if (NumberOfValsNotNA(groupdat[groupdat$Strategy %in% STRAT,], STRAT) == 0) next
        
        #select subset of data
        if(any(plot_type == "catch",plot_type == "discards",plot_type == "landings")){
          data2plot<- groupdat[groupdat$Strategy %in% STRAT,6:ncol(groupdat)] * 570000
        }
        
        if(any(plot_type == "mort_hcrf_cons",plot_type == "mort_hcrf_targ")){
          data2plot<- groupdat[groupdat$Strategy %in% STRAT, 3:ncol(groupdat)]
          if(length(data2plot)*-9999==sum(data2plot)) {
            next
          }
        }
        
        if(any(plot_type == "mort_real_f",plot_type == "mort_real_landf",plot_type == "mort_real_discf")){
          data2plot<- groupdat[groupdat$Strategy %in% STRAT, 3:ncol(groupdat)]
        }
        
        if(any(plot_type == "quota_hcrf_cons",plot_type == "quota_hcrf_targ")){
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
      if(sum(MEANS[,-1],na.rm=T)==0 & !any(plot_type == "mort_real_f",plot_type == "mort_real_landf",plot_type == "mort_real_discf") ) {
        graphics.off()
        next
      }

      par(mar=c(5.1, 4.1, 4.1, 15), xpd=TRUE)
      if(params$PLOT_CONFIDENCE_INTERVALS){
        plot(TimeStepVals,MEANS[,2],type='l',ylim=c(0,1.25*(max(MEANS[,-1],UPPS[,-1],na.rm=T))),lty=params$LTY[1],col=params$COL[1],ylab=y_label,xlab="year",font=20,lwd=params$lineweight)
        if(ncol(MEANS)>2){
          for(i in 3:ncol(MEANS)) {
            lines(TimeStepVals,MEANS[,i],lty=params$LTY[(i-1)],col=params$COL[(i-1)],lwd=params$lineweight)
          }
        }
        for(i in 2:ncol(LOWS)) {
          lines(TimeStepVals,LOWS[,i],lty=params$LTY[(i)],col=params$COL[(i-1)],lwd=params$lineweight*0.5)
          lines(TimeStepVals,UPPS[,i],lty=params$LTY[(i)],col=params$COL[(i-1)],lwd=params$lineweight*0.5)
        }
      } else {
        plot(TimeStepVals,MEANS[,2],type='l',ylim=c(0,1.25*(max(MEANS[,-1],na.rm = T))),lty=params$LTY[1],col=params$COL[1],ylab=y_label,xlab="year",font=20,lwd=params$lineweight)
        if(ncol(MEANS)>2){
          for(i in 3:ncol(MEANS)) {
            lines(TimeStepVals,MEANS[,i],lty=params$LTY[(i-1)],col=params$COL[(i-1)],lwd=params$lineweight)
          }
        }
      }
      #Changed all the source paths from absolute path to relative path
      if(any(plot_type == "mort_real_f",plot_type == "mort_real_landf",plot_type == "mort_real_discf")) title(c("F trajectory (mean) by strategy",FILENAME),font.main=20)#only individual plots   
      if(any(plot_type == "catch",plot_type == "discards",plot_type == "landings"))   title(c("Catch (mean) by strategy",FILENAME),font.main=20)
      if(any(plot_type == "mort_hcrf_cons",plot_type == "mort_hcrf_targ",plot_type == "quota_hcrf_cons",plot_type == "quota_hcrf_targ")) title(FILENAME,font.main=20)
      
      if(params$LEGEND){
        legend('topright',params$strat,col = params$COL,lty =params$LTY,inset=c(params$legend_x_inset2,-0.2),lwd=1,text.font=3,pt.cex = 1,cex=0.5)
      }
      if (any(plot_type == "mort_real_f", plot_type == "mort_real_landf", plot_type == "mort_real_discf")){
        if(params$WRITE) write.csv(PERCS[,-1],paste(params$plot.path, "\\OUTPUT_FcatchBySTRATEGIES\\", FILENAME, ".csv", sep=""))
      } else if (any(plot_type == "catch",plot_type == "discards",plot_type == "landings")) {
        
      } else if (any(plot_type == "mort_hcrf_cons", plot_type == "mort_hcrf_targ", plot_type == "quota_hcrf_cons",plot_type == "quota_hcrf_targ")) {
        
      }
      
      graphics.off()
    }#next G
    if(!params$SAVE) dev.off()
  
}

