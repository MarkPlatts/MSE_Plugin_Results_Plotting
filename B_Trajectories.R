#' Save biomass trajectories.
#' 
#' @details 
#'
#' @param params This contains all the parameter settings specified in the file initialisation
#'
#'
library(stringr)




plot_biomass_trajectories <- function(params){

  #Load up biomass reference file
  if(file.exists(paste0(params$plot.path,"/Biom_refs.csv"))){
    biom_refs = read.csv(paste(params$plot.path,"/Biom_refs.csv",sep=''))
  } else {
    biom_refs = NA
  }

  #initialise plotting params
  plotting_params = initialise_plotting_params("Biomass", params$plot_each_timestep, params$StartRun_Year, params$EndRun_Year, params$RootPath)

  for(G in plotting_params$g){
    
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

    #Setup the connection for saving plots to file
    png(filename = paste(params$plot.path,"/BIOMASS/",FILENAME,"_PERCS.png",sep=""), res=900, width=8, height=4, units='in')
    
    #Load the data from file
    plotting_params$dat <- read.csv(paste(params$RootPath,"/Biomass/",G, sep=''),skip=7, head=T)

    #Modify the values so that they are for the entire region
    area <- get_area(params = params, file.name = G)
    plotting_params$dat[,-c(1:4)] <- plotting_params$dat[,-c(1:4)] * area / 1000 #Multiplying it by area gives absolute biomass across area
                                                                                    #Dividing by 1000 gives value in kt - we do this to prevent scientific units
    #Calculate the values to be plotted
    plotting_params = calc_vals_for_plotting(params, plotting_params)
    
    #Extract reference points
    GroupName = plotting_params$dat[1,1]
    bpa = read_biom_refs(biom_refs, GroupName, "bpa")     #Needs to be specified in the file as kt
    blim = read_biom_refs(biom_refs, GroupName, "blim")   #Needs to be specified in the file as kt
    
    #now summary plot
    par(mar=c(5.1, 4.1, 4.1, 16), xpd=TRUE)
    
    #figure out what the highest value the y-axis needs to be
    if(!is.na(bpa)){
      y_upper = max(plotting_params$MEANS[,-1],plotting_params$UPPS[,-1],bpa)
    } else {
      y_upper = max(plotting_params$MEANS[,-1],plotting_params$UPPS[,-1])
    }

    #Plot a strategy results
    if(params$PLOT_CONFIDENCE_INTERVALS){
      plot(plotting_params$TimeStepVals,plotting_params$MEANS[,2],type='l',ylim=c(0,1.25*y_upper),lty=params$LTY[1],col=params$COL[1],ylab="biomass (Kt)",xlab="year",font=20,lwd=params$lineweight*3)
      for(i in 3:ncol(plotting_params$MEANS)) {
        lines(plotting_params$TimeStepVals,plotting_params$MEANS[,i],lty=params$LTY[(i-1)],col=params$COL[(i-1)],lwd=params$lineweight*3)
      }
      for(i in 2:(ncol(plotting_params$LOWS))) {
        lines(plotting_params$TimeStepVals,plotting_params$LOWS[,i],lty=params$LTY[(i-1)],col=params$COL[(i-1)],lwd=params$lineweight*0.5)
        lines(plotting_params$TimeStepVals,plotting_params$UPPS[,i],lty=params$LTY[(i-1)],col=params$COL[(i-1)],lwd=params$lineweight*0.5)
      }
    } else {
      plot(plotting_params$TimeStepVals,plotting_params$MEANS[,2],type='l',ylim=c(0,1.25*y_upper),lty=params$LTY[1],col=params$COL[1],ylab="relative biomass (kt)",xlab="year",font=20,lwd=params$lineweight)
      for(i in 2:ncol(plotting_params$MEANS)) {
        lines(plotting_params$TimeStepVals,plotting_params$MEANS[,i],lty=params$LTY[(i-1)],col=params$COL[(i-1)],lwd=params$lineweight)
      }
    }
    
    #plot the reference points
    if(!is.na(bpa)){
      lines(c(plotting_params$TimeStepVals[1],plotting_params$TimeStepVals[length(plotting_params$TimeStepVals)]),c(bpa,bpa),col=1,lwd=0.5, lty=3)
      text(plotting_params$TimeStepVals[1]+1, bpa+0.05*y_upper, "Bpa", cex=0.5)
    }
    if(!is.na(blim)){
      lines(c(plotting_params$TimeStepVals[1],plotting_params$TimeStepVals[length(plotting_params$TimeStepVals)]),c(blim,blim),col=1,lwd=0.5, lty=3)
      text(plotting_params$TimeStepVals[1]+1, blim+0.05*y_upper, "Blim", cex=0.5)
    }
    
    #Add a title and legend
    title(c("Biomass trajectory (mean) by strategy",FILENAME),font.main=20)#only individual plots
    if(params$LEGEND){
      legend('topright',params$strats,col = params$COL,lty =params$LTY,inset=c(-0.72,-0.2),lwd=1,text.font=3,pt.cex = 1,cex=0.5)
    }
    
    graphics.off()
    
  }
  graphics.off()
}
