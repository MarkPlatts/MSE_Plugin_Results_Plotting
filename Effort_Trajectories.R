source("C:/Users/Mark/Desktop/MSE_Plugin_Results_Plotting/share_tools.R")

###Value Trajectories
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot_effort_trajectories <- function(params){
  
  #reset the director
  #setwd(paste(params$RootPath,"\\Effort", sep=''))
  
  #initialise plotting params
  plotting_params = initialise_plotting_params("Effort", params)
  
  for(G in plotting_params$g){
    
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
    
    png(filename = paste(params$plot.path,"\\OUTPUT_GEARSbySTRATEGIES\\",FILENAME,"_PERCS.png",sep=""), res=900, width=8, height=4, units='in')
    #Load the data from the file represented by G
    #browser()
    plotting_params$dat<-read.csv(paste(params$RootPath,"/Effort/",G, sep=''),skip=7, head=T)
    
    #timeseries of FLEET effort by FleetNumber 1:12 for the 10 strategies
    #if(!params$plot_each_timestep & !params$Plot_yearly_files) dat<-dat[,c(1:3,4+seq(1,params$nyrs*12,12))] 
    names(plotting_params$dat)[names(plotting_params$dat)=="StrategyName"]  <- "Strategy"
    
    #Calculate the values to be plotted
    plotting_params = calc_vals_for_plotting(params, plotting_params)
    
    #Setup for plotting
    par(mar=c(5.1, 4.1, 4.1, 12), xpd=TRUE)

    #Do the plotting and add annotation
    plot(plotting_params$TimeStepVals,plotting_params$MEANS[,2],type='l',ylim=c(0,1.25*(max(plotting_params$MEANS[,-1]))),lty=params$LTY[1],col=params$COL[1],ylab="relative effort (t)",xlab="year",font=20,lwd=params$lineweight)
    for(i in 3:ncol(plotting_params$MEANS)) {
      lines(plotting_params$TimeStepVals,plotting_params$MEANS[,i],lty=params$LTY[(i-1)],col=params$COL[(i-1)],lwd=params$lineweight)
    }
    title(FILENAME,font.main=20)
    if(params$LEGEND){
        #legend('topright',params$strats,col = params$COL,lty =params$LTY,inset=c(-0.5,0),lwd=1,text.font=3,pt.cex = 1,cex=0.5)
        legend('topright',params$strats,col = params$COL,lty =params$LTY,inset=c(params$legend_x_inset2,-0.2),lwd=1,text.font=3,pt.cex = 1,cex=0.5)
    }
    
    graphics.off()
    
  } 
}