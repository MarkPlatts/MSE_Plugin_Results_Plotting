###HIGHEST_VALUE Pie chart
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_highestvalue_pies <- function(params)
{
  
  
  setwd(paste(params$RootPath,"\\HighestValueGroup", sep=''))
  
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