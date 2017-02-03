###Pie charts
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot_pies <- function(plot.path, fleet.data, file.name){
  
  Groups = unique(fleet.data)
  slices = vector()
  
  for(iGroup in Groups){
    slices = c(slices,length(fleet.data[fleet.data==iGroup]))
  }
  
  png(filename = paste(plot.path,file.name,".png",sep=""), res=900, width=9, height=8, units='in')
  
  pct <- round(slices/sum(slices)*100,1)
  Groups <- paste(Groups, " ", pct, "%", sep='') # add percents to labels
  pie(slices, labels = Groups, main = "Highest value: percentage of years across all models", col=rainbow(length(Groups)))
  mtext(file.name)
  
  graphics.off()
  
}

run_plot_pies <- function(params, parents.folder.for.plots, 
                                results.folder.name,by.regulations){
  
  g <- list.files(paste(params$RootPath,"/",results.folder.name, sep=''), full.names = T)     # which groups are there?
  
  for (G in g){
    
    file.name.without.path = basename(G)
    file.name.without.ext <- substr(basename(file.name.without.path),1,nchar(file.name.without.path)-4)
    
    fleet.data = read.csv(G, skip=6, head=T)
    
    fleet.data.whole = as.vector(as.matrix(fleet.data[!is.na(fleet.data$X1),5:ncol(fleet.data)]))
    if(length(fleet.data.whole)==0) next
    
    path = paste(params$plot.path, parents.folder.for.plots, sep="")
    
    plot_pies(path, fleet.data.whole, file.name.without.ext)
    
    #chop up by strategy and apply=============================
    strategies = unique(fleet.data$StrategyName)
    
    #filter by strategy and plot
    for(iStrategy in strategies){
      fleet.data.by.strategy = fleet.data[fleet.data$StrategyName==iStrategy,]
      fleet.data.by.strategy = as.vector(as.matrix(fleet.data.by.strategy[!is.na(fleet.data.by.strategy$X1),5:ncol(fleet.data.by.strategy)]))
      if(length(fleet.data.by.strategy)==0) next
      CreateFolderIfDoesntExist(folder.name = iStrategy, path = paste(params$plot.path,parents.folder.for.plots,sep=""))
      path = paste(params$plot.path, parents.folder.for.plots, iStrategy, "/", sep="")
      plot_pies(path, fleet.data.by.strategy, file.name.without.ext)
    }
    #=============================================
    if(by.regulations){
      #filter by regulation type and plot
      for(iRegulation in params$reg.types){
        fleet.data.by.regulation = ExtractDataByRegulationType(iRegulation, fleet.data, strategies)
        fleet.data.by.regulation = as.vector(as.matrix(fleet.data.by.regulation[!is.na(fleet.data.by.regulation$X1),5:ncol(fleet.data.by.regulation)]))
        if(length(fleet.data.by.regulation)==0) next
        CreateFolderIfDoesntExist(folder.name = iRegulation, path = paste(params$plot.path,parents.folder.for.plots,sep=""))
        path = paste(params$plot.path, parents.folder.for.plots, iRegulation, "/", sep="")
        plot_pies(path, fleet.data.by.regulation, file.name.without.ext)
      }     
    }
    
    
  }    
}


ExtractDataByRegulationType = function(regulation.type, data, unique.strategies)
{

  strategies.of.reg.type = SubsetVectorStrings_ContainingString(unique.strategies, regulation.type)
  
  data.filtered = data[data$StrategyName %in% strategies.of.reg.type,]
  
  return(data.filtered)
  
}

