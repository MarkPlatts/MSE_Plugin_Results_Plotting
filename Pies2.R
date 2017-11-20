library(ggplot2)
library(data.table)
library(RColorBrewer)

###Pie charts
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot_pies <- function(plot.path, fleet.data, file.name, pie_name){
  
  Groups = unique(fleet.data)
  slices = vector()
  
  for(iGroup in Groups){
    slices = c(slices,length(fleet.data[fleet.data==iGroup]))
  }
  
  png(filename = paste(plot.path,file.name,".png",sep=""), res=900, width=9, height=8, units='in')
  
  pct <- round(slices/sum(slices)*100,1)
  Groups <- paste(Groups, " ", pct, "%", sep='') # add percents to labels
  pie(slices, labels = Groups, main = pie_name, col=rainbow(length(Groups)))

  mtext(file.name)
  
  graphics.off()
  
  browser()
  
}

run_plot_pies <- function(params, parents.folder.for.plots, 
                                results.folder.name, by.regulations, pie.name){
  
  g <- list.files(paste(params$RootPath,"/",results.folder.name, sep=''), full.names = T)     # which groups are there?
  
  fleet.data <- NULL
  
  for(G in g) {
    
    file.name.without.path = basename(G)
    file.name.without.ext <- substr(basename(file.name.without.path),1,nchar(file.name.without.path)-4)
    
    fleet.temp = fread(G, skip=6, head=T)
    fleet.data <- rbind(fleet.data, fleet.temp)
    
  }
  
  fleet.data <- melt(data = fleet.data, 
                     measure.vars = names(fleet.data)[5:ncol(fleet.data)], 
                     variable.name = "Year",
                     value.name = "Group")
  

  temp_colors <- palette(c(rgb(170,167,216, maxColorValue = 255),
  rgb(203,83,53, maxColorValue = 255),
  rgb(113,102,217, maxColorValue = 255),
  rgb(94,176,74, maxColorValue = 255),
  rgb(171,86,192, maxColorValue = 255),
  rgb(188,177,69, maxColorValue = 255),
  rgb(202,71,156, maxColorValue = 255),
  rgb(81,175,138, maxColorValue = 255),
  rgb(213,69,107, maxColorValue = 255),
  rgb(111,124,54, maxColorValue = 255),
  rgb(109,115,189, maxColorValue = 255),
  rgb(197,131,67, maxColorValue = 255),
  rgb(205,135,196, maxColorValue = 255),
  rgb(182,98,112, maxColorValue = 255)))
  
  getPalette = colorRampPalette(brewer.pal(8, "Accent"))
  
  dt_plot <- na.omit(fleet.data[StrategyName == "T4_9 NSMAP 2020_TargetF_Weakest stock"])

  ggsave(filename = "test.pdf", 
         plot = ggplot(data = dt_plot, aes(x = FleetName, fill = factor(Group))) +
           geom_bar(position = "fill") +
           theme_bw() +
           theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9), text = element_text(size = 9), 
                 legend.key.size = unit(0.3, "cm")) +
           labs(y = "Percentage of Time", x = "Fleet", fill = "Functional Group") + 
           scale_fill_manual(values = getPalette(15))
         )
  browser()
  for (G in g){
    
    
    
    fleet.data.whole = as.vector(as.matrix(fleet.data[!is.na(fleet.data$X1),5:ncol(fleet.data)]))
    if(length(fleet.data.whole)==0) next
    
    path = paste(params$plot.path, parents.folder.for.plots, sep="")
    
    plot_pies(path, fleet.data.whole, file.name.without.ext, pie.name)
    
    #chop up by strategy and apply=============================
    strategies = unique(fleet.data$StrategyName)
    
    browser
    
    #filter by strategy and plot
    for(iStrategy in strategies){
      fleet.data.by.strategy = fleet.data[fleet.data$StrategyName==iStrategy,]
      fleet.data.by.strategy = as.vector(as.matrix(fleet.data.by.strategy[!is.na(fleet.data.by.strategy$X1),5:ncol(fleet.data.by.strategy)]))
      if(length(fleet.data.by.strategy)==0) next
      CreateFolderIfDoesntExist(folder.name = iStrategy, path = paste(params$plot.path,parents.folder.for.plots,sep=""))
      path = paste(params$plot.path, parents.folder.for.plots, iStrategy, "/", sep="")
      plot_pies(path, fleet.data.by.strategy, file.name.without.ext, pie_name = pie.name)
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
        plot_pies(path, fleet.data.by.regulation, file.name.without.ext, pie_name = pie.name)
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

