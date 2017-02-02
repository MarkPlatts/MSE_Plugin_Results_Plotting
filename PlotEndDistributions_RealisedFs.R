#use data.table because it is more efficient and good practice for big data objects
library(data.table)
library(reshape2)
library(ggplot2)

path2results = "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/North Sea MultiAnnual Plan/ResultsType1 and 3_141216/Results/"

CreateGGPlot = function(realised.f.df){
  plot = ggplot(realised.f.df,aes(x=Realised_F)) + xlim(0,3)
  plot = plot + geom_histogram(binwidth =0.02 ,aes(y=..density..))
  plot = plot + facet_wrap(~StrategyName, ncol=1)
  return(plot)
}

PlotEnd_dist_realisedFs = function(path.to.results.folder){
  
  path.to.results.folder = "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/North Sea MultiAnnual Plan/ResultsType1 and 3_141216/Results/"
  
  #Load the data
  realised.f.df = fread(paste(path.to.results.folder, "RealisedF/RealisedTotalFsYearly_Cod (adult)_GroupNo14.csv", sep=""))
  
  #Name columns
  colnames(realised.f.df) = as.character(realised.f.df[1,])
  realised.f.df = realised.f.df[-1,]
  
  #melt the columns into a better format for analysis etc
  realised.f.df = melt(realised.f.df, id.vars = colnames(realised.f.df)[1:4])
  colnames(realised.f.df)[5:6] = c("TimeStep", "Realised_F")
  
  #Alter variable types of some columns
  realised.f.df$TimeStep = as.numeric(vals$TimeStep)
  realised.f.df$StrategyName = as.factor(vals$StrategyName)
  
  #Output using GGPlot
  realised.F.plot = CreateGGPlot(realised.f.df)
  
  ggsave(realised.F.plot, file=paste(path.to.results.folder, "a_test.png", sep=""), width=6, height=40, limitsize = FALSE)

}

PlotEnd_dist_realisedFs(path2results)

