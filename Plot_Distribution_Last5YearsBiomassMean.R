#==========================================================
#Plot the end distribution for the last 5 year mean biomass
#==========================================================

isNotGroupToPlot = function(biomass.file.name){
  startloc = str_locate(biomass.file.name, "GroupNo")[1]
  endloc = str_locate(biomass.file.name, ".csv")[1] - 1
  group.groupnoxx.format = str_sub(biomass.file.name, startloc, endloc)
  if(!group.groupnoxx.format %in% params$Groups2Plot) return (TRUE)
  #else
  return(FALSE)
  
}

if(TRUE){
  
  # INITIALISATION START ===============================================================================================
  
  #start with a clean sheet
  rm(list = ls())
  
  #load libraries
  library(ggplot2)
  library(stringr)
  
  #load sources
  source.folder.location = dirname(sys.frame(1)$ofile)
  setwd(source.folder.location)
  source("share_tools.R")
  source("initialisation.R")
  
  params = initialise_params()
  output_folder = paste(params$plot.path,"OUTPUT_END_DISTRIBUTIONS/", sep="")
  #root results path
  # root.plot =     "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/North Sea MultiAnnual Plan/ResultsType1-4_220117/Plots/"
  # RootPath =  "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/North Sea MultiAnnual Plan/ResultsType1-4_220117/Results/"
  
  # hcr.folders = c("C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/NorthSea Model/2015 FINAL Key Run/DATA/HCRs/Type1_BmsytoZero",
  #                 "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/NorthSea Model/2015 FINAL Key Run/DATA/HCRs/Type2_BmsyBlimClifftoZero",
  #                 "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/NorthSea Model/2015 FINAL Key Run/DATA/HCRs/Type3_BmsytoZeroatBlim",
  #                 "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/NorthSea Model/2015 FINAL Key Run/DATA/HCRs/Type4_BmsyBlimClifftoFmin")
  
  # INITIALISATION END ===============================================================================================
  
  
  #RootPath =  "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/North Sea MultiAnnual Plan/ResultsType1-4_220117/Results/"
  
  #get a list of all the groups
  unique.groups = LoadUniqueGroups(params$RootPath)
  
  # #get a list of strategy.tables with strategy name
  # strategies.table = getStrategyTable(hcr.folders)
  
  # #remove all conservation hcrs from strategy table
  # strategies.table = filter(strategies.table, Target_or_Conservation==0)
  
  #Create a data.table in which to compile all the tables from each file ready for saving to csv
  dt.all = data.table()
  
  for(igroup in unique.groups)
  {
 
    #get the file in HCRQuota_Targ folder that are for "AllFleets"
    biomass.file.name = GetFileName_ContainsStrings(FolderPath = paste(params$RootPath, "Biomass/", sep=""), 
                                                 Strings = c(igroup), WithPath=T)
    
    #check whether group is listed to be plotted
    if(isNotGroupToPlot(biomass.file.name)) next
    
    #load the files and sum
    biomass = calcLast5Year(biomass.file.name, "biomass.last5yearsum", 4, function.type = 2)
    
    #Modify the values so that they are for the entire region
    biomass$biomass.last5yearsum = biomass$biomass.last5yearsum*params$Area/1000
    
    #add the group name to a column because I'm going to facet plot using this
    biomass = appendVariableToDataTable(dt = biomass, variable = igroup, variablename = "GroupName", beg=TRUE, end=FALSE)
    
    #bind them together ready for plotting
    dt.all = rbind(dt.all, biomass)
    
    max.axis.x = max(biomass$biomass.last5yearsum)
    number_bins = 30
    
    #filter for the strategies selected in initialisation.R
    biomass = biomass[biomass$StrategyName %in% params$strats, ]
    
    #plot them  
    B_SPECIES = ggplot(biomass,aes(x=biomass.last5yearsum))
    B_SPECIES = B_SPECIES + geom_histogram(binwidth = (max.axis.x)/number_bins, aes(y=..density..), colour="darkgreen", fill="darkgreen")
    B_SPECIES = B_SPECIES + facet_wrap(~StrategyName, ncol=1)
    
    ggsave(B_SPECIES, file=paste(output_folder, "Biomass5YearMean_",igroup,".png", sep=""), width=6, height=length(params$strats)*1.5, limitsize = FALSE)

  }
  

  # B_SPECIES = B_SPECIES + ylab("Density") + xlab(plotdata$xaxis.label[result.index]) #xlab("Minimum Biomass (1000 t)")
  # B_SPECIES = B_SPECIES + labs(title=paste(plotdata$species_in_resultsfile[species.index[1]], "\n", plotdata$result.names[result.index], sep=""))
  # B_SPECIES = B_SPECIES + theme(strip.text = element_text(size=6), panel.background = element_blank(), panel.grid.major=element_blank(), panel.grid.minor=element_blank(),axis.line=element_line(colour="Black"), axis.text=element_text(colour="Black", size=6))
  # if(plotdata$result.names[result.index] =="BiomassEnd" || plotdata$result.names[result.index]=="BiomassMin"){
  #   B_SPECIES = B_SPECIES + geom_text(data = plotdata$blim.species3.lab, aes(x = x, y = y, label = lab), size = 2)
  #   B_SPECIES = B_SPECIES + geom_text(data = plotdata$bpa.species3.lab, aes(x = x, y = y, label = lab), size = 2)
  #   B_SPECIES = B_SPECIES + geom_vline(data=plotdata$blim.bpa.xaxis, aes(xintercept=blim.axis),linetype="dotdash", size=0.15)
  #   B_SPECIES = B_SPECIES + geom_vline(data=plotdata$blim.bpa.xaxis, aes(xintercept=bpa.axis),linetype="longdash", size=0.15)
  #   B_SPECIES = B_SPECIES + geom_text(data = plotdata$bpa.species5.lab, aes(x = x, y = y, label = lab), size = 2, angle=90, vjust=-0.5)
  #   B_SPECIES = B_SPECIES + geom_text(data = plotdata$blim.species5.lab, aes(x = x, y = y, label = lab), size = 2, angle=90, vjust=-0.5)
  # }
  # B_SPECIES = B_SPECIES + coord_cartesian(ylim = c(0, as.numeric(plotdata$max.axis$y)*1.1), xlim = c(0, as.numeric(plotdata$max.axis$x)))
  # # median, Blim and Bpa lines and labels
  # B_SPECIES = B_SPECIES + geom_vline(data=plotdata$ResultMedian.species, aes(xintercept=median.species),linetype="dotted", size=0.15)
  # B_SPECIES = B_SPECIES + geom_text(data = plotdata$b.species4.lab, aes(x = x, y = y, label = lab), size = 2, angle=90, vjust=-0.5)
  # if(plotdata$result.names[result.index]=="DiscardMortalities" || plotdata$result.names[result.index]=="DiscardSurvivals" || plotdata$result.names[result.index]=="Landings"){
  #   geom_text(data = plotdata$c.species4.lab, aes(x = x, y = y, label = lab), size = 2, angle=90, vjust=-0.5)+
  #     geom_text(data = plotdata$c.species5.lab, aes(x = x, y = y, label = lab), size = 2, angle=90, vjust=-0.5)+
  #     geom_text(data = plotdata$c.species6.lab, aes(x = x, y = y, label = lab), size = 2, angle=90, vjust=-0.5)
  # }
  

  
}
  



  

  
if(FALSE){
  

  
  #Load up biomass reference file
  biom_refs = read.csv(paste(params$plot.path,"/Biom_refs.csv",sep=''))
  
  #initialise plotting params
  plotting_params = initialise_plotting_params("Biomass", params$plot_each_timestep, params$StartRun_Year, params$EndRun_Year, params$RootPath)
  
  for(G in plotting_params$g){
    
    #Get the filename to be used to check whether yearly in name, to name files of plots and to add text to plots
    FILENAME = substr(G,1,nchar(G)-4)
    
    #Only use year files
    if(IsIncorrectFileType_YearlyMonthly(FILENAME, plot_yearly == T)) next
    
    #Setup the connection for saving plots to file
    png(filename = paste(params$plot.path,"/OUTPUT_END_DISTRIBUTIONS/",FILENAME,"_5YrMean.png",sep=""), res=900, width=8, height=4, units='in')
    
    #Load the data from file
    plotting_params$dat <- read.csv(paste(params$RootPath,"/Biomass/",G, sep=''),skip=7, head=T)
    
    #Modify the values so that they are for the entire region
    plotting_params$dat[,-c(1:4)] <- plotting_params$dat[,-c(1:4)]*params$Area/1000 #Multiplying it by area gives absolute biomass across area
                                                                                    #Dividing by 1000 gives value in kt - we do this to prevent scientific units
    
    Biomass.file = GetFileName_ContainsStrings(FolderPath = paste(params$RootPath, "/Biomass/", sep=""), 
                                                 Strings = c(igroup), WithPath=T)
    
    #Extract reference points
    GroupName = plotting_params$dat[1,1]
    bpa = read_biom_refs(biom_refs, GroupName, "bpa")     #Needs to be specified in the file as kt
    blim = read_biom_refs(biom_refs, GroupName, "blim")   #Needs to be specified in the file as kt
    
    #load the files and sum
    realised.f = calcLast5Year(realisedF.file, "realised.f.last5yearsum", 5, function.type = 2)
    
  }
  
  #Load the biomass data time series
  
  
  #Calculate the last 5 year means for each model,group strategy combination
  
  
  #Get into a format with which to plot
  
  
  #Gather or calc any other values required to plot
  
  
  #Plot the distributions
  
  

}


