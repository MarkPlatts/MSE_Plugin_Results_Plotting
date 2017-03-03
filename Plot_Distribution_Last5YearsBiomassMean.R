#==========================================================
#Plot the end distribution for the last 5 year mean biomass
#==========================================================



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
  source("plot_tools.R")
  source("initialisation.R")
  
  params = initialise_params()
  output_folder = paste(params$plot.path,"OUTPUT_END_DISTRIBUTIONS/", sep="")
  biomrefs_csv_inc_path = paste(params$plot.path,"Biom_refs.csv", sep="")
  #root results path
  # root.plot =     "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/North Sea MultiAnnual Plan/ResultsType1-4_220117/Plots/"
  # RootPath =  "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/North Sea MultiAnnual Plan/ResultsType1-4_220117/Results/"
  
  # hcr.folders = c("C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/NorthSea Model/2015 FINAL Key Run/DATA/HCRs/Type1_BmsytoZero",
  #                 "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/NorthSea Model/2015 FINAL Key Run/DATA/HCRs/Type2_BmsyBlimClifftoZero",
  #                 "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/NorthSea Model/2015 FINAL Key Run/DATA/HCRs/Type3_BmsytoZeroatBlim",
  #                 "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/NorthSea Model/2015 FINAL Key Run/DATA/HCRs/Type4_BmsyBlimClifftoFmin")
  
  # INITIALISATION END ===============================================================================================
  
  
  # FUNCTIONS START  ===============================================================================================
  
  isNotGroupToPlot = function(biomass.file.name){
    startloc = str_locate(biomass.file.name, "GroupNo")[1]
    endloc = str_locate(biomass.file.name, ".csv")[1] - 1
    group.groupnoxx.format = str_sub(biomass.file.name, startloc, endloc)
    if(!group.groupnoxx.format %in% params$Groups2Plot) return (TRUE)
    #else
    return(FALSE)
    
  }
  
  # FUNCTIONS END  ===============================================================================================
  
  
  #RootPath =  "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/North Sea MultiAnnual Plan/ResultsType1-4_220117/Results/"
  
  #get a list of all the groups
  unique.groups = LoadUniqueGroups(params$RootPath)
  
  # #get a list of strategy.tables with strategy name
  # strategies.table = getStrategyTable(hcr.folders)
  
  # #remove all conservation hcrs from strategy table
  # strategies.table = filter(strategies.table, Target_or_Conservation==0)
  
  # Loading reference points Blim and Bpa
  biom_refs<-read.csv(biomrefs_csv_inc_path,sep=",", header=TRUE)
  
  summary.dt = data.table()
  
  
  #Configure the tables and variables for calculating percent <Blim & Bpa
  SumBlim=list()  #The number of results that are above the Blim
  percBlim=list() #The percentage of the results above Blim
  SumBpa=list()   #The number of results that are above Bpa
  percBpa=list()  #The percentage of the results above Bpa
  df.perc.Blim = data.table()
  df.perc.Bpa = data.table()
  nUniqueStrategies = unique(params$strats)
  
  for(igroup in unique.groups){
 
    #get the file in HCRQuota_Targ folder that are for "AllFleets"
    biomass.file.name = GetFileName_ContainsStrings(FolderPath = paste(params$RootPath, "Biomass/", sep=""), 
                                                 Strings = c(igroup), WithPath=T)
    
    #check whether group is listed to be plotted
    if(isNotGroupToPlot(biomass.file.name)) next
    
    #load the files and sum
    biomass = calcLast5Year(biomass.file.name, "biomass.last5yearmean", 4, function.type = 2)
    
    #filter for the strategies selected in initialisation.R
    biomass = biomass[biomass$StrategyName %in% params$strats, ]
    
    #Modify the values so that they are for the entire region
    biomass$biomass.last5yearmean = biomass$biomass.last5yearmean*params$Area/1000
    
    #add the group name to a column because I'm going to facet plot using this
    biomass = appendVariableToDataTable(dt = biomass, variable = igroup, variablename = "GroupName", beg=TRUE, end=FALSE)
    
    max.axis.x = max(biomass$biomass.last5yearmean)
    
    #Calc the percent below Bpa & Blim
    bpa = biom_refs[biom_refs$Group=="Cod (adult)",]$Bpa
    blim = biom_refs[biom_refs$Group=="Cod (adult)",]$Blim
    
    biomass$below.bpa = biomass$biomass.last5yearmean<bpa
    biomass$above.bpa = biomass$biomass.last5yearmean>=bpa
    biomass$below.blim = biomass$biomass.last5yearmean<blim
    biomass$above.blim = biomass$biomass.last5yearmean>=blim
    
    biomass.percent.below.refpoints = biomass[,list(bpa = sum(below.bpa)/length(below.bpa), 
                                                    blim = sum(below.blim)/length(below.blim)), by="StrategyName"]
    
    #calculate the 5 number summary for each strategy
    biomass.summary.by.strategy = biomass[,list(Min = min(biomass.last5yearmean), 
                  LQ = quantile(biomass.last5yearmean, .25, na.rm=TRUE), 
                  Median = median(biomass.last5yearmean),
                  UQ = quantile(biomass.last5yearmean, .75, na.rm=TRUE),
                  Max = max(biomass.last5yearmean),
                  Mean = mean(biomass.last5yearmean),
                  Percent.Below.Bpa = sum(below.bpa)/length(above.bpa),
                  Percent.Below.Blim = sum(below.blim)/length(below.blim)), by="StrategyName"]
    biomass.summary.by.strategy = appendVariableToDataTable(biomass.summary.by.strategy, igroup, "GroupName", beg=TRUE, end=FALSE)
    
    summary.dt = rbind(summary.dt, biomass.summary.by.strategy)
    

    
    #nBelow.bpa = length(biomass[biomass.last5yearmean<Bpa, by=""])
    #nBelow.blim = length(biomass[biomass.last5yearmean<Blim])
    
    # for (iStrategy in 1:nUniqueStrategies){
    #   SumBlim[iStrategy]<-list(sum(n[[iStrategy]]$tx1K_km2>=Blim.species))  # percentage of "trials" bigger than Blim 
    #   percBlim[iStrategy]<-list((SumBlim[[iStrategy]]*100)/nrow(n[[iStrategy]]))
    #   
    #   #percentage of iterations=trials bigger than the Bpa for each harvest control rules strategy
    #   #Harvest Control Rule
    #   SumBpa[iStrategy]<-list(sum(n[[iStrategy]]$tx1K_km2>=Bpa.species))  # percentage of "trials" bigger than Bpa 
    #   percBpa[iStrategy]<-list((SumBpa[[iStrategy]]*100)/nrow(n[[iStrategy]]))
    #   
    # }

    #get 10th of plot width to add this to where the vertical lines are
    distance.from.vlines = max.axis.x/100
    
    #for each group with a blim or bpa create a table with all the labels in it for plotting vertical lines
    #I believe we need to have a unique row for each strategy because that is how ggplot knows how to plot for each strategy plot with facet
    #Extract reference points
    if(igroup %in% biom_refs[,"Group"]){

      nStrategies = length(params$strats)
      bpa = read_biom_refs(biom_refs, igroup, "bpa")     #Needs to be specified in the file as kt
      blim = read_biom_refs(biom_refs, igroup, "blim")   #Needs to be specified in the file as kt
      max.axis.x = max(max.axis.x, bpa, blim)

      ref.points = data.table(StrategyName = params$strats, 
                              bpa = rep(bpa, nStrategies), bpa_lab = rep("Bpa", nStrategies), bpa_lab_pos = rep(bpa - distance.from.vlines, nStrategies), 
                              blim = rep(blim, nStrategies), blim_lab = rep("Blim", nStrategies), blim_lab_pos = rep(blim - distance.from.vlines, nStrategies),
                              bmedian = biomass.summary.by.strategy$Median, bmedian_lab = rep("Median", nStrategies), bmedian_lab_pos = biomass.summary.by.strategy$Median - distance.from.vlines)
      
    }
    

    number_bins = 40
    
    #plot them  
    B_SPECIES = ggplot(biomass,aes(x=biomass.last5yearmean))
    B_SPECIES = B_SPECIES + geom_histogram(binwidth = (max.axis.x)/number_bins, aes(y=..density..), colour="darkgreen", fill="lightgreen")
    B_SPECIES = B_SPECIES + facet_wrap(~StrategyName, ncol=1)

    #if the group has a blim and bpa then extract from table and plot
    #species_in_biomrefsfile<-levels(blim.bpa[,"Group"])

    if(igroup %in% biom_refs[,"Group"]){
      max.y = max(ggplot_build(B_SPECIES)$data[[1]]$density)
      B_SPECIES = B_SPECIES + geom_text(data = ref.points, aes(x = blim_lab_pos, y = max.y*9/10, label = blim_lab), size = 2, angle = 90)
      B_SPECIES = B_SPECIES + geom_text(data = ref.points, aes(x = bpa_lab_pos, y = max.y*9/10, label = bpa_lab), size = 2, angle=90)
      B_SPECIES = B_SPECIES + geom_text(data = ref.points, aes(x = bmedian_lab_pos, y = max.y*9/10, label = bmedian_lab), size = 2, angle=90)
      B_SPECIES = B_SPECIES + geom_vline(data =ref.points, aes(xintercept=blim),linetype="dotdash", size=0.15)
      B_SPECIES = B_SPECIES + geom_vline(data =ref.points, aes(xintercept=bpa),linetype="longdash", size=0.15)
      B_SPECIES = B_SPECIES + geom_vline(data =ref.points, aes(xintercept=bmedian),linetype="longdash", size=0.15)
    }
    
    ggsave(B_SPECIES, file=paste(output_folder, "Biomass5YearMean_",igroup,".png", sep=""), width=6, height=length(params$strats)*1.5, limitsize = FALSE)

  }
  
  write.csv(summary.dt, file = "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/North Sea MultiAnnual Plan/ResultsType1-4_220117/Plots/Tables/biomass_5NoSummary.csv")
  

  # B_SPECIES = B_SPECIES + ylab("Density") + xlab(plotdata$xaxis.label[result.index]) #xlab("Minimum Biomass (1000 t)")
  # B_SPECIES = B_SPECIES + labs(title=paste(plotdata$species_in_resultsfile[species.index[1]], "\n", plotdata$result.names[result.index], sep=""))
  # B_SPECIES = B_SPECIES + theme(strip.text = element_text(size=6), panel.background = element_blank(), panel.grid.major=element_blank(), panel.grid.minor=element_blank(),axis.line=element_line(colour="Black"), axis.text=element_text(colour="Black", size=6))
  # if(plotdata$result.names[result.index] =="BiomassEnd" || plotdata$result.names[result.index]=="BiomassMin"){

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
    

    
    #load the files and sum
    realised.f = calcLast5Year(realisedF.file, "realised.f.last5yearsum", 5, function.type = 2)
    
  }
  
  

}


