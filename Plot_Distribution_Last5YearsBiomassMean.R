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
  # source.folder.location = dirname(sys.frame(1)$ofile)
  # setwd(source.folder.location)
  setwd("C:/Users/Mark/Desktop/MSE_Plugin_Results_Plotting")
  source("share_tools.R")
  source("plot_tools.R")
  source("initialisation_baltic.R")
  
  params = initialise_params(batch = "0")
  output_folder = paste(params$plot.path,"OUTPUT_END_DISTRIBUTIONS/", sep="")
  biomrefs_csv_inc_path = paste(params$plot.path,"Biom_refs.csv", sep="")

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
    #if(igroup != "Cod (adult)") next
    
    #get the file in HCRQuota_Targ folder that are for "AllFleets"
    biomass.file.name = GetFileName_ContainsStrings(FolderPath = paste(params$RootPath, "Biomass/", sep=""), 
                                                 Strings = c(igroup), WithPath=T)
    
    #check whether group is listed to be plotted
    if(isNotGroupToPlot(biomass.file.name)) next
    
    #Load the file
    biomass = fread(biomass.file.name, skip=7, header=T)
    
    #load the files and sum
    biomass = calcLast5Year(biomass, "biomass.last5yearmean", 4, function.type = 2)
    
    #filter for the strategies selected in initialisation.R
    biomass = biomass[biomass$StrategyName %in% params$strats, ]
    
    #Modify the values so that they are for the entire region
    biomass$biomass.last5yearmean = biomass$biomass.last5yearmean*params$Area/1000
    
    #add the group name to a column because I'm going to facet plot using this
    biomass = appendVariableToDataTable(dt = biomass, variable = igroup, variablename = "GroupName", beg=TRUE, end=FALSE)
    
    max.axis.x = max(biomass$biomass.last5yearmean)
    
    #Calc the percent below Bpa & Blim
    bpa = biom_refs[biom_refs$Group==igroup,]$Bpa
    blim = biom_refs[biom_refs$Group==igroup,]$Blim
    
    biomass$below.bpa = biomass$biomass.last5yearmean<bpa
    biomass$above.bpa = biomass$biomass.last5yearmean>=bpa
    biomass$below.blim = biomass$biomass.last5yearmean<blim
    biomass$above.blim = biomass$biomass.last5yearmean>=blim
    
    # biomass.percent.below.refpoints = biomass[,list(bpa = sum(below.bpa)/length(below.bpa), 
    #                                                 blim = sum(below.blim)/length(below.blim)), by="StrategyName"]
    # 
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

    #get 10th of plot width to add this to where the vertical lines are
    distance.from.vlines = max.axis.x/100
    
    #for each group with a blim or bpa create a table with all the labels in it for plotting vertical lines
    #I believe we need to have a unique row for each strategy because that is how ggplot knows how to plot for each strategy plot with facet
    #Extract reference points
    nStrategies = length(params$strats)
    if(igroup %in% biom_refs[,"Group"]){
      bpa = read_biom_refs(biom_refs, igroup, "bpa")     #Needs to be specified in the file as kt
      blim = read_biom_refs(biom_refs, igroup, "blim")   #Needs to be specified in the file as kt
      max.axis.x = max(max.axis.x, bpa, blim)
      ref.points = data.table(StrategyName = params$strats, 
                            bpa = rep(bpa, nStrategies), bpa_lab = rep("Bpa", nStrategies), bpa_lab_pos = rep(bpa - distance.from.vlines, nStrategies), 
                            blim = rep(blim, nStrategies), blim_lab = rep("Blim", nStrategies), blim_lab_pos = rep(blim - distance.from.vlines, nStrategies))
    }
#browser()
    # bmedian.dt = data.table(StrategyName = params$strats, 
    #                         bmedian = biomass.summary.by.strategy$Median, 
    #                         bmedian_lab = rep("Median", nStrategies), 
    #                         bmedian_lab_pos = biomass.summary.by.strategy$Median - distance.from.vlines)
    bmedian.dt = biomass.summary.by.strategy[, bmedian_lab:= "Median"][,bmedian_lab_pos:= biomass.summary.by.strategy$Median - distance.from.vlines]
    
    

    number_bins = 40
    
    #plot them  
    B_SPECIES = ggplot(biomass,aes(x=biomass.last5yearmean)) + labs(title =  paste("Mean Biomass Across the Last 5 Years for", igroup)) +
                                                                    xlab("Biomass (t)") + ylab("Density")
    B_SPECIES = B_SPECIES + geom_histogram(binwidth = (max.axis.x)/number_bins, aes(y=..density..), colour="darkgreen", fill="lightgreen")
    B_SPECIES = B_SPECIES + facet_wrap(~StrategyName, ncol=1)

    #if the group has a blim and bpa then extract from table and plot
    #species_in_biomrefsfile<-levels(blim.bpa[,"Group"])
    max.y = max(ggplot_build(B_SPECIES)$data[[1]]$density)
    
    print(igroup)
    browser()
    
    if(igroup %in% biom_refs[,"Group"]){
      B_SPECIES = B_SPECIES + geom_text(data = ref.points, aes(x = blim_lab_pos, y = max.y*9/10, label = blim_lab), size = 2, angle = 90)
      B_SPECIES = B_SPECIES + geom_text(data = ref.points, aes(x = bpa_lab_pos, y = max.y*9/10, label = bpa_lab), size = 2, angle=90)
      B_SPECIES = B_SPECIES + geom_vline(data =ref.points, aes(xintercept=blim),linetype="dotdash", size=0.15)
      B_SPECIES = B_SPECIES + geom_vline(data =ref.points, aes(xintercept=bpa),linetype="longdash", size=0.15)
    }

    B_SPECIES = B_SPECIES + geom_text(data = bmedian.dt, 
                                      aes(x = bmedian_lab_pos, y = max.y*9/10, label = bmedian_lab), 
                                      size = 2, 
                                      angle=90)

    B_SPECIES = B_SPECIES + geom_vline(data =bmedian.dt, 
                                       aes(xintercept=Median),
                                       linetype="longdash", size=0.15)

    ggsave(B_SPECIES, file=paste(output_folder, "Biomass5YearMean_",igroup,".png", sep=""), width=6, height=length(params$strats)*2, limitsize = FALSE)

  }
  
  #write.csv(summary.dt, file = "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/North Sea MultiAnnual Plan/ResultsType1-4_220117/Plots/Tables/biomass_5NoSummary.csv")
  
  
}


