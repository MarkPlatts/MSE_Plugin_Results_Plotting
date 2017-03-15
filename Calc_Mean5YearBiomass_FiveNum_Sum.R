# INITIALISATION START ===============================================================================================

#start with a clean sheet
rm(list = ls())

#load sources
# source.folder.location = dirname(sys.frame(1)$ofile)
# setwd(source.folder.location)
setwd("C:/Users/Mark/Desktop/MSE_Plugin_Results_Plotting")
source("share_tools.R")
source("initialisation.R")
#library(reshape)
params = initialise_params("0")

#root results path
#root.plot =     "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/North Sea MultiAnnual Plan/ResultsType1-4_220117/Plots/"
#root.results =  "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/North Sea MultiAnnual Plan/ResultsType1-4_220117/Results/"

hcr.folders = c("C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/NorthSea Model/2015 FINAL Key Run/DATA/HCRs/Type1_BmsytoZero",
                "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/NorthSea Model/2015 FINAL Key Run/DATA/HCRs/Type2_BmsyBlimClifftoZero",
                "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/NorthSea Model/2015 FINAL Key Run/DATA/HCRs/Type3_BmsytoZeroatBlim",
                "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/NorthSea Model/2015 FINAL Key Run/DATA/HCRs/Type4_BmsyBlimClifftoFmin")

groups.for.f.or.biomass = "biomass"

biomrefs_csv_inc_path = paste(params$plot.path,"Biom_refs.csv", sep="")

# INITIALISATION END ===============================================================================================



# FUNCTION START  ===============================================================================================

CreateBiomassFiveNumSum = function(){
  
  params = initialise_params("0")
  
  #get a list of hcrs by listed by strategy and group name with only biomass limits
  # strategies.table = getStrategyTable(hcr.folders)
  # strategies.table = filter(strategies.table, Target_or_Conservation==1)
  # strategies.table = strategies.table[,.(StrategyName, GroupName = GroupNameForBiomass, Limit = get(BLimitType))]#, StepBiomass, UpperLimit)]
  # strategies.table = unique(strategies.table)
  

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
  
  
  #Create a data.table in which to compile all the tables from each file ready for saving to csv
  dt.all = data.table()
  
  for(igroup in unique.groups)
  {

    #get a list of all the files in CatchTrajectories that are for "AllFleets"
    biomass.file = GetFileName_ContainsStrings(FolderPath = paste(params$RootPath, "Biomass/", sep=""),
                                               Strings = c(igroup), WithPath=T)
    
    #Load the file
    biomass = fread(biomass.file, skip=7, header=T)
    
    #Determine file is valid
    if(!isNotAll(dt = biomass, col.data.starts = 4, val.to.check = -9999)) next
    
    #Sum last 5 year
    biomass = calcLast5Year(biomass, "biomass.last5yearmean", 4, function.type = 2)
    
    #add a column with the group name - need this to merge with the strategy data.table
    biomass = appendVariableToDataTable(dt=biomass, variable=igroup, variablename="GroupName", beg=TRUE, end=FALSE)
    
    #****************************************************************************************
    #****************************************************************************************

    #Modify the values so that they are for the entire region
    biomass$biomass.last5yearmean = biomass$biomass.last5yearmean*params$Area/1000
    
    #add the group name to a column because I'm going to facet plot using this
    biomass = appendVariableToDataTable(dt = biomass, variable = igroup, variablename = "GroupName", beg=TRUE, end=FALSE)
    
    #max.axis.x = max(biomass$biomass.last5yearmean)
    
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
    
    #for each group with a blim or bpa create a table with all the labels in it for plotting vertical lines
    #I believe we need to have a unique row for each strategy because that is how ggplot knows how to plot for each strategy plot with facet
    #Extract reference points
    # nStrategies = length(params$strats)
    # if(igroup %in% biom_refs[,"Group"]){
    #   bpa = read_biom_refs(biom_refs, igroup, "bpa")     #Needs to be specified in the file as kt
    #   blim = read_biom_refs(biom_refs, igroup, "blim")   #Needs to be specified in the file as kt
    #   max.axis.x = max(max.axis.x, bpa, blim)
    #   ref.points = data.table(StrategyName = params$strats, 
    #                           bpa = rep(bpa, nStrategies), bpa_lab = rep("Bpa", nStrategies), bpa_lab_pos = rep(bpa - distance.from.vlines, nStrategies), 
    #                           blim = rep(blim, nStrategies), blim_lab = rep("Blim", nStrategies), blim_lab_pos = rep(blim - distance.from.vlines, nStrategies))
    # }
    # 
    # bmedian.dt = data.table(StrategyName = params$strats, 
    #                         bmedian = biomass.summary.by.strategy$Median, 
    #                         bmedian_lab = rep("Median", nStrategies), 
    #                         bmedian_lab_pos = biomass.summary.by.strategy$Median - distance.from.vlines)
    
    
    
    # number_bins = 40
    
    #****************************************************************************************
    #****************************************************************************************
    
    #merge the two together so that we can easily calculate the difference between two columns
    #dt = merge(x=biomass, y=strategies.table, by = c("StrategyName", "GroupName"), all.x = TRUE)
    
    # dt = dt[Limit!="NA"]
    # dt = cbind(dt, data.table(avbiomass.hcr.limit.diff = dt[,biomass.last5yearsum] - dt[,Limit]))
    
    #Check where the catch is greater than or less than the quota
    # dt[, "avbiomass.greaterthan.hcr.upperlimit.diff"] = dt$avbiomass.hcr.limit.diff>=0
    # dt[, "avbiomass.lessthan.hcr.upperlimit.diff"] = dt$avbiomass.hcr.limit.diff<0
    
    #For each strategy count how many models with realisedFs above and below maxFs and merge them together
    # NumberAbove = dt[,.(NumberAbove=sum(avbiomass.greaterthan.hcr.upperlimit.diff)), by=.(StrategyName)]
    # NumberBelow = dt[,.(NumberBelow=sum(avbiomass.lessthan.hcr.upperlimit.diff)), by=.(StrategyName)]
    # dt.counts.temp = merge(NumberAbove,NumberBelow, by = c("StrategyName"))
    
    #Create new column that turns the counts into percentages
    # dt.counts.temp$PercentAbove = dt.counts.temp$NumberAbove/(dt.counts.temp$NumberAbove+dt.counts.temp$NumberBelow)*100
    # dt.counts.temp$PercentBelow = dt.counts.temp$NumberBelow/(dt.counts.temp$NumberAbove+dt.counts.temp$NumberBelow)*100
    # 
    #add column with groupname to differentiate after binding
    #dt.counts.temp = cbind(data.table(GroupName = rep(igroup, dim(dt.counts.temp)[1])), dt.counts.temp) 
    
    #join table to strategy table so we can output the limit
    #dt.counts.temp = merge(dt.counts.temp, strategies.table[,c("StrategyName", "GroupName", "Limit")], by=c("GroupName", "StrategyName"))
    
    #dt.counts.temp = rename(dt.counts.temp, c("Limit" = BLimitType))
    
    #bind them all together ready to be saved to csv
    #dt.all = rbind(dt.all, dt.counts.temp)
    
  }
  
  #finally save the table to csv
  write.csv(summary.dt, file = paste(params$plot.path, "Tables/biomass_5NoSummary.csv", sep=""))
}

# FUNCTION END  ===============================================================================================



# FUNCTION CALLS START ===============================================================================================

# CreatePercentBelowConservationLimits("LowerLimit")
# CreatePercentBelowConservationLimits("UpperLimit")
CreateBiomassFiveNumSum()

# FUNCTION CALLS END ===============================================================================================



# TESTING START ===================================================================================================



# TESTING END ===================================================================================================