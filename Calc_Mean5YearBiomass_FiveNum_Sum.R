library(data.table)

# FUNCTION START  ===============================================================================================
CreateBiomassFiveNumSum = function(plot.path, area){
  #Tested by hand - correct MP 14/8/17

  unique.groups = LoadUniqueGroups(params$RootPath)
  
  # Loading reference points Blim and Bpa
  biomrefs_csv_inc_path = paste(plot.path,"Biom_refs.csv", sep="")
  if(file.exists(biomrefs_csv_inc_path)){
    biom_refs = read.csv(biomrefs_csv_inc_path)
  } else {
    biom_refs = NA
  }
  
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
    
    #Calc mean last 5 year
    biomass = calcLast5Year(biomass, "biomass.last5yearmean", 4, function.type = "mean")
    
    #add a column with the group name - need this to merge with the strategy data.table
    biomass = appendVariableToDataTable(dt=biomass, variable=igroup, variablename="GroupName", beg=TRUE, end=FALSE)

    #Modify the values so that they are for the entire region
    area <- get_area(plot.path = params$plot.path, file.name = igroup, Area = params$Area)
    biomass$biomass.last5yearmean = biomass$biomass.last5yearmean * area / 1000
    
    #add the group name to a column because I'm going to facet plot using this
    biomass = appendVariableToDataTable(dt = biomass, variable = igroup, variablename = "GroupName", beg=TRUE, end=FALSE)
    
    #Calc the percent below Bpa & Blim
    if(!is.na(biom_refs)){
      bpa = biom_refs[biom_refs$Group==igroup,]$Bpa
      blim = biom_refs[biom_refs$Group==igroup,]$Blim
      biomass$below.bpa = biomass$biomass.last5yearmean<bpa
      biomass$above.bpa = biomass$biomass.last5yearmean>=bpa
      biomass$below.blim = biomass$biomass.last5yearmean<blim
      biomass$above.blim = biomass$biomass.last5yearmean>=blim
    }
    

    #calculate the 5 number summary for each strategy
    if(is.na(biom_refs)){
      biomass.summary.by.strategy = biomass[,list(Min = min(biomass.last5yearmean), 
                                                  LQ = quantile(biomass.last5yearmean, .25, na.rm=TRUE), 
                                                  Median = median(biomass.last5yearmean),
                                                  UQ = quantile(biomass.last5yearmean, .75, na.rm=TRUE),
                                                  Max = max(biomass.last5yearmean),
                                                  Mean = mean(biomass.last5yearmean)), by="StrategyName"]
    } else {
      biomass.summary.by.strategy = biomass[,list(Min = min(biomass.last5yearmean), 
                                                LQ = quantile(biomass.last5yearmean, .25, na.rm=TRUE), 
                                                Median = median(biomass.last5yearmean),
                                                UQ = quantile(biomass.last5yearmean, .75, na.rm=TRUE),
                                                Max = max(biomass.last5yearmean),
                                                Mean = mean(biomass.last5yearmean),
                                                Percent.Below.Bpa = 100 * sum(below.bpa)/length(above.bpa),
                                                Percent.Below.Blim = 100 * sum(below.blim)/length(below.blim)), by="StrategyName"]
    }
    biomass.summary.by.strategy = appendVariableToDataTable(biomass.summary.by.strategy, igroup, "GroupName", beg=TRUE, end=FALSE)
    summary.dt = rbind(summary.dt, biomass.summary.by.strategy)
  }
  
  #finally save the table to csv
  write.csv(summary.dt, file = paste(params$plot.path, "Tables/biomass_5NoSummary.csv", sep=""))
}

# FUNCTION END  ===============================================================================================