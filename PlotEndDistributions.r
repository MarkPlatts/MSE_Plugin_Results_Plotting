# Functions ---------------------------------------------------------------
use_GGPLOT = function(plotdata, variable_to_plot, fill_with_variable){

  B_SPECIES = ggplot(plotdata$results.species,aes(x=get(variable_to_plot), fill=get(fill_with_variable)))
  B_SPECIES = B_SPECIES + geom_histogram(binwidth = (plotdata$max.axis$x)/number_bins, aes(y=..density..))
  B_SPECIES = B_SPECIES + facet_wrap(~Strategy, ncol=1)
  B_SPECIES = B_SPECIES + ylab("Density") + xlab(plotdata$xaxis.label[result.index]) #xlab("Minimum Biomass (1000 t)")
  B_SPECIES = B_SPECIES + labs(title=paste(plotdata$species_in_resultsfile[species.index[1]], "\n", plotdata$result.names[result.index], sep=""))
  B_SPECIES = B_SPECIES + theme(strip.text = element_text(size=6), panel.background = element_blank(), panel.grid.major=element_blank(), panel.grid.minor=element_blank(),axis.line=element_line(colour="Black"), axis.text=element_text(colour="Black", size=6))
  if(plotdata$result.names[result.index] =="BiomassEnd" || plotdata$result.names[result.index]=="BiomassMin"){
    B_SPECIES = B_SPECIES + geom_text(data = plotdata$blim.species3.lab, aes(x = x, y = y, label = lab), size = 2)
    B_SPECIES = B_SPECIES + geom_text(data = plotdata$bpa.species3.lab, aes(x = x, y = y, label = lab), size = 2)
    B_SPECIES = B_SPECIES + geom_vline(data=plotdata$blim.bpa.xaxis, aes(xintercept=blim.axis),linetype="dotdash", size=0.15)
    B_SPECIES = B_SPECIES + geom_vline(data=plotdata$blim.bpa.xaxis, aes(xintercept=bpa.axis),linetype="longdash", size=0.15)
    B_SPECIES = B_SPECIES + geom_text(data = plotdata$bpa.species5.lab, aes(x = x, y = y, label = lab), size = 2, angle=90, vjust=-0.5)
    B_SPECIES = B_SPECIES + geom_text(data = plotdata$blim.species5.lab, aes(x = x, y = y, label = lab), size = 2, angle=90, vjust=-0.5)
  }
  B_SPECIES = B_SPECIES + coord_cartesian(ylim = c(0, as.numeric(plotdata$max.axis$y)*1.1), xlim = c(0, as.numeric(plotdata$max.axis$x)))
  # median, Blim and Bpa lines and labels
  B_SPECIES = B_SPECIES + geom_vline(data=plotdata$ResultMedian.species, aes(xintercept=median.species),linetype="dotted", size=0.15)
  B_SPECIES = B_SPECIES + geom_text(data = plotdata$b.species4.lab, aes(x = x, y = y, label = lab), size = 2, angle=90, vjust=-0.5)
  if(plotdata$result.names[result.index]=="DiscardMortalities" || plotdata$result.names[result.index]=="DiscardSurvivals" || plotdata$result.names[result.index]=="Landings"){
    geom_text(data = plotdata$c.species4.lab, aes(x = x, y = y, label = lab), size = 2, angle=90, vjust=-0.5)+
    geom_text(data = plotdata$c.species5.lab, aes(x = x, y = y, label = lab), size = 2, angle=90, vjust=-0.5)+
    geom_text(data = plotdata$c.species6.lab, aes(x = x, y = y, label = lab), size = 2, angle=90, vjust=-0.5)
  }
  return(B_SPECIES)
}

#Adds the regulation to a dataframe if it is present in the strategy name
Add_Reg = function(list2ammend){
  list2ammend$Regulation = "Other"
  list2ammend$Regulation[grep("Highest value", list2ammend$Strategy)] = "Highest value"
  list2ammend$Regulation[grep("Weakest stock", list2ammend$Strategy)] = "Weakest stock"
  list2ammend$Regulation[grep("Selective", list2ammend$Strategy)] = "Selective"
  list2ammend$Regulation = as.factor(list2ammend$Regulation)
  return(list2ammend)
}

### Select name of species you would like to analyse
species.select <- function(species_in_resultsfile, species_in_biomrefsfile){	# function returns a vector with numerical species indices
  # corresponding to the species index in each data file
  species.input.exists_in_resultscsv<-FALSE	# logic labels telling us if we have selected a valid species
  species.input.exists_in_biomrefs<-FALSE
  
  if(interactive()) {	# need to run this interactively to enter species name
    
    found_in_both_resultscsv_and_biomrefs = FALSE
    
    while(!found_in_both_resultscsv_and_biomrefs)
    {	# continue until we have found the correct species
      
      species.input<-readline("Input species name: ")
      
      list[species.input.exists_in_resultscsv,species.index.where_in_resultscsv_list] = find_species_in_file(species.input, species_in_resultsfile, "Results.csv")
      list[species.input.exists_in_biomrefs, species.index.where_in_biomrefscsv_list] = find_species_in_file(species.input, species_in_biomrefsfile, "Biom_refs.csv")
      
      if(species.input.exists_in_resultscsv & species.input.exists_in_biomrefs) found_in_both_resultscsv_and_biomrefs = TRUE
      
    }
    
    print(c("Selected species from Results.csv is: ", species_in_resultsfile[species.index.where_in_resultscsv_list]))
    print(c("Species name from  Blim.Bpa.csv: ", species_in_biomrefsfile[species.index.where_in_biomrefscsv_list]))    
    
  } else {
    
    print('You need to run this script in interactive mode.')
    
  }
  
  return(c(species.index.where_in_resultscsv_list, species.index.where_in_biomrefscsv_list))	# function returns a vector with numerical species indeces
  
}


find_species_in_file = function(species.input, species_list_in_file, filename){
  #does this species exist in file, i.e. Results.csv?
  
  species.index.where_in_list<-grep(as.character(species.input), ignore.case=TRUE, species_list_in_file)
  
  if (length(species.index.where_in_list)==0) {			# no species found
    print(paste("No matching species found in ", filename))
    species.input.exists = FALSE
  }
  
  if (length(species.index.where_in_list)==1) {			# found exactly one species from Results.csv
    species.input.exists= TRUE
  }
  
  if (length(species.index.where_in_list)>1) {			# more than one species found
    list[species.input.exists,species.index.where_in_list] <- select_which_in_list(species.index.where_in_list, species_list_in_file)
  }
  
  return(list(species.input.exists,species.index.where_in_list))
  
}


select_which_in_list = function(species.index.where_in_resultscsv_list, species_list_in_file){
#if found more than one species with name in list get user to choose which one

  species.select.3 = FALSE
  print(c("Found more than one species containing this name"))
  
  #BUG 13/1/17 why does species_in_resultsfile not get passed in
  Possible_Species<-species_list_in_file[species.index.where_in_resultscsv_list]		# generate a data.frame to display species to choose from
  display.data.frame<-data.frame(Possible_Species, species.index.where_in_resultscsv_list)
  while(!species.select.3){			# select one from shortlist
    print(c("Please select index from:"))
    print(display.data.frame)
    select.index<-readline("Select species number ")

    valid<-(match(select.index, species.index.where_in_resultscsv_list))	# have we selected a valid index?
    if (!is.na(valid)){
      species.input.exists_in_list<-TRUE
      species.select.3<-TRUE
      species.index.where_in_list<-as.integer(select.index)		# set species.index.where_in_resultscsv_list to the species we are working with now
    }  
  }
  return(list(species.input.exists_in_list,species.index.where_in_list))
}






### Scale axes
scale.axis.nice <- function(max) {

  scale.base=c(1,2,2.5,3,5,7.5,10)		# let's use scales whose maximum is a multiple of the scale.base, e.g. 100, 200, or 250	
  
  exp<-0
  base.index<-1
  flag<-c(FALSE, FALSE)
  while(flag[1]!=TRUE){
    if(max>10^exp && max<10^(exp+1)) {	# we found the correct range between 1eX and 1e(X+1), e.g. 100 & 1000
      flag[1]<-TRUE
      while(flag[2]!=TRUE){			# now let's find a "nice" scale based on the scale.base sequence
	      if(max>scale.base[base.index]*10^exp) {	# smaller than the next value based on the scale.base sequence
	        base.index<-base.index+1
	      } else {
	        flag[2]<-TRUE
	      }
      }
    } else {
      if(max<10^(exp+1)) {			# we need to check whether the exponent needs to be increased or decreased to 	find the correct range
	      exp<-exp-1
      }
      else {
        exp<-exp+1
      }
    }
  }  
  max.nice<-scale.base[base.index]*10^exp
  
  max.nice
}


scale.axes <- function(data.set, median, a, b) {
  
# find the maximum of density functions
  max.xaxis<-as.numeric(format(max(data.set$x), scientific=TRUE))
  max.yaxis<-as.numeric(format(max(data.set$y), scientific=TRUE))
  
# sometimes median, Blim or Bpa are larger than the max of the density function!
  max.xaxis<-max(max.xaxis, median[,2], a, b)

# now scale "nicely"  
  scale<-c(scale.axis.nice(max.xaxis), scale.axis.nice(max.yaxis))

  return(scale)
  
}

scale.axes.hist <- function(data, median, a, b, nbins, nUniqueStrategies) {

  #calculate what the x limits are
  max.xaxis<-as.numeric(format(max(data[[1]]$tx1K_km2, median[,2], a, b), scientific=TRUE))
  if (length(data)>1){
    for (iStrategy in 2:nUniqueStrategies){
      max.xaxis<-as.numeric(format(max(max.xaxis, data[[iStrategy]]$tx1K_km2), scientific=TRUE))
    }
  }
  min.xaxis<- 0
  
  #calc size of breaks
  sizeofbreak = (max.xaxis-min.xaxis)/nbins #calc size of bins
  #max.xaxis<-max(max.xaxis, median[,2], a, b)
  
  #calculate for data what maximum y value is
  breaks=seq(min.xaxis, max.xaxis, sizeofbreak) #determine a vector of where the bins should begin

  max.yaxis=-9999
  for (iStrategy in 1:nUniqueStrategies){
    datacut = cut(data[[iStrategy]]$tx1K_km2, breaks, right=FALSE)
    freqtab = table(datacut) #create the table
    freqvec = as.numeric(freqtab)
    propvec = freqvec/sum(freqvec)  #convert into proportions
    densityvec = propvec/sizeofbreak
    max.yaxis = as.numeric(format(max(max.yaxis,densityvec), scientific=TRUE))
    
  }
  
  return(list(x=max.xaxis, y=max.yaxis))

}

### select the ResultName
result.name.select <- function(result.names) {
  Resultname<-result.names		# generate a data.frame to display ResultNames to choose from
  Index<-c(1:length(result.names))
  display.data.frame<-data.frame(Resultname, Index)

  result.select<-FALSE

  while(!result.select){		# select one from shortlist
    print(c("Please select Result name from:"))
    print(display.data.frame)
    result.index<-as.numeric(readline("Enter index number "))
    valid<-(match(result.index, Index))	# have we selected a valid index?
    if (!is.na(valid)){
      result.select<-TRUE
    }
  }
  
  result.index				# return result.index

}

### select the MethodName
method.name.select <- function(method.names) {

  Methodname<-method.names		# generate a data.frame to display ResultNames to choose from
  Index<-c(1:length(method.names))
  display.data.frame<-data.frame(Methodname, Index)

  method.select<-FALSE

  while(!method.select){		# select one from shortlist
    print(c("Please select fisheries method from:"))
    print(display.data.frame)
    method.index<-as.numeric(readline("Enter index number "))
    valid<-(match(method.index, Index))	# have we selected a valid index?
    if (!is.na(valid)){
      method.select<-TRUE
    }
  }
  
  method.index				# return method.index

}


