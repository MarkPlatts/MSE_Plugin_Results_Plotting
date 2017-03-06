#Graphs frotx1K_km2m results of the Harvest Control Rules - Steve Mack, Mark Platts
#generic code - Silvia Hadeler March 2013
#print(this.dir)

# Initialisation ----------------------------------------------------------

#to clear any previous calculation which may interfere with the current one 
rm(list=ls())

#load libraries
#loading packages which will be used in the following calculations
library(ggplot2)
library(dplyr)
#library(plyr)

#load sources
this.dir <- dirname(sys.frame(1)$ofile)
setwd(this.dir)
source("PlotEndDistributions.r")
source("https://raw.githubusercontent.com/ggrothendieck/gsubfn/master/R/list.R")

#Set paths
rootpath = "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/North Sea MultiAnnual Plan/ResultsType1-4_220117/"
results_folder_path = paste(rootpath,"Results/",sep="")
results_csv_inc_path = paste(results_folder_path,"Results.csv", sep="")
plots_folder_path = paste(rootpath,"Plots/",sep="")
biomrefs_csv_inc_path = paste(plots_folder_path,"Biom_refs.csv", sep="")
output_folder = paste(plots_folder_path,"OUTPUT_END_DISTRIBUTIONS/", sep="")
output_csv_folder = paste(plots_folder_path,"Tables/", sep="")

#init plot variables
plotdata = list()
number_bins = 30


#FUNCTIONS =========================================================================================================================
PrepareResults = function(results)
{
  results.t<- as.matrix(as.numeric(results$Value)) # biomass and catch in t/km^2
  results.t<-results.t * 570
  results.t<-cbind(results, results.t)
  colnames(results.t)<-c("Iteration","Strategy","GroupNumber", "GroupName","ResultName","value_t","tx1K_km2")
  return(results.t)
}

# =========================================================================================================================



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~ start programme ~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

############### Data manipulation ############################################################

#adding extra colum to facilitate further manipulation
# Transforming Biomass and catch from t/km^2 into 1000 t and add as another column to the data
# to convert minimum biomass and catch from  t/km2 into 1000t (ie. kTon) multiply by the area
# of North Sea = 570000 km2 and divide by 1000, i.e. multiply by 570
results<-read.csv(results_csv_inc_path, sep=",", header=TRUE, skip=7)
results.t = PrepareResults(results)

# Loading reference points Blim and Bpa
blim.bpa<-read.csv(biomrefs_csv_inc_path,sep=",", header=TRUE)

# IMPORTANT: from now we are working with results.t, last column contains biomass and catch in kTons 

##########  SELECT RESULTS  #######################################################################

# which ResultNames do we have in Results.csv?
plotdata$result.names<-levels(results.t$ResultName)
#select ResultName
result.index<-result.name.select(plotdata$result.names)	# returns index of chosen ResultName in plotdata$result.names

#######################################
########## SELECT SPECIES #############
#######################################
### minimum biomass reference points, percentages re Bli and Bpa, catch quantiles
if (plotdata$result.names[result.index]=="BiomassEnd" || plotdata$result.names[result.index]=="BiomassMin" ||
    plotdata$result.names[result.index]=="DiscardMortalities" || plotdata$result.names[result.index]=="DiscardSurvivals" ||
    plotdata$result.names[result.index]=="Landings"){
  # which species do we have in Results.csv?
  plotdata$species_in_resultsfile<-levels(results.t[["GroupName"]]) # they are ordered alphabetically 
  # which species do we have in blim.bpa.csv
  species_in_biomrefsfile<-levels(blim.bpa[,"Group"])
  
  #select species
  species.index<-as.numeric(species.select(plotdata$species_in_resultsfile, species_in_biomrefsfile))
  
  # minimum biomass, catch etc. in ton x10^3 (=1kTon) for all North Sea
  plotdata$results.species<-subset(results.t, ResultName==plotdata$result.names[result.index] & GroupName==plotdata$species_in_resultsfile[species.index[1]])
  
}

#######################################
########## SELECT METHOD ##############
#######################################
### data related to fishery methods require a different approach to fish stock data
if (plotdata$result.names[result.index]=="TotalEndValue"){
  # which methods do we have in Results.csv?
  # this requires several steps, don't ask why this is in R!
  temp1<-subset(results.t, ResultName==plotdata$result.names[result.index]) # subset containing TotalEndValue
  temp2<-temp1[,4] # 3rd column contains fisheries method
  temp3<-levels(droplevels(temp2)) # now drop the fish stock levels which we don't need.
  
  method.index<-as.numeric(method.name.select(temp3))
  plotdata$results.species<-subset(results.t, ResultName==plotdata$result.names[result.index] & GroupName==temp3[method.index])
}

##checking functions
#head(plotdata$results.species)
#range(plotdata$results.species$tx1K_km2)

#######################################
######### RESULTS STATISTICS ##########
#######################################

### mean, median and quantiles of biomass, catch etc.
#ResultMean.species <- ddply(plotdata$results.species, .(Strategy), summarise, mean.species=mean(tx1K_km2))
#ResultMean.species
plotdata$ResultMedian.species<-ddply(plotdata$results.species, .(Strategy), summarise, median.species=median(tx1K_km2))
#plotdata$ResultMedian.species
#ResultQuantile.species<-ddply(plotdata$results.species, .(Strategy), summarise, quantile.species=quantile(tx1K_km2))
ResultQuantile.species<-ddply(plotdata$results.species, .(Strategy, GroupName), function(x) quantile(x$tx1K_km2))
#ResultQuantile.species

UniqueGroupNames = unique(subset(results.t, ResultName=="BiomassMin")$GroupName)
UniqueFleetNames = unique(subset(results.t, ResultName=="TotalEndValue")$GroupName)
UniqueStrategyNames = levels(results$Strategy)
NumberUniqueStrategyNames = length(UniqueStrategyNames)

n=list()


if (plotdata$result.names[result.index]=="BiomassEnd" || plotdata$result.names[result.index]=="BiomassMin" ||
    plotdata$result.names[result.index]=="DiscardMortalities" || plotdata$result.names[result.index]=="DiscardSurvivals" ||
    plotdata$result.names[result.index]=="Landings"){
  ### Harvest Control Rule 1 data
  for (iStrategy in 1:NumberUniqueStrategyNames){
    n[iStrategy]<-list(as.data.frame(subset(results.t,ResultName==plotdata$result.names[result.index] & GroupName==plotdata$species_in_resultsfile[species.index[1]] & Strategy==UniqueStrategyNames[iStrategy])))
  }
  #n1<-as.data.frame(subset(results.t,ResultName==plotdata$result.names[result.index] & GroupName==plotdata$species_in_resultsfile[species.index[1]] & HCR=="HCR1"))
  
  ### Harvest Control Rule 2
  #n2<-as.data.frame(subset(results.t,ResultName==plotdata$result.names[result.index] & GroupName==plotdata$species_in_resultsfile[species.index[1]] & HCR=="HCR2"))
} else {
  ### Harvest Control Rule 1 data
  for (iStrategy in 1:NumberUniqueStrategyNames){
    n[iStrategy]<-list(as.data.frame(subset(results.t,ResultName==plotdata$result.names[result.index] & GroupName==temp3[method.index] & Strategy==UniqueStrategyNames[iStrategy])))
  }
  #n1<-as.data.frame(subset(results.t,ResultName==plotdata$result.names[result.index] & GroupName==temp3[method.index] & HCR=="HCR1"))
  
  ### Harvest Control Rule 2
  #n2<-as.data.frame(subset(results.t,ResultName==plotdata$result.names[result.index] & GroupName==temp3[method.index] & HCR=="HCR2"))
}

### Blim, Bmax and catch reference points
# reference points Blim and Bmax from excel file (NorthSea_HCRinfo_SteveUpdate.xlsx)                                                  
if (plotdata$result.names[result.index]=="BiomassEnd" || plotdata$result.names[result.index]=="BiomassMin" ||
    plotdata$result.names[result.index]=="DiscardMortalities" || plotdata$result.names[result.index]=="DiscardSurvivals" ||
    plotdata$result.names[result.index]=="Landings"){

  Blim.species<-as.vector(subset(blim.bpa, Group==levels(blim.bpa[,1])[species.index[2]])[1,2])
  Bpa.species<-as.vector(subset(blim.bpa, Group==levels(blim.bpa[,1])[species.index[2]])[1,3])
  Catch.species<-as.vector(subset(blim.bpa, Group==levels(blim.bpa[,1])[species.index[2]])[1,5])
  
  ### percentage of iterations=trials bigger than the Blim for each harvest control rules strategy
  # note that in the following 3 lines hc1 was capitalised HC1
  SumBlim=list()  #The number of results that are above the Blim
  percBlim=list() #The percentage of the results above Blim
  SumBpa=list()   #The number of results that are above Bpa
  percBpa=list()  #The percentage of the results above Bpa
  
  df.perc.Blim = data.frame("StrategyName" = NULL, "Group" = NULL, "Percentage.Below" = NULL)
  df.perc.Bpa = data.frame("StrategyName" = NULL, "Group" = NULL, "Percentage.Below" = NULL)
  
  for (iStrategy in 1:NumberUniqueStrategyNames){
    SumBlim[iStrategy]<-list(sum(n[[iStrategy]]$tx1K_km2>=Blim.species))  # percentage of "trials" bigger than Blim 
    percBlim[iStrategy]<-list((SumBlim[[iStrategy]]*100)/nrow(n[[iStrategy]]))
    
    
    #percentage of iterations=trials bigger than the Bpa for each harvest control rules strategy
    #Harvest Control Rule
    SumBpa[iStrategy]<-list(sum(n[[iStrategy]]$tx1K_km2>=Bpa.species))  # percentage of "trials" bigger than Bpa 
    percBpa[iStrategy]<-list((SumBpa[[iStrategy]]*100)/nrow(n[[iStrategy]]))
    
  }
  
}

#######################################
########## PLOT PREPARATION ###########
########### KERNEL DENSITTY ###########
#######################################

# adding the individual info of each facet graph in form of data frames

# find appropriate axes scales
if (plotdata$result.names[result.index]=="BiomassEnd" || plotdata$result.names[result.index]=="BiomassMin" ||
    plotdata$result.names[result.index]=="DiscardMortalities" || plotdata$result.names[result.index]=="DiscardSurvivals" ||
    plotdata$result.names[result.index]=="Landings") {
  #max.axes.1=scale.axes(density(n1$tx1K_km2), plotdata$ResultMedian.species, Blim.species, Bpa.species)
  #max.axes.2=scale.axes(density(n2$tx1K_km2), plotdata$ResultMedian.species, Blim.species, Bpa.species)
  plotdata$max.axis = scale.axes.hist(n, plotdata$ResultMedian.species, Blim.species, Bpa.species, number_bins, NumberUniqueStrategyNames)
  #max.axes.2 = scale.axes.hist(n2$tx1K_km2, plotdata$ResultMedian.species, Blim.species, Bpa.species,number_bins)
} else {	# we are plotting catch data which plots quantile lines
  plotdata$max.axis = scale.axes.hist(n, plotdata$ResultMedian.species, ResultQuantile.species[4,2], ResultQuantile.species[7,2], number_bins, NumberUniqueStrategyNames)
  #max.axes.2=scale.axes.hist(n2$tx1K_km2, n2$tx1K_km2, plotdata$ResultMedian.species,, ResultQuantile.species[7,2], number_bins)
  
}

# choose the maximum values of x- and y-axis scales to plot both graphs on the same scale later
#max.xaxis<-max(max.axes.1[1], max.axes.2[1])
#max.yaxis<-max(max.axes.1[2], max.axes.2[2])

# xaxis labels for the different ResultNames
plotdata$xaxis.label<-c("End Biomass (1000 t)", "Minimum Biomass (1000 t)", "Final Catch (1000 t)", "Total End Value (1000 Euros)")

# generate formated strings from numerical values for ResultMedian, percentages larger than Blim and Bpa and Catch
plotdata$ResultMedian.species.hcr.string = list()
for (iStrategy in 1:NumberUniqueStrategyNames){
  plotdata$ResultMedian.species.hcr.string[iStrategy]<-list(format(plotdata$ResultMedian.species[iStrategy,2], digits=3, nsmall=0))
}
#plotdata$ResultMedian.species.hc1.string<-format(plotdata$ResultMedian.species[1,2], digits=3, nsmall=0)
#plotdata$ResultMedian.species.hc2.string<-format(plotdata$ResultMedian.species[2,2], digits=3, nsmall=0)

Larger.than.Blim.hcr.string = list()
Larger.than.Bpa.hcr.string = list()
if (plotdata$result.names[result.index]=="BiomassEnd" || plotdata$result.names[result.index]=="BiomassMin" ||
    plotdata$result.names[result.index]=="DiscardMortalities" || plotdata$result.names[result.index]=="DiscardSurvivals" ||
    plotdata$result.names[result.index]=="Landings"){ # i.e. 1 or 2
  for (iStrategy in 1:NumberUniqueStrategyNames){
    Larger.than.Blim.hcr.string[iStrategy]<-format(percBlim[iStrategy], digits=1, nsmall=0)
    Larger.than.Bpa.hcr.string[iStrategy]<-format(percBpa[iStrategy], digits=1, nsmall=0)
  }
  #Larger.than.Blim.hc1.string<-format(percBlim.hc1, digits=1, nsmall=0)
  #Larger.than.Blim.hc2.string<-format(percBlim.hc2, digits=1, nsmall=0)
  #Larger.than.Bpa.hc1.string<-format(percBpa.hc1, digits=1, nsmall=0)
  #Larger.than.Bpa.hc2.string<-format(percBpa.hc2, digits=1, nsmall=0)
}  
# do i need the following?
#Catch.string<-format(percBlim.hc1, digits=1, nsmall=0)

# generate the label positions according to ResultMedian, Blim, Bpa and catch quantiles
bmedian.label.xpos=list()
for (iStrategy in 1:NumberUniqueStrategyNames){
  bmedian.label.xpos[[iStrategy]]<-c(as.numeric(format(plotdata$ResultMedian.species[iStrategy,2], digits=1, nsmall=0)))
  
}
#bmedian.label.xpos<-c(as.numeric(format(plotdata$ResultMedian.species[1,2], digits=1, nsmall=0)),as.numeric(format(plotdata$ResultMedian.species[2,2], digits=1, nsmall=0)))
bmedian.label.ypos<-c(0.75*as.numeric(plotdata$max.axis$y))

if (plotdata$result.names[result.index]=="BiomassEnd" || plotdata$result.names[result.index]=="BiomassMin" ||
    plotdata$result.names[result.index]=="DiscardMortalities" || plotdata$result.names[result.index]=="DiscardSurvivals" ||
    plotdata$result.names[result.index]=="Landings"){ # i.e. 1 or 2
  blim.label.xpos<-c(Blim.species)
  blim.label.ypos<-c(0.5*as.numeric(plotdata$max.axis$y))
  
  bpa.label.xpos<-c(Bpa.species)
  bpa.label.ypos<-c(0.25*as.numeric(plotdata$max.axis$y))
}

lower.quantile.label.xpos<-vector()
upper.quantile.label.xpos<-vector()

for (iStrategy in 1:NumberUniqueStrategyNames){
  lower.quantile.label.xpos[iStrategy]<-c(ResultQuantile.species[(5*iStrategy-3),2])
  upper.quantile.label.xpos[iStrategy]<-c(ResultQuantile.species[(5*iStrategy-1),2])
}

lower.quantile.label.ypos<-c(0.25*as.numeric(plotdata$max.axis$y))
upper.quantile.label.ypos<-c(0.75*as.numeric(plotdata$max.axis$y))

#lower.quantile.label.xpos<-c(ResultQuantile.species[2,2], ResultQuantile.species[7,2])
#lower.quantile.label.ypos<-c(0.25*as.numeric(plotdata$max.axis$y))
#upper.quantile.label.xpos<-c(ResultQuantile.species[4,2], ResultQuantile.species[9,2])
#upper.quantile.label.ypos<-c(0.75*as.numeric(plotdata$max.axis$y))

# position the 4 percentage labels towards the right along the x-axis and at 65% and 75% along y-axis
larger.than.xpos<-c(rep(as.numeric(plotdata$max.axis$x)*.85,NumberUniqueStrategyNames*2))	
larger.than.ypos<-c(as.numeric(plotdata$max.axis$y)*0.60, as.numeric(plotdata$max.axis$y)*0.80)
#larger.than.ypos<-c(as.numeric(plotdata$max.axis$y)*0.65, as.numeric(plotdata$max.axis$y)*0.75, as.numeric(plotdata$max.axis$y)*0.65, as.numeric(plotdata$max.axis$y)*0.75)

# position the catch label towards the right along the x-axis
catch.xpos<-c(plotdata$max.axis$x*0.55) 
catch.ypos<-c(plotdata$max.axis$y*4)

if (plotdata$result.names[result.index]=="DiscardMortalities" || plotdata$result.names[result.index]=="DiscardSurvivals" ||
    plotdata$result.names[result.index]=="Landings"){
  # labels for catch plots#####################change HCR 1 and 2 to the name given
  catch.species3.lab = data.frame()
  for (iStrategy in 1:NumberUniqueStrategyNames){
    catch.species3.lab<- rbind(catch.species3.lab, data.frame(x = catch.xpos, # x = c(rep(270, 2)), 
                                                              y = catch.ypos, # y = c(0.0075,0.0075),
                                                              lab = paste('Median Catch (1991-2007)=', Catch.species,'(*1000 t)'),
                                                              Strategy= UniqueStrategyNames[[iStrategy]]))
  }
  
  catch.species3.lab$lab <- as.character(catch.species3.lab$lab)    # convert labels to character
}
###############################

if (plotdata$result.names[result.index]=="BiomassEnd" || plotdata$result.names[result.index]=="BiomassMin" ||
    plotdata$result.names[result.index]=="DiscardMortalities" || plotdata$result.names[result.index]=="DiscardSurvivals" ||
    plotdata$result.names[result.index]=="Landings"){ # i.e. 1 or 2
  # labels "% trials larger than Blim/Bpa" 
  plotdata$blim.species3.lab=data.frame()
  plotdata$bpa.species3.lab=data.frame()
  plotdata$blim.species5.lab=data.frame()
  plotdata$bpa.species5.lab=data.frame()
  for (iStrategy in 1:NumberUniqueStrategyNames){
    plotdata$blim.species3.lab<- rbind(plotdata$blim.species3.lab, data.frame(x = larger.than.xpos[iStrategy*2-1], # c(rep(1000, 4)), 
                                                            y = larger.than.ypos[1], # c(0.0015, 0.0010,0.0015,0.0010),
                                                            lab = paste(Larger.than.Blim.hcr.string[[iStrategy]],'% trials > Blim'),
                                                            Strategy= UniqueStrategyNames[[iStrategy]]))
    
    
    plotdata$bpa.species3.lab<- rbind(plotdata$bpa.species3.lab, data.frame(x = larger.than.xpos[iStrategy*2], # c(rep(1000, 4)), 
                                                          y = larger.than.ypos[2], # c(0.0015, 0.0010,0.0015,0.0010),
                                                          lab = paste(Larger.than.Bpa.hcr.string[[iStrategy]],'% trials > Bpa'),
                                                          Strategy= UniqueStrategyNames[[iStrategy]]))
    
    plotdata$blim.species5.lab <- rbind(plotdata$blim.species5.lab, data.frame(x= blim.label.xpos, y = blim.label.ypos, lab='Blim', Strategy=UniqueStrategyNames[[iStrategy]]))
    plotdata$bpa.species5.lab <- rbind(plotdata$bpa.species5.lab, data.frame(x= bpa.label.xpos, y = bpa.label.ypos, lab='Bpa', Strategy=UniqueStrategyNames[[iStrategy]]))
    
    #plotdata$blim.species5.lab[[iStrategy]]<- data.frame(x= c(blim.label.xpos, bpa.label.xpos), y = c(blim.label.ypos, bpa.label.ypos), lab=c('Blim', 'Bpa'), Strategy=UniqueStrategyNames[[iStrategy]] )
    #plotdata$blim.species5.lab[[iStrategy]]$lab <- as.character(b.species5.lab[[iStrategy]]$lab)
  }
  plotdata$blim.species3.lab$lab <- as.character(plotdata$blim.species3.lab$lab)    # convert labels to character
  plotdata$bpa.species3.lab$lab <- as.character(plotdata$bpa.species3.lab$lab)
  plotdata$blim.species5.lab$lab <- as.character(plotdata$blim.species5.lab$lab)
  plotdata$bpa.species5.lab$lab <- as.character(plotdata$bpa.species5.lab$lab)
  #b.species3.lab[iStrategy]<- data.frame(x = c(larger.than.xpos[iStrategy*2-1],larger.than.xpos[iStrategy*2]), # c(rep(1000, 4)), 
  #                 y = larger.than.ypos, # c(0.0015, 0.0010,0.0015,0.0010),
  #                 lab = c(paste(Larger.than.Blim.hcr.string,'% trials > Blim'), paste(Larger.than.Bpa.hcr.string,'% trials > Bpa'), paste(Larger.than.Blim.hc2.string,'% trials > Blim'), paste(Larger.than.Bpa.hc2.string,'% trials > Bpa')),
  #                 Strategy= c(StrategyNames[1], StrategyNames[1], StrategyNames[2], StrategyNames[2]))
  #b.species3.lab$lab <- as.character(b.species3.lab$lab)    # convert labels to character
  
  # labels for Blim and Bpa lines
  #b.species5.lab<- data.frame(x= c(blim.label.xpos, bpa.label.xpos), y = c(blim.label.ypos, bpa.label.ypos), lab=c('Blim', 'Bpa'), Strategy=UniqueStrategyNames[[iStrategy]] )
  #b.species5.lab$lab <- as.character(b.species5.lab$lab)    # convert labels to character
  
}

plotdata$b.species4.lab=data.frame()
c.species4.lab=data.frame()
c.species5.lab=data.frame()
c.species6.lab=data.frame()

for (iStrategy in 1:NumberUniqueStrategyNames){
  plotdata$b.species4.lab <- rbind(plotdata$b.species4.lab, data.frame(x=bmedian.label.xpos[[iStrategy]], y=bmedian.label.ypos, lab='B median', Strategy=UniqueStrategyNames[[iStrategy]]))
  c.species4.lab <- rbind(c.species4.lab, data.frame(x=bmedian.label.xpos[[iStrategy]], y=bmedian.label.ypos, lab='Median', Strategy= UniqueStrategyNames[[iStrategy]]))
  c.species5.lab <- rbind(c.species5.lab,data.frame(x=lower.quantile.label.xpos[[iStrategy]], y = lower.quantile.label.ypos, lab=c('LQ'), Strategy= UniqueStrategyNames[[iStrategy]]))
  c.species6.lab <- rbind(c.species6.lab, data.frame(x=upper.quantile.label.xpos[[iStrategy]], y = upper.quantile.label.ypos, lab=c('UQ'), Strategy= UniqueStrategyNames[[iStrategy]]))
}

plotdata$b.species4.lab$lab <- as.character(plotdata$b.species4.lab$lab)    # convert labels to character
c.species4.lab$lab <- as.character(c.species4.lab$lab)    # convert labels to character
c.species5.lab$lab <- as.character(c.species5.lab$lab)    # convert labels to character
c.species6.lab$lab <- as.character(c.species6.lab$lab)    # convert labels to character

# labels for BMedian lines
#plotdata$b.species4.lab<- data.frame(x=bmedian.label.xpos, y=bmedian.label.ypos, lab=c('B median', 'B median'), Strategy=UniqueStrategyNames[[iStrategy]])
#plotdata$b.species4.lab$lab <- as.character(plotdata$b.species4.lab$lab)    # convert labels to character

# labels for CMedian lines
#c.species4.lab<- data.frame(x=bmedian.label.xpos, y=bmedian.label.ypos, lab=c('Median', 'Median'), Strategy= UniqueStrategyNames[[iStrategy]])
#c.species4.lab$lab <- as.character(c.species4.lab$lab)    # convert labels to character

# labels for quantile lines
#c.species5.lab<- data.frame(x=lower.quantile.label.xpos, y = lower.quantile.label.ypos, lab=c('LQ', 'LQ'), Strategy= c(StrategyNames[1],StrategyNames[2]))
#c.species5.lab$lab <- as.character(c.species5.lab$lab)    # convert labels to character
#c.species6.lab<- data.frame(x=upper.quantile.label.xpos, y = upper.quantile.label.ypos, lab=c('UQ', 'UQ'), Strategy= c(StrategyNames[1],StrategyNames[2]))
#c.species6.lab$lab <- as.character(c.species6.lab$lab)    # convert labels to character

#######################################
######## KERNEL DENSITTY PLOT #########
#######################################
if (plotdata$result.names[result.index]=="BiomassEnd" || plotdata$result.names[result.index]=="BiomassMin" ||
    plotdata$result.names[result.index]=="DiscardMortalities" || plotdata$result.names[result.index]=="DiscardSurvivals" ||
    plotdata$result.names[result.index]=="Landings"){ # i.e. 1 or 2
  plotdata$blim.bpa.xaxis<-data.frame(blim.axis=Blim.species, bpa.axis=Bpa.species)	# we seem to need the position of the 
  # blim and bpa lines as a data.frame to plot them in the same way as we plot the medan line.
}
#quantile.xaxis<-data.frame(Strategy= c( StrategyNames[1],StrategyNames[2]), lq.axis=c(ResultQuantile.species[2,2], ResultQuantile.species[7,2]), uq.axis=c(ResultQuantile.species[4,2], ResultQuantile.species[9,2]))
quantile.xaxis=data.frame()
for (iStrategy in 1:length(UniqueStrategyNames)){
  quantile.xaxis<-rbind(quantile.xaxis,data.frame(Strategy=UniqueStrategyNames[iStrategy], lq.axis=ResultQuantile.species[5*iStrategy-3,2], uq.axis=ResultQuantile.species[5*iStrategy-1,2]))
}

plotdata$results.species = Add_Reg(plotdata$results.species)
plotdata$blim.species3.lab = Add_Reg(plotdata$blim.species3.lab)
plotdata$bpa.species3.lab = Add_Reg(plotdata$bpa.species3.lab)
plotdata$ResultMedian.species = Add_Reg(plotdata$ResultMedian.species)
plotdata$blim.bpa.xaxis = Add_Reg(plotdata$blim.bpa.xaxis)
plotdata$b.species4.lab = Add_Reg(plotdata$b.species4.lab)
plotdata$bpa.species5.lab = Add_Reg(plotdata$bpa.species5.lab)
plotdata$blim.species5.lab = Add_Reg(plotdata$blim.species5.lab)


#Write % below Blim and Bpa to tables and out to csv
# df.perc.Blim and df.perc.Bpa = data.frame("StrategyName" = NULL, "Group" = NULL, "Percentage.Below" = NULL)
for (iStrategy in 1:NumberUniqueStrategyNames){
  
  StrategyName = UniqueStrategyNames[iStrategy]
  species.name = plotdata$species_in_resultsfile[species.index[1]]

  perc.below.Blim = 100 - percBlim[[iStrategy]]
  df.perc.Blim = rbind(df.perc.Blim, data.frame("StrategyName" = StrategyName, "Group" = species.name, "Percentage.Below" = perc.below.Blim))
  
  perc.below.Bpa = 100 - percBpa[[iStrategy]]
  df.perc.Bpa = rbind(df.perc.Bpa, data.frame("StrategyName" = StrategyName, "Group" = species.name, "Percentage.Below" = perc.below.Bpa))
  
  
}

if(file.exists(paste(output_csv_folder, "Blim.csv", sep="")))
{
  write.table(df.perc.Blim, file=paste(output_csv_folder, "Blim.csv", sep=""), sep=",", append = T, row.names=F, col.names = F)
} else {
  write.table(df.perc.Blim, file=paste(output_csv_folder, "Blim.csv", sep=""), sep=",", append = T, row.names=F)
}

if(file.exists(paste(output_csv_folder, "Bpa.csv", sep="")))
{
  write.table(df.perc.Bpa, file=paste(output_csv_folder, "Bpa.csv", sep=""), sep=",", append = T, row.names=F, col.names = F)
} else {
  write.table(df.perc.Bpa, file=paste(output_csv_folder, "Bpa.csv", sep=""), sep=",", append = T, row.names=F)
}

if(file.exists(paste(output_csv_folder, "B_Quartiles.csv", sep="")))
{
  write.table(ResultQuantile.species, file=paste(output_csv_folder, "B_Quartiles.csv", sep=""), sep=",", append = T, row.names=F, col.names = F)
} else {
  write.table(ResultQuantile.species, file=paste(output_csv_folder, "B_Quartiles.csv", sep=""), sep=",", append = T, row.names=F)
}

#Reorder Strategy levels so that regulation types are plotted in specified order
plotdata$blim.species3.lab$Regulation = factor(plotdata$blim.species3.lab$Regulation, levels = c("Weakest stock", "Selective", "Highest value", "Other"))
OrderedStrategies = as.vector(plotdata$blim.species3.lab[order(plotdata$blim.species3.lab$Regulation),]$Strategy)
plotdata$results.species <- within(plotdata$results.species, Strategy <- factor(Strategy, levels= OrderedStrategies))

if (plotdata$result.names[result.index]=="BiomassEnd" || plotdata$result.names[result.index]=="BiomassMin"){ # i.e. 1 or 2
  if (Blim.species!=0){
    B_SPECIES = use_GGPLOT(plotdata, variable_to_plot = "tx1K_km2", fill_with_variable = "Regulation")
  } else {
    #B_SPECIES<-ggplot(plotdata$results.species,aes(x=tx1K_km2)) + geom_histogram(fill="red", binwidth = (plotdata$max.axis$x)/number_bins, aes(y=..density..)) +
    B_SPECIES = ggplot(plotdata$results.species,aes(x=tx1K_km2, fill=Regulation)) + geom_histogram(binwidth = (plotdata$max.axis$x)/number_bins, aes(y=..density..))
    B_SPECIES = B_SPECIES + facet_wrap(~Strategy, ncol=1)
    B_SPECIES = B_SPECIES + ylab("Density") + xlab(plotdata$xaxis.label[result.index]) #xlab("Minimum Biomass (1000 t)")
    B_SPECIES = B_SPECIES + labs(title=plotdata$species_in_resultsfile[species.index[1]])
    B_SPECIES = B_SPECIES + theme(panel.background = element_blank(), panel.grid.major=element_blank(), panel.grid.minor=element_blank(),axis.line=element_line(colour="Black"), axis.text=element_text(colour="Black", size=6), strip.text = element_text(size=6))
    B_SPECIES = B_SPECIES + coord_cartesian(ylim = c(0, as.numeric(plotdata$max.axis$y)*1.1), xlim = c(0, as.numeric(plotdata$max.axis$x)))
    # median, Blim and Bpa lines and labels
    B_SPECIES = B_SPECIES + geom_vline(data=plotdata$ResultMedian.species, aes(xintercept=median.species),linetype="dotted", size=0.7)
    B_SPECIES = B_SPECIES + geom_vline(data=plotdata$blim.bpa.xaxis, aes(xintercept=blim.axis),linetype="dotdash", size=0.7)
    B_SPECIES = B_SPECIES + geom_vline(data=plotdata$blim.bpa.xaxis, aes(xintercept=bpa.axis),linetype="longdash", size=0.7)
    B_SPECIES = B_SPECIES + geom_text(data = plotdata$b.species4.lab, aes(x = x, y = y, label = lab), size = 2, angle=90, vjust=-0.5)
    B_SPECIES = B_SPECIES + geom_text(data = plotdata$bpa.species5.lab, aes(x = x, y = y, label = lab), size = 2, angle=90, vjust=-0.5)
    B_SPECIES = B_SPECIES + geom_text(data = plotdata$blim.species5.lab, aes(x = x, y = y, label = lab), size = 2, angle=90, vjust=-0.5)

  }
  
  

  ggsave(B_SPECIES, file=paste(output_folder, plotdata$species_in_resultsfile[species.index[1]],"_",plotdata$result.names[result.index],".png", sep=""), width=6, height=length(UniqueStrategyNames)*1.5, limitsize = FALSE)
}  



if (plotdata$result.names[result.index]=="DiscardMortalities" || plotdata$result.names[result.index]=="DiscardSurvivals" ||
    plotdata$result.names[result.index]=="Landings"){ 
  browser()
      B_SPECIES = use_GGPLOT(variable_name = plotdata$result.names[result.index],
               dataframe2plot = plotdata$results.species, variable_name = "tx1K_km2", 
               fill_with_variable = "Regulation", axis_label = plotdata$xaxis.label[result.index],
               plot_title = plotdata$species_in_resultsfile[species.index[1]], label_percent_over_blim = plotdata$blim.species3.lab,
               label_percent_over_bpa = plotdata$bpa.species3.lab, maximum_y_value = plotdata$max.axis$y, maximum_x_value = plotdata$max.axis$x,
               Median_df = plotdata$ResultMedian.species, blim_bpa_x_val_vertical_line = plotdata$blim.bpa.xaxis, 
               B_Median_Line_Name = plotdata$b.species4.lab, Bpa_Line_Name = plotdata$bpa.species5.lab, Blim_Line_Name = plotdata$blim.species5.lab)
    
  C_SPECIES = use_GGPLOT(variable_name = plotdata$result.names[result.index],
               dataframe2plot = plotdata$results.species, variable_name = "tx1K_km2", 
               fill_with_variable = "Regulation", axis_label = plotdata$xaxis.label[result.index],
               plot_title = plotdata$species_in_resultsfile[species.index[1]], label_percent_over_blim = plotdata$blim.species3.lab,
               label_percent_over_bpa = plotdata$bpa.species3.lab, maximum_y_value = plotdata$max.axis$y, maximum_x_value = plotdata$max.axis$x,
               Median_df = plotdata$ResultMedian.species, blim_bpa_x_val_vertical_line = plotdata$blim.bpa.xaxis, 
               B_Median_Line_Name = plotdata$b.species4.lab, Bpa_Line_Name = plotdata$bpa.species5.lab, Blim_Line_Name = plotdata$blim.species5.lab)

  
  C_SPECIES<-ggplot(plotdata$results.species,aes(x=tx1K_km2)) + geom_histogram(fill="red", binwidth = (plotdata$max.axis$x)/number_bins, aes(y=..density..)) +
    facet_wrap(~Strategy, ncol=1)+
    ylab("Density")+xlab("Biomass (1000 t)")+ 
    labs(title=plotdata$species_in_resultsfile[species.index[1]]) +
    theme(panel.background = element_blank(), panel.grid.major=element_blank(), panel.grid.minor=element_blank(),axis.line=element_line(colour="Black"), axis.text=element_text(colour="Black"))+
    geom_text(data = catch.species3.lab, aes(x = x, y = y, label = lab), size = 3) +
    theme(legend.position = "none") + 
    #scale_x_continuous(limits=c(0,as.numeric(max.xaxis)))+ scale_y_continuous(limits=c(0,as.numeric(max.yaxis))) +
    #median and quantile lines and labels 
    coord_cartesian(ylim = c(0, as.numeric(plotdata$max.axis$y)*1.1), xlim = c(0, as.numeric(plotdata$max.axis$x))) +
    geom_vline(data=plotdata$ResultMedian.species, aes(xintercept=median.species),linetype="dotted", size=0.7) +
    geom_vline(data=quantile.xaxis, aes(xintercept=lq.axis),linetype="dashed", size=0.7) + 
    geom_vline(data=quantile.xaxis, aes(xintercept=uq.axis),linetype="dashed", size=0.7) + 
    
    #  geom_vline(data=LQ.species, aes(xintercept=quantile.species),linetype="dashed", size=0.7) + 
    #  geom_vline(data=UQ.species, aes(xintercept=quantile.species),linetype="dashed", size=0.7) + 
    
    #  geom_text(data = NULL,size=4, x = 40, y = 0.01, label = "LQ") +
    #  geom_text(data = NULL,size=4, x = 67, y = 0.0075, label = "median") +
    #  geom_text(data = NULL,size=4, x = 98, y = 0.01, label = "UQ")
  #annotate("segment", x = 77.6, xend = 77.6, y =0, yend = 10,colour = "black", linetype="longdash")+
  #annotate("text", x = 80, y = 0.005, label = "Median Historical Catch", colour="black", size=4) +   
  C_SPECIES
  ggsave(C_SPECIES, file=paste(sep="", output_folder, plotdata$species_in_resultsfile[species.index[1]],"_",plotdata$result.names[result.index],".jpg"), dpi=300, width=4, height=length(UniqueStrategyNames))
}  



if (plotdata$result.names[result.index]=="TotalEndValue"){ 
  T_SPECIES<-ggplot(plotdata$results.species,aes(x=tx1K_km2)) + geom_histogram(fill="red", binwidth = (plotdata$max.axis$x)/number_bins, aes(y=..density..)) +
    facet_wrap(~Strategy, ncol=1)+ylab("Density")+xlab("Total End Value (1000 Euros/km^2)")+ labs(title=temp3[method.index]) +
    theme(panel.background = element_blank(), panel.grid.major=element_blank(), panel.grid.minor=element_blank(),axis.line=element_line(colour="Black"), axis.text=element_text(colour="Black"))+
    #    geom_text(data = catch.species3.lab, aes(x = x, y = y, label = lab), size = 4) +
    theme(legend.position = "none")+ 
    #scale_x_continuous(limits=c(0,as.numeric(max.xaxis)))+ scale_y_continuous(limits=c(0,as.numeric(max.yaxis))) +
    #median and quantile lines and labels 
    geom_vline(data=plotdata$ResultMedian.species, aes(xintercept=median.species),linetype="dotted", size=0.7) +
    geom_vline(data=quantile.xaxis, aes(xintercept=lq.axis),linetype="dashed", size=0.7) + 
    geom_vline(data=quantile.xaxis, aes(xintercept=uq.axis),linetype="dashed", size=0.7) + 
    
    #  geom_vline(data=LQ.species, aes(xintercept=quantile.species),linetype="dashed", size=0.7) + 
    #  geom_vline(data=UQ.species, aes(xintercept=quantile.species),linetype="dashed", size=0.7) + 
    
    #  geom_text(data = NULL,size=4, x = 40, y = 0.01, label = "LQ") +
    #  geom_text(data = NULL,size=4, x = 67, y = 0.0075, label = "median") +
    #  geom_text(data = NULL,size=4, x = 98, y = 0.01, label = "UQ")
    geom_text(data = c.species4.lab, aes(x = x, y = y, label = lab), size = 2, angle=90, vjust=-0.5)+
    geom_text(data = c.species5.lab, aes(x = x, y = y, label = lab), size = 2, angle=90, vjust=-0.5)+
    geom_text(data = c.species6.lab, aes(x = x, y = y, label = lab), size = 2, angle=90, vjust=-0.5)
  #annotate("segment", x = 77.6, xend = 77.6, y =0, yend = 10,colour = "black", linetype="longdash")+
  #annotate("text", x = 80, y = 0.005, label = "Median Historical Catch", colour="black", size=4) +   
  T_SPECIES
  ggsave(T_SPECIES, file=paste(temp3[method.index[1]],"_",plotdata$result.names[result.index],".jpg", sep=""), dpi=300, width=4, height=length(UniqueStrategyNames))
}  