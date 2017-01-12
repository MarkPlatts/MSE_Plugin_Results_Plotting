#Graphs from results of the Harvest Control Rules - Steve Mack, Mark Platts
#generic code - Silvia Hadeler March 2013

#to clear any previous calculation which may interfere with the current one 
rm(list=ls())

#setting directory where data is from
setwd("C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/R Script_Plotting Results/codes_Jan 2015")

#loading packages which will be used in the following calculations
library(ggplot2)
library(plyr)

#Strategies = c("Mixed Fishery MSY HighestValue", "Mixed Fishery MSY None")


#######################################
############ FUNCTIONS ################
#######################################

### Select name of species you would like to analyse
species.select <- function(species.1){  # function returns a vector with numerical species indeces
  # corresponding to the species index in each data file
  
  species.select.1<-FALSE	# logic labels telling us if we have selected a valid species
  species.select.3<-FALSE
  
  if(interactive()) {	# need to run this interactively to enter species name
    
    while(!species.select.1){	# continue until we have found the correct species
      species.name<-readline("Input species name: ") 	# enter species name
      #does this species exist in species.1, i.e. Results.csv?
      species.index.1<-grep(as.character(species.name), ignore.case=TRUE, species.1)
      
      if (length(species.index.1)==0) {			# no species found
        print(c("No matching species found")) 
      }
      
      if (length(species.index.1)==1) {			# found exactly one species from Results.csv
        species.index.1<-as.numeric(species.index.1)	# species.index.1 is a string otherwise
        species.select.1<-TRUE
      }
      
      if (length(species.index.1)>1) {			# more than one species found
        print(c("Found more than one species containing this name"))
        Species<-species.1[species.index.1]		# generate a data.frame to display species to choose from
        Index<-species.index.1
        display.data.frame<-data.frame(Species, Index)
        while(!species.select.3){			# select one from shortlist
          print(c("Please select index from:"))
          print(display.data.frame)
          select.index<-readline("Select species number ")
          valid<-(match(select.index, species.index.1))	# have we selected a valid index?
          if (!is.na(valid)){
            species.select.1<-TRUE
            species.select.3<-TRUE
            species.index.1<-select.index		# set species.index.1 to the species we are working with now
            # from Fleet.csv
          }  
        }  
      }
    }
    
    print(c("Selected species from Results.csv is: ", species.1[as.numeric(species.index.1)]))
    
  } else {
    
    output<-c('You need to run this script in interactive mode.')
    print(output)
    
  }
  
  species.index.1	# function returns numerical species index
  
}
##############################end of species selection function


##################################
### Scale axes
#################################
scale.axis.nice <- function(max_in) {
  
  max<-as.numeric(max_in)  		# sometimes the input might be a string!
  scale.base=c(1,2,2.5,3,5,7.5,10)		# let's use scales whose maximum is a multiple of the scale.base, e.g. 100, 200, or 250	
  
  exp<-0
  base.index<-1
  flag<-c(FALSE, FALSE)
  while(flag[1]!=TRUE){
    #    print(max)
    #    print(base.index)
    #    print(exp)
    if(max>10^exp && max<10^(exp+1)) {	# we found the correct range between 1eX and 1e(X+1), e.g. 100 & 1000
      flag[1]<-TRUE
      while(flag[2]!=TRUE){			# now let's find a "nice" scale based on the scale.base sequence
        #      print(scale.base[base.index]*10^exp)
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


# scale.axes <- function(data.set, median, LQ, UQ) {   ##function to determine the scale of both axis 
#   browser()
#   # find the maximum of density functions
#   max.xaxis<-as.numeric(format(max(data.set$x), scientific=TRUE))
#   max.yaxis<-as.numeric(format(max(data.set$y), scientific=TRUE))
#   
#   # sometimes median, Blim or Bpa are larger than the max of the density function!
#   max.xaxis<-max(max.xaxis, median[,2], LQ, UQ)
#   #  max.xaxis<-max(5., median[,2],LQ, UQ)
#   
#   # now scale "nicely"  
#   scale<-c(scale.axis.nice(max.xaxis), scale.axis.nice(max.yaxis))
#   
#   scale
#   
# }

scale.axes <- function(data.set, UQ) {   ##function to determine the scale of both axis 

  # find the maximum of density functions
  max.xaxis<-as.numeric(format(max(data.set$x), scientific=TRUE))
  max.yaxis<-as.numeric(format(max(data.set$y), scientific=TRUE))
  
  # sometimes median, Blim or Bpa are larger than the max of the density function!
  max.xaxis<-max(max.xaxis, UQ)
  #  max.xaxis<-max(5., median[,2],LQ, UQ)
  
  # now scale "nicely"  
  scale<-c(scale.axis.nice(max.xaxis), scale.axis.nice(max.yaxis))
  
  scale
  
}

scale.axes.hist <- function(data, median, nbins, nUniqueStrategies) {
  #updated to handle varying number of strategies
  
  #calculate what the x limits are 
  max.xaxis<-as.numeric(format(max(data[[1]]$Catch_1000t, median[,2]), scientific=TRUE))
  if (length(data)>1){
    for (iStrategy in 2:nUniqueStrategies){
      max.xaxis<-as.numeric(format(max(max.xaxis, data[[iStrategy]]$Catch_1000t), scientific=TRUE))
    }
  }
  min.xaxis<- 0
  
  #calc size of breaks
  sizeofbreak = (max.xaxis-min.xaxis)/nbins #calc size of bins
  #max.xaxis<-max(max.xaxis, median[,2], a, b)
  max.yaxis = -9999
  
  #calculate for data what maximum y value is
  breaks=seq(min.xaxis, max.xaxis, sizeofbreak) #determine a vector of where the bins should begin
  
  for (iStrategy in 1:nUniqueStrategies){
    datacut = cut(data[[iStrategy]]$Catch_1000t, breaks, right=FALSE)
    freqtab = table(datacut) #create the table
    freqvec = as.numeric(freqtab)
    propvec = freqvec/sum(freqvec)  #convert into proportions
    densityvec = propvec/sizeofbreak
    max.yaxis = as.numeric(format(max(max.yaxis,densityvec), scientific=TRUE))
    
  }
  
  # sometimes median, Blim or Bpa are larger than the max of the density function!
  
  # now scale "nicely"  
  #scale<-c(scale.axis.nice(max.xaxis), scale.axis.nice(max.yaxis))
  #scale=c(max.xaxis,max.yaxis)
  
  #scale = max.xaxis
  #scale$y = max.yaxis
  
  list(x=max.xaxis, y=max.yaxis)
  
}

##########################################end of axis scaling

###########################################
### select the FleetName
###########################################
fleet.name.select <- function(fleet.names) {

  Fleetname<-fleet.names  	# generate a data.frame to display FleetNames to choose from
  Index<-c(1:length(fleet.names))
  display.data.frame<-data.frame(Fleetname, Index)
  
  fleet.select<-FALSE
  
  while(!fleet.select){		# select one from shortlist
    print(c("Please select Fleet name from:"))
    print(display.data.frame)
    fleet.index<-as.numeric(readline("Enter index number "))
    valid<-(match(fleet.index, Index))	# have we selected a valid index?
    if (!is.na(valid)){
      fleet.select<-TRUE
    }
  }
  
  fleet.index				# return fleet.index
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~ start programme ~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#######################################
############ READ DATA ################
#######################################
# Loading Excel fleet file.csv
results<-read.csv("Fleet.csv",sep=",", header=TRUE, skip=7)
#### optionally, check if the file is read correctly
head(results)

nbins=50

#######################################
####### DATA MANIPULATION #############
####### t/km^2 into 1000 t #############
##### for the entire North Sea #############
#######################################
#adding extra colum to facilitate further manipulation

# Transforming catch from t/km^2 into 1000 t by adding another column to the data in order to convert catch from  t/km2 (as outputed from EwE) into t (tonnes). Multiply catch by the area of North Sea = 570000 km2. To make the results better to present divide by 1000, i.e. so resulsts would be 1000t (ie. kTon). So to make matters easier just multiply by 570

results.t<-as.matrix(as.numeric(results$Value)) # catch in t/km^2
results.t<-results.t* 570           # now catch in 1000t= 10^3t
results.t<-cbind(results, results.t)
colnames(results.t)<-c("Iteration","Strategy","Fleetnumber", "FleetName", "GroupNumber", "GroupName","value_t_per_km2","Catch_1000t")

#######################################
########## FLEET_NAME #################
#######################################
# which FleetNames do we have in Fleet.csv?
fleet.names<-levels(results.t$FleetName)

#######################################
############ SPECIES ##################
#######################################
# which species do we have in Fleet.csv?
# and we add "species" Total if we want to show total catch
species<-c(levels(results.t$GroupName), "Total")

#strategy<-unique(results$Strategy)

total.data = matrix(nrow=0,ncol=8)
colnames(total.data) = c("Iteration","Strategy","Fleetnumber","FleetName","GroupNumber","GroupName","value_t_per_km2","Catch_1000t")

#######################################
####### DATA MANIPULATION #############
####### find total catch  #############
##### per fleet and interation ########
#######################################
# store the total fleet catch data here first before inserting it into results.t
#total.data<-cbind(0, "dummy", "dummy", "Total", 0, 0, 0) 

for (i in 1:length(fleet.names)) {  # go through all fleet names
  
  results.fleet<-subset(results.t, FleetName==fleet.names[i])	# data for specific fleet
  # for each iteration sum catch (value_t & Catch_1000t) over all species and separate by Strategy/HCRs
  sum.fleet<-ddply(results.fleet, .(Iteration, Strategy), summarise, sum.value_t.species=sum(value_t_per_km2), sum.species=sum(Catch_1000t))
  
  # rearrange result so it can be added to results.t further down
  for (j in 1:length(sum.fleet[,1])) {	# go through all lines of sum.fleet
    line = data.frame(Iteration=sum.fleet[j,1], Strategy=toString(sum.fleet[j,2]), Fleetnumber=i, FleetName=fleet.names[i],GroupNumber=0, GroupName="Total", value_t_per_km2=as.numeric(sum.fleet[j,3]), Catch_1000t=as.numeric(sum.fleet[j,4]))
    #line<-cbind(sum.fleet[j,1], toString(sum.fleet[j,2]),i, fleet.names[i],0, "Total", as.numeric(sum.fleet[j,3]), as.numeric(sum.fleet[j,4]))
    #colnames(line)<-c("Iteration","Strategy","Fleetnumber","FleetName","GroupNumber","GroupName","value_t_per_km2","Catch_1000t")  
    total.data<-rbind(total.data, line) 
    
  }
  
}

# append the total fleet catch data to the original data
results.t<-rbind(results.t, total.data)
strategy<-levels(results$Strategy)

# IMPORTANT: from now we are working with results.t, last column contains catch in 1000 t 
head(results.t)

#######################################
###########  SELECT FLEET  ############
#######################################
#select FleetName
fleet.index<-fleet.name.select(fleet.names)  # returns index of chosen ResultName in result.names

#######################################
########## SELECT SPECIES #############
#######################################
#select species
species.index<-as.numeric(species.select(species))

# catch for this species and fleet - if species = "Total" then we are looking at the total catch of that fleet 
results.fleet<-subset(results.t, FleetName==fleet.names[fleet.index] & GroupName==species[species.index[1]])
##checking functions
#head(results.species)
#range(results.species$Catch_1000t)

#######################################
######### RESULTS STATISTICS ##########
#######################################

### sum and quantiles of fleet catch
#sum of fleet catch per iteration and HCR
#ResultMean <- ddply(results.fleet, .(HCR), summarise, mean.species=mean(Catch_1000t))
#ResultMean
ResultMedian<-ddply(results.fleet, .(Strategy), summarise, median.species=median(as.numeric(Catch_1000t)))
ResultMedian
ResultQuantile<-ddply(results.fleet, .(Strategy), summarise, quantile.species=quantile(as.numeric(Catch_1000t)))
ResultQuantile

UniqueStrategyNames = levels(results$Strategy)
NumberUniqueStrategyNames = length(UniqueStrategyNames)

n=list()

for (iStrategy in 1:NumberUniqueStrategyNames){
  n[iStrategy]<-list(as.data.frame(subset(results.t,FleetName==fleet.names[fleet.index] & GroupName==species[species.index[1]] & Strategy==UniqueStrategyNames[iStrategy])))
}

#Harvest Control Rule 1
#n1<-as.data.frame(subset(results.t,FleetName==fleet.names[fleet.index] & Strategy==Strategies[1] & GroupName==species[species.index]))

#Harvest Control Rule 2
#n2<-as.data.frame(subset(results.t,FleetName==fleet.names[fleet.index] & Strategy==Strategies[2] & GroupName==species[species.index]))

### INSERT ANY OTHER CALCULATIONS HERE

#######################################
########## PLOT PREPARATION ###########
########### KERNEL DENSITTY ###########
#######################################

# adding the individual info of each facet graph in form of data frames

# find appropriate axes scales
# left if else here in case different plots need different axes
#if (result.index!=3) {
#  max.axes.1=scale.axes(density(as.numeric(n1$Catch_1000t), adjust=adjustment), ResultMedian, ResultQuantile[4,2], ResultQuantile[4,2])
#  max.axes.2=scale.axes(density(as.numeric(n2$Catch_1000t), adjust=adjustment), ResultMedian, ResultQuantile[7,2], ResultQuantile[7,2])

# max.axes.1=scale.axes(density(as.numeric(n1$Catch_1000t)), ResultMedian, ResultQuantile[4,2], ResultQuantile[4,2])
# max.axes.2=scale.axes(density(as.numeric(n2$Catch_1000t)), ResultMedian, ResultQuantile[7,2], ResultQuantile[7,2])

# max.axes.1=scale.axes(density(as.numeric(n1$Catch_1000t)), ResultQuantile[4,2])
# max.axes.2=scale.axes(density(as.numeric(n2$Catch_1000t)), ResultQuantile[9,2])

max.axes.1=scale.axes.hist(n, ResultMedian, nbins, NumberUniqueStrategyNames)$x
max.axes.2=scale.axes.hist(n, ResultMedian, nbins, NumberUniqueStrategyNames)$y


# max.axes.1=scale.axes(density(n1$tx1K_km2), ResultMedian.species, Blim.species, Bpa.species)

# choose the maximum values of x- and y-axis scales to plot both graphs on the same scale later
#max.xaxis<-max(max.axes.1[1], max.axes.2[1])
#max.yaxis<-max(max.axes.1[2], max.axes.2[2])
max.xaxis<-max.axes.1
max.yaxis<-max.axes.2

# xaxis labels
### change x labels here!
xaxis.label<-c("Catch (1000 t)")

# generate formated strings from numerical values for ResultMedian, Quantiles
ResultMedian.hcr.string = list()
for (iStrategy in 1:NumberUniqueStrategyNames){
  ResultMedian.hcr.string[iStrategy]<-list(format(ResultMedian[iStrategy,2], digits=3, nsmall=0))
}
#ResultMedian.hc1.string<-format(ResultMedian[1,2], digits=3, nsmall=0)
#ResultMedian.hc2.string<-format(ResultMedian[2,2], digits=3, nsmall=0)

# generate the label positions according to ResultMedian and catch quantiles
bmedian.label.xpos=list()
for (iStrategy in 1:NumberUniqueStrategyNames){
  bmedian.label.xpos[[iStrategy]]<-c(as.numeric(format(ResultMedian[iStrategy,2], digits=1, nsmall=0)))
  
}
#bmedian.label.xpos<-c(as.numeric(format(ResultMedian[1,2], digits=1, nsmall=0)), as.numeric(format(ResultMedian[2,2], digits=1, nsmall=0)))+0.075*as.numeric(max.xaxis)
bmedian.label.ypos<-c(3.5*as.numeric(max.yaxis))

lower.quantile.label.xpos<-vector()
upper.quantile.label.xpos<-vector()
for (iStrategy in 1:NumberUniqueStrategyNames){
  lower.quantile.label.xpos[iStrategy]<-c(as.numeric(ResultQuantile[(5*iStrategy-3),2]+0.035*as.numeric(max.xaxis)))
  upper.quantile.label.xpos[iStrategy]<-c(as.numeric(ResultQuantile[(5*iStrategy-1),2]+0.035*as.numeric(max.xaxis)))
}
#lower.quantile.label.xpos<-c(as.numeric(ResultQuantile[2,2])+0.035*as.numeric(max.xaxis), as.numeric(ResultQuantile[7,2])+0.035*as.numeric(max.xaxis))
#upper.quantile.label.xpos<-c(as.numeric(ResultQuantile[4,2])+0.035*as.numeric(max.xaxis), as.numeric(ResultQuantile[9,2])+0.035*as.numeric(max.xaxis))
lower.quantile.label.ypos<-c(2.5*as.numeric(max.yaxis))
upper.quantile.label.ypos<-c(1.5*as.numeric(max.yaxis))

# labels for BMedian lines
#b.fleet4.lab<- data.frame(x=bmedian.label.xpos, y=bmedian.label.ypos, lab=c('B median', 'B median'), Strategy= Strategies)
#b.fleet4.lab$lab <- as.character(b.fleet4.lab$lab)    # convert labels to character
b.fleet4.lab = data.frame()
for (iStrategy in 1:NumberUniqueStrategyNames){
  b.fleet4.lab<-rbind(b.fleet4.lab, data.frame(x = bmedian.label.xpos[[iStrategy]],
                                               y = bmedian.label.ypos,
                                               lab = as.character('B median'),
                                               Strategy= UniqueStrategyNames[[iStrategy]]))
}


# labels for quantile lines
b.fleet5.lab = data.frame()
for (iStrategy in 1:NumberUniqueStrategyNames){
  b.fleet5.lab<- rbind(b.fleet5.lab, data.frame(x=lower.quantile.label.xpos[iStrategy], 
                                               y = lower.quantile.label.ypos, 
                                               lab=as.character('LQ'), 
                                               Strategy=strategy[iStrategy]))
}
  
#b.fleet5.lab<- data.frame(x=lower.quantile.label.xpos, y = lower.quantile.label.ypos, lab=c('LQ', 'LQ'), Strategy=Strategies)
#c("Harvest Control Rule 1","Harvest Control Rule 2"))
#b.fleet5.lab$lab <- as.character(b.fleet5.lab$lab)    # convert labels to character
b.fleet6.lab = data.frame()
for (iStrategy in 1:NumberUniqueStrategyNames){
  b.fleet6.lab<- rbind(b.fleet6.lab, data.frame(x=upper.quantile.label.xpos[iStrategy], 
                                                y = upper.quantile.label.ypos, 
                                                lab=as.character('UQ'), 
                                                Strategy=strategy[iStrategy]))
}

#b.fleet6.lab<- data.frame(x=upper.quantile.label.xpos, y = upper.quantile.label.ypos, lab=c('UQ', 'UQ'), Strategy=Strategies)
#b.fleet6.lab$lab <- as.character(b.fleet6.lab$lab)    # convert labels to character

#plot.title<-paste(fleet.names[fleet.index], species.1[species.index[1]], sep=" ")
plot.title<-paste(fleet.names[fleet.index], species[species.index], sep=" ")

#######################################
######## KERNEL DENSITTY PLOT #########
#######################################
quantile.xaxis=data.frame()
for (iStrategy in 1:length(UniqueStrategyNames)){
  quantile.xaxis<-rbind(quantile.xaxis,data.frame(Strategy=strategy[iStrategy], lq.axis=ResultQuantile[5*iStrategy-3,2], uq.axis=ResultQuantile[5*iStrategy-1,2]))
}
#quantile.xaxis<-data.frame(Strategy= Strategies, lq.axis=c(ResultQuantile[2,2], ResultQuantile[7,2]), uq.axis=c(ResultQuantile[4,2], ResultQuantile[9,2]))

# #B_FLEET<-ggplot(results.fleet,aes(x=as.numeric(Catch_1000t))) + geom_density(adjust=1, aes(group=Strategy, colour=Strategy)) +
# B_FLEET<-ggplot(results.fleet,aes(x=as.numeric(Catch_1000t))) + geom_histogram(binwidth = 0.1) +
#   facet_wrap(~Strategy, ncol=1)+ylab("Frequency")+xlab(xaxis.label)+
#   labs(title=plot.title) +
#   theme(panel.background = element_blank(), panel.grid.major=element_blank(), panel.grid.minor=element_blank(),axis.line=element_line(colour="Black"), axis.text=element_text(colour="Black"))+
# #  geom_text(data = b.species3.lab, aes(x = x, y = y, label = lab), size = 4) +
#   #theme(legend.position = "none")+ scale_x_continuous(limits=c(0,as.numeric(max.xaxis)))+ scale_y_continuous(limits=c(0,as.numeric(max.yaxis))) +
#   theme(legend.position = "none")+ xlim(c(0,as.numeric(max.xaxis)))+ ylim(c(0,as.numeric(max.yaxis))) +
#   # median, Blim and Bpa lines and labels
# geom_vline(data=ResultMedian, aes(xintercept=median.species),linetype="dotted", size=0.7) +
#   geom_vline(data=quantile.xaxis, aes(xintercept=lq.axis),linetype="dashed", size=0.7) + 
#   geom_vline(data=quantile.xaxis, aes(xintercept=uq.axis),linetype="dashed", size=0.7) + 
#   geom_text(data = b.fleet4.lab, aes(x = x, y = y, label = lab), size = 4) +
#   geom_text(data = b.fleet5.lab, aes(x = x, y = y, label = lab), size = 4) +
#   geom_text(data = b.fleet6.lab, aes(x = x, y = y, label = lab), size = 4)
# B_FLEET

B_FLEET<-ggplot(results.fleet,aes(x=as.numeric(Catch_1000t))) + geom_histogram(fill="red", binwidth = (max.xaxis)/nbins, aes(y=..density..)) +
  facet_wrap(~Strategy, ncol=1)+ylab("Density")+xlab(xaxis.label)+
  labs(title=plot.title) +
  theme(panel.background = element_blank(), panel.grid.major=element_blank(), panel.grid.minor=element_blank(),axis.line=element_line(colour="Black"), axis.text=element_text(colour="Black"))+
  #  geom_text(data = b.species3.lab, aes(x = x, y = y, label = lab), size = 4) +
  #theme(legend.position = "none")+ 
  coord_cartesian(ylim = c(0, as.numeric(max.yaxis)*1.1), xlim = c(0, as.numeric(max.xaxis))) +
  #scale_x_continuous(limits=c(0,as.numeric(max.xaxis)))+ scale_y_continuous(limits=c(0,as.numeric(max.yaxis))) +
  theme(legend.position = "none")+ xlim(c(0,as.numeric(max.xaxis))) +
  # median, Blim and Bpa lines and labels
  geom_vline(data=ResultMedian, aes(xintercept=median.species),linetype="dotted", size=0.7) +
  geom_vline(data=quantile.xaxis, aes(xintercept=lq.axis),linetype="dashed", size=0.7) + 
  geom_vline(data=quantile.xaxis, aes(xintercept=uq.axis),linetype="dashed", size=0.7) + 
  geom_text(data = b.fleet4.lab, aes(x = x, y = y, label = lab), size = 2) +
  geom_text(data = b.fleet5.lab, aes(x = x, y = y, label = lab), size = 2) +
  geom_text(data = b.fleet6.lab, aes(x = x, y = y, label = lab), size = 2)
B_FLEET

ggsave(B_FLEET, file=paste(species[species.index],"_", fleet.names[fleet.index],".jpg", sep=""), dpi=600, width=4, height=16)

# data in results.t varies over several magnitudes -> let's cut this output
# note that this works for particular species but not for the total catch
# if you want to use the whole set comment out the following line
#results.t<-subset(results.t, Catch_1000t>0 & Catch_1000t<1e4)

# in addition we can also try and change the bandwidth of the density plot later to get a nicer graph, default value =1
#adjustment=1


#save plots
#my.out.file      <- '../output/fleet_data'



strategy