
#Change elements in this vector to choose which batches to plot
batches = c(3)

for(iBatch in batches){
  setwd("C:/Users/Mark/Desktop/MSE_Plugin_Results_Plotting")
  batch = toString(iBatch)
  cat("Batch:", batch)
  source("plotting 15July_2016_ChokeHighestValue_byFleet - Refactor.R")
}