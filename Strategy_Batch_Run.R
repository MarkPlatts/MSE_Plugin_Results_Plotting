setwd("C:/Users/Mark/Desktop/MSE_Plugin_Results_Plotting")
batch = "1"

source("plotting 15July_2016_ChokeHighestValue_byFleet - Refactor.R")

setwd("C:/Users/Mark/Desktop/MSE_Plugin_Results_Plotting")
batch = "2"
source("plotting 15July_2016_ChokeHighestValue_byFleet - Refactor.R")



for(iBatch in 1:3){
  setwd("C:/Users/Mark/Desktop/MSE_Plugin_Results_Plotting")
  batch = iBatch
  cat("Batch:", batch)
  source("plotting 15July_2016_ChokeHighestValue_byFleet - Refactor.R")
}