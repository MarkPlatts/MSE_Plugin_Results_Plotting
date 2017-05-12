#Average Quota plot script


#Initialisation===========================================================================================================================================
rm(list=ls())
# Set to directory where this source code is
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
source("Calc_average_quota_per_fleet_group_regulation.R")


#Input======================================================================================================================================================
Root.Path = "C:/Users/Mark/Box Sync/Baltic - Stockholm/MSEtest_simlerRules - Copy (2)/"
Results.Path = paste(Root.Path, "Results/", sep="")
Plot.Path = paste(Root.Path, "Plots/", sep="")
# Groups = c("Cod (adult)", "Haddock (adult)", "Whiting (adult)")
Groups = LoadUniqueGroups(Results.Path)
TimeSteps = 1:20
RegulationTypes = c("Highest value", "Weakest stock", "Selective")
# Fleet = "FleetNo1"
Fleets = c("AllFleets", "FleetNo1", "FleetNo2", "FleetNo3", "FleetNo4", "FleetNo5", "FleetNo6", "FleetNo7",
                       "FleetNo8", "FleetNo9", "FleetNo10")


#Plot======================================================================================================================================================
Plot_Average_Quotas(Results.Path, Plot.Path, Groups, Fleets, TimeSteps, RegulationTypes)