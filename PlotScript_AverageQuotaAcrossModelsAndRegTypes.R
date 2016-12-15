#Average Quota plot script

#Initialisation
rm(list=ls())
setwd("C:/Users/Mark/Desktop/MSE_Plugin_Results_Plotting/")
source("Calc_average_quota_per_fleet_group_regulation.R")

#Input
Path = "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/R Code/TestFolder_R_Plotting_MSE_Plugin/withBiomassForcing_Yearly_Results HCR type1 and 3/Results"
Groups = c("Cod (adult)", "Haddock (adult)", "Herring (adult)")
TimeSteps = 1:20
RegulationTypes = c("Highest value", "Weakest stock", "Selective")
Fleet = "FleetNo4"

#Test:
Plot_Average_Quotas(Path, Groups, Fleet, TimeSteps, RegulationTypes)