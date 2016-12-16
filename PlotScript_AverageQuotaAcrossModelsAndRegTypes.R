#Average Quota plot script


#Initialisation===========================================================================================================================================
rm(list=ls())
# Set to directory where this source code is
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
source("Calc_average_quota_per_fleet_group_regulation.R")


#Input======================================================================================================================================================
Path = "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/R Code/TestFolder_R_Plotting_MSE_Plugin/withBiomassForcing_Yearly_Results HCR type1 and 3/Results"
Groups = c("Cod (adult)", "Haddock (adult)", "Whiting (adult)")
TimeSteps = 1:20
RegulationTypes = c("Highest value", "Weakest stock", "Selective")
Fleet = "FleetNo1"



#Plot======================================================================================================================================================
Plot_Average_Quotas(Path, Groups, Fleet, TimeSteps, RegulationTypes)