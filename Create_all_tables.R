# INITIALISATION START ===============================================================================================

#start with a clean sheet
rm(list = ls())

#load libraries & sources
setwd("C:/Users/Mark/Desktop/Desktop etc/GAP/MSE_Plugin_Results_Plotting")
source("share_tools.R")
source("initialisation.R")

params = initialise_params("0")

#root results path
root.plot <-     params$plot.path 
root.results =  params$RootPath

hcr.folders = params$hcr.folders

groups.for.f.or.biomass = "f"

setwd("C:/Users/Mark/Desktop/Desktop etc/GAP/MSE_Plugin_Results_Plotting")

# INITIALISATION END ===============================================================================================

# print("Calc_Catch_Start_End_Ratio.R")
# source("Calc_Catch_Start_End_Ratio.R")
# print("Calc_Percent_Catch_Above_Below_Quota.R")
# source("Calc_Percent_Catch_Above_Below_Quota.R")
print("Calc_Percent_RealisedF_Above_Below_FMax.R")
source("Calc_Percent_RealisedF_Above_Below_FMax.R")
# print("Calc_Percent_Below_Conservation_Limits.R")
# source("Calc_Percent_Below_Conservation_Limits.R")
# print("Calc_Mean5YearBiomass_FiveNum_Sum.R")
# source("Calc_Mean5YearBiomass_FiveNum_Sum.R")
# print("Calc_Mean5YearCatch_FiveNum_Sum.R")
# source("Calc_Mean5YearCatch_FiveNum_Sum.R")
