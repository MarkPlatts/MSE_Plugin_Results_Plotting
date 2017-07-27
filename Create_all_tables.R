# INITIALISATION START ===============================================================================================

#start with a clean sheet
rm(list = ls())

#load libraries & sources
setwd("C:/Users/Mark/Desktop/Desktop etc/GAP/MSE_Plugin_Results_Plotting")
source("share_tools.R")
source("initialisation_baltic.R")

params = initialise_params("0")

#root results path
root.plot <-    params$plot.path 
root.results <- params$RootPath

hcr.folders = params$hcr.folders

groups.for.f.or.biomass = "f"

setwd("C:/Users/Mark/Desktop/Desktop etc/GAP/MSE_Plugin_Results_Plotting")

# INITIALISATION END ===============================================================================================

print("CreateCatchRatioTables")
source("Calc_Catch_Start_End_Ratio.R")
CreateCatchRatioTables(CatchType = "Catch", root.plot, root.results)
CreateCatchRatioTables(CatchType = "Landings", root.plot, root.results)
CreateCatchRatioTables(CatchType = "Discards", root.plot, root.results)
CreateCatchRatioTables(CatchType = "Value", root.plot, root.results)

print("CreatePercentCatchAboveBelowQuotaTable")
source("Calc_Percent_Catch_Above_Below_Quota.R")
CreatePercentCatchAboveBelowQuotaTable(root.plot, root.results)

print("CreatePercentRealisedLandedFAboveBelowFMax")
source("Calc_Percent_RealisedF_Above_Below_FMax.R")
CreatePercentRealisedLandedFAboveBelowFMax(root.plot, root.results)

print("Calc_Percent_Below_Conservation_Limits.R")
source("Calc_Percent_Below_Conservation_Limits.R")
CreatePercentBelowConservationLimits("LowerLimit")
CreatePercentBelowConservationLimits("UpperLimit")

print("Calc_Mean5YearBiomass_FiveNum_Sum.R")
source("Calc_Mean5YearBiomass_FiveNum_Sum.R")
CreateBiomassFiveNumSum(plot.path = params$plot.path,
                        area = params$Area)

print("Calc_Mean5YearCatch_FiveNum_Sum.R")
source("Calc_Mean5YearCatch_FiveNum_Sum.R")
CreateCatchTables(CatchType = "Catch", root.results, root.plot)
CreateCatchTables(CatchType = "Landings", root.results, root.plot)
CreateCatchTables(CatchType = "Discards", root.results, root.plot)
CreateCatchTables(CatchType = "Value", root.results, root.plot)


