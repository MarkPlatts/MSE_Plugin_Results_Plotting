R Plotting code for Cefas MSE Plugin
July 2017

R code for plotting can be found at:
https://github.com/MarkPlatts/MSE_Plugin_Results_Plotting/

Main project file is:
Plotting_Trajectories_2016.Rproj

Explanation of what each source file includes/does:

plotting 15July_2016_ChokeHighestValue_byFleet - Refactor.R
This is one of the main files that can be used to do the vast majority of the plotting all in one go.
To use it you need to change the path that the setwd statement points to at the top of the file.
Also many settings need to be set in initialise_params that this file points to
At the bottom of the file you can specify:
•	Which plots you want to generate by specifying the indices of the plots in vector_indices_plot_types. To see what each number refers to scroll through the file checking the if(type2plot==X) statements and seeing what plots are generated dependent on the value of X. e.g. vector_indices_plot_types = c(2, 5) will plot the trajectories for HCRF_Targ and the RealisedLandedF.
•	Setting vector_indices_plot_types = 1:17 will plot everything

Initialisation.R
This is an important file that sets many of the settings to be used in the previous file.
Need to set:
•	path.data.folder to the path of the folder where all the data is held from the plugin
•	batch is used if you want to break the plots up into several subgroups of strategies to make plots clearer. You will need to create folders in path.data.folder and specify them in the switch routine.
•	I think hcr.folders is obsolete now and not used it, but have not deleted it just in case some code is using it that I can’t find.
•	params$Base_NYears to the number of years that EwE runs without projecting
•	params$Projected_NYears to the number of years projecting
•	params$StartRun_Year to the year that the EwE simulation begins running
•	params$Area to the area that the EwE model represents so that values can be scaled up from units/area to units
•	params$Groups2Plot to a vector of all the groups to be plotted, each group indicated using the form GroupNoX.
•	params$Fleets2Plot to a vector of all the fleets to be plotted, each fleet indicated using the form FleetNoX.
•	You might need to alter the switch batch segment of the code depending on how many groups of strategies you are plotting and the ranges of the strategies in each group.
•	Alternatively you can specify the strategies by name in the single vector params$strats
•	Or interactively by commenting out previous lines for specifying strategies and uncommenting params$strats <- create_list_strategies(params$RootPath)

B_Trajectories.R
Plots the biomass trajectories. Need to have created a biom_refs.csv file in the main plots folder if you want it to show the reference points
Example layout of biom_refs.csv
Group	Blim	Bpa
Cod (adult)	118	165
Whiting (adult)	184	250
Haddock (adult)	63	88
Saithe (adult)	106	200
Hake	5.2	7.3
Norway pout	90	150

Calc_average_quota_per_fleet_group_regulation.R
Plots the average quota across models and regulation types

Calc_Catch_Start_End_Ratio.R
Calculates the last 5 year mean of catch (as total catch, landings, discards and value) and divides to by the first year catch of projection to get a ratio. Then creates a table of the 5 number summary and mean for this ratio for each group fleet strategy combination and writes the results as a table to csv.
You can specify for which of total catch, landings, discards and value you want this file to do the calculations at the bottom of the file.

Calc_Mean5YearBiomass_FiveNum_Sum.R
Calculates the last 5 year mean biomass and calculates the 5 number summary, mean, percent below Bpa and percent below Blim. It does this for each group strategy combination, across models.

Calc_Mean5YearCatch_FiveNum_Sum.R
Calculates the mean and 5 number summary for last 5 year mean catch (landings, discards, total catch and value) across all models by each group, fleet and strategy combination.

Calc_Percent_Below_Conservation_Limits.R
Calculates the percentage of the last 5 year means that were below the upper and lower conservation limits, by each group and strategy combination and outputs results as a csv table.

Calc_Percent_Catch_Above_Below_Quota.R
Calculates the percentage of the total last 5 year catches below and above the total last 5 year quota and outputs results as a csv table. Values calculated for each group strategy combination.

Calc_Percent_RealisedF_Above_Below_FMax.R
Calculates the percentage of the realised landed Fs that were above or below Fmax for each group strategy combination.

cleaning_results_folder_of_null_files.R
You can point this at a folder and this code will delete all files containing a specified null value.

Create_all_tables.R
This is just a way of running all the source code files that generate table csvs

Create_landings-quota_file.R
Calculates the difference between the landings and the quota and uses the result to produce 3 different files. (1) The values for each group strategy model timestep combination, (2) The values averaged across time series and models for strategy group combination, and (3) the sum across timeseries for each model strategy group combination.

Effort_Trajectories.R
Plots the effort trajectories.

Extract_BasicInfo.R
Obselete
F_Trajectories.R
Is used to plot trajectories for HCRF_Targ, HCRF_Cons, HCRQuota_Cons, HCRQuota_Targ, RealisedLandedF, RealisedDiscardedF, RealisedF, CatchTrajectories, LandingsTrajectories, DiscardsTrajectories.

Pies.R
Plots the piecharts for highest value and choke species groups.

Plot_Distribution_Last5YearsBiomassMean.R
Plots the distribution of the last 5 year biomass mean for each group strategy combination. Calculates the last 5 year mean biomass and then from this the 5 number summary, mean and the percent of models below bpa and blim and adds to plots.
Run_DistributionPlots.R
Creates distributions for the biomass end, biomass minimum, discard mortalities, discard survivals and landings stored in the results.csv file.
PlotEndDistributions.r
Plots the distribution of the biomass’ at the last time step. This should also work for other data types but it doesn’t work at the moment.

PlotEndDistributions_RealisedFs.R
Doesn’t work at the moment.

PlotFleetEndDistributions.R
Doesn’t work at the moment.

PlotScript_AverageQuotaAcrossModelsAndRegTypes.R
This is the script that sets up lots of variables and uses them to call Plot_Average_Quotas

Produce_Tables.R
This is just a short script that loads the source files for producing tables. The sources are:
•	source("Calc_Percent_RealisedF_Above_Below_FMax.R")
•	source("Calc_Percent_Catch_Above_Below_Quota.R")
•	source("Calc_Catch_Start_End_Ratio.R")
•	source("Calc_Percent_Below_Conservation_Limits.R")
•	source("Calc_Mean5YearBiomass_FiveNum_Sum.R")

removing_last_line_when_incomplete.R
This script is used to go through all the folders checking the results files for when the last row is incomplete and needs deleting.

riskrewardtable.R
Generates risk-reward tables.

sample_dirichlet.R
Used to plot Dirichlet distributions and select the most appropriate multiplier to represent uncertainty in a predators diet.

share_tools.R
A collection of shared functions that are used by many of the core functions listed here.

Strategy_Batch_Run.R
Allows plotting 15July_2016_ChokeHighestValue_byFleet - Refactor.R to be used to plot for several sub groups of strategies. This is important when there are so many strategies that plots become too crowded and we need to plot different strategy groups on different plots.

Tests.R
Contains unit tests for a number of functions. Note that these tests are not comprehensive and were written alongside learning unit testing for the first time; some tests may not be useful.

Value_Trajectories.R
Plots the value trajectories. 

Create_landings-quota_file.R
Produces three different files aggregated in different ways for the difference between the landings and quota. Landings.minus.quota.modified is the same as Landings.minus.quota with the modification that if the species is the main choke species for a year-fleet-strategy-model combination then it returns a zero.
landings-quota: This is not aggregated and provides values for each year-fleet-strategy-model combination.
sumByModelID,StrategyName: Summed across the timeseries
averageAcrossModels: Averages “sumByModelID,StrategyName” across models.