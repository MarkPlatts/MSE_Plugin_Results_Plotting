rm(list=ls())

setwd("C:/Users/Mark/Desktop/MSE_Plugin_Results_Plotting/")
source("Calc_average_quota_per_fleet_group_regulation.R")
source("share_tools.R")
source("riskrewardtable.R")
library("dplyr")

print(Sys.time())

#Test
Test__Check_Average_Quota_across_models_4_AdultCod_Fleet1_HighestValue = function()
{
  print("Test__Check_Average_Quota_across_models_4_AdultCod_Fleet1_HighestValue")
  
  #Input data:
  Group_Cod = "Cod (adult)"
  Fleet_DemersalTrawl = "FleetNo1"
  StrategyType_Highest_Value = "Highest value"
  Test_Path = GetResultsLocation()
  
  #Test:
  Cod_adult_Average_Across_Quota = Average_Quota_Across_Models_And_RegTypes(Group_Cod,Fleet_DemersalTrawl,StrategyType_Highest_Value,Test_Path)
  Correct = c(0.0562363408,	0.0691622814,	0.0740509911,	0.0683924373,	0.0587781737,	0.0484810997,	0.0413565884,	0.0390158918,	0.0417923933,	0.0486157672,	0.0489791314,	0.0408512426,	0.0334640422,	0.028770516,	0.0250194448,	0.0224817797,	0.0219612043,	0.021143502,	0.0180477091,	0.0148551648)
  
  #Outcome
  if(isTRUE(all.equal(Cod_adult_Average_Across_Quota, Correct, tolerance = 1e-8))){
    print("PASSED")
  } else{
    print("FAILED")
    
  }
}


#Test
Test__LoadFile_ContainsListStrings = function()
{
  print("Test__LoadFile_ContainsListStrings")
  
  #Input data:
  Group_Skate_and_Ray = "Targ_Skate + cuckoo ray"
  Fleet_GearsUsingHooks = "FleetNo7"
  Test_Path = paste(GetResultsLocation(),"HCRQuota_Targ/",sep="")
  
  #Test:
  ResultFile = LoadFile_ContainsListStrings(Dir.Path = Test_Path, StringsInFileName = c(Group_Skate_and_Ray,Fleet_GearsUsingHooks))
  CorrectFile = read.csv(paste(GetResultsLocation(), "HCRQuota_Targ/HCR_Quota_Targ_Skate + cuckoo ray_GroupNo12_FleetNo7.csv",sep=""),skip=7, head=T)
 
  #Outcome
  if(all.equal(ResultFile,CorrectFile)){
    print("PASSED")
  } else {
    print("FAILED")
    
  }
}

#Test
Test__StringContains_StringDoesNotExist = function()
{
  print("Test__StringContains_StringDoesNotExist")
  
  #input:
  FileName = "HCR_Quota_Targ_Benthic microflora (incl Bacteria protozoa))_GroupNo63_FleetNo3"
  String2test = "Hello"
  
  #Test:
  Outcome = StringContains(FileName, String2test)
  
  #Results
  if (Outcome==TRUE){
    print("FAILED!")
    
  }
  if (Outcome==FALSE){
    print("PASSED!")
  }
  
}


Test__StringContains_StringDoesExist = function()
{
  print("Test__StringContains_StringDoesExist")
  
  #Input:
  FileName = "HCR_Quota_Targ_Benthic microflora (incl Bacteria protozoa))_GroupNo63_FleetNo3"
  String2test = "Benthic microflora (incl Bacteria protozoa)"
  
  #Test:
  Outcome = StringContains(FileName, String2test)
  
  #Result:
  if (Outcome==TRUE){
    print("PASSED!")
  }
  if (Outcome==FALSE){
    print("FAILED!")
    
  }
}

Test__StringContains_AllStrings_StringsDoExist_FindsThem = function()
{
  print("Test__StringContains_AllStrings_StringsDoExist_Finds_Them") 
  
  #Input
  MultipleStringsVector = c("Hello", "Goodbye")
  String_Containing_String1_and_String2 = "Hello John, its very nice to meet you, Sorry I cant stop. Goodbye John"
  
  #Test:
  Outcome = StringContains_AllStrings(ContainingString = String_Containing_String1_and_String2,
                                      MultipleStrings2Check = MultipleStringsVector)
    
  #Result:
  if(Outcome == FALSE){
    print("FAILED!")
    
  }
  if(Outcome == TRUE){
    print("PASSED!")
  }
}

Test__StringContains_AllStrings_StringsDontExist_DoesNotFindThem = function()
{
  print("Test__StringContains_AllStrings_StringsDontExist_DoesNotFindThem")
  
  #Input
  MultipleStringsVector = c("Hello", "Goodbye")
  String_Containing_String1_and_String2 = "Hello John, its very nice to meet you, Sorry I cant stop. John"
  
  #Test:
  Outcome = StringContains_AllStrings(ContainingString = String_Containing_String1_and_String2,
                                      MultipleStrings2Check = MultipleStringsVector)
  #Result:
  if(Outcome == TRUE){
    print("FAILED!")
    
  }
  if(Outcome == FALSE){
    print("PASSED!")
  }
}


Test__SubsetVectorStrings_ContainingString__Finds_All_Highest_Value = function()
{
  print("Test__SubsetVectorStrings_ContainingString__Finds_All_Highest_Value")
  #Input
  UniqueStrategies = Load_Strategies("../TestInput/UniqueStrategies.csv")
  Regulation_HighestValue = "Highest value"
  CorrectStrategies = as.vector(Load_Strategies("../CorrectResults/Highest_value_Strategies.csv"))
  
  #Test:
  Outcome = SubsetVectorStrings_ContainingString(UniqueStrategies, Regulation_HighestValue)

  #Result:
  if(all.equal(Outcome, CorrectStrategies)){
    print("PASSED!")
  } else {
    print("FAILED!")
    
  }
}


Test__LoadUniqueStrategies__SameUniqueFile = function()
{
  print("Test__LoadUniqueStrategies__SameUniqueFile")
  
  #Input
  Outcome = LoadUniqueStrategies(GetResultsLocation())
  
  #Test:
  CorrectStrategies = as.vector(Load_Strategies("../TestInput/UniqueStrategies.csv"))
  
  #Result:
  if(all.equal(Outcome, CorrectStrategies)){
    print("PASSED!")
  } else {
    print("FAILED!")
    
  }
}


Test__LoadUniqueGroups__Loads_when_file_exists = function()
{
  print("Test__LoadUniqueGroups__Loads_when_file_exists")
  
  #Input
  Correct_First_Group = "Baleen whales"
  Correct_30th_Group = "Sprat"
  Correct_Last_Group = "Phytoplankton"
  
  #Test:
  Outcome = LoadUniqueGroups(paste(GetResultsLocation(),"../TestInput/LoadsUniqueGroupsWhenExists/",sep=""))
  
  #Result:
  if(Outcome[1] == Correct_First_Group & Outcome[30] == Correct_30th_Group & tail(Outcome, n=1) == Correct_Last_Group){
    print("PASSED!")
  } else {
    print("FAILED!")
    
  }
}


Test__LoadUniqueGroups__Creates_from_results.csv_when_doesnt_exist = function()
{
  print("Test__LoadUniqueGroups__Creates_from_results.csv_when_doesnt_exist")
  
  #Input
  Correct_First_Group = "Baleen whales"
  Correct_30th_Group = "Sprat"
  Correct_Last_Group = "Phytoplankton"
  #Get rid of old file if exists
  if(file.exists(paste(GetResultsLocation(),"../TestInput/CreatesUniqueGroupsWhenDoesntExist/UniqueGroups.csv",sep=""))) 
  {
    file.remove(paste(GetResultsLocation(),"../TestInput/CreatesUniqueGroupsWhenDoesntExist/UniqueGroups.csv",sep=""))
  }           
  
  #Test:
  Outcome = LoadUniqueGroups(paste(GetResultsLocation(),"../TestInput/CreatesUniqueGroupsWhenDoesntExist/",sep=""))

  #Result:
  if(Outcome[1] == Correct_First_Group & Outcome[30] == Correct_30th_Group & tail(Outcome, n=1) == Correct_Last_Group){
    print("PASSED!")
  } else {
    print("FAILED!")
  }
}


Test__Calc_Biomass_Last5YearMean__Cod_adult_Model_6_Strat_10_Safegd_LowF_Weakest_stock= function()
{
  print("Test__Calc_Biomass_Last5YearMean__Cod_adult_Model_6_Strat_10_Safegd_LowF_Weakest_stock")
  
  #Input
  GroupName = "Cod (adult)"
  ModelID = 6
  Biomass_StartAverageAtTimeStep = 39
  Path_And_FileName = "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/R Code/TestFolder_R_Plotting_MSE_Plugin/withBiomassForcing_Yearly_Results HCR type1 and 3/Results/Biomass/BiomassYearly_Cod (adult)_GroupNo14.csv"
  GroupsOnly_Vars2MeltBy = c("GroupName","ModelID","StrategyName","ResultType")
  
  #Test:
  Averages = Calc_Last5YearMean(Path_And_FileName, "Biomass_Last5YearMean", GroupsOnly_Vars2MeltBy, Biomass_StartAverageAtTimeStep)
  calculated_value = as.numeric(filter(Averages,ModelID==6 & StrategyName == "10 Safegd_LowF_Weakest stock")[4])
  
  #Correct value
  correct_value = 0.4603413244
  
  #Result:
  if(isTRUE(all.equal(correct_value, calculated_value, 1e-7))){
    print("PASSED!")
  } else {
    print("FAILED!")
  }
}


Test__Calc_CatchLast5YearMean__Cod_adult_Model_6_Strat_10_Safegd_LowF_Weakest_stock= function()
{
  print("Test__Calc_CatchLast5YearMean__Cod_adult_Model_6_Strat_10_Safegd_LowF_Weakest_stock")
  
  #Input
  GroupName = "Cod (adult)"
  ModelID = 6
  CatchTrajectories_StartAverageAtTimeStep = 16
  Path_And_FileName = "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/R Code/TestFolder_R_Plotting_MSE_Plugin/withBiomassForcing_Yearly_Results HCR type1 and 3/Results/CatchTrajectories/TotalCatchYearly_Cod (adult)_GroupNo14_AllFleets.csv"
  GroupFleet_Vars2MeltBy = c("GroupName","FleetName","ModelID","StrategyName","ResultType")
  
  #Test:
  Averages = Calc_Last5YearMean(Path_And_FileName, "Catches_Last5YearMean", GroupFleet_Vars2MeltBy, CatchTrajectories_StartAverageAtTimeStep)
  calculated_value = as.numeric(filter(Averages,ModelID==6 & StrategyName == "10 Safegd_LowF_Weakest stock")[4])
  
  #Correct value
  correct_value = 0.0587325677
  
  #Result:
  if(isTRUE(all.equal(correct_value, calculated_value, 1e-7))){
    print("PASSED!")
  } else {
    print("FAILED!")
  }
}



Test__Calc_ValueLast5YearMean__Cod_adult_Model_6_Strat_10_Safegd_LowF_Weakest_stock= function()
{
  print("Test__Calc_ValueLast5YearMean__Cod_adult_Model_6_Strat_10_Safegd_LowF_Weakest_stock")
  
  #Input
  GroupName = "Cod (adult)"
  ModelID = 6
  CatchTrajectories_StartAverageAtTimeStep = 16
  Path_And_FileName = "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/R Code/TestFolder_R_Plotting_MSE_Plugin/withBiomassForcing_Yearly_Results HCR type1 and 3/Results/ValueTrajectories/ValueYearly_Cod (adult)_GroupNo14_AllFleets.csv"
  GroupFleet_Vars2MeltBy = c("GroupName","FleetName","ModelID","StrategyName","ResultType")
  
  #Test:
  Averages = Calc_Last5YearMean(Path_And_FileName, "Catches_Last5YearMean", GroupFleet_Vars2MeltBy, CatchTrajectories_StartAverageAtTimeStep)
  calculated_value = as.numeric(filter(Averages,ModelID==6 & StrategyName == "10 Safegd_LowF_Weakest stock")[4])
  
  #Correct value
  correct_value = 0.1355333742

  
  #Result:
  if(isTRUE(all.equal(correct_value, calculated_value, 1e-7))){
    print("PASSED!")
  } else {
    print("FAILED!")
  }
}



Test__Calc_StartBiomass__Cod_adult_Model_6_Strat_10_Safegd_LowF_Weakest_stock= function()
{
  print("Test__Calc_StartBiomass__Cod_adult_Model_6_Strat_10_Safegd_LowF_Weakest_stock")
  
  #Input
  GroupName = "Cod (adult)"
  ModelID = 6
  Biomass_StartTimeStep = 23
  Path_And_FileName = "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/R Code/TestFolder_R_Plotting_MSE_Plugin/withBiomassForcing_Yearly_Results HCR type1 and 3/Results/Biomass/BiomassYearly_Cod (adult)_GroupNo14.csv"
  GroupsOnly_Vars2MeltBy = c("GroupName","ModelID","StrategyName","ResultType")
  
  #Test:
  BiomassStarts = Get_StartBiomass(Path_And_FileName, "Biomass_Start", GroupsOnly_Vars2MeltBy, Biomass_StartTimeStep)
  calculated_value = as.numeric(filter(BiomassStarts,ModelID==6 & StrategyName == "10 Safegd_LowF_Weakest stock")[4])
  
  #Correct value
  correct_value = 0.352146329979102
  
  #Result:
  if(isTRUE(all.equal(correct_value, calculated_value, 1e-7))){
    print("PASSED!")
  } else {
    print("FAILED!")
  }
}




# Testing_Helper_Methods --------------------------------------------------

#Load the strategies from uniquestrategies file to save having to load a big file up
Load_Strategies = function(path2strats)
{
  return(as.matrix(read.csv(FileLocation(path2strats))))
}

FileLocation = function(branch)
{
  ResultsPath = "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/R Code/TestFolder_R_Plotting_MSE_Plugin/withBiomassForcing_Yearly_Results HCR type1 and 3/Results/"
  return(paste(ResultsPath,branch,sep=""))
}

GetResultsLocation = function()
{
  return("C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/R Code/TestFolder_R_Plotting_MSE_Plugin/withBiomassForcing_Yearly_Results HCR type1 and 3/Results/")
}

# Run Tests ---------------------------------------------------------------
# Test__Check_Average_Quota_across_models_4_AdultCod_Fleet1_HighestValue()
# Test__LoadUniqueStrategies__SameUniqueFile()
# Test__SubsetVectorStrings_ContainingString__Finds_All_Highest_Value()
# Test__StringContains_AllStrings_StringsDontExist_DoesNotFindThem()
# Test__StringContains_AllStrings_StringsDoExist_FindsThem()
# Test__LoadFile_ContainsListStrings()
# Test__StringContains_StringDoesNotExist()
# Test__StringContains_StringDoesExist()
# Test__LoadUniqueGroups__Loads_when_file_exists()
# Test__LoadUniqueGroups__Creates_from_results.csv_when_doesnt_exist()
Test__Calc_Biomass_Last5YearMean__Cod_adult_Model_6_Strat_10_Safegd_LowF_Weakest_stock()
Test__Calc_CatchLast5YearMean__Cod_adult_Model_6_Strat_10_Safegd_LowF_Weakest_stock()
Test__Calc_ValueLast5YearMean__Cod_adult_Model_6_Strat_10_Safegd_LowF_Weakest_stock()
Test__Calc_StartBiomass__Cod_adult_Model_6_Strat_10_Safegd_LowF_Weakest_stock()
