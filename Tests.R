setwd("C:/Users/Mark/Desktop/MSE_Plugin_Results_Plotting/")
source("Calc_average_quota_per_fleet_group_regulation.R")

print(Sys.time())

#Test
Test__Check_Average_Quota_across_models_4_AdultCod_Fleet1_HighestValue = function()
{
  #Input data:
  Group_Cod = "Cod (adult)"
  Fleet_DemersalTrawl = "FleetNo1"
  StrategyType_Highest_Value = "Highest value"
  Test_Path = "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/R Code/TestFolder_R_Plotting_MSE_Plugin/withBiomassForcing_Yearly_Results HCR type1 and 3/Results/"
  
  #Test:
  Cod_adult_Average_Across_Quota = Average_Quota_Across_Models_And_RegTypes(Group_Cod,Fleet_DemersalTrawl,StrategyType_Highest_Value,Test_Path)
  Correct = c(0.0562363408,	0.0691622814,	0.0740509911,	0.0683924373,	0.0587781737,	0.0484810997,	0.0413565884,	0.0390158918,	0.0417923933,	0.0486157672,	0.0489791314,	0.0408512426,	0.0334640422,	0.028770516,	0.0250194448,	0.0224817797,	0.0219612043,	0.021143502,	0.0180477091,	0.0148551648)
  
  #Outcome
  if(sum(Cod_adult_Average_Across_Quota!=Correct)>0){
    print("Test__Check_Average_Quota_across_models_4_AdultCod_Fleet1_HighestValue FAILED")
    #print(paste("Cod_adult_Average_Across_Quota = ", Cod_adult_Average_Across_Quota))
    #print(paste("Correct = ", Correct))
  } else{
    print("Test__Check_Average_Quota_across_models_4_AdultCod_Fleet1_HighestValue PASSED")
  }
}


#Test
Test__LoadFile_ContainsListStrings = function()
{
  #Input data:
  Group_Skate_and_Ray = "Targ_Skate + cuckoo ray"
  Fleet_GearsUsingHooks = "FleetNo7"
  Test_Path = "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/R Code/TestFolder_R_Plotting_MSE_Plugin/withBiomassForcing_Yearly_Results HCR type1 and 3/Results/HCRQuota_Targ/"
  
  #Test:
  ResultFile = LoadFile_ContainsListStrings(Dir.Path = Test_Path, StringsInFileName = c(Group_Skate_and_Ray,Fleet_GearsUsingHooks))
  CorrectFile = read.csv("C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/R Code/TestFolder_R_Plotting_MSE_Plugin/withBiomassForcing_Yearly_Results HCR type1 and 3/Results/HCRQuota_Targ/HCR_Quota_Targ_Skate + cuckoo ray_GroupNo12_FleetNo7.csv",skip=7, head=T)
  #Outcome
  if(ResultFile!=CorrectFile){
    print("Test__LoadFile_ContainsListStrings FAILED")
  } else{
    print("Test__LoadFile_ContainsListStrings PASSED")
  }
}

#Test
Test__StringContains_StringDoesNotExist = function()
{
  #input:
  FileName = "HCR_Quota_Targ_Benthic microflora (incl Bacteria protozoa))_GroupNo63_FleetNo3"
  String2test = "Hello"
  
  #Test:
  Outcome = StringContains(FileName, String2test)
  
  #Results
  if (Outcome==TRUE){
    print("Test__StringContains_StringDoesNotExist FAILED!")
  }
  if (Outcome==FALSE){
    print("Test__StringContains_StringDoesNotExist PASSED!")
  }
  
}


Test__StringContains_StringDoesExist = function()
{
  #Input:
  FileName = "HCR_Quota_Targ_Benthic microflora (incl Bacteria protozoa))_GroupNo63_FleetNo3"
  String2test = "Benthic microflora (incl Bacteria protozoa)"
  
  #Test:
  Outcome = StringContains(FileName, String2test)
  
  #Result:
  if (Outcome==TRUE){
    print("Test__StringContains_StringDoesNotExist PASSED!")
  }
  if (Outcome==FALSE){
    print("Test__StringContains_StringDoesNotExist FAILED!")
  }
}

Test__StringContains_AllStrings_StringsDoExist_FindsThem = function()
{
  #Input
  MultipleStringsVector = c("Hello", "Goodbye")
  String_Containing_String1_and_String2 = "Hello John, its very nice to meet you, Sorry I cant stop. Goodbye John"
  
  #Test:
  Outcome = StringContains_AllStrings(ContainingString = String_Containing_String1_and_String2,
                                      MultipleStrings2Check = MultipleStringsVector)
    
  #Result:
  if(Outcome == FALSE){
    print("Test__StringContains_AllStrings_StringsDoExist_Finds_Them FAILED!")
  }
  if(Outcome == TRUE){
    print("Test__StringContains_AllStrings_StringsDoExist_Finds_Them PASSED!")
  }
}

Test__StringContains_AllStrings_StringsDontExist_DoesNotFindThem = function()
{
  #Input
  MultipleStringsVector = c("Hello", "Goodbye")
  String_Containing_String1_and_String2 = "Hello John, its very nice to meet you, Sorry I cant stop. John"
  
  #Test:
  Outcome = StringContains_AllStrings(ContainingString = String_Containing_String1_and_String2,
                                      MultipleStrings2Check = MultipleStringsVector)
  #Result:
  if(Outcome == TRUE){
    print("Test__StringContains_AllStrings_StringsDontExist_DoesNotFindThem FAILED!")
  }
  if(Outcome == FALSE){
    print("Test__StringContains_AllStrings_StringsDontExist_DoesNotFindThem PASSED!")
  }
}


Test__SubsetVectorStrings_ContainingString__Finds_All_Highest_Value = function()
{

  #Input
  UniqueStrategies = Load_Strategies("../TestInput/UniqueStrategies.csv")
  Regulation_HighestValue = "Highest value"
  CorrectStrategies = as.vector(Load_Strategies("../CorrectResults/Highest_value_Strategies.csv"))
  
  #Test:
  Outcome = SubsetVectorStrings_ContainingString(UniqueStrategies, Regulation_HighestValue)

  #Result:
  if(All_Vector_Indices_Identical(Outcome, CorrectStrategies)){
    print("Test__SubsetVectorStrings_ContainingString__Finds_All_Highest_Value PASSED!")
  } else {
    print("Test__SubsetVectorStrings_ContainingString__Finds_All_Highest_Value FAILED!")
  }
}


Test__LoadUniqueStrategies__SameUniqueFile = function()
{
  
  #Input
  Outcome = LoadUniqueStrategies(GetResultsLocation())
  
  #Test:
  CorrectStrategies = as.vector(Load_Strategies("../TestInput/UniqueStrategies.csv"))
  #Result:
  if(All_Vector_Indices_Identical(Outcome, CorrectStrategies)){
    print("Test__LoadUniqueStrategies__SameUniqueFile PASSED!")
  } else {
    print("Test__LoadUniqueStrategies__SameUniqueFile FAILED!")
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

#Check all indices of a vector are identical
All_Vector_Indices_Identical = function(vec1, vec2)
{
  if(is.null(vec1) | is.null(vec2)) return (FALSE)
  return(sum(vec1!=vec2)==0)
}


# Run Tests ---------------------------------------------------------------
Test__LoadUniqueStrategies__SameUniqueFile()
Test__SubsetVectorStrings_ContainingString__Finds_All_Highest_Value()
Test__StringContains_AllStrings_StringsDontExist_DoesNotFindThem()
Test__StringContains_AllStrings_StringsDoExist_FindsThem()
#Test__Check_Average_Quota_across_models_4_AdultCod_Fleet1_HighestValue()
Test__LoadFile_ContainsListStrings()
Test__StringContains_StringDoesNotExist()
Test__StringContains_StringDoesExist()

