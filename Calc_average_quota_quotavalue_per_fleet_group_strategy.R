#Test
Calc_Average_Quota_across_models_Test = function()
{
  Group_Cod = "Cod (adult)"
  Fleet_DemersalTrawl = "Fleet1"
  StrategyType_Highest_Value = "Highest value"
  Test_Path = "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/R Code/TestFolder_R_Plotting_MSE_Plugin/withBiomassForcing_Yearly_Results HCR type1 and 3/Results/HCRF_Targ"
  Cod_adult_Average_Across_Quota = Calc_Average_Quota_across_models(Group_Cod,StrategyType_Highest_Value,Test_Path)
  Correct = c(0.0562363408,	0.0691622814,	0.0740509911,	0.0683924373,	0.0587781737,	0.0484810997,	0.0413565884,	0.0390158918,	0.0417923933,	0.0486157672,	0.0489791314,	0.0408512426,	0.0334640422,	0.028770516,	0.0250194448,	0.0224817797,	0.0219612043,	0.021143502,	0.0180477091,	0.0148551648)
  if(sum(Cod_adult_Average_Across_Quota!=Correct))
}

Calc_Average_Quota_across_models = function(Groups, StrategyType, ResultsPath)
{
  
  Allfiles <- list.files(paste(ResultsPath,"/HCRQuota_Targ", sep=''))
  
  #Need to loop across all files so that we can extract
  for (iFile in AllFiles){
    #Check that current iFile is for the grou
  }
  
}