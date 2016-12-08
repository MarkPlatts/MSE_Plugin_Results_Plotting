
#Calculate/load basic info  =======================================================================================
#Need to put what info this is here

Extract_BasicInfo = function(params)
{
  #change this to params$RootPath and plot_path when completed testing and it works
  test_rootpath = "C://Users/Mark/Dropbox/GAP2_MSE Plugin2/North Sea MultiAnnual Plan/withBiomassForcing_Yearly_Results HCR type1 and 3/Results"
  test_plotpath = "C://Users/Mark/Dropbox/GAP2_MSE Plugin2/North Sea MultiAnnual Plan/withBiomassForcing_Yearly_Results HCR type1 and 3/Plots/"
  
  BasicInfoFile_Exists = file.exists(paste(test_plotpath,"BasicInfo.csv",sep=''))
  
  #Check where the file should exist for storing this info
  if(BasicInfoFile_Exists){
    #Load up the file and check that it is up-to-date with the result files
    BasicInfo = read.csv(paste(test_plotpath,"BasicInfo.csv",sep=''))
    
    #read the date of the Results.csv file
    file_info = file.info(paste(test_rootpath,"Results.csv",sep=''))
    
    if (TRUE){ #todo need to check whether info is correct

    }

  } else {
    ### Load results files to get unique strats
    results<-read.table(paste(test_rootpath,"Results.csv",sep=''),sep=',',skip=8,col.names=c("Model","Strategy","GroupID","GroupName","Variable","Value"), fill=T)
    results<-results[results$Strategy!="Z",]#odd one in _SR_final
    params$strats <- as.character(unique(results$Strategy))  # 10/15 strategies
    #params$strats <- params$strats[params$strats != "NONE"]
    
    df_output = data.frame()
    #write the values to file
    df_output = rbind(df_output, c("Date_time"))
    for (iStrat in params$strats){
      df_output = rbind(df_output,c("Strategy", iStrat))
    }
    write.csv(df_output,paste(test_plotpath,"BasicInfo.csv",sep=''), )
  }


  
}

#Do this by checking whether a file exists with this information
#If the file exists check within the file that the time/date of the files used to extract this
#information are the same as the current files time/dates
#If they are then extract the info from the file
#If not then recalculate and save the info in this file