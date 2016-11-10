
cleanfile = function(file_2_clean)
{
  my_file = tryCatch(readLines(file_2_clean),error=function(e) e, warning=function(w) w)
  if(is(my_file,"warning")) {
    my_file = readLines(file_2_clean)
    my_file_cleaned = my_file[-length(my_file)]
    fileConn<-file(file_2_clean)
    writeLines(my_file_cleaned, fileConn)
    close(fileConn)
  }
}

cleanfolder = function(folder_2_clean){
  AllFiles = list.files(folder_2_clean, full.names=T)
  for(iFile in AllFiles){
    cleanfile(iFile)
  }
  
}

RootFolder = "C:/Users/Mark/Desktop/1000Models_May2016/Results"

cleanfolder(paste(RootFolder,"/Biomass", sep=""))
cleanfolder(paste(RootFolder,"/CatchTrajectories", sep=""))
cleanfolder(paste(RootFolder,"/DiscardsTrajectories", sep=""))
cleanfolder(paste(RootFolder,"/ChokeGroup", sep=""))
cleanfolder(paste(RootFolder,"/Effort", sep=""))
# cleanfolder(paste(RootFolder,"/HCRF_Cons", sep=""))
cleanfolder(paste(RootFolder,"/HCRF_Targ", sep=""))
cleanfolder(paste(RootFolder,"/HCRQuota_Cons", sep=""))
cleanfolder(paste(RootFolder,"/HCRQuota_Targ", sep=""))
cleanfolder(paste(RootFolder,"/HighestValueGroup", sep=""))
cleanfolder(paste(RootFolder,"/LandingsTrajectories", sep=""))
cleanfolder(paste(RootFolder,"/RealisedDiscardedF", sep=""))
cleanfolder(paste(RootFolder,"/RealisedF", sep=""))
cleanfolder(paste(RootFolder,"/RealisedLandedF", sep=""))
cleanfolder(paste(RootFolder,"/ValueTrajectories", sep=""))