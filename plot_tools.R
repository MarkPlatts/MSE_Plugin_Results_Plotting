create.plot.dirs = function(){
  FolderNames = c("OUTPUT_GEARSGROUPSbySTRATEGIES", "OUTPUT_GROUP_FIGS", "OUTPUT_COMPARE_STRATS", 
                  "OUTPUT_GEARSbySTRATEGIES", "OUTPUT_HIGHEST_CHOKE_FIGS", "OUTPUT_percentiles")
  for (iFolder in FolderNames){
    #browser()
    if (!dir.exists(paste(plot.path,iFolder,sep=""))){
      dir.create(paste(plot.path,iFolder,sep=""))
    }
  }
}

read_biom_refs = function(biom_refs, group, ref_type)
{
  group = as.character(group)
  e_blim = 2
  e_bpa = 3
  #load up the file with the values in
  for(iRow in 1:(dim(biom_refs)[1])){
    if(group == biom_refs[iRow,1]){
      if (ref_type == "blim") return (biom_refs[iRow, e_blim])
      if (ref_type == "bpa") return (biom_refs[iRow, e_bpa])
      break
    }
    if (iRow == (dim(biom_refs)[1])){
      print ("Did not find the group specified")
      return (NA)
    }
  }
}