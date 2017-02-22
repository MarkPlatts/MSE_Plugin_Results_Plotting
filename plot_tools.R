create.plot.dirs = function(params){
  if(!dir.exists(params$plot.path)){
    dir.create(params$plot.path)
  }
  FolderNames = c(
    # "OUTPUT_GEARSGROUPSbySTRATEGIES", "OUTPUT_GROUP_FIGS", "OUTPUT_END_DISTRIBUTIONS", 
    #               "OUTPUT_GEARSbySTRATEGIES", "OUTPUT_CHOKE_PIES", "OUTPUT_HIGHEST_PIES", "AVERAGE_REGS", 
                  "HCRF_Cons", "HCRF_Targ", "HCRQuota_Cons", "HCRQuota_Targ", 
                  "RealisedLandedF", "RealisedDiscardedF", "RealisedF", 
                  "CatchTrajectories", "LandingsTrajectories", "DiscardsTrajectories",
                  "EFFORT", "VALUE", "BIOMASS", 
                  "HIGHEST_VALUE", "CHOKE_GROUPS", 
                  "AverageQuota_EachFleet",
                  "Tables", 
                  "OUTPUT_END_DISTRIBUTIONS")
  for (iFolder in FolderNames){
    if (!dir.exists(paste(params$plot.path,iFolder,sep=""))){
      dir.create(paste(params$plot.path,iFolder,sep=""))
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
      print (paste("Did not find biomrefs for ", group, "group specified"))
      return (NA)
    }
  }
}