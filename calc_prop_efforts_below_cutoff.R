library(ggplot2)
library(stringr)


LoadEfforts <- function(file){
  print(str_c("File: ", file))
  
  nkey_columns <- 4
  
  nrows_to_skip <- 7
  
  dt <- fread(file, head = TRUE, skip = nrows_to_skip)
  nCols  <- length(names(dt))
  dt_mod <- dt[, -((nkey_columns + 1) : (nCols - 1))]
  
  last_col_name <- names(dt)[length(names(dt))]
  
  setnames(dt_mod, last_col_name, "Last_Effort")
  
  return(dt_mod)

}

folder <- "C:/Users/Mark/Dropbox/GAP2_MSE Plugin2/North Sea MultiAnnual Plan/ResultsType1-4_220117/Results/Effort"

effort_cutoff <- 10^-6

efforts <- data.table()

list_effort_files <- list.files(path = folder, full.names = TRUE)


for (iFile in list_effort_files){
  
  temp_efforts <- LoadEfforts(iFile)
  
  efforts <- rbind(efforts, temp_efforts)

  nUniqueModels <- length(unique(efforts$ModelID))
  
  proportions <- efforts[Last_Effort < effort_cutoff, .(Proportion_Zero = .N/nUniqueModels), by = .(FleetName, StrategyName)]
}

# Manual levels
a <- proportions[FleetName == "Beam trawl"] #<<<<<<<<=========== tweak fleet to order by here!!!!
strategy_levels <- a[order(a$Proportion_Zero)]$StrategyName
proportions$StrategyName2 <- factor(proportions$StrategyName, levels = strategy_levels)

g <- ggplot(data = proportions, aes(x = StrategyName2, y = Proportion_Zero))
g <- g + labs(y = str_c("Proportion of Runs with Effort Below ", effort_cutoff))
g <- g + geom_bar(stat = "identity")
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
g <- g + facet_grid(FleetName~.)
g <- g + theme(strip.text.y = element_text(angle = 0))
print(g)
