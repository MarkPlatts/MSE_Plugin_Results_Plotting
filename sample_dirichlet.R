#Plotting dirichlet distributions
rm(list = ls())

library(data.table)
library(DirichletReg)
library(ggplot2)
library(dtplyr)
library(dplyr)

plot_dirichlet = function(diets, predator.to.plot, multipliers, prey.to.exclude, y.max.coord, bwidth,
                          nSamples){

  colnames = names(diets)[2:length(names(diets))]

  dt = data.table()
  
  diet.for.iPred = diets[,list(get("Prey \\ predator"), get(predator.to.plot))]
  diet.for.iPred = diet.for.iPred[V2!=0]
  prey.names = diet.for.iPred$V1
  dirichlet.params = diet.for.iPred$V2
  
  for(iMultiplier in multipliers){

    dirichlet.params.multiplied = dirichlet.params * iMultiplier
    sampled.diets = rdirichlet(1000,dirichlet.params.multiplied)
    
    for(iPrey in 1:length(prey.names)){
      temp.dt = data.table(Predator = predator.to.plot, Prey = prey.names[iPrey], Multiplier=iMultiplier, diet = sampled.diets[,iPrey])
      dt = rbind(dt, temp.dt)           
    }

  }
  
  dt = filter(dt, !(Prey %in% prey.to.exclude))

  q = qplot(data=dt, x=diet, facets = Multiplier~Prey, 
            binwidth=bwidth, main=paste("Predator:",predator.to.plot), xlim=c(0,1))
  q = q + coord_cartesian(ylim = c(0, y.max.coord))
  # ggsave(q, file=paste("C:/Users/Mark/Box Sync/Baltic - Stockholm/Plots/dirichlet_plots/",predator.to.plot, ".png", sep=""), width=6, height=length(params$strats)*1.5, limitsize = FALSE)
  ggsave(q, file=paste("C:/Users/Mark/Box Sync/Baltic - Stockholm/Plots/dirichlet_plots/plots/",predator.to.plot, ".png", sep=""), width=18, height=12)


}



diet.table = fread(header=T, "C:/Users/Mark/Box Sync/Baltic - Stockholm/Plots/dirichlet_plots/data/DietMeans.csv")
#chosen_multipliers = seq(5,45,5)
chosen_multipliers = c(0.5,1,2,4,8,16,32,64,128,256,512)
prey.exclude = c("")
y.max = 300
binwidth = 0.01
nSamples = 1000
pred.to.plot = c(
  "Grey seal",
  "Fish-feeding birds",
  "JuvCod",
  "AdCod_3",
  "JuvHer",
  "AdHer_2",
  "JuvSpr",
  "AdSpr_2",
  "JuvFlo",
  "AdFlo_3",
  "Saduria entomon",
  "Mytilus sp.",
  "Macoma b.",
  "Oth.macrob",
  "Meiobenthos",
  "Mysids",
  "Other zooplankton",
  "Pseudocalanus sp",
  "Acartia sp.",
  "Temora sp")
for(iPred in pred.to.plot){
  plot_dirichlet(diets=diet.table, predator.to.plot=iPred, multipliers=chosen_multipliers, 
               prey.to.exclude = prey.exclude, y.max.coord = y.max, bwidth = binwidth,
               nSamples = nSamples)
}
