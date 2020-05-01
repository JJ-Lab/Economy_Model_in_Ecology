
library(tidyverse)
library(igraph)
library(fitdistrplus)


folder_base <- "web-of-life_WDB/"
files_base <- list.files(folder_base)
list_files <- files_base[grepl("M_PL_", files_base)]

for (i in seq(length(list_files))){

  
  # Extract the incidence matrix
  inc_matrix <- read.csv(paste0(folder_base,list_files[i]), header=TRUE, row.names=1)
  
  # Create a graph for each layer
  g_i <- graph_from_incidence_matrix(inc_matrix, directed = FALSE, weighted = TRUE)
  
  # Extract plant and pollinator labels, respectively
  poll_names <- colnames(inc_matrix)
  plant_names <- rownames(inc_matrix)
  
  # Calcualte plant and pollinator strengths, respectively
  strength_plants <- strength(g_i,vids = plant_names)
  strength_poll <- strength(g_i,vids = poll_names)
  
  #plotdist(strength_plants, histo = TRUE, demp = TRUE)
  
  # fitting log-normal distributions to data
  
  name_i <- strsplit(list_files[i],".csv")
  pdf(paste0("lnor_fit_plots/Plants_in_",name_i[[1]],".pdf"),
      width = 11.69, # The width of the plot in inches
      height = 8.27)
  
  fln_plants <- fitdist(strength_plants, "lnorm")
  par(mfrow = c(2, 2))
  denscomp(fln_plants)
  qqcomp(fln_plants)
  cdfcomp(fln_plants)
  ppcomp(fln_plants)
  mtext(paste0("Plants in ",list_files[i]," (MLE)"), outer=TRUE,  cex=1, line=-1.3)
  
  dev.off()
  
  pdf(paste0("lnor_fit_plots/Pollinators_in_",name_i[[1]],".pdf"),
      width = 11.69, # The width of the plot in inches
      height = 8.27)
  
  fln_poll <- fitdist(strength_poll, "lnorm")
  par(mfrow = c(2, 2))
  denscomp(fln_poll)
  qqcomp(fln_poll)
  cdfcomp(fln_poll)
  ppcomp(fln_poll)
  mtext(paste0("Pollinators in ",list_files[i]," (MLE)"), outer=TRUE,  cex=1, line=-1.3)
  
  dev.off()
  
}
