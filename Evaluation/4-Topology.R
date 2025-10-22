rm(list=ls())
library(igraph)
library("VennDiagram")
source("Topo.R")

#################################################################
list_names <- c( "phylum","class","order")
Group = c("total","G0","G1")
p <- 20
for (k in 1:length(Group)){
  for (i in seq_along(list_names)) {
    name <- list_names[i]
    # Read the adjacency matrices based on the list name
    adj1 <- read.csv(paste0("Result/", name,"/",Group[k], "/sparcc_", name, "_",Group[k],".csv"), row.names = 1)
    adj2 <- read.csv(paste0("Result/", name,"/",Group[k], "/SE_glasso_", name, "_",Group[k],".csv"), row.names = 1)
    adj3 <- read.csv(paste0("Result/", name,"/",Group[k], "/SP_", name, "_",Group[k],".csv"), row.names = 1)
    adj4 <- read.csv(paste0("Result/", name,"/",Group[k], "/BN_", name, "_",Group[k],".csv"), row.names = 1)
    # Define the path for saving results
    if (name=="phylum"){
      data_phylum = read.csv("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/data/phylum/da_phylum_clr_scabpit.csv")
      name_otu = colnames(data_phylum)[-ncol(data_phylum)]
    }else if (name=="class"){
      data_class = read.csv("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/data/class/da_class_clr_scabpit.csv")
      name_otu = colnames(data_class)[-ncol(data_class)] 
    }else{
      data_order = read.csv("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/data/order/da_order_clr_scabpit.csv")
      name_otu = colnames(data_order)[-ncol(data_order)]
    }
    
    colnames(adj1)=colnames(adj2)= colnames(adj3)=colnames(adj4) = name_otu
    rownames(adj1)=rownames(adj2)= rownames(adj3)=rownames(adj4) = name_otu
    
    pathh <- paste0("Result/", name,"/",Group[k],"/")
    
    # Run the Topo function
    Topo(pathh, adj1, adj2, adj3, adj4)
  }}

    
methods = c("sparcc","SE_glasso","SP","BN")
for (i in seq_along(list_names)){
  for (j in 1:length(methods)){
    name <- list_names[i]
    # Compare adjacency matrices for G0 and G1
    adj1= read.csv(paste0("Result/",name,"/G0/",methods[j],"_",name,"_G0.csv"),row.names =1)
    adj2= read.csv(paste0("Result/",name,"/G1/",methods[j],"_",name,"_G1.csv"),row.names =1)
    pathh <- paste0("Result/", name, "/compare/",methods[j])
    Topo2(pathh, adj1, adj2, p)
    }}


