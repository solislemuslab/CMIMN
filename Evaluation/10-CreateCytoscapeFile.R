###Create csv file for cytoscape

##For D1
rm(list=ls())
library(readxl)
library(purrr)
folders <- c("phylum", "class","order")
net1=c("G0", "G1")
net2 = c("df1", "df2")

for (N in 1:2){
  for (folder in folders) {
      Net = read.csv(paste0("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/Result/cyto/",folder,"/","combinations",net1[N],".csv"),row.names =1)
      Color_node=read.csv(paste0("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/Result/cyto/",folder,"/","unique_namestotal.csv"),row.names =1)
      Color_node1=read.csv(paste0("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/Result/cyto/",folder,"/",net2[N],"selected_OTU.csv"),row.names =1)
      cyto_matrix <- cbind(
        Net,
        Color_node_intersect = as.integer(Net[, 1] %in% Color_node$x),
        Color_node_centrality= as.integer(Net[, 1] %in% Color_node1$name_OTU),
        Color_node_intersect2 = as.integer(Net[, 2] %in% Color_node$x),
        Color_node_centrality2= as.integer(Net[, 2] %in% Color_node1$name_OTU))
        #Color_node_intersect2 = c(setdiff(Color_node$x,unique(Net[, 1])), rep(NA, (nrow(Net)-length(setdiff(Color_node$x,unique(Net[, 1])))))),
        #Color_node_centrality2 = c(setdiff(Color_node1$name_OTU,unique(Net[, 1])), rep(NA, (nrow(Net)-length(setdiff(Color_node1$name_OTU,unique(Net[, 1])))))))
      
      cyto_matrix[is.na(cyto_matrix)] <- ""
      
      write.csv(cyto_matrix, file = paste0("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/Result/cyto/",folder,"/",net1[N],"cyto_matrix.csv"), row.names = TRUE)
  }  
  }


