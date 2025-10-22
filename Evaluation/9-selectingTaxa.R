# rm(list=ls())
# library(readxl)
# library(purrr)
# p <- 20
# max_length <- 60
# 
# ##################################################high Degree-Bet-Clossness
# folders <- c("phylum", "class","order")
# methods = c("sparcc","SE_glasso","SP","BN")
# net = c("df1", "df2")
# for (N in 1:2){
# K=c()
# for (folder in folders) {
#   for (j in 1:length(methods)){
#    DF2 = read.csv(paste0("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/Result/",folder,"/","compare/",methods[j],net[N],".csv"),row.names =1)
#    dif=cbind(1:dim(DF2)[1],DF2) 
#    D_degree = dif[order(dif$degree, decreasing = TRUE),][,1:2][1:round((dim(DF2)[1])*p/100),]
#    D_betweenness = dif[order(dif$betweenness, decreasing = TRUE),][,c(1,3)][1:round((dim(DF2)[1])*p/100),]
#    D_closeness = dif[order(dif$closeness, decreasing = TRUE),][,c(1,4)][1:round((dim(DF2)[1])*p/100),]
#    Selecte_taxa = Reduce(intersect, list(D_degree[, 1], D_betweenness[, 1], D_closeness[, 1]))
#    
#    if (folder=="phylum"){
#      data_phylum = read.csv("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/data/phylum/da_phylum_clr_scabpit.csv")
#      name_otu = colnames(data_phylum)[-ncol(data_phylum)]
#    }else if (folder=="class"){
#      data_class = read.csv("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/data/class/da_class_clr_scabpit.csv")
#      name_otu = colnames(data_class)[-ncol(data_class)] 
#    }else{
#      data_order = read.csv("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/data/order/da_order_clr_scabpit.csv")
#      name_otu = colnames(data_order)[-ncol(data_order)]
#    }
#    ids <-  as.numeric(Selecte_taxa)
#    names <- name_otu[ids]
#    
#    
#    
#    Selecte_taxa <- c(names , rep(NA, max_length - length(Selecte_taxa)))
#    K = rbind(K , c(folder,methods[j], Selecte_taxa) )
#   }
# }
# K[is.na(K)] <- ""
# write.csv(K, file = paste0("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/Result/",net[N],"highD-B-C.csv"), row.names = TRUE)
# }
# 
# ###############################################
# rm(list=ls())
# library(readxl)
# library(purrr)
# p <- 20
# max_length <- 60
# ##################################################high Degree-Bet-Page rank
# folders <- c("phylum", "class","order")
# methods = c("sparcc","SE_glasso","SP","BN")
# net = c("df1", "df2")
# for (N in 1:2){
#   K=c()
#   for (folder in folders) {
#     for (j in 1:length(methods)){
#       DF2 = read.csv(paste0("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/Result/",folder,"/","compare/",methods[j],net[N],".csv"),row.names =1)
#       dif=cbind(1:dim(DF2)[1],DF2) 
#       D_degree = dif[order(dif$degree, decreasing = TRUE),][,1:2][1:round((dim(DF2)[1])*p/100),]
#       D_betweenness = dif[order(dif$betweenness, decreasing = TRUE),][,c(1,3)][1:round((dim(DF2)[1])*p/100),]
#       D_PageRank = dif[order(dif$page_rank, decreasing = TRUE),][,c(1,4)][1:round((dim(DF2)[1])*p/100),]
#       Selecte_taxa = Reduce(intersect, list(D_degree[, 1], D_betweenness[, 1], D_PageRank[, 1]))
#       
#       if (folder=="phylum"){
#         data_phylum = read.csv("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/data/phylum/da_phylum_clr_scabpit.csv")
#         name_otu = colnames(data_phylum)[-ncol(data_phylum)]
#       }else if (folder=="class"){
#         data_class = read.csv("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/data/class/da_class_clr_scabpit.csv")
#         name_otu = colnames(data_class)[-ncol(data_class)] 
#       }else{
#         data_order = read.csv("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/data/order/da_order_clr_scabpit.csv")
#         name_otu = colnames(data_order)[-ncol(data_order)]
#       }
#       ids <-  as.numeric(Selecte_taxa)
#       names <- name_otu[ids]
#       
#       
#       
#       Selecte_taxa <- c(names , rep(NA, max_length - length(Selecte_taxa)))
#       K = rbind(K , c(folder,methods[j], Selecte_taxa) )
#     }
#   }
#   K[is.na(K)] <- ""
#   write.csv(K, file = paste0("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/Result/",net[N],"highD-P-B.csv"), row.names = TRUE)
# }
# 
# ##################################################high Degree-Page rank low Closeness
# rm(list=ls())
# library(readxl)
# library(purrr)
# p <- 20
# max_length <- 60
# folders <- c("phylum", "class","order")
# methods = c("sparcc","SE_glasso","SP","BN")
# net = c("df1", "df2")
# for (N in 1:2){
#   K=c()
#   for (folder in folders) {
#     for (j in 1:length(methods)){
#       DF2 = read.csv(paste0("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/Result/",folder,"/","compare/",methods[j],net[N],".csv"),row.names =1)
#       dif=cbind(1:dim(DF2)[1],DF2) 
#       D_degree = dif[order(dif$degree, decreasing = TRUE),][,1:2][1:round((dim(DF2)[1])*p/100),]
#       D_page = dif[order(dif$page_rank, decreasing = TRUE),][,c(1,3)][1:round((dim(DF2)[1])*p/100),]
#       D_closeness = dif[order(dif$closeness, decreasing = FALSE),][,c(1,4)]
#       D_closeness <- na.omit(D_closeness)
#       D_closeness = D_closeness[1:round((dim(DF2)[1])*p/100),]
#       Selecte_taxa = Reduce(intersect, list(D_degree[, 1], D_page[, 1], D_closeness[, 1]))
#       
#       if (folder=="phylum"){
#         data_phylum = read.csv("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/data/phylum/da_phylum_clr_scabpit.csv")
#         name_otu = colnames(data_phylum)[-ncol(data_phylum)]
#       }else if (folder=="class"){
#         data_class = read.csv("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/data/class/da_class_clr_scabpit.csv")
#         name_otu = colnames(data_class)[-ncol(data_class)] 
#       }else{
#         data_order = read.csv("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/data/order/da_order_clr_scabpit.csv")
#         name_otu = colnames(data_order)[-ncol(data_order)]
#       }
#       ids <-  as.numeric(Selecte_taxa)
#       names <- name_otu[ids]
#       
#       
#       
#       Selecte_taxa <- c(names , rep(NA, max_length - length(Selecte_taxa)))
#       K = rbind(K , c(folder,methods[j], Selecte_taxa) )
#     }
#   }
#   K[is.na(K)] <- ""
#   write.csv(K, file = paste0("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/Result/",net[N],"highD-P-LowC.csv"), row.names = TRUE)
# }
# 
# ###########################################high D -C low B
# rm(list=ls())
# library(readxl)
# library(purrr)
# p <- 20
# max_length <- 60
# folders <- c("phylum", "class","order")
# methods = c("sparcc","SE_glasso","SP","BN")
# net = c("df1", "df2")
# for (N in 1:2){
#   K=c()
#   for (folder in folders) {
#     for (j in 1:length(methods)){
#       DF2 = read.csv(paste0("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/Result/",folder,"/","compare/",methods[j],net[N],".csv"),row.names =1)
#       dif=cbind(1:dim(DF2)[1],DF2) 
#       D_degree = dif[order(dif$degree, decreasing = TRUE),][,1:2][1:round((dim(DF2)[1])*p/100),]
#       D_betweenness = dif[order(dif$betweenness, decreasing = FALSE),][,c(1,3)]
#       D_betweenness = na.omit(D_betweenness)
#       D_betweenness = D_betweenness [1:round((dim(DF2)[1])*p/100),]
#       
#       D_closeness = dif[order(dif$closeness, decreasing = TRUE),][,c(1,4)][1:round((dim(DF2)[1])*p/100),]
#       Selecte_taxa = Reduce(intersect, list(D_degree[, 1], D_betweenness[, 1], D_closeness[, 1]))
#       
#       if (folder=="phylum"){
#         data_phylum = read.csv("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/data/phylum/da_phylum_clr_scabpit.csv")
#         name_otu = colnames(data_phylum)[-ncol(data_phylum)]
#       }else if (folder=="class"){
#         data_class = read.csv("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/data/class/da_class_clr_scabpit.csv")
#         name_otu = colnames(data_class)[-ncol(data_class)] 
#       }else{
#         data_order = read.csv("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/data/order/da_order_clr_scabpit.csv")
#         name_otu = colnames(data_order)[-ncol(data_order)]
#       }
#       ids <-  as.numeric(Selecte_taxa)
#       names <- name_otu[ids]
#       
#       
#       
#       Selecte_taxa <- c(names , rep(NA, max_length - length(Selecte_taxa)))
#       K = rbind(K , c(folder,methods[j], Selecte_taxa) )
#     }
#   }
#   K[is.na(K)] <- ""
#   write.csv(K, file = paste0("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/Result/",net[N],"highD-C-LowB.csv"), row.names = TRUE)
# }
# 
# 
# ###########################################high D-P-B-E
# rm(list=ls())
# library(readxl)
# library(purrr)
# p <- 20
# max_length <- 60
# folders <- c("phylum", "class","order")
# methods = c("sparcc","SE_glasso","SP","BN")
# net = c("df1", "df2")
# for (N in 1:2){
#   K=c()
#   for (folder in folders) {
#     for (j in 1:length(methods)){
#       DF2 = read.csv(paste0("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/Result/",folder,"/","compare/",methods[j],net[N],".csv"),row.names =1)
#       dif=cbind(1:dim(DF2)[1],DF2) 
#       D_degree = dif[order(dif$degree, decreasing = TRUE),][,1:2][1:round((dim(DF2)[1])*p/100),]
#       D_betweenness = dif[order(dif$betweenness, decreasing =TRUE),][,c(1,3)][1:round((dim(DF2)[1])*p/100),]
#       D_evcent = dif[order(dif$evcent, decreasing = TRUE),][,c(1,4)][1:round((dim(DF2)[1])*p/100),]
#       D_Page = dif[order(dif$page_rank, decreasing = TRUE),][,c(1,4)][1:round((dim(DF2)[1])*p/100),]
#      
#       Selecte_taxa = Reduce(intersect, list(D_degree[, 1], D_betweenness[, 1], D_evcent [, 1],D_Page[, 1]))
#       
#       if (folder=="phylum"){
#         data_phylum = read.csv("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/data/phylum/da_phylum_clr_scabpit.csv")
#         name_otu = colnames(data_phylum)[-ncol(data_phylum)]
#       }else if (folder=="class"){
#         data_class = read.csv("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/data/class/da_class_clr_scabpit.csv")
#         name_otu = colnames(data_class)[-ncol(data_class)] 
#       }else{
#         data_order = read.csv("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/data/order/da_order_clr_scabpit.csv")
#         name_otu = colnames(data_order)[-ncol(data_order)]
#       }
#       ids <-  as.numeric(Selecte_taxa)
#       names <- name_otu[ids]
#       
#       
#       
#       Selecte_taxa <- c(names , rep(NA, max_length - length(Selecte_taxa)))
#       K = rbind(K , c(folder,methods[j], Selecte_taxa) )
#     }
#   }
#   K[is.na(K)] <- ""
#   write.csv(K, file = paste0("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/Result/",net[N],"highD-B-P-E.csv"), row.names = TRUE)
# }
# 
# 
# 
# ###########################################high D-P-B-E-C
# rm(list=ls())
# library(readxl)
# library(purrr)
# p <- 20
# max_length <- 60
# folders <- c("phylum", "class","order")
# methods = c("sparcc","SE_glasso","SP","BN")
# net = c("df1", "df2")
# for (N in 1:2){
#   K=c()
#   for (folder in folders) {
#     for (j in 1:length(methods)){
#       DF2 = read.csv(paste0("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/Result/",folder,"/","compare/",methods[j],net[N],".csv"),row.names =1)
#       dif=cbind(1:dim(DF2)[1],DF2) 
#       D_degree = dif[order(dif$degree, decreasing = TRUE),][,1:2][1:round((dim(DF2)[1])*p/100),]
#       D_betweenness = dif[order(dif$betweenness, decreasing =TRUE),][,c(1,3)][1:round((dim(DF2)[1])*p/100),]
#       D_evcent = dif[order(dif$evcent, decreasing = TRUE),][,c(1,4)][1:round((dim(DF2)[1])*p/100),]
#       D_Page = dif[order(dif$page_rank, decreasing = TRUE),][,c(1,4)][1:round((dim(DF2)[1])*p/100),]
#       D_Closeness = dif[order(dif$closeness, decreasing = TRUE),][,c(1,4)][1:round((dim(DF2)[1])*p/100),]
#       Selecte_taxa = Reduce(intersect, list(D_degree[, 1], D_betweenness[, 1], D_evcent [, 1],D_Page[, 1],D_Closeness[,1]))
#       
#       if (folder=="phylum"){
#         data_phylum = read.csv("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/data/phylum/da_phylum_clr_scabpit.csv")
#         name_otu = colnames(data_phylum)[-ncol(data_phylum)]
#       }else if (folder=="class"){
#         data_class = read.csv("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/data/class/da_class_clr_scabpit.csv")
#         name_otu = colnames(data_class)[-ncol(data_class)] 
#       }else{
#         data_order = read.csv("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/data/order/da_order_clr_scabpit.csv")
#         name_otu = colnames(data_order)[-ncol(data_order)]
#       }
#       ids <-  as.numeric(Selecte_taxa)
#       names <- name_otu[ids]
#       
#       
#       
#       Selecte_taxa <- c(names , rep(NA, max_length - length(Selecte_taxa)))
#       K = rbind(K , c(folder,methods[j], Selecte_taxa) )
#     }
#   }
#   K[is.na(K)] <- ""
#   write.csv(K, file = paste0("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/Result/",net[N],"highD-B-P-E-C.csv"), row.names = TRUE)
# }

########################################################################Final Idea For all methods and all centrality measures
rm(list=ls())
library(readxl)
library(purrr)
p <- 20
max_length <- 60
folders <- c("phylum", "class","order")
methods = c("sparcc","SE_glasso","SP","BN")
net = c("df1", "df2")
for (N in 1:2){
  for (folder in folders) {
    D_Total = c()
    for (j in 1:length(methods)){
      DF2 = read.csv(paste0("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/Result/",folder,"/","compare/",methods[j],net[N],".csv"),row.names =1)
      DF2 <- apply(DF2, 2, function(x) ifelse(is.na(x), 0, x))
      DF2 <-data.frame(DF2)
      DFF2 = 0.05*(DF2$degree)+0.15*(DF2$evcent)+0.15*(DF2$page_rank)+0.2*(DF2$closeness)+0.5*(DF2$betweenness)
      if (j==2){
        DFF2 = 2*DFF2 
      }
      D_Total =  D_Total+DFF2
    }
    if (folder=="phylum"){
      data_phylum = read.csv("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/data/phylum/da_phylum_clr_scabpit.csv")
      name_otu = colnames(data_phylum)[-ncol(data_phylum)]
    }else if (folder=="class"){
      data_class = read.csv("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/data/class/da_class_clr_scabpit.csv")
      name_otu = colnames(data_class)[-ncol(data_class)] 
    }else{
      data_order = read.csv("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/data/order/da_order_clr_scabpit.csv")
      name_otu = colnames(data_order)[-ncol(data_order)]
    }
    
    DFF2 <- as.numeric(DFF2)
    D_Total <- data.frame(name_OTU = name_otu, Centrality = DFF2)
    D_Total <- D_Total[order(D_Total$Centrality, decreasing = TRUE), ]
    
    # Select the top 20% of rows
    D_Selected <- D_Total[1:round(nrow(D_Total) * (p/100)), ]
    D_Selected$colorcode_centrality = rep(1,nrow(D_Selected))
    
    write.csv(D_Selected, file = paste0("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/Result/cyto/",folder,"/",net[N],"selected_OTU.csv"), row.names = TRUE)
    
    }
}   


########################    Combine centrality measures but for different methods 

rm(list=ls())
library(readxl)
library(purrr)
p <- 20
max_length <- 60
folders <- c("phylum", "class","order")
methods = c("sparcc","SE_glasso","SP","BN")
net = c("df1", "df2")
for (N in 1:2){
  K=c()
  for (folder in folders) {
    print(folder)
    for (j in 1:length(methods)){
      DF2 = read.csv(paste0("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/Result/",folder,"/","compare/",methods[j],net[N],".csv"),row.names =1)
      DF2 <- apply(DF2, 2, function(x) ifelse(is.na(x), 0, x))
      DF2 <-data.frame(DF2)
      DFF2 = 0.05*(DF2$degree)+0.15*(DF2$evcent)+0.15*(DF2$page_rank)+0.2*(DF2$closeness)+0.5*(DF2$betweenness)
      if (j==2){
        DFF2 = 2*DFF2 
      }
    if (folder=="phylum"){
      data_phylum = read.csv("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/data/phylum/da_phylum_clr_scabpit.csv")
      name_otu = colnames(data_phylum)[-ncol(data_phylum)]
    }else if (folder=="class"){
      data_class = read.csv("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/data/class/da_class_clr_scabpit.csv")
      name_otu = colnames(data_class)[-ncol(data_class)] 
    }else{
      data_order = read.csv("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/data/order/da_order_clr_scabpit.csv")
      name_otu = colnames(data_order)[-ncol(data_order)]
    }
    
    DFF2 <- as.numeric(DFF2)
    D_Total <- data.frame(name_OTU = name_otu, Centrality = DFF2)
    D_Total <- D_Total[order(D_Total$Centrality, decreasing = TRUE), ]
    
    # Select the top 20% of rows
    D_Selected <- D_Total[1:round(nrow(D_Total) * (p/100)), ][,1]
    Selecte_taxa <- c(D_Selected , rep(NA, max_length - length(D_Selected)))
    K = rbind(K , c(folder,methods[j], Selecte_taxa) )
    }
    }
  K[is.na(K)] <- ""
  write.csv(K, file = paste0("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/Result/cyto/",net[N],"selected_OTU_DM.csv"), row.names = TRUE)
}
     