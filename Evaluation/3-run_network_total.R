rm(list=ls())
library(SpiecEasi)
library(SPRING) 
library(Matrix)
source('fun_BN.R')
source('OIPC_1.R')
library(CMIMN)
############################################Total
path_data ="/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/data/"
list_names = c("phylum","class","order")
for (i in seq_along(list_names)) {
  name <- list_names[i]
  data = read.csv(paste0(path_data, name,"/da_",name,"_count_scabpit.csv"))
  data_otu = data[-ncol(data)]
  ##sparcc
  sparcc.ot <- sparcc(data_otu)
  sparcc.graph <- abs(sparcc.ot$Cor) >= 0.4
  diag(sparcc.graph) <- 0
  ##SE
  se_glasso <- SE_original(as.matrix(data_otu), 'glasso')
  SE_glasso <- se_glasso[[2]]
  ##SPRING
  sp_mb <- SP_original(data_otu)   #SPring had just mb
  SP <- sp_mb[[2]]
  ##BN
  data_BN = log(data_otu+1)
  n_gene<-nrow(t(data_BN))
  G<-matrix(1,n_gene,n_gene)
  order=1
  t=0
  diag(G)<-0
  Gval=G
  nn=dim(G)[1]
  q1=0.8
  q2=0.9
  data = t(data_BN)
  result = edgereduce(G,Gval,data,q1,q2)
  BN = result$G_order1
  
  Xt <- t(data_otu) 
  clr_data <- clr(Xt + 1e-6)
  res <- conditional_MI(clr_data, q1 = 0.8, q2 = 0.9, quantitative = FALSE)
  BN2 = res$G_order1
  
  write.csv(sparcc.graph, file = paste0("Result/",name,"/sparcc_",name,"_total.csv"), row.names = TRUE)
  write.csv(SE_glasso, file = paste0("Result/",name,"/SE_glasso_",name,"_total.csv"), row.names = TRUE)
  write.csv(SP, file = paste0("Result/",name,"/SP_",name,"_total.csv"), row.names = TRUE)
  write.csv(BN, file = paste0("Result/",name,"/BN_",name,"_total.csv"), row.names = TRUE)
  write.csv(BN2, file = paste0("Result/",name,"/BN2_",name,"_total.csv"), row.names = TRUE)
}

############G1
path_data ="/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/data/"
for (i in seq_along(list_names)) {
  name <- list_names[i]
  data = read.csv(paste0(path_data, name,"/da_",name,"_count_scabpit.csv"))
  subset_data_1 <- data[data[, ncol(data)] == 1, ]
  dim(subset_data_1)
  data_G1 = subset_data_1[,-ncol(subset_data_1)]
  data_G1<- as.matrix(data_G1)
  
  
  data_otu = data_G1
  ##sparcc
  sparcc.ot <- sparcc(data_otu)
  sparcc.graph <- abs(sparcc.ot$Cor) >= 0.4
  diag(sparcc.graph) <- 0
  ##SE
  se_glasso <- SE_original(as.matrix(data_otu), 'glasso')
  SE_glasso <- se_glasso[[2]]
  ##SPRING
  sp_mb <- SP_original(data_otu)   #SPring had just mb
  SP <- sp_mb[[2]]
  ##BN
  
  data_BN = log(data_otu+1)
  n_gene<-nrow(t(data_BN))
  G<-matrix(1,n_gene,n_gene)
  order=1
  t=0
  diag(G)<-0
  Gval=G
  nn=dim(G)[1]
  q1=0.8
  q2=0.9
  data = t(data_BN)
  result = edgereduce(G,Gval,data,q1,q2)
  BN = result$G_order1
  write.csv(sparcc.graph, file = paste0("Result/",name,"/sparcc_",name,"_G1.csv"), row.names = TRUE)
  write.csv(SE_glasso, file = paste0("Result/",name,"/SE_glasso_",name,"_G1.csv"), row.names = TRUE)
  write.csv(SP, file = paste0("Result/",name,"/SP_",name,"_G1.csv"), row.names = TRUE)
  write.csv(BN, file = paste0("Result/",name,"/BN_",name,"_G1.csv"), row.names = TRUE)
}
############G0
path_data ="/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/data/"
for (i in seq_along(list_names)) {
  name <- list_names[i]
  data = read.csv(paste0(path_data, name,"/da_",name,"_count_scabpit.csv"))
  subset_data_1 <- data[data[, ncol(data)] == 0, ]
  dim(subset_data_1)
  data_G0 = subset_data_1[,-ncol(subset_data_1)]
  data_G0<- as.matrix(data_G0)
  
  
  data_otu = data_G0
  ##sparcc
  sparcc.ot <- sparcc(data_otu)
  sparcc.graph <- abs(sparcc.ot$Cor) >= 0.4
  diag(sparcc.graph) <- 0
  ########################################################SE
  se_glasso <- SE_original(as.matrix(data_otu), 'glasso')
  SE_glasso <- se_glasso[[2]]
  ########################################################SPRING
  sp_mb <- SP_original(data_otu)   #SPring had just mb
  SP <- sp_mb[[2]]
  ########################################################BN
  
  data_BN = log(data_otu+1)
  n_gene<-nrow(t(data_BN))
  G<-matrix(1,n_gene,n_gene)
  order=1
  t=0
  diag(G)<-0
  Gval=G
  nn=dim(G)[1]
  q1=0.8
  q2=0.9
  data = t(data_BN)
  result = edgereduce(G,Gval,data,q1,q2)
  BN = result$G_order1
  write.csv(sparcc.graph, file = paste0("Result/",name,"/sparcc_",name,"_G0.csv"), row.names = TRUE)
  write.csv(SE_glasso, file = paste0("Result/",name,"/SE_glasso_",name,"_G0.csv"), row.names = TRUE)
  write.csv(SP, file = paste0("Result/",name,"/SP_",name,"_G0.csv"), row.names = TRUE)
  write.csv(BN, file = paste0("Result/",name,"/BN_",name,"_G0.csv"), row.names = TRUE)
}