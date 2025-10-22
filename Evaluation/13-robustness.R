rm(list=ls())
library(SpiecEasi)
library(SPRING) 
library(Matrix)
source('fun_BN.R')
path_data ="/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/data/"
list_names = c("phylum","class","order")
method = c("sparcc","SE_glasso","SP","BN")
n_R= 50

####Simulate data
# select_70_percent <- function(data) {
#   num_rows <- nrow(data)
#   num_rows_to_select <- round(0.7 * num_rows)
#   selected_rows <- sample(1:num_rows, num_rows_to_select, replace = FALSE)
#   return(data[selected_rows, ])
# }
# for (i in seq_along(list_names)) {
#   name <- list_names[i]
#   data = read.csv(paste0(path_data, name,"/da_",name,"_count_scabpit.csv"))
#   data_otu = data[-ncol(data)]
# for (ran in 1:n_R){
#   selected_data <- select_70_percent(data_otu)
#   write.csv(selected_data,paste0("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/Result/",name,"/simulate/",ran,".csv"))
# }}

pnet <- function(selected_data, m) {
  # Check if m is one of the expected values
  if (m %in% c("sparcc", "SE_glasso", "SP", "BN")) {
    if (m == "sparcc") {
      sparcc.ot <- sparcc(selected_data)
      sparcc.graph <- abs(sparcc.ot$Cor) >= 0.4
      diag(sparcc.graph) <- 0
      predicted_network <- sparcc.graph
    } else if (m == "SE_glasso") {
      library(SpiecEasi)
      se_glasso <- SE_original(as.matrix(selected_data), 'glasso')
      predicted_network <- se_glasso[[2]]
    } else if (m == "SP") {
      sp_mb <- SP_original(selected_data)
      predicted_network <- sp_mb[[2]]
    } else if (m == "BN") {
      data_BN <- log(selected_data + 1)
      q1 <- 0.8
      q2 <- 0.9
      data <- t(data_BN)
      result <- edgereduce(data, q1, q2)
      predicted_network <- result$G_order1
    }
    
    # Return the predicted network
    return(predicted_network)
  } else {
    stop("Invalid value for 'm'. Use one of: sparcc, SE_glasso, SP, BN")
  }
}

###########################################70percent

FI=c()
for (i in seq_along(list_names)) {
  name <- list_names[i]
  print(name)
  data = read.csv(paste0(path_data, name,"/da_",name,"_count_scabpit.csv"))
  data_otu = data[-ncol(data)]
      for (j in 1:4){
        FS =c()
        for (ran in 1:n_R){
          m = method[j]
          True = read.csv(paste0("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/Result/",name,"/total/",m,"_",name,"_total.csv"), row.names = 1)
          True = as.matrix(True,dim(True)[1],dim(True)[1],dim(True)[2])
          selected_data = read.csv(paste0("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/Result/",name,"/simulate/",ran,".csv"), row.names = 1)
          predicted_network<- pnet(selected_data,m)
          true_graph <- graph.adjacency(True, mode = "undirected", diag = FALSE)
          predicted_graph <- graph.adjacency(predicted_network, mode = "undirected", diag = FALSE)

          # Calculate precision, recall, and F-score
          precision <- sum(E(true_graph) %in% E(predicted_graph)) / length(E(predicted_graph))
          recall <- sum(E(true_graph) %in% E(predicted_graph)) / length(E(true_graph))
          f_score <- 2 * (precision * recall) / (precision + recall)
          FS =rbind(FS,f_score)
  }
  FI = cbind(FI, c(name,m,FS) )
      }}
write.csv(FI,paste0("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/Result/FScore.csv"))
