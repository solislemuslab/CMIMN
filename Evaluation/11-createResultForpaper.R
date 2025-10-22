#write.csv(otu_list$clr, file = paste0(path,"1.csv"), row.names = TRUE)
#write.csv(otu_list$count, file = paste0(path,"2.csv"), row.names = TRUE)
#write.csv(otu_list$log, file = paste0(path,"3.csv"), row.names = TRUE)
#write.csv(otu_list$rowSum, file = paste0(path,"4.csv"), row.names = TRUE)
# Define the list of folders
rm(list = ls())
folders <- c("phylum", "class","order")
# Define the list of methods
methods <- c("BN", "sparcc", "SE_glasso", "SP")
net = c("df1", "df2")
for (N in 1:2){
R=c()
ll<-read.csv(paste0("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/Result/cyto/",net[N],"selected_OTU_DM.csv"), row.names = 1)
for (folder in folders) {
  for (method in methods) {
    # Create the intersection matrix
    subset_ll <- ll[ll[, 1] == folder & ll[, 2] == method, ]
    subset_ll <- subset_ll[, -c(1, 2)]
    subset_ll[subset_ll == "NA"] <- NA
    subset_ll[subset_ll == ""] <- NA
    # Remove rows with all NA values
    subset_ll <- subset_ll[!apply(subset_ll, 1, function(x) all(is.na(x))), ]
    # Extract IDs (non-NA values) from the remaining data frame
    subset_ll<- subset_ll[!is.na(subset_ll)]
    intersection_matrix <- matrix(0, nrow = length(subset_ll), ncol = 4,dimnames = list(subset_ll))
    
    for (kk in 1:4){
      uTotal = read.csv(file = paste0("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/Result/",folder,"/ML_FS/",kk,".csv"), row.names = 1)
      uTotal = unlist(uTotal[,1])
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
      ids <-  as.numeric(uTotal)
      names <- name_otu[ids]
      intersection_matrix[, kk] <- as.numeric(subset_ll %in% names)
      }
    intersection_matrix = rbind(intersection_matrix, colMeans(intersection_matrix))
    intersection_matrix = cbind(intersection_matrix, rowSums(intersection_matrix))
    row.names(intersection_matrix )[nrow(intersection_matrix )] <- "mean"
    colnames(intersection_matrix ) = c("clr","original","log","TSS","Sum")
    folder_row <- rep(folder, 5)
    method_row<-rep(method,5)
    first_row<-rbind(folder_row,method_row)
    intersection_matrix  = rbind(first_row,intersection_matrix)
    R = rbind(R,intersection_matrix)
  }}
mean_rows <- R[row.names(R) == "mean",1:4]
mean_rows <- apply(mean_rows, 2, as.numeric)
mean_rows <- rbind (mean_rows,colMeans(mean_rows))
write.csv(R,file =paste0("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/Result/cyto/",net[N],"intersect.csv"),row.names=TRUE)
write.csv(mean_rows ,file =paste0("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/Result/cyto/",net[N],"mean_rows.csv"),row.names=TRUE)
}



#####################################################################################be on df1 and also df2 and also ML

rm(list = ls())
R=c()
folders <- c("phylum", "class","order")
# Define the list of methods
methods <- c("BN", "sparcc", "SE_glasso", "SP")
ll1<-read.csv(paste0("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/Result/cyto/","df1selected_OTU_DM.csv"), row.names = 1)
ll2<-read.csv(paste0("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/Result/cyto/","df2selected_OTU_DM.csv"), row.names = 1)
  
for (folder in folders) {
    for (method in methods) {
      # Create the intersection matrix
      subset_ll1 <- ll1[ll1[, 1] == folder & ll1[, 2] == method, ]
      subset_ll1 <- subset_ll1[, -c(1, 2)]
      subset_ll1[subset_ll1 == "NA"] <- NA
      subset_ll1[subset_ll1 == ""] <- NA
      subset_ll1 <- subset_ll1[!apply(subset_ll1, 1, function(x) all(is.na(x))), ]
      subset_ll1<- subset_ll1[!is.na(subset_ll1)]
      #l2
      subset_ll2 <- ll2[ll2[, 1] == folder & ll2[, 2] == method, ]
      subset_ll2 <- subset_ll2[, -c(1, 2)]
      subset_ll2[subset_ll2 == "NA"] <- NA
      subset_ll2[subset_ll2 == ""] <- NA
      subset_ll2 <- subset_ll2[!apply(subset_ll2, 1, function(x) all(is.na(x))), ]
      subset_ll2<- subset_ll2[!is.na(subset_ll2)]
      ####
      subset_ll = intersect(subset_ll1,subset_ll2)
      

      intersection_matrix <- matrix(0, nrow = length(subset_ll), ncol = 4,dimnames = list(subset_ll))
      
      for (kk in 1:4){
        uTotal = read.csv(file = paste0("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/Result/",folder,"/ML_FS/",kk,".csv"), row.names = 1)
        uTotal = unlist(uTotal[,1])
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
        ids <-  as.numeric(uTotal)
        names <- name_otu[ids]
        intersection_matrix[, kk] <- as.numeric(subset_ll %in% names)
      }
      intersection_matrix = rbind(intersection_matrix, colMeans(intersection_matrix))
      intersection_matrix = cbind(intersection_matrix, rowSums(intersection_matrix))
      row.names(intersection_matrix )[nrow(intersection_matrix )] <- "mean"
      colnames(intersection_matrix ) = c("clr","original","log","TSS","Sum")
      folder_row <- rep(folder, 5)
      method_row<-rep(method,5)
      first_row<-rbind(folder_row,method_row)
      intersection_matrix  = rbind(first_row,intersection_matrix)
      R = rbind(R,intersection_matrix)
    }
  }
write.csv(R,file =paste0("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/Result/cyto/intersect3STRATEGYYY.csv"),row.names=TRUE)
##########################################################################################

#Intersect different methods 
rm(list = ls())
max_length <- 20
folders <- c("phylum", "class","order")
# Define the list of methods
methods <- c("BN", "sparcc", "SE_glasso", "SP")
net = c("df1", "df2")
for (N in 1:2){
  Final=c()
  ll<-read.csv(paste0("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/Result/cyto/",net[N],"selected_OTU_DM.csv"), row.names = 1)
  for (folder in folders) {
    for (method in methods) {
      # Create the intersection matrix
      subset_ll <- ll[ll[, 1] == folder & ll[, 2] == method, ]
      subset_ll <- subset_ll[, -c(1, 2)]
      subset_ll[subset_ll == "NA"] <- NA
      subset_ll[subset_ll == ""] <- NA
      # Remove rows with all NA values
      subset_ll <- subset_ll[!apply(subset_ll, 1, function(x) all(is.na(x))), ]
      # Extract IDs (non-NA values) from the remaining data frame
      subset_ll<- subset_ll[!is.na(subset_ll)]
      if (method=="BN"){
        IN=subset_ll}
      IN = intersect(IN,subset_ll)
    }
    IN = c(IN , rep(NA, max_length - length(IN)))
      Final = rbind(Final,IN)
      
  }
  row.names(Final)=c("Phylum","Class","Order")
  Final[is.na(Final)] <- ""
   write.csv(Final,file =paste0("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/Result/cyto/",net[N],"intersectNEtworkbasedmethod.csv"),row.names=TRUE)
}

















