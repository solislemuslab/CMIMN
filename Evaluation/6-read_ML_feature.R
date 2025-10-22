rm(list=ls())
library(readxl)
library(purrr)
folders <- c("phylum", "class","order")
for (folder in folders) {
  # Set the path to the folder containing the Excel files
  path_result <- paste0("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/ML/",folder,"/")
  path = paste0("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/Result/",folder,"/ML_FS/") 
  # Get the file names matching the pattern
  file_names <- list.files(path_result, pattern = "feature_selection")
  # Read and filter the files
  
  
  # Function to read and filter a single file
  read_and_filter_file <- function(file) {
    df <- read_excel(file)
    #filtered_df <- df[df$Count > 3, c(1, ncol(df))]
    filtered_df <- df[df$Count > 3, c(1, ncol(df))]
    return(filtered_df)
  }
  
  filtered_files <- map(file_names, ~ read_and_filter_file(paste0(path_result, .x)))
  
  #Create a list of variables with high scores
  otu_list <- lapply(filtered_files, function(df) as.character(df[[1]]))
  names(otu_list)[1:4] <- c("clr","count","log","rowSum")
  #otu_list_numeric <- lapply(otu_list, as.numeric)
  
  ML_SF_scabpit = crossprod(table(stack(otu_list)))
  write.csv(ML_SF_scabpit, file = paste0(path,"ML_SF_scabpit.csv"), row.names = TRUE)
  
  # Find the overlap between combinations of otu_list vectors
  overlap <- combn(otu_list, 2, function(x) {
    intersect(x[[1]], x[[2]])
  }, simplify = FALSE)
  
  # Print the overlap results
  for (i in seq_along(overlap)) {
    vec_names <- names(otu_list)[combn(length(otu_list), 2)[, i]]
    print(paste("Overlap between", vec_names[1], "and", vec_names[2], ":"))
    print(overlap[[i]])
  }
  write.csv(otu_list$clr, file = paste0(path,"1.csv"), row.names = TRUE)
  write.csv(otu_list$count, file = paste0(path,"2.csv"), row.names = TRUE)
  write.csv(otu_list$log, file = paste0(path,"3.csv"), row.names = TRUE)
  write.csv(otu_list$rowSum, file = paste0(path,"4.csv"), row.names = TRUE)
  c12 = intersect(otu_list$clr,otu_list$count)
  c34 = intersect(otu_list$log,otu_list$rowSum)
  set_5 = intersect(c12,c34)
  set_6 = union(c12,c34)
  write.csv(set_5, file = paste0(path,"5.csv"), row.names = TRUE)
  write.csv(set_6, file = paste0(path,"6.csv"), row.names = TRUE)
}


# Define the list of folders
folders <- c("phylum", "class","order")
# Define the list of methods
methods <- c("BN", "sparcc", "SE_glasso", "SP")
# Iterate over the folders
Per = c()
for (folder in folders) {
  # Define the path to the directory
  path <- paste0("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/Result/", folder, "/compare/")
  # Iterate over the methods
  for (method in methods) {
    for (kk in 1:6){
    # Read the LL.csv file
    ll <- read.csv(paste0(path, method, "LL.csv"), row.names = 1)
    uTotal = read.csv(file = paste0("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/Result/",folder,"/ML_FS/",kk,".csv"), row.names = 1)
    uTotal = unlist(uTotal[,1])
    # Create the intersection matrix
    intersection_matrix <- matrix(0, nrow = length(uTotal), ncol = ncol(ll), dimnames = list(uTotal, colnames(ll)))
    
    for (i in 1:ncol(ll)) {
      intersection_matrix[, i] <- as.numeric(uTotal %in% as.character(ll[, i]))
    }
    numb = dim(intersection_matrix)[1]
    intersection_matrix = rbind(colSums(intersection_matrix)/numb,intersection_matrix)
    # Write the intersection matrix to a CSV file
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
    names <- c("OTU_name" , names)
    
    
    
    per = c(folder,method,kk,colSums(intersection_matrix)/numb)
    Per = rbind(Per,per)
    ID = cbind(names,intersection_matrix)
    write.csv(intersection_matrix, file = paste0("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/Result/",folder,"/ML_FS/", "intersectML_Net",method,kk,".csv"), row.names = TRUE)
    write.csv(ID, file = paste0("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/Result/",folder,"/ML_FS/", "IDML_Net",method,kk,".csv"), row.names = TRUE)
    
    
    
    # # Save the heatmap as a figure (e.g., PNG format)
    # png(paste0("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/Result/",folder,"/ML_FS/", "intersectML_Net",method,kk,".png"))
    # 
    # # Add any additional plotting options, if needed
    # # (e.g., title, labels, etc.)
    # heatmap(intersection_matrix, col = heat.colors(256))
    # # Close the plot device to save the figure
    # dev.off()
    
     }
  }
}
write.csv(Per, file = paste0("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/Result/Per.csv"), row.names = TRUE)

