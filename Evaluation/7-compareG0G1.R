# Set the working directory
setwd('/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/Result/phylum/compare/')

folders <- c("phylum", "class","order")
for (folder in folders) {
  path = paste0("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/Result/",folder,"/","compare/")
# List of file names
file_names <- c("BNLL.csv", "SE_glassoLL.csv", "sparccLL.csv", "SPLL.csv")

# Initialize a list to store data frames
data_frames <- list()

# Read CSV files and store in data_frames list
for (file_name in file_names) {
  data_frames[[file_name]] <- read.csv(paste0(path,file_name))
}

# Specify the column names you want to find the intersection for
column_names <- c("degree", "betweenness", "closeness", "evcent", "page_rank")

# Initialize a data frame to store the intersection results
result_df_id <- data.frame(Column = character(0), Intersection = integer(0), stringsAsFactors = FALSE)
result_df_name <- data.frame(Column = character(0), Intersection = integer(0), stringsAsFactors = FALSE)
# Extract the specified columns from each data frame
for (column_name in column_names) {
  columns <- lapply(data_frames, function(df) df[[column_name]])
  intersection <- Reduce(intersect, columns)
  
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
  ids <-  as.numeric( intersection)
  names <- name_otu[ids]
  # Add the intersection result to the result data frame
  result_df_id <- rbind(result_df_id, data.frame(Column = column_name, Intersection = paste( intersection, collapse = ", ")))
  result_df_name <- rbind(result_df_name, data.frame(Column = column_name, Intersection = paste(names, collapse = ", ")))
}

write.csv(result_df_id, paste0(path,"intersection_results_id.csv"), row.names = FALSE)
write.csv(result_df_name, paste0(path,"intersection_results_name.csv"), row.names = FALSE)
}