##########################################################################################
#install.packages("compositions")
library(compositions)

# Specify the group names
groups <- c("phylum", "class","order")

# Specify the corresponding file paths
file_paths <- list(
  phylum = "/Users/rosa/Desktop/ALLWork/Madison/Project/Soil-nn/Code/python code local/Git_RF/OTU/Count_data/Phylum/CountOTUY1_F_Phylum.csv",
  class = "/Users/rosa/Desktop/ALLWork/Madison/Project/Soil-nn/Code/python code local/Git_RF/OTU/Count_data/Class/CountOTUY1_F_Class.csv",
  order = "/Users/rosa/Desktop/ALLWork/Madison/Project/Soil-nn/Code/python code local/Git_RF/OTU/Count_data/Order/CountOTUY1_F_Order.csv"
  )

# Specify the merged data file path
scabpit_path <- "/Users/rosa/Desktop/ALLWork/Madison/Project/Soil-nn/Code/python code local/Git_RF/response/response_original/Scabpit/response_No_tuber_scabpit.csv"

# Load the merged data
scabpit <- read.csv(scabpit_path)

# Loop through each group
for (group in groups) {
  print(group)
  # Load the OTU data for the current group
  data_OTU <- read.csv(file_paths[[group]])

  
  # Merge the data
  merged_data <- merge(data_OTU, scabpit, by.x = "X", by.y = "Column1")
  
  # Extract the necessary columns
  OTU_original <- merged_data[-ncol(merged_data)]
  resp <- merged_data[ncol(merged_data)]
  
  # Calculate the threshold based on the total number of samples
  th <- round(nrow(OTU_original) * 85 / 100)
  
  
  
  # Remove columns with less than the threshold number of zero values
  filtered_data_n <- OTU_original[, colSums(OTU_original == 0, na.rm = TRUE) < th]
  filtered_data = filtered_data_n[,-1]
  # Convert to matrix
  filtered_data <- as.matrix(filtered_data)
  
  # Create output data frame
  da_data <- cbind(filtered_data, resp)
  colnames(da_data)[ncol(da_data)] <- "response"
  
  # Write the data to a CSV file
  output_file <- paste0("data/", group, "/da_", group, "_count_scabpit.csv")
  write.csv(da_data, file = output_file, row.names = FALSE)
  
  # Perform additional operations (log, rowS, clr)
  filtered_data_log <- log(filtered_data + 1)
  da_data_log <- cbind(filtered_data_log, resp)
  colnames(da_data_log)[ncol(da_data_log)] <- "response"
  output_file_log <- paste0("data/", group, "/da_", group, "_log_scabpit.csv")
  write.csv(da_data_log, file = output_file_log, row.names = FALSE)
  
  filtered_data_rowS <- filtered_data / rowSums(filtered_data)
  da_data_rowS <- cbind(filtered_data_rowS, resp)
  colnames(da_data_rowS)[ncol(da_data_rowS)] <- "response"
  output_file_rowS <- paste0("data/", group, "/da_", group, "_rowS_scabpit.csv")
  write.csv(da_data_rowS, file = output_file_rowS, row.names = FALSE)
  
  da_data_clr <- clr(filtered_data)
  da_data_clr <- cbind(da_data_clr, resp)
  colnames(da_data_clr)[ncol(da_data_clr)] <- "response"
  output_file_clr <- paste0("data/", group, "/da_", group, "_clr_scabpit.csv")
  write.csv(da_data_clr, file = output_file_clr, row.names = FALSE)
}

