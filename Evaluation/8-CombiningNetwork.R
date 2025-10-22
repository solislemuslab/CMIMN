list_names = c("phylum","class","order")
Group = c("total","G0","G1")
summary_ven = c()
for (k in 1:length(Group)){
  for (i in seq_along(list_names)) {
    name <- list_names[i]
    adj1= read.csv(paste0("Result/",name,"/",Group[k],"/sparcc_",name,"_",Group[k],".csv"),row.names =1)
    adj2= read.csv(paste0("Result/",name,"/",Group[k],"/SE_glasso_",name,"_",Group[k],".csv"),row.names =1)
    adj3= read.csv(paste0("Result/",name,"/",Group[k],"/SP_",name,"_",Group[k],".csv"),row.names =1)
    adj4= read.csv(paste0("Result/",name,"/",Group[k],"/BN_",name,"_",Group[k],".csv"),row.names =1)
    ADJ = adj1+adj2+adj3+adj4
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
    
    rownames(ADJ) =colnames(ADJ) = name_otu
    upper_triangle <- upper.tri(ADJ, diag = FALSE)
    combinations <- data.frame(
      Row = rownames(ADJ)[row(ADJ)[upper_triangle]],
      Column = colnames(ADJ)[col(ADJ)[upper_triangle]],
      Value = ADJ[upper_triangle]
    )
    combinations <- combinations[order(-combinations$Value), ]
    
    # Create a subset where the "Value" column is higher than 6
    subset_combinations<- combinations[combinations$Value > 2, ]
    # Extract unique names from columns 1 and 2
    unique_names_col1 <- unique(subset_combinations$Row)
    unique_names_col2 <- unique(subset_combinations$Column)
    
    # Combine the unique names from both columns into a single vector
    unique_names <- unique(c(unique_names_col1, unique_names_col2))
    
    write.csv(ADJ, file = paste0("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/Result/cyto/",name,"/","ADJ",Group[k],".csv"), row.names = TRUE)
    write.csv(subset_combinations, file = paste0("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/Result/cyto/",name,"/","combinations",Group[k],".csv"), row.names = TRUE)
    write.csv(unique_names, file = paste0("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/Result/cyto/",name,"/","unique_names",Group[k],".csv"), row.names = TRUE)
    }
}





for (i in seq_along(list_names)) {
    name <- list_names[i]
    u_name1= read.csv(paste0("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/Result/",name,"/","unique_names","G0.csv"),row.names =1)
    u_name2=  read.csv(paste0("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/Result/",name,"/","unique_names","G1.csv"),row.names =1)
    u_nameTotal = intersect(u_name1$x,u_name2$x)
    u_nameTotal = cbind(u_nameTotal,rep(1,length(u_nameTotal)))
    colnames(u_nameTotal)=c("OTU","color_code")
    write.csv(u_nameTotal, file = paste0("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/Result/cyto/",name,"/","unique_namestotal.csv"), row.names = TRUE)
  }



####################################################################################################


list_names = c("phylum","class","order")
Group = c("G0","G1")
summary_ven = c()
for (i in seq_along(list_names)) {
    name <- list_names[i]
    adj1= read.csv(paste0("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/Result/",name,"/","ADJ","G0.csv"),row.names =1)
    adj2= read.csv(paste0("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/Result/",name,"/","ADJ","G1.csv"),row.names =1)

    ADJ2Method = adj1+adj2
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
    
    rownames(ADJ2Method) =colnames(ADJ2Method) = name_otu
    upper_triangle <- upper.tri(ADJ2Method, diag = FALSE)
    combinations2Methods <- data.frame(
      Row = rownames(ADJ2Method)[row(ADJ2Method)[upper_triangle]],
      Column = colnames(ADJ2Method)[col(ADJ2Method)[upper_triangle]],
      Value = ADJ2Method[upper_triangle]
    )
    combinations2Methods <- combinations2Methods[order(-combinations2Methods$Value), ]
    
    # Create a subset where the "Value" column is higher than 6
    subset_df <- combinations2Methods[combinations2Methods$Value > 5, ]
    
    write.csv(ADJ2Method, file = paste0("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/Result/cyto/",name,"/","ADJ2Methods.csv"), row.names = TRUE)
    write.csv(subset_df, file = paste0("/Users/rosa/Desktop/ALLWork/Madison/Project/BayesianNetwork/R_BN_code/Result/cyto/",name,"/","combinations2Methods.csv"), row.names = TRUE)
    }








