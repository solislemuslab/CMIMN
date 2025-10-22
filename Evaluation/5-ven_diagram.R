rm(list=ls())
library("VennDiagram")
source('vv2.R')
#################################################################################
###########################################################
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
    summary_ven = rbind(summary_ven,c(dim(adj1)[1],dim(adj1)[2],dim(adj2)[1],dim(adj2)[2],dim(adj3)[1],dim(adj3)[2],dim(adj4)[1],dim(adj4)[2],sum(adj1),sum(adj2),sum(adj3),sum(adj4)))
    pdf(paste0("Result/",name,"/",Group[k],"/venn",name,"_",Group[k],".pdf"))
    vv(adj1,adj2,adj3,adj4)
    dev.off()
  } 
}
write.csv(summary_ven,file=paste0("Result/summary/dim_edges_total.csv"))

#############################################################################Compare one method in 0 and 1 groups
list_names = c("phylum","class","order")
methods = c("sparcc","SE_glasso","SP","BN")
for (i in seq_along(list_names)){
  for (j in 1:length(methods)){
  name <- list_names[i]
  adj1= read.csv(paste0("Result/",name,"/G0/",methods[j],"_",name,"_G0.csv"),row.names =1)
  adj2= read.csv(paste0("Result/",name,"/G1/",methods[j],"_",name,"_G1.csv"),row.names =1)
  pdf(paste0("Result/",name,"/compare/Compare",methods[j],"_",name,".pdf"))
  vv2(adj1,adj2)
  dev.off()
}}
################################################################

#############################################################################
###################################compare Normalization method
# adj1= read.csv('BN_phylum_1.csv',row.names =1)##1064
# sum(adj1)
# dim(adj1)
# adj2= read.csv('BN_phylum_2.csv',row.names =1) ##360
# sum(adj2)
# dim(adj2)
# adj3= read.csv('BN_phylum_3.csv',row.names =1)##352
# sum(adj3)
# dim(adj2)
# adj4= read.csv('BN_phylum_4.csv',row.names =1)
# sum(adj4)
# dim(adj4)
# ###########################################################
# # Calculate the intersection sizes
# n123 <- sum(adj1 & adj2 & adj3)
# n124 <- sum(adj1 & adj2 & adj4)
# n234 <- sum(adj2 & adj3 & adj4)
# n134 <- sum(adj1 & adj3 & adj4)
# n1234 <- sum(adj1 & adj2 & adj3 & adj4)
# 
# pdf("Figure_BN/venn_diagram_ComparNormalization methods.pdf", width = 8, height = 6)
# library("VennDiagram")
# # Create the Venn diagram
# draw.quad.venn(
#   area1 = sum(adj1),
#   area2 = sum(adj2),
#   area3 = sum(adj3),
#   area4 = sum(adj4),
#   n12 = sum(adj1 & adj2),
#   n23 = sum(adj2 & adj3),
#   n13 = sum(adj1 & adj3),
#   n14 = sum(adj1 & adj4),
#   n24 = sum(adj2 & adj4),
#   n34 = sum(adj3 & adj4),
#   n123 = n123,
#   n124 = n124,
#   n234 = n234,
#   n134 = n134,
#   n1234 = n1234,
#   category = c("BN_1", "BN_2", "BN_3", "BN_4"),
#   col = "Blue",
#   fill = c("Red", "Pink", "Blue", "Orange"),
#   lty = "solid"
# )
# # Close the PDF device
# dev.off()
###############################################
################################################
# m = as.vector(result$Gval_order2)
# 
# q = quantile(m, probs = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9))
# length(result$Gval_order2[result$Gval_order2<q[5]])



