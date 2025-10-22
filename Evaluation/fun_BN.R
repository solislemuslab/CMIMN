##################################################################
#######    FUNC SE   return adj  network    ######  Method  'mb'   or   'glasso'
SE_original <- function(data, Method){
  output <- list()
  se <- spiec.easi(data, method=Method, lambda.min.ratio=1e-2, nlambda=15)
  se_adj <- se$refit$stars
  se_ADJ <- as.matrix(se_adj)
  output$se <- se
  output$se_ADJ <- se_ADJ
  return(output)
}
##################################################################
SP_original <- function(data){
  output <- list()
  start <- Sys.time()
  SP_graph <- SPRING(data, Rmethod = "original", quantitative = TRUE, ncores = 5,
                     lambdaseq = "data-specific", nlambda = 15, rep.num = 20)
  print(Sys.time() - start)
  opt.K <- SP_graph$output$stars$opt.index
  adj.K <- as.matrix(SP_graph$fit$est$path[[opt.K]])
  output$SP_graph <- SP_graph
  output$adj_SP <- adj.K
  return(output)
}
#############   FUNC: Synth data with  SE   ######
