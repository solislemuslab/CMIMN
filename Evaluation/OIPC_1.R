cmi<-function(v1,v2,vcs){ 
  if (missing(vcs)){
    c1<-var(v1)
    c2<-var(v2)
    c3=det(cov(cbind(v1,v2)))
    cmiv=0.5*log(c1*c2/c3)
  }else{
    c1=det(cov(cbind(v1,vcs)))
    c2=det(cov(cbind(v2,vcs)))
    c3=det(cov(cbind(vcs)))
    c4=det(cov(cbind(v1,v2,vcs)))
    cmiv=0.5*log((c1*c2)/(c3*c4));       
  }
  if (!is.finite(cmiv)) {
    cmiv <- 0
  }
  return(cmiv)
}

edgereduce<-function(G,Gval,data,q1,q2){
  order = 0
  for (i in 1:(nrow(G)-1)){
    for (j in (i+1):nrow(G)){
      if (G[i,j]!=0){
        cmiv=cmi(data[i,],data[j,]);
        Gval[i,j]=cmiv;  Gval[j,i]=cmiv;
      }
    }
  }
  quantile_order0 <- quantile(Gval, probs = q1)
  for (i in 1:(nrow(G) - 1)) {
    for (j in (i + 1):nrow(G)) {
      if (Gval[i, j] < quantile_order0) {
        G[i, j] <- 0
        G[j, i] <- 0
      }
    }
  }
  G_order0=G
  Gval_order0=Gval
  quantile_order0=quantile_order0
  order =1
  for (i in 1:(nrow(G)-1)){
    for (j in (i+1):nrow(G)){
      if (G[i,j]!=0){
        adj<-NULL
        for (k in 1:nrow(G)){
          if (G[i,k]!=0 & G[j,k]!=0){
            adj<-cbind(adj,k)
          }
        }
        if (length(adj)>=order){
          combntnslist<-t(combn(adj,order))
          combntnsrow<-nrow(combntnslist)  
          cmiv=0;
          v1=data[i,];v2=data[j,]
          for (k in 1:combntnsrow){
            vcs=data[combntnslist[k,],]  
            a=cmi(v1,v2,vcs) ;
            cmiv=max(cmiv,a);
          }
          Gval[i,j]=cmiv; Gval[j,i]=cmiv
        } #ifncol
      }#ifG
      
    }
  }
  
  quantile_order1 <- quantile(Gval, probs = q2)
  for (i in 1:(nrow(G) - 1)) {
    for (j in (i + 1):nrow(G)) {
      if (Gval[i, j] < quantile_order1) {
        G[i, j] <- 0
        G[j, i] <- 0
      }
    }
  }
  G_order1=G
  Gval_order1=Gval
  quantile_order1=quantile_order1
  
  out<-list(G_order0=G_order0,G_order1=G_order1, Gval_order0=Gval_order0,Gval_order1=Gval_order1,quantile_order0,quantile_order1,sum(G_order0),sum(G_order1))
  return(out)
}
