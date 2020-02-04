# Order dependency
Order.sep <- function(X, f, h, G, q){
  MI=matrix(0,nrow=ncol(X), ncol=ncol(X))
  for (i in 1:(ncol(X)-1)){
    print(i)
    for (j in (i+1):ncol(X)){
      x1=X[,i]
      x2=X[,j]
      x=cbind(x1,x2)
      x <- apply(x, 2L, function(x) (x - min(x, na.rm = TRUE))/(max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
      x1=x[,1]
      x2=x[,2]
      M.M1=Grid(x1)
      M.M2=Grid(x2)
      H=h(x1,x2, M.M1, M.M2)
      F=f(x1,x2, M.M1, M.M2)
      MI[i,j]=GI(F,H,G)
    }
  }
  MI=MI+t(MI)
  
  MI.max=which(MI==max(MI),arr.ind=TRUE)
  mat.liste=c()
  MI.S=c()
  MI.liste=c(NA, max(MI))
  MI.liste2=c()
  for(k in 1:2){
    liste.int=seq(1,ncol(X), by=1)
    liste=c(MI.max[k,])
    liste.int=liste.int[-which(liste.int==liste[1])]
    liste.int=liste.int[-which(liste.int==liste[2])]
    MI.sum=max(MI)
    while(length(liste.int)>0 && length(liste)<q){
      i=liste[length(liste)]
      ii=which(MI[,i]==max(MI[liste.int,i]))	
      MI.sum=MI.sum+max(MI[liste.int,i])
      MI.liste=c(MI.liste, max(MI[liste.int,i]))
      liste<-c(liste,ii)
      liste.int=liste.int[-which(liste.int==ii)]
    }
    mat.liste=rbind(mat.liste,liste)
    MI.S=c(MI.S,MI.sum)
    MI.liste2=rbind(MI.liste2,MI.liste)
  }
  ord.opt=which(MI.S==max(MI.S))
  liste.opt=mat.liste[ord.opt[1],]
  MI.liste.opt=cbind(liste.opt,MI.liste2[ord.opt[1],])
  
  return(MI.liste.opt)
}