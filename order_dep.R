# Order dependency
Order <- function(X, f, h, G, q){
  MI=matrix(0,nrow=ncol(X), ncol=ncol(X))
  for (i in 1:(ncol(X)-1)){
  	for (j in (i+1):ncol(X)){
  		x1=X[,i]
  		x2=X[,j]
  		H=h(x1,x2)
  		F=f(x1,x2)
  		MI[i,j]=GI(F,H,G)
  	}
  }
  MI=MI+t(MI)
  MI.max=which(MI==max(MI),arr.ind=TRUE)
  mat.liste=c()
  MI.S=c()
  MI.liste2=c()
  for(k in 1:2){
  	MI.liste=c(NA, max(MI))
  	liste.int=seq(1,ncol(X), by=1)
  	liste=c(MI.max[k,])
  	liste.int=liste.int[-which(liste.int==liste[1])]
  	liste.int=liste.int[-which(liste.int==liste[2])]
  	MI.sum=max(MI)
  	while(length(liste.int)>0 && length(liste)<q){
  		i=liste[length(liste)]
  		ii=which(MI[,i]==max(MI[liste.int,i]))	
  		ii=ii[1]
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