# This code is for reordering high dimensional data for a separation purpose
# To simplify we start by fixing the first variable: for that the variable that separates best the data 
source('~\functions.R')
source('~\General_info.R')


order_dep_uni <- function(X, i_x1,f, h, G ){
  
  ind.1 = i_x1
  M.M1=Grid(X[,ind.1])
  x1=X[,ind.1]
  MI=rep(0, ncol(X))
  for (i in 1:ncol(X)){
    if(i!=ind.1){
      x2=X[,i]
      M.M2= Grid(x2)
      f=f.mix.kmeans(x1,x2, M.M1, M.M2)
      h=f.Gauss (x1,x2, M.M1, M.M2)
      MI[i]=GI(f,h,G)
    }
    
  }
  
  ind=which(MI==max(MI))
  liste=c(ind.1, ind)
  
  while (length(liste)<q){
    x1=X[,ind]
    M.M1=Grid(X[,ind])
    ind.ad=1:ncol(X)
    indices=ind.ad[-liste]
    MI=rep(0, ncol(X))
    for (j in indices){
      x2=X[,j]
      M.M2= Grid(x2)
      f=f.mix.kmeans(x1,x2, M.M1, M.M2)
      h=f.Gauss (x1,x2, M.M1, M.M2)
      MI[j]=GI(f,h,G)
    }
    ind=which(MI==max(MI))
    liste=c(liste, ind)
  }
  return (liste)
}