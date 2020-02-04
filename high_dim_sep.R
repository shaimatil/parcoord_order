# This code is for reordering high dimensional data for a separation purpose
# To simplify we start by fixing the first variable: for that the variable that separates best the data 
source('~\fit_Gauss.R')
source('~\kmeans_fit.R')
source('~\General_info.R')

selec.uni <- function(x, c){
  set.seed(123)
  library(mvtnorm)
  library(mixtools)
  fit <- kmeans(x, )
  mu=fit$centers
  s=0
  data= cbind( x, fit$cluster)
  data = data [order(fit$cluster),]
  l=1
  for (i in 1:c){

    if(fit$size[i]>1){
      s=s + var(data[l:(l+fit$size[i]-1),1])*(fit$size[i]-1)
      l=l+fit$size[i]
    }
  }
  s = s/(length(x)-1)
  lambda=fit$size/length(x)
  P.Mix <- function(lambda, mu, sigma, I) {
    p=0
    for( i in 1:length(lambda)){
      p=p+lambda[i]*(pnorm(I[2],mean=mu[i], sd=sqrt(sigma))-pnorm(I[1],mean=mu[i], sd=sqrt(sigma)))
    }
    return(p)
  }
  I=Grid(x)
  f=I[,1]
  for (k in 1:nrow(I)){

    f[k]=P.Mix (lambda, mu, s, I[k,])

  }

  par = mvnX(x)$parameters
  Sigma.s  = sqrt(unlist(par$variance$sigma))

  mu.s=par$mean

  h=I[,1]
  for (k in 1:nrow(I)){
    h[k]= pnorm(I[2],mean=mu.s, sd=Sigma.s)-pnorm(I[1],mean=mu.s, sd=Sigma.s)
  }

  return(GI(f,h, G))
}

order_sep_uni <- function(X){
  critere.uni=rep(0, ncol(X))
  for (i in 1:ncol(X)){
    if (unique(X[,i])>=c){
      critere.uni[i]=selec.uni(X[,i],c)
    }
    
  }
  
  ind.1=which(critere.uni==max(critere.uni))
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
  return(liste)
}