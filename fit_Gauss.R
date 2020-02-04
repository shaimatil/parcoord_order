#h for data separation
f.Gauss <- function(x1,x2, M.M1, M.M2){
  library(mvtnorm)
  library(mixtools)
  library(mclust)
  
  par = mvn("XXI", cbind(x1,x2))$parameters
  S = unlist(par$variance$sigma)
  Sigma = S[,,1]
  mu=par$mean
  P.Mix <- function( mu, sigma, I) {
    
    p=pmvnorm(I[,1], I[,2], mean=mu, sigma=Sigma )
    
    return(p)
  }
  
  min1=M.M1[,1]
  max1=M.M1[,2]
  min2=M.M2[,1]
  max2=M.M2[,2]
  
  #expand.grid
  I1=seq(1,length(max1))
  I2=seq(1,length(max2))
  MG=expand.grid(I1,I2)
  
  
  f=matrix(0, nrow=length(max1), ncol=length(max2))
  for (k in 1:nrow(MG)){
    I=matrix(c(min1[MG[k,1]], max1[MG[k,1]],min2[MG[k,2]], max2[MG[k,2]]), nrow=2, byrow=TRUE)
    
    f[MG[k,1],MG[k,2]]=P.Mix (as.numeric(mu), Sigma, I)
    
  }
  return(f)
}


