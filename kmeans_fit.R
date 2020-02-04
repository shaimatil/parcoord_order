#f for data separation
f.mix.kmeans <- function(x1,x2, M.M1, M.M2, k){
  set.seed(123)
  library(mvtnorm)
  library(mixtools)
  
  #number of clusters
  x=cbind(x1,x2)
  
  fit <- kmeans(x, k)
  #center of clusters 
  mu=fit$centers
  data= cbind( x1, x2, fit$cluster)
  data = data [order(fit$cluster),]
  s11=c()
  s22=c()
  l=1
  s1=0
  s2=0
  for (i in 1:k){
    
    s11=cbind(s11, var(data[l:(l+fit$size[i]-1),1]))
    s22=cbind(s22, var (data[l:(l+fit$size[i]-1),2]))
    if(fit$size[i]>1){
      s1=s1 + var(data[l:(l+fit$size[i]-1),1])*(fit$size[i]-1)
      s2=s2 + var (data[l:(l+fit$size[i]-1),2])*(fit$size[i]-1)
      l=l+fit$size[i]	
    }
  }
  
  #variance
  
  s = c(s1/(nrow(x)-1), s2/(nrow(x)-1))
  Sigma = rbind(c(s[1], 0), c(0, s[2]))
  

  lambda=fit$size/nrow(x)
  P.Mix <- function(lambda, mu, sigma, I) {
    p=0	
    for( i in 1:length(lambda)){
      p=p+lambda[i]*pmvnorm(I[,1], I[,2],mean=mu[i,], sigma=Sigma,algorithm = Miwa(steps = 1280))
    }
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
    
    f[MG[k,1],MG[k,2]]=P.Mix (lambda, mu, Sigma, I)
    
  }
  return(f)
}

