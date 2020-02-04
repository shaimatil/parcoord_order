Grid <- function(x1){
  X=cbind(x1)
  for (i in 1:1){
  	n.class=min(length(levels(factor(X[,i]))) , nclass.Sturges(X[,i]))
  	factorx <- factor(cut(X[,i], breaks=n.class))
  	#Tabulate and turn into data.frame
  	x.out <- as.data.frame(table(factorx))
  	#Add cumFreq and proportions
  	x.out <- transform(x.out, relative = prop.table(Freq))
  	pos.vir=regexpr(",", x.out[,1])
  		
  	len=nchar(as.character.factor(x.out[,1]))
  	prob1=x.out[,3]
  	min1= as.numeric(substr(x.out[,1], 2, pos.vir-1))
  	max1=as.numeric(substr(x.out[,1], pos.vir+1, len-1))
  }
  return(cbind(min1, max1))

}