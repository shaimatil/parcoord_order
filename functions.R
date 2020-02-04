#produit de proba
h <- function(x1, x2){
	X=cbind(x1, x2)

	m=dim(X)
	n.obs=m[1]
	n.var=m[2]
	classes=c()
	prob=c()
	#frequencies and probability for each class for each variable

	max=c()
	min=c()
	for (i in 1:n.var){
		n.class=min(length(levels(factor(X[,i]))) , nclass.Sturges(X[,i]))
		factorx <- factor(cut(X[,i], breaks=n.class))
		#Tabulate and turn into data.frame
		x.out <- as.data.frame(table(factorx))
		#Add cumFreq and proportions
		x.out <- transform(x.out, relative = prop.table(Freq))
		pos.vir=regexpr(",", x.out[,1])
		
		len=nchar(as.character.factor(x.out[,1]))

		if(i==1){
			prob1=x.out[,3]
			min1= as.numeric(substr(x.out[,1], 2, pos.vir-1))
			max1=as.numeric(substr(x.out[,1], pos.vir+1, len-1))
		}

		if(i==2){
			prob2=x.out[,3]
			min2= as.numeric(substr(x.out[,1], 2, pos.vir-1))
			max2=as.numeric(substr(x.out[,1], pos.vir+1, len-1))
		}
	}

	p=prob1%*%t(prob2)
	return(p)
}


#####Probabilité jointe
f <- function(x1, x2){

	X=cbind(x1, x2)

	n.obs=nrow(X)
	n.var=ncol(X)
	classes=c()
	prob=c()
	#frequencies and probability for each class for each variable
	max=c()
	min=c()
	for (i in 1:n.var){
		n.class=min(length(levels(factor(X[,i]))) , nclass.Sturges(X[,i]))
		factorx <- factor(cut(X[,i], breaks=n.class))
		#Tabulate and turn into data.frame
		x.out <- as.data.frame(table(factorx))
		#Add cumFreq and proportions
		x.out <- transform(x.out, relative = prop.table(Freq))
		pos.vir=regexpr(",", x.out[,1])

		len=nchar(as.character.factor(x.out[,1]))
		if(i==1){
			max1=as.numeric(substr(x.out[,1], pos.vir+1, len-1))
			min1=as.numeric(substr(x.out[,1], 2, pos.vir-1))
		}
		if(i==2){
			max2=as.numeric(substr(x.out[,1], pos.vir+1, len-1))
			min2=as.numeric(substr(x.out[,1], 2, pos.vir-1))
		}

	}
	
	P.joint=matrix(rep(0, length(max1)*length(max2)), nrow=length(max1))
	total=0
	#occurence class cx et cy
	for (k in 1:length(max1)){
		for (l in 1:length(max2)){
			#nombre d obs dont la var X[,i] est dans la classe k et la var X[,j] est dans la classe l
			freq=length(which (X[,1]<=max1[k] & X[,1]>=min1[k] & X[,2]<=max2[l] & X[,2]>=min2[l]))
			P.joint[k,l]=freq/n.obs 
		}

	}
return(P.joint)
}

#Exemple de fonction G
#f"=1
G <- function(u){
	return(u*log(u))
}
#f"=2 Pearson
G1 <- function(u){
	return((u-1)^2)
}
#f"=1 Tukey
G2 <- function(u){
	return(u*(1-1/sqrt(u)))
}
#f"= Neyman
G3 <- function(u){
	V=(1-u)^2/(u)
	V[which(V>99999999999999999999999999999,arr.ind=T)]=0
	return(V)

}
#Cressie Read
G4 <- function(u){
	return(u*((u^2)^(1/3)-1))
}