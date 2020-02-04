# Read data 
X=read.csv("~/winequality-white.csv", dec=".", sep=";")

source("~/order_dep.R")

source("~/General_info.R")

source("~/functions.R")

source("~/Grid.R")

q = ncol(X)

#MI
liste.MI=Order (X, f, h, G, q)
#Pearson
liste.Pearson=Order (X, f, h, G1, q)
#freeman tukey
liste.freeman=Order (X, f, h, G2, q)
#Neyman
liste.Neyman=Order (X, f, h, G3, q)
#Cressie Read
liste.Cressie=Order (X, f, h, G4, q)

# Separation order
source("~/order_sep.R")

source("~/General_info.R")

source("~/fit_Gauss.R")

source("~/kmeans_fit.R")

liste.sep=Order (X, f.mix.kmeans, f.Gauss, G, q)



source("~/Grid.R")