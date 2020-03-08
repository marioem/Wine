##################################### SVD #######################################################
#
#
# Getting the PCA output (prcomp) using SVD
#
#
#************************************************************************************************

library(tidyverse)
library(broom)
library(ggbiplot)

data(wine)

wine.svd <- svd(scale(wine))


####################################### ROTATION ################################################
#
# Principal Components, prcomp's rotation matrix
#
#************************************************************************************************
rot.svd <- wine.svd$v

###################################### IND COORD ################################################
#
# Ind coordinates, a.k.a loadings, prcomp's x matrix
#
#************************************************************************************************
# Equivalent of wine.pca$x
# Y = X P = X V
# P - rotation matrix from PCA
# X - original data
# Y - transformed data into PC coordinates

# Y = X V
ind.coord.svd <- scale(wine) %*% wine.svd$v
ind.coord.svd

# or Y = U D
ind.coord.svd.2 <- wine.svd$u %*% diag(wine.svd$d)
ind.coord.svd.2

identical(round(ind.coord.svd, 10), round(ind.coord.svd.2, 10)) # equivalence of the above methods

ind.coord.pca <- ind.coord
dimnames(ind.coord.pca) <- NULL

identical(round(ind.coord.svd, 10), round(ind.coord.pca, 10)) # equivalence of PCA and SVD

################################# Variance explained ############################################
#
# Variance explained
#
#************************************************************************************************

wine.svd$d^2/sum(wine.svd$d^2)

# Therefore
# wine.svd$d^2/sum(wine.svd$d^2) = wine.pca$sdev^2/nrow(wine.pca$rotation)

###################################### STDEV ####################################################
#
# StDev along principal directions, prcomp's stdev vector
#
#************************************************************************************************
sdev.svd <- wine.svd$d*sqrt(length(wine.svd$d))/sqrt(sum(wine.svd$d^2))


###################################### VAR COORD ################################################
#
# Var coordinates
#
#************************************************************************************************
var_coord_func <- function(rot, comp.sdev){
    rot*comp.sdev
}

wine.var.coord <- t(apply(rot.svd, 1, var_coord_func, sdev.svd)) 
wine.var.coord

var.coord.pca <- var.coord
dimnames(var.coord.pca) <- NULL

identical(round(wine.var.coord, 10), round(var.coord.pca, 10))

