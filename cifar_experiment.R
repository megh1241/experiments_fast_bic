library(rerf)
library(readmnist)
library(stats)
library(ggplot2)
library(MASS)
library(Matrix)
library(randomForest)
library(scatterplot3d)
library(gridExtra)
require(ggplot2)
source("~/pr_utility_functions.R")
library(ISLR)
library("readr")

#Choose one of Filenames: "s2", "s4", "a1", "a3", "b1", "b2" , "dim-1024"
fname = "cifar" 

#Save the graphs/plots to the following pdf
pdf_filename = "cifar.pdf" 

#Load datasets from data dir. Here the dataset and labels are in separate files
loadDataFromTwoFiles <- function(data_filename, gt_labels_filename){
    X = as.matrix(read_table(data_filename))
    y = as.matrix(read_table(gt_labels_filename))
    return( list(X, y))
}

loadCifar <- function(data_filename, gt_labels_filename){
#    system("curl https://raw.githubusercontent.com/mlampros/DataSets/master/cifar_10.zip")      

cifar_10 <- read.table("cifar_10.csv", nrows = 10000, header = T, 
                       quote = "\"", sep = ",")
X = as.matrix(cifar_10[1:5000, -ncol(cifar_10)])
y = cifar_10[1:5000, ncol(cifar_10)]
    #X[is.na(X)] <- 0
    #X[is.nan(X)] <- 0
#X = as.matrix(X[, colSums(X) > 0])
print (y) 
    return( list(X, y))
}


#Load datasets from data dir. Here the dataset and labels are in the same file.
#The last column of the file consists of the labels
loadDataFromSingleFile <- function(data_filename){
    dat = read.table(data_filename)
    y = as.matrix(dat[,ncol(dat)])
    X = as.matrix(dat[,-ncol(dat)])
    y <- as.numeric(factor(y))
    X[is.na(X)] <- 0
    y[is.na(y)] <- 0
    return( list(X, y))
}

#Load Iris dataset
loadIrisData <- function(){
   X <- as.matrix(iris[,1:4])
   y <- as.matrix(iris[,5])
    X[is.na(X)] <- 0
    y[is.na(y)] <- 0
   return (list(X, y))
}

#Load BCW dataset (Breast cancer dataset from UCI repo)
loadBCWData <- function(){
    data("bcw_original", package = "ucidata")
    X <- data.matrix(bcw_original[,1:10])
    X[is.na(X)] <- 0
    y <- bcw_original[,11]
    y[is.na(y)] <- 0
    y <- as.numeric(factor(y))
    return (list(X, y))
}


# save plots to a pdf file
pdf(file=pdf_filename)

#normalize the data
normalizeData <- function(X) {
  X <- sweep(X, 2, apply(X, 2, min), "-")
  sweep(X, 2, apply(X, 2, max), "/")
}

# number of trees for forest
numtrees <- 500

#uncomment to use datasets in the data dir
#lst <- loadDataFromTwoFiles(paste("data/",fname,".txt", sep=""), paste("data/",fname,"-label.txt", sep=""))
#lst <- loadIrisData()
lst <- loadCifar()
#lst <- loadDataFromTwoFiles("data/cifar10_train100.txt", "data/cifar10_test100.txt")
X<- lst[[1]]
y<- lst[[2]]
num_of_points <- length(y)

#for precision@k Feel free to change this
start_k = 10
end_k = 100
at_K = seq(1, 50, by=5)
data_label=y
#X <- normalizeData(X)
print(length(X))
print(length(y))
# urf + two means loss
urerfStructure <- Urerf(X, trees=numtrees, splitCrit="twomeans", LinearCombo=FALSE)
#simMat = randomForest(X, ntree=numtrees)$proximity
simMat = urerfStructure$similarityMatrix
D_rf=1-simMat
D_rf_p_r_list = p_r_list(D_rf, data_label, at_K, num_of_points)
D_rf_precision_list= D_rf_p_r_list$precisionList
print(D_rf_precision_list)
D_rf_recall_list=D_rf_p_r_list$recallList

print("random forest done")
# urf + BIC loss
urerfStructure <- Urerf(X, trees=numtrees, splitCrit="bicfast", LinearCombo=FALSE)
simMat = urerfStructure$similarityMatrix
D_rf=1-simMat
D_rf_p_r_list = p_r_list(D_rf, data_label, at_K, num_of_points)
D_rf_bic_precision_list= D_rf_p_r_list$precisionList
print(D_rf_bic_precision_list)
D_rf_bic_recall_list=D_rf_p_r_list$recallList


# Urerf + two means loss
urerfStructure <- Urerf(X, trees=numtrees, splitCrit="twomeans")
simMat = urerfStructure$similarityMatrix
D_rf=1-simMat
D_rf_p_r_list = p_r_list(D_rf, data_label, at_K, num_of_points)
D_rerf_precision_list= D_rf_p_r_list$precisionList
print(D_rerf_precision_list)
D_rerf_recall_list=D_rf_p_r_list$recallList


# Urerf + BIC loss
urerfStructure <- Urerf(X, trees=numtrees, splitCrit="bicfast")
simMat = urerfStructure$similarityMatrix
D_rf=1-simMat
D_rf_p_r_list = p_r_list(D_rf, data_label, at_K, num_of_points)
D_rerf_bic_precision_list = D_rf_p_r_list$precisionList
print(D_rerf_bic_precision_list)
D_rerf_bic_recall_list=D_rf_p_r_list$recallList


# Urerf + BIC loss

#Uncomment for expt 3
# PCA using SVD
dPca = prcomp(X)
ncomp = 50
trainPca = as.matrix(X) %*% dPca$rotation[ , 1:ncomp]  # columns are eigenvectors, use first 150 principal components
D_rf = as.matrix(dist(trainPca))
D_rf_p_r_list = p_r_list(D_rf, data_label, at_K, num_of_points)
D_pca_precision_list= D_rf_p_r_list$precisionList
D_pca_recall_list=D_rf_p_r_list$recallList
#invoke utility function to compute precision and recall


#############################take care of the plotting################################
################## plot precision vs K , ylim = c(0, 1)
plot(at_K, D_rf_precision_list, type="l", col="green", xlab = "@k", ylab = "Precision", ylim = c(0, 1))
lines(at_K, D_rf_bic_precision_list, type="l", col="red")
lines(at_K, D_rerf_precision_list, type="l", col="blue")
lines(at_K, D_rerf_bic_precision_list, type = "l", col="orange")
lines(at_K, D_pca_precision_list, type="l", col="cyan")
legend("topright", inset=.05,  cex = 0.7, title="Variant",  c("randomForest","URF+BICFAST", "URerF+two means", "URerF+BICFAST", "PCA"),   horiz=FALSE, lty=c(1,1), lwd=c(2,2),col=c("green",  "red", "blue",  "orange"),  bg="grey96")

############### plot recall vs K
plot(at_K, D_rf_recall_list, type='l', col="green", xlab = "@k", ylab = "Recall")
lines(at_K, D_rf_bic_recall_list, type='l', col="red")
lines(at_K, D_rerf_recall_list, type='l', col="blue")
lines(at_K, D_rerf_bic_recall_list, type = "l", col="orange")
legend("bottomright", inset=.05,  cex = 0.5, title="Variant",  c("randomForest","URF+BICFAST", "URerF+two means", "URerF+BICFAST", "PCA" ),   horiz=FALSE,  lty=c(1,1), lwd=c(2,2),  col=c("green", "red", "blue",  "orange"),  bg="grey96")

################# plot precision vs recall
plot(D_rf_recall_list, D_rf_precision_list, type = 'l', col="green", xlab = "Recall", ylab = "Precision", ylim = c(0, 1))
lines(D_rf_bic_recall_list, D_rf_bic_precision_list, type = 'l', col="red")
lines(D_rerf_recall_list, D_rerf_precision_list, type = 'l', col='blue')
lines(D_rerf_bic_recall_list, D_rerf_bic_precision_list, type = 'l', col='orange')
legend("bottomright", inset=.01,  cex = 0.4, title="Variant",  c("randomForest","URF+BICFAST", "URerF+two means", "URerF+BICFAST", "PCA"),   horiz=FALSE,  lty=c(1,1), lwd=c(2,2),  col=c("green", "red","blue", "orange" ),  bg="grey96")

