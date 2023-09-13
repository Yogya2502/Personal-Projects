install.packages("corrr")
install.packages("ggcorrplot")
install.packages("FactoMineR")

#set the working directory
setwd("C:/Users/Yogya Hridey Sareen/Desktop/PSA")

#open required libraries
library(corrr)
library(ggplot2)
library(ggcorrplot)
library(FactoMineR)
install.packages("factoextra")
library(factoextra)

#upload the dataset onto R
data_set <- read.csv("C:/Users/Yogya Hridey Sareen/Desktop/PSA/Pizza.csv")

str(data_set)

#checking for missing values
colSums(is.na(data_set))

#removing ID and Brand Name from the data set since they are not numeric 
#variables and will thus hinder PCA
data_set$id <- NULL
data_set$brand <- NULL

#convert the data to normalized form
data_normalized <- scale(data_set)
head(data_normalized)

#make a correlation matrix
corr_matrix <- cor(data_normalized)
ggcorrplot(corr_matrix)

#perform Principal Component Analysis
data.pca <- princomp(corr_matrix)
summary(data.pca)

#changes data in decimal terms rather than in terms of e 
options(scipen = 10)

#create the loading table
data.pca$loadings[, 1:2]

#make the scree plot
fviz_eig(data.pca, addlabels = TRUE)
fviz_pca_var(data.pca, col.var = "black")

?fviz

#make the biplot
fviz_pca_var(data.pca, col.var = "cos2", gradient.cols = c("black", "orange", "green"),repel = TRUE)
