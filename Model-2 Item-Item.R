# Set the Working data

#Load the Libraries 
library(dplyr)
library(plyr)

# Assumptions 
# 1. We are Using Pearson-Correlation to find the Item-Item Similarity.
# 2. We are Using the Similarity to find the results. We have scaled the data with mean before finding the
#   correlation

#Laod the data
ratingsdummydata <- read.csv("ratingsdummydata.csv",sep = ",",header = TRUE)

#Create a dummy martix 
movies <- as.list(colnames(ratingsdummydata)[2:11])
pearson_correlation <- matrix(data = NA , nrow = ncol(ratingsdummydata)-1, ncol = ncol(ratingsdummydata)-1, dimnames = list(c(movies),c(movies)))

#Creating another data frame by removing users from the previous ratingsdummydata
Itemratingsdummydata <- ratingsdummydata[,-1]


# Calculate the correlations 
for(i in 1:length(Itemratingsdummydata)){
  for(j in 1:length(Itemratingsdummydata)){
    P1 <- Itemratingsdummydata[,i]
    P2 <- Itemratingsdummydata[,j]
    data <- cbind(P1,P2)
    data <- data.frame(data)
    data <- na.omit(data)
    mean1 <- mean(data$P1,na.rm = TRUE)
    mean2 <- mean(data$P2,na.rm = TRUE)
    data$P1 <- data$P1 - mean1
    data$P2 <- data$P2 - mean2
    data <- mutate(data, P1P2 = P1*P2)
    data <- mutate(data, P1sq = P1^2)
    data <- mutate(data, P2sq = P2^2)
    pearson_correlation[i,j] <- sum(data$P1P2)/((sum(data$P1sq)*sum(data$P2sq))^0.5)
    P1 <- NULL
    P2 <- NULL
  }
}


# Predictions 
# Created the same data frame with another name so that 

Predictions <- Itemratingsdummydata

for ( i in 1:nrow(Predictions)){
  for ( j in 1:ncol(Predictions)){
    if(is.na(Itemratingsdummydata[i,j])== TRUE){
      x <- colnames(Itemratingsdummydata)
      x <- x[j]
      x <- which(colnames(Itemratingsdummydata) == x)
      matrix1row <- pearson_correlation[j,]
      matrix1row <- matrix1row[-x]
      matrix1row <- sort(matrix1row,decreasing = TRUE)
      m <- Itemratingsdummydata[i,c(names(matrix1row))]
      z <- which(is.na(Itemratingsdummydata[i,c(names(matrix1row))]))
      if(length(z) == 0){
        m <- m
      }
      else{
        m <-m[-z]
      }
      meanmovie <- mean(Itemratingsdummydata[,j],na.rm=TRUE)
      Predictions[i,j]<-meanmovie+((pearson_correlation[j,names(m)] %*% t(Itemratingsdummydata[i,names(m)]-meanmovie)))/sum(abs(pearson_correlation[j,names(m)]))
    }
    else{
      Predictions[i,j] <- NA
    }
  }
}

#Final Predictions
Predictions <- cbind(ratingsdummydata[,1],Predictions)
colnames(Predictions)[1] <- "User"


# Two DataFrames
# Itemsratingsdummydata - User rated ratings
# Predictions           - User Predicted Ratings

# For time Being we will give the number of the Customer Instead of his name.
Username <- function(x){
  UserPredictions <- Predictions[x,2:ncol(Predictions)]
  meanuserrating <- mean(as.matrix(ratingsdummydata[x,2:ncol(ratingsdummydata)]),na.rm = TRUE)
  GreaterthanMeanNum <- which(Predictions[x,2:ncol(Predictions)] <= meanuserrating)
  UserPredictions <- UserPredictions[-(GreaterthanMeanNum)]
  UserPredictions <- sort(UserPredictions[-which(is.na(UserPredictions))],decreasing = TRUE)
  if(length(UserPredictions) == 0){
    return("No Predictions")
  }
  else{
    return (colnames(UserPredictions))
  }
}

# Only with number it is working 
