# Load the libraries 
library(tidyr)
library(ggplot2)
library(dplyr)

# Set the Working directory and save the data file in that particular folder 
## I have done some pre-processing and removed some of the variables from the given dataset for ease of calculations

musical_ratings <- read.csv("ratings_Musical_Instruments.csv", header = FALSE)
colnames(musical_ratings) <- c("User","Product","Rating","TimeStamp")

## Segregate the data where a customer has rated more than 20 reviews.
str(musical_ratings)

# What the Data said?
#1. There are total of 339231 customers who rated music instruments they purchased
#2. There are 83045 products which have been rated by different customers.
#3. Time is varaible which we have been neglecting here to make the analysis simple 


## Filter the data. Keep only those users who have rated more than 20 products.

library(sqldf)
music <- sqldf("select * from musical_ratings where User in (select User from musical_ratings group by User having count(User)>20)")


# Converted the data set into 
#rows = Users
#Column = Products
#Value = Rating
music <- music[,-4]
music_rating_matrix <- spread(music,Product,Rating)


## Still der are many NA values with in the data . So we are keeping only those instruments which got more than 
#20 ratings
music_user_product_rating <- sqldf("select * from music where Product in (select Product from music group by Product having count(Product)>20)")
music_rating_product_matrix <- spread(music_user_product_rating,Product,Rating)

## This has very less data.


data <- music_rating_product_matrix[,-1]

## Correlation Matrix.
## Using the data_frame music_rating_matrix_product 

#Creating a dummy matrix
matrix_probabilty <- matrix(data = NA, nrow=5,ncol=5, dimnames = list(c(colnames(data)),c(colnames(data))))

# Calculating the correlations.
for (i in 1:length(data)){
  for( j in 1:length(data)){
    frame <- cbind(data[,i],data[,j])
    frame <- data.frame(frame)
    colnames(frame)<- c("P1","P2")
    mean1 <- mean(frame$P1,na.rm = TRUE)
    mean2 <- mean(frame$P2,na.rm = TRUE)
    frame <- na.omit(frame)
    frame$P1 <- frame$P1 - mean1
    frame$P2 <- frame$P2 - mean2
    frame <- mutate(frame, Product = P1 * P2)
    frame <- mutate(frame, Productsq = (P1^2)*(P2^2))
    matrix_probabilty[i,j] <- sum(frame$Product)/sum(frame$Productsq) 
  }
}

## Adjusted Cosine Matrix.
## Using the data_frame music_rating_matrix_product 
#Creating a dummy matrix
matrix_cosine <- matrix(data = NA, nrow=5,ncol=5, dimnames = list(c(colnames(data)),c(colnames(data))))

data_cos <- data
# Calculating the correlations.
data_cos <- mutate(data_cos, meanrow= rowMeans(data_cos,na.rm = TRUE) )
data_cos <- data_cos[,1:5]-data_cos$meanrow


for (i in 1:length(data_cos)){
  for( j in 1:length(data_cos)){
    frame <- cbind(data_cos[,i],data_cos[,j])
    frame <- data.frame(frame)
    colnames(frame)<- c("P1","P2")
    frame <- na.omit(frame)
    frame <- mutate(frame, Product = P1 * P2)
    frame <- mutate(frame, Productsq = (P1^2)*(P2^2))
    matrix_cosine[i,j] <- sum(frame$Product)/sum(frame$Productsq) 
  }
}

## This will give us a matrix of similarity correlation using Adjusted Cosine formula

