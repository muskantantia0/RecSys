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


# We are using Correlation to predict the user ratings using Item_Item Similarity Matrix

#Creating a dummy matrix
matrix_correlation <- matrix(data = NA, nrow=5,ncol=5, dimnames = list(c(colnames(data)),c(colnames(data))))

music_rating_product_matrix <- spread(music_user_product_rating,Product,Rating,fill=0)

# Removing the users from this space
data <- music_rating_product_matrix[,-1]

# Calculating the correlations.
for (i in 1:length(data)){
  for( j in 1:length(data)){
    frame <- cbind(data[,i],data[,j])
    frame <- data.frame(frame)
    colnames(frame)<- c("P1","P2")
    mean1 <- mean(frame$P1,na.rm = TRUE)
    mean2 <- mean(frame$P2,na.rm = TRUE)
    frame <- mutate(frame, Product = P1 * P2)
    frame <- mutate(frame, P1sq = (P1^2))
    frame <- mutate(frame, P2sq = (P2^2))
    matrix_correlation[i,j] <- sum(frame$Product)/((sum(frame$P1sq))^0.5*(sum(frame$P2sq))^0.5)
  }
}



# Predictions 
# We are creating 2 data frames. One to compute values and the other to impute the predicted values
data_withoutNA <- spread(music_user_product_rating,Product,Rating,fill=0)
data_withoutNA <- data_withoutNA[,-1]
data_predict <- spread(music_user_product_rating,Product,Rating)
data_predict <- data_predict[,-1]


# Assumption
# We are taking only the top 3 similar products for each and every product.

for ( i in 1:nrow(data)){
  for ( j in 1:ncol(data)){
    if(is.na(data[i,j]) == TRUE){
      matrix1row <- matrix_correlation[j,]
      matrix1row <- c(matrix1row)
      matrix1row <- sort(matrix1row,decreasing = TRUE)
      matrix1row <- matrix1row[2:(2+2)]
      p <- which(matrix_correlation[j,] == matrix1row[1])
      q <- which(matrix_correlation[j,] == matrix1row[2])
      r <- which(matrix_correlation[j,] == matrix1row[3])
      data_predict[i,j] <- ((data_withoutNA[i,p]*matrix_correlation[j,p])+
                              (data_withoutNA[i,p]*matrix_correlation[j,q])+
                              (data_withoutNA[i,p]*matrix_correlation[j,r]))/(matrix_correlation[j,p]+
                                                                                matrix_correlation[j,q]+
                                                                                matrix_correlation[j,r])
      
    }
  }
}

## data_predict data.frame has imputed ratings for each and every product for each user.
## Since the data is too less , our algo has predicted 0 rating for some users.










