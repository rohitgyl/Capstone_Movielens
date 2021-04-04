# This code is written using R v 4.0 on Mac

##########################################################
# Data Download and reading of files 
##########################################################

# Note: this process could take few minutes for the data download

# Install the required packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr")
if(!require(dplyr)) install.packages("ggplot2")
if(!require(dplyr)) install.packages("stringr")
if(!require(dplyr)) install.packages("readr")
if(!require(dplyr)) install.packages("tidyr")
if(!require(dplyr)) install.packages("lubridate")
if(!require(dplyr)) install.packages("tidyr")
if(!require(dplyr)) install.packages("knitr")
if(!require(dplyr)) install.packages("kableExtra")

# load libraries
library(tidyverse)
library(caret)
library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(readr)
library(tidyr)
library(lubridate)
library(knitr)


# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

# Download the zip file
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

# Read ratings file
ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))
# Read movies file
movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later: format the column data types
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


##########################################################
# Create edx set (which will be used for model building)
# validation set (final hold-out test set)
# 
##########################################################

# Join ratings and movie data sets
movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

# remove unwanted variables
rm(dl, ratings, movies, test_index, temp, movielens, removed)

##########################################################
# Transform datetimestamp attribute and extract year of rating
##########################################################

# derive year in which the rating was given and store as new attribute called rate_year
edx <- edx %>% mutate(rate_year =year(as_datetime(timestamp)))


##########################################################
# Data exploration and visualization
##########################################################

# Check sample data to inspect data attributes
head(edx) %>% knitr::kable("simple")

# dimension of dataset
dim(edx) %>% knitr::kable(col.names = "Dimensions","simple") 

# distinct users and movies
edx %>%
  summarize(distinct_users = n_distinct(userId),distinct_movies = n_distinct(movieId)) %>% knitr::kable("simple")

# Top 10 most rated movies - find top 10 movieId and then join with a distinct set of movieId and title
top_movieId <- edx %>% group_by(movieId) %>% summarise(no_of_ratings=n()) %>%arrange(desc(no_of_ratings)) %>%
  top_n(10)

movie_title <- edx %>% distinct(movieId,title)

top_movieId %>% inner_join(movie_title,by="movieId") %>%
  knitr::kable("simple")

# Distribution of no of ratings given by Movie
edx %>% group_by(movieId) %>% summarise(no_of_ratings = n()) %>% 
  ggplot(aes(no_of_ratings)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Distribution of No. of Ratings by Movie")

# Top 10 most given ratings
edx %>% group_by(rating) %>% summarise(rating_count=n()) %>% arrange(desc(rating_count))%>% top_n(10)%>%
  knitr::kable("simple")

# Top 10 genres with most number of ratings

edx %>% group_by(genres) %>% summarise(genres_count=n()) %>%arrange(desc(genres_count)) %>% top_n(10)%>% knitr::kable("simple")

# Distribution of Avg rating by Movie 
  edx %>% 
    group_by(movieId) %>% 
    summarize(avg_rating = mean(rating)) %>% 
    ##filter(n()>=100) %>%
    ggplot(aes(avg_rating)) + 
    geom_histogram(bins = 30, color = "black")+ 
    ggtitle("Distribution of  Avg Rating by Movie")
  
# Distribution of Avg rating by Users 
edx %>% 
  group_by(userId) %>% 
  summarize(avg_rating = mean(rating)) %>% 
  ggplot(aes(avg_rating)) + 
  geom_histogram(bins = 30, color = "black")+ 
  ggtitle("Distribution of Avg Rating by User")


# Plot Avg rating trend vs Genres
# filter genres having atleast 50000 ratings to analyse trend of most frequently rated genres.
edx %>% group_by(genres) %>% summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 50000) %>% 
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Avg Rating Trend vs Genres")
  
# Plot Avg rating trend vs ratings per year

edx %>% group_by(movieId) %>% summarise(cnt = n(), avg_rating = mean(rating), years =max(rate_year)-min(rate_year)+1)%>%
  mutate(ratings_per_year = cnt/years) %>% 
  ggplot(aes(ratings_per_year,avg_rating)) + 
  geom_point()+
  geom_smooth()+
  ggtitle("Avg Rating vs Ratings per year")



####################################################################################
# Modeling 
# simple mean,movie effect, movie+user, movie+user+genres,movie+user+genres with regularization 
####################################################################################

set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`

# split edx data set further into 20% for test and 80% for training
test_index_edx <- createDataPartition(y = edx$rating, times = 1, p = 0.2, list = FALSE)
edx_train <- edx[-test_index_edx,]
edx_test <- edx[test_index_edx,]

#  Make sure userId and movieId in edx_test set are also in edx_train set
edx_test <- edx_test %>% 
  semi_join(edx_train, by = "movieId") %>%
  semi_join(edx_train, by = "userId")

# define RMSE function for evaluation of proposed model
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

####################################################################################
# Modeling 1 - use simple avg of all ratings
####################################################################################
# calculate mean of all ratings in training set
mu <- mean(edx_train$rating)

# calculate RMSE for test set
model_1_rmse <- RMSE(edx_test$rating, mu)

# Add model results to the table
rmse_results <- data_frame(method = "Simple average", RMSE = model_1_rmse)

rmse_results  %>%  knitr::kable(col.names = c("Model","RMSE"),"simple")

####################################################################################
# Modeling 2 - Movie effect added
####################################################################################

# calculate mean of all ratings in training set
mu <- mean(edx_train$rating) 

# calculate movie effect term 
movie_avgs <- edx_train %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

# calculate predicted ratings of test set 
predicted_ratings <- mu + edx_test %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i

# calcluate RMSE for model with movie effect
model_2_rmse <- RMSE( edx_test$rating, predicted_ratings)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",
                                     RMSE = model_2_rmse ))
# Add model results to the table
rmse_results  %>%  knitr::kable(col.names = c("Model","RMSE"),"simple")


####################################################################################
# Modeling 3 - User effect added
####################################################################################

# calculate user effect term
user_avgs <- edx_train %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

# calculate predicted ratings of test set 
predicted_ratings <- edx_test %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

# calcluate RMSE for model with user effect added
model_3_rmse <- RMSE(edx_test$rating, predicted_ratings )

# Add model results to the table
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = model_3_rmse ))
rmse_results  %>%  knitr::kable(col.names = c("Model","RMSE"),"simple")

####################################################################################
# Modeling 4 - genres effect added
####################################################################################

# calculate genres effect term
genre_avgs <- edx_train %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu - b_i-b_u))

# calculate predicted ratings of test set 
predicted_ratings <- edx_test %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  .$pred

# calcluate RMSE for model with genres effect added
model_4_rmse <- RMSE(edx_test$rating,predicted_ratings)

# Add model results to the table
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User+ Genres Effects Model",  
                                     RMSE = model_4_rmse ))
rmse_results  %>%  knitr::kable(col.names = c("Model","RMSE"),"simple")

####################################################################################
# Modeling 5 - Apply regularization technique
####################################################################################


# train lambda on edx train sub set 
lambdas <- seq(0, 10, 0.25)

# calculate rmse with various values of lambda
rmses <- sapply(lambdas, function(l){
  mu <- mean(edx_train$rating) 
  b_i <- edx_train %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- edx_train %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating -mu - b_i)/(n()+l))
  
  b_g <- edx_train %>% 
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - mu - b_i-b_u)/(n()+l))
  
  predicted_ratings <- edx_test %>% 
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    left_join(b_g, by='genres') %>%
    mutate(pred = mu + b_i + b_u + b_g) %>%
    .$pred
  
  return(RMSE( edx_test$rating,predicted_ratings))
})

# plot the rmses  for various lambdas to see the minima
qplot(lambdas, rmses)  

# chose the lambda which minimizes rmse
lambda <- lambdas[which.min(rmses)]
lambda  %>%  knitr::kable(col.names = c("Regularziation lambda"),"simple")

# find the RMSE achieved for chosen lambda
model_5_rmse <- min(rmses)

# Add model results to the table
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User+ Genres Effects + Regularization Model",  
                                     RMSE = model_5_rmse ))
rmse_results  %>%  knitr::kable(col.names = c("Model","RMSE"),"simple")


## generate final model with chosen lambda but using full training set i.e. edx set

mu <- mean(edx$rating) 
b_i <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))
b_u <- edx %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu - b_i )/(n()+lambda))

b_g <- edx %>% 
  left_join(b_i, by='movieId') %>%
  left_join(b_u, by='userId') %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating - mu - b_i-b_u)/(n()+lambda))



######################################################################
# Test final model against validation set or the final hold out set
######################################################################

# calculate predicted ratings of Validation set using the final model
predicted_ratings <- validation %>% 
  left_join(b_i, by='movieId') %>%
  left_join(b_u, by='userId') %>%
  left_join(b_g, by='genres') %>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  .$pred

# calculate rmse of final model on Validation set
final_model_remse_validationset <- RMSE(validation$rating,predicted_ratings)

final_model_remse_validationset %>%  knitr::kable(col.names = c("Final Model RMSE with Validation Set"),"simple")
