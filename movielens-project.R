---
  title: "movielens-harvardx-capstone"
author: "Thiago do Couto"
date: "10/06/2019"
output: html_document
---

# 1 - Load required libraries

# Load required libraries
library(tidyverse)
library(caret)

# 2 - Load pre-processed RDS

# Load pre-processed RDS
edx <- readRDS("edx.rds")
validation <- readRDS("validation.rds")
head(edx)

# 3 - Define workspace vars - the "ratings" target and the "genres" correlated variable

# Define working variables
rating_var <- edx$rating
genres_var <- edx$genres

# 4 - Quiz time

# 4.1 - How many rows and columns are there in the EDX dataset?
data_observations <- nrow(edx)
data_observations # 9000055

# 4.2 - How many zeros and three were given in the EDX dataset?
sum(edx$rating == 0) # 0

# 4.3 - How many different movies are in the EDX dataset?
edx %>% summarize(dif_movies = n_distinct(movieId)) # 10677

# 4.4 - How many different users are in the EDX dataset?
edx %>% summarize(dif_users = n_distinct(userId)) # 

# 4.5 - How many movie ratings are in each of the following genres in the EDX dataset?
drama_filter <- edx %>% filter(str_detect(genres, "Drama")) %>% nrow()
comedy_filter <- edx %>% filter(str_detect(genres, "Comedy")) %>% nrow()
thriller_filter <- edx %>% filter(str_detect(genres, "Thriller")) %>% nrow()
romance_filter <- edx %>% filter(str_detect(genres, "Romance")) %>% nrow()

print('Drama:')
drama_filter # 3910127
print('Comedy:')
comedy_filter # 3540930
print('Thriller:')
thriller_filter # 2325899
print('Romance:')
romance_filter # 1712100

# 4.6 - Which movie has the greatest number of ratings?
greatest_ratings <- edx %>% group_by(title) %>% summarize(number = n()) %>% arrange(desc(number))
greatest_ratings[1,1] # Pulp Fiction (1994)

# 4.7 - What are the five most given ratings in order from most to least?
most_given_ratings <- edx %>% group_by(rating) %>% summarize(number = n()) %>% arrange(desc(number))
head(most_given_ratings$rating, 5) # 4.0 3.0 5.0 3.5 2.0

# 4.8 - True or False: In general, half star ratings are less common than whole star ratings?
half_stars <- edx %>% filter(str_detect(rating, ".5")) %>% nrow()
whole_stars <- data_observations - half_stars
print(half_stars < whole_stars) # True

# 5 - RMSE - Evaluating the model
# RMSE
RMSE <- function(actual, predicted){
sqrt(mean((actual - predicted)^2))
}

lambdas <- seq(0, 5, 0.25)

rmses <- sapply(lambdas, function(l) {

# Assess the Mu average from the training set ratings 
mu <- mean(edx$rating)

# Assess the average by movie only - penalize movies with few ratings
moa_balance <- edx %>% 
group_by(movieId) %>%
summarize(moa_balance = sum(rating - mu) / (n()+l))

# Assess the average by movie and user - penalize movies with few ratings
mua_balance <- edx %>% 
left_join(moa_balance, by = "movieId") %>%
group_by(userId) %>%
summarize(mua_balance = sum(rating - moa_balance - mu) / (n()+l))
predicted_ratings <- edx %>% 
left_join(moa_balance, by = "movieId") %>%
left_join(mua_balance, by = "userId") %>%
mutate(col_pred = mu + moa_balance + mua_balance) %>%
.$col_pred

return(RMSE(predicted_ratings, edx$rating))
})

plot(lambdas, rmses)

min_lambda <- lambdas[which.min(rmses)]

print('Minimal RMSE:')
min(rmses)
print('Minimal Lambda:')
min_lambda

y_hat <- sapply(min_lambda, function(l) {
  
  # Assess the Mu average from the training set ratings 
  mu <- mean(edx$rating)
  
  # Adequate Movie Only Average to min Lambda
  moa_balance <- edx %>% 
    group_by(movieId) %>%
    summarize(moa_balance = sum(rating - mu) / (n()+l))
  
  # Adequate Movie and User Average to min Lambda
  mua_balance <- edx %>% 
    left_join(moa_balance, by = "movieId") %>%
    group_by(userId) %>%
    summarize(mua_balance = sum(rating - moa_balance - mu) / (n()+l))
  
  # Assess the prediction over the validation set
  predicted_ratings <- 
    validation %>% 
    left_join(moa_balance, by = "movieId") %>%
    left_join(mua_balance, by = "userId") %>%
    mutate(col_pred = mu + moa_balance + mua_balance) %>%
    .$col_pred
  return(predicted_ratings)
})
# 6 - Save file to CSV
write.csv(validation %>% select(userId, movieId) %>% mutate(rating = y_hat), "submission.csv", na = "", row.names = FALSE)
