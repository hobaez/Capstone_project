################################
# Create edx set, validation set

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")

#Data dowloading###############################

# Note: this process could take a couple of minutes
# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
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

rm(dl, ratings, movies, test_index, temp, movielens, removed)

#Exploration of the dataset=========================================
#First, let us explore the estructure of the dataset
glimpse(edx)

#A better look of the first lines of the dataset
head(edx)

#Since is a movie ratings dataset, let us explore the number of movies
length(unique(edx$movieId))

#Now, let us explore the number of ratings per movie
edx %>% group_by(movieId, title) %>%
  summarize(count = n()) %>%
  arrange(desc(count))%>%
  head()

#Now, we can check the distribution of movies vs the number of ratings they have
#We know from intuition that not every movie has the same number of ratings
edx %>% group_by(movieId)%>%
  summarize(count=n())%>%
  ggplot(aes(count))+
  geom_histogram(color="blue")+
  scale_x_log10()+
  xlab("Number of ratings")+
  ylab("Number of movies")+
  theme_economist()

#Now, let us explore the number of users
length(unique(edx$userId))

##Now, let us explore the number of movies rated per user
edx %>% group_by(userId) %>%
  summarize(count = n()) %>%
  arrange(desc(count))%>%
  head()

#Now we can do the same for users, we can expect that not many users would rate a lot of movies (e.g. >500)
edx%>% group_by(userId)%>%
  summarize(count=n())%>%
  ggplot(aes(count))+
  geom_histogram(color="white")+
  scale_x_log10()+
  xlab("Number of ratings")+
  ylab("Number of users")+
  theme_economist()

#Now, let us check the number of genres availables
length(unique(edx$genres))

#Now, let us check the number of movies per genre
edx %>% group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(desc(count))%>%
  head()

#Now, we are going to explore the dates of the movies to see a 14 years period approximately
tibble('Initial Date'=date(as_datetime(min(edx$timestamp),origin="1970-01-01")),
       "Final Date"=date(as_datetime(max(edx$timestamp),origin="1970-01-01")))%>%
  mutate(Period=duration(max(edx$timestamp)-min(edx$timestamp)))

#Let us explore now the distribucion of ratings vs years
edx%>% mutate(year=year(as_datetime(timestamp,origin="1970-01-01")))%>%
  ggplot(aes(x=year))+
  geom_histogram()+
  theme_economist()

#Going on, let us explore how the rating califications is  
edx %>% group_by(rating) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

#Now we can plot this to see the distribution
edx %>% group_by(rating) %>% 
  ggplot(aes(x=rating)) + 
  geom_histogram()+
  theme_economist()

#Data preparation for creation of the model==========================
#First, create the data partition into train and test sets
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
train_set <- edx[-test_index,]
temp <- edx[test_index,]

# Make sure userId and movieId in test set are also in train set
test_set <- temp %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Add rows removed from test set back into train set
removed <- anti_join(temp, test_set)
train_set <- rbind(train_set, removed)

rm(test_index, temp, removed)

#Since we are not using all variables, we are going to select only some of them
train_set <- train_set %>% select(userId, movieId, rating, title)
test_set  <- test_set  %>% select(userId, movieId, rating, title)

#Train and validation of the model===========================
#Calculate the average rating for all movies
avg_rating<- mean(train_set$rating)
avg_rating

#The error of this first approach
naive_error<- RMSE(test_set$rating, avg_rating)
naive_error

#The error compared with the goal of the project
results <- tibble(Method = "Goal of the project", RMSE = 0.86490)
results <- bind_rows(results,
                     tibble(Method = "Just the average prediction",
                            RMSE = naive_error))
results #Still very far from the result we want to get

#Movie effect (b_i)
#The effect (or bias) of each movie
movie_effect <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - avg_rating)) 
head(movie_effect)

#Distribution of the movie effect
movie_effect %>% qplot(b_i, geom ="histogram", 
                       bins = 10, data = ., 
                       color = I("black"))+
  ylab("Count")+
  xlab("Movie effect")+
  theme_economist()

#Predict the rating with the movie effect
y_hat_bi <- avg_rating + test_set %>% 
  left_join(movie_effect, by = "movieId") %>% 
  .$b_i
head(y_hat_bi)

#Compare the result with the goal of the project
results<- bind_rows(results,
                    tibble(Method = "Avg + Movie Effect (bi)",
                           RMSE= RMSE(test_set$rating, y_hat_bi)))
results #RMSE when using avg as well as movie effect

#User effect (bu)
user_effect <- train_set %>% 
  left_join(movie_effect, by = 'movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - avg_rating - b_i))
head(user_effect)

#Distribution of the user effect
user_effect %>% qplot(b_u, geom ="histogram", 
                      bins = 10, data = ., 
                      color = I("black"))+
  ylab("Count")+
  xlab("Movie effect")+
  theme_economist()

#Prediction including user effect as well
y_hat_bi_bu <- test_set %>% 
  left_join(movie_effect, by='movieId') %>%
  left_join(user_effect, by='userId') %>%
  mutate(pred = avg_rating + b_i + b_u) %>%
  .$pred
RMSE(test_set$rating, y_hat_bi_bu)

#The result compared with the goal of the project
results<- bind_rows(results,
                    tibble(Method = "Avg + Movie Effect (bi) + User Effect (bu)",
                           RMSE = RMSE(test_set$rating, y_hat_bi_bu)))
#RMSE when using avg as well as movie effect and user effect
results

#Regularization function for the linear model that includes average rating, movie effect and user effect
regularization <- function(lambda, train_set, test_set){
  
  # Avg Rating (mu)
  avg_rating <- mean(train_set$rating)
  
  # Movie effect (bi)
  movie_effect <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - avg_rating)/(n()+lambda))
  
  # User effect (bu)  
  user_effect <- train_set %>% 
    left_join(movie_effect, by="movieId") %>%
    filter(!is.na(b_i)) %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - avg_rating)/(n()+lambda))
  
  # Prediction: mu + bi + bu  
  predicted_ratings <- test_set %>% 
    left_join(movie_effect, by = "movieId") %>%
    left_join(user_effect, by = "userId") %>%
    filter(!is.na(b_i), !is.na(b_u)) %>%
    mutate(pred = avg_rating + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test_set$rating))
}

#Since lambda is a tunning parameter, we are going to try with several values
lambdas<-seq(0:10)

#Testing lambda with the `edx` dataset
rmses<-sapply(lambdas,
              regularization,
              train_set=train_set,
              test_set=test_set)

#Visualization of which value of lambda give us the minimun RMSE
tibble(Lambda = lambdas, RMSE = rmses) %>%
  ggplot(aes(x = Lambda, y = RMSE))+
  geom_point()+
  geom_line()+
  theme_economist()

#Selecting the value of lambda that give us the minimun RMSE
lambda<- lambdas[which.min(rmses)]

#Compare the result of the regularization with the goal of the project
results <- bind_rows(results, 
                     tibble(Method = "Regularized bi and bu", 
                            RMSE = regularization(lambda,train_set, test_set)))
results                    


#Final evaluation of Linear Model regularized with the `validation` dataset
avg_edx<- mean(edx$rating)

#Movie effect (b_i)
movie_effect_edx<- edx%>%
  group_by(movieId)%>%
  summarize(b_i=sum(rating-avg_edx)/(n()+lambda))

#User effect(b_u)
user_effect_edx<- edx%>%
  left_join(movie_effect_edx, by="movieId")%>%
  group_by(userId)%>%
  summarize(b_u=sum(rating-avg_edx-b_i)/(n()+lambda))

#Prediction
y_hat_edx<- validation%>%
  left_join(movie_effect_edx, by="movieId")%>%
  left_join(user_effect_edx, by="userId")%>%
  mutate(pred=avg_edx + b_i + b_u)%>%
  pull(pred)

#Final result
options(digits = 5)
final_result<- as.data.frame(bind_rows(results[1,],
                                       tibble(Method= "Final Validation",
                                              RMSE=RMSE(y_hat_edx, validation$rating))))
final_result
