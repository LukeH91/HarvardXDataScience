## MNIST

library(dslabs)
# the MNIST data set contains a training set and a test set
mnist <- read_mnist()
names(mnist)

# each of these components include a matrix with features in the columns
dim(mnist$train$images)

# and a vector with classes as integers
class(mnist$train$labels)
table(mnist$train$labels)

# to make the code run faster, we will sample 10k rows from training, 1k rows from test
set.seed(123)
index <- sample(nrow(mnist$train$images), 10000)
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])
index <- sample(nrow(mnist$test$images), 1000)
x_test <- mnist$test$images[index,]
y_test <- factor(mnist$test$labels[index])

## Preprocessing

# many of the variables have very low or no variability - we can see this
# using the below
library(matrixStats)
sds <- colSds(x)
qplot(sds, bins = 256, color = I("black"))

# this is because there are pats of the image which contain very few dark pixels
# (those around the edges), and we can use nearZeroVar to remove them
library(caret)
nzv <- nearZeroVar(x)
image(matrix(1:784 %in% nzv, 28, 28))

# we are keeping this many columns:
col_index <- setdiff(1:ncol(x), nzv)
length(col_index)

## Model Fitting

# we will now implement KNN and random forests on the data
# we must first add column names to the feature matrices for caret - we will
# use the column numbers as names
colnames(x) <- 1:ncol(mnist$train$images)
colnames(x_test) <- colnames(x)

# we will now optimise the number of neighbours, but because this is a big data
# set, we will use cross-validation in caret to improve speed
# this code will find the model that maximises accuracy
control <- trainControl(method = "cv", number = 10, p = .9)
train_knn <- train(x[,col_index], y,
                   method = "knn", 
                   tuneGrid = data.frame(k = c(1,3,5,7)),
                   trControl = control)
ggplot(train_knn)

# if we wanted to test this on a smaller bit of code first, we could do this:
n <- 1000
b <- 2
index <- sample(nrow(x), n)
control <- trainControl(method = "cv", number = b, p = .9)
train_knn <- train(x[index ,col_index], y[index],
                   method = "knn",
                   tuneGrid = data.frame(k = c(3,5,7)),
                   trControl = control)

# once our algorithm is optimised, we can fit the entire dataset
fit_knn <- knn3(x[ ,col_index], y,  k = 3)
y_hat_knn <- predict(fit_knn,
                     x_test[, col_index],
                     type="class")
cm <- confusionMatrix(y_hat_knn, factor(y_test))
cm$overall["Accuracy"]
# our overall accuracy is very good

# our sens/spec output from the CF shows us that 8s are the hardest to detect (sens)
# and the most common incorrectly-predicted digit is 7 (spec)
cm$byClass[,1:2]

# can we do better with random forests?
# each forest takes hundreds of trees, so computation time is a bigger concern
# we will therefore use the rborist package, and we will reduce the number
# of trees as well as only using 5-fold cross-validation. we will also take
#a  random sample of observations from each tree using nSamp
library(Rborist)
control <- trainControl(method="cv", number = 5, p = 0.8)
grid <- expand.grid(minNode = c(1,5) , predFixed = c(10, 15, 25, 35, 50))
train_rf <-  train(x[, col_index], y,
                   method = "Rborist",
                   nTree = 50,
                   trControl = control,
                   tuneGrid = grid,
                   nSamp = 5000)
# ggplot shows us the final plot, and we can view the best parameters
ggplot(train_rf)
train_rf$bestTune

# now we can optimise the final tree
fit_rf <- Rborist(x[, col_index], y,
                  nTree = 1000,
                  minNode = train_rf$bestTune$minNode,
                  predFixed = train_rf$bestTune$predFixed)

y_hat_rf <- factor(levels(y)[predict(fit_rf, x_test[ ,col_index])$yPred])
# we can now view the accuracy using the confusion tree - it is an improvement
cm <- confusionMatrix(y_hat_rf, y_test)
cm$overall["Accuracy"]

# if we look at some random draws, we see that our predictions are good
rafalib::mypar(3,4)
for(i in 1:12){
  image(matrix(x_test[i,], 28, 28)[, 28:1], 
        main = paste("Our prediction:", y_hat_rf[i]),
        xaxt="n", yaxt="n")
}

## Variable Importance

# variable importance helps us interpret random forests
# rborist does not suppose this, so we will use randomForest instead
library(randomForest)
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])
rf <- randomForest(x, y,  ntree = 50)
# we can now see the importance of each feature
imp <- importance(rf)
imp
# many features have no importance - these are the ones around the edges

# we will look at where each feature is plotted in the image it came from
image(matrix(imp, 28, 28))
# we can see that the important features are generally in the centre

# we want to look at cases where we made a prediction that was wrong in KNN
p_max <- predict(fit_knn, x_test[,col_index])
p_max <- apply(p_max, 1, max)
ind  <- which(y_hat_knn != y_test)
ind <- ind[order(p_max[ind], decreasing = TRUE)]
rafalib::mypar(3,4)
for(i in ind[1:12]){
  image(matrix(x_test[i,], 28, 28)[, 28:1],
        main = paste0("Pr(",y_hat_knn[i],")=",round(p_max[i], 2),
                      " but is a ",y_test[i]),
        xaxt="n", yaxt="n")
}

# here we do the same for random forest
p_max <- predict(fit_rf, x_test[,col_index])$census  
p_max <- p_max / rowSums(p_max)
p_max <- apply(p_max, 1, max)
ind  <- which(y_hat_rf != y_test)
ind <- ind[order(p_max[ind], decreasing = TRUE)]
rafalib::mypar(3,4)
for(i in ind[1:12]){
  image(matrix(x_test[i,], 28, 28)[, 28:1], 
        main = paste0("Pr(",y_hat_rf[i],")=",round(p_max[i], 2),
                      " but is a ",y_test[i]),
        xaxt="n", yaxt="n")
}

## Ensembles

# an ensemble is combining multiple machine learning algs into a single one
# for example, we can calculate new class probabilities from the average
# probabilities of random forest and KNN
p_rf <- predict(fit_rf, x_test[,col_index])$census
p_rf <- p_rf / rowSums(p_rf)
p_knn <- predict(fit_knn, x_test[,col_index])
p <- (p_rf + p_knn)/2
y_pred <- factor(apply(p, 1, which.max)-1)
confusionMatrix(y_pred, y_test)
# now the accuracy is improved over both models

## Assessment

library(dslabs)
library(caret)
library(dslabs)
library(tidyverse)
set.seed(1, sample.kind = "Rounding")
data("mnist_27")

# Use the training set to build a model with several of the models available from
# the caret package. We will test out 10 of the most common machine learning 
# models in this exercise:
models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", 
            "multinom", "qda", "rf", "adabag")

# Apply all of these models using train() with all the default parameters.
fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models

## Recommendation Systems

library(dslabs)
library(tidyverse)
data("movielens")
# in the movielens dataset, each row is a rating given by one user to one film
head(movielens)

# we can see the number of unique users and movies here:
movielens %>%
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))
# yet, if we multiply these numbers together, we get a number much smaller than the
# number of rows - this means that each user did not rate every film. 
# thus, each row is a user and each column is a film, with each row having many empty cells

# here we see 7 random users and their ratings for movies, with N/A for those they did not rate
keep <- movielens %>%
  dplyr::count(movieId) %>%
  top_n(4) %>%
  pull(movieId)
tab <- movielens %>%
  filter(userId %in% c(13:20)) %>% 
  filter(movieId %in% keep) %>% 
  select(userId, title, rating) %>% 
  spread(title, rating)
tab %>% knitr::kable()
# we can think of the task of a recommendation system as filling in these N/As

# those are the top 4 films of all time, but really, the matrix is incredibly sparse:
users <- sample(unique(movielens$userId), 100)
rafalib::mypar()
movielens %>% filter(userId %in% users) %>% 
  select(userId, movieId, rating) %>%
  mutate(rating = 1) %>%
  spread(movieId, rating) %>% select(sample(ncol(.), 100)) %>% 
  as.matrix() %>% t(.) %>%
  image(1:100, 1:100,. , xlab="Movies", ylab="Users")
abline(h=0:100+0.5, v=0:100+0.5, col = "grey")

# the ML challenge here is hard because each outcome y has a different set of predictors
# if we are trying to predict how a user will rate a movie, in principle, we can use
# all other ratings given by other users to the same film as predictors - but everyone
# rates different films, and we might also be able to use ratings from similar films
# or from similar users. thus, essentially, the entire matrix can predict a given cell.

# some movies get rated more than others
movielens %>% 
  dplyr::count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Movies")

# some users give more ratings than others
movielens %>%
  dplyr::count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() +
  ggtitle("Users")

# we will first create a test set using caret
library(caret)
set.seed(755)
test_index <- createDataPartition(y = movielens$rating, times = 1,
                                  p = 0.2, list = FALSE)
train_set <- movielens[-test_index,]
test_set <- movielens[test_index,]

# we use semi-join to ensure that users and movies are not in both sets
test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# to compare models, we must create a loss function based on the residual
# mean squared error
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}


## Building the Recommendation System

# we will start with the simplest possible model - the same rating for every movie
# the rating will be the mean rating from all films
mu_hat <- mean(train_set$rating)
mu_hat

# this approach gives us the following RMSE:
naive_rmse <- RMSE(test_set$rating, mu_hat)
naive_rmse
# this RMSE is quite big, but using any number other than the mean makes it worse
predictions <- rep(2.5, nrow(test_set))
RMSE(test_set$rating, predictions)

# we will create a table to compare the RMSE of each method
rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)

# some films are just rated higher than others - so we should use the average
# rating for each film (b_i) to improve our model
# since every film gets it own factor, this runs very slowly
fit <- lm(rating ~ as.factor(userId), data = movielens)
# however, there is a faster way, involving calculating the difference between
# each film and the mean rating across all films
mu <- mean(train_set$rating) 
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))
# unsurprisingly, there is a lot of variance in the ratings given to films
# because the deducted overall avg is 1.5, 3.5 represents a perfect 5 star rating
movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"))

# how much does this model improve our predictions?
predicted_ratings <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i
model_1_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",
                                     RMSE = model_1_rmse ))
rmse_results %>% knitr::kable()

# we will now incorporate the average rating per user
# a hitogram of average ratings for users who have rated over 100 movies is
# show below - there is substantial variability between users
train_set %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "black")

# again, we could fit this using a linear model, but it is slow:
lm(rating ~ as.factor(movieId) + as.factor(userId))
# instead, we can calculate the difference between each user mean and the overall user mean:
user_avgs <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))
# now we can use it for predictions
predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

# and compare it to the other RMSEs
model_2_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = model_2_rmse ))
rmse_results %>% knitr::kable()

## Assessment
library(tidyverse)
library(lubridate)
library(dslabs)
data("movielens")

# Compute the number of ratings for each movie and then plot it against the year
# the movie came out. Use the square root transformation on the counts. What
# year has the highest median number of ratings?

movielens %>% group_by(movieId) %>%
  summarize(n = n(), year = as.character(first(year))) %>%
  qplot(year, n, data = ., geom = "boxplot") +
  coord_trans(y = "sqrt") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# We see that, on average, movies that came out after 1993 get more ratings. We
# also see that with newer movies, starting in 1993, the number of ratings
# decreases with year: the more recent a movie is, the less time users have had
# to rate it.

# Among movies that came out in 1993 or later, what are the 25 movies with the
# most ratings per year, and what is the average rating of each of the top 25
# movies?

# What is the average rating for the movie The Shawshank Redemption?

movies <- movielens %>% 
  select(movieId, year, title, genres, rating)

z <- movies %>% group_by(title)
z <- data.frame(x[order(x$title, decreasing=FALSE),])
head(z)

p <- z %>% filter(title == "Shawshank Redemption, The")
mean(p$rating)
# [1] 4.487138

# What is the average number of ratings per year for the movie Forrest Gump?

z <- movies %>% group_by(title)
ny <- 0
nr <- z %>% filter(title == "Forrest Gump") # 341


fg <- data.frame(year = 1:22, num_ratings = 1:22)
x <- z %>% filter(title == "Forrest Gump" & year == 1994)
nrow(x) # 341 ratings between 1994 and 2016. Ave # ratings 14.826
for(i in 1:1000) {
  print(x$year)
}

yc <- 1
nr <- 0
for(i in 1994:2016) {
  x <- z %>% filter(title == "Forrest Gump" & year == i)
  nr <- nr + nrow(x)
  yc <- yc + 1
}
nr # 341
yc # 24
nr / yc # 14.20833

# From the table constructed in Q2, we can see that the most frequently rated
# movies tend to have above average ratings. This is not surprising: more people
# watch popular movies. To confirm this, stratify the post-1993 movies by
# ratings per year and compute their average ratings. Make a plot of average
# rating versus ratings per year and show an estimate of the trend.

post_1993 <- movies %>% filter(year > 1993)
nrow(post_1993) # 60253

df <- data.frame(year = 1:23, rpy = 1:23, avg_rpy = 1:23)

counter <- 1
tot_ratings <- 0
for(i in 1994:2016) {
  x <- post_1993 %>% filter(year == i)
  df[counter,1] <- i
  df[counter,2] <- nrow(x)
  df[counter,3] <- mean(x$rating)
  counter <- counter + 1
}
df %>% ggplot(aes(x = avg_rpy, y = rpy)) + scale_y_log10() + geom_point()
df %>% ggplot(aes(x = rpy, y = avg_rpy)) + scale_y_log10() + geom_point() +
  geom_smooth(method="lm")

# Suppose you are doing a predictive analysis in which you need to fill in the
# missing ratings with some value.

# Given your observations in the exercise in Q3, which of the following
# strategies would be most appropriate?

# Answer: Fill in the missing values with a lower value than the average rating
# across all movies. Because a lack of ratings is associated with lower ratings,
# it would be most appropriate to fill in the missing value with a lower value
# than the average. You should try out different values to fill in the missing
# value and evaluate prediction in a test set.

#
# Q5
#

# The movielens dataset also includes a time stamp. This variable represents the
# time and data in which the rating was provided. The units are seconds since
# January 1, 1970. Create a new column date with the date.

library(lubridate)
data("movielens")
movielens %>% 
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))

# Which code correctly creates this new column?
movielens <- mutate(movielens, date = as.date(timestamp))     # Not correct
movielens <- mutate(movielens, date = as_datetime(timestamp)) # Correct but mis-typed
movielens <- mutate(movielens, date = as.data(timestamp))     # Does not exist
movielens <- mutate(movielens, date = timestamp)              # Just dups the final field

head(movielens)

#
# Q6
#

# Compute the average rating for each week and plot this average against day.
# Hint: use the round_date function before you group_by. What type of trend do
# you observe?

movielens %>% mutate(date = round_date(as_datetime(timestamp), unit = "week")) %>%
  group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth()

# We can see that there is some evidence of a time effect in the plot, but there
# is not a strong effect of time.

#
# Q7
#

# Consider again the plot you generated in Q6. If we define du,i as the day for
# user's u rating of movie i, which of the following models is most appropriate?

# Answer: Yu,i = u + bi + bu + f(du,i) + Eu,i where f(du,i) is a smooth function

#
# Q8
#

# The movielens data also has a genres column. This column includes every genre
# that applies to the movie. Some movies fall under several genres. Define a
# category as whatever combination appears in this column. Keep only categories
# with more than 1,000 ratings. Then compute the average and standard error for
# each category. Plot these as error bar plots.

# Which genre has the lowest average rating? Enter the name of the genre exactly
# as reported in the plot, including capitalization and punctuation.

new_category <- unique(movielens$genres) # 901
len_category <- length(new_category)

df <- data.frame(movielens[order(movielens$genres, decreasing=FALSE),])
df <- df %>% select(-movieId, -title, -year, genres, -userId, rating, -timestamp)
#str(df)
df <- mutate(df, avg_rating = 0, sd_rating = 0, nrows = 0)
#str(df)
df <- df[1:901,]
#str(df)

counter <- 1
for(i in 1:len_category) {
  x <- movielens %>% filter(genres == new_category[i])
  df[counter,1] <- new_category[i]
  df[counter,3] <- mean(x$rating, na.rm=TRUE)
  df[counter,4] <- sd(x$rating, na.rm=TRUE)
  df[counter,5] <- nrow(x)
  counter <- counter + 1
}

nrow(df) # 901
head(df,50)

final_df <- df %>% filter(nrows > 1000)
nrow(final_df)
final_df[order(final_df$avg_rating, decreasing=FALSE),]

# Instructor answer
movielens %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 1000) %>% 
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## Regularization

# all this just sets the data up
library(dslabs)
library(tidyverse)
library(caret)
data("movielens")
set.seed(755)
test_index <- createDataPartition(y = movielens$rating, times = 1,
                                  p = 0.2, list = FALSE)
train_set <- movielens[-test_index,]
test_set <- movielens[test_index,]
test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}
mu_hat <- mean(train_set$rating)
naive_rmse <- RMSE(test_set$rating, mu_hat)
rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)
mu <- mean(train_set$rating) 
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))
predicted_ratings <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i
model_1_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",
                                     RMSE = model_1_rmse ))
user_avgs <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))
predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred
model_2_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = model_2_rmse ))

# despite the larghe amount of movie-to-movie variance, we onyl gained 5% when
# adding this to the model. why? here are some of the biggest errors we made:
test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  mutate(residual = rating - (mu + b_i)) %>%
  arrange(desc(abs(residual))) %>% 
  select(title,  residual) %>% slice(1:10) %>% knitr::kable()
# these are mostly relatively obscure films with large predictions

# let's look at the best and worst films based on estimates of the movie effect
movie_titles <- movielens %>% 
  select(movieId, title) %>%
  distinct()
# best 10 films:
movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i) %>% 
  slice(1:10) %>%  
  knitr::kable()
# worst 10 films:
movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i) %>% 
  slice(1:10) %>%  
  knitr::kable()
# all of these are quite obscure. how often were they rated?
train_set %>% dplyr::count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()
train_set %>% dplyr::count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()
# so all of these were rated by very few users - in many cases just 1

# regularisation allows us to adjust for the fact that some films have very
# few observations - these should be factored less strongly into our final model
# we do this by defining lambda - this is a penalty term which shrinks when 
# the amount of observations is high. a larger lambda causes more shrinkage. 
lambda <- 3
mu <- mean(train_set$rating)
movie_reg_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n()) 
# here we can see the impact - when n is small, the values shrink more towards 0
data_frame(original = movie_avgs$b_i, 
           regularlized = movie_reg_avgs$b_i, 
           n = movie_reg_avgs$n_i) %>%
  ggplot(aes(original, regularlized, size=sqrt(n))) + 
  geom_point(shape=1, alpha=0.5)

# now we will look again at the top and bottom 10 films using regularisation
train_set %>%
  dplyr::count(movieId) %>% 
  left_join(movie_reg_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()

train_set %>%
  dplyr::count(movieId) %>% 
  left_join(movie_reg_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()
# suddenly, all of these make a lot more sense. 

predicted_ratings <- test_set %>% 
  left_join(movie_reg_avgs, by='movieId') %>%
  mutate(pred = mu + b_i) %>%
  .$pred
model_3_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie Effect Model",  
                                     RMSE = model_3_rmse ))
rmse_results %>% knitr::kable()
# we can see that this approach improves our results

# since lambda is a tuning parameter, we can use cross-validation to choose it
lambdas <- seq(0, 10, 0.25)
mu <- mean(train_set$rating)
just_the_sum <- train_set %>% 
  group_by(movieId) %>% 
  summarize(s = sum(rating - mu), n_i = n())
rmses <- sapply(lambdas, function(l){
  predicted_ratings <- test_set %>% 
    left_join(just_the_sum, by='movieId') %>% 
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    .$pred
  return(RMSE(predicted_ratings, test_set$rating))
})
qplot(lambdas, rmses)  
lambdas[which.min(rmses)]
# this shows us why we picked 3 as lambda

# we can also do this for a model including user effect
lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  return(RMSE(predicted_ratings, test_set$rating))
})

qplot(lambdas, rmses)  

# for the full model, the optimal lambda is 3.75
lambda <- lambdas[which.min(rmses)]
lambda

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie + User Effect Model",  
                                     RMSE = min(rmses)))
rmse_results %>% knitr::kable()

## Assessment

# The exercises in Q1-Q8 work with a simulated dataset for 1000 schools. 
# This pre-exercise setup walks you through the code needed to simulate the dataset.
# 
# If you have not done so already since the Titanic Exercises, please 
# restart R or reset the number of digits that are printed with options(digits=7).
# 
# An education expert is advocating for smaller schools. The expert bases this
# recommendation on the fact that among the best performing schools, many are small
# schools. Let's simulate a dataset for 1000 schools. First, let's simulate the 
# number of students in each school, using the following code:

set.seed(1986, sample.kind="Rounding") # if using R 3.6 or later
n <- round(2^rnorm(1000, 8, 1))

# Now let's assign a true quality for each school that is completely independent from
# size. This is the parameter we want to estimate in our analysis. The true quality 
# can be assigned using the following code:

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:1000),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))

# We can see the top 10 schools using this code: 
  
schools %>% top_n(10, quality) %>% arrange(desc(quality))

# Now let's have the students in the school take a test. There is random variability
# in test taking, so we will simulate the test scores as normally distributed with 
# the average determined by the school quality with a standard deviation of 30 
# percentage points. This code will simulate the test scores:
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
mu <- round(80 + 2*rt(1000, 5))

scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))

# What are the top schools based on the average score? Show just the ID, size, and the average score.

schools %>% top_n(10, score) %>% arrange(desc(score)) %>% select(id, size, score)

# Compare the median school size to the median school size of the top 10 schools based on the score.

median(schools$size)
med <- schools %>% top_n(10, score) %>% select(id, size, score)
median(med$size)

# According to this analysis, it appears that small schools produce better test 
# scores than large schools. Four out of the top 10 schools have 100 or fewer 
# students. But how can this be? We constructed the simulation so that quality 
# and size were independent. Repeat the exercise for the worst 10 schools.
# What is the median school size of the bottom 10 schools based on the score?

med <- schools %>% top_n(-10, score) %>% arrange(desc(score)) %>% select(id, size, score)
median(med$size)

# Plot the average score versus school size to see what's going on. 

schools %>% top_n(10, score) %>% arrange(desc(score)) %>% select(id, size, score)
head(schools)
x <- schools %>% group_by(size) %>% arrange(desc(size)) %>%
  mutate(avg_score = mean(score)) %>% 
  select(id, size, avg_score) %>%
  ggplot(aes(size, avg_score)) +
  geom_point() + scale_x_log10()

schools %>% ggplot(aes(size, score)) +
  geom_point(alpha = 0.5) +
  geom_point(data = filter(schools, rank<=10), col = 2) 

# Let's use regularization to pick the best schools. Remember regularization 
# shrinks deviations from the average towards 0. To apply regularization here,
# we first need to define the overall average for all schools, using the following code:
overall <- mean(sapply(scores, mean))
# Then, we need to define, for each school, how it deviates from that average.
# Write code that estimates the score above the average for each school but
# dividing by n + a instead of n, with n the schools size and a a regularization
# parameters. Try a = 25.

alpha <- 25
score_reg <- sapply(scores, function(x)  overall + sum(x-overall)/(length(x)+alpha))
schools %>% mutate(score_reg = score_reg) %>%
  top_n(10, score_reg) %>% arrange(desc(score_reg))

# Notice that this improves things a bit. The number of small schools that are
# not highly ranked is now lower. Is there a better ? Find the  that minimizes
# the RMSE = .
#
# What value of  gives the minimum RMSE?

alphas <- seq(10,250)
rmse <- sapply(alphas, function(alpha){
  score_reg <- sapply(scores, function(x) overall+sum(x-overall)/(length(x)+alpha))
  mean((score_reg - schools$quality)^2)
})
plot(alphas, rmse)
alphas[which.min(rmse)]  

# Rank the schools based on the average obtained with the best . Note that no
# small school is incorrectly included.
#
# What is the ID of the top school now? What is the regularized average score of
# the 10th school now?

alpha <- alphas[which.min(rmse)]
score_reg <- sapply(scores, function(x)  overall + sum(x-overall)/(length(x)+alpha))
schools %>% mutate(score_reg = score_reg) %>%
  top_n(10, score_reg) %>% arrange(desc(score_reg))

# A common mistake made when using regularization is shrinking values towards 0
# that are not centered around 0. For example, if we don't subtract the overall
# average before shrinking, we actually obtain a very similar result. Confirm
# this by re-running the code from the exercise in Q6 but without removing the
# overall mean.
#
# What value of  gives the minimum RMSE here?

alphas <- seq(10,250)
rmse <- sapply(alphas, function(alpha){
  score_reg <- sapply(scores, function(x) sum(x)/(length(x)+alpha))
  mean((score_reg - schools$quality)^2)
})
plot(alphas, rmse)
alphas[which.min(rmse)]  

## Matrix Factorisation

library(ggplot2)
library(tidyverse)
library(dslabs)
data("movielens")

#matrix factorisation is related to SVD and PCA
# to account for similarity between users and films, we want to study the residuals
# to do this, we wil convert the data into a matrix, such that every user gets a row
# and every film gets a column
# we will only consider a small subset of users and movies
train_small <- movielens %>% 
  group_by(movieId) %>%
  filter(n() >= 50 | movieId == 3252) %>% ungroup() %>% #3252 is Scent of a Woman used in example
  group_by(userId) %>%
  filter(n() >= 50) %>% ungroup()
y <- train_small %>% 
  select(userId, movieId, rating) %>%
  spread(movieId, rating) %>%
  as.matrix()
rownames(y)<- y[,1]
y <- y[,-1]
colnames(y) <- with(movie_titles, title[match(colnames(y), movieId)])
# we then convert these to residuals by removing the column and row means
y <- sweep(y, 1, rowMeans(y, na.rm=TRUE))
y <- sweep(y, 2, colMeans(y, na.rm=TRUE))
# if our model explains all the signal, the rest should just be noise.
# therefore, all of these should be independe of one another.
m_1 <- "Godfather, The"
m_2 <- "Godfather: Part II, The"
qplot(y[,m_1], y[,m_2], xlab = m_1, ylab = m_2)
m_1 <- "Godfather, The"
m_3 <- "Goodfellas"
qplot(y[ ,m_1], y[,m_3], xlab = m_1, ylab = m_3)
m_4 <- "You've Got Mail" 
m_5 <- "Sleepless in Seattle" 
qplot(y[ ,m_4], y[,m_5], xlab = m_4, ylab = m_5)
cor(y[, c(m_1, m_2, m_3, m_4, m_5)], use="pairwise.complete") %>% 
  knitr::kable()
# code doesn't work for some reason, but they are all correlated
# users who enjoyed one enjoyed the other
# in other words, there is structure in the data that is not accounted for
# we can use matrix factorisation to find it
# matrix factorisation effectively collapses large amounts of data into a few facotrs
set.seed(1)
options(digits = 2)
Q <- matrix(c(1 , 1, 1, -1, -1), ncol=1)
rownames(Q) <- c(m_1, m_2, m_3, m_4, m_5)
P <- matrix(rep(c(2,0,-2), c(3,5,4)), ncol=1)
rownames(P) <- 1:nrow(P)
X <- jitter(P%*%t(Q))
X %>% knitr::kable(align = "c")
cor(X)
t(Q) %>% knitr::kable(aling="c")
P
# for example, people who like godfather 1 and goodfells enjoy gangster films
# and people who enjoy you've got mail and sleepless in seattle enjoy rom coms
# these films are correlated with one another, but not with the other genres
set.seed(1)
options(digits = 2)
m_6 <- "Scent of a Woman"
Q <- cbind(c(1 , 1, 1, -1, -1, -1), 
           c(1 , 1, -1, -1, -1, 1))
rownames(Q) <- c(m_1, m_2, m_3, m_4, m_5, m_6)
P <- cbind(rep(c(2,0,-2), c(3,5,4)), 
           c(-1,1,1,0,0,1,1,1,0,-1,-1,-1))/2
rownames(P) <- 1:nrow(X)
X <- jitter(P%*%t(Q), factor=1)
X %>% knitr::kable(align = "c")
cor(X)
t(Q) %>% knitr::kable(align="c")
P
# now we have to add another factor - people who enjoy al pacino films
six_movies <- c(m_1, m_2, m_3, m_4, m_5, m_6)
tmp <- y[,six_movies]
cor(tmp, use="pairwise.complete")

## SVD and PCA

# we can't always define these factors ourselves - SVD and PCA can give them to us
# singular value decomposition and principal components analysis allow us to write 
# the residuals out in such a way that the variability of the terms is decreasing
# and they are not correlated with one another
# we must first make all the NAs zero
y[is.na(y)] <- 0
y <- sweep(y, 1, rowMeans(y))
pca <- prcomp(y)
# the vectors are called principal components and are stored in this matrix
dim(pca$rotation)
# the p vectors, user effects, are in this matrix
dim(pca$x)
# the PCA function returns a component which has the variability of each of the 
# principal components and it can be plotted
plot(pca$sdev)
# even with just a few of these, we can see that we have explained a lot of the data
# with just 50, we already explain half the variability
var_explained <- cumsum(pca$sdev^2/sum(pca$sdev^2))
plot(var_explained)
# we can already see some meaningful clusters just from looking
library(ggrepel)
pcs <- data.frame(pca$rotation, name = colnames(y))
pcs %>%  ggplot(aes(PC1, PC2)) + geom_point() + 
  geom_text_repel(aes(PC1, PC2, label=name),
                  data = filter(pcs, 
                                PC1 < -0.1 | PC1 > 0.1 | PC2 < -0.075 | PC2 > 0.1))
# these are all critically acclaimed
pcs %>% select(name, PC1) %>% arrange(PC1) %>% slice(1:10)
# blocbusters are on the other end of the same component
pcs %>% select(name, PC1) %>% arrange(desc(PC1)) %>% slice(1:10)
# this is all artsy independent films
pcs %>% select(name, PC2) %>% arrange(PC2) %>% slice(1:10)
# this is all nerd films
pcs %>% select(name, PC2) %>% arrange(desc(PC2)) %>% slice(1:10)
# the PCA has already found structure in the data - but fitting this is very 
# complicated, and you may want to use recommernderlab package to try

