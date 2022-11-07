## Notation

# in machine learning, the outcome is what we want to predict, and the features
# are what we use to predict it. we therefore want to build a model which takes
# feature input, and predicts the unknown outcome

# to achieve this, we train the model on a data set where we know the outcome,
# and then use that to make predictions when we don't know the outcome

# the outcome will be denoted with Y, and the features (predictors, covariates)
# will be X1 and so on. 

# categorical prediction problems (classifications) are those where Y can be any
# one of class K there can be any number of categories, but it can also just 
# be true/false. they can also be continuous (predictions), where Y is a number. 

## Caret Packages, Training and Test Sets, and Accuracy

# the caret package has many useful functions for machine learning
library(caret)

# to illustrate this, we will try to predict sex by height
library(dslabs)
data(heights)

# in this example, we have only one predictor, so:
y <- heights$sex
x <- heights$height

# this is a categorical outcome - a classification problem. however, male and 
# female heights do not have a large effect size, so we do not expect a lot
# of accuracy - but can we do better than guessing?

# to evaluate this, we split the data into "training" and "test" sets
# we do this randomly using the createDataPartition function, which creates
# a set of indexes. times is how many sets to return, p is the proportion to use,
# and list is whether you want it as a list or not
set.seed(2)
test_index <- createDataPartition(y, times=1, p=0.5, list=FALSE)
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]

# we will now develop an algorithm using the training data, then test it
# overall accuracy will be assessed by the amount of true/false guesses

# our first algorithm will simply guess the outcome and ignore the predictor
y_hat <- sample(c("Male", "Female"),
                length(test_index), replace=TRUE)

# the proportion of correct guesses can be computed like this:
mean(y_hat==test_set$sex)
# because we are guessing, our accuracy is around 50%

# we can do better = males are slightly taller than females
library(dplyr)
heights %>% group_by(sex) %>% summarize(mean(height), sd(height))

# so what if we guess that someone is male if they're within 2 SDs of male mean?
y_hat <- ifelse(x>62, "Male", "Female") %>% factor(levels=levels(test_set$sex))
mean(y==y_hat)
# our accuracy is now much better

# but do other cutoffs work better than 62 inches?
# examine the accuracy of 10 cutoffs
library(purrr)
cutoff <- seq(61, 70)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
})
# then we plot the accuracy by cutoff point
data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 

# we can determine which cutoff gives the best accuracy
max(accuracy)
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

# now we test the trained model on the actual test data
y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)
mean(y_hat == test_set$sex)

## Confusion Matrix

# a confusion matrix shows us all combinations of predictions and actual values
table(predicted =y_hat, actual = test_set$sex)

# so we have a problem - our accuracy is high for men but low for women
test_set %>% 
  mutate(y_hat = y_hat) %>%
  group_by(sex) %>% 
  summarize(accuracy = mean(y_hat == sex))

# however, overall accuracy is high because women are a small part of the data set

# a confusion matrix can be used to show us sensitivity: the ability to correctly
# predict a positive outcome, and specificity, correctly predicting a negative outcome
# we call the cells in the confusion matrix true positives, false positives, false 
# negatives and true negatives. therefore:

# sensitivity = TP/(TP+FN) - true positive rate (TPR) - 
# proportion of positives which are called positive

# specificity = TN/(TN+FP) - true negative rate (TNR) - 
# proportion of negatives which are called negatives

# precision = TP/(TP+FP) -  positive predictive value (PPV) - 
# proportion of called positives which are truly positive
# unlike the other two, this one depends on prevalence

# confusionMatrix() calculates all of these for us, we just define what a positive is
# the first level is considered the positive outcome
# in our example, female is the first level because it is first alphabetically
confusionMatrix(data = y_hat, reference = test_set$sex)

# as we can see, we have high accuracy despite low sensitivity, because women are
# a small part of the sample - therefore, them being incorrectly called males barely 
# affects accuracy, whereas males being called females would impact it a lot

## Balanced Accuracy and F1 Score

# Balanced Accuracy is the average of specificity and sensitivity - since these are
# rates, it is better to compute the harmonic average of the two. this is the F1 score

# depending on the context, we might care more about one type of error than another:
# for plane safety, we want sensitivity over specificity - we want to capture all positives
# for a court case, we want specificity over sensitivity - we want to avoid false positives
# F1 can be adjusted to reflect their relative importance, with beta representing 
# how much more important sensitivity is. the F_meas function calculates it all for us.
# maximize F-score
cutoff <- seq(61, 70)
F_1 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(train_set$sex))
})
data.frame(cutoff, F_1) %>% 
  ggplot(aes(cutoff, F_1)) + 
  geom_point() + 
  geom_line()
# here we can see that F1 is mazimised at a cutoff of 66 inches
# 66 icnhes balances our specificity and sensitivity, as seen here:
confusionMatrix(data = y_hat, reference = test_set$sex)
# this is our first machine learning algorithm - it takes height as a predictor,
# and predicts female if you are 66 inches or shorter. 

## Prevalence Matters

# high sensitivity and specificity may not be a good thing if prevalence is close to 0 or 1
# if a doctor wants to find patients with a rare disease, a model with high sensitivity 
# would be likely to predict correctly. however, maximising true positives will also 
# increase false positives. sensitivity, in this case, matters - we want to make sure the 
# predicted positives are true positives. 

## ROC and Precision-Recall Curves

# we looked at a model which guesses earlier, but it was guessing male or female
# 50:50 despite them not being 50:50 in the data. if we instead guess male 90%
# of the time, we can improve accuracy at the cost of sensitivity
p <- 0.9
n <- length(test_index)
y_hat <- sample(c("Male", "Female"), n, replace = TRUE, prob=c(p, 1-p)) %>% 
  factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)

# we can compare methods by plotting both sens and spec in an ROC curve
# this is the one for guessing
probs <- seq(0, 1, length.out = 10)
guessing <- map_df(probs, function(p){
  y_hat <- 
    sample(c("Male", "Female"), n, replace = TRUE, prob=c(p, 1-p)) %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Guessing",
       FPR = 1 - specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})
guessing %>% qplot(FPR, TPR, data =., xlab = "1 - Specificity", ylab = "Sensitivity")

# this is the one for the height-based approach
cutoffs <- c(50, seq(60, 75), 80)
height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       FPR = 1-specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})

# plot both curves together - the height-based approach has better sens the entire way
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(FPR, TPR, color = method)) +
  geom_line() +
  geom_point() +
  xlab("1 - Specificity") +
  ylab("Sensitivity")

# we can also add the cutoffs to the points
library(ggrepel)
map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       cutoff = x, 
       FPR = 1-specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
}) %>%
  ggplot(aes(FPR, TPR, label = cutoff)) +
  geom_line() +
  geom_point() +
  geom_text_repel(nudge_x = 0.01, nudge_y = -0.01)

# however, neither of the measures plotted depend on prevalence
# in cases where prevalence matters, we could plot precision vs recall
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index), 
                  replace = TRUE, prob=c(p, 1-p)) %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Guess",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})
height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point()


# we can see that prevalence matters, because if we make males the positive
# case, the precision-recall plot changes too
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE, 
                  prob=c(p, 1-p)) %>% 
    factor(levels = c("Male", "Female"))
  list(method = "Guess",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})

height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Male", "Female"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point()


## Assessment
library(dslabs)
library(dplyr)
library(lubridate)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

# The type column of dat indicates whether students took classes in person ("inclass")
# or online ("online"). What proportion of the inclass group is female? What proportion
# of the online group is female?
class <- dat %>% filter(type=="inclass")
mean(class$sex=="Female")
online <- dat %>% filter(type=="online")
mean(online$sex=="Female")

# Use the type variable to predict sex. Assume that for each class type the students
# are either all male or all female, based on the most prevalent sex in each class 
# type you calculated in Q1.
ifelse(x == "inclass", "Female", "Male") %>% 
  factor(levels = levels(y)) -> y_hat 

mean(y_hat==y)

# Write a line of code using the table() function to show the confusion matrix between y_hat and y
table(y_hat, y)

# What is the sensitivity of this prediction? 
sensitivity(y_hat, y)

# What is the specificity of this prediction?
specificity(y_hat, y)

# What is the prevalence (% of females) in the dat dataset defined above? 
mean(dat$sex=="Female")

library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

# set.seed(2) # if using R 3.5 or earlier
set.seed(2, sample.kind="Rounding")
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

# Using only the train iris dataset, for each feature, perform a simple search 
# to find the cutoff that produces the highest accuracy, predicting virginica if 
# greater than the cutoff and versicolor otherwise.
f <- function(x){
  rv <- seq(min(x), max(x), by=0.1) #rv = ranged values
  sapply(rv,
         function(i){
           y_hat <- ifelse(x > i,'virginica','versicolor')
           mean(y_hat == train$Species)} #here we can find the accuracy 
  )}

predictions <- apply(train[,-5],MARGIN = 2, FUN = f)

sapply(predictions,max) 

# use the smart cutoff value from the training data to calculate overall accuracy
# in the test data. What is the overall accuracy?
predictions <- f(train[,3]) #f is previously created function
rv <- seq(min(train[,3]),max(train[,3]),by=0.1) #rv = ranged values
cutoffs <-rv[which(predictions==max(predictions))]

y_hat <- ifelse(test[,3]>cutoffs[1],'virginica','versicolor')
mean(y_hat==test$Species)

# Repeat the analysis in Q8 but this time using the test data instead of the training data.
predictions <- apply(test[,-5], MARGIN = 2, FUN = f) #f is the previously created function
sapply(predictions,max) 

# Optimize the the cutoffs for Petal.Length and Petal.Width separately in the train dataset
# by using the seq function with increments of 0.1. Then, report the overall accuracy when
# applied to the test dataset by creating a rule that predicts virginica if Petal.Length is
# greater than the length cutoff OR Petal.Width is greater than the width cutoff, and 
# versicolor otherwise.

petalLR <- seq(min(train$Petal.Length),max(train$Petal.Length),by=0.1) #PetalLR = Petal Length Range
petalWR <- seq(min(train$Petal.Width),max(train$Petal.Width),by=0.1) #PetalWR = Petal Width Range

length_predictions <- sapply(petalLR,function(i){
  y_hat <- ifelse(train$Petal.Length>i,'virginica','versicolor')
  mean(y_hat==train$Species)
})

length_cutoff <- petalLR[which.max(length_predictions)] # 4.7

width_predictions <- sapply(petalWR,function(i){
  y_hat <- ifelse(train$Petal.Width>i,'virginica','versicolor')
  mean(y_hat==train$Species)
})

width_cutoff <- petalWR[which.max(width_predictions)] # 1.5

y_hat <- ifelse(test$Petal.Length>length_cutoff | test$Petal.Width>width_cutoff,'virginica','versicolor')
mean(y_hat==test$Species)

## Conditional Probabilities

set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))
 
# What is the probability that a test is positive?
mean(test)

# What is the probability that an individual has the disease if the test is negative?
mean(disease[test==0])

# What is the probability that you have the disease if the test is positive?
mean(disease[test==1]==1) 

# If a patient's test is positive, by how many times does that increase their risk of having the disease?
mean(disease[test==1]==1)/mean(disease==1) 
