## Trees Motivation

# we not meant to use lda and qda with datasets that feature many predictors
# because the number of correlations between factors increase exponentially.
# similarly, kernel approaches struggle because of the increase in dimensions.

## Classification and Regression Trees

# we will look at a new dataset which shows the fatty acid composition of olives
library(tidyverse)
library(dslabs)
data("olive")
olive %>% as_tibble()
# we will try ot predict the region by fatty acid composition
table(olive$region)
# we will remove the area column because we do not use it as a predictor
olive <- select(olive, -area)

# we can do this using KNN using caret
library(caret)
fit <- train(region ~ .,  method = "knn", 
             tuneGrid = data.frame(k = seq(1, 15, 2)), 
             data = olive)
ggplot(fit)
# we get an accuracy of 0.97 - pretty good

# however, we can do better
# Plot distribution of each predictor stratified by region
olive %>% gather(fatty_acid, percentage, -region) %>%
  ggplot(aes(region, percentage, fill = region)) +
  geom_boxplot() +
  facet_wrap(~fatty_acid, scales = "free") +
  theme(axis.text.x = element_blank())
# here we see that one of the fatty acids is only present in southern italy, 
# and another can separate northern italy from sardinia - so we should be able to
# build a perfect predictor

# plot values for eicosenoic and linoleic to show this - we can even separate them
# perfectly by eye. specifically, the prediction rule is:
# if predictor 1 is larger than 0.065, predict southern italy
# if not, and p2 is lower than 10.54, predict northern italy, else predict sardinia
p <- olive %>% 
  ggplot(aes(eicosenoic, linoleic, color = region)) + 
  geom_point()
p + geom_vline(xintercept = 0.065, lty = 2) + 
  geom_segment(x = -0.2, y = 10.54, xend = 0.065, yend = 10.54, color = "black", lty = 2)

# Regression trees partition the predictor space in order to predict y.
# we will once again use the poll data as an example
data("polls_2008")
qplot(day, margin, data = polls_2008)
# we will try to predict the cond exp - f(x) - the expected value of y given x - 
# with y, the poll margin, and x, the day
# the general idea is to build a decision tree, and at the end of each node, 
# we have a different predict Y-hat
# we first partition the data into non-overlapping regions (R1, R2, etc)
# predictions are then made based on the averages in the region
library(rpart)
fit <- rpart(margin ~ ., data = polls_2008)

# this can tell us where the splits were made visually
plot(fit, margin = 0.1)
text(fit, cex = 0.75)
# we can also plot the onto the time graph itself
polls_2008 %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")
# again, we can overtrain - if we split the model into so many partitions that 
# every point has its own, there is no error adn thus no predictive power
# the complexity parameter (CP) dictates how much the residual sum of squares
# must improve to justify a new partition
# there is also a minsplit argument, the minimum number of splits

# change parameters
fit <- rpart(margin ~ ., data = polls_2008, control = rpart.control(cp = 0, minsplit = 2))
polls_2008 %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")
# this robs us of all predictive power by overfitting

# prune the tree by removing branches which do not give enough cp
pruned_fit <- prune(fit, cp = 0.01)

# we can use cross-validation to choose the cp threshold
library(caret)
train_rpart <- train(margin ~ ., method = "rpart", tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)), data = polls_2008)
ggplot(train_rpart)

# access the final model and plot it
plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex = 0.75)
polls_2008 %>% 
  mutate(y_hat = predict(train_rpart)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")

## Classification (Decision) Trees

# when the outcome is categorical, we refer to it as a "classification" tree.
# instead of taking the average at the end of each node, we predict with 
# the class that has the majority vote in each node


# here is how a decision tree works on the 2s and 7s data - here is the accuracy
train_rpart <- train(y ~ .,
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)),
                     data = mnist_27$train)
plot(train_rpart)

# the overall accuracy is better than logistic, but not as good as kernel
confusionMatrix(predict(train_rpart, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]

## Random Forests

# to improve prediction performance and reduce instability, we can average many
# decision trees via "bagging" or bootstrap aggregation - a random forest. 

# we can apply random forest to the 2008 data
library(randomForest)
fit <- randomForest(margin~., data = polls_2008) 
plot(fit)
# here, we have plotted the error versus how many trees are used in the model

# this plot is the final result for the 2008 data
polls_2008 %>%
  mutate(y_hat = predict(fit, newdata = polls_2008)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_line(aes(day, y_hat), col="red")
# it is quite smooth, and not a step function like the individual trees
# this is because of the averaging process

# for the 2s and 7s data, we end up with accuracy of 0.79
library(randomForest)
train_rf <- randomForest(y ~ ., data=mnist_27$train)
confusionMatrix(predict(train_rf, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]

# here we use Rborist, a slightly faster random forest function, and get better accuracy
train_rf_2 <- train(y ~ .,
                    method = "Rborist",
                    tuneGrid = data.frame(predFixed = 2, minNode = c(3, 50)),
                    data = mnist_27$train)
confusionMatrix(predict(train_rf_2, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]

## Assessment
# Create a simple dataset where the outcome grows 0.75 units on average for 
# every increase in a predictor, using this code:
library(rpart)
library(dplyr)
library(ggplot2)
n <- 1000
sigma <- 0.25
set.seed(1, sample.kind = "Rounding")
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)

# fitting and plotting a tree
fit <- rpart(y ~ ., data = dat) 
plot(fit)
text(fit, cex = 0.75)

# scatter plot of x versus y based on fit
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col=2)

# Now run Random Forests instead of a regression tree using randomForest() 
# from the randomForest package, and remake the scatterplot with the prediction line.
library(randomForest)
fit <- randomForest(y ~ x, data = dat)
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")

# Use the plot() function to see if the Random Forest from Q4 has converged or if we need more trees.
plot(fit)

# It seems that the default values for the Random Forest result in an estimate
# that is too flexible (unsmooth). Re-run the Random Forest but this time with a 
# node size of 50 and a maximum of 25 nodes. Remake the plot.
library(randomForest)
fit <- randomForest(y ~ x, data = dat, nodesize = 50, maxnodes = 25)
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")

## Caret
library(tidyverse)
library(dslabs)
data("mnist_27")

# the caret package provides a consistent interface and syntax for different
# machine learning packages in R

library(caret)
# the train fucntion lets us train different algorithms using the same syntax
train_glm <- train(y ~ ., method = "glm", data = mnist_27$train)
train_knn <- train(y ~ ., method = "knn", data = mnist_27$train)

# we can also extratc predictions from these models easily
y_hat_glm <- predict(train_glm, mnist_27$test, type = "raw")
y_hat_knn <- predict(train_knn, mnist_27$test, type = "raw")

# the same is true for confusion matrices
confusionMatrix(y_hat_glm, mnist_27$test$y)$overall[["Accuracy"]]
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall[["Accuracy"]]

## Tuning Parameters with Caret

# the train function automatically uses cross-validation to decide between
# a few different parameters
# we can use these functions to quickly see which parameters are optimised (k)
getModelInfo("knn")
modelLookup("knn")

# so if we run the function train with default values, we can view the results 
# of the cross-validation using ggplot - the highlight argument shows us the 
# optimum parameter. by default, it checks 5 7 and 9 for KNN
train_knn <- train(y ~ ., method = "knn", data = mnist_27$train)
ggplot(train_knn, highlight = TRUE)
# we can change the default settings using traingrid, and this plot
# shows us the k that maximises accuracy
train_knn <- train(y ~ ., method = "knn", 
                   data = mnist_27$train,
                   tuneGrid = data.frame(k = seq(9, 67, 2)))
ggplot(train_knn, highlight = TRUE)
# the best model here was using the training set - we did not use the test set

train_knn$bestTune
train_knn$finalModel
# we can test the accuracy of our model on the test set
confusionMatrix(predict(train_knn, mnist_27$test, type = "raw"),
                mnist_27$test$y)$overall["Accuracy"]

# we can also change the way we use cross-validation using trainControl
# here, we 10 validation samples using 10% of obs each
control <- trainControl(method = "cv", number = 10, p = .9)
train_knn_cv <- train(y ~ ., method = "knn", 
                      data = mnist_27$train,
                      tuneGrid = data.frame(k = seq(9, 71, 2)),
                      trControl = control)
ggplot(train_knn_cv, highlight = TRUE)
# here, the accuracy estimates are more variable

# we can also view the standard deviations for each of the validation sets
train_knn$results %>% 
  ggplot(aes(x = k, y = Accuracy)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(x = k, 
                    ymin = Accuracy - AccuracySD,
                    ymax = Accuracy + AccuracySD))

# the best-fitting model is a fairly good representation of the true cond prob
# however, it is wiggly because knn did not use a smoothing kernel
plot_cond_prob <- function(p_hat=NULL){
  tmp <- mnist_27$true_p
  if(!is.null(p_hat)){
    tmp <- mutate(tmp, p=p_hat)
  }
  tmp %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
    geom_raster(show.legend = FALSE) +
    scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
    stat_contour(breaks=c(0.5),color="black")
}
plot_cond_prob(predict(train_knn, mnist_27$true_p, type = "prob")[,2])

# to make it smoother, we can use gamLoess
install.packages("gam")
# we can see that we have two parameters to optimise
modelLookup("gamLoess")

# we will keep degree fixed at 1, but we will try to optimise Span
# however, for caret, we still need to include the argument for Degree
grid <- expand.grid(span = seq(0.15, 0.65, len = 10), degree = 1)

# we then use the default cross-validation parameters
train_loess <- train(y ~ ., 
                     method = "gamLoess",
                     tuneGrid=grid,
                     data = mnist_27$train)
# check for the best-performing model
ggplot(train_loess, highlight = TRUE)
# and view the accuracy
confusionMatrix(data = predict(train_loess, mnist_27$test), 
                reference = mnist_27$test$y)$overall["Accuracy"]
# we can see that this helped smooth the boundary
p1 <- plot_cond_prob(predict(train_loess, mnist_27$true_p, type = "prob")[,2])
p1

## Assessment

# Load the rpart package and then use the caret::train() function with method = 
#   "rpart" to fit a classification tree to the tissue_gene_expression dataset. 
# Try out cp values of seq(0, 0.1, 0.01). Plot the accuracies to report the 
# results of the best model. Set the seed to 1991.
library(caret)
library(dslabs)
set.seed(1991, sample.kind = "Rounding") # if using R 3.6 or later
data("tissue_gene_expression")

fit1 <- with(tissue_gene_expression, 
             train(x, y, method = "rpart",
                   tuneGrid = data.frame(cp = seq(0, 0.1, 0.01))))

ggplot(fit1)  

# Note that there are only 6 placentas in the dataset. By default, rpart requires
# 20 observations before splitting a node. That means that it is difficult to 
# have a node in which placentas are the majority. Rerun the analysis you did 
# in the exercise in Q1, but this time, allow rpart to split any node by using 
# the argument control = rpart.control(minsplit = 0). Look at the confusion 
# matrix again to determine whether the accuracy increases. Again, set the seed to 1991.
library(caret)
library(broom)
library(dslabs)
set.seed(1991, sample.kind = "Rounding") # if using R 3.6 or later
data(tissue_gene_expression)
fit_rpart <- with(tissue_gene_expression, 
                  train(x, y, method = "rpart",
                        tuneGrid = data.frame(cp = seq(0, 0.10, 0.01)),
                        control = rpart.control(minsplit = 0)))
ggplot(fit_rpart)
confusionMatrix(fit_rpart)

# Plot the tree from this fit
plot(fit_rpart$finalModel)
text(fit_rpart$finalModel)
