## Nearest Neighbours

## Distance

library(tidyverse)
library(dslabs)
if(!exists("mnist")) mnist <- read_mnist()
set.seed(0, sample.kind = "Rounding") # if using R 3.6 or later
# we will look at a random sample of 2s and 7s
ind <- which(mnist$train$labels %in% c(2,7)) %>% sample(500)
#the predictors are in x and the labels in y
x <- mnist$train$images[ind,]
y <- mnist$train$labels[ind]

# when talking about distance with more than one predictor - in this case we
# have many hundreds - we must remember that we are using an abstract space
# which has more than three dimensions. 

# as an example, we will look at the first three observations
y[1:3]
# we will save the vector of predictors for each one as these objects
x_1 <- x[1,]
x_2 <- x[2,]
x_3 <- x[3,]

# the first two are 7s and the last is a 2, so we expect the distance to be smaller
# between 1 and 2 to be smaller
sqrt(sum((x_1 - x_2)^2))
sqrt(sum((x_1 - x_3)^2))
sqrt(sum((x_2 - x_3)^2))

# we can also compute this distance using matrix algebra
sqrt(crossprod(x_1 - x_2))
sqrt(crossprod(x_1 - x_3))
sqrt(crossprod(x_2 - x_3))

# or use dist() to compute all distances at once
d <- dist(x)
class(d)
as.matrix(d)[1:3,1:3]

# we can visualise the distances using this
image(as.matrix(d))

# to make it meaningful, we order by label
image(as.matrix(d)[order(y), order(y)])

# we can compute the distance between predictors:
d <- dist(t(x))
dim(as.matrix(d))

# we can also pick a random pixel - the 492nd pixel - and see which ones it 
# is close to mathematically. unsurprisingly, these are the physically close ones
d_492 <- as.matrix(d)[492,]
image(1:28, 1:28, matrix(d_492, 28, 28))

## Knn

# Knn is similar to bin smoothing, but is easier to adapt to multiple dimensions
# we look at the k nearest points to a given point - this is the neighbourhood
library(tidyverse)
library(dslabs)
data("mnist_27")
mnist_27$test %>% ggplot(aes(x_1, x_2, color = y)) + geom_point()
# the neighbourhood provides us with an estimate of the conditional probability
# of a given point. larger neighbourhoods give us a smoother estimate, and smaller
# ones give a more flexible but less stable estimate

# we will compare knn to logistic regression
library(caret)
fit_glm <- glm(y~x_1+x_2, data=mnist_27$train, family="binomial")
p_hat_logistic <- predict(fit_glm, mnist_27$test)
y_hat_logistic <- factor(ifelse(p_hat_logistic > 0.5, 7, 2))
confusionMatrix(data = y_hat_logistic, reference = mnist_27$test$y)$overall[1]

# we first fit a knn model - the formula is (outcome ~ predictor1 + predictor2)
# if we want to use all the predictors, we would include a dot
knn_fit <- knn3(y ~ ., data = mnist_27$train)
# we can also call it using a matrix of predictors and a vector of outcomes
x <- as.matrix(mnist_27$train[,2:3])
y <- mnist_27$train$y
knn_fit <- knn3(x, y)
# the formula approach is quicker, but using larger data sets, we will want to
# use the second approach

# we also need to specify the number of neighbours to include - default 5
knn_fit <- knn3(y ~ ., data = mnist_27$train, k=5)

# the data set is balanced (between 2s and 7s) and we care equally about spec/
# sens, we will use accuracy to quantify performance
# the predict function produces a prob for each class, or the outcome that maximises prob
y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]

## Overtraining and Oversmoothing

# how did we improve over logistic regression even though we only have 2 predictors?
y_hat_knn <- predict(knn_fit, mnist_27$train, type = "class") 
confusionMatrix(data = y_hat_knn, reference = mnist_27$train$y)$overall["Accuracy"]
y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")  
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]
# there is evidence of over-training here - we do better on the training set
# than on the test set

# overtraining is worst at k=1, where the estimate is only a single point
# this will result in almost perfect accuracy in training
knn_fit_1 <- knn3(y ~ ., data = mnist_27$train, k = 1)
y_hat_knn_1 <- predict(knn_fit_1, mnist_27$train, type = "class")
confusionMatrix(data=y_hat_knn_1, reference=mnist_27$train$y)$overall[["Accuracy"]]
# however, in reality, this results in poor accuracy
y_hat_knn_1 <- predict(knn_fit_1, mnist_27$test, type = "class")
confusionMatrix(data=y_hat_knn_1, reference=mnist_27$test$y)$overall[["Accuracy"]]

# so what if we do a huge k?
knn_fit_401 <- knn3(y ~ ., data = mnist_27$train, k = 401)
y_hat_knn_401 <- predict(knn_fit_401, mnist_27$test, type = "class")
confusionMatrix(data=y_hat_knn_401, reference=mnist_27$test$y)$overall["Accuracy"]
# the accuracy is not good - similar to logistic regression

# so how do we pick k? we can try the model with many values of k using map_df
# we will test it against both the training and test data - in practice, we should
# only use it on the test data
ks <- seq(3, 251, 2)
library(purrr)
accuracy <- map_df(ks, function(k){
  fit <- knn3(y ~ ., data = mnist_27$train, k = k)
  
  y_hat <- predict(fit, mnist_27$train, type = "class")
  cm_train <- confusionMatrix(data = y_hat, reference = mnist_27$train$y)
  train_error <- cm_train$overall["Accuracy"]
  
  y_hat <- predict(fit, mnist_27$test, type = "class")
  cm_test <- confusionMatrix(data = y_hat, reference = mnist_27$test$y)
  test_error <- cm_test$overall["Accuracy"]
  
  tibble(train = train_error, test = test_error)
  
})

#pick the k that maximizes accuracy using the estimates built on the test data
ks[which.max(accuracy$test)]
max(accuracy$test)
# this is a much more accurate model than logistic regression
# however, we cannot just select a k based on the test set.

## Assessment

# Set the seed to 1, then use the caret package to partition the dslabs heights
# data into a training and test set of equal size. 

library(caret)
library(dslabs)
set.seed(1)
y <- heights$sex
x <- heights$height
test_index <- createDataPartition(y, times=1, p=0.5, list=FALSE)
test_set <- heights[test_index,]
train_set <- heights[-test_index,]

# Use the sapply() function to 
# perform knn with k values of seq(1, 101, 3) and calculate F1 scores with the 
# F_meas() function using the default value of the relevant argument.

ks <- seq(1, 101, 3)
F_1 <- sapply(ks, function(k){
  fit <- knn3(sex ~ height, data = train_set, k = k)
  y_hat <- predict(fit, test_set, type = "class") %>% 
    factor(levels = levels(train_set$sex))
  F_meas(data = y_hat, reference = test_set$sex)
})
plot(ks, F_1)
max(F_1)
ks[which.max(F_1)]

#####
# 
# First, set the seed to 1 and split the data into training and test sets with 
# p = 0.5. Then, report the accuracy you obtain from predicting tissue type using
# KNN with k = seq(1, 11, 2) using sapply() or map_df(). Note: use the 
# createDataPartition() function outside of sapply() or map_df().

set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
test_index <- createDataPartition(y, list = FALSE)
sapply(seq(1, 11, 2), function(k){
  fit <- knn3(x[-test_index,], y[-test_index], k = k)
  y_hat <- predict(fit, newdata = data.frame(x=x[test_index,]),
                   type = "class")
  mean(y_hat == y[test_index])
})


## K-Fold Cross-Validation

# we can estimate the true error in a model by generating many small random samples
# that are not used for training, and use those to estimate the error
# this is called k-fold cross-validation

# we split the training data into k sets, and use each set one by one to estimate
# the MSE. by doing this one for each set, we can obtain an optimised set of parameters
# which minimise the MSE. this optimisation is based on the training set, so we 
# get a final estimate of the MSE using the test data.
# larger values of k will give better results, but are also slower to compute
# therefore, values of 5-10 are popular
# we can also just take random sets from the data, and not worry if they overlap or not
# this is often called the bootstrap approach

## Assessment

library(tidyverse)
library(caret)

# set up sample
set.seed(1996, sample.kind="Rounding") #if you are using R 3.6 or later
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()
x_subset <- x[ ,sample(p, 100)]

# perform cross-validation
fit <- train(x_subset, y, method = "glm")
fit$results

# Now, instead of using a random selection of predictors, we are going to search
# for those that are most predictive of the outcome. We can do this by comparing
# the values for the  group to those in the  group, for each predictor, using a
# t-test. You can perform this step like this:
install.packages("BiocManager")
BiocManager::install("genefilter")
library(genefilter)
tt <- colttests(x, y)

# generate a vector of p-values
pvals <- tt$p.value

pvals
# Create an index ind with the column numbers of the predictors that were 
# "statistically significantly" associated with y. Use a p-value cutoff of 
# 0.01 to define "statistically significantly."
ind <- pvals<0.01
sum(ind)

# Now re-run the cross-validation after redefinining x_subset to be the subset 
# of x defined by the columns showing "statistically significant" association with y.
set.seed(1996)
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()
tt <- colttests(x, y)
x_subset <- x[ ,tt$p.value <= 0.01]
fit <- train(x_subset, y, method = "glm")
fit$results

# Re-run the cross-validation again, but this time using kNN. Try out the following
# grid k = seq(101, 301, 25) of tuning parameters. Make a plot of the resulting accuracies.
fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)

# Use the train() function with kNN to select the best k for predicting tissue from
# gene expression on the tissue_gene_expression dataset from dslabs. Try k = seq(1,7,2)
# for tuning parameters. For this question, do not split the data into test and train 
# sets (understand this can lead to overfitting, but ignore this for now).
library(dslabs)
data("tissue_gene_expression")
dim(tissue_gene_expression$x)
table(tissue_gene_expression$y)

## Bootstrap

library(ggplot2)
library(dplyr)
# suppose the income distribution of a population is as follows:
n <- 10^6
income <- 10^(rnorm(n, log10(45000), log10(3)))
qplot(log10(income), bins = 30, color = I("black"))
# the median income is about 45,000
m <- median(income)
m

# suppose we don't have access to the population, but want to estimate the median
# we take a sample of 250 and use its median as a guess
set.seed(1, sample.kind="Rounding")
N <- 250
X <- sample(income, N)
M<- median(X)
M

# we can check the distribution of the sample median using a monte carlo sim
library(gridExtra)
B <- 1000
M <- replicate(B, {
  X <- sample(income, N)
  median(X)
})
p1 <- qplot(M, bins = 30, color = I("black"))
p2 <- qplot(sample = scale(M)) + geom_abline()
grid.arrange(p1, p2, ncol = 2)
mean(M)
sd(M)
# the sample median, SD and distribution roughly match those of the pop

# the problem is, we won't know the distribution - and the CLT does not apply
# to medians, only averages. 
# bootstrapping lets us approximate a monte carlo sim without the population
# we treat the sample as a population and sample, with replacement, random sets
# of the same size. this gives us a "bootstrap sample", where the distribution
# approximates the distribution of our actual statistic. 

# this code lets us bootstrap from a sample:
B <- 1000
M_star <- replicate(B, {
  X_star <- sample(X, N, replace = TRUE)
  median(X_star)
})

# we can check how close it is to the actual distribution
tibble(monte_carlo = sort(M), bootstrap = sort(M_star)) %>%
  qplot(monte_carlo, bootstrap, data = .) + 
  geom_abline()
# and the quantiles are close too
quantile(M, c(0.05, 0.95))
quantile(M_star, c(0.05, 0.95))

# the CLT would give us this confidence interval:
median(X) + 1.96 * sd(X) / sqrt(N) * c(-1, 1)
mean(M) + 1.96 * sd(M) * c(-1,1)

# if we know a distribution is normal, we can bootstrap the mean and SE
# and form a confidence interval that way instead:
mean(M_star) + 1.96 * sd(M_star) * c(-1, 1)

## Assessment

# The createResample() function can be used to create bootstrap samples. 
# For example, we can create the indexes for 10 bootstrap samples for the mnist_27 
# dataset like this:
library(dslabs)
library(caret)
data(mnist_27)
set.seed(1995, sample.kind="Rounding")
indexes <- createResample(mnist_27$train$y, 10)

# How many times do 3, 4, and 7 appear in the first resampled index?
sum(indexes$Resample01==3)
sum(indexes$Resample01==4)
sum(indexes$Resample01==7)

# We see that some numbers appear more than once and others appear no times. This 
# has to be this way for each dataset to be independent. 
# What is the total number of times that 3 appears in all of the resampled indexes?
sum(indexes$Resample01==3)
sum(indexes$Resample02==3)
sum(indexes$Resample03==3)
sum(indexes$Resample04==3)
sum(indexes$Resample05==3)
sum(indexes$Resample06==3)
sum(indexes$Resample07==3)
sum(indexes$Resample08==3)
sum(indexes$Resample09==3)
sum(indexes$Resample10==3)

# A random dataset can be generated with the following code:
y <- rnorm(100, 0, 1)

# Estimate the 75th quantile, which we know is qnorm(0.75), with the sample 
# quantile: quantile(y, 0.75).
# Now, set the seed to 1 and perform a Monte Carlo simulation with 10,000 
# repetitions, generating the random dataset and estimating the 75th quantile each
# time. What is the expected value and standard error of the 75th quantile?
set.seed(1, sample.kind="Rounding")
B <- 10000
q_75 <- replicate(B, {
  y <- rnorm(100, 0, 1)
  quantile(y, 0.75)
})
mean(q_75)
sd(q_75)

# In practice, we can't run a Monte Carlo simulation. Use the sample:
set.seed(1, sample.kind = "Rounding")
y <- rnorm(100, 0, 1)
# Set the seed to 1 again after generating y and use 10 bootstrap samples
# to estimate the expected value and standard error of the 75th quantile.
set.seed(1, sample.kind="Rounding") # if R 3.6 or later
indexes <- createResample(y, 10000)
q_75_star <- sapply(indexes, function(ind){
	y_star <- y[ind]
	quantile(y_star, 0.75)
})
mean(q_75_star)
sd(q_75_star)

## Generative Models

# methods that model the joint distribution of y and predictors x are known as 
# generative models

## Naive Bayes

# Generating train and test set
library("caret")
library(dslabs)
library(dplyr)
data("heights")
y <- heights$height
set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

# in this case, we know that the normal distribution is appropriate for the 
# data (heights), so the naive bayes approach is suitable

# the sample average and sd are therefore good approximations
params <- train_set %>%
  group_by(sex) %>%
  summarize(avg = mean(height), sd = sd(height))
params

# we can also estimate the prevalence
pi <- train_set %>% summarize(pi=mean(sex=="Female")) %>% pull(pi)
pi

# we can use these estimates of avg and sd to get the actual rule
x <- test_set$height
f0 <- dnorm(x, params$avg[2], params$sd[2])
f1 <- dnorm(x, params$avg[1], params$sd[1])
p_hat_bayes <- f1*pi / (f1*pi + f0*(1 - pi))

## Controlling Prevalence

# the naive bayes approach includes a parameter which can account for prevalence

# if the cond prob has to be greater than 0.5 to predict females, we lose accuracy
# because females are less prevalent in the sample and our sensitivity is low
y_hat_bayes <- ifelse(p_hat_bayes > 0.5, "Female", "Male")
sensitivity(data = factor(y_hat_bayes), reference = factor(test_set$sex))

# Computing specificity
specificity(data = factor(y_hat_bayes), reference = factor(test_set$sex))

# for this sample it makes sense - we have more men than women - but if we wanted
# to use this on the whole population, we would need to change pi hat accordingly
p_hat_bayes_unbiased <- f1 * 0.5 / (f1 * 0.5 + f0 * (1 - 0.5))
y_hat_bayes_unbiased <- ifelse(p_hat_bayes_unbiased > 0.5, "Female", "Male")
sensitivity(data = factor(y_hat_bayes_unbiased), reference = factor(test_set$sex))
specificity(data = factor(y_hat_bayes_unbiased), reference = factor(test_set$sex))

# this new rule also gives us a more sensible cutoff, somewhere in the middle of
# the heights
qplot(x, p_hat_bayes_unbiased, geom = "line") +
  geom_hline(yintercept = 0.5, lty = 2) +
  geom_vline(xintercept = 67, lty = 2)

# QDA

# quadratic discriminant analysis (QDA) is where we assume that the conditional
# probs of the predictors are multivariate normal

# we will use the 2s and 7s example for this
data("mnist_27")

# we have two predictors, so we assume their conditional distribution is
# bivariate normal - we therefore need to estimate avgs, SDs and a correlation
# between the 7s and the 2s
params <- mnist_27$train %>%
  group_by(y) %>%
  summarize(avg_1 = mean(x_1), avg_2 = mean(x_2),
            sd_1 = sd(x_1), sd_2 = sd(x_2),
            r = cor(x_1, x_2))
params

# Contour plots can also show us what the estimated normal densities look like
mnist_27$train %>% mutate(y = factor(y)) %>%
  ggplot(aes(x_1, x_2, fill = y, color = y)) +
  geom_point(show.legend = FALSE) +
  stat_ellipse(type="norm", lwd = 1.5)

# caret can fit the model and obtain predictors - what is the cond prob of y=1
# given x1 and x2?
library(caret)
train_qda <- train(y ~., method = "qda", data = mnist_27$train)
# Obtain predictors and accuracy
y_hat <- predict(train_qda, mnist_27$test)
confusionMatrix(data = y_hat, reference = mnist_27$test$y)$overall["Accuracy"]
# this model has relatively good accuracy
# however, it is not as good as the earlier kernel method - this is because
# the boundary must be a quadratic

# Draw separate plots for 2s and 7s
mnist_27$train %>% mutate(y = factor(y)) %>%
  ggplot(aes(x_1, x_2, fill = y, color = y)) +
  geom_point(show.legend = FALSE) +
  stat_ellipse(type="norm") +
  facet_wrap(~y)
# here we see that the bivariate normal holds true for the 2s, but for the 7s, 
# there appears to be a curvature. 
# QDA begins to struggle as you add more predictors, because of all the correlations

# More than Three Classes

# imagine we want to do this but with more than three classes
# we will use the same data, but now with 1s, 2s and 7s
# all of the below code is just generating the data
if(!exists("mnist"))mnist <- read_mnist()
set.seed(3456)
index_127 <- sample(which(mnist$train$labels %in% c(1,2,7)), 2000)
y <- mnist$train$labels[index_127] 
x <- mnist$train$images[index_127,]
index_train <- createDataPartition(y, p=0.8, list = FALSE)
# get the quadrants
# temporary object to help figure out the quadrants
row_column <- expand.grid(row=1:28, col=1:28)
upper_left_ind <- which(row_column$col <= 14 & row_column$row <= 14)
lower_right_ind <- which(row_column$col > 14 & row_column$row > 14)
# binarize the values. Above 200 is ink, below is no ink
x <- x > 200 
# cbind proportion of pixels in upper right quadrant and proportion of pixels in lower right quadrant
x <- cbind(rowSums(x[ ,upper_left_ind])/rowSums(x),
           rowSums(x[ ,lower_right_ind])/rowSums(x)) 
train_set <- data.frame(y = factor(y[index_train]),
                        x_1 = x[index_train,1],
                        x_2 = x[index_train,2])
test_set <- data.frame(y = factor(y[-index_train]),
                       x_1 = x[-index_train,1],
                       x_2 = x[-index_train,2])
# we can visualise the data here
train_set %>%  ggplot(aes(x_1, x_2, color=y)) + geom_point()
train_qda <- train(y ~ ., method = "qda", data = train_set)

# we estimate 3 cond probs, but they all have to add up to 1:
predict(train_qda, test_set, type = "prob") %>% head()
# we predict the one with the highest prob
predict(train_qda, test_set) %>% head()
# our predictors are three classes

# the confusion matrix is now a 3x3 table because we can make two errors with
# the ones, two errors with the twos, etc. 
confusionMatrix(predict(train_qda, test_set), test_set$y)$table
confusionMatrix(predict(train_qda, test_set), test_set$y)$overall["Accuracy"]

# for LDA, boundary regions must be lines, so the accuracy is worse
train_lda <- train(y ~ ., method = "lda", data = train_set)
confusionMatrix(predict(train_lda, test_set), test_set$y)$overall["Accuracy"]
train_knn <- train(y ~ ., method = "knn", tuneGrid = data.frame(k = seq(15, 51, 2)),
                   data = train_set)
confusionMatrix(predict(train_knn, test_set), test_set$y)$overall["Accuracy"]
train_set %>% mutate(y = factor(y)) %>% ggplot(aes(x_1, x_2, fill = y, color=y)) + geom_point(show.legend = FALSE) + stat_ellipse(type="norm")
# the 1s are not bivariate normal, so QDA and especially LDA do not work here

## Assessment

# Create a dataset of samples from just cerebellum and hippocampus, two parts of
# the brain, and a predictor matrix with 10 randomly selected columns using the following code:
library(dslabs)
library(caret)
library(tidyverse)
data("tissue_gene_expression")
set.seed(1993, sample.kind="Rounding")
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

# Use the train() function to estimate the accuracy of LDA. 
#  Report the accuracy from the train() results (donot make predictions).
fit_lda <- train(x, y, method = "lda")
fit_lda$results["Accuracy"]

# Look at the fitted model by looking at the finalModel component of the result of train().
# Notice there is a component called means that includes the estimated means of both
# distributions. Plot the mean vectors against each other and determine which 
# predictors (genes) appear to be driving the algorithm.

t(fit_lda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()

# Repeat the exercise in Q1 with QDA.

library(dslabs)      
library(caret)
data("tissue_gene_expression")
set.seed(1993, sample.kind="Rounding")
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

fit_qda <- train(x, y, method = "qda")
fit_qda$results["Accuracy"]

t(fit_qda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()


# One thing we saw in the previous plots is that the values of the predictors 
# correlate in both groups: some predictors are low in both groups and others 
# high in both groups. The mean value of each predictor found in colMeans(x) is 
# not informative or useful for prediction and often for purposes of interpretation,
# it is useful to center or scale each column. This can be achieved with the 
# preProcess argument in train(). Re-run LDA with preProcess = "center". Note 
# accuracy does not change, but it is now easier to identify the predictors that
# differ more between groups than based on the plot made in Q2.
fit_lda <- train(x, y, method = "lda", preProcess = "center")
fit_lda$results["Accuracy"]
t(fit_lda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(predictor_name, hippocampus)) +
  geom_point() +
  coord_flip()

# Now we are going to increase the complexity of the challenge slightly. Repeat 
# the LDA analysis from Q5 but using all tissue types. Use the following code to 
# create your dataset:
library(dslabs)      
library(caret)
data("tissue_gene_expression")
set.seed(1993, sample.kind="Rounding")
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]

fit_lda <- train(x, y, method = "lda", preProcess = "center")
fit_lda$results["Accuracy"]

