## Linear Regression for Prediction

# we can take an ML approach with LR - for example, using the height data
# we want to predict the son's height (Y) using the father's height (X)
library(tidyverse)
library(HistData)

# we first generate the height data 
galton_heights <- GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

# create testing and training sets
library(caret)
y <- galton_heights$son
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- galton_heights %>% slice(-test_index)
test_set <- galton_heights %>% slice(test_index)

# if we were simply guessing, then we'd guess the average height of sons
avg <- mean(train_set$son)
avg
# our squared loss can be shown using this:
mean((avg - test_set$son)^2)

# if a pair xy follows the bivariate normal distribution, then the conditional
# expectation is equivalent to the regression line. 
# fit linear regression model
fit <- lm(son ~ father, data = train_set)
fit$coef
# this provides a simple estimate of the conditional expectation:
# intercept + (father height*x)

# this does indeed provide an improvement over our guessing approach
y_hat <- fit$coef[1] + fit$coef[2]*test_set$father
mean((y_hat - test_set$son)^2)

## Predict Function

# the predict function is very useful for ML. it takes a fitted object such as
# an lm or glm and a data frame with the new predictors, and returns a prediction

# in our heights example, instead of writing out the formula, we can just do this:
y_hat <- predict(fit, test_set)
mean((y_hat - test_set$son)^2)

# predict does not always return objects of the same type - read help files
?predict.lm
?predict.glm

## Assessment

library(tidyverse)
library(caret)

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

# Within a replicate() loop, (1) partition the dataset into test and training 
# sets with p = 0.5 and using dat$y to generate your indices, (2) train a linear
# model predicting y from x, (3) generate predictions on the test set, and (4) 
# calculate the RMSE of that model. 
set.seed(1)
rmse <- replicate(100, {
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y ~ x, data = train_set)
  y_hat <- predict(fit, newdata = test_set)
  sqrt(mean((y_hat-test_set$y)^2))
})

mean(rmse)
sd(rmse)

# Write a function that takes a size n, then (1) builds a dataset using the code
# provided at the top of Q1 but with n observations instead of 100 and without the
# set.seed(1), (2) runs the replicate() loop that you wrote to answer Q1, which
# builds 100 linear models and returns a vector of RMSEs, and (3) calculates the
# mean and standard deviation of the 100 RMSEs.
set.seed(1)
n <- c(100, 500, 1000, 5000, 10000)
res <- sapply(n, function(n){
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  rmse <- replicate(100, {
    test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    fit <- lm(y ~ x, data = train_set)
    y_hat <- predict(fit, newdata = test_set)
    sqrt(mean((y_hat-test_set$y)^2))
  })
  c(avg = mean(rmse), sd = sd(rmse))
})

res

# Now repeat the exercise from Q1, this time making the correlation between x and
# y larger, as in the following code:
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
mean(rmse)
sd(rmse)

# Create a data set using the following code.

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2")) 

# Note that y is correlated with both x_1 and x_2 but the two predictors are 
# independent of each other, as seen by cor(dat).
# 
# Set the seed to 1, then use the caret package to partition into test and training
# sets with p = 0.5. Compare the RMSE when using just x_1, just x_2 and both 
# x_1 and x_2. Train a single linear model for each (not 100 like in the previous questions).

set.seed(1)
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)

fit <- lm(y ~ x_1, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))

fit <- lm(y ~ x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))

fit <- lm(y ~ x_1 + x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))


## Regression for a Categorical Outcome

# to show regression for a category, we will apply it to male vs female heights
library(dslabs)
data("heights")
y <- heights$height
set.seed(2, sample.kind = "Rounding") #if you are using R 3.6 or later

test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

# if we define the outcome Y as 1 for females and 0 for males, we are interested
# in the probability of being female given the height

# an example prediction for a student who is 66 inches tall:
train_set %>% 
  filter(round(height)==66) %>%
  summarize(y_hat = mean(sex=="Female"))

# we can plot it for several predicted values, and we can see that
# it is somewhat linear. 
heights %>% 
  mutate(x = round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarize(prop = mean(sex == "Female")) %>%
  ggplot(aes(x, prop)) +
  geom_point()

# if we convert the factors to 0 and 1, we can fit a linear model using this:
lm_fit <- mutate(train_set, y = as.numeric(sex == "Female")) %>% lm(y ~ height, data = .)
p_hat <- predict(lm_fit, test_set)
# we then make a decision rule - we predict female if the conditional probability
# is more than 50%. 
y_hat <- ifelse(p_hat > 0.5, "Female", "Male") %>% factor()
# a confusion matrix shows our accuracy:
confusionMatrix(y_hat, test_set$sex)$overall["Accuracy"]

## Logistic Regression

heights %>% 
  mutate(x = round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarize(prop = mean(sex == "Female")) %>%
  ggplot(aes(x, prop)) +
  geom_point() + 
  geom_abline(intercept = lm_fit$coef[1], slope = lm_fit$coef[2])

range(p_hat)

# logistic regression builds upon this but guarantees that the cond prob will
# be between 0 and 1
# to do this, we calculate the maximum likelihoos estimate, fitted using GLM

# fit logistic regression model - we use the family argument to tell it to use log
glm_fit <- train_set %>% 
  mutate(y = as.numeric(sex == "Female")) %>%
  glm(y ~ height, data=., family = "binomial")

# we can use predict to get a logistic model too, but we need to use the 
# type argument to get conditional probabilities
p_hat_logit <- predict(glm_fit, newdata = test_set, type = "response")

tmp <- heights %>% 
  mutate(x = round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarize(prop = mean(sex == "Female")) 
logistic_curve <- data.frame(x = seq(min(tmp$x), max(tmp$x))) %>%
  mutate(p_hat = plogis(glm_fit$coef[1] + glm_fit$coef[2]*x))
tmp %>% 
  ggplot(aes(x, prop)) +
  geom_point() +
  geom_line(data = logistic_curve, mapping = aes(x, p_hat), lty = 2)
# this model fits the data slightly better than the line

## 2 or 7

# we want to build an algorithm that detects whether a written digit is a 2 or a 7
# we will do this using two predictors: the proportion of dark pixels in the upper
# left and lower right quadrants of the image
mnist <- read_mnist()
is <- mnist_27$index_train[c(which.min(mnist_27$train$x_1), which.max(mnist_27$train$x_1))]
titles <- c("smallest","largest")
tmp <- lapply(1:2, function(i){
  expand.grid(Row=1:28, Column=1:28) %>%
    mutate(label=titles[i],
           value = mnist$train$images[is[i],])
})
tmp <- Reduce(rbind, tmp)
tmp %>% ggplot(aes(Row, Column, fill=value)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradient(low="white", high="black") +
  facet_grid(.~label) +
  geom_vline(xintercept = 14.5) +
  geom_hline(yintercept = 14.5)

# a reduced dataset is available - it contains 1000 digits from the set
data("mnist_27")
# we can examine the two quadrants and immediately see a pattern:
mnist_27$train %>% ggplot(aes(x_1, x_2, color = y)) + geom_point()
# if x1, the top left panel is dark, it is likely to be a seven

# here we can see examples of two sevens
is <- mnist_27$index_train[c(which.min(mnist_27$train$x_2), which.max(mnist_27$train$x_2))]
titles <- c("smallest","largest")
tmp <- lapply(1:2, function(i){
  expand.grid(Row=1:28, Column=1:28) %>%
    mutate(label=titles[i],
           value = mnist$train$images[is[i],])
})
tmp <- Reduce(rbind, tmp)
tmp %>% ggplot(aes(Row, Column, fill=value)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradient(low="white", high="black") +
  facet_grid(.~label) +
  geom_vline(xintercept = 14.5) +
  geom_hline(yintercept = 14.5)

# we can fit the logistic regression model using this code:
fit_glm <- glm(y ~ x_1 + x_2, data=mnist_27$train, family = "binomial")
p_hat_glm <- predict(fit_glm, mnist_27$test)
# whenever it is bigger than 0.5 we predict a 7, otherwise 2
y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, 7, 2))
confusionMatrix(data = y_hat_glm, reference = mnist_27$test$y)$overall["Accuracy"]
# fairly good accuracy for a basic model

# because this is a fake dataset, we can access the true cond prob
mnist_27$true_p %>% ggplot(aes(x_1, x_2, fill=p)) +
  geom_raster()
# using better colours and separating the two with a curve
mnist_27$true_p %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
  stat_contour(breaks=c(0.5), color="black") 

# we can compare the true cond prob to the estimated one
p_hat <- predict(fit_glm, newdata = mnist_27$true_p)
mnist_27$true_p %>%
  mutate(p_hat = p_hat) %>%
  ggplot(aes(x_1, x_2,  z=p_hat, fill=p_hat)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
  stat_contour(breaks=c(0.5),color="black") 
# because of the way log reg works, the boundary cannot be anything other
# than a straight line - so some information is guaranteed to be lost

# to see our mistakes, we plot the predictions against the real boundary
p_hat <- predict(fit_glm, newdata = mnist_27$true_p)
mnist_27$true_p %>%
  mutate(p_hat = p_hat) %>%
  ggplot() +
  stat_contour(aes(x_1, x_2, z=p_hat), breaks=c(0.5), color="black") +
  geom_point(mapping = aes(x_1, x_2, color=y), data = mnist_27$test)
# logistic regression forces our estimates to be planes, and our boundary to be a line
# we need a more flexible approach

## Introduction to Smoothing

# smoothing is also known as curve fitting and low-band pass filtering
# it gets its name from the assumption that the underlying trend is smooth, 
# but that it is buried below unpredictable noise

#we will examine the time trend data for obama/mccain 2008
data("polls_2008")
qplot(day, margin, data = polls_2008)
# we are only interested in learning the shape of the trend
# we assume that there is a true preference (f) for each day (x), but each
# data point has a random error (ε)
# therefore, a model for the observation is y = f(x)+ε

## Bin Smoothing and Kernels

# we think f of x (the underlying trend) changes slowly, so we can split the data
# into strata where f of x remains constant
# in this case, we assume it won't change over a week - or 7 points

# the final result looks like this - the bin smoother is quite wiggly, because 
# every time the interval moves, 2 points change. in order to correct this, we
# can weight the model so that the centre point has more priority than the outer ones
span <- 7 
fit <- with(polls_2008,ksmooth(day, margin, x.points = day, kernel="box", bandwidth =span))
polls_2008 %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(day, smooth), color="red")

# the functions that compute this weighting are called kernels, and they result
# in a smoother curve - our previous one was a "box", where all points are given
# the same weighting. we can change it to "normal", where those which are centred
# have a greater weighting
span <- 7
fit <- with(polls_2008, ksmooth(day, margin,  x.points = day, kernel="normal", bandwidth = span))
polls_2008 %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(day, smooth), color="red")

## Local Weighted Regression (loess)

# the bin smoother approach means that we need small intervals, which gives us 
# a small number of data points to average. local weighted regression lets us
# use wider intervals
# we will begin by considering a three week window, in which we assume that
# the trend is linear. the final result is a smoother fit. 
# ggplot uses loess in the geom_smooth function
polls_2008 %>% ggplot(aes(day, margin)) +
  geom_point() + 
  geom_smooth(color="red", span = 0.15, method = "loess", method.args = list(degree=1))

## Assessment

library(tidyverse)
library(lubridate)
library(purrr)
library(pdftools)

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
dat <- map_df(str_split(pdf_text(fn), "\n"), function(s){
  s <- str_trim(s)
  header_index <- str_which(s, "2015")[1]
  tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
  month <- tmp[1]
  header <- tmp[-1]
  tail_index  <- str_which(s, "Total")
  n <- str_count(s, "\\d+")
  out <- c(1:header_index, which(n==1), which(n>=28), tail_index:length(s))
  s[-out] %>%
    str_remove_all("[^\\d\\s]") %>%
    str_trim() %>%
    str_split_fixed("\\s+", n = 6) %>%
    .[,1:5] %>%
    as_data_frame() %>% 
    setNames(c("day", header)) %>%
    mutate(month = month,
           day = as.numeric(day)) %>%
    gather(year, deaths, -c(day, month)) %>%
    mutate(deaths = as.numeric(deaths))
}) %>%
  mutate(month = recode(month, "JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6, 
                        "JUL" = 7, "AGO" = 8, "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12)) %>%
  mutate(date = make_date(year, month, day)) %>%
  dplyr::filter(date <= "2018-05-01")

# Use the loess() function to obtain a smooth estimate of the expected number of 
# deaths as a function of date. Plot this resulting smooth function. Make the span
# about two months long and use degree = 1.

span <- 60 / as.numeric(diff(range(dat$date)))
fit <- dat %>% mutate(x = as.numeric(date)) %>% loess(deaths ~ x, data = ., span = span, degree = 1)
dat %>% mutate(smooth = predict(fit, as.numeric(date))) %>%
  ggplot() +
  geom_point(aes(date, deaths)) +
  geom_line(aes(date, smooth), lwd = 2, col = "red")

# Work with the same data as in Q1 to plot smooth estimates against day of the
# year, all on the same plot, but with different colors for each year.

dat %>% 
  mutate(smooth = predict(fit), day = yday(date), year = as.character(year(date))) %>%
  ggplot(aes(day, smooth, col = year)) +
  geom_line(lwd = 2)
.

library(broom)
library(dslabs)
data("mnist_27")
mnist_27$train %>% glm(y ~ x_2, family = "binomial", data = .) %>% tidy()

mnist_27$train %>% 
  mutate(y = ifelse(y=="7", 1, 0)) %>%
  ggplot(aes(x_2, y)) + 
  geom_smooth(method = "loess")

## Matrices

library(tidyverse)
library(dslabs)
mnist <- read_mnist()
# in the mnist data set, the predictors are saved in a matrix and the outcomes
# in a vector rather than using a data frame. 
class(mnist$train$images)

# because these are so big, we will take only the first 1000 images and labels
x <- mnist$train$images[1:1000,] 
y <- mnist$train$labels[1:1000]
# matrices are important because we can use linear algebra on them

## Matrix Notation

# a scalar is a single number (lowercase letter)
# a vector is several scalars (X1-XN)
length(x[,1])
# a matrix is a series of vectors of the same size, joined together, 
# where each vector is a column
x_1 <- 1:5
x_2 <- 6:10
cbind(x_1, x_2)
# dim gives us the dimensions of a matrix
dim(x)
# vectors do not have dimensions
dim(x_1)
# but we can convert them into matrices
dim(as.matrix(x_1))
dim(x)

## Converting a Vector to a Matrix

# converting a vector to a matrix is useful - for example, we could convert the 
# rows of pixel intensities into a matrix representing the grid

# we create a vector
my_vector <- 1:15

# fill the matrix by column
mat <- matrix(my_vector, 5, 3)
mat

# fill by row instead
mat_t <- matrix(my_vector, 3, 5, byrow = TRUE)
mat_t
# these are perfect transpositions of one another
identical(t(mat), mat_t)
# be careful though - it will repeat your vector without warning if the size
# does not match properly
matrix(my_vector, 5, 5)

# if we want to put the pixel densities of the third entry (4)
grid <- matrix(x[3,], 28, 28)
image(1:28, 1:28, grid)

# it is upside down because the top of the image - pixel 1 - is shown at the bottom
# because of the way the axes work. we can flip it back:
image(1:28, 1:28, grid[, 28:1])

## Row/Column Summaries and Apply

# we want to calculate the total darkness of a row, so we need to sum the values
# by row and then visualise the differences by height
sums <- rowSums(x)
avg <- rowMeans(x)

# we can now draw a box plot to see how the average pixel intensity differs
# from digit to digit
data_frame(labels = as.factor(y), row_averages = avg) %>%
  qplot(labels, row_averages, data = ., geom = "boxplot")

# note that we could also use colsums for columns, and the package matrixStats
# lets us perform operations on each row or colum efficiently using rowSds and colSds
# these functions do something similar to sapply and map - they apply the same
# function to a row or column
# apply lets you apply any function, not just sum or mean, to a matrix
# apply(matrix, dimension, function)
# row means:
avgs <- apply(x, 1, mean)
# column SDs:
sds <- apply(x, 2, sd)

## Filtering Columns by Summaries

library(matrixStats)

# we also need to study the variation of each pixel and remove the columns
# associated with pixels which don't change much - we measure this using SD
# since each column represents a pixel, we use colSds
sds <- colSds(x)
# a look at the distribution shows that many of the columns have very little variance
qplot(sds, bins = "30", color = I("black"))
# a heat map shows us that these are mostly around the outsides
image(1:28, 1:28, matrix(sds, 28, 28)[, 28:1])

#extract columns
x[ ,c(351,352)]
# and rows
x[c(2,3),]

# we can also use logical indices for this - we want only those with SDs above 60
new_x <- x[ ,colSds(x) > 60]
# this has made it smaller
dim(new_x)
# note that we have lost the matrix class here
class(x[,1])
dim(x[1,])

# if we want to preserve it, we could do this:
class(x[ , 1, drop=FALSE])
dim(x[, 1, drop=FALSE])

# Indexing and Binarising

# we can also turn matrices back into vectors
mat <- matrix(1:15, 5, 3)
as.vector(mat)

# this is a histogram of our predictors
qplot(as.vector(x), bins = 30, color = I("black"))
# we see a dichotomy - parts with and without ink
# what if we assume that values below 25 are just smudges and round them to 0?
new_x <- x
new_x[new_x < 50] <- 0

# we can see what this does using a smaller matrix
mat <- matrix(1:15, 5, 3)
mat[mat < 3] <- 0
mat
mat <- matrix(1:15, 5, 3)
mat[mat > 6 & mat < 12] <- 0
mat

# we want to binarise the data - pixels should be either "ink" or "no ink"
# we do this just using matrix operations
# binarize the data
bin_x <- x
bin_x[bin_x < 255/2] <- 0
bin_x[bin_x > 255/2] <- 1
bin_X <- (x > 255/2)*1

## Vectorisation and Algebra

# we now need to standardise the rows. in R, algebra from vectors are done 
# pairwise - item 1 subtracted from row 1, etc

#therefore, we can scale each row of a matrix like this - but NOT columns
(x - rowMeans(x)) / rowSds(x)

# in order to scale columns, we have to transpose the matrix, do the operation,
# the transpose it back
t(t(x) - colMeans(x))

# the sweep function also lets us take each entry of a vector and subtract 
# it from the corresponding row or column (in this code, 2 means column)
# sweep defaults to subtraction
x_mean_0 <- sweep(x, 2, colMeans(x))

# however, we can change this - and divide by the standard deviation
x_mean_0 <- sweep(x, 2, colMeans(x))
x_standardized <- sweep(x_mean_0, 2, colSds(x), FUN = "/")


# For each observation in the mnist training data, compute the proportion of 
# pixels that are in the grey area, defined as values between 50 and 205 (but 
# not including 50 and 205). 

mnist <- read_mnist()
y <- rowMeans(mnist$train$images>50 & mnist$train$images<205)
qplot(as.factor(mnist$train$labels), y, geom = "boxplot")
mean(y) # proportion of pixels
