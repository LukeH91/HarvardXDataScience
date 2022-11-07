## The CLT in Practice

library(dslabs)
library(tidyverse)
ds_theme_set()

# what if we want to know the odds that our expected value (X bar) is
# less than 1% away from p, the true value?

# because of the normal distribution, this is the same as asking what is the
# probability that X < (p+.01) minus Pr X < (p-.01)

# to do this, we can subtract the expected value and divide by the standard error
# on both sides of the equation
# this means that the probability is:
# Pr(Z<.01)/SE(X) - # Pr(Z<-.01)/SE(X)

# we can't calculate the SE because we don't know P, but fortunately, we can 
# use X bar instead of P and it will stil work. It is therefore:
# SE(X) = sqrt(X*(1-X)/N)

# in our first draw from the urn, we had 12 blue beads and 13 red, so X=0.48
# our SE is therefore:
X_hat <- 0.48
se <- sqrt(X_hat*(1-X_hat)/25)
se

# we wanted to be 1% away - did we succeed?
pnorm(0.01/se) - pnorm(-0.01/se)
# our probability is about 8%, so no, but we can use this to determine a good sample size

## Margin of Error

# the margin of error is 2* the standard error, because this corresponds to a 
# 95% confidence interval around x

## A Monte Carlo Simulation for the CLT

# the code would look like this, but unfortunately we do not know p:
B <- 10000
N <- 1000
X_hat <- replicate(B, {
  X <- sample(c(0,1), size=N, replace=TRUE, prob=c(1-p, p))
  mean(X)
})

# we can pick several values of p, then run simulations using those
p <- 0.45
X_hat <- replicate(B, {
  X <- sample(c(0,1), size=N, replace=TRUE, prob=c(1-p, p))
  mean(X)
})

# with a p of 0.45, we expect an X_hat of 0.45 and a standard error around .015
mean(X_hat)
sd(X_hat)

## The Spread

# because there are only two parties, the spread is p - (1-p) or 2p-1

# since we only know the estimate Xbar, we use 2*Xbar-1

# therefore, for our first example with 25 beads, our estimate of p was 0.48 and 
# the margin of error was 0.2 - so the spread would be .04, and the margin of error
# 0.4. This is not a useful estimate, and the sample size is too small. 

## Huge Polls

# if we just do a sample of 100k, we could get a maximum margin of error of 
# about 0.3% as long as the proportion is between 0.35 and 0.65
N <- 100000
p <- seq(0.35, 0.65, length=100)
SE <- sapply(p, function(x) 2*sqrt(x*(1-x)/N))
data.frame(p=p, SE=SE) %>% ggplot(aes(p, SE)) +
  geom_line()

# this graph shows us that the standard error stays very small with such a big sample
# so why not just do huge samples?
# 1. cost
# 2. people might be dishonest
# 3. sampling issues (i.e. cannot be truly representative)
# this means that such large polls are inevitably biased

## Assessment

# Write a function called `take_sample` that takes `p` and `N` as arguements and returns the average value of a randomly sampled population.
take_sample <- function(p, N){
  x <- sample(c(1,0), size=N, replace=TRUE, prob=c(p, 1-p))
  mean(x)
}

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# Call the `take_sample` function to determine the sample average of `N` randomly selected people from a population containing a proportion of Democrats equal to `p`. Print this value to the console.
take_sample(p, N)

# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# The variable `B` specifies the number of times we want the sample to be replicated
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Create an objected called `errors` that replicates subtracting the result of the `take_sample` function from `p` for `B` replications
errors <- replicate(B, (p-take_sample(p, N)))

# Calculate the mean of the errors. Print this value to the console.
mean(errors)

# Calculate the mean of the absolute value of each simulated error. Print this value to the console.
mean(abs(errors))

# Calculate the standard deviation of `errors`
sqrt(mean(errors^2))

## Assessment

# `N` represents the number of people polled
N <- 25

# Create a variable `p` that contains 100 proportions ranging from 0 to 1 using the `seq` function
p <- seq(0,1,length.out=100)

# Create a variable `se` that contains the standard error of each sample average
se <- sqrt(p*(1-p)/N)

# Plot `p` on the x-axis and `se` on the y-axis
plot(p, se)

# The vector `p` contains 100 proportions of Democrats ranging from 0 to 1 using the `seq` function
p <- seq(0, 1, length = 100)

# The vector `sample_sizes` contains the three sample sizes
sample_sizes <- c(25, 100, 1000)

# Write a for-loop that calculates the standard error `se` for every value of `p` 
# for each of the three samples sizes `N` in the vector `sample_sizes`. Plot the 
# three graphs, using the `ylim` argument to standardize the y-axis across all three plots.
for (val in sample_sizes) {
  se <- sqrt(p*(1-p)/sample_sizes)
  plot(p, se, ylim=c(0,0.1))
}

# `N` represents the number of people polled
N <- 25

# `p` represents the proportion of Democratic voters
p <- 0.45

# Calculate the standard error of the spread. Print this value to the console.
2*sqrt(p*(1-p)/N)

# Define `p` as the expected value equal to 0.45
p <- 0.45

# Define `N` as the sample size
N <- 100

# Calculate the standard error
sqrt(p*(1-p)/N)

# Define `X` as a random sample of `N` voters with a probability of picking a Democrat ('1') equal to `p`
X <- sample(c(1,0), replace=TRUE, N, prob=c(p,1-p))

# Define `X_bar` as the average sampled proportion
X_bar <- mean(X)

# Calculate the standard error of the estimate. Print the result to the console.
sqrt(X_bar*(1-X_bar)/N)

# Generate `errors` by subtracting the estimate from the actual proportion of Democratic voters
errors <- replicate(B, p - take_sample(p, N))

# Generate a qq-plot of `errors` with a qq-line showing a normal distribution
qqnorm(errors)
qqline(errors)

# Calculate the probability that the estimated proportion of Democrats in the population is greater than 0.5. Print this value to the console.
1-pnorm(0.5, mean = p, sd=(sqrt(p*(1-p)/N)))

# Define `se_hat` as the standard error of the sample average
se_hat <- sqrt(X_hat*(1-X_hat)/N)

# Calculate the probability that the error is 0.01 or larger
1 - pnorm(.01, 0, se_hat) + pnorm(-0.01, 0, se_hat)