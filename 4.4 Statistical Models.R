## Poll Aggregators

library(tidyverse)
library(ggplot2)
library(dslabs)
data("polls_us_election_2016")

# aggregating polls and combining their sample sizes allows more precise predictions

# in this case, although the confidence intervals contain zero, they all contain
# the real result
d <- 0.039
Ns <- c(1298, 533, 1342, 897, 774, 254, 812, 324, 1291, 1056, 2172, 516)
p <- (d+1)/2

# calculate confidence intervals of the spread
confidence_intervals <- sapply(Ns, function(N){
  X <- sample(c(0,1), size=N, replace=TRUE, prob = c(1-p, p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  2*c(X_hat, X_hat - 2*SE_hat, X_hat + 2*SE_hat) - 1
})

# generate a data frame storing results
polls <- data.frame(poll = 1:ncol(confidence_intervals),
                    t(confidence_intervals), sample_size = Ns)
names(polls) <- c("poll", "estimate", "low", "high", "sample_size")
polls

# without having access to the data, we can effectively combine these polls into
# a single huge one with 11,269 participants. we do this by constructing an
# estimate of the spread - d - with a weighted average
# we first multiply each estimate by the sample size, then divide by the sum of the sample size
d_hat <- polls %>%
  summarize(avg = sum(estimate*sample_size) / sum(sample_size)) %>%
  .$avg
# we can then create an estimate of the proportions, which can be used to 
# estimate the standard error
p_hat <- (1+d_hat)/2
moe <- 2*1.96*sqrt(p_hat*(1-p_hat)/sum(polls$sample_size))   
round(d_hat*100,1)
round(moe*100, 1)
# this tells us that the estimate is 3.1% plus or minue 1.8%, which does not include 0

## Poll Data and Pollster Bias

data(polls_us_election_2016)
names(polls_us_election_2016)

# we are only interested in national polls which occurred a week before the election,
# as well as "unreliable" polls graded with a B or less

polls <- polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-10-31" &
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade)))

# we also want to add an estimate of the spread
polls <- polls %>% mutate(spread=rawpoll_clinton/100 - rawpoll_trump/100)

# to simplify, we will assume there are only two parties. thus, p is clinton's vote,
# and 1-p is trump's vote. these spreads can be called d, and we have one from each poll
# they are a random variable with an approximately normal distribution

# the expected value is the election night spread, d, and the standard error is
# 2*sqrt(p*(1-p)/N) - but the sample size is now the aggregate sample size
d_hat <- polls %>% summarize(d_hat = sum(spread*samplesize)/sum(samplesize)) %>%
  .$d_hat

p_hat <- (d_hat+1)/2
moe <- 1.96 * 2 * sqrt(p_hat*(1-p_hat)/sum(polls$samplesize))
moe
# so our margin of error is .0066, which is very small
# so, if we used this data, we would report a spread od 1.43% and moe of 0.66%
# on election night, the spread is 2.1% - outside of the CI. so what happened?
polls %>% ggplot(aes(spread)) + geom_histogram(color="black", binwidth=.01)
# the data is not normally distributed, and the standard error is larger than 0.66%
# this is because many pollsters are involved, and some do more than one poll per week
polls %>% group_by(pollster) %>% filter(n()>=6) %>%
  ggplot(aes(pollster,spread)) +
  geom_point() +
  theme(axis.text.x = element_text(angle=90, hjust=1))
# standard errors within each pollster
polls %>% group_by(pollster) %>%
  filter(n() >= 6) %>%
  summarize(se = 2 * sqrt(p_hat * (1-p_hat) / median(samplesize)))
# as we can see, although the standard error is fine across all, pollsters,
# they are making predictions in different directions. to fix this, we will
# make a data-driven model

## Data-Driven Models

# for each pollster, collect last result before the election
one_poll_per_pollster <- polls %>% group_by(pollster) %>%
  filter(enddate == max(enddate)) %>%      # keep latest poll
  ungroup()

# the histogram looks like this
one_poll_per_pollster %>%
  ggplot(aes(spread)) + geom_histogram(binwidth = 0.01)

# using an urn model to just combine all these results does not work because
# of pollster bias, so instead, we imagine it as an urn where each bead
# is the result of a given poll - therefore, the expected value
# should be the actual spread, d

# construct 95% confidence interval
results <- one_poll_per_pollster %>%
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - 1.96*se, end = avg + 1.96*se)
round(results*100, 1)

## Assessment

# Load the 'dslabs' package and data contained in 'heights'
library(dslabs)
data(heights)

# Make a vector of heights from all males in the population
x <- heights %>% filter(sex == "Male") %>%
  .$height

# Calculate the population average. Print this value to the console.
mean(x)

# Calculate the population standard deviation. Print this value to the console.
sd(x)

# Define `N` as the number of people measured
N <- 50

# Define `X` as a random sample from our population `x`
X <- sample(x, N, replace=TRUE)

# Calculate the sample average. Print this value to the console.
mean(X)

# Calculate the sample standard deviation. Print this value to the console.
sd(X)

# Define `se` as the standard error of the estimate. Print this value to the console.
se <- sd(X)/sqrt(N)
se

# Construct a 95% confidence interval for the population average based on our sample. Save the lower and then the upper confidence interval to a variable called `ci`.
ci <- c(mean(X)-qnorm(0.975)*se, mean(X)+qnorm(0.975)*se)
ci

# Define `B` as the number of times to run the model
B <- 10000

# Define an object `res` that contains a logical vector for simulated intervals that contain mu
res <- replicate(B,{
  X <- sample(x, N, replace=TRUE)
  X_hat <- mean(X)
  se_hat <- sd(X)
  se <- se_hat / sqrt(N)
  interval <- c(qnorm(0.025, mean(X), se) , qnorm(0.975, mean(X), se))
  between(mu, interval[1], interval[2])
})

# Calculate the proportion of results in `res` that include mu. Print this value to the console.
mean(res)

# Load the libraries and data you need for the following exercises
library(dslabs)
library(dplyr)
library(ggplot2)
data("polls_us_election_2016")

# These lines of code filter for the polls we want and calculate the spreads
polls <- polls_us_election_2016 %>% 
  filter(pollster %in% c("Rasmussen Reports/Pulse Opinion Research","The Times-Picayune/Lucid") &
           enddate >= "2016-10-15" &
           state == "U.S.") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) 

# Make a boxplot with points of the spread for each pollster
polls %>% group_by(pollster) %>% ggplot(aes(pollster, spread)) + geom_boxplot() + geom_point()

# The `polls` data have already been loaded for you. Use the `head` function to examine them.
head(polls)

# Create an object called `sigma` that contains a column for `pollster` and a column for `s`, the standard deviation of the spread
sigma <- polls %>% group_by(pollster) %>% summarize(s=sd(spread))

# Print the contents of sigma to the console
sigma

# Create an object called `res` that summarizes the average, standard deviation, and number of polls for the two pollsters.
res <- polls %>% group_by(pollster) %>% summarize(avg=mean(spread), s=sd(spread), N=n())

# Store the difference between the larger average and the smaller in a variable called `estimate`. Print this value to the console.
estimate <-  max(res$avg) - min(res$avg)
estimate

# Store the standard error of the estimates as a variable called `se_hat`. Print this value to the console.
se_hat <- sqrt(res$s[2]^2/res$N[2] + res$s[1]^2/res$N[1])
se_hat

# Calculate the 95% confidence interval of the spreads. Save the lower and then the upper confidence interval to a variable called `ci`.
ci <- c(estimate - qnorm(0.975)*se_hat, estimate + qnorm(0.975)*se_hat)

