## Bayes' Theorem

# if we have a test for a disease with 1/3900 prevalence, and the test has a 
# 99% accuracy, a person who gets a positive result still only has a 2%
# chance of having the disease, because we must factor in the odds
# that a randomly-chosen person would have the disease.
# this is shown here:
N <- 100000
prev <- 0.00025
outcome <- sample(c("Disease", "Healthy"), N, replace=TRUE, prob=c(prev, 1-prev))
# the low prevalence means that the disease is not common in the sample
N_D <- sum(outcome=="Disease")
N_D
N_H <- sum(outcome=="Healthy")
N_H
# considering this alone, it is obvious that false positives would be quite high
# just because the disease is so rare
# this turns the above vector into + or -, indicating whether the test would be
# positive or negative if done on them (NOT if they really have the disease)
accuracy <- 0.99
test <- vector("character",N)
test[outcome=="Disease"] <- sample(c("+","-"), N_D, replace=TRUE, prob=c(accuracy, 1-accuracy))
test[outcome=="Healthy"] <- sample(c("-","+"), N_H, replace=TRUE, prob=c(accuracy, 1-accuracy))
# we now have four possible outcomes
table(outcome, test)
# in the bottom right, we can see the number of healthy people who 
# get a positive test even though they are negative. it is comparatively
# large compared to the number of people who actually have the disease.
24 / 982
# in other words, there is roughly a 2.5% chance that a person testing positive
# really has the disease

## Bayes in Practice

# after 20 swings, jose inglesias had a batting average of 0.45, meaning he hit 45% of the time
# we can use hierarchical models to predict his average by the end of the season
# the frequentist approach just gives us an SE and a CI: SE = 0.111 and the CI
# is .228 to .672. this is a large range, and it is centered on .450, which is 
# an abnormally high batting average and one which would be unlikely to continue
# all season.
# if we look at all players, we find that they average 0.275 with an SD of 0.027
# .450 would be 6 standard deviations above the average!

# The Hierarchical Model

# first, we pick a player at random with an ability score summarised by p
# then we see 20 random outcomes 
# so we assume that players have a "baseline" ability, p. if a player batted 
# repeatedly, we would see their average converge to p
# we know that the expected value of this is 0.27, and the standard error is .027
# the next level of variability is about luck - a player has a success chance of p,
# but the actual outcomes will be normally distributed with expected value p and 
# a standard error of sqrt(p*(1-p)/N)
# the two layers are called prior distribution - the player variability - and the
# sampling distribution, the effect of random chance
# if we want to predict p, his true average: p is normal with expected value 0.275
# and standard error .027. Y, given p, is normal with expected value p, and standard error
# 0.111. 
# using an equation, we can find that the expected value of his batting average is .285
# this number is between the observed .450 and the expected .270

## Assessment

# Define `Pr_1` as the probability of the first son dying of SIDS
Pr_1 <- 1/8500

# Define `Pr_2` as the probability of the second son dying of SIDS
Pr_2 <- 1/100

# Define `Pr_B` as the probability of both sons dying of SIDS
Pr_B <- Pr_1*Pr_2

# Define Pr_A as the rate of mothers that are murderers
Pr_A <- 1/1000000

# Define Pr_BA as the probability that two children die without evidence of harm, given that their mother is a murderer
Pr_BA <- 0.50

# Define Pr_AB as the probability that a mother is a murderer, given that her two children died with no evidence of physical harm. Print this value to the console.
Pr_AB <- (Pr_BA*Pr_A)/Pr_B
Pr_AB

# Load the libraries and poll data
library(dplyr)
library(dslabs)
data(polls_us_election_2016)

# Create an object `polls` that contains the spread of predictions for each candidate in Florida during the last polling days
polls <- polls_us_election_2016 %>% 
  filter(state == "Florida" & enddate >= "2016-11-04" ) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# Examine the `polls` object using the `head` function
head(polls)

# Create an object called `results` that has two columns containing the average spread (`avg`) and the standard error (`se`). Print the results to the console.
results <- polls %>% summarize(avg=mean(spread), se=sd(spread)/sqrt(n()))

# Define `mu` and `tau`
mu <- 0
tau <- 0.01

# Define a variable called `sigma` that contains the standard error in the object `results`
sigma <- results$se

# Define a variable called `Y` that contains the average in the object `results`
Y <- results$avg

# Define a variable `B` using `sigma` and `tau`. Print this value to the console.
tau <- 0.01
miu <- 0
B <- sigma^2 / (sigma^2 + tau^2)
B

# Calculate the expected value of the posterior distribution
miu + (1 - B) * (Y - miu)

# Compute the standard error of the posterior distribution. Print this value to the console.
sqrt(1 / (1 / sigma ^2 + 1 / tau ^2))

# Construct the 95% credible interval. Save the lower and then the upper confidence interval to a variable called `ci`.
ci <- c(est - qnorm(0.975) * se, est + qnorm(0.975) * se)
ci

# Assign the expected value of the posterior distribution to the variable `exp_value`
exp_value <- B*mu + (1-B)*Y 

# Assign the standard error of the posterior distribution to the variable `se`
se <- sqrt( 1/ (1/sigma^2 + 1/tau^2))

# Using the `pnorm` function, calculate the probability that the actual spread was less than 0 (in Trump's favor). Print this value to the console.
pnorm(0, exp_value, se)

# Define the variables from previous exercises
mu <- 0
sigma <- results$se
Y <- results$avg

# Define a variable `taus` as different values of tau
taus <- seq(0.005, 0.05, len = 100)

# Create a function called `p_calc` that generates `B` and calculates the probability of the spread being less than 0
p_calc <- function(tau) {
  B <- sigma ^ 2 / (sigma^2 + tau^2)
  se <- sqrt(1 / (1/sigma^2 + 1/tau^2))
  exp_value <- B * mu + (1 - B) * Y
  pnorm(0, exp_value, se)
}

# Create a vector called `ps` by applying the function `p_calc` across values in `taus`
ps <- p_calc(taus)

# Plot `taus` on the x-axis and `ps` on the y-axis
plot(taus, ps)