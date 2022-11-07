## Random Variables

# random variables are numeric outcomes resulting from a random process
# for example, for the beads:
beads <- rep(c("red", "blue"), time=c(2,3))
# we then select a random bead, which makes X either 1 or 0
X <- sample(ifelse(beads,1) == "blue",1,0)

## Sampling Models

# imagine we want to decide whether or not to offer a roulette table
# 1000 people will play, and they can only bed red or black (50/50)
# a roulette wheel has 18 black pockets, 18 red, and 2 green
# red means the casino lose 1 dollar, and black/green means they win 1 dollar
color <- rep(c("black", "red", "green"), time=c(18,18,2))
# we can code 1000 independent draws in this way
n <- 1000
X <- sample(ifelse(color=="red",-1,1),n,replace=TRUE)
# here are the first 10 outcomes
X[1:10]
# note that, because we know the proportions, we can skip the colours entirely
X <- sample(c(-1,1), n, replace=TRUE, prob=c(9/19, 10/19))
# this is called a sampling model, and the sample is the random set of draws
# how many net wins do we get in each draw of 1000?
X <- sample(c(-1,1), n, replace=TRUE, prob=c(9/19, 10/19))
sum(X)

# using repeated measures of S, we can create a probability distribution:
# the probability that S will fall into a given interval
# we can use a monte carlo simulation to generate many instances of S
# we will have 1000 people play roulette 10000 times
n <- 1000
B <- 10000
S <- replicate(B,{
  X <- sample(c(-1,1), n, replace=TRUE, prob=c(9/19, 10/19))
  sum(X)
})
# now we have S, a vector of samples - how often is it below a specific threshold?
mean(S <= 0)
sum(S <= 0)
# In other words, the casino only gets a negative payout in 5.41% of cases
# we can create a histogram of the distribution
hist(S, breaks=100)
# this is a normal distribution, which we can confirm via qqplot or by adding
# a normal distribution with the same parameters
library(tidyverse)
s <- seq(min(S), max(S), length = 100)    # sequence of 100 values across range of S
normal_density <- data.frame(s = s, f = dnorm(s, mean(S), sd(S))) # generate normal density for S
data.frame (S = S) %>%    # make data frame of S for histogram
  ggplot(aes(S, ..density..)) +
  geom_histogram(color = "black", binwidth = 10) +
  ylab("Probability") +
  geom_line(data = normal_density, mapping = aes(s, f), color = "blue")

## Central Limit Theorem

# for a distribution of a list of numbers, 
# F(a) is the propotion of a list that is less than or equal to a
# if we want the avg or sd, we calculate them in the normal manner

# for a distribution function, which does not have or need a list of numbers, 
# F(a) is the probability that X is less than or equal to a
# the avg and sd are called the expected value and the standard error

## Notation for random variables

# capital letters are used to denote random variables, and small letters
# for actual observed values

## Central Limit Theorem

# CLT holds that when the number of draws is large, the sum of the draws
# is itself normal

# E[X] denotes the expected value of X, in other words, the expected mean of 
# any given draw. in our earlier example, since there are 20 1s and 18 -1s, 
# the expected value is E[X] = (20+(-18))/38, which is around 0.05 or 5 cents
# in other words, if we play the game repeatedly, the casino wins around 0.05
# per game on average.
# We can confirm this with a monte carlo simulation
B <- 10000000
X <- sample(c(-1,1), B, replace=TRUE, prob=c(9/19, 10/19))
mean(X)

(9/19)*(10/19)

sqrt(90)/19

# but the expected value is not the only important thing. the casino wins $50
# on average for every 1000 people who play the game, but how likely is a 
# negative outcome? to answer this, we use the standard error SE[X]
# if done without replacement, this is sqrt(n)*sd, or |b-a|*sqrt(p(1-p))
# in our game: |(1-(-1))|sqrt(10/19)*(9/19)
n <- 1000
sqrt(n)*abs(1-(-1))*sqrt(9/19*(1-(9/19)))
# or, more simply
sqrt(n)*2*sqrt(90)/19
# so when 1000 people bet on red, the casino wins 50$ with an SE of 32$
# these values match with the ones from the monte carlo simulation, meaning
# we can skip this simulation entirely and just use pnorm
mu <- n*(20-18)/38
se <- sqrt(n)*2*sqrt(90)/19
pnorm(0,mu,se)

## Assessment 

# The variables 'green', 'black', and 'red' contain the number of pockets for each color
green <- 2
black <- 18
red <- 18

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- green / (green+black+red)

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1-p_green

# Calculate the expected outcome if you win $17 if the ball lands on green and you lose $1 if the ball doesn't land on green
((17*green)+(-1*(black+red)))/(green+black+red)

# Compute the standard error of the random variable
abs(17-(-1))*sqrt(p_green*(p_not_green))

# Define the number of bets using the variable 'n'
n <- 1000

# Calculate the expected outcome of 1,000 spins if you win $17 when the ball lands on green and you lose $1 when the ball doesn't land on green
((17*p_green)+((-1)*p_not_green))*1000

# Compute the standard error of the sum of 1,000 outcomes
sqrt(n) * abs((17 - -1))*sqrt(p_green*p_not_green)

## Averages and Proportions

# 1.because the expected value (on average) of each draw from an urn is the same,
# the sum of the objects in the urn is the same as the sum of their expected 
# values. 

# 2.the expected value of a random variable times a non-random constant is
# the expected value times the non-random constant

# because of these two properties, the expected value of the average of draws
# from an urn is the same as the expected value of the urn

# 3.the square of the standard error of the sum of independent random variables
# is the sum of the square of the standard error of each random variable

# 4.the standard error of a random variable times a non-random constant is
# the standard error times the non-random constant

# as a result, the average error of the average of independent draws from the 
# same urn is the standard deviation of the entire urn

## The Law of Large Numbers

# another result of these properties is that as n grows larger and larger, the 
# standard error of the average of the draws gets smaller and smaller.
# over time, the average of the draws converges on the real average

## How large is large?

# in many cases, 30 draws is enough for the CLT to become useful.
# however, when the probability of success is small, we need many samples
# for the lottery, tens or hundreds of thousands play, and only 1-4 win
# this is not approximated well by the normal distribution. instead, we could
# use the poisson distribution in this case.

## Assessment

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- 2 / 38

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1-p_green

# Define the number of bets using the variable 'n'
n <- 100

# The variable `B` specifies the number of times we want the simulation to run. Let's run the Monte Carlo simulation 10,000 times.
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)

# Create an object called `S` that replicates the sample code for `B` iterations and sums the outcomes.
S <- replicate(B,{
  X <- sample(c(17,-1),n, replace=TRUE, prob=c(p_green, p_not_green))
  sum(X)
})

# Compute the average value for 'S'
mean(S)

# Calculate the standard deviation of 'S'
sd(S)

## An old version of the SAT college entrance exam had a -0.25 point penalty 
# for every incorrect answer and awarded 1 point for a correct answer. The 
# quantitative test consisted of 44 multiple-choice questions each with 5 answer
# choices. Suppose a student chooses answers by guessing for all questions on the test.

# What is the probability of guessing correctly for one question?
0.2

# What is the expected value of points for guessing on one question?
(1*0.2)+(-0.25*0.8)

# What is the expected score of guessing on all 44 questions?
((1*0.2)+(-0.25*0.8))*44

# What is the standard error of guessing on all 44 questions?
sqrt(44) * abs((1 - -0.25))*sqrt(0.2*0.8)

# Use the Central Limit Theorem to determine the probability that a guessing
# student scores 8 points or higher on the test.
1-pnorm(8,
        ((1*0.2)+(-0.25*0.8)),
        (sqrt(44) * abs((1 - -0.25))*sqrt(0.2*0.8)))

# Set the seed to 21, then run a Monte Carlo simulation of 10,000 students guessing on the test.
# What is the probability that a guessing student scores 8 points or higher?
set.seed(21)
X <- replicate(10000,{
  Z <- sample(c(1,-0.25),44, replace=TRUE, prob=c(0.2, 0.8))
  sum(Z)
})
mean(X>=8)

# The SAT was recently changed to reduce the number of multiple choice options 
# from 5 to 4 and also to eliminate the penalty for guessing.

# What is the expected value of the score when guessing on this new test?
((1*0.25)+(-0*0.75))*44

# Consider a range of correct answer probabilities p <- seq(0.25, 0.95, 0.05) 
# representing a range of student skills.
# What is the lowest p such that the probability of scoring over 35 exceeds 80%?
# disable scientific notation
options(scipen = 100)
p <- seq(0.25, 0.95, 0.05)
guess <- function(p){
  # get expected value for a given p
  EVp <- ((1*p)+(0*(1-p)))*44
  # get standard error for a given p
  SEp <- sqrt(44) * abs((1))*sqrt(p*(1-p))
  # check what the percentage chance of getting over 35 is
  1-pnorm(35, EVp, SEp)
}

# check this on every value of p
X <- sapply(p, guess)
data.frame(p, X)

# A casino offers a House Special bet on roulette, which is a bet on five pockets
# (00, 0, 1, 2, 3) out of 38 total pockets. The bet pays out 6 to 1. In other words
# a losing bet yields -$1 and a successful bet yields $6. A gambler wants to know
# the chance of losing money if he places 500 bets on the roulette House Special.

# What is the expected value of the payout for one bet?
pwin <- 5/38
plose <- 33/38
(6*pwin)+(-1*plose)

# What is the standard error of the payout for one bet?
abs((6 - -1))*sqrt(pwin*plose)

# What is the expected value of the average payout over 500 bets?
# same as one bet
(6*pwin)+(-1*plose)

# What is the standard error of the average payout over 500 bets?
sqrt(500) * abs((6 - -1))*sqrt(pwin*plose)/500

# What is the expected value of the sum of 500 bets?
sum((6*pwin)+(-1*plose))*500

# What is the standard error of the sum of 500 bets?
sqrt(500) * abs((6 - -1))*sqrt(pwin*plose)

#Use pnorm() with the expected value of the sum and standard error of the sum 
# to calculate the probability of losing money over 500 bets, Pr (X <= 0).
pnorm(0,
      sum((6*pwin)+(-1*plose))*500, 
      sqrt(500) * abs((6 - -1))*sqrt(pwin*plose))

