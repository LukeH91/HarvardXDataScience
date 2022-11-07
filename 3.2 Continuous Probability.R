## Continuous Probability

# when dealing with continuous variables, we can define an ECDF (empirical 
# cumulative distribution function). 
library(tidyverse)
library(dslabs)
data(heights)
# define a vector with the list of male heights
x <- heights %>% filter(sex=="Male") %>% .$height
# turn this vector into a function which tells us the proportion 
# smaller or equal to a
F <- function(a) mean(x<=a)
# picking a student at random, what is the chance he is shorter than 55.5 inches?
F(55.5)
# what about taller than 70 inches?
1-F(70)

## Theoretical Distribution

# the cumulative distribution for a normal distribution is pnorm in R
# using pnorm means that we assume the distribution is normal, and only need
# the mean and SD in order to estimate proportions above or below values

# what are the chances that a random student is taller than 70.5 inches?
1- pnorm(70.5, mean(x), sd(x))

# the nature of the normal distribution means it makes no sense to ask about
# specific values - rather, we ask about intervals
# proportion of students between 68.5 and 67.5 using actual data
mean(x<=68.5) - mean(x<=67.5)
# proportion of students between 68.5 and 67.5 approximated using pnorm
pnorm(68.5, mean(x), sd(x)) - pnorm(67.5, mean(x), sd(x))
# these are good, but it is worse if you don't include an integer
mean(x<=68.9) - mean(x<=68.5)
pnorm(68.9, mean(x), sd(x)) - pnorm(68.5, mean(x), sd(x))
# this is known as discretisation - reported heights are more common at
# discrete values, even though the data is continuous. 

## Probability Density

# to get the probability density function, we use dnorm
# this is useful when a pre-defined function is not available
dnorm(68.5, mean(x), sd(x)) - dnorm(67.5, mean(x), sd(x))

# dnorm(z) gives the probability f(z) for a certain z-score, so we can make
# it into a curve by calculating it over a random of values of z
# we will do this over 4 standard deviations to cover the entire curve
x <- seq(-4, 4, length = 100)
data.frame(x, f = dnorm(x)) %>%
  ggplot(aes(x, f)) +
  geom_line()

## Monte Carlo Simulations of Continuous Variables

# R has a function to generate normally-distributed outcomes: rnorm, which takes
# size, average (default 0) and SD (default 1)
# here is the code to generate outcomes which resemble the heights
# we filter for heights in the vector x
x <- heights %>% filter(sex=="Male") %>% .$height
# calulcate descriptives
n <- length(x)
avg <- mean(x)
s <- sd(x)
# and create a normal distribution based on the values
simulated_heights <- rnorm(n, avg, s)
# they look normal - unsurprising, because they were generated to be
ds_theme_set()
data.frame(simulated_heights=simulated_heights) %>% 
  ggplot(aes(simulated_heights)) + geom_histogram(color="black",binwidth=2)

# this ability to generate random datasets based on real parameters is useful
# how rare is it that the tallest person in a sample will be 7 feet tall?
B <- 10000
# we run 10k simulations of 800 values, then get the max
tallest <- replicate(B, {
  simulated_data <- rnorm(800, avg, s)
  max(simulated_data)
})
mean(tallest >= 7*12)

## Other Distributions

# R has a consistent naming scheme for functions to computer density (d), 
# quantiles (q), probability density function (p) and random (r)
# by using this, we can generate data for non-normal distributions too
# norm (normal distribution) - pnorm, qnorm, etc
# t (student's t distribution) - dt, qt, etc

## Assessment

set.seed(16)
act_scores <- rnorm(10000, 20.9, 5.7)

# What is the mean of act_scores?
mean(act_scores)

# What is the standard deviation of act_scores?
sd(act_scores)

# A perfect score is 36 or greater (the maximum reported score is 36).
# In act_scores, how many perfect scores are there out of 10,000 simulated tests?
sum(act_scores>=36)

# In act_scores, what is the probability of an ACT score greater than 30?
sum(act_scores>30)/10000

# In act_scores, what is the probability of an ACT score less than or equal to 10?
sum(act_scores<10)/10000

# Convert act_scores to Z-scores. 
zact <- (act_scores-mean(act_scores))/sd(act_scores)

# What is the probability of a Z-score greater than 2 (2 standard deviations above the mean)?
sum(zact>2)/10000

# What ACT score value corresponds to 2 standard deviations above the mean (Z = 2)?
mean(act_scores)+(sd(act_scores)*2)

# A Z-score of 2 corresponds roughly to the 97.5th percentile. Use qnorm() to determine the 
# 97.5th percentile of normally distributed data with the mean and standard deviation  
# observed in act_scores. What is the 97.5th percentile of act_scores?
qnorm(0.975, mean(act_scores), sd(act_scores))

# What is the minimum integer score such that the probability of that score or lower is at least .95?
qnorm(0.95, mean(act_scores), sd(act_scores))

# Use qnorm() to determine the expected 95th percentile, the value for which the probability of 
# receiving that score or lower is 0.95, given a mean score of 20.9 and standard deviation of 5.7.
# What is the expected 95th percentile of ACT scores?
qnorm(0.95, 20.9, 5.7)

# As discussed in the Data Visualization course, we can use quantile() to determine sample quantiles
# from the data. Make a vector containing the quantiles for p <- seq(0.01, 0.99, 0.01), the 1st through
# 99th percentiles of the act_scores data. Save these as sample_quantiles.
# In what percentile is a score of 26?
p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- quantile(act_scores, p)
sample_quantiles

theoretical_quantiles <- qnorm(p, 20.9, 5.7)
qqplot(theoretical_quantiles, sample_quantiles)
?qqline
