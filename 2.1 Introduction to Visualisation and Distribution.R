## Describe Heights to ET

# imagine we want to describe heights to ET. we gather heights in inches 
# from students and put them in a data frame.
library(dslabs)
data(heights)
head(heights)

# we had to split heights into males and females because they are different
# we can view the proportions of each
prop.table(table(heights$sex))

# this approach does not work as well for numerical data
prop.table(table(heights$height))

# instead, it is better to report the propotion of data which falls below
# a certain value - a cumulative distribution function (CDF)
plot(ecdf(heights$height))

# since this shows every point in the dataset, we can use it to calculate
# the number of people who fall between any two values

# however, this does not make the descriptives easy to see.
# instead, we tend to use histograms, which make it easier to determine this
# information. however, we do lose the individual numeric counts
hist(heights$height)

## Smooth Density Plots

# smooth density plots are similar to histograms, but look nicer
# these are essentially histograms with incredibly small bins
# however, this means that we'd need a lot of data - and we don't.
# but we can fit a line to the tops of the bars based on probability, not count
hist(heights$height, prob=TRUE)
lines(density(heights$height))

# note that to interpret the amount of data points in a given range, you 
# basically have to take an interval and calculate the proportion of the curved
# are that falls under it

## The Normal Distribution

# the normal distribution allows our summary statistics to work
# for a vector x:
average <- sum(x) / length(x)
SD <- sqrt(sum((x-average)^2)/length(x))

# for our heights:
index <- heights$sex =="Male"
x <-heights$height[index]

average <- mean(x)
SD <- sd(x)
c(average=average, SD=SD)

# using this, we can see that the approximation given by a standard normal 
# distribution is basically the same as our fit line, as long as the mean
# and SD are the same. 
hist(x, prob=TRUE, breaks=15)
lines(density(x))
curve(dnorm(x, mean=average, sd=SD), add=TRUE, col="darkgreen")

# we can quickly z-score in R using scale
Z <- scale(x)

## The Normal CDF and pnorm

# we can calculate the CDF for the normal distribution using pnorm
# this means we don't need the entire dataset to find some probabilities:
# what is the probability that a randomly-selected student is taller than 70.5?
1 - pnorm(70.5, mean(x), sd(x))

# using this principle, we can see that the normal approximation matches
# the real data very closely.
mean(x <= 68.5) - mean(x <= 67.5)
pnorm(68.5, mean(x), sd(x)) - pnorm(67.5, mean(x), sd(x))

mean(x <= 69.5) - mean(x <= 68.5)
pnorm(69.5, mean(x), sd(x)) - pnorm(68.5, mean(x), sd(x))

mean(x <= 70.5) - mean(x <= 69.5)
pnorm(70.5, mean(x), sd(x)) - pnorm(69.5, mean(x), sd(x))

# however, note that this falls apart if the interval does not include
# an actual integer, since most people in this dataset round their heights.
mean(x <= 70.9) - mean(x <= 70.1)
pnorm(70.9, mean(x), sd(x)) - pnorm(70.1, mean(x), sd(x))

## Quantiles

# quantiles are cut-off points which divide data into sections
# the qth quantile is the point at which q% of the data is equal to or below
# said value

# we can calculate these using the quantile function
quantile(data, q)

# percentiles are the same thing, but split the data into 100 parts
# you can find them all using this generic code:
p <- seq(0.01, 0.99, 0.01)
quantile(data, p)

# quartiles do the same thing but split it into 4 big chunks (25th, 50th, 75th)
# we can return these easily using summary()

library(dslabs)
data(heights)

summary(heights$height)

p <- seq(0.01, 0.99, 0.01)
percentiles <- quantile(heights$height, p)

percentiles[names(percentiles) == "25%"]
percentiles[names(percentiles) == "75%"]

## Finding Quantiles with qnorm

# qnorm gives the theoretical probability quantiles from a normal distribution
qnorm(probability, mean, stdev)

# mean and stdev default to 1 and 0, the standard normal distribution
qnorm(0.25)

# in other words, this is the probability associated with a given z-score
pnorm(-1.96)
qnorm(0.0249979)

# thus, qnorm is for theoretical quantiles of a dataset if it is normal
# assuming male heights are normal with M 69 and SD 3
p <- seq(0.01, 0.99, 0.01)
qnorm(p, 69, 3)

## Quantile-Quartile Plots

# we use quantile-quantile (q-q) plots to see if descriptives fit data well
# we define a series of proportions (p) and then, for each p, we find q, such
# that the proportion of the values below q is p.

# for example, for male heights, 50% is below 69.5 inches, so if p 0.5,
# the q is 69.5. we do this for a series of Ps, and then check if they match 
# the normal distribution. if they do, it is a good fit for the data.

# we first generate a series of ps, which are stored as p
p <- seq(0.05, 0.95, 0.05)

# then we get the quantiles from the data
observed_quantiles <- quantile(heights$height,p)

# we then take the corresponding theoretical normally-distributed Ps
theoretical_quantiles <- qnorm(p, mean(heights$height), sd(heights$height))

# we then make a plot to see how well they match
plot(theoretical_quantiles, observed_quantiles)
abline(0,1)

# these fit quite nicely to the line, which shows that the normal distribution
# is a good approximation in this case

# this becomes simnpler if we use standard units (z-scores) because we
# no longer need to define the mean and SD in the qnorm function
p <- seq(0.05, 0.95, 0.05)
z <- heights$height
observed_quantiles <- quantile(z,p)
theoretical_quantiles <- qnorm(p)
plot(theoretical_quantiles, observed_quantiles)
abline(0,1)

## Percentiles

# percentiles are the quantiles when p is defined in 0.01 increments

## Boxplots

data(murders)
murder_rate <- (murders$total/murders$population)*100000

p <- seq(0.05, 0.95, 0.05)
z <- murder_rate
observed_quantiles <- quantile(z,p)
theoretical_quantiles <- qnorm(p)
plot(theoretical_quantiles, observed_quantiles)
abline(0,1)

# the murder rate data is not normally distributed. instead, we use a boxplot.
boxplot(murder_rate)


# Exploring Female Heights

# load in data and create a female height variable
library(dslabs)
data(heights)
fheights <- heights$height[heights$sex == "Female"]

# the histogram does not look very symmetrical
hist(fheights, prob=TRUE, breaks=25)
lines(density(fheights))

# the q-q plot shows that it is a poor match for the normal distribution
p <- seq(0.05, 0.95, 0.05)
observed_quantiles <- quantile(fheights,p)
theoretical_quantiles <- qnorm(p)
plot(theoretical_quantiles, observed_quantiles)
abline(65,3)
# i don't know why this doesn't work

# the upper end shows some surprisingly tall women - this is probably because
# when the data was entered, the default setting was female, so some males
# may have incorrectly logged their height as female

