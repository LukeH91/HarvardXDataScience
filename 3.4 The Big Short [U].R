## Interest Rates

options(scipen = 100)

# imagine our bank is going to give out 1000 loans of $180000 per year
# for every forclosure, we lose $200000, and there is a 2% rate of forclosure
n <- 1000
loss_per_forclosure <- -200000
p <- 0.02
defaults <- sample(c(0,1), n, prob=c(1-p, p), replace=TRUE)
sum(defaults*loss_per_forclosure)
# we can see that this results in an average loss, but what is the distribution?
B <- 10000
losses <- replicate(B,{
  defaults <- sample(c(0,1), n, prob=c(1-p, p), replace=TRUE)
  sum(defaults*loss_per_forclosure)
})
hist(losses)
mean(losses)
# we don't need a simulation though - the results are the sum of independent
# draws, its expected value and SE are given by the following formulae:
n*(p*loss_per_forclosure+(1-p)*0)
sqrt(n)*abs(loss_per_forclosure)*sqrt(p*(1-p))

# we can now set an interest rate which guarantees that we break even on average
# in other words, we need to make this true by determining x:
loss_per_forclosure+x(1-p)=0
# this can be calculated using this code:
loss_per_forclosure*p/(1-p)
# this is an interest rate of roughly 2%, but there is still a 50% chance of loss
# what if we want our chance of losing money to be 1 in 100?
# we want Pr(S < 0) = 0.01
# we subtract the expected value of S and divide by the SE of S on both sides
# In other words, Pr(S-E[S])/SE[S] < -E[S]/SE[S])
# now the term on the left is a standard normal random variable, Z
# this allows us to solve using a formula - we know that Z must be equal to qnorm(0.01)
l <- loss_per_forclosure
z <- qnorm(0.01)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))
