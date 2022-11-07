## Conditionals

# This is the structure of an if/else statement in R
a = 0
# this prints the reciprocal of a, unless a is 0
if (a!=0){
  print(1/a)
} else{
  print("No reciprocal for 0.")
}

# in the general form:
# if(boolean condition){
#    expressions
# } else{
#   alternative expressions
# }

# which states, if any, have murder rates lower than 0.5?
ind <- which.min(murder_rate)
if (murder_rate[ind] < 0.5){
  print(murders$state[ind])
} else{
  print("No state has a murder rate that low.")
}

# we can also use an if/else statement, which returns an answer if true and
# a different answer if false
a <- 0
ifelse(a>0, 1/a, NA)
# this is useful because it can work on an entire vector item by item
a <- c(0,1,2,-4,5)
result <- ifelse(a>0, 1/a, NA)
result

# we can use if/else statements to replace all NAs in a vector with another value
data(na_example)
sum(is.na(na_example))
no_nas <- ifelse(is.na(na_example), 0, na_example)
sum(is.na(no_nas))

# the any function takes a vector of logicals and results true if any is true
z <- c(TRUE, TRUE, FALSE)
any(z)
z <- c(FALSE, FALSE, FALSE)
any(z)

# the all function does the same thing, if all entries are true
z <- c(TRUE, TRUE, FALSE)
all(z)
z <- c(TRUE, TRUE, TRUE)
all(z)


## Functions

# functions can help automate things you do regularly

# for example, this gives an average
sum(x) / length(x)
# but the mean function can do this more quickly
mean(x)
# if we wanted to make our own function, it would look like this:
avg <- function(x){
  s <- sum(x)
  n <- length(x)
  s/n
}

# it first defines variables, then the final part is what it returns
x <- 1:100
avg(x)
identical(mean(x),avg(x))

# in the general form:

# my_function <- function(x){
#   operations that affect x
#   which are defined by the function
#   final line is returned by the function
# }


# you can also give it more than one argument
avg <- function(x, arithmetic=TRUE){
  n <- length(x)
  ifelse(arithmetic, sum(x)/n, prod(x)^(1/n))
}
# in this case, arithmetic has a default value of true, but it could be false
# if it set to false, it gives the geometric mean instead of arithmetic

## For Loops

# we can create a function which computes the sum of an array
compute_s_n <- function(n){
  x <- 1:n
  sum(x)
}

compute_s_n(20000)

# in the general form:

# for (i in range){
#   operations that use i,
#   which is changing 
#   across the range of values
# }

for(i in 1:5){
  print(i)
}

# if we want to compute the sum of n repeatedly within a range:
m <- 25
# we create an empty vector
s_n <- vector(length = m)
for(n in 1:m){
  s_n[n] <- compute_s_n(n)
}
s_n

# we can check that this was done correctly by plotting n vs s_n
n <- 1:m
plot(n,s_n)


## Other Functions

# for loops are actually rarely used in R, because there are better ways
# to achieve the same outcome
# instead, we tend to use apply, sapply, tapply and mapply - the "apply" family
# others are split, cut, quantile, reduce, identical, unique, and many others

