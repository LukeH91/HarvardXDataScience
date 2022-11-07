## Setup

# These commands install packages
install.packages("dslabs")
install.packages("tidyverse")

# This command tells you every package you have installed
installed.packages()

## Objects

# to assign variables, we use an arrow
a <- 1
b <- 1
c <- -1
# to call up an object, we can use either of these
a
print(a)
# defined objects become entities in the workspace, which can be listed via
ls()
# since we have defined these variables, we can type out the quadratic equation
(-b + sqrt(b^2 - 4*a*c))/ (2*a)
(-b - sqrt(b^2 - 4*a*c))/ (2*a)

## Functions

# if we just type a function name, it will show us the code
ls
# if we want to evaluate a function, we must add brackets
ls()
# most functions need an argument to work
log(8)
log(a)
# they can also be "nested" within one another, and are evaluated from the
# inside out
exp(1)
log(2.718282)
log(exp(1))
# functions also have documented help
help("log")
?log
# in the help file, any arguments which have a "default value" are not necessary
# to run the function, but we can change them. all of these are the same:
log(8, base=2)
log(8,2)
log(x=8, base=2)
# there are examples of functions which do not need parentheses to operate:
3^2
# R also includes some datasets to help users practice
data()
co2
pi


## Data Types

# the class function helps us determine the type of an object
a <- 2
class(a)
class(ls)

# the easiest way to store data in R is as a data frame - essentially a table
library(dslabs)
data("murders")
class(murders)

# murders is a data frame - we can view its structure, or list column names
str(murders)
names(murders)

# to access variables (columns), we use the accessor
# this gives us a vector, or a list of numbers
murders$population

# we can save this as a variable, then view its length
pop <- murders$population
length(pop)

# we can also store characters as a vector using quotes
a <- "a"
a
class(murders$state)

# there are also logical (true/false) vectors
# == asks R if two things are equal, and returns true or false
z <- 3 == 2
z
class(z)

# another data type is factors, which are categories ("levels") of data
class(murders$region)
levels(murders$region)

## Vectors

# we can create a vector using the c (concatenate) function
codes <- c(380, 124, 818)
country <- c("italy", "canada", "egypt")

# we can also name the entries in a vector if we want to - it is still numeric
codes <- c(italy=380, canada=124, egypt=818)
codes
class(codes)

# it worth noting that you can use quotes for this, and it is the exact same
codes <- c("italy"=380, "canada"=124, "egypt"=818)

# this can also be achieved using the names function
codes <- c(380, 124, 818)
country <- c("italy", "canada", "egypt")
names(codes) <- country
codes

# R has a function intended to create sequences
seq(1,10)

# you can change how much it jumps in each step
seq(1,10,2)

# there is also a shorthand if you want consecutive integers
1:10

# square brackets are used to access elements of a vector
codes[2]
codes[c(1,3)]
codes[1:2]

# when the elements are named, we can use the names to access them
codes["canada"]
codes[c("egypt", "italy")]


## Vector Coercion

# coercion is R's attempt to fit data into a particular type
# all items in a vector must be the same type, but this gives no error:
x <- c(1, "canada", 3)

# but beware: this has been coerced into characters, including the numbers
class(x)

# we can coerce things manually
x <- 1:5
y <- as.character(x)
y
as.numeric(y)

# missing data can turn vectors into characters - since missing data is filled
# with an NA
x <- c("1","b","3")
as.numeric(x)

## Sorting

# sorting sorts a vector in increasing order
library(dslabs)
data(murders)
sort(murders$total)

# however, this does not tell us the states. we want "order", which returns
# a list of indices which sorts the vector in the way we want it
x <- c(31,4,15,92,65)
x
sort(x)
index <- order(x)
x[index]

# applying this to the murders example
index <-order(murders$total)
murders$state[index]

# the biggest or smallest can be retrieved this way:
max(murders$total)

# this only gives us the number, but we can find the index this way:
i_max <- which.max(murders$total)
murders$state[i_max]
i_min <- which.min(murders$total)
murders$state[i_min]

# we can also get a list of the ranked orders of the items
rank(x)

## Vector Arithmetic

# so california has the most murders - but is it just a bigger state?
# here we check if california is the most populated state
murders$state[which.max(murders$population)]
max(murders$population)

# vector arithmetic occurs element-wise - on every element in a vector
heights <- c(69,62,66,70,70,73,67,73,67,70)
heights * 2.54
heights - mean(heights)

# this also works when using two vectors
murder_rate <- murders$total/murders$population*100000
murders$state[order(murder_rate, decreasing=TRUE)]

## Indexing

# R has many ways to index vectors based on other vectors, such as logicals
murder_rate <- murders$total/murders$population*100000

# if we want to find which states have murder rates lower than italy's:
index <- murder_rate < 0.71
index <- murder_rate <= 0.71
index

# we can use this index to tell us which entries meet the criteria
murders$state[index]

# we can also count the entries where the condition is true
sum(index)

# if we want a murder rate less than 1, and the state to be in the west:
west <- murders$region == "West"
safe <- murder_rate <= 1
index <- safe & west
murders$state[index]
# or
murders$state[(murders$region == "West") & (murder_rate <= 1)]

## Indexing Functions

# "which" gives us the entries of a vector which are true
x <- c(FALSE, TRUE, FALSE, TRUE, TRUE, FALSE)
which(x)

# if we want to check the murder rate of Mass:
index <- which(murders$state == "Massachusetts")
index
murder_rate[index]

# "match" looks for entries in a vector which match those in another vector
index <- match(c("New York", "Florida", "Texas"),murders$state)
index
murders$state[index]
murder_rate[index]

# in checks whether each element of one vector appears in another
x <- c("a", "b", "c", "d", "e")
y <- c("a", "d", "f")
y %in% x

c("Boston", "Dakota", "Washington") %in% murders$state

## Basic Data Wrangling

library(dslabs)
# dplyr is used to manipulate data tables
# install.packages("dplyr")
library(dplyr)

# this package includes various functions which are clearly named:
# mutate: lets us add a new column or change an existing one
# select: subsets the data
# we can also feed these results to one another using the pipe operator %>%

# for the murder rate example, we want all our necessary information included
# we must add the murder rate to the data frame
murders <- mutate(murders, rate=total/population*100000)
head(murders)

# note how the mutate function knows to look for total and population in the
# murders data frame, which makes things a lot shorter and easier

# if we want to filter the dataframe to show only states where the rate is
# lower than 0.71:
filter(murders, rate <= 0.71)

# we can use the select function to select specific columns
new_table <- select(murders, state, region, rate)
filter(new_table, rate <= 0.71)

# we can do all of these at once using the pipe func
murders %>% select(state, region, rate) %>% filter(rate <= 0.71)

# again, note that we do not need to specify the use of the murders table:
# it is being piped, so it is the thing being operated on.

## Creating Data Frames

# this is done using the data.frame function
grades <- data.frame(names=c("John", "Juan", "Jean", "Yao"),
                     exam_1=c(95, 80, 90, 85),
                     exam_2=c(90, 85, 85, 90))
grades

# previous versions of R changed characters into factors when doing this
class(grades$names)
# this does not seem to be a problem any more but can be fixed like this
grades <- data.frame(names=c("John", "Juan", "Jean", "Yao"),
                     exam_1=c(95, 80, 90, 85),
                     exam_2=c(90, 85, 85, 90),
                     stringsAsFactors = FALSE)

## Basic Conditionals

# the most common conditional is the if-else statement
if(a!=0){
  print(1/a)
} else{
  print("No reciprocal for 0.")
}

a <- 2

if(a!=0){
  print(1/a)
} else{
  print("No reciprocal for 0.")
}

# the general form is this:
#if (boolean condition){
#  expressions
#} else{
#  alternative expressions
#}

library(dslabs)
data(murders)
murder_rate <- murders$total/murders$population*100000

# so if we want to check which states have murder rates lower than 0.5:
ind <- which.min(murder_rate)
if(murder_rate[ind] < 0.5){
  print(murders$state[ind])
} else{print("No state has a murder rate so low.")
}

if(murder_rate[ind] < 0.25){
  print(murders$state[ind])
} else{print("No state has a murder rate so low.")
}

# we can also roll these into a single expression: ifelse
# this argument simply takes a logical and two answers
a <- 0
ifelse(a>0, print("a is bigger than 0"), print("a is not bigger than 0"))

# this also works on vectors, for example:
a <- c(0,1,2,-4,5)
result <- ifelse (a > 0, 1/a, NA)
result

# you can also use it to replace all NAs (missing) in a data set with 0s
data(na_example)
sum(is.na(na_example))

# so we begin with 145 NAs in the dataset
no_nas <- ifelse (is.na(na_example), 0, na_example)
sum(is.na(no_nas))

# the any function takes a vector of logicals and returns true if any are true
z <- c(TRUE, TRUE, FALSE)
any(z)
z <- c(FALSE, FALSE, FALSE)
any(z)  

# the all function does the same thing, but only if they are all true
z <- c(TRUE, TRUE, FALSE)
all(z)
z <- c(TRUE, TRUE, TRUE)
all(z)  

## Functions

# we can create our own functions for things we do often
# for example, we can calculate the mean of a vector x using this:
sum(x) / length(x)

# we could define this as a function like so:
avg <- function(x){
  s <- sum(x)
  n <- length(x)
  s/n
}

x <- 1:100
avg(x)

# we can see that these are the same thing:
identical(mean(x), avg(x))

# objects such as s, used inside the functions, are not saved in the workspace
s <- 3
avg(1:10)
s

# the general form of this:
# my_function <- function(x,y,z){
#  operations that operate on x,y,z defined by the user of the function
#  final line is returned
# }

# you can also use an if/else statement and a boolean argument like this:
avg <- function(x, arithmetic=TRUE){
  n <- length(x)
  ifelse(arithmetic, sum(x)/n, prod(x)^(1/n))
}
# this function takes a boolean (defaulting to true) as an argument, then
# based on whether it is true or false, it gives either the arithmetic
# or geometric mean. 

## For Loops

# in maths, to add up a series of numbers up to n, we can use (n(n+1))/2
# but we can use R to check this is correct
# we first define a function to add up the series:
compute_s_n <- function(n){
  x <- 1:n
  sum(x)
}
compute_s_n(3)
compute_s_n(1337)

# so now we want to compute it many times, one for each n up to 25
# we can do this without typing the entire thing out 25 times using a for loop
# we tend to use i in for loops, but it can be anything
for(i in 1:5){
  print(i)
}


m <- 25
# create an empty vector
s_n <- vector(length = m)

for(n in 1:m){
  s_n[n] <- compute_s_n(n)
}

# this goes through the numbers from 1:m, and for each one, it calls the 
# compute_s_n function. it then stores the result in the nth entry of 
# the empty vector we created.

# we can check this via plotting
n <- 1:m
plot(n, s_n)

# or by checking the formula results
(n*(n+1))/2
s_n

# we can even plot them together
plot(n, s_n)
lines(n, (n*(n+1))/2)

## Other Functions

# for loops are rarely used in R.
# instead, we use apply, sapply, tapply and mapply (apply family)
# also split, cut, quantile, reduce, indentical, unique, etc
