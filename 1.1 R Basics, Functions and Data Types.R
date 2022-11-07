## Installing and loading necessary packages

install.packages("dslabs")
library(dslabs)

install.packages("tidyverse")
library(tidyverse)

## Objects

# assigning values to variables
a <- 1
b <- 1
c <- -1

# making R show us a variable's contents
a
print(a)

# view entire workspace
ls()

# solving the quadratic equation
(-b + sqrt(b^2 - 4*a*c))/(2*a)
(-b - sqrt(b^2 - 4*a*c))/(2*a)


## Functions

# to evaluate a function, we must follow it with parentheses.
# if we do not, it will simply give us all the code for the function
ls
ls()

# we can also nest functions together, for example:
log(2)
exp(log(2))

# functions come with help files
help(log)
?log
?"+"

# R has some built in data as well
data()

# objects can have different types
a <- 2
class(a)
class(ls)
z <- "a" == 2
class(z)

# loading the dslabs library and murders dataset
library(dslabs)
data("murders")
class(murders)
# to look at the structures of an object, we use str
str(murders)
# and we can look at the head
head(murders)
# columns within the data frame can be accessed using the dollar sign
murders$state
# we can have the dataframe list the columns using names
names(murders)
pop <- murders$population
length(pop)

# we can also have factors, which are used to store categorical data
# this is done because it is more memory-efficient, as they are stored as integers
class(murders$region)
levels(murders$region)