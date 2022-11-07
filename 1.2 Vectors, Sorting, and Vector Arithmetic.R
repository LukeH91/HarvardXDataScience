## Vectors

# the c (concatenate) function can make vectors quickly
codes <- c(380, 124, 818)
country <- c("italy", "canada", "egypt")

# we can also name the entries of a vector
codes <- c(italy=380, canada=124, egypt=818)
codes
# but the vector is still numeric
class(codes)
# the above code would be the exactly the same even if it had quotes
codes <- c("italy"=380, "canada"=124, "egypt"=818)
codes

# you can also use the names function to assign names to a vector
codes <- c(380, 124, 818)
country <- c("italy", "canada", "egypt")
names(codes) <- country
codes

# the sequence function can generate vectors quickly too
seq(1,10)
seq(1,10,0.1)
1:10

# these sequences can be used for subsetting, or selecting chunks of a vector
codes[2]
codes[2:3]
codes[1:3]
codes[c(1,3)]
# if your elements are named, you can use those too
codes["canada"]
codes[c("canada","italy")]


## Vector Coercion

# R attempts to figure out which data types a vector should be
x <- c(1, "canada", 3)
x
class(x)
# so x is converted to characters - even 1 and 3 - the data has been "coerced"
# you can do this yourself as well
x <- 1:5
y <- as.character(x)
y
y <- as.numeric(y)
y

# if we attempt to coerce into an incompatible type, we get an NA
x <- c("1","b","3")
as.numeric(x)

# loading the dslabs package and the murders dataset
library(dslabs)
data(murders)

# the sort function can sort a vector in increasing order
sort(murders$total)
# but this only tells us about the total, not anything else
# instead, we can use order to give us a vector which will sort the vector parameter
x <- c(31,4,15,92,65)
x
sort(x)
order(x)
# we can therefore assign the order of x to its own variable, then use it to sort the vector
index <- order(x)
x[index]

# this means that we can use this index, sorted by murders, and apply it to state names
murder_order <- order(murders$total)
# this will give us the state names ordered by the amount of murders
murders$state[murder_order]

# there is a quicker way of doing this
# this tells us the maximum
max(murders$total)
# and this tells us the index location where it occurs
which.max(murders$total)
# we can use this index location to tell us the name of the state
i_max <- which.max(murders$total)
murders$state[i_max]
# the same thing works for minimums also
i_min <- which.min(murders$total)
murders$state[i_min]

# we can also create a list of ranks of the items in the vector
x <- c(31,4,15,92,65)
x
rank(x)

# We know that california has the most murders, but we need to control for population
murders$state[which.max(murders$population)]
max(murders$population)

# arithmetic operations on vectors occur element-wise
heights <- c(69,62,66,70,70,73,67,73,67,70)
# we can convert it to inches
heights * 2.54
# or measure deviation from the mean
heights - 69

# if we have two vectors of equal length, operations occur pairwise
# we divide total murders by population, then multiply by 100k to get per 100k stats
murder_rate <- (murders$total/murders$population)*100000
# we can then use this to order the list of states as normal
murders$state[order(murder_rate, decreasing=TRUE)]
