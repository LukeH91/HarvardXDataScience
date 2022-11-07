## Indexing 

# loading the dslabs package and the murders dataset
library(dslabs)
data(murders)
# adding murder rate variable
murder_rate <- murders$total / murders$population * 100000

## Indexing

# We can use logicals to index vectors
index <- murder_rate <=0.71
index
murders$state[index]
# we can also sum an index to see how many TRUEs exist
sum(index)

# we can use & to find states which meet both requirements
west <- murders$region  == "West"
safe <- murder_rate <= 1
murders$state[west & safe]


## Indexing Functions

# "which" gives us the entries of a vector which are true
# in other words, it tells us directly which entries meet a criteria
x <- c(FALSE, TRUE, FALSE, TRUE, TRUE, FALSE)
which(x)
# this is good when the vector is very long, so we only want the indices
index <- which(murders$state == "Massachusetts")
index
murder_rate[index]

# match looks for entries in a vector and gives us the indexes needed for them
index <- match(c("New York", "Florida", "Texas"), murders$state)
index
murders$state[index]
murder_rate[index]

# %in% tells us whether each element of the first vector is in a second vector
# this does not give us the index
x <- c("a", "b", "c", "d", "e")
y <- c("a", "d", "x")
y %in% x

c("Boston", "Dakota", "Washington") %in% murders$state

## Basic Data Wrangling

install.packages("dplyr")
library(dplyr)

# dplyr allows us to perform common data operations easily

# we first need to add the murder rate variable to our data frame using mutate
# this takes the df as the first argument and the name and value as the second
murders <- mutate(murders,rate=total/population*100000)
# the function knows that total and population are in the data frame
head(murders)

# we can also filter the data table to show certain entries
filter(murders,rate <= 0.71)

# select allows us to only work with specific columns
new_table <- select(murders,state,region,rate)
filter(new_table,rate <= 0.71)

# instead of defining a new object, we can use the pipe, %>%
# the pipe basically passes the result of one function into another
murders %>% select(state,region,rate) %>% filter(rate<=0.71)

## Creating Data Frames

# to create a data frame, we use the data.frame function
grades <- data.frame(names=c("John","Juan","Jean","Yao"),
                     exam_1=c(95,80,90,85),
                     exam_2=c(90,85,85,90))
grades
# if we want to avoid characters being turned into classes
# (which does not happen on R 4.0 by default)
# we can add stringsAsFactors = FALSE as an argument

## Basic Plots

# to create a basic scatter plot, we first must create the two variables

population_in_millions <- murders$population/1000000
total_gun_murders <- murders$total
plot(population_in_millions, total_gun_murders)

# histograms simply need a variable
hist(murders$rate)

# boxplots can be split by a factor using ~
boxplot(rate~region, data=murders)
