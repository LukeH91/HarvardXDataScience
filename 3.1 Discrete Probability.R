## Monte Carlo Simulations

# R has a function which we can use to generate random numbers
# imagine we have an urn with 2 red beads and 3 blue beads
# to do this we use the repeat function, which takes a vector and then a
# vector telling it how many times to repeat each element
beads <- rep(c("red", "blue"), times=c(2, 3))
# we can use the sample function to pick one at random
sample(beads, 1)
# the replicate function allows us to repeat this task many times
B <- 10000
replicate(B, sample(beads, 1))
# if we save this as an object, we can view its distribution with table
events <- replicate(B, sample(beads, 1))
tab <- table(events)
# and then its proportions with prop.table
prop.table(tab)

# replicate was used for this example, but sample allows us to pick more than 
# one object from the urn
# to do this for a sample larger than the population, we activate replacement
sample(beads, 1000, replace = TRUE)

# also, you can use the mean function to get the proportions of a vector which 
# are TRUE. this is because TRUE is counted as a 1, and FALSE as a 0.
# for example, if you want to check how many beads were blue:
sample1 <- sample(beads, 100, replace=TRUE)
sample1 == "blue"
sum(sample1 == "blue")
mean(sample1 == "blue")


## Important Note:
# If you want to set the seed, you can do so like this:
set.seed(1986)
# If you are using materials from before 2019, you will need to set the seed
# like this, since the method was since changed:
set.seed(1, sample.kind="Rounding")

## Independence

# events are independent if they do not affect one another (coin tosses)
# if we do not replace after sampling, then it is not independent

# in the beads example, the first draw is most likely to be blue
# but if we look at the last 4 draws, we know automatically what the first was
x <- (sample(beads, 5))
x[2:5]

# when two events are not independent, we must use conditional probability
# in this case, we multiply the probabilities together

## Combinations and Permutations

# to demonstrate this, we create a deck of cards using paste and expand.grid
# paste pastes two things together, and also works on vectors
paste(letters[1:5], as.character(1:5))

# expand.grid creates a grid showing all possible combinations of inputs
expand.grid(pants = c("blue", "black"), shirt = c("white", "red", "yellow"))

# using these, we create a deck of cards
suits <- c("Spades", "Hearts", "Clubs", "Diamonds")
cards <- c("Ace", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", 
           "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number=cards, suit=suits)
deck <- paste(deck$number, deck$suit)

# we can now use this deck to calculate some probabilities
# we create a vector showing the four ways to get a king
kings <- paste("King", suits)
# we can then check the probability that the first card is a king
mean(deck %in% kings)

# but what about the probability that the second card is a king?
# we need to use the combinations and permutations funcions from gtools
library(gtools)

# permutations computes, for a list of size n, how many ways we can select r 
# items. note that order matters: 3,1 is not the same as 1,3
permutations(5,2)

# for example, if you want 5 random 7-digit numbers:
# generate all phone numbers
all_phone_numbers <- permutations(10,7,v=0:9)
# create an object with the index numbers of all of them
n <-nrow(all_phone_numbers)
# pick 5 at random (by index number)
index <- sample(n,5)
# display these 5 from the list
all_phone_numbers[index,]

# to omputer how many ways we can pick 2 cards when order matters, we use this
# 52 cards, 2 being chosen, taken from the vector deck
hands <- permutations(52,2, v=deck)
# this produces a 2-column vector with 2652 rows, for every permutation
#now we define the first and second cards as objects by taking the columns
first_card <- hands[,1]
second_card <- hands[,2]
# we can now check how many cases have the first card as a king
sum(first_card %in% kings)
# and then both first and second
sum(first_card %in% kings & second_card %in% kings) / sum(first_card %in% kings)

# if order does not matter, we use combinations, not permutations
# observe the difference:
permutations(3,2)
combinations(3,2)

# so to compute how many ways we could hit 21 in blackjack:
aces <- paste("Ace", suits)
facecard <- c("King", "Queen", "Jack", "Ten")
facecard <- expand.grid(number=facecard, suit=suits)
facecard <- paste(facecard$number, facecard$suit)
hands <- combinations(52,2, v=deck)
# how often do we get an ace and a face?
mean(hands[,1] %in% aces & hands[,2] %in% facecard)

# we can also continually draw samples until we calcualte the propotion:
B <- 10000
results <- replicate(B, {
  hand <- sample(deck, 2)
  (hand[1] %in% aces & hand[2] %in% facecard) |
    (hand[2] %in% aces & hand[1] %in% facecard)
  })
mean(results)

## The Birthday Paradox
# what are the odds that two people in a group of 50 have the same birthday?
# a birthday is just a number between 1 and 365, so we can sample them:
n <- 50
bdays <- sample(1:365, n, replace=TRUE)
# the function duplicated returns true if two things in a set are the same
duplicated(bdays)
any(duplicated(bdays))
# again, we can run a monte carlo simulation to estimate the chance of this
B <- 10000
results <- replicate(B, {
  bdays <- sample(1:365, n, replace=TRUE)
  any(duplicated(bdays))
  })
mean(results)

## Sapply

# we will now create a lookup table to examine how the probability changes
# with group size
# we will first define a function which checks whether a duplicate exists
# in a sample of size n
compute_prob <- function(n, B=10000){
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace=TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}
# we can then define n as a sequence from 1-60
n <- seq(1,60)
# we could then run a for loop, but these loops are not used often in R
# instead, we tend to perform operations on entire vectors, but the code we
# wrote is expecting a scalar (n). instead we must use sapply
compute_prob(n)
# sapply lets us feed a vector to any function and use it element-wise
x <- 1:10
sapply(x, sqrt)
# therfore, we can do this:
prob <- sapply(n, compute_prob)
# and create a simple plot
plot(n, prob)

# instead of using a monte carlo, we can calculate the exact probability
# we will examine the odds of people NOT sharing a birthday, since this means
# we can use the multiplication rule
exact_prob <- function(n){
  prob_unique <- seq(365, 365-n-1)/365
  1-prod(prob_unique)
}
eprob <- sapply(n, exact_prob)
# we can then plot them against each other
plot(n, prob)
lines(n, eprob, col="red")

## How many samples is enough?

# this is a tough question, but we can check when the estimate becomes stable
# defines vector of many B values
B <- 10^seq(1, 5, len = 100)    
# function to run Monte Carlo simulation with each B
compute_prob <- function(B, n = 22){    
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}

# apply compute_prob to many values of B
prob <- sapply(B, compute_prob)    
# plot a line graph of estimates 
plot(log10(B), prob, type = "l")   

## The Addition Rule

# another way to calculate the odds of a 21 in blackjack is:
# you can get an ace or a face card, or a face card and an ace
# so if you calculate these seperately, you can just add them together
# probability of ace and face:
(4/52) * (16/51)
# probability of face and ace:
(16/52) * (4/51)
# therefore:
((4/52) * (16/51)) + ((16/52) * (4/51))

## Monty Hall Problem

# contestants must pick 1 of 3 doors, with a prize behind one. once they make 
# their choice, one of the other two doors is opened, revealing nothing. 
# what should the contestant do? should they switch doors?
# the odds are not 50/50 - they are 1/3 if you don't switch doors. 
# we first create a simulation of sticking to the same door
stick <- replicate(B, {
  doors <- as.character(1:3)
  # assign prize to a door
  prize <- sample(c("car", "goat", "goat"))
  # we then note the number of the prize door
  prize_door <- doors[prize=="car"]
  # your pick is selected randomly from the three
  my_pick <- sample(doors, 1)
  # we determine which door you are shown - this is not the pick or prize
  show <- sample(doors[!doors %in% c(my_pick, prize_door)], 1)
  # we then determine that you stick to the door
  stick <- my_pick
  # w then check if your door is the prize door
  stick == prize_door
})

mean(stick)
# so the real chance is 1/3
# but if we change:
change <- replicate(B, {
  doors <- as.character(1:3)
  # assign prize to a door
  prize <- sample(c("car", "goat", "goat"))
  # we then note the number of the prize door
  prize_door <- doors[prize=="car"]
  # your pick is selected randomly from the three
  my_pick <- sample(doors, 1)
  # we determine which door you are shown - this is not the pick or prize
  show <- sample(doors[!doors %in% c(my_pick, prize_door)], 1)
  # we then determine that you stick to the door
  change <- doors[!doors %in% c(my_pick, show)]
  # w then check if your door is the prize door
  change == prize_door
})
mean(change)

## Assessment

library(gtools)
library(tidyverse)

head(esoph)

all_cases <- sum(esoph$ncases)
all_controls <- sum(esoph$ncontrols)

# What is the probability that a subject in the highest alcohol consumption group is a cancer case?
boozers <- esoph %>% filter(alcgp == "120+") 
(sum(boozers$ncases)/(sum(boozers$ncontrols)+sum(boozers$ncases)))

# What is the probability that a subject in the lowest alcohol consumption group is a cancer case?
nonboozers <- esoph %>% filter(alcgp == "0-39g/day")
(sum(nonboozers$ncases)/(sum(nonboozers$ncontrols)+sum(nonboozers$ncases)))

# Given that a person is a case, what is the probability that they smoke 10g or more a day?
smokers <- esoph %>% filter(tobgp!="0-9g/day")
sum(smokers$ncases)/sum(esoph$ncases)

# Given that a person is a control, what is the probability that they smoke 10g or more a day?
sum(smokers$ncontrols)/sum(esoph$ncontrols)

# For cases, what is the probability of being in the highest alcohol group?
sum(boozers$ncases)/sum(esoph$ncases)

# For cases, what is the probability of being in the highest tobacco group?
megasmokers <- esoph %>% filter(tobgp == "30+") 


# For cases, what is the probability of being in the highest alcohol group and the highest tobacco group?
boozmoke <- esoph %>% filter(alcgp == "120+" & tobgp =="30+")
sum(boozmoke$ncases)/all_cases

# For cases, what is the probability of being in the highest alcohol group or the highest tobacco group?
boozor <- esoph %>% filter(alcgp == "120+" | tobgp =="30+")
sum(boozor$ncases)/all_cases

# For controls, what is the probability of being in the highest alcohol group?
sum(boozers$ncontrols)/sum(esoph$ncontrols)

# How many times more likely are cases than controls to be in the highest alcohol group?
(sum(boozers$ncases)/all_cases) / (sum(boozers$ncontrols)/all_controls)

# For controls, what is the probability of being in the highest tobacco group?
sum(megasmokers$ncontrols)/sum(esoph$ncontrols)

# For controls, what is the probability of being in the highest alcohol group and the highest tobacco group?
toband <- esoph %>% filter(alcgp=="120+" & tobgp == "30+")
sum(toband$ncontrols)/all_controls

# For controls, what is the probability of being in the highest alcohol group OR the highest tobacco group?
tobor <- esoph %>% filter(alcgp=="120+" | tobgp == "30+")
sum(tobor$ncontrols)/all_controls

# How many times more likely are cases than controls to be in the highest alcohol group or the highest tobacco group?
boozorsmok <- esoph %>% filter(alcgp=="120+" | tobgp == "30+")
(sum(boozorsmok$ncases) / all_cases) / 
  (sum(boozorsmok$ncontrols) / all_controls)
