## Moneyball

# regression has been used to predict which players are best in baseball
# 1. we will determine which player-specific stats predict wins
# 2. we will examine which players, based on the above, are undervalued

## Baseball Basics

# the goal is to score more runs than the other team
# each time has 9 batters, who bat in order and then repeat. each batting is 
# a "plate appearance" (PA). each PA has a vinary outcome - you make an "out", 
# a failure, and sit back down, or you hit it and run around the bases
# each team gets 9 tries, innings, to score a run, and each inning ends after 3 outs
# the batting average has been considered the most important statistic:
# this consists of hits and at bats, with at bats being the number of times
# you make a hit or make an out - base on balls are excluded
# therefore, the batting average is hits / at bats - generally ranges from .2 to .38
# we refer to these in thousands, so .25 is batting a 250

## Bases on Balls or Stolen Bases?

# do teams that hit more home runs score more runs?
library(Lahman)
library(tidyverse)
library(dslabs)
ds_theme_set()

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR / G, R_per_game = R / G) %>%
  ggplot(aes(HR_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

# what about stolen bases and wins?
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(SB_per_game = SB / G, R_per_game = R / G) %>%
  ggplot(aes(SB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

# and bases on balls and runs
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB / G, R_per_game = R / G) %>%
  ggplot(aes(BB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

# so do bases on balls cause runs? it could be tht home runs cause bases on balls
# if this is the case, home runs is a confounding variable - it explains both

# Make a scatterplot of runs per game versus at bats (AB) per game.
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(AB_per_game = AB / G, R_per_game = R / G) %>%
  ggplot(aes(AB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

# Make a scatterplot of win rate (number of wins per game) versus number of 
# fielding errors (E) per game.
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(W_per_game = W / G, E_per_game = E / G) %>%
  ggplot(aes(W_per_game, E_per_game)) + 
  geom_point(alpha = 0.5)

# Make a scatterplot of triples (X3B) per game versus doubles (X2B) per game.
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(X3B_per_game = X3B / G, X2B_per_game = X2B / G) %>%
  ggplot(aes(X3B_per_game, X2B_per_game)) + 
  geom_point(alpha = 0.5)

## Correlation

# Galton invented correlation by examining the heredity of heights
# create the dataset
library(tidyverse)
library(HistData)
data("GaltonFamilies")
set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

# because the variables are approximately normal, we can use two averages and
# two standard deviations to summarise them
galton_heights %>%
  summarize(mean(father), sd(father), mean(son), sd(son))

# however, the summary statistics do not tell us this:
galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5)
# the taller the father, the taller the son

## Correlation Coefficient

# for our data set, we can find the correlation like this:
galton_heights %>% summarize(cor(father, son))

## Sample Correlation is a Random Variable

# the sample correlation is the most commonly used estimate of the population
# correlation, so it is a random variable. 
# if our sample of 179 father/son pairs was the whole population, and we sample 25:
my_sample <- slice_sample(galton_heights, n=25, replace=TRUE)
R <- my_sample %>% summarize(cor(father, son))
# R is a random variable, and we can see its distribution using a monte carlo sim
B <- 1000
N <- 25
R <- replicate(B, {
  slice_sample(galton_heights, n = N, replace = TRUE) %>% 
    summarize(r=cor(father, son)) %>% .$r
})
data.frame(R) %>% ggplot(aes(R)) + geom_histogram(binwidth = 0.05, color = "black")

# expected value is the population correlation
mean(R)
# standard error is high relative to its size
sd(R)

# QQ-plot to evaluate whether N is large enough
data.frame(R) %>%
  ggplot(aes(sample = R)) +
  stat_qq() +
  geom_abline(intercept = mean(R), slope = sqrt((1-mean(R)^2)/(N-2)))

data(Teams)
# What is the correlation coefficient between number of runs per game and number 
# of at bats per game?
Teams <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(AB_per_game = AB / G, R_per_game = R / G)
cor(Teams$AB_per_game, Teams$R_per_game)

# What is the correlation coefficient between win rate (number of wins per game) 
# and number of errors per game?
Teams <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(W_per_game = W / G, E_per_game = E / G)
cor(Teams$W_per_game, Teams$E_per_game)

# What is the correlation coefficient between doubles (X2B) per game and triples (X3B) per game?
Teams <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(X3B_per_game = X3B / G, X2B_per_game = X2B / G)
cor(Teams$X3B_per_game, Teams$X2B_per_game)

## Anscome's Quartet

# a correlation coefficient is not an ideal way of showing a relationship
# this is because one correlation coefficient can be many patterns

## Stratification

data("GaltonFamilies")
set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)
# if we have the guess the height of a random son, and the average height is 69 
# inches, this would be the best prediction. however, if we know his father is 72
# inches (1.1 standard deviations above avg), do we expect the son will be 1.1
# sds above the mean too? no, this would be an overestimation. 
# if we stratify the data for fathers - a conditional average, because it is 
# conditioned on the father being 72 inches tall - we reduce our sample, because
# there are only 8 such fathers in the sample
sum(galton_heights$father == 72)
# if we want exactly 72.5, there is only one
sum(galton_heights$father == 72.5)
# this means our standard errors will be huge
# therefore, instead, we pick fathers with similar heights using stratification
# we will round father heights to the nearest inch, then look at 72
conditional_avg <- galton_heights %>%
  filter(round(father) == 72) %>%
  summarize(avg = mean(son)) %>%
  pull(avg)
conditional_avg
# so the son is predicted to be 70.5 inches tall, 0.5 sds above the mean

# we can also see a boxplot of each strata
galton_heights %>% mutate(father_strata = factor(round(father))) %>%
  ggplot(aes(father_strata, son)) +
  geom_boxplot() +
  geom_point()

# the means follow a linear relationship, with a slope of around 0.45
galton_heights %>%
  mutate(father = round(father)) %>%
  group_by(father) %>%
  summarize(son_conditional_avg = mean(son)) %>%
  ggplot(aes(father, son_conditional_avg)) +
  geom_point()

# this is no coincidence: the slope of the line is the correlation between
# father and son heights - this is the regression line
r <- galton_heights %>% summarize(r = cor(father, son)) %>% pull(r)
galton_heights %>% 
  mutate(father = scale(father), son = scale(son)) %>%
  mutate(father = round(father)) %>%
  group_by(father) %>%
  summarize(son = mean(son)) %>%
  ggplot(aes(father, son)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = r)

# we can add a regression line to original data
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m <-  r * s_y / s_x
b <- mu_y - m*mu_x

galton_heights %>% 
  ggplot(aes(father, son)) + 
  geom_point(alpha = 0.5) +
  geom_abline(intercept = b, slope = m )

# plot in standard units and see that intercept is 0 and slope is rho
galton_heights %>% 
  ggplot(aes(scale(father), scale(son))) + 
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = r)

# by using the regression line instead of stratifying the data, we use
# the entire data set to make the prediction instead of just those fathers
# who were around 72 inches. this makes the estimation far more stable. 

## Bivariate Normal Distribution

# when a pair of random variables is estimated using a bivariate normal,
# the scatter plots look like a thin oval with strong correlation
# and a wide oval with weak/no correlation
# here we stratify son heights by standardised father heights, and can see that the
# data is approximately normal
galton_heights %>%
  mutate(z_father = round((father - mean(father))/sd(father))) %>%
  filter(z_father %in% -2:2) %>%
  ggplot() +  
  stat_qq(aes(sample=son)) +
  facet_wrap(~z_father)

## Two Regression Lines

# we predicted the son's height from the father's height using regression
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m <-  r * s_y / s_x
b <- mu_y - m*mu_x

# but what if we wanted to predict the father's height from the son's height?
# we do not simply use the inverse of this code, instead we use this:
m <-  r * s_x / s_y
b <- mu_x - m*mu_y

## Assessment 
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

# Calculate the mean and standard deviation of mothers' heights, the mean and 
# standard deviation of daughters' heights, and the correlaton coefficient between
# mother and daughter heights.
female_heights %>%
  summarize(mean(mother), sd(mother), mean(daughter), sd(daughter))
female_heights %>% summarize(cor(mother, daughter))

# Calculate the slope and intercept of the regression line predicting daughters' 
# heights given mothers' heights.
mu_x <- mean(female_heights$mother)
mu_y <- mean(female_heights$daughter)
s_x <- sd(female_heights$mother)
s_y <- sd(female_heights$daughter)
r <- cor(female_heights$mother, female_heights$daughter)
slope <-  r * s_y / s_x
intercept <- mu_y - m*mu_x
# What percent of the variability in daughter heights is explained by the mother's height?
r^2
# What is the conditional expected value of her daughter's height given a mother's height of 60?
intercept + (slope*60)
