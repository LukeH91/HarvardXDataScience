## Are BBs more predictive?

# the slope of a regression line predicting runs from bases on balls is 0.735
library(tidyverse)
library(Lahman)
get_slope <- function(x, y) cor(x, y) * sd(y) / sd(x)

bb_slope <- Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(BB_per_game = BB/G, R_per_game = R/G) %>% 
  summarize(slope = get_slope(BB_per_game, R_per_game))

bb_slope 

# so if we hire cheap players with many bases on balls, we get 1.5 more runs per game?
# correlation is not causation!
# 2 more bases on balls per game than the average team scores 1.5 more runs per game
# but we do not have evidence that BBs are the cause
# predicting runs from singles gives us 0.5 correlation, but these give you a 
# better chance of scoring than a base on ball. so why is the assocation stronger?
singles_slope <- Teams %>% 
  filter(yearID %in% 1961:2001 ) %>%
  mutate(Singles_per_game = (H-HR-X2B-X3B)/G, R_per_game = R/G) %>%
  summarize(slope = get_slope(Singles_per_game, R_per_game))
singles_slope 

# the answer is confounding
Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(Singles = (H-HR-X2B-X3B)/G, BB = BB/G, HR = HR/G) %>%  
  summarize(cor(BB, HR), cor(Singles, HR), cor(BB,Singles))
# we see that BBs are correlated with home runs - it turns out that pitchers, 
# afraid of batters who regularly hit HRs, sometimes deliberately throw BBs to them
# therefore, to see the true effect of BBs on HRs, we must control for HRs.

## Multivariate Regression

# we must fix home runs at a certain value, then look at the relationship between
# runs and BB. we will stratify HR to the nearest 10th. 
# we first generate an informative data set
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_strata = round(HR/G, 1), 
         BB_per_game = BB / G,
         R_per_game = R / G) %>%
  filter(HR_strata >= 0.4 & HR_strata <=1.2)

# then create a scatterplot of runs per BB for each strata of HRs
dat %>% 
  ggplot(aes(BB_per_game, R_per_game)) +  
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ HR_strata)

# calculate slope of regression line after stratifying by HR
dat %>%  
  group_by(HR_strata) %>%
  summarize(slope = cor(BB_per_game, R_per_game)*sd(R_per_game)/sd(BB_per_game))
# these values are mostly closer to the slope we obtain from singles (0.449)
# which makes sense, since both of those get us to first base

# we now want to check if BBs cause home runs or vice-versa
# stratify by BB
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_strata = round(BB/G, 1), 
         HR_per_game = HR / G,
         R_per_game = R / G) %>%
  filter(BB_strata >= 2.8 & BB_strata <=3.9) 

# scatterplot for each BB stratum
dat %>% ggplot(aes(HR_per_game, R_per_game)) +  
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ BB_strata)

# slope of regression line after stratifying by BB
dat %>%  
  group_by(BB_strata) %>%
  summarize(slope = cor(HR_per_game, R_per_game)*sd(R_per_game)/sd(HR_per_game)) 
# the original slope estimate was 1.8, so these are not far off. 

# so if we stratify by HRs we get a normal bivariate distribution for runs vs BBs
# similarly, stratifying by BBs gives us a normal distribution for runs vs HRs
# rather than fitting a line to each strata, we can use a simpler model: we fix 
# the number of home runs, and then we see a linear relationship between 
# runs and BBs. this slope does not depend on the number of HRs.

## Linear Models

# MLR helps us overcome confounds. e.g. if you want to examine the effect of fast
# food on health, you will find a huge impact - but those who eat fast food are
# also more likely to drink, smoke, etc. you need to control for these.


## Least Squares Estimates

# linear models estimate the betas, and they do this by minimising the distance
# from the fitted line to the data (the RSS). 
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

# compute RSS for any pair of beta0 and beta1 in Galton's data
rss <- function(beta0, beta1){
  resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
  return(sum(resid^2))
}

# to prevent this being a 3d plot, plot RSS as a function of beta1 when beta0=25
beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 25))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss))

## The lm Function

# a simple way to run a linear regression model is the lm command
# fit regression line to predict son's height from father's height
#  the predicted goes on the left of the tilde, and the predictor on the right
fit <- lm(son ~ father, data = galton_heights)
fit

# summary statistics give us even more information about the fit
summary(fit)

## LSEs are random variables

# since LSEs are derived from the (random) data, they are random themselves.
# we can prove this with a monte carlo simulation sampling 50 pairs from 
# the son/father data 1000 times
B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% 
    .$coef 
})
lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 

# to see the variability of the estimates, we plot the distribution of beta_0 and beta_1
library(gridExtra)
p1 <- lse %>% ggplot(aes(beta_0)) + geom_histogram(binwidth = 5, color = "black") 
p2 <- lse %>% ggplot(aes(beta_1)) + geom_histogram(binwidth = 0.1, color = "black") 
grid.arrange(p1, p2, ncol = 2)
# these are normal because the CLT applies here as well

# summary statistics provide the standard errors
sample_n(galton_heights, N, replace = TRUE) %>% 
  lm(son ~ father, data = .) %>% 
  summary

lse %>% summarize(se_0 = sd(beta_0), se_1 = sd(beta_1))

## Predicted Variables are Random Variables

# if we want to plot confidence intervals around a prediction, we can use the
# method = "lm" argument in geom_smooth
galton_heights %>% ggplot(aes(father, son)) +
  geom_point() +
  geom_smooth(method = "lm")

# the function predict() takes an LM object and returns these predictions
fit <- galton_heights %>% lm(son ~ father, data = .) 
Y_hat <- predict(fit, se.fit = TRUE)
names(Y_hat)

# plot best fit line
galton_heights %>%
  mutate(Y_hat = predict(lm(son ~ father, data=.))) %>%
  ggplot(aes(father, Y_hat))+
  geom_line()



## Assessment

# Load the Lahman library and filter the Teams data frame to the years 1961-2001. 
# Mutate the dataset to create variables for bases on balls per game, runs per game,
# and home runs per game, then run a linear model in R predicting the number of runs
# per game based on both the number of bases on balls per game and the number of home
# runs per game.
library(Lahman)
Teams <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(BB_per_game = BB/G, R_per_game = R/G, HR_per_game=HR/G)

Teams %>% lm(R_per_game ~ HR_per_game+BB_per_game,data = .)


# Female Heights
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")
options(digits = 3)    # report 3 significant digits

female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

# Fit a linear regression model predicting the mothers' heights using daughters' heights.
female_heights %>% lm(mother ~ daughter,data = .)

# Predict mothers' heights using the model from Question 7 and the predict() function.
fit <- female_heights %>% lm(mother ~ daughter,data = .)
predict(fit, se.fit = TRUE)

## Baseball
 
# Before we get started, we want to generate two tables: one for 2002 and another 
# for the average of 1999-2001 seasons. We want to define per plate appearance 
# statistics, keeping only players with more than 100 plate appearances. Here is 
# how we create the 2002 table:
library(Lahman)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

# selecting columns and variables
bat_01 <- Batting %>% filter(yearID %in% c(1999, 2000, 2001)) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb) 
# grouping by player ID and averaging
bat_01 <- bat_01 %>% group_by(playerID) %>%
  summarise(across(.cols=everything(), mean, na.rm=TRUE))
# How many players had a single rate mean_singles of greater than 0.2 per plate appearance over 1999-2001?
bat_01 %>% filter(singles > 0.2)
# How many players had a BB rate mean_bb of greater than 0.2 per plate appearance over 1999-2001?
bat_01 %>% filter(bb > 0.2)
# Use inner_join() to combine the bat_02 table with the table of 1999-2001 rate 
# averages you created in the previous question.
comb <- inner_join(bat_01, bat_02, by="playerID")
comb %>% summarize(cor(singles.x, singles.y))
# Make scatterplots of mean_singles versus singles and mean_bb versus bb.
comb %>% ggplot(aes(singles.x, singles.y)) +  
  geom_point(alpha = 0.5)
comb %>% ggplot(aes(bb.x, bb.y)) +  
  geom_point(alpha = 0.5)
# Fit a linear model to predict 2002 singles given 1999-2001 mean_singles.
comb %>% lm(singles.y ~ singles.x, data=.)
# Fit a linear model to predict 2002 bb given 1999-2001 mean_bb.
comb %>% lm(bb.y ~ bb.x, data=.)
