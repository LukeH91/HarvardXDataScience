## Spurious Correlation

library(tidyverse)
library(dplyr)
library(ggplot2)
library(dslabs)

# spurious correlations are correlations which are strong or significant
# but actually are unrelated - such as divorces and margarine consumption

# a monte carlo sim can show us how this happens:
# generate the Monte Carlo simulation
N <- 25
g <- 1000000
sim_data <- tibble(group = rep(1:g, each = N), x = rnorm(N * g), y = rnorm(N * g))

# because we built this from normally distributed data, we know x and Y and not correlated
# calculate correlation between X,Y for each group
res <- sim_data %>% 
  group_by(group) %>% 
  summarize(r = cor(x, y)) %>% 
  arrange(desc(r))
res

# however, when we plot them, we find compelling evidence of a correlation somewhere
# plot points from the group with maximum correlation
sim_data %>% filter(group == res$group[which.max(res$r)]) %>%
  ggplot(aes(x, y)) +
  geom_point() + 
  geom_smooth(method = "lm")

# since both variables are random, their correlations are normal
# histogram of correlation in Monte Carlo simulations
res %>% ggplot(aes(x=r)) + geom_histogram(binwidth = 0.1, color = "black")

# if we ran a regression on this, the p-value would be very low

## Outliers

# outliers can also cause a correlation where there would not be one otherwise

# imagine we take measurements for x and y, and standardise it, but forget to
# standardise one data point

# simulate independent X, Y and standardize all except entry 23
set.seed(1985)
x <- rnorm(100,100,1)
y <- rnorm(100,84,1)
x[-23] <- scale(x[-23])
y[-23] <- scale(y[-23])

# plot shows the outlier
qplot(x, y, alpha = 0.5)

# because a line can be fitted to this pattern, the outlier makes it seem like
# there is a correlation
cor(x,y)
cor(x[-23], y[-23])

# to fix this, we can use the spearman ranked correlation
qplot(rank(x), rank(y))
cor(rank(x), rank(y))
cor(x, y, method = "spearman")

## Reversing Cause and Effect

# sometimes we might mistake the effect for the cause, such as
# using son heights to predict father heights
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

galton_heights %>% summarize(tidy(lm(father ~ son)))

## Confounders

# a confounder is a variable which causes changes in both x and y

# UC-Berkeley admission data
library(dslabs)
data(admissions)
admissions

# we can see that more men were accepted to berkeley than women
admissions %>% group_by(gender) %>% 
  summarize(percentage = 
              round(sum(admitted*applicants)/sum(applicants),1))

# a chi-squared test tells us that gender and admissions are not independent
admissions %>% group_by(gender) %>% 
  summarize(total_admitted = round(sum(admitted / 100 * applicants)), 
            not_admitted = sum(applicants) - sum(total_admitted)) %>%
  select(-gender) %>% 
  summarize(tidy(chisq.test(.)))

# however, when we look at it by major, we find a different much smaller than the 
# 14% overall difference when we look at men vs women overall
admissions %>% select(major, gender, admitted) %>%
  pivot_wider(names_from = gender, values_from = admitted) %>%
  mutate(women_minus_men = women - men)

# this is a case where an uncounted confounder is affecting the data
# plot total percent admitted to major versus percent women applicants
admissions %>% 
  group_by(major) %>% 
  summarize(major_selectivity = sum(admitted * applicants) / sum(applicants),
            percent_women_applicants = sum(applicants * (gender=="women")) /
              sum(applicants) * 100) %>%
  ggplot(aes(major_selectivity, percent_women_applicants, label = major)) +
  geom_text()

# plot percent of applicants accepted by gender
admissions %>% 
  mutate(percent_admitted = admitted*applicants/sum(applicants)) %>%
  ggplot(aes(gender, y = percent_admitted, fill = major)) +
  geom_bar(stat = "identity", position = "stack")
# this plot suggests an association - women were more likely to apply to the 
# selective majors. the majority of selected men came from A and B, the easy majors
# to get into

# if we stratify by major, we control for the counfounder, and the effect vanishes
admissions %>% 
  ggplot(aes(major, admitted, col = gender, size = applicants)) +
  geom_point()

# average difference by major
admissions %>%  group_by(gender) %>% summarize(average = mean(admitted))

## Simpsons Paradox

# the above is an example of Simpsons Paradox - the entire direction of the 
# correlation reverses when we stratify by another variable

## Assessment

library(dslabs)
data("research_funding_rates")
research_funding_rates
table(research_funding_rates)

# Construct a two-by-two table of gender (men/women) by award status 
# (awarded/not) using the total numbers across all disciplines.
two_by_two <- research_funding_rates %>% 
  select(-discipline) %>% 
  summarize_all(funs(sum)) %>%
  summarize(yes_men = awards_men, 
            no_men = applications_men - awards_men, 
            yes_women = awards_women, 
            no_women = applications_women - awards_women) %>%
  gather %>%
  separate(key, c("awarded", "gender")) %>%
  spread(gender, value)
two_by_two

# Run a chi-squared test on the two-by-two table to determine whether the 
# difference in the two success rates is significant. 
two_by_two %>% select(-awarded) %>% chisq.test() %>% tidy() %>% pull(p.value)

# To settle this dispute, use this dataset with number of applications, awards, 
# and success rate for each gender:

dat <- research_funding_rates %>% 
  mutate(discipline = reorder(discipline, success_rates_total)) %>%
  rename(success_total = success_rates_total,
         success_men = success_rates_men,
         success_women = success_rates_women) %>%
  pivot_longer(-discipline) %>%
  separate(name, c("type", "gender")) %>%
  pivot_wider(names_from = type, values_from = value) %>%
  filter(gender != "total")
dat

# To check if this is a case of Simpson's paradox, plot the success rates versus
# disciplines, which have been ordered by overall success, with colors to denote 
# genders and size to denote the number of applications.
dat %>% 
  ggplot(aes(discipline, success, size = applications, color = gender)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_point()
