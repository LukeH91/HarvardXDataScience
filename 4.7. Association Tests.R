## Association Tests

library(dslabs)

# binary, categorical and ordinal data can be tested using assocaition tests
# for example, there was evidence of gender bias in researcher appraisals 
# in a journal from the netherlands
data("research_funding_rates")
research_funding_rates

# to look at the differences between men and women, we look at the totals
totals <- research_funding_rates %>% 
  select(-discipline) %>%
  summarize_all(funs(sum)) %>%
  summarize(yes_men = awards_men,
            no_men = applications_men - awards_men,
            yes_women = awards_women,
            no_women = applications_women - awards_women)

totals %>% summarize(percent_men = yes_men/(yes_men+no_men),
                     percent_women = yes_women/(yes_women+no_women))

# but how do we know if this is just random chance?

# a woman claimed to be able to tell if milk was poured into tea before or after.
# she was given 8 cups of tea - 4 before, 4 after, and was asked to pick out 
# the before teas. her odds of picking 3 correctly are 16/70, and 4 correctly are 
# 1/70. 
# using fisher's exact test, we can check the p-value of a given outcome

tab <- matrix(c(3,1,1,3),2,2)
rownames(tab) <- c("Poured Before", "Poured After")
colnames(tab) <- c("Guessed Before", "Guessed After")
tab
fisher.test(tab, alternative="greater")

## Chi-Square Tests

# in the above example, the quantities in each row and column are experimentally
# fixed, which is what lets us use the hypergeometric distribution
# in most cases this will not be true, and we should instead use a chi-squared test
# for funding, we know that about 18% of men and 15% of women were funded
# we must first calculate the overall funding rate
# compute overall funding rate
funding_rate <- totals %>%
  summarize(percent_total = (yes_men + yes_women) / (yes_men + no_men + yes_women + no_women)) %>%
  .$percent_total
funding_rate
# the we construct a 2x2 table for the observed data
two_by_two <- tibble(awarded = c("no", "yes"),
                     men = c(totals$no_men, totals$yes_men),
                     women = c(totals$no_women, totals$yes_women))
two_by_two

# and another one for what we'd expect if the overall rate was the same for both
tibble(awarded = c("no", "yes"),
       men = (totals$no_men + totals$yes_men) * c(1-funding_rate, funding_rate),
       women = (totals$no_women + totals$yes_women) * c(1-funding_rate, funding_rate))

# then we conduct a chi-squared test to check if they differ
# chi-squared test
chisq_test <- two_by_two %>%
  select(-awarded) %>% chisq.test()
chisq_test
# so the odds of seeing this pattern with no true difference is quite low

# the odds ratio is a useful summary statistic for 2x2 tables
# in this case, X = 1 if male, and 0 otherwise, and Y = 1 if funded, and 0 otherwise
# the odds of being funded if you're a man can be computed using:
odds_men <- (two_by_two$men[2] / sum(two_by_two$men)) / 
  (two_by_two$men[1] / sum(two_by_two$men))

# odds of getting funding for women
odds_women <- (two_by_two$women[2] / sum(two_by_two$women)) /
  (two_by_two$women[1] / sum(two_by_two$women))

# odds ratio - how many times larger odds are for men than women
odds_men/odds_women

# note also that p-values but not odds ratios are affected by sample size
# multiplying all observations by 10 decreases p-value without changing odds ratio
two_by_two %>%
  select(-awarded) %>%
  mutate(men = men*10, women = women*10) %>%
  chisq.test()

## Assessment

# Generate an object called 'totals' that contains the numbers of good and bad predictions for polls rated A- and C-
totals <- errors %>%
  filter(grade %in% c("A-", "C-")) %>%
  group_by(grade,hit) %>%
  summarize(num = n()) %>%
  spread(grade, num)

# Print the proportion of hits for grade A- polls to the console
totals[[2,3]]/sum(totals[[3]])

# Print the proportion of hits for grade C- polls to the console
totals[[2,2]]/sum(totals[[2]])

# The 'totals' data have already been loaded. Examine them using the `head` function.
head(totals)

# Perform a chi-squared test on the hit data. Save the results as an object called 'chisq_test'.
chisq_test <- totals %>% 
  select(-hit) %>%
  chisq.test()
chisq_test
# Print the p-value of the chi-squared test to the console
chisq_test$p.value

#Generate a variable called `odds_C` that contains the odds of getting the prediction right for grade C- polls
odds_C <- (totals[[2,2]] / sum(totals[[2]])) / 
  (totals[[1,2]] / sum(totals[[2]]))

# Generate a variable called `odds_A` that contains the odds of getting the prediction right for grade A- polls
odds_A <- (totals[[2,3]] / sum(totals[[3]])) / 
  (totals[[1,3]] / sum(totals[[3]]))

# Calculate the odds ratio to determine how many times larger the odds ratio is for grade A- polls than grade C- polls
odds_A/odds_C
