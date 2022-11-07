## Sampling Model Parameters and Estimates
library(dslabs)
library(tidyverse)
ds_theme_set()

# imagine we want to estimate the proportions of blue and red in an urn
# dslabs has a function to let us take a sample from said urn
take_poll(25)
# we could simply use this as the population estimate, but we get different
# numbers each time we run it
take_poll(25)
take_poll(25)

## The Sample Average

# imagine we define a blue as 1 and a red as 0
# if we sum the draw and then divide by the sample size, this gives us the
# proportion of blues

# Properties of our Estimate

# the law of large numbers tells us that our estimate converges on p as
# the sample size grows



