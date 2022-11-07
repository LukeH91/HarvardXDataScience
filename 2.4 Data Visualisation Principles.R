## Encoding Data using Visual Cues

# much of this is based on Karl Broman's "Creating Effective Figures and Tables"
library(dslabs)
library(tidyverse)

# position and length (bar charts) are better for comparing two things
# than angle and area (pie charts)
# create a data frame of browsers, use percentages, and years
browsers <- c("Opera", "Chrome", "Safari", "Firefox", "IE", 
              "Opera", "Chrome", "Safari", "Firefox", "IE")
use <- c(3, 26, 21, 23, 28, 
             2, 29, 22, 21, 27)
year <- c(2000, 2000, 2000, 2000, 2000,
          2010, 2010, 2010, 2010, 2010)
# convert the year and browser to factors for use as categories
browseruse <- data.frame(browsers, use, year)
browseruse$browsers <- factor(browseruse$browsers)
browseruse$year <- factor(browseruse$year)

# plot as pie charts
ggplot(data=browseruse, aes(x=" ", y=use, group=browsers, colour=browsers, fill=browsers)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  facet_grid(.~year)

# these are extremely hard to compare, whereas a bar chart is easier:
browseruse %>% ggplot(aes(x=browsers, y=use)) +
  geom_bar(stat="identity") +
  facet_grid(.~year)


## When to Include Zero

# removing zero from a bar chart can exaggerate small differences
browsers <- c("Firefox", "Chrome")
use <- c(53, 47)
buse <- data.frame(browsers, use)  
buse$browsers <- factor(buse$browsers)

# compare these two graphs
buse %>% ggplot(aes(x=browsers, y=use)) + geom_bar(stat="identity")
buse %>% ggplot(aes(x=browsers, y=use)) + geom_bar(stat="identity") +
  coord_cartesian(ylim = c(46, 54)) 

# be careful when using circles to represent quantities - ensure that they 
# are set to area instead of radius, since the radius is squared

# order categories in a meaningful way - do not use factor level, which is
# alphabetical by default, and thus arbitrary, UNLESS you want to make multiple
# graphs which are compared between categories (such as the browser graph)
# if you are looking at something numeric, use this to order

# Show the Data

# if we are comparing categories, the default plot in excel would be a bar chart
# however, this does not tell us about the distribution of the data
# even a simple scatter plot is better since it shows all the points
heights %>% ggplot(aes(sex,height)) + geom_point()

# we can make it better by jittering the points, and using alpha blending
heights %>% ggplot(aes(sex,height)) + geom_jitter(width = 0.1, alpha = 0.2)

## Using Common Axes

# so we want to show the distribution - this can be done with histograms
# remember to align plots vertically to show horizontal contrasts and vice versa
heights %>% 
  ggplot(aes(height, ..density..)) +
  geom_histogram(binwidth = 1, color="black") +
  facet_grid(sex~.)

heights %>% 
  ggplot(aes(height, ..density..)) +
  geom_histogram(binwidth = 1, color="black") +
  facet_grid(.~sex)

## Slope Charts

# when we compare variables of the same type in a small number, for example
# life expectancy in 2010 and 2015, we might use a slope chart

west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")

dat <- gapminder %>%
  filter(year %in% c(2010, 2015) & region %in% west & !is.na(life_expectancy) & population > 10^7)

dat %>%
  mutate(location = ifelse(year == 2010, 1, 2),
         location = ifelse(year == 2015 & country %in% c("United Kingdom", "Portugal"),
                           location + 0.22, location),
         hjust = ifelse(year == 2010, 1, 0)) %>%
  mutate(year = as.factor(year)) %>%
  ggplot(aes(year, life_expectancy, group = country)) +
  geom_line(aes(color = country), show.legend = FALSE) +
  geom_text(aes(x = location, label = country, hjust = hjust), show.legend = FALSE) +
  xlab("") +
  ylab("Life Expectancy") 

# the Bland-Altman plot simply dedicates an entire axis to differences
library(ggrepel)
dat %>%
  mutate(year = paste0("life_expectancy_", year)) %>%
  select(country, year, life_expectancy) %>% spread(year, life_expectancy) %>%
  mutate(average = (life_expectancy_2015 + life_expectancy_2010)/2,
         difference = life_expectancy_2015 - life_expectancy_2010) %>%
  ggplot(aes(average, difference, label = country)) +
  geom_point() +
  geom_text_repel() +
  geom_abline(lty = 2) +
  xlab("Average of 2010 and 2015") +
  ylab("Difference between 2015 and 2010")

## Case Study: Vaccines

# vaccine impact data is included in dslabs
data(us_contagious_diseases)
str(us_contagious_diseases)

# create a tempirary object with only the measles data, a per 100k rate, orders
# states by rate, and removes alaska and hawaii
the_disease <- "Measles"
dat <- us_contagious_diseases %>%
  filter(!state %in% c("Hawaii", "Alaska") & disease == the_disease) %>%
  mutate(rate = count/population*10000) %>%
  mutate(state=reorder(state,rate))

# we can now easily plot measles data per year for any state
dat %>% filter(state=="California") %>%
  ggplot(aes(year, rate)) + 
  geom_line() + ylab("Cases per 10,000") + 
  geom_vline(xintercept=1963, col="blue")

# we want to use x for year, y for state, and colour for rate
# we can choose between two palettes: sequential (high to low) and diverging
# (diverging from a middle value)
# in our case, we want sequential
library(RColorBrewer)
dat %>% ggplot(aes(year, state, fill=rate)) +
  geom_tile(color="grey50") +
  scale_x_continuous(expand=c(0,0)) +
  scale_fill_gradientn(colors=brewer.pal(9,"Reds"), trans="sqrt") +
  geom_vline(xintercept=1963, col="blue") +
  theme_minimal() + theme(panel.grid = element_blank()) +
  ggtitle(the_disease) +
  ylab("") +
  xlab("")


### Assessment: Titanic Survival

options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)

titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

temp_carbon %>% filter(!is.na(temp_anomaly)) %>% 
  ggplot() +
  geom_line(aes(year,temp_anomaly), color="green") +
  geom_line(aes(year,ocean_anomaly), color="blue") +
  geom_line(aes(year,land_anomaly), color="red")
  
library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

greenhouse_gases %>%
  ggplot(aes(year, concentration)) +
  geom_line() +
  facet_grid(gas~., scales = "free") +
  geom_vline(aes(xintercept = 1850)) +
  ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")

temp_carbon %>%
  ggplot(aes(year, carbon_emissions)) +
  geom_line()

data(historic_co2)
co2_time <- historic_co2 %>%   
  ggplot(aes(year, co2, color=source)) +
  geom_line()


co2_time + coord_cartesian(xlim = c(-800000, -775000))

co2_time + coord_cartesian(xlim = c(-375000, -330000))

co2_time + coord_cartesian(xlim = c(-140000, -120000))

co2_time + coord_cartesian(xlim = c(-3000, 2018))
