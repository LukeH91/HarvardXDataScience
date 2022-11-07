## Intro to Gapminder Dataset

# Gapminder is an organisation that gathers data about world health and income
# Its data is included in dslabs
library(dslabs)
data(gapminder)
head(gapminder)

# we can compare countries using this code:
gapminder %>% filter(year == 2015 & country %in% c("Sri Lanka", "Turkey")) %>%
  select(country, infant_mortality)

# we can also compare life expectancy versus fertility rates in the past
ds_theme_set()
filter(gapminder, year==1962) %>% ggplot(aes(fertility, life_expectancy)) +
  geom_point()

# there appears to be two groups, so we will colour by continent
filter(gapminder, year==1962) %>% 
  ggplot(aes(fertility, life_expectancy, color=continent)) +
  geom_point()

## Faceting

# faceting allows us to plot graphs side by side, for easy comparisons
# we use facet_grid as a layer, with continent in rows and year in columns
filter(gapminder, year %in% c(1962, 2012)) %>% 
  ggplot(aes(fertility, life_expectancy, color=continent)) +
  geom_point() +
  facet_grid(continent~year)

# if we simply want to compare the years, we remove continent from the facet
filter(gapminder, year %in% c(1962, 2012)) %>% 
  ggplot(aes(fertility, life_expectancy, color=continent)) +
  geom_point() +
  facet_grid(.~year)

# we can add more years to the plot, but we can't have them all on the same row
filter(gapminder, year %in% c(1970, 1980, 1990, 2000),
       continent %in% c("Europe", "Asia")) %>% 
  ggplot(aes(fertility, life_expectancy, color=continent)) +
  geom_point() +
  facet_wrap(~year)

# using facet, range is determined by all data sets, making comparisons easier

## Time Series Plots

# we can plot data over time in a time series plot
gapminder %>% filter(country=="United States") %>% 
  ggplot(aes(year, fertility)) +
  geom_line()

# however, this results in a problem if we include two countries
gapminder %>% filter(country %in% c("South Korea","Germany")) %>% 
  ggplot(aes(year, fertility)) +
  geom_line()

# to fix this, we must assign each country a group in the aes
gapminder %>% filter(country %in% c("South Korea","Germany")) %>% 
  ggplot(aes(year, fertility, group=country)) +
  geom_line()

# again, giving each country a color fixes the problem anyway
gapminder %>% filter(country %in% c("South Korea","Germany")) %>% 
  ggplot(aes(year, fertility, color=country)) +
  geom_line()

# to add labels, we must define the labels as their own data frame
# we must also include graph locations for them which we choose manually
countries <- c("South Korea", "Germany")
labels <- data.frame(country = countries, x = c(1975, 1965), y = c(60,72))
gapminder %>% filter(country %in% countries) %>% 
  ggplot(aes(year, life_expectancy, col=country)) +
  geom_line() + 
  # here we pass the data frame as the contents and positions for the labels
  geom_text(data = labels, aes(x,y,label=country), size = 5) +
  # and we turn the legend off
  theme(legend.position="none")

## Transformations

# to assess poverty levels, we want to see each country's GDP per day
gapminder <- gapminder %>% mutate(dollars_per_day = gdp/population/365)

# let's look at per-day incomes in 1970
gapminder %>% filter(year==1970 & !is.na(gdp)) %>% 
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth=1, color="darkblue")

# the rich countries expand the axis too much - what if we use a log trans?
# log 2 means that for every time the data doubles, the trans increases by 1
gapminder %>% filter(year==1970 & !is.na(gdp)) %>% 
  ggplot(aes(log2(dollars_per_day))) +
  geom_histogram(binwidth=1, color="darkblue")

# the log2 trans reveals that there are two modes - local modes.
# as we can see, one of them is at about 2$ per day (1 in log2) and one is at
# about 32 dollars per day (5 in the log2 scale)

# this is a bit hard to read because the data is log transformed
# instead, we can just do this to the scales, producing an easier-to-read graph
gapminder %>% filter(year==1970 & !is.na(gdp)) %>% 
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth=1, color="darkblue") +
  scale_x_continuous(trans="log2")

## Stratify and Boxplot

# we know that the data is bimodal, but not where each country resides
length(levels(gapminder$region))

# because there are many regions, we need to use box plots to compare them
p <- gapminder %>%
  filter(year == 1970 & !is.na(gdp)) %>%
  ggplot(aes(region, dollars_per_day))
p + geom_boxplot()

# the labels are horizontal and cannot be read, so we rotate them
p + geom_boxplot() +
  theme(axis.text.x = element_text(angle=90, hjust=1))

# in R, factors are automatically alphabetical
fac <- factor(c("Asia", "Asia", "West", "West", "West"))
levels(fac)
# however, if we assign them each a value, we can reorder using this
value <- c(10,11,12,6,4)
fac <- reorder(fac, value, FUN=mean)
levels(fac)

# we can therefore reorder the countries by their median income
# we can also fill the plots in by continent
p <- gapminder %>%
  filter(year == 1970 & !is.na(gdp)) %>%
  mutate(region=reorder(region, dollars_per_day, FUN=median)) %>%
  ggplot(aes(region, dollars_per_day, fill = continent)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle=90, hjust=1)) +
  xlab("")
p

# finally, we will change it to log2 scale
p <- gapminder %>%
  filter(year == 1970 & !is.na(gdp)) %>%
  mutate(region=reorder(region, dollars_per_day, FUN=median)) %>%
  ggplot(aes(region, dollars_per_day, fill = continent)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle=90, hjust=1)) +
  xlab("") +
  scale_y_continuous(trans="log2")
p

# since we do not have many data points, we can add a geom_point layer
# this shows individual countries
p <- gapminder %>%
  filter(year == 1970 & !is.na(gdp)) %>%
  mutate(region=reorder(region, dollars_per_day, FUN=median)) %>%
  ggplot(aes(region, dollars_per_day, fill = continent)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle=90, hjust=1)) +
  xlab("") +
  scale_y_continuous(trans="log2") +
  geom_point(show.legend = FALSE)
p

## Comparing Distributions

# since the rich countries are mostly in the west, we will define its regions
west <- c("Western Europe", "Northern Europe", "Southern Europe", 
          "Northern America", "Australia and New Zealand")

# using this vector, we can now check if the bimodal distribution earlier
# is because of a dichotomy between the west and the rest of the world
gapminder %>%
  filter(year == 1970 & !is.na(gdp)) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth=1, color="darkblue") +
  scale_x_continuous(trans="log2") +
  facet_grid(.~group)

# now we want to compare this to the present day
gapminder %>%
  filter(year %in% c(1970, 2010) & !is.na(gdp)) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth=1, color="darkblue") +
  scale_x_continuous(trans="log2") +
  facet_grid(year~group)

# more countries existed or had data in 2010 compared to 1970
# we will therefore remake the graph only showing countries who are in both
# to do this, we need to make vectors with lists of both, then intersect them
country_list_1 <- gapminder %>%
  filter(year==1970 & !is.na(dollars_per_day)) %>% .$country
country_list_1 <- gapminder %>%
  filter(year==2010 & !is.na(dollars_per_day)) %>% .$country
country_list <- intersect(country_list_1, country_list_2)

# we can now filter the graph to only the 108 countries present in both
gapminder %>%
  filter(year %in% c(1970, 2010) & country %in% country_list) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth=1, color="darkblue") +
  scale_x_continuous(trans="log2") +
  facet_grid(year~group)

# we can also do the same to the boxplots from earlier
p <- gapminder %>%
  filter(year %in% c(1970,2010) & country %in% country_list) %>%
  mutate(region=reorder(region, dollars_per_day, FUN=median)) %>%
  ggplot() +
  theme(axis.text.x = element_text(angle=90, hjust=1)) +
  xlab("") +
  scale_y_continuous(trans="log2")
p + geom_boxplot(aes(region, dollars_per_day, fill=continent)) + 
  facet_grid(year~.)

# the vertical stacking of the box plots makes it a bit hard to compare
# instead, we ask ggplot to keep the years together, but colour the block
p + geom_boxplot(aes(region, dollars_per_day, fill=factor(year))) 

## Density Plots

# one smooth density plot can convey that the gap between rich and poor 
# countries is closing, but we first need a way to show that the West
# group is much smaller than the developing group
# we can do this by multiplying the y-axis by the size of each group using 
# the count argument. we want this variable to be on the y-axis instead of dens,
# and we can access thse variables by putting .. around their names
aes(x=dollars_per_day, y=..count..)
# then, we just change the mapping from the last code
p <- gapminder %>%
  filter(year %in% c(1970,2010) & country %in% country_list) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day, y=..count.., fill=group)) +
  scale_x_continuous(trans="log2")
p + geom_density(alpha=0.2) + facet_grid(year ~ .)
# we can also change the bw to change the smoothness
p + geom_density(alpha=0.2,bw=0.75) + facet_grid(year ~ .)

# we see the formation of a third mode, the countries which have improved most
# we can view it by region using case_when, which does not take data arguments
# and has to be fed a series of equations
gapminder <- gapminder %>%
  mutate(group = case_when(
    .$region %in% west ~ "West",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    .$region %in% c("Caribbean", "Central America", "South America") ~ 
      "Latin America",
    .$continent == "Africa" & .$region != "Northern Africa" ~ 
      "Sub-Saharan Africa",
    TRUE ~ "Others"))
# this basically assigns groups based on the region, and now we can use this
# new group variable into a factor with a specific order we want later
gapminder <- gapminder %>%
  mutate(group = factor(group, levels = c("Others", "Latin America", 
                                          "East Asia", "Sub-Saharan Africa", 
                                          "West")))
# now that we've done this, we can easily plot the density for each one
p <- gapminder %>%
  filter(year %in% c(1970,2010) & country %in% country_list) %>%
  ggplot(aes(dollars_per_day, y=..count.., fill=group)) +
  scale_x_continuous(trans="log2")
p + geom_density(alpha=0.2) + facet_grid(year ~ .)

# this plot is a bit hard to read: we can fix it with a stacking approach
p + geom_density(alpha = 0.2, bw = 0.75, position = "stack") +
  facet_grid(year ~ . )
# now the density plots are stacked on top of one another

# in the current graph, every country is weighted equally, so large countries 
# like china are weighted the same. if we weight by population, we get this:
gapminder %>%
  filter(year %in% c(1970,2010) & country %in% country_list) %>%
  group_by(year) %>%
  mutate(weight=population/sum(population)*2) %>%
  ungroup() %>%
  ggplot(aes(dollars_per_day, fill = group, weight = weight)) +
  scale_x_continuous(trans = "log2") + 
  geom_density(alpha = 0.2, bw = 0.75, position = "stack") +
  facet_grid(year ~ .)
  
# this plot now represents populations

## Ecological Fallacy

# we will now compare child mortality with income. we will define more regions.
gapminder <- gapminder %>%
  mutate(group = case_when(
    .$region %in% west ~ "West",
    .$region %in% "Northern Africa" ~ "Northern Africa", 
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    .$region == "Southern Asia" ~ "Southern Asia",
    .$region %in% c("Caribbean", "Central America", "South America") ~ 
      "Latin America",
    .$continent == "Africa" & .$region != "Northern Africa" ~ 
      "Sub-Saharan Africa",
    .$region %in% c("Melanesia", "Micronesia", "Polynesia") ~ 
      "Pacific Islands"))

# we can now compare these regions for infant survival rate
surv_income <- gapminder %>% 
  filter(year == 2010 & !is.na(gdp)
         & !is.na(infant_mortality) & !is.na(group)) %>%
  group_by(group) %>%
  summarise(income=sum(gdp)/sum(population)/365,
            infant_survival_rate = 
              1-sum(infant_mortality/1000*population)/sum(population))
surv_income %>% arrange(income)
            
# the relationship is almost perfectly linear. we plot it like this
# we use the limit argument to tell R how to scale the axes
surv_income %>% ggplot(aes(income,infant_survival_rate,label=group, color=group)) +
  scale_x_continuous(trans="log2", limit=c(0.25, 150)) +
  scale_y_continuous(trans="logit", limit=c(0.875, .9981),
                     breaks= c(.85, .90, .95, .99, .995, .998)) +
  geom_label(size = 3, show.legend = FALSE)
 
# however, we cannot just assume that all countries in these regions follow
# the pattern, or that all counties with low income will have high mortality
# this is known as the "ecological fallacy" - in other words, that our data
# trends are ecologically valid

surv_income %>% ggplot(aes(income,infant_survival_rate,label=region, color=region)) +
  scale_x_continuous(trans="log2", limit=c(0.25, 150)) +
  scale_y_continuous(trans="logit", limit=c(0.875, .9981),
                     breaks= c(.85, .90, .95, .99, .995, .998)) +
  geom_label(size = 3, show.legend = FALSE) + 
  geom_point()

per_couincome <- gapminder %>% 
  filter(year == 2010 & !is.na(gdp)
         & !is.na(infant_mortality) & !is.na(group)) %>%
  group_by(group) %>%
  summarise(income=sum(gdp)/sum(population)/365,
            infant_survival_rate = 
              1-sum(infant_mortality/1000*population)/sum(population))
surv_income %>% arrange(income)


## Assessment

gapminder %>% filter(continent == "Africa" & year %in% c(1970,2010) &
                       !is.na(gdp) & !is.na(infant_mortality)) %>% 
  mutate(dollars_per_day=(gdp/population/365)) %>% 
  ggplot(aes(dollars_per_day, infant_mortality, color=region, label=country)) +
  geom_point() + 
  scale_x_continuous(trans="log2") + 
  geom_text() + 
  facet_grid(year~.) 