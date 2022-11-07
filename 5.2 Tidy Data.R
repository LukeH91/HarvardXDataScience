## Tidy Data

library(tidyverse)
library(dslabs)
library(ggrepel)

# previously, we made a plot using this data and code:
# create and inspect a tidy data frame
tidy_data <- gapminder %>% 
  filter(country %in% c("South Korea", "Germany")) %>%
  select(country, year, fertility)
head(tidy_data)

# plotting tidy data is simple
tidy_data %>% 
  ggplot(aes(year, fertility, color = country)) +
  geom_point()

# this works seamlessly because the data is already tidy - each point is a row
# for tidy data, each row must be one observation, and each column must be one variable
# originally, the data was not like this:
path <- system.file("extdata", package="dslabs")
filename <- file.path(path, "fertility-two-countries-example.csv")
wide_data <- read_csv(filename)
# as we can see here, each year was a column before, and this will not work
# we must therefore wrangle the data into being tidy

## Pivot_Longer

# the tidyr package lets us reshape data in many ways
# we want to convert wide data (many columns) into long data (many rows)
# we can use pivot_longer for this. the first argument is the data frame, and
# the second is the list of values to move

# we want to move the years in wide_data to a single column
wide_data %>% pivot_longer("1960":"2015")
# since country is the only column not being pivoted, we could also do this:
wide_data %>% pivot_longer(-country)

# this leaves us with columns called "name" and "value", but we can fix this:
new_tidy_data <- wide_data %>% 
  pivot_longer(-country, names_to = "year", values_to = "fertility")

#we are almost there - but the year is treated as a character, and we want
# integer. we can do this either via mutate:
mutate(year = as.numeric(year))
# or by using names_transform:
new_tidy_data <- wide_data %>% 
  pivot_longer(-country, names_to = "year", values_to = "fertility",
               names_transform = list(year=as.numeric))

# now that it is tidy, we can use the same ggplot code we used before
new_tidy_data %>% ggplot(aes(year,fertility,color=country)) + geom_point()

## Pivot_Wider

# sometimes we want to turn tidy data into wide data
# for this, we use pivot_wider
new_wide_data <- new_tidy_data %>%
  pivot_wider(names_from=year, values_from=fertility)
select(new_wide_data, country, "1960":"1967")

## Separate

# in this example, life expectancy and fertility are both stored, and not optimally
path <- system.file("extdata", package="dslabs")
fname <- "life-expectancy-and-fertility-two-countries-example.csv"
filename <- file.path(path, fname)
raw_dat <- read_csv(filename)
select(raw_dat, 1:4)
# the column headers contain not just the year, but also the type of variable
# we begin with pivot_longer, but we can not call the new column "year", since 
# it does not just contain the year any more. for now, we call it "name"
dat <- raw_dat %>% pivot_longer(-country)
head(dat)
# this is not a tidy data set, because each observation is two rows, not one.
# the year and variable names are split by an underscore
# separate() take the name of the column to be separated, the names for the new 
# columns, and the separating character
dat %>% separate(name, c("year", "name"), sep="_")
# underscore is the default separator, and we want to convert years to numeric,
# so we can use this code:
dat %>% separate(name, c("year", "name"), convert=TRUE)
# the weird error is because an underscore is used between life_expectancy too
# we can merge the last two variables when there is another separation
dat %>% separate(name, c("year", "name"), extra="merge", convert=TRUE)
# we still need a column for each variable - pivot_wider can help with this
# since we already have "name" and "value" and these are the defaults, we can simply:
dat %>% separate(name, c("year", "name"), extra="merge", convert=TRUE) %>%
  pivot_wider()

## Unite
# another approach is that we might ask it for two columns:
dat %>% separate(name, c("year", "name_1", "name_2"), fill="right", convert=TRUE)
# then we merge the two resulting columns:
dat %>% separate(name, c("year", "name_1", "name_2"), 
                 fill="right", convert=TRUE) %>%
  unite(variable_name, name_1, name_2, sep="_")
# then we spread the columns using this code:
dat %>% separate(name, c("year", "name_1", "name_2"), 
                 fill="right", convert=TRUE) %>%
  unite(name, name_1, name_2, sep="_") %>%
  spread(name, value) %>%
  rename(fertility=fertility_NA)


## Combining Tables

# suppose we want to explore population size and electoral results, from two tables:
data(murders)
data(polls_us_election_2016)
# we cannot simply join the tables, because the order of the states is not the same:
identical(results_us_election_2016$state, murders$state)
# the join functions in dplyr ensure that the tables are combined so that rows match
# we do this by specifying the columns to join by
tab <- left_join(murders, results_us_election_2016, by = "state")
head(tab)
# plot electoral votes versus population
tab %>% ggplot(aes(population/10^6, electoral_votes, label = abb)) +
  geom_point() +
  geom_text_repel() + 
  scale_x_continuous(trans = "log2") +
  scale_y_continuous(trans = "log2") +
  geom_smooth(method = "lm", se = FALSE)

# however, it is not always true that tables can be neatly matched
# to illustrate this, we will create 2 subsets with different states (and some overlap)
tab1 <- slice(murders, 1:8) %>% select(state, population)
tab2 <- slice(results_us_election_2016, c(1:4, 5:8, 10:30)) %>% select(state, electoral_votes)
tab1
tab2
# imagine we want to make a table the same as tab1, but with vote data from tab2
# for the states which have it available. for this, we use left_join
left_join(tab1, tab2)
# we can also use the pipe:
tab1 %>% left_join(tab2)
# if we want a table like tab2 instead, we can use right_join:
tab1 %>% right_join(tab2)
# if we only want the rows that have information in both tables, we use inner_join
inner_join(tab1, tab2)
# or if we want to keep all rows and fill missing values with NA, we full_join:
full_join(tab1, tab2)
# there are two more functions which, rather than joining the tables, let us 
# keep parts of one table based on what we have in the other
semi_join(tab1, tab2)
# anti-join does the opposite - it only keeps parts where there is no corresponding info
anti_join(tab1, tab2)

## Binding

# we can also combine data sets by binding, regardless of row order
# bind_cols puts two data sets into a tibble, but it requires that we assign names
bind_cols(a=1:3, b=4:6)
# cbind does the same thing, but does not give a tibble (instead a data frame, matrix, etc)
# bind_cols can also bind data frames
tab1 <- tab[,1:3]
tab2 <- tab[,4:6]
tab3 <- tab[,7:9]
new_tab <- bind_cols(tab1, tab2, tab3)
head(new_tab)
# bind_rows is similar, but binds rows instead of columns
tab1 <- tab[1:2,]
tab2 <- tab[3:4,]
bind_rows(tab1, tab2)

## Set Operators

# these are usually for vectors, but dplyr lets us use them on data frames
# intersect shows us where two vectors intersect
intersect(c("a", "b", "c"), c("b", "c", "d"))
intersect(1:5, 3:7)
# dplyr lets us do this for tables - where are the column names the same?
tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
intersect(tab1, tab2)
# union takes the union of the two arguments:
union(c("a", "b", "c"), c("b", "c", "d"))
# we can do this for tables which have the same column names:
tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
union(tab1, tab2)
# setdiff can give us the set differences. this is not a symmetric function:
setdiff(1:10, 6:15)
setdiff(6:15, 1:10)
setdiff(tab1, tab2)
setdiff(tab2, tab1)
# setequal tells us if two sets are the same regardless of order
setequal(1:5, 1:6)
setequal(1:5, 5:1)
# this also tells us how dataframes differ
setequal(tab1, tab2)

## Web Scraping

# web scraping is when you take data from HTML in a webpage
# we will do this using a tidyverse package called rvest
library(rvest)
url <- "https://en.wikipedia.org/wiki/Murder_in_the_United_States_by_state"
h <- read_html(url)
# h now contains the entire wikipedia page
class(h)
# in the HTML, we see the node <table_class> and know the data is in a table
# rvest allows html_nodes, which extracts all nodes of a given type, 
# and html_node, which only extracts the first one of a given type
tab <- h %>% html_nodes("table")
# we then select the second table
tab <- tab[[2]]
# we must now convert the HTML table inot a data table
tab <- tab %>% html_table()
class(tab)
tab
# and change the names of the columns
tab <- tab %>% setNames(c("state", "population", "total", "murders", 
                          "gun_murders", "gun_ownership", "total_rate", 
                          "murder_rate", "gun_murder_rate"))
head(tab)
# we must now learn how to get rid of various artifacts such as the commas

# note: we can use selectorgadget.com to see which part of a site's code we need

## Assessment:
library(rvest)
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)

#  Use the html_nodes() function and the table node type to extract the first table.
# Store it in an object nodes:
nodes <- html_nodes(h, "table")
#The html_nodes() function returns a list of objects of class xml_node. We can see
#the content of each one using, for example, the html_text() function. You can see 
#the content for an arbitrarily picked component like this:
html_text(nodes[[8]])
# If the content of this object is an html table, we can use the html_table() 
# function to convert it to a data frame:
html_table(nodes[[8]])

# Convert the first four tables in nodes to data frames and inspect them.
sapply(nodes[1:4], html_table)    # 2, 3, 4 give tables with payroll info

# Create a table called tab_1 using entry 10 of nodes. Create a table called 
# tab_2 using entry 19 of nodes.
tab_1 <- html_table(nodes[[10]])
tab_2 <- html_table(nodes[[19]])
# remove the first column of tab_1 and the first row of both tables
tab_1 <- subset(tab_1, select = -X1)
tab_1 <- tab_1[-1,]
tab_2 <- tab_2[-1,]
# change column names
tab_1 <- tab_1 %>% setNames(c("Team", "Payroll", "Average"))
tab_2 <- tab_2 %>% setNames(c("Team", "Payroll", "Average"))
# complete a full join by team
joined <- full_join(tab_1, tab_2, by="Team")


library(rvest)
library(tidyverse)
# load brexit data into an object
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
h <- read_html(url)
# get list of tables
tab <- html_nodes(h, "table")
# turn the object into an array of tables, with NAs filling gaps
html_table(tab, fill=TRUE)
