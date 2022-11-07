## Dates and Times

library(lubridate)
library(tidyverse)
library(dslabs)

# we can represent dates with a string, but also, they can be represented in numbers
# which show how far they are from the epoch
# however, times and dates get confusing in the eopch format because they are
# just huge numbers. R has a date format, and they can be converted into numbers.
# inspect the startdate column of 2016 polls data, a Date type
data("polls_us_election_2016")
polls_us_election_2016$startdate %>% head
class(polls_us_election_2016$startdate)
as.numeric(polls_us_election_2016$startdate) %>% head

# ggplot can handle dates, so you can do a scatterplot with a date
polls_us_election_2016 %>% filter(pollster == "Ipsos" & state =="U.S.") %>%
  ggplot(aes(startdate, rawpoll_trump)) +
  geom_line()

# lubridate helps with dealing with dates - we will take a random sample of dates
set.seed(2)
dates <- sample(polls_us_election_2016$startdate, 10) %>% sort
dates

# the year, month and day functions extract those values
data.frame(date = days(dates),
           month = month(dates),
           day = day(dates),
           year = year(dates))

# we can also add labels to the months
month(dates, label=TRUE)

# it also has parsers to convert strings into dates
x <- c(20090101, "2009-01-02", "2009 01 03", "2009-1-4",
       "2009-1, 5", "Created on 2009 1 6", "200901 !!! 07")
ymd(x)

# different parsers extract year, month and day in different orders
x <- "09/01/02"
ymd(x)
mdy(x)
ydm(x)
myd(x)
dmy(x)
dym(x)

now()    # current time in your time zone
now("GMT")    # current time in GMT
now() %>% hour()    # current hour
now() %>% minute()    # current minute
now() %>% second()    # current second

# parse time
x <- c("12:34:56")
hms(x)

#parse datetime
x <- "Nov/2/2012 12:34:56"
mdy_hms(x)

## Assessment

data(brexit_polls)
# How many polls had a start date (startdate) in April (month number 4)?
sum(month(brexit_polls$enddate)=="4")
# Use the round_date() function on the enddate column with the argument unit="week"
# How many polls ended the week of 2016-06-12?
sum(round_date(brexit_polls$enddate, unit="week")=="2016-06-12")
# Use the weekdays() function from lubridate to determine the weekday on which each poll ended (enddate).
table(weekdays(brexit_polls$enddate))

data(movielens)
#Convert the timestamp column to dates using the lubridate as_datetime() function.
movielens <- movielens %>% mutate(newdate=as_datetime(movielens$timestamp))
table(year(movielens$newdate))
table(hour(movielens$newdate))

library(tidyverse)
library(gutenbergr)
library(tidytext)
options(digits = 3)
dat <- gutenberg_metadata

# Use str_detect() to find the ID of the novel Pride and Prejudice.
dat %>% filter(title=="Pride and Prejudice")
str_detect(dat$title, "Pride and Prejudice")
# What is the correct ID number?
gutenberg_works(title=="Pride and Prejudice")
#Use the gutenberg_download() function to download the text for Pride and Prejudice.
pp <- gutenberg_download(1342, mirror = "http://mirrors.xmission.com/gutenberg/")
# Use the tidytext package to create a tidy table with all the words in the text. 
# Save this object as words.
words <- pp %>% unnest_tokens(word, text)
# Remove stop words from the words object. Recall that stop words are defined in
# the stop_words data frame from the tidytext package.
words <- words %>% filter(!word %in% stop_words$word)
# After removing stop words, detect and then filter out any token that contains a digit from words.
nums <- words %>% filter(!str_detect(word, "\\d+"))
# Analyze the most frequent words in the novel after removing stop words and tokens with digits.
count(words) %>% filter(n>= 100)
counts <- pp %>% unnest_tokens(word, text)  %>% anti_join(stop_words, by = 'word') %>% filter(!str_detect(word, "\\d+")) %>% dplyr::count(word) %>% filter(n>= 100) %>% arrange(desc(n))

# Use this afinn lexicon to assign sentiment values to words. Keep only words that
# are present in both words and the afinn lexicon. Save this data frame as afinn_sentiments.
afinn <- get_sentiments("afinn")
afinn_sentiments <- nums %>% inner_join(afinn) %>% summarise(n = mean(value > 0), n1 = sum(value == 4))
dat %>% inner_join(afinn)
