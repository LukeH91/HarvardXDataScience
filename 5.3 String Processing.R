## String Parsing

library(tidyverse)
library(dslabs)
library(rvest)
library(purrr)
# sometimes, especially when taking information from web pages, we find 
# junk data, such as commas in numbers
url <- "https://en.wikipedia.org/w/index.php?title=Gun_violence_in_the_United_States_by_state&direction=prev&oldid=810166167"
murders_raw <- read_html(url) %>% 
  html_nodes("table") %>% 
  html_table() %>%
  .[[1]] %>%
  setNames(c("state", "population", "total", "murder_rate"))

# here, we find that the population and total columns are actually characters
head(murders_raw)
class(murders_raw$population)
class(murders_raw$total)

# because removing commas is so common, parse_number() exists to do it easily

## Quotes and Escape

# we can use both double " and single quotes ' to define a string in R
# if the string includes one of them, use the other to define the string
s <- '10"'
# we can view a string as it really appears using cat()
cat(s)
# similarly, if we want to type 5 feet:
s <- "5'"
cat(s)
# but what if we want to say them together, as 5'10"? both approaches give an error
# we "escape" the quotes using a backslash
s <- '5\'10"'
cat(s)
s <- "5'10\""
cat(s)

## Stringr

# in our previous example, we scraped the murders table from the web, and the 
# population column is a character vector because of commas. 
murders_raw$population[1:3]
# these cannot just be coerced into nuimbers, because they have actual characters
as.numeric(murders_raw$population[1:3])
# we need to locate the comma, then replace with blank
# the stringr package makes it very easy to do this - all the functions are str_*
# so if you type it and hit tab, you will be shown all the functions
# also, the string is always the first argument, which allows easy piping

## Case Study 1: Murders

# in the murders table, we can use str_detect to see where the commas are
commas <- function(x) any(str_detect(x, ","))
murders_raw %>% summarize_all(funs(commas))

# we then use str_replace_all to remove them
test_1 <- str_replace_all(murders_raw$population, ",", "")
test_1 <- as.numeric(test_1)
class(test_1)
# using mutate_all, we can then do this to every column where it is needed
# parse_number() lets us do this before coercing
test_2 <- parse_number(murders_raw$population)
identical(test_1, test_2)
# therefore, we can use this code:
murders_new <- murders_raw %>% mutate_at(2:3, parse_number)
murders_new %>% head

## Case Study 2: Heights

# the heights data from earlier is also available in raw format
data(reported_heights)
# the heights were reported via web form, and had some non-numeric entries
class(reported_heights$height)
x <- as.numeric(reported_heights$height)
# many heights are as requested, height in inches:
head(x)
# however, we have many NAs:
sum(is.na(x))
# we can view some of the entries that resulted in NAs like this:
reported_heights %>% mutate(new_height = as.numeric(height)) %>%
  filter(is.na(new_height)) %>%
  head(n=10)
# we could remove these, but many of them could be converted into inches
# we should write code that does this automatically
# we include a range of plausible heights, and suppress warnings to avoid
# being spammed when we convert to numbers. we want to look at all entries
# which fall outside of the boundaries, or result in an NA
not_inches <- function(x, smallest=50, tallest=84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches < smallest | inches > tallest
  ind
}
# we now apply the function
problems <- reported_heights %>% filter(not_inches(height)) %>% .$height
length(problems)
# viewing the list of problems, we notice 3 main groups of problems:
# 1. heights listed as x'y or x'y" or x'y\"
# 10 examples of x'y or x'y" or x'y\"
pattern <- "^\\d\\s*'\\s*\\d{1,2}\\.*\\d*'*\"*$"
str_subset(problems, pattern) %>% head(n=10) %>% cat
# 2. heights listed as x.y or x,y
# 10 examples of x.y or x,y
pattern <- "^[4-6]\\s*[\\.|,]\\s*([0-9]|10|11)$"
str_subset(problems, pattern) %>% head(n=10) %>% cat

# 3. heights listed in cm and not inches
# 10 examples of entries in cm rather than inches
ind <- which(between(suppressWarnings(as.numeric(problems))/2.54, 54, 81) )
ind <- ind[!is.na(ind)]
problems[ind] %>% head(n=10) %>% cat


## Regular Expressions (Regex)

# we can use regular expressions to take a standardised approach to converting these
# we can use regexes to search:
pattern <- ","
str_detect(murders_raw$total, pattern)
# we can search for cm:
str_subset(reported_heights$height,"cm")
# we now ask which of the following strings satisfy the pattern - yes or no
yes <- c("180 cm", "70 inches")
no <- c("180", "70''")
s <- c(yes, no)
# we can then search for applicable strings like this:
str_detect(s, "cm") | str_detect(s, "inches")

# however, we don't need to do this - we can use special characters instead
# | means "or", if either cm or inches appear, we can ask this:
str_detect(s, "cm|inches")
# \d means any digit, with the backslash distinguishing it from the character d
# in R we have to escape the backslash as well, so it is effectively always \\d
yes <- c("5", "6", "5'10", "5 feet", "4'11")
no <- c("", ".", "Five", "six")
s <-c(yes, no)
pattern <- "\\d"
str_detect(s, pattern)

# str_view is good for troubleshooting, and shows us the first match for a criteria
str_view(s, pattern)
# str_view_all shows us every match
str_view_all(s, pattern)


## Character Classes, Anchors and Quantifiers

# as seen before, we create test vectors containing strings which we know
# will match (yes) and ones we know will not (no)
# using this we can check for both errors - failing to match and incorrectly matching
# character classes [] define a series of characters that can be matched 
# if we want the parent to match only if we have a 5 or a 6, we can use [56]
str_view(s, "[56]")
# between 4 and 7
str_view(s, "[4-7]")
# note that in regex, everything is a character - there is no number 4
# so, if we use [1-20], we get 0, 1 and 2. however, characters do have an order,
# so this is actually 012, and [a-z] would give us the entire lowercase alphabet
# for all letters, we write this: [a-zA-Z]
# what if we want to match when we have exactly one digit?
# for this, we use anchors, which define patterns that must begin or end at 
# specific places. the beginning ^ and end $ of a string are the most common
# therefore, ^\\d$ is the start, one digit, then an end. 
pattern <- "^\\d$"
yes <- c("1","5","9")
no <- c("12", "123", " 1", "a4", "b")
s <- c(yes, no)
str_view(s, pattern)
# for the inches, we can have one or two digits, so we need a quantifier
# this is {}, with the possible number of times the previous entry repeats
# so the code for 1 or 2 digits is \\d{1,2}
pattern <- "^\\d{1,2}$"
yes <-c("1", "5", "9", "12")
no <-c("123", "a4", "b")
str_view(c(yes, no), pattern)
# so, to look for feet and inches, we can add the symbol for feet and inches
# string begins, numbers from 4-7, feet symbol, 1 or 2 digits, inches symbol, end of string
pattern <- "^[4-7]'\\d{1,2}\"$"
yes <- c("5'7\"", "6'2\"",  "5'12\"")
no <- c("6,2\"", "6.2\"","I am 5'11\"", "3'2\"", "64")
str_detect(yes, pattern)
str_detect(no, pattern)

## Search and Replace

# only 14 entries match this pattern of x'y"
pattern <- "^[4-7]'\\d{1,2}\"$"
sum(str_detect(problems, pattern))
# we're not finding them all - some students wrote the words "feet" and "inches"
str_subset(problems, "inches")
# some entries use single quotes twice to represent inches
str_subset(problems, "''")
# to fix this, we standardise the way inches and feet are represented to x'y format
# this means we can remove the inches symbol from the end, giving us this:
pattern <- "^[4-7]'\\d{1,2}$"
# by doing this replacement before the search, we get many more matches
problems %>% 
  str_replace("feet|ft|foot","'") %>%
  str_replace("inches|in|''|\"", "") %>%
  str_detect(pattern) %>%
  sum
# however, we are still missing some - for example, those with spaces
# a space can be represented with \s, so we can find them like this:
pattern_2 <- "^[4-7]'\\s\\d{1,2}\"$"
str_subset(problems, pattern_2)
# we don't need two patterns - we can use quantifiers to allow but not require spaces
# this is the *, meaning zero or more instances of the previous characters
yes <- c("AB", "A1B", "A11B", "A111B", "A1111B")
no <- c("A2B", "A21B")
str_detect(yes, "A1*B")
str_detect(no, "A1*B")
# so we add an asterisk after the space character to detect more spaces
# for none or one, we can use ? and for one or more we can
data.frame(string = c("AB", "A1B", "A11B", "A111B", "A1111B"),
           none_or_more = str_detect(yes, "A1*B"),
           none_or_one = str_detect(yes, "A1?B"),
           once_or_more = str_detect(yes, "A1+B"))

# update pattern by adding optional spaces before and after feet symbol
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
problems %>% 
  str_replace("feet|ft|foot", "'") %>% # replace feet, ft, foot with ' 
  str_replace("inches|in|''|\"", "") %>% # remove all inches symbols
  str_detect(pattern) %>% 
  sum()


# we need to be careful - some of th examples have spaces but no symbols, so
# removing the spaces means 6 1 turns into 61 inches

## Groups with Regex

# another problematic group is x,y, x.y and x y
# we want to change all of these to x'y, but replace will not work because
# we would change 70.5 to 70'5
# we therefore need to check for a pattern showing both feet and inches
# groups allow this, and are defined within brackets
# we therefore need to require the first digit to be between 4 and 7, and the 
# second number to be 0 or more digits
# these are two groups, so we contain them in brackets
# this does not affect detections, it just tells R that we want to keep this information
# define regex with and without groups
pattern_without_groups <- "^[4-7],\\d*$"
pattern_with_groups <-  "^([4-7]),(\\d*)$"
# create examples
yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)
# demonstrate the effect of groups
str_detect(s, pattern_without_groups)
str_detect(s, pattern_with_groups)

# once we define groups, string_match can extract the information we want
# demonstrate difference between str_match and str_extract
str_match(s, pattern_with_groups)
str_extract(s, pattern_with_groups)

# these values can also be referred to using \\i, so \\1 is the first group
# so this code replaces a comma with ', but only if it is between two digits:
pattern_with_groups <-  "^([4-7]),(\\d*)$"
yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)
str_replace(s, pattern_with_groups, "\\1'\\2")

# we can now define a pattern to replace x,y, x.y and x y
# start of string, digit between 4-7, none or more spaces, feet symbol is either 
# , . or a space, then none or more spaces, none or more digits, end
pattern_with_groups <- "^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$"

## Testing and Improving

# we will now define a function which can capture everything which won't be converted
# into numbers
# function to detect entries with problems
not_inches_or_cm <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- !is.na(inches) &
    ((inches >= smallest & inches <= tallest) |
       (inches/2.54 >= smallest & inches/2.54 <= tallest))
  !ind
}

# identify entries with problems
problems <- reported_heights %>% 
  filter(not_inches_or_cm(height)) %>%
  .$height
length(problems)

# so we have 200 problems - let's apply our conversion
converted <- problems %>% 
  str_replace("feet|foot|ft", "'") %>% #convert feet symbols to '
  str_replace("inches|in|''|\"", "") %>%  #remove inches symbols
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") ##change format

# find proportion of entries that fit the pattern after reformatting
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
index <- str_detect(converted, pattern)
mean(index)

# we now match more than half - what remains?
converted[!index]

# the remaining problems are:
# 1. students measuring exactly 5 or 6 feet just entered 5 or 6
# 2. inches entered with decimal points
# 3. some entries have spaces at the end
# 4. some entries are in meters, some of which also use commas
# 5. some students typed "cm"
# 6. one typed out the numbers

# these cases are rare, but it may be worth fixing them if you want

## Seperate with Regex

# we want to extract the feet and inches to help us convert them
s <- c("5'10", "6'1")
tab <- data.frame(x=s)
tab %>% separate(x, c("feet", "inches"), sep="'")
# the extract function lets us use regex groups to do this
tab %>% extract(x, c("feet", "inches"), regex="(\\d)'(\\d{1,2})")
# using groups gives us more flexibility - separate fails here
s <- c("5'10", "6'1", "5'8inches")
tab <- data.frame(x=s)
tab %>% separate(x, c("feet", "inches"), sep="'")
# however, regex (with single quote in space permitted) succeeds
tab %>% extract(x, c("feet", "inches"), regex="(\\d)'(\\d{1,2})")

# the end result is this:
pattern <- "^([4-7])\\s*'\\s*(\\d+\\.?\\d*)$"

smallest <- 50
tallest <- 84
new_heights <- reported_heights %>% 
  mutate(original = height, 
         height = words_to_numbers(height) %>% convert_format()) %>%
  extract(height, c("feet", "inches"), regex = pattern, remove = FALSE) %>% 
  mutate_at(c("height", "feet", "inches"), as.numeric) %>%
  mutate(guess = 12*feet + inches) %>%
  mutate(height = case_when(
    !is.na(height) & between(height, smallest, tallest) ~ height, #inches 
    !is.na(height) & between(height/2.54, smallest, tallest) ~ height/2.54, #centimeters
    !is.na(height) & between(height*100/2.54, smallest, tallest) ~ height*100/2.54, #meters
    !is.na(guess) & inches < 12 & between(guess, smallest, tallest) ~ guess, #feet'inches
    TRUE ~ as.numeric(NA))) %>%
  select(-guess)

## String Splitting

# imagine we need to read a csv line by line
filename <- system.file("extdata/murders.csv", package = "dslabs")
lines <- readLines(filename)
lines %>% head()
# each string is a row, and we can split it using str_split
x <- str_split(lines, ",")
x %>% head()
# we can also separate out the column name 
col_names <-x[[1]]
x <- x[-1]
# we can use map to put this into a data frame
map(x, function(y) y[1]) %>% head()
# this is a common task, so there is a shortcut:
map(x,1) %>% head()
# to force a character vector instead of a list, we can use map_chr()
# map_int is the same for integers
dat <- data.frame(map_chr(x,1),
                  map_chr(x,2),
                  map_chr(x,3),
                  map_chr(x,4),
                  map_chr(x,5)) %>%
  mutate_all(parse_guess) %>%
  setNames(col_names)
dat %>%head                    

## Recoding

# imagine we want to change the level names in a data frame
# for example, "United Kingdom" to "UK"
# we can use case_when for this, but recode() is quicker
library(dslabs)
data("gapminder")
# we want to show life expectancy for countries in the Carribbean
gapminder %>% filter(region=="Caribbean") %>%
  ggplot(aes(year, life_expectancy, color=country)) +
  geom_line()
# in the graph, we waste space because of the long country names
gapminder %>% filter(region=="Caribbean") %>%
  filter(str_length(country)>=12) %>%
  distinct(country)
# if we change them, we need to change them all consistently
# recode long country names and remake plot
gapminder %>% filter(region=="Caribbean") %>%
  mutate(country = recode(country, 
                          'Antigua and Barbuda'="Barbuda",
                          'Dominican Republic' = "DR",
                          'St. Vincent and the Grenadines' = "St. Vincent",
                          'Trinidad and Tobago' = "Trinidad")) %>%
  ggplot(aes(year, life_expectancy, color = country)) +
  geom_line()

## Assessment: Brexit

library(rvest)
library(tidyverse)
library(stringr)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab <- read_html(url) %>% html_nodes("table")
polls <- tab[[6]] %>% html_table(fill = TRUE)

# Update polls by changing the column names to c("dates", "remain", "leave", 
# "undecided", "lead", "samplesize", "pollster", "poll_type", "notes") and only
# keeping rows that have a percent sign (%) in the remain column.
colnames(polls) <- c("dates", "remain", "leave", "undecided", "lead", "samplesize", "pollster", "poll_type", "notes")
polls[str_detect(polls$remain, "%")]
      

temp <- str_extract_all(polls$dates, "\\d+\\s[a-zA-Z]{3,5}")
end_date <- sapply(temp, function(x) x[length(x)]) # take last element (handles polls that cross month boundaries)
