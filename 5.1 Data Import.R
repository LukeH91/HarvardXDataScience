## Importing Spreadsheets
library(tidyverse)

# tidyverse makes it a lot easier to import spreadsheets for analysis
# when saving spreadsheets, a character is needed to denote where a new row
# and column begins. most often these are tabs, semicolons, commas, or spaces
# these files also typically have a "header", a list of colum headings

## Paths and the Working Directory

# the working directory is the "base" directory that R is working in
getwd()
# you can change it using setwd()
setwd("./EDX - R Data Science Professional Certificate/murders")
# unless a full path is given, commands will search the working directory
list.files()
# dslabs comes with some test data, found here
system.file("extdata", package="dslabs")
# the file.path function can be used to create a full path to a file
filename <- "murders.csv"
path <- getwd()
fullpath <- file.path(path, filename)
fullpath

# we can also copy a file to the directory like this
file.copy("D:/Windows/Downloads/textfile.txt", getwd())

# we can check that files exist using file.exists
file.exists("murders.csv")

## readr and readxl

# the following commands can be used to read in data files:
# read_table - white space seperated values, txt
# read_csv - comma seperated values, csv
# read_csv2 - semicolon seperated values, csv
# read_tsv - tab seperated values, tsv
# read_delim - general text file format, delimiter must be defined - txt

# there are also commands specifically for excel:

# read_excel - auto-detect format - xls, xlsx
# read_xls - old format - xls
# read_xlsx - new format - xlsx

# excel_sheets gives you the list of names of sheets within a single file, 
# and these names can be passed to the sheet argument in the three commands above

# we can look at the files to make sure that the filename is correct
read_lines("murders.csv", n_max=3)

# we can see that we should use read_csv
dat <- read_csv("murders.csv")

## Importing Data Using R-Base

# R has its own data reading commands, such as read.table, read.csv and read.delim
# these are different in a few ways - for example, they produce data frames, not tibbles
dat2 <- read.csv("murders.csv")
class(dat)
class(dat2)

## Downloading Files from the Internet

# read_csv can also read files directly from URLs
url <- "https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv"
dat <- read_csv(url)
# to make a local copy, you can download it
download.file(url, "murders.csv")

# when downloading, it is sometimes a good idea to use tempdir and tempfile
tempfile()

# therefore, we can use this to download a file, give it a fake name, then delete it
tmp_filename <- tempfile()
download.file(url, tmp_filename)
dat <- read_csv(tmp_filename)
file.remove(tmp_filename)
