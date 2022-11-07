## ggplot

## ggplot allows beginners to make complex, nice-looking plots easily
# gg means "grammar of graphics", in other words, commands that work on 
# many kinds of plots
# however, ggplot only works with data tables, in which rows must be
# observations and columns must be variables

# ggplot2, dplyr and other useful packages can be loaded via tidyverse
library(tidyverse)

library(dslabs)
data(murders)

## Graph Components

# a basic plot has three components:
# 1. data: the data set that is being summarised
# 2. geometry: the type of plot (scatter, bar, hist, smooth dens, q-q, box)
# 3. aesthetic mappings: x and y-axes, text, fit lines, colours, etc
# 4. scale component: the scale and range of the axes
# 5. labels, title, legend: the text components on the graph

## Creating a Plot

# we first define a ggplot object
p <- ggplot(data=murders)

# we can also pipe data into ggplot
data(murders) %>% ggplot

p
library(tidyverse)
library(dslabs)
data(murders)
data(heights)

## Layers

p <- ggplot(data=murders)

# the documentation tells us we need geom_point for a scatter plot
# this function also has its own documentation
?geom_point

# the documentation tells us that it requires two arguments, x and y
# the aes function connects the data with the graph, and is often used
# as the argument in a geometry function
murders %>% ggplot() + 
  geom_point(aes(x=population/10^6, y=total))

# we can also define an object, then add a layer to it later
p <- ggplot(data = murders)
p + geom_point(aes(population/10^6, total))
# note that aes lifts the variable names from the data object directly

# we also want to add labels and text using geom_label and geom_text
# each state has a label, so we need to add this
p + geom_point(aes(population/10^6, total)) +
  geom_text(aes(population/10^6, total, label=abb))

## Tinkering

# if we want to change the size of the points, we look in the help file for
# geom_point, where we see that size is an aesthetic it understands
p + geom_point(aes(population/10^6, total, size=3)) +
  geom_text(aes(population/10^6, total, label=abb))

# with the larger points, we now need to move the labels a bit
p + geom_point(aes(population/10^6, total, size=3)) +
  geom_text(aes(population/10^6, total, label=abb), nudge_x=1)

# our code is inefficient - we define points twice, once for each geom
# instead, we can define a globalaes mapping for it when we call ggplot
# then, all layers will default to this mapping
p <- murders %>% ggplot(aes(population/10^6, total, label=abb))
p + geom_point(size=3) +
  geom_text(nudge_x=1.5)                        

# note that the local mappings override the global ones:
p + geom_point(size=3) +
  geom_text(aes(x=10, y=800, label="hello"))

## Scales, Labels and Color

# we want a log scale, which is not the default
p + geom_point(size=3) + 
  geom_text(nudge_x=0.05) +
  scale_x_continuous(trans = "log10") + 
  scale_y_continuous(trans = "log10")

# since log10 is so common, it also has a dedicated command
p + geom_point(size=3) + 
  geom_text(nudge_x=0.05) +
  scale_x_log10() + 
  scale_y_log10()                                   

# we then add labels to the axes
p + geom_point(size=3) + 
  geom_text(nudge_x=0.05) +
  scale_x_log10() + 
  scale_y_log10() +
  xlab("Populations in millions (log scale)") + 
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010")

# we also want colour. to demonstrate this, we will redefine p
p <- murders %>% ggplot(aes(population/10^6, total, label=abb)) +
  geom_text(nudge_x=0.05) +
  scale_x_log10() + 
  scale_y_log10() +
  xlab("Populations in millions (log scale)") + 
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010")

# we can easily set all points to one colour
p + geom_point(color="blue")

# instead, we want to colour by region, easy with a categorical variable
# we also include size outside of the aes func, since we want it to size all
p + geom_point(aes(col=region), size=3)

# we also want a line showing the average murder rate 
# we first have to calculate the rate for the whole country
r <- murders %>% summarize(rate=sum(total)/sum(population)*10^6) %>% .$rate

# then we add it to the graph using abline
p + geom_point(aes(col=region), size=3) + 
  geom_text(nudge_x=0.05) +
  scale_x_log10() + 
  scale_y_log10() +
  xlab("Populations in millions (log scale)") + 
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010") +
  geom_abline(intercept = log10(r))


# we want to make it dashed, change the color, and put it under the points
# to do this easily, we redefine p, but we could also just put 
# abline before geom_point in the original command
p <- p + 
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  geom_point(aes(col=region), size = 3)

# we want to capitalise the r in region in the legend
# this means we must change the default behaviour of ggplot
p <- p + scale_color_discrete(name = "Region")

## Addon Packages

# ggplot2 includes several themes we can use by calling functions
p

ds_theme_set()
p

# other themes can be added from ggthemes
library(ggthemes)
p + theme_economist()
p + theme_fivethirtyeight()

# finally, some of our labels fall on top of each other and are hard to read
# ggrepel can fix this
library(ggrepel)

# we will build the entire thing from scratch

# first load necessary libraries
library(ggthemes)
library(ggrepel)
# we define the slope of the line by dividing total murders by total population
r <- murders %<% summarize(rate=sum(total)/sum(population)*10^6) %>% .$rate
# then we make the plot
murders %>% ggplot(aes(population/10^6, total, label=abb)) +
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  geom_point(aes(col=region), size = 3) +
  geom_text_repel() +
  scale_x_log10() + 
  scale_y_log10() +
  xlab("Populations in millions (log scale)") + 
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010") + 
  scale_color_discrete(name = "Region") + 
  theme_economist()

## Other Examples

# making a histogram of male heights
data(heights)

# first define an aes which filters the heights to only include males
p <- heights %>% filter(sex=="Male") %>% ggplot(aes(x=height))

# then we create the histogram with the bin width we want
p + geom_histogram(binwidth = 1)

# we can also colour it in and add a title
p + geom_histogram(binwidth = 1, fill = "darkblue", col = "black") +
  xlab("Male heights in inches") +
  ggtitle("Histogram")

# we could also use this for a smooth density plot
p + geom_density(fill = "darkblue") +
  xlab("Male heights in inches") +
  ggtitle("Density Plot")

# we can also make density plots by group using the group argument
heights %>% ggplot(aes(height, group=sex, color=sex)) + geom_density()

# color means we no longer need the group argument, and we can use
# a transparent fill instead
heights %>% ggplot(aes(height, fill=sex)) + geom_density(alpha=0.2)

# qq plot are also possible, but they need a sample instead of a population
p <- heights %>% filter(sex=="Male") %>% ggplot(aes(sample=height))
p + geom_qq()

# by default this is compared to the standard normal distribution
# we can change it to be a normal distribution with the same m/sd as the data
params <- heights %>% filter(sex=="Male") %>% 
  summarize(mean=mean(height), sd = sd(height))
p + geom_qq(dparams=params)

#  we can add an identity line to see how well it fits the distribution
p + geom_qq(dparams=params) +
  geom_abline()

# another approach is to standardise the data, then just plot it against
# the standard normal distribution for the same result

heights %>% filter(sex=="Male") %>%
  ggplot(aes(sample=scale(height))) +
  geom_qq() +
  geom_abline()

# we can also make grids combining several plots using gridExtra
p <- heights %>% filter(sex=="Male") %>% ggplot(aes(x=height))
p1 <- p + geom_histogram(binwidth = 1, fill = "darkblue", col = "black")
p2 <- p + geom_histogram(binwidth = 2, fill = "darkgreen", col = "black")
p3 <- p + geom_histogram(binwidth = 3, fill = "darkred", col = "black")
library(gridExtra)
grid.arrange(p1,p2,p3,ncol=3)
