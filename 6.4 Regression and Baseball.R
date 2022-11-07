## Building a Better Offensive Metric

library(tidyverse)
library(Lahman)
library(dslabs)
library(broom)
ds_theme_set()

# NOTE: The dataset Master has been renamed as People in newer versions of Lahman package

# if we want to predict runs by bases on balls and home runs, we need to include
# two predictors like this:
fit <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(BB = BB/G, HR = HR/G,  R = R/G) %>%  
  lm(R ~ BB + HR, data = .)
# and we can use the tidy function to see a neat summary:
tidy(fit, conf.int = TRUE)

# now we want a model which predicts based on singles, doubles and triples as well
# we are therefore assuming that these variables are all normally distributed
fit <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(BB = BB / G, 
         singles = (H - X2B - X3B - HR) / G, 
         doubles = X2B / G, 
         triples = X3B / G, 
         HR = HR / G,
         R = R / G) %>%  
  lm(R ~ BB + singles + doubles + triples + HR, data = .)
coefs <- tidy(fit, conf.int = TRUE)
coefs

# to test how well our metric actually predicts runs, we predict the runs in 
# 2002, which was not included in the original data
Teams %>% 
  filter(yearID %in% 2002) %>% 
  mutate(BB = BB/G, 
         singles = (H-X2B-X3B-HR)/G, 
         doubles = X2B/G, 
         triples =X3B/G, 
         HR=HR/G,
         R=R/G)  %>% 
  mutate(R_hat = predict(fit, newdata = .)) %>%
  ggplot(aes(R_hat, R, label = teamID)) + 
  geom_point() +
  geom_text(nudge_x=0.1, cex = 2) + 
  geom_abline()
# our model does quite well - the points are close to the line

# these metrics are based on team-level summaries, so it is not for individual
# players. we want a model to cover players, because we are buying individual players.
# however, not all players get equal opportunities to play, but these are all still
# considered a game played. therefore, we must calculate stats per plate appearance
pa_per_game <- Batting %>% filter(yearID == 2002) %>% 
  group_by(teamID) %>%
  summarize(pa_per_game = sum(AB+BB)/max(G)) %>% 
  pull(pa_per_game) %>% 
  mean

# compute per-plate-appearance rates for players available in 2002 using previous data
players <- Batting %>% filter(yearID %in% 1999:2001) %>% 
  group_by(playerID) %>%
  mutate(PA = BB + AB) %>%
  summarize(G = sum(PA)/pa_per_game,
            BB = sum(BB)/G,
            singles = sum(H-X2B-X3B-HR)/G,
            doubles = sum(X2B)/G, 
            triples = sum(X3B)/G, 
            HR = sum(HR)/G,
            AVG = sum(H)/sum(AB),
            PA = sum(PA)) %>%
  filter(PA >= 300) %>%
  select(-G) %>%
  mutate(R_hat = predict(fit, newdata = .))
# therefore, this model can be defined as the amount of runs we'd expect if
# a team was full of players exactly like the player in question

# plot player-specific predicted runs
qplot(R_hat, data = players, geom = "histogram", binwidth = 0.5, color = I("black"))
#we see wide variability between player performances, as expected

# to make a purchasing decision, we also need to factor their salary in
players <- Salaries %>% 
  filter(yearID == 2002) %>%
  select(playerID, salary) %>%
  right_join(players, by="playerID")

# we also need to know their position, because we need one of each, and to remove pitchers
position_names <- c("G_p","G_c","G_1b","G_2b","G_3b","G_ss","G_lf","G_cf","G_rf")
tmp_tab <- Appearances %>% 
  filter(yearID == 2002) %>% 
  group_by(playerID) %>%
  summarize_at(position_names, sum) %>%
  ungroup()  
pos <- tmp_tab %>%
  select(position_names) %>%
  apply(., 1, which.max) 
players <- data_frame(playerID = tmp_tab$playerID, POS = position_names[pos]) %>%
  mutate(POS = str_to_upper(str_remove(POS, "G_"))) %>%
  filter(POS != "P") %>%
  right_join(players, by="playerID") %>%
  filter(!is.na(POS)  & !is.na(salary))

# add players' first and last names
players <- People %>%
  select(playerID, nameFirst, nameLast, debut) %>%
  mutate(debut = as.Date(debut)) %>%
  right_join(players, by="playerID")

# and then view the top 10 players by run production
players %>% select(nameFirst, nameLast, POS, salary, R_hat) %>% 
  arrange(desc(R_hat)) %>% 
  top_n(10) 

# we find that players with higher metrics have better salaries
players %>% ggplot(aes(salary, R_hat, color = POS)) + 
  geom_point() +
  scale_x_log10()

# it takes players some time to negotiate a salary and debut, so we remove players
# who did not begin until after 1998
library(lubridate)
players %>% filter(year(debut) < 1998) %>%
  ggplot(aes(salary, R_hat, color = POS)) + 
  geom_point() +
  scale_x_log10()
# we can now look for good deals by looking for players who produce more runs 
# than other players with similar salaries

## Linear Programming

# we can also approach this using linear programming. 
library(reshape2)
library(lpSolve)

players <- players %>% filter(debut <= "1997-01-01" & debut > "1988-01-01")
constraint_matrix <- acast(players, POS ~ playerID, fun.aggregate = length)
npos <- nrow(constraint_matrix)
constraint_matrix <- rbind(constraint_matrix, salary = players$salary)
constraint_dir <- c(rep("==", npos), "<=")
constraint_limit <- c(rep(1, npos), 50*10^6)
lp_solution <- lp("max", players$R_hat,
                  constraint_matrix, constraint_dir, constraint_limit,
                  all.int = TRUE) 

# the above algorithm chooses these 9 players:
our_team <- players %>%
  filter(lp_solution$solution == 1) %>%
  arrange(desc(R_hat))
our_team %>% select(nameFirst, nameLast, POS, salary, R_hat)

my_scale <- function(x) (x - median(x))/mad(x)
players %>% mutate(BB = my_scale(BB), 
                   singles = my_scale(singles),
                   doubles = my_scale(doubles),
                   triples = my_scale(triples),
                   HR = my_scale(HR),
                   AVG = my_scale(AVG),
                   R_hat = my_scale(R_hat)) %>%
  filter(playerID %in% our_team$playerID) %>%
  select(nameFirst, nameLast, BB, singles, doubles, triples, HR, AVG, R_hat) %>%
  arrange(desc(R_hat))

## On Base Plus Slugging (OPS)

# eventually, statisticians realised that walks are important, and doubles, triples
# and home runs should be weighted more heavily than singles. they created a new metric
# which did this, called on-base percentage plus slugging percentage (OPS)

## Regression Fallacy

# a sophopmore slump is when a second attempt at something (2nd year students, 
# athletes, etc) don't do as well as the first. but is this true?

# we first create a table with player ID, name, and their most played position
library(Lahman)
playerInfo <- Fielding %>%
  group_by(playerID) %>%
  arrange(desc(G)) %>%
  slice(1) %>%
  ungroup %>%
  left_join(People, by="playerID") %>%
  select(playerID, nameFirst, nameLast, POS)

# then we make a table with only the Rookie of the Year winners and their stats:
ROY <- AwardsPlayers %>%
  filter(awardID == "Rookie of the Year") %>%
  left_join(playerInfo, by="playerID") %>%
  rename(rookie_year = yearID) %>%
  right_join(Batting, by="playerID") %>%
  mutate(AVG = H/AB) %>%
  filter(POS != "P")

# and remove players who did not play a sophomore season:
ROY <- ROY %>%
  filter(yearID == rookie_year | yearID == rookie_year+1) %>%
  group_by(playerID) %>%
  mutate(rookie = ifelse(yearID == min(yearID), "rookie", "sophomore")) %>%
  filter(n() == 2) %>%
  ungroup %>%
  select(playerID, rookie_year, rookie, nameFirst, nameLast, AVG)

# we use spread to have one column for rookie and sophomore batting averages:
ROY <- ROY %>% spread(rookie, AVG) %>% arrange(desc(rookie))
ROY
# from this, it appears to be real - the overall proportion of people who batted
# worse in their second season is 69.8%
mean(ROY$sophomore - ROY$rookie <= 0)

# to try and see why this happens, we will look at all players in 2013 and 2014
# players must have batted 130 times to be eligible for the award
two_years <- Batting %>%
  filter(yearID %in% 2013:2014) %>%
  group_by(playerID, yearID) %>%
  filter(sum(AB) >= 130) %>%
  summarize(AVG = sum(H)/sum(AB)) %>%
  ungroup %>%
  spread(yearID, AVG) %>%
  filter(!is.na(`2013`) & !is.na(`2014`)) %>%
  left_join(playerInfo, by="playerID") %>%
  filter(POS!="P") %>%
  select(-POS) %>%
  arrange(desc(`2013`)) %>%
  select(nameFirst, nameLast, `2013`, `2014`)
two_years
# the same pattern arises - batting averages go down - but these are not rookies?

# then if we look at the worst performers of 2013 - they go up!
arrange(two_years, `2013`)
# is this a reveger sophomore slump?

# no - the correlation of performance between two years is high, but not perfect
qplot(`2013`, `2014`, data = two_years)
summarize(two_years, cor(`2013`,`2014`))

# the data is a bivariate normal distribution, but because of the way regression works,
# we expect regression towards the mean in the next year - either positive or negative
# in other words, rookies of the year (and the worst performers) are already far from
# the mean, so they are expected to regress towards it in the next season. 

## Measurement Error Models

# until now, all our models have included random variables only. however, you can also
# include non-random variables, such as time. if you do this, most of your error will
# come from measurement error. These are called Measurement Error Models. 

# imagine you are galileo trying to describe the velocity of a falling object
# an assistant climbs a tower and drops a ball, and you measure the position as it drops
library(dslabs)
falling_object <- rfalling_object()

# we can draw the trajectory of the ball on a graph:
falling_object %>%
  ggplot(aes(time, observed_distance)) +
  geom_point() +
  ylab("Distance in meters") +
  xlab("Time in seconds")
# the relationship is parabolic, but not exactly - this must be measurement error

# we can use lm() to estimate the parameters:
fit <- falling_object %>%
  mutate(time_sq = time^2) %>%
  lm(observed_distance~time+time_sq, data=.)
tidy(fit)

# and we can check if the parabola fits the data:
augment(fit) %>%
  ggplot() +
  geom_point(aes(time, observed_distance)) +
  geom_line(aes(time, .fitted), col = "blue")

# and summarise the regression:
tidy(fit, conf.int = TRUE)

## Assessment

# Use the Teams data frame from the Lahman package. Fit a multivariate linear 
# regression model to obtain the effects of BB and HR on Runs (R) in 1971. Use 
# the tidy() function in the broom package to obtain the results in a data frame.
fit <- Teams %>% 
  filter(yearID == 1971) %>% 
  mutate(BB = BB/G, HR = HR/G,  R = R/G) %>%  
  lm(R ~ BB + HR, data = .)
tidy(fit, conf.int = TRUE)

# Repeat the above exercise to find the effects of BB and HR on runs (R) for 
# every year from 1961 to 2018 using do() and the broom package.
res <- Teams %>%
  filter(yearID %in% 1961:2018) %>%
  group_by(yearID) %>%
  do(tidy(lm(R ~ BB + HR, data = .))) %>%
  ungroup() 
res %>%
  filter(term == "BB") %>%
  ggplot(aes(yearID, estimate)) +
  geom_point() +
  geom_smooth(method = "lm")

# Fit a linear model on the results from Question 10 to determine the effect of 
# year on the impact of BB.
mod <- res %>% filter(term=="BB") %>% lm(estimate ~ yearID, data = .) 
tidy(mod, conf.int = TRUE)
  
## Assessment 2
library(tidyverse)
library(broom)
library(Lahman)
Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G)

# Use runs (R) per game to predict average attendance.
Teams_small %>% mutate(RPG = R/G) %>% lm(avg_attendance ~ RPG, data=.)
# Use home runs (HR) per game to predict average attendance.
Teams_small %>% mutate(HRPG = HR/G) %>% lm(avg_attendance ~ HRPG, data=.)

# Use number of wins to predict average attendance; do not normalize for number of games.
# For every game won in a season, how much does average attendance increase?
Teams_small %>% lm(avg_attendance ~ W, data=.)

# Use year to predict average attendance.
Teams_small %>% lm(avg_attendance ~ yearID, data=.)

# Are wins and runs per game or wins and home runs per game correlated?
Teams_small %>% mutate(HRPG = HR/G) %>% summarize(cor(HRPG, W))
Teams_small %>% mutate(RPG = R/G) %>% summarize(cor(RPG, W))

# Stratify Teams_small by wins: divide number of wins by 10 and then round to the
# nearest integer. Filter to keep only strata 5 through 10. (The other strata have
# fewer than 20 data points, too few for our analyses).
dat <- Teams_small %>%
  mutate(W_strata = round(W/10)) %>%
  filter(W_strata >= 5 & W_strata <= 10)
# How many observations are in the 8 win strata?
sum(dat$W_strata == 8)

# Calculate the slope of the regression line predicting average attendance 
# given runs per game for each of the win strata.
dat %>%  
  group_by(W_strata) %>%
  summarize(slope = cor(R/G, avg_attendance)*sd(avg_attendance)/sd(R/G))

# Calculate the slope of the regression line predicting average attendance given
# HR per game for each of the win strata.
dat %>%  
  group_by(W_strata) %>%
  summarize(slope = cor(HR/G, avg_attendance)*sd(avg_attendance)/sd(HR/G))

# Fit a multivariate regression determining the effects of runs per game, home 
# runs per game, wins, and year on average attendance.
Teams_small %>% mutate(RPG = R/G, HRPG = HR/G) %>% 
  lm(avg_attendance ~ HRPG+RPG+W+yearID, data=.)

# Suppose a team averaged 5 runs per game, 1.2 home runs per game, and won 80 
# games in a season. Use the predict() function to generate predictions for this team.
fit <- Teams_small %>% mutate(RPG = R/G, HRPG = HR/G) %>% 
  lm(avg_attendance ~ HRPG+RPG+W+yearID, data=.)
predict(fit, data.frame(RPG = 5, HRPG = 1.2, W = 80, yearID = 2002))
predict(fit, data.frame(RPG = 5, HRPG = 1.2, W = 80, yearID = 1960))

# Use your model from Question 4 to predict average attendance for teams in 2002
# in the original Teams data frame.
# What is the correlation between the predicted attendance and actual attendance?
newdata <- Teams %>%
  filter(yearID == 2002) %>%
  mutate(avg_attendance = attendance/G,
         RPG = R/G,
         HRPG = HR/G)
preds <- predict(fit, newdata)
cor(preds, newdata$avg_attendance)
