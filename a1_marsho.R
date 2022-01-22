#=========================================================================
# Econ 613 Homework 1: Data
# Hannah Marsho
#=========================================================================

library(dplyr)
library(readr)
library(haven)
library(tidyverse)
library(ggplot2)
setwd("/Users/hannahmarsho/Desktop/econ613_wd/Data")

#=========================================================================
# Exercise 1: Basic Statistics
#=========================================================================

# a) Number of households surveyed in 2007.

dathh2007 <- read.csv('dathh2007.csv')
num_hh_2007 <- nrow(dathh2007)
num_hh_2007 # 10498

# b) Number of households with marital status "Couple with kids" in 2005.

dathh2005 <- read.csv('dathh2005.csv')
num_hh_couplewkids_2005 <- sum(dathh2005$mstatus == "Couple, with Kids")
num_hh_couplewkids_2005 # 3374

# c) Number of individuals surveyed in 2008.

datind2008 <- read.csv('datind2008.csv')
num_ind_2008 <- nrow(datind2008)
num_ind_2008 # 25510

# d) Number of individuals aged between 25 and 35 in 2016.

datind2016 <- read.csv('datind2016.csv')
num_ind_age2535_2016 <- sum(25 <= datind2016$age & datind2016$age <= 35)
num_ind_age2535_2016 # 2765

# e) Cross-table gender/profession in 2009.

datind2009 <- read.csv('datind2009.csv')
gender_profession_2009 <- table(datind2009$gender, datind2009$profession)
gender_profession_2009 # displays required cross-table

# f) Distribution of wages in 2005 and 2019. Report the mean, the standard deviation, the inter-decile ratio D9/D1, and the Gini coefficient.

datind2005 <- read.csv('datind2005.csv')
datind2005 <- datind2005 %>% filter(wage > 0 | empstat == "Unemployed") %>% filter(is.na(wage) == FALSE) # must limit only to those in the labor force (no children or retirees, for example)
mean_wage_2005 <- mean(datind2005$wage, na.rm = TRUE)
mean_wage_2005 # 21219.94
stdev_wage_2005 <- sd(datind2005$wage, na.rm = TRUE)
stdev_wage_2005 # 18300.66
decile_wage_2005 <- quantile(datind2005$wage, probs = seq(0.1, 0.9, by = 0.1), na.rm = TRUE)
D9D1_ratio_2005 <- decile_wage_2005[9] / decile_wage_2005[1]
D9D1_ratio_2005 # 23.2218
total_wage_2005 <- sum(decile_wage_2005)
prop_wage_2005 <- datind2005 %>% summarise(decile_wage_2005/total_wage_2005)
area_under_curve_2005 <- datind2005 %>% summarise(0.1*prop_wage_2005)
total_area_under_curve_2005 = sum(area_under_curve_2005)
# area under perfect equality Lorenz curve is 0.5
gini_coef_2005 <- (0.5-total_area_under_curve_2005)/0.5 # 0.8

datind2019 <- read.csv('datind2019.csv')
datind2019 <- datind2019 %>% filter(wage > 0 | empstat == "Unemployed") %>% filter(is.na(wage) == FALSE) # must limit only to those in the labor force (no children or retirees, for example)
mean_wage_2019 <- mean(datind2019$wage, na.rm = TRUE)
mean_wage_2019 # 26550.06
stdev_wage_2019 <- sd(datind2019$wage, na.rm = TRUE)
stdev_wage_2019 # 25182.74
decile_wage_2019 <- quantile(datind2019$wage, probs = seq(0.1, 0.9, by = 0.1), na.rm = TRUE)
D9D1_ratio_2019 <- decile_wage_2019[9] / decile_wage_2019[1]
D9D1_ratio_2019 # 26.31411
total_wage_2019 <- sum(decile_wage_2019)
prop_wage_2019 <- datind2019 %>% summarise(decile_wage_2019/total_wage_2019)
area_under_curve_2019 <- datind2019 %>% summarise(0.1*prop_wage_2019)
total_area_under_curve_2019 = sum(area_under_curve_2019)
# area under perfect equality Lorenz curve is 0.5
gini_coef_2019 <- (0.5-total_area_under_curve_2019)/0.5 # 0.8

# g) Distribution of age in 2010. Plot a histogram. Is there any difference between men and women?

datind2010 <- read.csv('datind2010.csv')
hist(datind2010$age, main = "Age", xlab = "Age") # displays required histogram

datind2010_male <-datind2010 %>% filter(datind2010$gender == "Male")
hist(datind2010_male$age, main = "Age - Male", xlab = "Age - Male") # histogram for age in 2010, male only
datind2010_female <- datind2010 %>% filter(datind2010$gender == "Female")
hist(datind2010_female$age, main = "Age - Female", xlab = "Age - Female") # histogram for age in 2010, female only

# There isn't a strong difference in the 2010 age distribution when we consider gender. However, the 2010 age distribution
# for males has slightly stronger humps for ages 0-20 and 40-70 than the female distribution. Also, the 2010 age distribution
# for females has a more gradual drop-off for old age, so women tend to live longer.

# h) Number of individuals in Paris in 2011.

dathh2011 <- read.csv('dathh2011.csv')
datind2011 <- read.csv('datind2011.csv')
dat2011 <- merge(dathh2011, datind2011, by = "idmen")
num_ind_paris_2011 <- count(dat2011$location == "Paris")
num_ind_paris_2011[2,] # 3514
  
#=========================================================================
# Exercise 2: Merge Datasets
#=========================================================================

# a) Read all individual datasets from 2004 to 2019. Append these datasets.

data_all_ind <- list.files(path = "/Users/hannahmarsho/Desktop/econ613_wd/Data", pattern = "datind", full.names = TRUE) %>%
  lapply(read_csv) %>% lapply(as.data.frame)
data_all_ind <- do.call(rbind.data.frame, data_all_ind) # the required appended dataset.

# b) Read all household datasets from 2004 to 2019. Append these datasets.

data_all_hh <- list.files(path = "/Users/hannahmarsho/Desktop/econ613_wd/Data", pattern = "dathh", full.names = TRUE) %>%
  lapply(read_csv) %>% bind_rows() # the required appended dataset.

# c) List the variables that are simultaneously present in the individual and household datasets.

data_all_ind_names <- names(data_all_ind)
data_all_hh_names <- names(data_all_hh)
intersect(data_all_ind_names, data_all_hh_names) # "...1", "idmen", and "year"

# d) Merge the appended individual and household datasets.

data_all <- merge(data_all_ind, data_all_hh, by = c("idmen", "year"), all = TRUE)

# e) Number of households in which there are more than four family members

data_all_e <- data_all %>% group_by(idmen, year) %>% summarise(n = n()) %>% filter(n > 4)
data_all_e <- data_all_e %>% group_by(idmen) %>% summarise(nmax = max(n))
nrow(data_all_e) # 3622

# f) Number of households in which at least one member is unemployed.

data_all_f <- data_all %>% filter(empstat == "Unemployed") %>% group_by(idmen, year) %>% summarise(n = n()) %>% filter(n >= 1)
data_all_f <- data_all_f %>% group_by(idmen) %>% summarise(nmax = max(n))
nrow(data_all_f) # 8162

# g) Number of households in which at least two members are of the same profession.

data_all_g <- data_all %>% group_by(idmen, year, profession) %>% filter(is.na(profession) == FALSE) %>% summarise(n = n()) %>% filter(n >= 2)
data_all_g <- data_all_g %>% group_by(idmen) %>% summarise(nmax = max(n))
nrow(data_all_g) # 3022

# h) Number of individuals in the panel that are from household-Couple with kids.

data_all_h <- data_all %>% filter(mstatus == "Couple, with Kids") %>% distinct(idind)
nrow(data_all_h) # 15646

# i) Number of individuals in the panel that are from Paris.

data_all_i <- data_all %>% filter(location == "Paris") %>% distinct(idind)
nrow(data_all_i) # 6186

# j) Find the household with the most number of family members. Report its idmen.

max(data_all_e$nmax) # 14
which(data_all_e$nmax == 14) # 1524 and 2114. the index positions of the two households tied for most members.
data_all_e[1524, 1] # 2.21e15. idmen of the first household with 14 members.
data_all_e[2114, 1] # 2.51e15. idmen of the second household with 14 members.

# k) Number of households present in 2010 and 2011.

data_all_k <- data_all %>% filter(year == "2010" | year == "2011") %>% group_by(idmen) %>% summarise(nyears = n_distinct(year)) %>% filter(nyears == 2)
nrow(data_all_k) # 8984

#=========================================================================
# Exercise 3: Migration
#=========================================================================

# a) Find out the year each household enters and exits the panel. Report the distribution of the time spent in the survey for each household.

data_all_3 <- data_all %>% group_by(idmen) %>% mutate(entrance_year = min(year)) %>% mutate(exit_year = max(year)) %>% mutate(time_spent = exit_year - entrance_year + 1) # need to add 1 for time_spent so it's inclusive of the first survey year
entrance_years_3 <- data.frame(idmen = data_all_3$idmen, entrance_year = data_all_3$entrance_year) %>% distinct() # the year each household enters the panel
exit_years_3 <- data.frame(idmen = data_all_3$idmen, exit_year = data_all_3$exit_year) %>% distinct() # the year each household exits the panel
hist(data_all_3$time_spent, main = "Distribution of Time Spent in Panel", xlab = "Time Spent (in years)") # displays required distribution with a histogram

# b) Based on datent, identify whether or not a household moved into its current dwelling at the year of survey. Report the first 10 rows of your result and plot the share of individuals in that situation across years.

data_all_3 <- data_all_3 %>% group_by(idmen, year) %>% mutate(sameyear = (datent == year)*1)
movedin_same_year <- data.frame(idmen = data_all_3$idmen, idind = data_all_3$idind, year = data_all_3$year, sameyear = data_all_3$sameyear) # returns 1 if household moved into its current dwelling at the year of survey. 0 if not.
head(movedin_same_year, 10) # first 10 rows of result
num_ind_movedin_same_year <- movedin_same_year %>% group_by(year) %>% summarise(num_ind_sameyear = n())
num_ind_movedin_same_year %>% ggplot(aes(x = year, y = num_ind_sameyear)) + geom_point() + labs(x = "Year", y = "Number of Individuals Who Moved in That Survey Year") #displays required plot

# c) Based on myear and move, identify whether or not household migrated at the year of survey. Report the first 10 rows of your result and plot the share of individuals in that situation across years.
# myear up to and including 2014, move is for after 2014

data_all_3 <- data_all_3 %>% group_by(idmen, year) %>% mutate(sameyear2_indicator = case_when(year <= 2014 ~ "use myear",
                                                                                             year > 2014 ~ "use move"))
if (data_all_3$sameyear2_indicator == "use myear") {
  data_all_3 <- data_all_3 %>% group_by(idmen, year) %>% mutate(sameyear2 = (myear == year)*1)
} else {
  data_all_3 <- data_all_3 %>% group_by(idmen, year) %>% mutate(sameyear2 = move - 1)
}
movedin_same_year2 <- data.frame(idmen = data_all_3$idmen, idind = data_all_3$idind, year = data_all_3$year, sameyear2 = data_all_3$sameyear2) # returns 1 if household moved into its current dwelling at the year of survey. 0 if not.
head(movedin_same_year2, 10) # first 10 rows of result
num_ind_movedin_same_year2 <- movedin_same_year2 %>% group_by(year) %>% summarise(num_ind_sameyear2 = n())
num_ind_movedin_same_year2 %>% ggplot(aes(x = year, y = num_ind_sameyear2)) + geom_point() + labs(x = "Year", y = "Number of Individuals Who Moved in That Survey Year") #displays required plot

# d) Mix the two plots you created above in one graph, clearly label the graph. Do you prefer one method over the other? Justify.

movedin_same_year_combined <- data.frame(movedin_same_year, movedin_same_year2)
ggplot(movedin_same_year_combined, aes(x = year)) +
  # plot from 3b with big points in red
  geom_point(data = num_ind_movedin_same_year, aes(y = num_ind_sameyear, colour = "red", size = 4.5)) +
  # plot from 3c with small points in black
  geom_point(data = num_ind_movedin_same_year2, aes(y = num_ind_sameyear2, size = 4)) +
  labs(x = "Year", y = "Number of Individuals Who Moved in That Survey Year") #displays required plot.

# I prefer the method used in part b. This is because you can simply use one variable, datent, in order to find our solution. With the method in
# part c, you are required to use two variables, myear and move, in order to obtain the same result. With this second method, you must create the solution
# conditionally on which of the two, myear and move, is available for a given data point. This makes the process a bit more cumbersome and increases the likelihood
# of error since there are two variables for participants and researchers to interpret and use. So, it is simpler to use the method in part c.

# e) For households who migrate, find out how many households had at least one family member changed his/her profession or employment status.

migrating_hh <- movedin_same_year
hh_profession_empstat <- data.frame(idmen = data_all$idmen, idind = data_all$idind, profession = data_all$profession, empstat = data_all$empstat)
migrating_hh_comp <- merge(migrating_hh, hh_profession_empstat, by = c("idmen", "idind"), all = TRUE) 
migrating_hh_comp <- migrating_hh_comp %>% mutate(sameyear_indicator = case_when(sameyear == 1 ~ "evaluate",
                                                                                sameyear == 0 ~ "don't"))
if (migrating_hh_comp$sameyear_indicator == "evaluate") {
  migrating_hh_comp <- migrating_hh_comp %>% group_by(idind) %>% mutate(profession_change, ifelse(n_distinct(profession > 1), 1, 0))
  migrating_hh_comp <- migrating_hh_comp %>% group_by(idind) %>% mutate(empstat_change, ifelse(n_distinct(empstat > 1), 1, 0))
}
migrating_hh_with_changes <- migrating_hh_comp %>% filter(profession_change == 1 | empstat_change == 1) %>% summarise(idmen) %>% distinct() 
nrow(migrating_hh_with_changes) # had issues getting the if statement to work. this line would give final answer though.

#=========================================================================
# Exercise 4: Attrition
#=========================================================================

# Compute the attrition across each year, where attrition is defined as the reduction in the number of individuals staying in the data panel. Report your final result as a table in proportions.

data_all_4 <- data_all %>% group_by(idind) %>% mutate(entrance_year = min(year)) %>% mutate(exit_year = max(year))
entrance_years_4 <- data.frame(idind = data_all_4$idind, entrance_year = data_all_4$entrance_year) %>% distinct() # the year each individual enters the panel
exit_years_4 <- data.frame(idind = data_all_4$idind, exit_year = data_all_4$exit_year) %>% distinct() # the year each individual exits the panel
ind_enter_exit <- merge(entrance_years_4, exit_years_4, by = "idind")
years = c(seq(from = 2004, to = 2019, by = 1))
ind_enter_exit <- cbind(ind_enter_exit, as.list(years))
n = 4
ind_enter_exit %>%
  for (i in years) {
    if (entrance_year <= i && exit_year >= i) {
      replace(ind_enter_exit, n, 1)
    } else {
      replace(ind_enter_exit, n, 0)
    }
  n = n + 1
}
ind_byyear <- mutate_all(ind_enter_exit, sum())
attrition_byyear <- ind_byyear %>% mutate(diff = value - lag(value, default = first(value))) # value would be whatever the name of the sum column becomes
attrition <- attrition_byyear / ind_byyear # puts the annual attrition into proportion
