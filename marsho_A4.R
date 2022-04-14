#=========================================================================
# Econ 613 Homework 4: Censoring and Panel Data
# Hannah Marsho
#=========================================================================

library(tidyverse)
library(readr)
library(ggplot2)
library(VGAM)

setwd("~/Desktop/econ613_wd/A4/Data")
dat <- read.csv('dat_A4.csv')
dat_panel <- read.csv('dat_A4_panel.csv')

#=========================================================================
# Exercise 1: Preparing the Data
#=========================================================================

# a) Create additional variable for the age of the agent "age", total work experience measured in years
# "work_exp".

# Let's calculate age first. I interpret that this question means age at time of final survey/present 
# day (in 2019).

yearofsurvey = 2019
dat <- dat %>% mutate(age_final = yearofsurvey - KEY_BDATE_Y_1997)

# Now let's calculate work experience. I interpret this question as work experience (in weeks) collected 
# from all jobs in total. So, even if the individual had multiple jobs at once (overlap), the experience (in weeks)
# will still be calculated separately for each position.

num_obs <- nrow(dat)
dat <- dat %>% mutate(work_exp = 0)
for (i in 1:num_obs) {
  all_work_exp <- sum(dat$CV_WKSWK_JOB_DLI.01_2019[i], dat$CV_WKSWK_JOB_DLI.02_2019[i], dat$CV_WKSWK_JOB_DLI.03_2019[i],
                 dat$CV_WKSWK_JOB_DLI.04_2019[i], dat$CV_WKSWK_JOB_DLI.05_2019[i], dat$CV_WKSWK_JOB_DLI.06_2019[i],
                 dat$CV_WKSWK_JOB_DLI.07_2019[i], dat$CV_WKSWK_JOB_DLI.08_2019[i], dat$CV_WKSWK_JOB_DLI.09_2019[i],
                 dat$CV_WKSWK_JOB_DLI.10_2019[i], dat$CV_WKSWK_JOB_DLI.11_2019[i], na.rm = TRUE)
  dat$work_exp[i] <- all_work_exp / 52
}

# b) Create additional education variables indicating total years of schooling from all 
# variables related to education in our data set.

# We can create education variables based on: biological father's highest grade completed, biological
# mother's highest grade completed, residential father's highest grade completed, residential
# mother's highest grade completed, and highest degree ever received.

to_drop1 <- which(dat$CV_HGC_BIO_DAD_1997 == 95)
to_drop2 <- which(dat$CV_HGC_BIO_MOM_1997 == 95)
to_drop3 <- which(dat$CV_HGC_RES_DAD_1997 == 95)
to_drop4 <- which(dat$CV_HGC_RES_MOM_1997 == 95)
dat$CV_HGC_BIO_DAD_1997[to_drop1] = NA
dat$CV_HGC_BIO_MOM_1997[to_drop2] = NA
dat$CV_HGC_RES_DAD_1997[to_drop3] = NA
dat$CV_HGC_RES_MOM_1997[to_drop4] = NA
dat <- dat %>% mutate(average_grade_parent = rowSums(dat[, 8:11], na.rm = TRUE) / 4)

# the final additional education variable created below, highest_degree_received, is pretty subjective. 
# I am not counting preschool or kindergarten as part of years of schooling since we have no 
# information on them. Additionally, I am assuming that the other degree programs are completed in 
# the commonly known amount. For example, bachelor's is 4 years, master's is 2 years, and 
# PhD/professional degree is 5 years here. I also assume that the individual went straight to the 
# program from which they received their highest degree (for example, if they completed PhD, I am 
# assuming they went straight from a four-year bachelor's to PhD program, with no master's in-between).

dat <- dat %>% mutate(highest_degree_received = case_when(dat$YSCH.3113_2019 == 1 ~ "12",
                                                          dat$YSCH.3113_2019 == 2 ~ "12",
                                                          dat$YSCH.3113_2019 == 3 ~ "12",
                                                          dat$YSCH.3113_2019 == 4 ~ "14",
                                                          dat$YSCH.3113_2019 == 5 ~ "16",
                                                          dat$YSCH.3113_2019 == 6 ~ "18",
                                                          dat$YSCH.3113_2019 == 7 ~ "21",
                                                          dat$YSCH.3113_2019 == 8 ~ "21"))

# c) Provide the following visualizations.

# For the following questions, I am interpreting "number of children" to mean number of biological 
# children under 18 living in the household at the time of the survey. 

# i) Plot the income data (where income is positive) by i) age groups, ii) gender groups, and iii) number of children groups.

# For the age and number of children groups, I calculate average incomes for each group and then plot that. For the gender groups, since
# there are only two options here, I simply made histograms to show the income distribution for each gender.

dat_income_filtered <- dat %>% filter(dat$YINC_1700_2019 != 0 & dat$YINC_1700_2019 != 'NA')
dat_filtered_age_group <- dat_income_filtered %>% group_by(age_final) %>%
  summarize_at(vars(YINC_1700_2019), list(average_income = mean))
ggplot(dat_filtered_age_group, aes(x = age_final, y = average_income)) + geom_point() + labs(x = "Age", y = "Average Income") 

dat_filtered_gender_male <- dat_income_filtered %>% filter(dat_income_filtered$KEY_SEX_1997 != 0 & dat_income_filtered$KEY_SEX_1997 != 2)
dat_filtered_gender_female <- dat_income_filtered %>% filter(dat_income_filtered$KEY_SEX_1997 != 0 & dat_income_filtered$KEY_SEX_1997 != 1)
hist(dat_filtered_gender_male$YINC_1700_2019, main = "Income - Male")
hist(dat_filtered_gender_female$YINC_1700_2019, main = "Income - Female")

dat_filtered_numchildren_group <- dat_income_filtered %>% filter(dat_income_filtered$CV_BIO_CHILD_HH_U18_2019 != 'NA') %>% 
  group_by(CV_BIO_CHILD_HH_U18_2019) %>% summarize_at(vars(YINC_1700_2019), list(average_income = mean))
ggplot(dat_filtered_numchildren_group, aes(x = CV_BIO_CHILD_HH_U18_2019, y = average_income)) + geom_point() + labs(x = "Number of Children", y = "Average Income") 

# ii) Table the share of "0" in the income data by i) age groups, ii) gender groups, iii) number of
# children and marital status.

dat$YINC_1700_2019[is.na(dat$YINC_1700_2019)] <- 0
dat_age_group <- dat %>% group_by(age_final) %>% summarise(N = n(), num_zeros = length(which(YINC_1700_2019 == 0))) %>% 
  mutate(share_zeros = num_zeros / N)
dat_gender_group <- dat %>% group_by(KEY_SEX_1997) %>% 
  summarise(N = n(), num_zeros = length(which(YINC_1700_2019 == 0))) %>% 
  mutate(share_zeros = num_zeros / N)
dat_numchildrenmarital_group <- dat %>% group_by(CV_BIO_CHILD_HH_U18_2019, CV_MARSTAT_COLLAPSED_2019) %>% 
  summarise(N = n(), num_zeros = length(which(YINC_1700_2019 == 0))) %>% mutate(share_zeros = num_zeros / N) %>%
  filter(CV_BIO_CHILD_HH_U18_2019 != 'NA' & CV_MARSTAT_COLLAPSED_2019 != 'NA')

# iii) Interpret the visualizations from above.

# Return to this later.

#=========================================================================
# Exercise 2: Heckman Selection Model
#=========================================================================

# Using the variables created above, estimate the following models.

# a) Specify and estimate an OLS model to explain the income variable (where income is positive).

reg1 <- lm(YINC_1700_2019 ~ age_final + work_exp + average_grade_parent + 
             highest_degree_received + KEY_SEX_1997 + CV_BIO_CHILD_HH_U18_2019 + 
             CV_MARSTAT_COLLAPSED_2019, data = dat_income_filtered)
summary(reg1)

# i) Interpret the estimation results.

# Return to this later.

# ii) Explain why there might be a selection problem when estimating an OLS this way.

# There are many 0's and NA's in the data, so OLS estimates might be biased.
# Return to this later.

# b) Explain why the Heckman model can deal with the selection problem.

# Return to this later.

# c) Estimate a Heckman selection model (Please write down the likelihood and optimize the two-stage Heckman model).
# Interpret the results from the Heckman selection model and compare the results to OLS results. Why does there
# exist a difference?

dat <- dat %>% mutate(intercept = 1, income_exists = 0)
dat$income_exists[which(dat$YINC_1700_2019 > 0)] <- 1

# Our Probit Function

flikelihood <- function(par, intercept, x1, x2, x3, x4, x5, x6, x7, income_exists) {
  yhat <- par[1] * intercept + par[2] * x1 + par[3] * x2 + par[4] * x3 + par[5] * x4 +
    par[6] * x5 + par[7] * x6 + par[8] * x7
  prob <- pnorm(yhat)
  prob[prob > 0.999999] <- 0.999999
  prob[prob < 0.000001] <- 0.000001
  like <- income_exists * log(prob) + (1 - income_exists) * log(1 - prob)
  return(-sum(like))
}

predictor <- function(par, intercept, x1, x2, x3, x4, x5, x6, x7) {
  yhat <- par[1] * intercept + par[2] * x1 + par[3] * x2 + par[4] * x3 + par[5] * x4 +
    par[6] * x5 + par[7] * x6 + par[8] * x7 
  return(yhat)
}

intercept <- dat$intercept
x1 <- dat$age_final
x2 <- dat$work_exp
x3 <- dat$average_grade_parent
x4 <- as.numeric(dat$highest_degree_received)
x5 <- dat$KEY_SEX_1997
x6 <- dat$CV_BIO_CHILD_HH_U18_2019
x7 <- dat$CV_MARSTAT_COLLAPSED_2019
income_exists = dat$income_exists

start <- runif(6, -1, 1)
result <- optim(start, fn = flikelihood, method = "BFGS", control = list(trace = 6, REPORT = 1, maxit = 1000),
                 intercept = intercept, x1 = x1, x2 = x2, x3 = x3, x4 = x4, x5 = x5, x6 = x6, x7 = x7, income_exists = income_exists, hessian = TRUE)
result$par

# Use Probit package to check result

reg2 <- glm(income_exists ~ x1 + x2 + x3 + x3 + x4 + x5 + x6 + x7, family = binomial(link = "probit"), data = dat)
summary(reg2)
reg2$coefficients

predictor <- predictor(result$par, intercept, x1, x2, x3, x4, x5, x6, x7)
invM_ratio <- dnorm(predictor) / pnorm(predictor) # Inverse Mills Ratio
reg3 <- lm(dat$YINC_1700_2019 ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + invM_ratio) # Heckman regression
summary(reg3)
reg3$coefficients

#=========================================================================
# Exercise 3: Censoring
#=========================================================================

# Note that the "YINC-1700" variable is censored because of privacy issues. In other 
# words, high wages are top-coded in this data set.

# a) Plot a histogram to check whether the distribution of the income variable is
# affected. What might be the censored value here?

hist(dat_income_filtered$YINC_1700_2019, main = "Income")

# From checking the data set, we can see that the highest possible value is $100,000
# for income. So the top-coded/censored value is simply $100,000.

# b) Propose a model to deal with the censoring problem.

# Tobit model. Return to this later.

# c) Estimate the appropriate model with the censored data.

# We have a mass point at $100,000. The density will thus be 0 if income is greater
# than $100,000. The density will be the regular OLS density if income is less than
# $100,000. If income is equal to our mass point, the density will be 
# [1 - phi((xi * beta) / sigma)].

dat_ex3 <- dat %>% filter(dat$YINC_1700_2019 != 0 & dat$YINC_1700_2019 != 'NA') %>% 
  mutate(intercept = 1)
dat_ex3$censored <- 1
dat_ex3$censored[which(dat_ex3$YINC_1700_2019 < 100000)] <- 0

# Our Tobit function

flikelihood2 <- function(par, intercept, x1, x2, x3, x4, x5, x6, x7, censored, income) {
  yhat <- par[1] * intercept + par[2] * x1 + par[3] * x2 + par[4] * x3 + par[5] * x4 +
    par[6] * x5 + par[7] * x6 + par[8] * x7
  sigma <- exp(par[9])
  residual <- income - yhat
  our_normcdf <- pnorm(yhat / sigma)
  our_normpdf <- dnorm(residual / sigma)
  log_for_censored <- log(1 - our_normcdf)
  log_for_uncensored <- log(1 / sigma * our_normpdf)
  like <- censored * log_for_censored + (1 - censored) * log_for_uncensored
  return(-sum(like))
}

intercept <- dat_ex3$intercept
x1 <- dat_ex3$age_final
x2 <- dat_ex3$work_exp
x3 <- dat_ex3$average_grade_parent
x4 <- as.numeric(dat_ex3$highest_degree_received)
x5 <- dat_ex3$KEY_SEX_1997
x6 <- dat_ex3$CV_BIO_CHILD_HH_U18_2019
x7 <- dat_ex3$CV_MARSTAT_COLLAPSED_2019
censored <- dat_ex3$censored
income <- dat_ex3$YINC_1700_2019

start2 <- runif(7, -10, 10)
result2 <- optim(start2, fn = flikelihood2, method = "BFGS", control = list(trace = 6, REPORT = 1, maxit = 1000),
                 intercept = intercept, x1 = x1, x2 = x2, x3 = x3, x4 = x4, x5 = x5, x6 = x6, x7 = x7,
                 censored = censored, income = income, hessian = TRUE)
result2$par

# Use Tobit package to check result

reg4 <- vglm(YINC_1700_2019 ~ age_final + work_exp + average_grade_parent + 
               highest_degree_received + KEY_SEX_1997 + CV_BIO_CHILD_HH_U18_2019 + 
               CV_MARSTAT_COLLAPSED_2019, left = 0, right = 100000, data = dat_ex3)
summary(reg4)
reg4$coefficients

# d) Interpret the results above and compare to those when not correcting for 
# the censored data.

# Return to this later.

#=========================================================================
# Exercise 4: Panel Data
#=========================================================================

# In the second part, we use the panel dimension of NLSY97 data.

# We are interested in the effect of education, marital status, experience,
# and education on wages.

# a) Explain the potential ability bias when trying to explain to understand the 
# determinants of wages.

# b) Exploit the panel dimension of the data to propose a model to correct for the
# ability bias. Estimate the model using the following strategy.

# i) Within estimator.

# ii) Between estimator.

# iii) Difference (any) Estimator.

# c) Interpret the results from each model and explain why different models yield 
# different parameter estimates.



