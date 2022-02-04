#=========================================================================
# Econ 613 Homework 2: OLS and Probit
# Hannah Marsho
#=========================================================================

library(tidyverse)
library(readr)
setwd("/Users/hannahmarsho/Desktop/econ613_wd/Data")
datind2009 <- read.csv('datind2009.csv')
datind2009 <- subset(datind2009, select = c("empstat", "age", "wage"))
datind2009_complete <- na.omit(datind2009) %>% filter(age > 0, wage != 0)

#=========================================================================
# Exercise 1: OLS Estimate
#=========================================================================

# a) Calculate the correlation between Y and X. 

cor <- cor(datind2009_complete$age, datind2009_complete$wage, use = "complete.obs")
cor # 0.14349

# b) Calculate the coefficients on this regression. 

X = cbind(1, datind2009_complete$age)
Y = datind2009_complete$wage
beta_hat = solve(t(X) %*% X) %*% t(X) %*% Y
beta_hat # 230.9923

# c) Calculate the standard errors of beta.

# i) Using the standard formulas of the OLS.

wage_hat = X %*% beta_hat
eps_hat = Y - wage_hat
sigma_sqrd_hat = t(eps_hat) %*% eps_hat / (nrow(X) - ncol(X))
var_cov_beta_hat = sigma_sqrd_hat %*% solve(t(X) %*% X)
std_err = sqrt(diag(var_cov_beta_hat))
std_err # 14.8774

reg <- lm(wage ~ age, data = datind2009_complete)
reg_sum <- summary(reg)
reg_sum # used lm function to check that values were correct

# ii) Using bootstrap with 49 and 499 replications respectively. Comment on the difference between the two strategies.

num_ind = nrow(datind2009_complete) # number of individuals in the data
num_var = length(reg$coefficients)  # number of variables in the data

reps = 49 # number of bootstraps for our 49 replication bootstrap

outputs <- mat.or.vec(reps, num_var)
set.seed(123)

for (i in 1:reps)
{
  our_sample = sample(1:num_ind, num_ind, rep = TRUE)
  sample_data = datind2009_complete[our_sample, ]
  reg <- lm(wage ~ age, data = datind2009_complete)
  outputs[i,] <- reg$coefficients
}

our_mean = apply(outputs, 2, mean)
our_sd = apply(outputs, 2, sd)

our_estimate = cbind(summary(reg)$coefficients[ , 1], summary(reg)$coefficients[ , 2], our_mean, our_sd)
colnames(our_estimate) = c("coefficient: estimate","coefficient: std dev","bootstrap (49): estimate","bootstrap (49): std dev")
our_estimate

reps2 = 499 # number of bootstraps for our 499 replication bootstrap

outputs2 <- mat.or.vec(reps2, num_var)
set.seed(123)

for (i in 1:reps2)
{
  our_sample2 = sample(1:num_ind, num_ind, rep = TRUE)
  sample_data2 = datind2009_complete[our_sample2, ]
  reg2 <- lm(wage ~ age, data = datind2009_complete)
  outputs2[i,] <- reg2$coefficients
}

our_mean2 = apply(outputs2, 2, mean)
our_sd2 = apply(outputs2, 2, sd)

our_estimate2 = cbind(summary(reg2)$coefficients[ , 1], summary(reg2)$coefficients[ , 2], our_mean2, our_sd2)
colnames(our_estimate2) = c("coefficient: estimate","coefficient: std dev","bootstrap (499): estimate","bootstrap (499): std dev")
our_estimate2

#=========================================================================
# Exercise 2: Detrend Data
#=========================================================================

datind = list.files(pattern = "datind")
for (i in 1:16) {
  assign(datind[i], read.csv(datind[i]))
}
datind_2005_to_2018 = rbind(datind2005.csv, datind2006.csv, datind2007.csv, datind2008.csv, datind2009.csv, datind2010.csv, datind2011.csv, datind2012.csv, datind2013.csv, 
                            datind2014.csv, datind2015.csv, datind2016.csv, datind2017.csv, datind2018.csv)
datind_2005_to_2018 <- subset(datind_2005_to_2018, select = c("year", "empstat", "age", "wage"))
datind_2005_to_2018_complete <- na.omit(datind_2005_to_2018) %>% filter(age > 0, wage != 0)

# a) Create a categorical variable ag, which bins the age variables into the specified groups.

ag <- data.frame(datind_2005_to_2018_complete, bin = cut(datind_2005_to_2018_complete$age, c(18, 25, 30, 35, 40, 45, 50, 55, 60, 100), include.lowest = TRUE))

# b) Plot the wage of each age group across years. Is there a trend?

ag_plot <- ag %>% group_by(year, bin) %>% summarise(mean_wage = mean(wage, na.rm = TRUE))
ggplot(data = ag_plot, mapping = aes(x = year, y = mean_wage, color = bin)) + geom_point()

# c) Consider Wageit = beta*Ageit + gammat*Yeari + eit. After including a time fixed effect, how do the estimated coefficients change?

reg3 <- lm(wage ~ age + year, data = ag)
reg3_sum <- summary(reg3)
reg3_sum

#=========================================================================
# Exercise 3: Numerical Optimization
#=========================================================================

datind2007 <- read.csv('datind2007.csv')
datind2007 <- subset(datind2007, select = c("empstat", "age", "wage"))
datind2007_complete <- na.omit(datind2007) %>% filter(age > 0, wage != 0)

# a) Exclude all individuals who are inactive.

datind2007_complete <- datind2007_complete %>% filter(empstat != "Inactive", empstat != "Retired")

# b) Write a function that returns the likelihood of the probit of being employed. 

datind2007_complete$empstat[which(datind2007_complete$empstat == "Employed")] = 1 
datind2007_complete$empstat[which(datind2007_complete$empstat == "Unemployed")] = 0 
datind2007_complete$empstat <- as.numeric(datind2007_complete$empstat)
datind2007_complete$age <- as.numeric(datind2007_complete$age)

flikelihood <- function(par, age, empstat) {
  x_beta = par[1] + par[2] * age
  prob = pnorm(x_beta)
  prob[prob > 0.999999] = 0.999999
  prob[prob < 0.000001] = 0.000001
  likelihood = empstat * log(prob) + (1 - empstat) * log(1 - prob)
  return(-sum(likelihood))
}

reg_probit <- glm(empstat ~ age, data = datind2007_complete, family = binomial(link = "probit"))
test_pars = reg_probit$coefficients
flikelihood(test_pars, datind2007_complete$age, datind2007_complete$empstat) # 2079.097
logLik(reg_probit) # tested that the values were correct

# c) Optimize the model and interpret the coefficients.

ntrys = 100
outputs3 <- mat.or.vec(ntrys, 3)
for (i in 1:ntrys) {
  start_point = runif(4, -10, 10)
  result = optim(start_point, fn = flikelihood, method = "BFGS", control = list(trace = 6, maxit = 3000), age = datind2007_complete$age, empstat = datind2007_complete$empstat)
  outputs3[i, ] = c(result$par, result$value)
}

outputs3 <- as.data.frame(outputs3)
outputs3[which(outputs3$V3 == min(outputs3$V3)), ]

# d) Can you estimate the same model including wages as a determinant of labor market participation? Explain.

# No.

#=========================================================================
# Exercise 4: Discrete Choice
#=========================================================================

datind = list.files(pattern = "datind")
for (i in 1:16) {
  assign(datind[i], read.csv(datind[i]))
}
datind_2005_to_2015 = rbind(datind2005.csv, datind2006.csv, datind2007.csv, datind2008.csv, datind2009.csv, datind2010.csv, datind2011.csv, datind2012.csv, datind2013.csv, 
                            datind2014.csv, datind2015.csv)
datind_2005_to_2015 <- subset(datind_2005_to_2015, select = c("year", "empstat", "age", "wage"))
datind_2005_to_2015_complete <- na.omit(datind_2005_to_2015) %>% filter(age > 0, wage != 0)

ag <- data.frame(datind_2005_to_2018_complete, bin = cut(datind_2005_to_2018_complete$age, c(18, 25, 30, 35, 40, 45, 50, 55, 60, 100), include.lowest = TRUE))

# a) Exclude all individuals who are inactive.

datind_2005_to_2015_complete <- datind_2005_to_2015_complete %>% filter(empstat != "Inactive", empstat != "Retired")

# b) Write and optimize the probit, logit, and the linear probability models

datind_2005_to_2015_complete$empstat[which(datind_2005_to_2015_complete$empstat == "Employed")] = 1 
datind_2005_to_2015_complete$empstat[which(datind_2005_to_2015_complete$empstat == "Unemployed")] = 0 
datind_2005_to_2015_complete$empstat <- as.numeric(datind_2005_to_2015_complete$empstat)
datind_2005_to_2015_complete$age <- as.numeric(datind_2005_to_2015_complete$age)

# Probit

flikelihood_probit2 <- function(par, age, year, empstat) {
  x_beta_probit2 = par[1] + par[2] * age + par[3] * year
  prob_probit2 = pnorm(x_beta_probit2)
  prob_probit2[prob_probit2 > 0.999999] = 0.999999
  prob_probit2[prob_probit2 < 0.000001] = 0.000001
  likelihood_probit2 = empstat * log(prob_probit2) + (1 - empstat) * log(1 - prob_probit2)
  return(-sum(likelihood_probit2))
}

reg_probit2 <- glm(empstat ~ age + year, data = datind_2005_to_2015_complete, family = binomial(link = "probit"))
test_pars_probit2 = reg_probit2$coefficients
flikelihood_probit2(test_pars_probit2, datind_2005_to_2015_complete$age, datind_2005_to_2015_complete$year, datind_2005_to_2015_complete$empstat) # 28369.53
logLik(reg_probit2) # tested that the values were correct

ntrys = 100
outputs4 <- mat.or.vec(ntrys, 5)
for (i in 1:ntrys) {
  start_point = runif(4, -10, 10)
  result = optim(start_point, fn = flikelihood_probit2, method = "BFGS", control = list(trace = 6, maxit = 3000), age = datind_2005_to_2015_complete$age, year = datind_2005_to_2015_complete$year, empstat = datind_2005_to_2015_complete$empstat)
  outputs4[i, ] = c(result$par, result$value)
}

outputs4 <- as.data.frame(outputs4)
outputs4[which(outputs4$V5 == min(outputs4$V5)), ]

# Logit

flikelihood_logit <- function(par, age, year, empstat) {
  x_beta_logit = par[1] + par[2] * age + par[3] * year
  prob_logit = exp(x_beta_logit) / (1 + exp(x_beta_logit))
  prob_logit[prob_logit > 0.999999] = 0.999999
  prob_logit[prob_logit < 0.000001] = 0.000001
  likelihood_logit = empstat * log(prob_logit) + (1 - empstat) * log(1 - prob_logit)
  return(-sum(likelihood_logit))
}

ntrys = 100
outputs5 <- mat.or.vec(ntrys, 5)
for (i in 1:ntrys)
{
  start_point = runif(4, -10, 10)
  result = optim(start_point, fn = flikelihood_logit, method = "BFGS", control = list(trace = 6, maxit = 1000), age = datind_2005_to_2015_complete$age, year = datind_2005_to_2015_complete$year, empstat = datind_2005_to_2015_complete$empstat)
  outputs5[i, ] = c(result$par, result$value)
}

outputs5 <- as.data.frame(outputs5)
outputs5[which(outputs5$V5 == min(outputs5$V5)), ]

# Linear Probability

flikelihood_linear <- function(par, age, year, empstat) {
  x_beta_linear = par[1] + par[2] * age + par[3] * year
  prob_linear = x_beta_linear
  prob_linear[prob_linear > 0.999999] = 0.999999
  prob_linear[prob_linear < 0.000001] = 0.000001
  likelihood_linear = empstat * log(prob_linear) + (1 - empstat) * log(1 - prob_linear)
  return(-sum(likelihood_linear))
}

ntrys = 100
outputs5 <- mat.or.vec(ntrys, 5)
for (i in 1:ntrys)
{
  start_point = runif(4, -10, 10)
  result = optim(start_point, fn = flikelihood_linear, method = "BFGS", control = list(trace = 6, maxit = 1000), age = datind_2005_to_2015_complete$age, year = datind_2005_to_2015_complete$year, empstat = datind_2005_to_2015_complete$empstat)
  outputs5[i, ] = c(result$par, result$value)
}

outputs5 <- as.data.frame(outputs5)
outputs5[which(outputs5$V5 == min(outputs5$V5)), ]

# d) Interpret and compare the estimated coefficients. How significant are they?


#=========================================================================
# Exercise 5: Marginal Effects
#=========================================================================

# a) Compute the marginal effect of the previous probit and logit models

# b) Construct the standard errors of the marginal effects. use bootstrap