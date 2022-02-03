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

X <- cbind(1, datind2009_complete$age)
Y <- datind2009_complete$wage
beta_hat <- solve(t(X) %*% X) %*% t(X) %*% Y
beta_hat # 230.9923

# c) Calculate the standard errors of beta.

# i) Using the standard formulas of the OLS.

wage_hat = X %*% beta_hat
eps_hat = Y - wage_hat
sigma_sqrd_hat <- t(eps_hat) %*% eps_hat / (nrow(X) - ncol(X))
var_cov_beta_hat <- c(sigma_sqrd_hat) * solve(t(X) %*% X)
std_err <- sqrt(diag(var_cov_beta_hat))
std_err # 14.8774

reg = lm(wage ~ age, data = datind2009_complete)
reg_sum = summary(reg)
reg_sum # used lm function to check that values were correct

# ii) Using bootstrap with 49 and 499 replications respectively. Comment on the difference between the two strategies.

reps = 49 # number of bootstraps
num_ind = nrow(datind2009_complete) # number of individuals
num_var = length(reg$coefficients)  # number of variables

outs = mat.or.vec(reps,nvar)
set.seed(123)

for (i in 1:reps)
{
  samp     = sample(1:nind,nind,rep=TRUE)
  dat_samp = CPS1985[samp,]
  reg1     = lm(log(wage) ~ gender*married + education + experience + I(experience^2),data = dat_samp)
  outs[i,] = reg1$coefficients
}

mean_est = apply(outs,2,mean)
sd_est   = apply(outs,2,sd)

est = cbind(summary(reg)$coefficients[,1],
            summary(reg)$coefficients[,2],
            mean_est,
            sd_est)
colnames(est) = c("CF: est","CF: sd","BT: est","BT: sd")
est

reps = 499 # number of bootstraps

for (i in 1:reps)
{
  samp     = sample(1:nind,nind,rep=TRUE)
  dat_samp = CPS1985[samp,]
  reg1     = lm(log(wage) ~ gender*married + education + experience + I(experience^2),data = dat_samp)
  outs[i,] = reg1$coefficients
}

mean_est = apply(outs,2,mean)
sd_est   = apply(outs,2,sd)

est = cbind(summary(reg)$coefficients[,1],
            summary(reg)$coefficients[,2],
            mean_est,
            sd_est)
colnames(est) = c("CF: est","CF: sd","BT: est","BT: sd")
est


