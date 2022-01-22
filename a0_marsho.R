# Exercise 1 - Introduction

#2) Install packages

install.packages("Hmisc")
install.packages("gdata")
install.packages("boot")
install.packages("MASS")
install.packages("moments")
install.packages("snow")
install.packages("mvtnorm")

#3) Set working directory

setwd("/Users/hannahmarsho/Desktop/econ613_wd")

#4) List directory and environment content

dir()
ls()

#5) Check whether 678 is a multiple of 9

678%%9==0 # this operator is called modulo

#6) Save your environment

save.image(file = "myenvironment.RData")

#7) Find help on the function mean, cut2

help(mean)
??cut2 # couldn't use help function. console recommended '??cut2'

#8) Find an operation that returns NaN

0/0

# Exercise 2 - Object Manipulation

Titanic
dimnames(Titanic) # this tells us what dimensions the data set has data on

#1) Answer questions below

#a) Total population

sum(Titanic)

#b) Total adults

sum(Titanic[,,"Adult",])

#c) Total crew

sum(Titanic["Crew",,,])

#d) 3rd class children

sum(Titanic["3rd",,"Child",])

#e) 2nd class adult female

sum(Titanic["2nd","Female","Adult",])

#f) 1st class children male

sum(Titanic["1st","Male","Child",])

#g) female crew survivor

sum(Titanic["Crew","Female",,"Yes"])

#h) 1st class adult male survivor

sum(Titanic["1st","Male","Adult","Yes"])

#2) Find below proportions

#a) the proportion of survivors among first class, male, adult

prop.table(Titanic["1st","Male","Adult",])

#b) the proportion of survivors among first class, female, adult

prop.table(Titanic["1st","Female","Adult",])

#c) the proportion of survivors among first class, male, children

prop.table(Titanic["1st","Male","Child",])

#d) the proportion of survivors among third class, female, adult

prop.table(Titanic["3rd","Female","Adult",])

# Exercise 3 - Vectors - Introduction

#1) Use three ways to create these vectors

#a) a = 1,2,...,50

seq(1,50, by=1)
1:50
rev(50:1)

#b) b = 50, 49,...,1

rev(seq(1,50, by=1))
rev(1:50)
rev(rev(50:1))

#2) Create the vectors

#a) a = 10,19,7,10,19,7,...,10,19,7 with 15 occurences

rep(c(10,19,7),15)

#b) b = 1,2,5,6,...,1,2,5,6 with 8 occurences

rep(c(1,2,5,6),8)

#3) Create a vector of the values of logxsinx at x=3.1,3.2,...,,6

x = seq(3.1,6,by=0.1)
log(x)*sin(x)

#4) Using the function sample, draw 90 values between (0,100) and calculate the mean. Then, re-do allowing for replacement.

x = 0:100
mean(sample(x,90, replace = FALSE))
mean(sample(x,90, replace = TRUE))

#5) Calculate

#a)

a = 1:20
b = t(c(1:15))
sum(exp(sqrt(a))*log(a^5)/(5+cos(a)%*%sin(b)))

#b)

vec5b = 0
for (i in a) {
  b = t(c(1:i))
  vec5b = vec5b + sum(exp(sqrt(a))*log(a^5)/(5+exp(a%*%b)*cos(a)%*%sin(b)))
}

#6) Create a vector of the values of exp(x)cos(x) at x=3,3.1,...6

x = seq(3,6,by=0.1)
exp(x)*cos(x)

# Exercise 4 - Vectors - Advanced

#1) Create two vectors xVec and yVec by sampling 1000 values between 0 and 999

x = 0:999
xVec = sample(x,1000, replace = TRUE)
yVec = sample(x,1000, replace = TRUE)

#2) Suppose xVec = (x1,...,xn) and yVec = (y1,...,yn)

#a) Create the vector (y2-x1,...,yn-xn-1) denoted by zVec

zVec = yVec[-1] - xVec[-length(xVec)]

#b) Create the vector (sin(y1)/cos(x2), sin(y2)/cos(x3),..., sin(yn-1)/cos(xn)) denoted by wVec

wVec = sin(yVec[-length(yVec)])/cos(xVec[-1])

#c) Create a vector subX which consists of the values of xVec which are >=200

subX = xVec[xVec >= 200]

#d) What are the index positions in yVec of the values which are >=600

which(yVec >= 600)

# Exercise 5 - Matrix

#1) Create the matrix

A = matrix(c(1,1,3,5,2,6,-2,-1,-3), nrow = 3, ncol = 3, byrow = TRUE)

#a) Check that A^3=0 (matrix 0)

A^3==0

#b) Bind a fourth column as the sum of the first and third column

cbind(A,A[,1]+A[,3])

#c) Replace the third row by the sum of the first and second rows

A[3,] = A[1,] + A[2,]

#d) Calculate the average by row and column

rowMeans(A)
colMeans(A)

#2) Consider this system of linear equations

B = matrix(c(2,1,3,10,1,1,1,6,1,3,2,13), nrow = 3, ncol = 4, byrow = TRUE)

#3) Solve this equation

solve(B[1:3,1:3],B[,4])

# Exercise 6 - Functions

#1) Write a function fun1 which takes two arguments (a,n) where (a) is a scalar and n is a positive integer and returns a+a^2/2+a^3/3+...+a^n/n

fun1 = function(a,n) {
  vecn = 1:n
  output = sum((a^vecn)/vecn)
  return(output)
}

#2) Consider the function and evaluate at -3, 0, and 3

fun2 = function(x) {
  if (x<0) {
    output = x^2+2*x+abs(x)
    return(output)
  } else if (0<=x & x<2) {
    output = x^2+3+log(1+x)
    return(output)
  } else {
    output = x^2+4*x-14
    return(output)
  }
}
fun2(-3)
fun2(0)
fun2(3)

# Exercise 7 - Indexes

#1) Sample 36 values between 1 and 20 and name it v1

v1 = sample(1:20,36,replace = TRUE)

#2) Use two different ways to create the subvector of elements that are not in the first position of the vector

v1[-1]
v1[2:length(v1)]

#3) Create a logical element, v2, which is true if v1>5. Can you convert this logical element into a dummy 1 (TRUE) and 0 (FALSE)

v2 = (v1>5)
as.integer(v2)

#4) Create a matrix m1 [6x6] which is filled by row using the vector v1

m1 = matrix(v1, nrow = 6 , ncol = 6 , byrow = TRUE)

#5) Create the following object

x = c(rnorm(10),NA,paste("d",1:16),NA,log(rnorm(10)))

#6) Test for the position of missing values and non-finite values. Return a subvector free of missing and non-finite values

is.na(x)
is.infinite(x)
condition = is.na(x) + is.infinite(x)
x[condition==0]

# Exercise 8 - Data Manipulation

#1) Load the library AER and the data set

install.packages("AER")
library(AER)
dat = data("GSOEP9402", package = "AER")
dat = GSOEP9402

#2) What type of object is it? Find the number of rows and columns. Provide variable names

typeof(dat)
nrow(dat)
ncol(dat)
colnames(dat)

#3) Evaluate and plot the average annual income by year

library(dplyr)
library(ggplot2)
dat_plot <- dat %>% group_by(year) %>% summarise(mean_income = mean(income))
ggplot(dat_plot, aes(x=year,y=mean_income)) + geom_line()

#4) Create an array that illustrates simultaneously the income differences (mean) by gender, school, and memployment

dat_plot2 <- dat %>% group_by(gender) %>% summarise(mean_income = mean(income))
dat_plot3 <- dat %>% group_by(school) %>% summarise(mean_income = mean(income))
dat_plot4 <- dat %>% group_by(memployment) %>% summarise(mean_income = mean(income))
incomes = c('male-female'=dat_plot2$mean_income[[1]]-dat_plot2$mean_income[[2]],
            'gymnasium-hauptschule'=dat_plot3$mean_income[[3]]-dat_plot3$mean_income[[1]],
            'realschule-hauptschule'=dat_plot3$mean_income[[2]]-dat_plot3$mean_income[[1]],
            'none-fulltime'=dat_plot4$mean_income[[3]]-dat_plot4$mean_income[[1]], 
            'none-parttime'= dat_plot4$mean_income[[2]]-dat_plot4$mean_income[[1]])

# Exercise 9 - First regression

#1) Load the dataset

data("CASchools", package = "AER")
dat1 = CASchools

#2) Using the function lm, run a regression of read on the following variables. Store as reg1

dat1$school = factor(dat1$school)
dat1$district = factor(dat1$district)
reg1 = lm(read ~  district+school+county+grades+students+teachers+calworks+lunch+computer+expenditure+income+english, dat1)
summary(reg1)

#3) Can you run a similar regression with the following line? Create reg2 that uses only the 200 first observations

formula = y ~ x.lm(formula)
reg2 = lm(read ~ district+school+county+grades+students+teachers+calworks+lunch+computer+expenditure+income+english, dat1[1:200,])
summary(reg2)

# Exercise 10 - Advanced Indexing

#1) Create a vector lu of 200 draws from a pareto distribution (1,1). How many values are higher than 10? Replace these values by draws from a logistic distribution (6.5,0.5)

install.packages("actuar")
library(actuar)
lu = rpareto(200,1,1)  
length(lu[lu>10])
lu[which(lu>10)] = rlogis(length(lu[lu>10]),6.5,0.5)

#2) Create a vector de of 200 draws from a normal distribution (1,2). Set de=log(de), and count the number of missing values or negative values.Replace these values by draws from a normal distribution (0,1) truncated at 0

install.packages("truncnorm")
library(truncnorm)
de = rnorm(200,1,2)
de = log(de)
length(de[is.na(de)])
length(de[de<0])
de[which(is.na(de))] = rtruncnorm(length(which(is.na(de))), a=0)
de[which(de<0)] = rtruncnorm(length(which(de<0)), a=0)

#3) Create two vectors, orig and dest as 200 draws from a uniform distribution [0,1]

orig = runif(200,0,1)
dest = runif(200,0,1)

#4) Create two matrices, hist and dist as 200*200 draws from a uniform distribution [0,1]

hist = matrix(runif(200*200,0,1), nrow = 200, ncol = 200)
dist = matrix(runif(200*200,0,1), nrow = 200, ncol = 200)

#5) Consider the function. #6) Create the matrices su and se

su = log(outer(orig, dest, "+") + dist) / (1 + log(outer(orig, dest, "+") + dist))
se = exp(outer(orig, dest, "+") + hist) / (1 + exp(outer(orig, dest, "+") + hist))

#7) Set r=0.05. Create a function to evaluate qjl(.). Evaluate qjl(9245) for all pairs (j,l)

r = 0.05
qjl = function(w) {
  outer1 = outer(r+de, r+de, "/") 
  first = outer1*w
  second = lu*log(w)
  third = lu*(1+log(w))
  fourth = outer1*sum(su)
  fifth = sum(su)
  sixth = outer1*sum(se)
  seventh = sum(se)
  output = first+second-third+fourth-fifth+sixth-seventh
  return(output)
}
qjl(9245)

#8) Create gridw, which consists of a sequence from 9100 to 55240 of length 50

install.packages("pracma")
library(pracma)
gridw = seq(9100,55240,50)

#9) Using the function sapply, evaluate qjl. Store the output into an array of dimension (50x200x200). How long does it take to evaluate qjl() for each value of w

eval <- array(sapply(gridw, qjl, simplify = FALSE), dim = c(50,200,200))
system.time(sapply(gridw, qjl, simplify = FALSE))

# Exercise 11 - Tests and Indexing

#1) Test if c(1,2,3) is an array? a vector? a matrix

x = c(1,2,3)
is.array(x)
is.vector(x)
is.matrix(x)

#2) x0 = rnorm(1000); Using the function table(), count the number of occurrences of x0>0, x0>1, x0>2, x0>0.5, x0<1, and x0>-1

x0 = rnorm(1000)
table(x0>0)[[2]]
table(x0>1)[[2]]
table(x0>2)[[2]]
table(x0>0.5)[[2]]
table(x0<1)[[2]]
table(x0>-1)[[2]]

#3) Run the following

install.packages("Hmisc")
library(Hmisc)
x1 = cut2(runif(100,0,1),g=10)
levels(x1)=paste("q",1:10,sep="")

#4) Test whether or not x1 is a factor

is.factor(x1)

#5) Verify that "q1" has 10 occurences

table(x1=="q1")[[2]] == 10 

#6) Convert x1 into a numeric variable. What happens to the levels

as.numeric(x1) # levels are integers now

#7) Run the following

rand = rnorm(1000)

#8) Using the function which(), find the indexes of positive values

which(rand>0)

#9) Create the object w of positive values of x using:

#a) Which

w1 = rand[which(rand>0)]

#b) Subset

w2 = subset(rand, rand>0)

#c) By indexing directly the values that respect a condition

w3 = rand[rand>0]

# Exercise 12 - Programming

u = function(N) {
  if (N==0|N==1) {return(1)}
  return(u(N-1)+u(N-2))
}

#1) Evaluate 1^2 + 2^2 + 3^2 + ... + 400^2

sum(c(1:400)^2)

#2) Evaluate 1*2 + 2*3 + 3*4 + ... + 249*250

sum(c(1:249) * c(2:250))

#3) Create a function "crra" with two arguments (c, theta) that returns (c^1-theta)/(1-theta). Add an if condition such that the utility is given by the log when theta is [0.97,1.03] = 1

crra = function(c,theta) {
  op = c^(1-theta)/(1-theta)
  if (0.97 <= theta & theta <=1.03) {return(log(op))}
  return(op)
}

#4) Create a function "fact" that returns the factorial of a number

fact = function(n) {
  if (n==0|n==1) {return(1)}
  return(prod(n)) 
}

# Exercise 13 - Apply Functions

#1) Using the object, calculate the mean, median, min, max, and standard deviation by row and column

m = matrix(c(rnorm(20,0,10),rnorm(20,-1,10)),nrow=20,ncol=2)
apply(m, MARGIN=1, FUN=mean)
apply(m, MARGIN=2, FUN=mean)
apply(m, MARGIN=1, FUN=min)
apply(m, MARGIN=2, FUN=min)
apply(m, MARGIN=1, FUN=max)
apply(m, MARGIN=2, FUN=max)
apply(m, MARGIN=1, FUN=sd)
apply(m, MARGIN=2, FUN=sd)

#2) Using the dataset iris in the package "datasets", calculate the average sepal.length by species. Evaluate the sum log of sepal.width by species.

library(datasets)
data(iris)
iris %>% group_by(Species) %>% summarise(mean_sepal_length = mean(Sepal.Length))
tibble = iris %>% group_by(Species) %>% summarise(mean_sepal_width = mean(Sepal.Width))
sum(log(tibble$mean_sepal_width))

#3) Run the following

y1 = NULL; for (i in 1:100) {y1[i]=exp(i)}
y2 = exp(1:100)
y3 = sapply(1:100,exp)

#a) Check the outcome of these three operations

# These three operations all have the same outcome

#b) Using proc.time() or system.time(), compare the execution time of these three equivalent commands

y1=NULL; system.time(for (i in 1:100) {y1[i]=exp(i)})
system.time(exp(1:100))
system.time(sapply(1:100,exp))

# The first command takes longer than the other two

# Exercise 14 - Simulating and Computing

#1) Simulate a vector x of 10,000 draws from a normal distribution. Use the function summary to provide basic characteristics of x

x = rnorm(10000)
summary(x)

#2) Create a function dsummary that returns the minimum, the 1st decile, the 1st quartile, the median, the mean, the standard deviation, the 3rd quartile, the 9th decile, and the maximum

dsummary = function(vec) { 
  min = summary(vec)[[1]]
  dec1 = quantile(vec,prob=c(0.1,0.9))[[1]]
  qt1 = summary(vec)[[2]]
  med = summary(vec)[[3]]
  mean = summary(vec)[[4]]
  qt3 = summary(vec)[[5]]
  dec9 = quantile(vec,prob=c(0.1,0.9))[[2]]
  max = summary(vec)[[6]]
  return(c(min,dec1,qt1,med,mean,qt3,dec9,max)) }

#3) Suppose X ~ N(2, 0.25). Evaluate f(0.5), F(2.5), F^-1(0.95)

dnorm(0.5, mean = 2, sd = 0.05)
pnorm(2.5, mean = 2, sd = 0.05)
qnorm(0.95,mean = 2,sd = 0.05)

#4) Repeat if X has t-distribution with 5 degrees of freedom

dt(0.5, df=5)
pt(2.5, df=5)
qt(0.95, df=5)

#5) Suppose X ~ P(3, 1), where P is the pareto distribution. Evaluate f(0.5), F(2.5), F^-1(0.95)

dpareto(0.5, 3, 1)
ppareto(2.5, 3, 1)
qpareto(0.95, 3, 1)

# Exercise 15 - Moments

# Consider a vector V = rnorm(100, -2, 5)

V = rnorm(100, -2, 5)

#1) Evaluate n as the length of V

n = length(V)

#2) Compute the mean

mean(V)

#3) Compute the variance

var(V)

#4) Compute the skewness

install.packages(moments)
library(moments)
skewness(V)

#5) Compute the kurtosis

kurtosis(V)

# Exercise 16 - OLS

#1) Create a matrix X of dimension (1000, 10). Fill it with draws from a beta distribution with shape 1 parameter 2 and shape 2 parameter 1. Make sure that there is no negative

X = matrix(NULL, nrow = 1000, ncol = 10)
dat = rbeta(1000*10, 2, 1)
X[1:1000, 1:10] = dat
length(X[X<0]) == 0

#2) Create a scalar denoted by sigma^2 and set it to 0.5. Generate a vector beta of size 10. Fill it with draws from a Gamma distribution with parameters 2 and 1

sigmasq = 0.5
beta = rgamma(10, 2, 1)

#3) Create a vector epsilon of 1000 draws from a normal distribution

eps = rnorm(1000)

#4) Create Y = X*Beta + sqrt(sigma^2)*epsilon

Y = X%*%beta + sqrt(sigmasq)*eps

#5) Recover Beta hat

solve(t(X)%*%X)%*%t(X)%*%Y

#6) Evaluate epsilon hat = y hat - y. Plot the histogram (filled in gray) and the kernel density of the distribution of the error term

Yhat = X %*% beta
epsHat = Yhat - Y
hist(epsHat, col="gray")
plot(density(epsHat))

#7) Estimate the following

s = t(epsHat) %*% epsHat / (1000-10-1)
v = s[1,1] * solve(t(X)%*%X)

#8) Create param that binds (beta, sqrt(V(beta hat))). Using the command lm, check these estimates

param = cbind(beta, sqrt(v)) 
fit0 = lm(Y ~ 0+X)
summary(fit0)

#9) Construct a confidence interval for beta

confint(fit0)

#10) Redo the exercise by setting sigma^2 = 0.01. How are your confidence intervals for beta

sigmasq = 0.01
Y = X %*% beta + sqrt(sigmasq)*eps
fit1 = lm(Y~0+X)
confint(fit1)

# the confidence intervals became tighter/more precise





