#=========================================================================
# Econ 613 Homework 3: Data and Multinomial Choices
# Hannah Marsho
#=========================================================================

library(tidyverse)
library(readr)
library(ggplot2)
library(mlogit)

library(snow)
library(nloptr)
library(boot)
library(data.table)
library(nnet)

setwd("~/Desktop/econ613_wd/Data")
datstu <- read.csv('datstu_v2.csv')
datjss <- read.csv('datjss.csv')
datsss <- read.csv('datsss.csv')
datstu <- datstu %>% filter(rankplace != 'NA' & score != 'NA')
datsss <- datsss %>% distinct(schoolcode, .keep_all = TRUE)

#=========================================================================
# Exercise 1: Basic Statistics
#=========================================================================

# Calculate the following statistics.

# a) Number of students, schools, programs.

num_students <- nrow(datstu) # 160935 students
unique_schools <- unique(datsss$schoolcode)
num_schools <- length(unique_schools) # 898 schools
tot_programs <- c(datstu$choicepgm1, datstu$choicepgm2, datstu$choicepgm3,
                   datstu$choicepgm4, datstu$choicepgm5, datstu$choicepgm6)
unique_programs <- unique(tot_programs)
num_programs <- length(unique_programs) # 32 programs

# b) Number of choices (school, program)

num_choices <- matrix(nrow = num_schools, ncol = num_programs)
for (each_col in 1:num_programs){
  for (each_row in 1:num_schools){
    num_choices[each_row, each_col] = paste(unique_programs[each_col], unique_schools[each_row], sep = ';')
  }          
}
colnames(num_choices) <- unique_programs
rownames(num_choices) <- unique_schools
unique_num_choices <- length(unique(num_choices)) #29634

# c) Number of students applying to at least one senior high school in the same district as their home.

schoolcode_sssdistrict <- data.frame(datsss$schoolcode, datsss$sssdistrict) %>% distinct()
colnames(schoolcode_sssdistrict) <- c("schoolcode", "sssdistrict")
schoolcode_jssdistrict <- data.frame(datstu$schoolcode1, datstu$schoolcode2, datstu$schoolcode3, datstu$schoolcode4,
                                     datstu$schoolcode5, datstu$schoolcode6, datstu$jssdistrict) %>% mutate(unique_id = 1:n()) %>% pivot_longer(cols = datstu.schoolcode1:datstu.schoolcode6, values_to = "schoolcode")
code_district_merged <- merge(x = schoolcode_jssdistrict, y = schoolcode_sssdistrict, by = "schoolcode") %>% mutate(same_district = (code_district_merged$datstu.jssdistrict == code_district_merged$sssdistrict)) %>% group_by(code_district_merged$unique_id) %>% mutate(atleast1 = sum(same_district))
merged_simplifed <- data.frame(code_district_merged$unique_id, code_district_merged$atleast1)
merged_simplifed <- unique(merged_simplifed)
num_atleast1 <- sum(merged_simplifed$code_district_merged.atleast1 != 0) # 265464 students

# d) Number of students each senior high school admitted.

admittance_totals <- data.frame(unique_schools) %>% mutate(admittance = 0)
for (i in 1:num_students) {
  rankplace <- datstu$rankplace[i]
  all_codes <- c(datstu$schoolcode1[i], datstu$schoolcode2[i], datstu$schoolcode3[i],
         datstu$schoolcode4[i], datstu$schoolcode5[i], datstu$schoolcode6[i])
  if (rankplace != 99 && is.na(rankplace) == FALSE) {
    admitted_school_code <- all_codes[rankplace]
    row <- which(admittance_totals$unique_schools == admitted_school_code)
    total <- admittance_totals[row, 2] 
    admittance_totals[row, 2] <- total + 1
  }
}

# the number of students admitted to each unique school code are available in admittance_totals

# e) The cutoff of senior high schools (the lowest score to be admitted).

lowest_scores <- data.frame(unique_schools) %>% mutate(lowest_score = 9999)
for (i in 1:num_students) {
  rankplace <- datstu$rankplace[i]
  all_codes <- c(datstu$schoolcode1[i], datstu$schoolcode2[i], datstu$schoolcode3[i],
                 datstu$schoolcode4[i], datstu$schoolcode5[i], datstu$schoolcode6[i])
  if (rankplace != 99 && is.na(rankplace) == FALSE) {
    admitted_school_code <- all_codes[rankplace]
    row <- which(lowest_scores$unique_schools == admitted_school_code)
    if (lowest_scores[row, 2] > datstu$score[i]) {
      lowest_scores[row, 2] <- datstu$score[i]  
    }
  }
}

# the cutoff scores for each unique school code are available in lowest_scores


# f) The quality of senior high schools (the average score of students admitted)

average_scores <- data.frame(unique_schools) %>% mutate(total_score = 0)
for (i in 1:num_students) {
  rankplace <- datstu$rankplace[i]
  all_codes <- c(datstu$schoolcode1[i], datstu$schoolcode2[i], datstu$schoolcode3[i],
                 datstu$schoolcode4[i], datstu$schoolcode5[i], datstu$schoolcode6[i])
  if (rankplace != 99 && is.na(rankplace) == FALSE) {
    admitted_school_code <- all_codes[rankplace]
    row <- which(average_scores$unique_schools == admitted_school_code)
    average_scores[row, 2] <- average_scores[row, 2] + datstu$score[i]
  }
}
average_scores <- average_scores %>% mutate(average_score = average_scores$total_score / admittance_totals$admittance)

# the average scores (our measure of quality) for each unique school code are available in average_scores

#=========================================================================
# Exercise 2: Data
#=========================================================================

# Create a school level dataset, where each row corresponds to a (school, program) with the following variables:
# the district where the school is located, the latitude and longitude of the district, cutoff (the lowest score to be admitted),
# quality (the average score of the students admitted), and size (number of students admitted).

school_level <- c(t(num_choices))
district <- rep(datsss$sssdistrict, each = 32)
sss_latitude <- rep(datsss$ssslat, each = 32)
sss_longitude <- rep(datsss$ssslong, each = 32)
cutoff <- rep(lowest_scores[, 2], each = 32)
quality <- rep(average_scores[, 3], each = 32)
size <- rep(admittance_totals[, 2], each = 32)
school_level <- data.frame(school_level, district, sss_latitude, sss_longitude, cutoff, quality, size)

#=========================================================================
# Exercise 3: Distance
#=========================================================================

# Using the formula provided, calculate the distance between junior high school and senior high school.
# You should generate a value of distance for each of students' choices.

jss_info <- merge(datstu, datjss, by = "jssdistrict") 
sss_longitudes <- matrix(nrow = num_students, ncol = 6)
sss_latitudes <- matrix(nrow = num_students, ncol = 6)
for (i in 1:num_students) {
  all_codes <- c(jss_info$schoolcode1[i], jss_info$schoolcode2[i], jss_info$schoolcode3[i],
                 jss_info$schoolcode4[i], jss_info$schoolcode5[i], jss_info$schoolcode6[i])
  for (j in 1:6) {
    if (is.na(all_codes[j]) == FALSE) {
      row <- which(datsss$schoolcode == all_codes[j])
      sss_longitudes[i, j] <- datsss$ssslong[row]
      sss_latitudes[i, j] <- datsss$ssslat[row]
    }
  }
}

distances <- matrix(nrow = num_students, ncol = 6)
for (i in 1:num_students) {
  for (j in 1:6) {
    distances[i, j] <- sqrt((69.172 * (sss_longitudes[i, j] - jss_info$point_x[i]) * cos(jss_info$point_y[i] / 57.3))^2+(69.712 * (sss_latitudes[i, j] - jss_info$point_y[i]))^2)
  }
}

# distances between junior high schools and chosen senior high schools for each student are available in distances

#=========================================================================
# Exercise 4: Dimensionality Reduction
#=========================================================================

schoolcode_program_separate = strsplit(school_level$school_level, split = ";")
separated <- data.frame(matrix(unlist(schoolcode_program_separate), ncol = 2, byrow = T))

# a) Recode the schoolcode into its first three digits (substr). Call this new variable scode_rev.

school_level <- school_level %>% mutate(scode_rev = substr(separated$X2, 1, 3))

# b) Recode the program variable into 4 categories: arts (general arts and visual arts), economics (business and 
# home economics), science (general science), and others. Call this new variable pgm_rev.

school_level <- school_level %>% mutate(pgm_rev = case_when(separated$X1 == "General Arts" | separated$X1 == "Visual Arts" ~ "Arts",
                                                            separated$X1 == "Business" | separated$X1 == "Home Economics" ~ "Economics",
                                                            separated$X1 == "General Science" ~ "Science",
                                                            TRUE ~ "Others"))

# c) Create a new choice variable choice_rev.

school_level <- school_level %>% mutate(choice_rev = paste(scode_rev, pgm_rev, sep = ';'))

# d) Recalculate the cutoff and the quality for each recoded choice.

school_level <- school_level %>% group_by(scode_rev) %>% mutate(cutoff_rev = min(cutoff))
school_level <- school_level %>% group_by(scode_rev) %>% mutate(quality_rev = mean(quality, na.rm = TRUE))

# e) Consider the 20,000 highest score students.

top_students  <- datstu %>% arrange(desc(datstu$score)) %>% slice_head(n = 20000)

#=========================================================================
# Exercise 5: First Model
#=========================================================================

# Using the new data with recoded choices, we want to understand the effect of the student test score on his first choice.

top_students <- top_students %>% mutate(schoolcode_firstchoice = substr(top_students$schoolcode1, 1, 3))
top_students <- top_students %>% mutate(program_firstchoice = case_when(top_students$choicepgm1 == "General Arts" | top_students$choicepgm1 == "Visual Arts" ~ "Arts",
                                                             top_students$choicepgm1 == "Business" | top_students$choicepgm1 == "Home Economics" ~ "Economics",
                                                             top_students$choicepgm1 == "General Science" ~ "Science",
                                                             TRUE ~ "Others"))
top_students <- top_students %>% mutate(choice_rev = paste(schoolcode_firstchoice, program_firstchoice, sep = ';'))
school_level_simplifed <- school_level %>% select(scode_rev, choice_rev, cutoff_rev, quality_rev)
top_students <- top_students %>% left_join(school_level_simplifed, by = "choice_rev") %>% filter(quality != 'NA')

set.seed(123)
x <- sample(1:nrow(top_students), 500)
our_sample <- top_students[x, ]
our_sample$choice_rev <- as.factor(our_sample$choice_rev)
our_sample$choice_rev <- as.numeric(our_sample$choice_rev)
n <- length(unique(our_sample$choice_rev))

# a) Propose a model specification. Write the Likelihood function.

# Since the test score is different for each student, I recommend using a multinomial logit model.

multilogit_like_fun = function(param, data) {
  score <- data$score
  choice <- data$choice_rev
  ni <- nrow(data)
  nj <- length(unique(choice))
  nj_par <- nj - 1
  out <- mat.or.vec(ni, nj)
  
  pn1 <- param[1:nj_par]
  pn2 <- param[(nj):(2*nj_par)]
  out[, 1] <- rep(0, ni)
  
  for (j in 1:nj) {
    out[, j] <- pn1[j] + score * pn2[j]
  }
  prob <- exp(out)       
  prob <- sweep(prob, MARGIN = 1, FUN = "/", STATS = rowSums(prob))
  
  prob_c <- NULL
  for (i in 1:ni) {
    prob_c[i] = prob[i, choice[i]]
  }
  prob_c[prob_c > 0.999999] <- 0.999999
  prob_c[prob_c < 0.000001] <- 0.000001
  like <- sum(log(prob_c))
  return(-like)
}

# b) Estimate parameters and compute marginal effect of the proposed model.

# Use computer to find true model

computers_multinom <- multinom(choice_rev ~ score, data = our_sample, maxit = 5000)
computers_summary <- summary(computers_multinom)
computers_summary$coefficients

# Optimize manually

set.seed(123)
options(scipen = 200)
start <- as.vector(computers_summary$coefficients) + runif((n - 1) * 2, -0.02, 0.02)
res1 <- optim(start, fn = multilogit_like_fun, method = "BFGS", control = list(trace = 6, REPORT = 1, maxit = 5000), data = our_sample, hessian = TRUE)
par_m <- res1$par

# Marginal Effect
# People-Choice Matrix

multilogit_prob_matrix = function(param,data) {
  score <- data$score
  choice <- data$choice_rev
  ni <- nrow(data)
  nj <- length(unique(choice))
  nj_par <- nj - 1
  out <- mat.or.vec(ni, nj)
  
  pn1 <- param[1:nj_par]
  pn2 <- param[(nj):(2 * nj_par)]
  out[, 1] <- rep(0, ni)
  for (j in 1:nj) {
    out[, j] <- pn1[j] + score * pn2[j]
  }
  prob_sums <- apply(exp(out), 1, sum)      
  prob <- exp(out)/prob_sum
  return(prob)
}

probij_matrix <- multilogit_prob_matrix(par_m, our_sample)
mb <- c(0, par_m[n:(2 * (n-1))])

multilogit_margeff <- matrix(0, nrow = 500, ncol = n)
for (i in 1:500) {
  beta_bar <- sum(probij_matrix[i, ] * mb)
  multilogit_margeff[i, ] = probij_matrix[i, ] * (mb - beta_bar)
}
multilogit_margeff <- apply(multilogit_margeff, 2, mean)
multilogit_margeff <- as.data.frame(multilogit_margeff)
colnames(multilogit_margeff) <-'Marginal Effect'

#=========================================================================
# Exercise 6: Second Model
#=========================================================================

# Using the new data with recoded choices, we want to understand the effect of the student test score on his first choice.

# a) Propose a model specification. Write the Likelihood function.

# Since the school quality is the same for each student, I recommend using a conditional logit model.

condlogit_like_fun = function(param, data) {
  quality <- data$quality_rev
  choice <- data$choice_rev
  ni <- nrow(data)
  nj <- length(unique(choice))
  nj_par <- nj - 1
  out <- mat.or.vec(ni, nj)
  
  unique_data <- distinct(data, choice_rev, .keep_all = TRUE)
  unique_choice_quality <- unique_data[order(unique_data$choice_rev), ]$quality_rev
  
  pn1 <- param[1:nj_par]
  out[, 1] <- rep(0, ni)
  
  for (j in 1:nj) {
    out[, j] <- pn1[j] + param[nj] * unique_choice_quality[j] 
  }
  prob_sum <- exp(out)
  prob <- sweep(prob, MARGIN = 1, FUN = "/", STATS = rowSums(prob))
  prob_c <- NULL
  for (i in 1:ni) {
    prob_c[i] <- prob[i, choice[i]]
  }
  prob_c[prob_c > 0.999999] <- 0.999999
  prob_c[prob_c < 0.000001] <- 0.000001
  like <- sum(log(prob_c))
  return(-like)
}

# b) Estimate parameters and compute marginal effect of the proposed model.

# Optimize manually

set.seed(123)
options(scipen = 200)
params <- runif(n)
res2 <- optim(params, fn = condlogit_like_fun, method = "BFGS", control = list(trace = 6, REPORT = 1, maxit = 5000), data = our_sample, hessian = TRUE)
par_m2 <- res2$par

# Marginal Effect
# People-Choice Matrix

condlogit_prob_matrix = function(param, data) {
  quality <- data$quality
  choice <- data$choice_rev
  ni <- nrow(data)
  nj <- length(unique(choice))
  nj_par <- nj - 1
  out <- mat.or.vec(ni, nj)
  
  unique_data <- distinct(data, choice_rev, .keep_all = TRUE)
  unique_choice_quality <- unique_data[order(unique_data$choice_rev), ]$quality_rev
  
  pn1 <- param[1:nj_par]
  out[, 1] <- rep(0, ni)
  
  for (j in 1:nj) {
    out[, j] <- pn1[j] + param[nj] * unique_choice_quality[j]
  }
  prob_sums <- apply(exp(out), 1, sum)      
  prob <- exp(out)/prob_sum
  return(prob)
}

condprobij_matrix <- condlogit_prob_matrix(par_m2, our_sample)
mb2 <- c(0, par_m[n:(2 * (n-1))])

condlogit_margeff <- matrix(0, nrow = 500, ncol = n)
for (i in 1:500) {
  for (j in 1:500) {
    for (k in 1:n) {
      condlogit_margeff[i, j, k] = condprobij_matrix[i, j] * (1 - condprobij_matrix[i, k] * par_m2[n])
    }
  }
}
condlogit_margeff <- apply(condlogit_margeff, 2:3, mean)
condlogit_margeff <- as.data.frame(condlogit_margeff)
colnames(condlogit_margeff) <-'Marginal Effect'


#=========================================================================
# Exercise 7: Counterfactual Simulations
#=========================================================================

# In this exercise, we are interested in the effect of excluding choices where the program is "Others".

# a) Explain and justify which model you think is appropriate to conduct this exercise.



# b) Calculate choice probabilities under the appropriate model.

# c) Simulate how these choice probabilities change when these choices are excluded.
