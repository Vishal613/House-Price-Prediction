# Lab: Cross-Validation and the Bootstrap
# Acknowledgement: ISLR2 textbook

library(ISLR2)
library(tidyverse)


## The Validation Set Approach

###
set.seed(1)
train <- sample(392, 196)

with(plot(horsepower, mpg), data = Auto)

###
lm.fit <- lm(mpg ~ horsepower, data = Auto, subset = train)
###
with(mean((mpg - predict(lm.fit, Auto))[-train]^2), data = Auto)
###
# polynomial fit with 2 degree polynomial
lm.fit2 <- lm(mpg ~ poly(horsepower, 2), data = Auto, 
              subset = train)
with(mean((mpg - predict(lm.fit2, Auto))[-train]^2), data = Auto)

# polynomial fit with 3 degree polynomial
lm.fit3 <- lm(mpg ~ poly(horsepower, 3), data = Auto, 
              subset = train)
with(mean((mpg - predict(lm.fit3, Auto))[-train]^2), data = Auto)
###
# try a different seed
set.seed(2)
train <- sample(392, 196)

with(plot(horsepower, mpg), data = Auto)

###
lm.fit <- lm(mpg ~ horsepower, data = Auto, subset = train)
###
with(mean((mpg - predict(lm.fit, Auto))[-train]^2), data = Auto)
###
# polynomial fit with 2 degree polynomial
lm.fit2 <- lm(mpg ~ poly(horsepower, 2), data = Auto, 
              subset = train)
with(mean((mpg - predict(lm.fit2, Auto))[-train]^2), data = Auto)

# polynomial fit with 3 degree polynomial
lm.fit3 <- lm(mpg ~ poly(horsepower, 3), data = Auto, 
              subset = train)
with(mean((mpg - predict(lm.fit3, Auto))[-train]^2), data = Auto)## Leave-One-Out Cross-Validation

###

# LOOCV
# family, by default is Gaussian i.e., linear regression
glm.fit <- glm(mpg ~ horsepower, data = Auto) 
coef(glm.fit)
###
lm.fit <- lm(mpg ~ horsepower, data = Auto)
coef(lm.fit)
###
library(boot)
glm.fit <- glm(mpg ~ horsepower, data = Auto)
# by default does LOOCV
cv.err <- cv.glm(Auto, glm.fit)
# two estimates of prediction error 
# -- first one is the raw error estimate we saw in class
# -- second one is an adjustment version (Davison and Hinkley, 1997)
cv.err$delta
###
# see the LOOCV for polynomial regression with degrees 1 to 10
cv.error <- rep(0, 10)
for (i in 1:10) {
  glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error[i] <- cv.glm(Auto, glm.fit)$delta[1]
}
cv.error
plot(1:10, cv.error, type = "b")



## $k = 10$-Fold Cross-Validation
###
set.seed(17)
cv.error.10 <- rep(0, 10)
for (i in 1:10) {
  glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error.10[i] <- cv.glm(Auto, glm.fit, K = 10)$delta[1]
}
cv.error.10

# functional programming

# base R
set.seed(17)
cv.error.10.b <- sapply(
  1:10,
  function(i) {
    glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
    cv.glm(Auto, glm.fit, K = 10)$delta[1]
  }
)
all.equal(cv.error.10, cv.error.10.b)


# if using tidyverse
set.seed(17)
cv.error.10.c <- map_dbl(
  1:10,
  function(i) {
    glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
    cv.glm(Auto, glm.fit, K = 10)$delta[1]
  }
)
all.equal(cv.error.10, cv.error.10.c)

## The Bootstrap


### Estimating the Accuracy of a Statistic of Interest


### Estimating the Accuracy of a Linear Regression Model

boot_coef_list <- list()
lm_fit <- lm(mpg ~ horsepower, data = Auto)
coef_lm_fit <- coef(lm_fit)
set.seed(1)
for (i in 1:1000) {
  rand_idx <- sample(nrow(Auto), nrow(Auto), replace = TRUE)
  boot_coef_list[[i]] <- lm(
    mpg ~ horsepower, 
    data = Auto, 
    subset = rand_idx
  ) %>% 
    coef()
  
}
boot_coef_df <- bind_rows(boot_coef_list)
boot_coef_df
apply(boot_coef_df, 2, summary)
apply(boot_coef_df, 2, sd)
hist(boot_coef_df$horsepower)
abline(v = coef_lm_fit[2], col = "red")

summary(lm_fit)$coef
summary(lm_fit)$coef[, 2]
apply(boot_coef_df, 2, sd)

###
# using package boot
# two input arguments - data, index
# output whatever vector of statistics/estimators you want to calculate
boot.fn <- function(data, index) {
  coef(lm(mpg ~ horsepower, data = data, subset = index))
  
}
boot.fn(Auto, 1:392)
###
set.seed(1)
boot.fn(Auto, sample(392, 392, replace = T))
boot.fn(Auto, sample(392, 392, replace = T))
###
boot(Auto, boot.fn, 1000)
###
summary(lm(mpg ~ horsepower, data = Auto))$coef

# using pipes
lm(lm(mpg ~ horsepower, data = Auto)) %>% 
  summary() %>% 
  .$coef

###
boot.fn <- function(data, index) {
  coef(
    lm(mpg ~ horsepower + I(horsepower^2), 
       data = data, subset = index)
  )
}

set.seed(1)
boot.fn(Auto, 1:392)
boot_obj <- boot(Auto, boot.fn, 1000)
boot_obj
hist(boot_obj$t[, 3])

# bootstrapped percentile CI
apply(boot_obj$t, 2, function(x) quantile(x, c(0.025, 0.975))) %>% 
  t() %>% 
  round(3)

lm_fit <- lm(mpg ~ horsepower + I(horsepower^2), data = Auto)

summary(lm_fit)$coef
confint(lm_fit) %>% 
  round(3)

apply(boot_obj$t, 2, function(x) quantile(x, c(0.025, 0.975))) %>% 
  t() %>% 
  round(3)

###


