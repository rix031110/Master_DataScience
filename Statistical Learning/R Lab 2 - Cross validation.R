########################
## Resampling methods ##
########################

# We need to download and install the package ISLR. How?
# In the right-hand side (bottom) window:
# Packages > Install > Install from: Repository (CRAN) > ISLR > Install
# Then we can load the the downloaded package using:

library(ISLR)

# This package contains many useful functions for cross validation, bootstrap,
# as well as several implementations of statistical learning methods


#############################
## Validation set approach ##
#############################

head(Auto)


## Model mgp as a function of horsepower

## [1] Using linear regression

plot(Auto$horsepower, Auto$mpg, ylab = "mpg", xlab = "horsepower", col = "blue4", pch = 20, ylim = c(8,50))

n = nrow(Auto) # there are 392 observations in the dataset

# I construct a training set by randomly selecting n/2 observations

set.seed(123)

train = sample(1:n, n/2) # these observations will be used to fit the model
train

# The remaining observations will be used as a test (validation) set

test = setdiff(1:n, train) # difference between sets 1:n and train
test

# parameter 'subset' can be used to fit linear model using a subset of the data only

lm_fit = lm(mpg ~ horsepower, data = Auto, subset = train)

abline(lm_fit) # add my regression model in the scatter plot

summary(lm_fit)


Auto_test = Auto[test,] # I create the test dataset

head(Auto_test)

fitted_test = predict(lm_fit, newdata = Auto_test)  # fitted values for mpg (only for obs' within the test set)
fitted_test

# Compute the test MSE
# as the mean of the residuals^2 between observed mpg and fitted mpg (for obs' in the test set)

y_test = Auto_test$mpg

mean((y_test - fitted_test)^2)

# The test MSE for linear model is equal to 21.24991


## [2] Using polynomial regression of order 2

poly(Auto$horsepower,2)

head(poly(Auto$horsepower,2))

lm_fit_2 = lm(mpg ~ poly(horsepower,2), data = Auto, subset = train)

# with poly(horsepower,2) we include both horsepower and horsepower^2
# Notice that by default variables are zero-centered. Check:

poly(Auto$horsepower, 2)
poly(Auto$horsepower, 2, raw = T) # with raw = T they are not centered

# centering variables do not affect model fitting only the value of the intercept

fitted_test_2 = predict(lm_fit_2, newdata = Auto_test)  # fitted values for mpg (only for obs' within the test set)
fitted_test_2

# Compute the test MSE

mean((Auto_test$mpg - fitted_test_2)^2)

# The test MSE for polynomial model of order 2 is equal to 16.48112


## [3] Using polynomial regression of order 3

lm_fit_3 = lm(mpg ~ poly(horsepower,3), data = Auto, subset = train)

fitted_test_3 = predict(lm_fit_3, newdata = Auto_test)

mean((Auto_test$mpg - fitted_test_3)^2)

# The test MSE for polynomial model of order 3 is equal to 16.58276


####################################
## Leave-One-Out Cross-Validation ##
####################################

# LOOCV is implemented in the function cv.glm in the library 'boot'
# can be applied to (generalized) linear models

library(boot)

# first fit the glm (without specifying the family we go back to lm function)

Auto

fit_lm = glm(mpg ~ horsepower, data = Auto)

# then apply cv.glm to the dataset Auto and fit_lm

loocv_out = cv.glm(Auto, fit_lm)

loocv_out$delta[1] # this is the estimated test error for LOOCV (CV_n)

# I can apply cross-validation to each polynomial model to choose the best one
# We do this using k-fold cross validation (see next)


#############################
## k-fold cross validation ##
#############################

# We apply k-fold cross validation to discriminate between polynomial models of order from 1 to 8

# We fix k = 10 (number of folds)
# and then divide the n = 392 observations into 10 groups of approximately same size

dim(Auto)

set.seed(1)
cv.error.8 = rep(NA, 8) # in this empty vector I will store the 8 CVs (one for each polynomial model)

for(i in 1:8){
  lm_fit = glm(mpg ~ poly(horsepower,i), data = Auto)
  cv.error.8[i] = cv.glm(Auto, lm_fit, K = 10)$delta[1]
}

cv.error.8

plot(cv.error.8, xlab = "polynomial degree (model complexity)", ylab = "CV", type = "b")

# "best" (i.e. balancing model complexity and model fitting) is polynomial model of order 2
