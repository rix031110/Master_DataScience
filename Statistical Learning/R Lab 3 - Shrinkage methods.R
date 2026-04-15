#######################
## Shrinkage methods ##
#######################

library(ISLR)

# We consider the Hitters dataset which contains information on n = 322 baseball players

?Hitters

head(Hitters)
tail(Hitters)

# There are some NA's in the dataset
# We can remove the corresponding records (rows) using the function 'complete.cases'
# which returns TRUe if row is complete, FALSE otherwise

complete.cases(Hitters)

# In this way I select only complete rows

Hitters = Hitters[complete.cases(Hitters),]

# The dataset contains some categorical variables
# Using 'model.matrix' function I convert categorical variables (if present in the dataset)
# into dummy variables

X = model.matrix(Salary ~., Hitters)[,-1] #Salary is not considered

head(X)

str(Hitters)

# This will be my (n,p) matrix with obs' from the predictors
# I remove first columns since it contain:
# a unit vector (for the intercept)

head(model.matrix(Salary ~., Hitters))

y = Hitters$Salary # This is the response variable

p = ncol(X)
n = nrow(X)

p;n

# Ridge and Lasso regression are implemented within the package glmnet

library(glmnet)


######################
## Ridge regression ##
######################

# I can fit ridge and lasso using the same function, 'glmnet'
# I set alpha = 0 for ridge, alpha = 1 for lasso

# see also
?glmnet

# for lambda = 0 I go back to lm regression

ridge_0 = glmnet(X, y, alpha = 0, lambda = 0, standardize = TRUE)#y is the variablòe to be predicted
# X are the predictors 
#I can standardize the predictors

lm(y ~ X)

ridge_0

# With parameter standardize (TRUE or FALSE) I can decide whether to standardize var's or not

out_summary = summary(ridge_0)
str(out_summary)

# Parameter estimates are in a0 and beta

ridge_0$a0
ridge_0$beta

# I can run glmnet for several values of lambda
# I create a grid of values for lambda (not uniform to better display results)

seq(-2,10, length = 100) #we create a uniform sequence 

grid = 10^seq(-2,10, length = 100) #create small values as lambda is more sensitive to small values
plot(grid)

ridge_grid = glmnet(X, y, alpha = 0, lambda = grid, standardize = TRUE)#lamda can be a vector

# I can print all the coefficient estimates using 'coef'
# I obtain a matrix with one row for each variable and one column for each value of lambda

coef_grid = coef(ridge_grid)
str(coef_grid)

# Also, I can select the corresponding values of lambda as

ridge_grid$lambda #by defaault it is sorted buy decreasing values
plot(ridge_grid$lambda)

# Let's see the behavior of some coefficient estimates as a function of lambda
# Notice that by default results are sorted by decreasing values of lambda
# Therefore, to sort them according to increasing values of lambda I can do

K = length(grid)

coef_grid[2,K:1] #for beta 2 hat select the second row but starting from k to 1
coef_grid[3,K:1]

# e.g. for beta_2, beta_3

par(mfrow = c(1,3), mar = c(5,5,2,2))

plot(coef_grid[2,K:1], type = "l", xlab = expression(lambda), ylab = expression(hat(beta)[1]), xaxt = "none")
abline(h = 0, col = "red", lty = "dashed")
axis(side = 1, at = c(0,100), labels = c(0,max(grid)))

plot(coef_grid[3,K:1], type = "l", xlab = expression(lambda), ylab = expression(hat(beta)[2]), xaxt = "none")
abline(h = 0, col = "red", lty = "dashed")
axis(side = 1, at = c(0,100), labels = c(0,max(grid)))

plot(coef_grid[4,K:1], type = "l", xlab = expression(lambda), ylab = expression(hat(beta)[3]), xaxt = "none")
abline(h = 0, col = "red", lty = "dashed")
axis(side = 1, at = c(0,100), labels = c(0,max(grid)))


## How to choose lambda?

# We can use validation set approach or k-fold cross validation
# In the following I adopt k-fold cross validation
# This is implemented in the function 'cv.glmnet'
# by default k = 10, otherwise I can choose a different number by setting 'nfolds'

?cv.glmnet

set.seed(1)
cv.out = cv.glmnet(X, y, lambda = grid, alpha = 0) # alpha 0 to do ridge regression

# The function returns a collection of cv errors (one for each value of lambda)

par(mfrow = c(1,1))
plot(cv.out)

# The optimal value of lambda is then

bestlam = cv.out$lambda.min
bestlam

# Finally, I can fit my model for lambda = bestlam on the complete dataset

ridge_best = glmnet(X, y, alpha = 0, lambda = bestlam, standardize = TRUE)

ridge_best$a0
ridge_best$beta


###########
## Lasso ##
###########

# We can implement lasso simply by setting alpha = 1 in 'glmnet' function
# Again, I consider a grid of values for lambda

grid = 10^seq(-2, 10, length = 100)

k = length(grid)

lasso_grid = glmnet(X, y, alpha = 1, lambda = grid, standardize = TRUE)

beta_lasso = coef(lasso_grid)

ridge_grid$lambda

coef(lasso_grid)

# Let's see the behavior of beta estimates as a function of lambda

par(mfrow = c(1,3), mar = c(5,5,2,2))

plot(coef(lasso_grid)[2,k:1], type = "l", xlab = expression(lambda), ylab = expression(hat(beta)[1]), xaxt = "none")
abline(h = 0, col = "red", lty = "dashed")
axis(side = 1, at = c(0,100), labels = c(0,max(grid)))

plot(coef(lasso_grid)[3,k:1], type = "l", xlab = expression(lambda), ylab = expression(hat(beta)[2]), xaxt = "none")
abline(h = 0, col = "red", lty = "dashed")
axis(side = 1, at = c(0,100), labels = c(0,max(grid)))

plot(coef(lasso_grid)[4,k:1], type = "l", xlab = expression(lambda), ylab = expression(hat(beta)[3]), xaxt = "none")
abline(h = 0, col = "red", lty = "dashed")
axis(side = 1, at = c(0,100), labels = c(0,max(grid)))

# Notice that, differently from ridge coefficients, from some value of lambda on, are set = 0

# Similarly as before, we can implement cross validation methods to choose lambda

set.seed(1)
cv.out = cv.glmnet(X, y, lambda = grid, alpha = 1)

# The function returns a collection of cv errors (one for each value of lambda)

par(mfrow = c(1,1))
plot(cv.out)

# The optimal value of lambda is then

bestlam = cv.out$lambda.min
bestlam

# Finally, I can fit my model for lambda = bestlam on the complete dataset

lasso_best = glmnet(X, y, alpha = 1, lambda = bestlam, standardize = TRUE)

lasso_best$a0
lasso_best$beta
