#####################
## Riboflavin data ##
#####################

library(hdi)

data(riboflavin)

# Reference:
# Buhlmann, P., Kalisch, M. and Meier, L. (2013)
# High-dimensional statistics with a view towards applications in biology
# Annual Review of Statistics and its Applications

?riboflavin

str(riboflavin)

y = riboflavin$y
X = riboflavin$x

length(y)
dim(X)

# Problem is to identify which genes (out of the 4088 available) are responsible of riboflavin production/regulation.
# Bacterium named "Bacillus Subtilis" is used in agriculture for plant-growth.
# Its production of riboflavin (vitamin B2) is the main responsible of plant-growth.
# One objective is therefore to control/increase riboflavin production, e.g. by means of gene manipulation.
# Which genes?

str(X)

p = ncol(X)
n = nrow(X)

n
p


## lm fails because n < p

out_lm = lm(y ~ X)
summary(out_lm)

## Stepwise subset selection methods are computational expensive
## Do not run the following (my laptot crashed!)

library(leaps)

out_fwd = regsubsets(y ~ X, data = X, nvmax = 50, method = "forward")
reg_summary_fwd = summary(out_fwd)


## So, in lasso we trust

library(glmnet)

grid = 10^seq(-2, 10, length = 100)
k = length(grid)

lasso_grid = glmnet(X, y, alpha = 1, lambda = grid, standardize = TRUE)
beta_lasso = coef(lasso_grid)

lasso_grid$lambda

# We implement cross validation methods to choose lambda

set.seed(1)
cv.out = cv.glmnet(X, y, lambda = grid, alpha = 1)

# CV errors (one for each value of lambda)

par(mfrow = c(1,1))
plot(cv.out)

# Optimal value of lambda is

bestlam = cv.out$lambda.min
bestlam

# Finally, fit the model for lambda = bestlam on the complete dataset

lasso_best = glmnet(X, y, alpha = 1, lambda = bestlam, standardize = TRUE)

lasso_best$a0
lasso_best$beta

# These are the genes included in the model and their parameters

genes_in = colnames(X)[which(lasso_best$beta != 0)]
coeff_in = lasso_best$beta[which(lasso_best$beta != 0)]

names(coeff_in) = genes_in
sort(round(coeff_in, 2), decreasing = TRUE)
