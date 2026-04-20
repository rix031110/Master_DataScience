###############
## Bootstrap ##
###############

library(boot)
library(ISLR)

###########################################
## Example on OLS parameter distribution ##
###########################################

# We consider the linear model Y = alpha + beta*x + eps where eps ~ N(0,sigma2)
# I generate n = 1000 obs for some values of parameters alpha, beta, sigma

n = 1000

beta0 = 10
beta1 = 4
sigma = 2

set.seed(300)

eps = rnorm(n, 0, sigma)
x   = rnorm(n)
y   = beta0 + beta1*x + eps #the 1000 realisations of y

plot(x, y, ylab = "y", xlab = "x", col = "blue4", pch = 20) # These are my n = 1000 data points

# I can compute the (true) mean and variance of the estimator of beta1

X = cbind(1, x)

head(X)

mean_beta1 = beta1

(sigma^2*solve(t(X)%*%X))[2,2]

var_beta1  = (sigma^2*solve((t(X)%*%X)))[2,2]

# Distribution of estimator beta1_hat is N(beta1, var_beta1)

# Now I want to approximate the distribution of beta1_hat using the bootstrap

B = 5000 # number of bootstrap iterations (draws)

out_boot = c() # in this empty vector I will store bootstrap estimates of beta1

set = sample(1:n, n, replace = TRUE)

y[set]
x[set]

out = lm(y[set] ~ x[set])

out$coefficients[2]

for(b in 1:B){
  
  set = sample(1:n, n, replace = TRUE) # each time I randomly draw (with replacement) a sample of size n from my population
  
  out_boot[b] = lm(y[set] ~ x[set])$coefficients[2] # I estimate (and store) beta1 using lm
  
}

out_boot

# Distribution of beta_hat approximated by bootstrap is

hist(out_boot, xlab = expression(hat(beta)[1]), main = "", col = "lightblue", freq = FALSE, ylim = c(0, 7), breaks = 30)

# I can add the true distribution of beta_hat

plot(function(x) dnorm(x, mean_beta1, sd = sqrt(var_beta1)), from = 3.5, to = 4.5,
add = TRUE, col = "black", lwd = 1.5)


#################################
## Example on asset allocation ##
#################################

# We consider the Portfolio dataset which contains 100 returns for two assets X and Y

?Portfolio # for more information on the dataset

head(Portfolio)

plot(Portfolio)

X=Portfolio$X
Y=Portfolio$Y



# I start by defining a function to estimate alpha (best allocation between the two assets)

# This function will depend on some "population" data
# and a vector (index) indexing the data that at each bootstrap iteration must be used to compute the estimate
# In our example, data is a two-column dataframe with X and Y

alpha.fn = function(data, index){
  
  X = data$X[index]
  Y = data$Y[index]
  
  alpha_hat = (var(Y) - cov(X,Y))/(var(X) + var(Y) - 2*cov(X,Y))
  
  return(alpha_hat)
  
}

# if for instance we fix index = 1:100 all 100 observations are used to estimate alpha:

alpha.fn(Portfolio, 1:100)

# Each bootstrap estimate can be instead obtained by randomly drawing with replacement 100 observations from the dataset
# i.e. by setting index = sample(1:100, 100, replace = T)

sample(1:100, 100, replace = T)

alpha.fn(data = Portfolio, index = sample(100, 100, replace = T))

# I need to reiterate the previous command for number of bootstrap iterations B = 5000
# I can do this using a for cycle (as in the first example) or the function 'boot'

# Function 'boot' requires three arguments: a dataset, a function to be applied on, the number of bootstrap iterations
# It outputs an estimate of the standard error of the estimator defined by alpha.fn

?boot

out_boot = boot(Portfolio, alpha.fn, R = 5000) #r is the number of replicates

str(out_boot)

out_boot$t #collect the bootstrap draws

# out_boot$t collects the R bootstrap draws

hist(out_boot$t, xlab = expression(hat(alpha)), main = "", col = "lightgreen", freq = FALSE, ylim = c(0, 5), breaks = 30)

# From the output we can recover
# the expected value and standard deviation of the estimator, as well as confidence intervals

mean(out_boot$t)
sd(out_boot$t)

quantile(out_boot$t, c(0.025,0.975))


