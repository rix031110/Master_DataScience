############################
## Linear model selection ##
############################

library(ISLR)

# Main functions are collected in library 'leaps' (need to install it the first time)

library(leaps)

# We first consider the Credit dataset
# ?Credit

str(Credit)

y = Credit$Balance # Response variable is Balance (Y)

# while I consider the following p = 9 predictors

X = Credit[,2:10]

colnames(X)


###########################
## Best subset selection ##
###########################

p = ncol(X) # this is the number of predictors
p

# I want to fit all the linear models including a subset of the predictors

# There are choose(p,k) different models including exactly k predictors
# I can find them as follows

# There are choose(p, 2) = 36 different models including exactly 2 predictors
choose(p, 2) #it chooses from p 2 elements

# These are my models
combn(1:p, 2) # each column corresponds to a model, i.e. to the labels of the predictors included in the model

combn(1:p, 3) # 84 models including exactly 3 predictors
choose(p, 3)

# I can build a loop cycle iterating over q = 1,...,p and the collection of models with exactly q predictors.
# Fortunately, function 'regsubsets' already implements best subset selection 

out_subset  = regsubsets(y ~ ., # it is the equation of the model, woth the dot we consider
                                #all variables in the data matrix X
                         data = X, #
                         nvmax = p) # with 'nvmax' I can also fix the maximum number of predictors (q) that can be included

str(out_subset)

# with 'summary' I display for each number of predictors included (q = 1,...,p) the best model

reg_summary = summary(out_subset)
reg_summary
#The star symbol shows the best model with q predictors.


# I can choose among the p best models using adjusted R squared, RSS, ...

reg_summary$adjr2 # These are the 9 R squared, one for each of the 9 best models
reg_summary$rss #These are the 9 RSS for each best model

par(mfrow = c(1,2))
plot(reg_summary$rss, xlab = "number of predictors (k)", ylab = " RSS", type = "b", xaxt = "none")
axis(1, 0:p)
plot(reg_summary$adjr2, xlab = "number of predictors (k)", ylab = expression(adjustedR^2), type = "b", xaxt = "none")
axis(1, 0:p)


########################
## Stepwise Selection ##
########################

# I can use function 'regsubsets' for forward and backward selection too
# I just need to set parameter 'method' equal to "forward" or "backward"

out_fwd = regsubsets(y ~ ., X, nvmax = p, method = "forward")
reg_summary_fwd = summary(out_fwd)

reg_summary_fwd

reg_summary_fwd$adjr2

out_bwd = regsubsets(y ~ ., X, nvmax = p, method = "backward")
reg_summary_bwd = summary(out_bwd)

reg_summary_bwd
reg_summary_bwd$adjr2


par(mfrow = c(1,2))

plot(0:p, c(0, reg_summary_fwd$adjr2), ylim = c(0,1), xlab = "number of predictors (k)", type = "b",
     ylab = expression(adjustedR^2), xaxt = "none", col = "cyan4", main = "Forward")
axis(1, 0:p)

plot(0:p, c(0, reg_summary_bwd$adjr2), ylim = c(0,1), xlab = "number of predictors (k)", type = "b",
     ylab = expression(adjustedR^2), xaxt = "none", col = "brown", main = "Backward")
axis(1, 0:p)



##############################
## With logistic regression ##
##############################

# We consider a dataset from breast cancer patients
# Goal is to predict the occurrence of breast cancer given clinical features

breast = read.csv("breast.csv", header = TRUE)

# Information at https://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Coimbra (Shift + Click to open the link)

head(breast)

breast$Classification # this is my response Y: it is 1 if healthy control, 2 if cancer patient

# For convenience I convert it into a 0-1 variable

y = ifelse(breast$Classification == 1, 0, #if classification is 1 make it 0 else 1
                                1)

# Select the predictors

X = breast[,-10] #we removed classification
p = ncol(X) #number of predictors
p

## Best subset, Forward and Backward selection for generalized linear models (including logistic regression)
## are not implemented in the library leaps. I therefore need the following library:

library(bestglm)

cbind(X,y) #bind matrix X with vector y, in this order

out_subset = bestglm(Xy = cbind(X,y),#the union of matrix and vector
                     family = binomial(link = "logit"), #the generalised linear model to find
                     IC = "AIC") #on what we compare the models, information criteria 

out_subset$BestModel #it shows the best model of all 
summary(out_subset)

out_subset$Subsets # these are the q best models with their AICs, with TRUE is the best model for that q
#There is a star on the best model near the number, the one with the lowest AIC

out_fwd = bestglm(Xy = cbind(X,y), family = binomial(link = "logit"), IC = "AIC", method = "forward")

out_fwd
summary(out_fwd)

out_bwd = bestglm(Xy = cbind(X,y), family = binomial(link = "logit"),IC = "AIC", method = "backward")

out_bwd
summary(out_bwd)
