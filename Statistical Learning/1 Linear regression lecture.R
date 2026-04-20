#####################
## 1. Data loading ##
#####################

## We implement linear regression on the Advertising dataset

advert = read.csv("advertising.csv", row.names = 1) # import the dataset 

head(advert)
attach(advert) # do you remember the attach command? Otherwise check ?attach


######################
## 2. Model fitting ##
######################

# We can use function 'lm' (linear model) to build a linear model
# Y = beta0 + beta1*X1 + ... + betap*Xp + error

# We consider sales (Y) as a function TV (X1), radio (X2) and newspaper (X3)

lm(sales ~ TV + radio + newspaper)

# Output consists of parameter estimates

# However, using the function 'summary' we obtain a richer output

out_lm = lm(sales ~ TV + radio + newspaper)

summary(out_lm)

# with standard errors of estimated coefficients, results of hypothesis tests and goodness of fit measures

# We can extract results from this output such as

out_lm$coefficients
out_lm$residuals

# or also

summary_lm = summary(out_lm)
summary_lm$adj.r.squared


#####################################
## 3. Prediction and model fitting ##
#####################################

# Suppose we want to predict sales for new values of predictors
# for instance TV = 250, radio = 50, newspaper = 0

predict(out_lm, newdata = data.frame(TV = 250, radio = 50, newspaper = 0)) # is the predicted value of sales

## Compute Mean Squared Error (MSE)

y_hat = predict(out_lm) # by omitting "new.data", computes predicted value of Y for each subject

y = sales
n = length(sales)

mse = sum((y - y_hat)^2)/n
mse
