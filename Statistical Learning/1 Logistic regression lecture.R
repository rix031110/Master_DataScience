#####################
## 1. Data loading ##
#####################

## We implement logistic regression on the Titanic dataset
## Load the data and remove rows containing NA's

titanic = read.csv("titanic_train.csv")
titanic = titanic[complete.cases(titanic),]

head(titanic)

## Select the response variable y (Survived, 0 no, 1 yes)

y = titanic$Survived

## Select covariates

X.tmp = as.data.frame(titanic[,c(3,5:8,10)])
str(X.tmp)

## Convert factor variables into dummies using the function model.matrix
## By default model.matrix includes a unit column (for the intercept term); we can remove it

X = model.matrix(y ~ ., X.tmp)[,-1]
head(X)


######################
## 2. Model fitting ##
######################

## Fit logistic regression with the function glm under family "binomial" and link "logit"

out_glm = glm(y ~ X, family = binomial(link = logit))
summary(out_glm)


######################################
## 3. Prediction and classification ##
######################################

## Build the data matrix (including now a unit column for the intercept term)

XX = cbind(1, X)
head(XX)

## Select the estimated regression coefficients from the glm output

beta_hat = out_glm$coefficients
beta_hat

## Estimate probabilities by computing the linear predictor (XX%*%beta_hat) and applying the inverse logit function

pi_hat = exp(XX%*%beta_hat)/(1 + exp(XX%*%beta_hat))
pi_hat

## Provide graphical representations of the results

col_lab = c()

col_lab[y == 0] = "deepskyblue1"
col_lab[y == 1] = "hotpink"

par(mfrow = c(1,2), mar = c(5,5,2,2))

plot(pi_hat, col = col_lab, pch = 20, xlab = "subject (i)", ylab = expression(hat(pi)[i]))
boxplot(pi_hat ~ y, xlab = "survival", ylab = expression(hat(pi)[i]), col = c("deepskyblue1", "hotpink"), pch = 20)

## For classification purposes, we can set y_fat = 1 if pi_hat > 0.5, otherwise y_hat = 0

y_hat = ifelse(pi_hat > 0.5, 1, 0)

## Compare true and estimated values of y and compute the Error Rate (ER)

table(y, y_hat)

er = sum(y != y_hat)/length(y)
er


###########################################################
## 4. Predict probability of survival for new passengers ##
###########################################################

## Covariates in the glm output are:

## Intercept : the intercept (1 for all observations)
## Pclass    : the travel class, treated as integer in {1,2,3}
## Sexmale   : the passenger gender, binary in {0,1} (0 male, 1 female)
## Age       : the age of the passenger, numerical
## SibSp     : the number of siblings on board, integer
## Parch     : the number of parents/children on board, integer
## Fare      : the ticket fare, numerical

## Consider two passengers with the following levels of the covariates

x.star.rose = c(1, 1, 0, 17, 0, 2, 150)
x.star.jack = c(1, 3, 1, 19, 0, 0, 40)

## Estimate the corresponding probabilities of survival

pi_hat_rose = exp(x.star.rose%*%beta_hat)/(1 + exp(x.star.rose%*%beta_hat))
pi_hat_jack = exp(x.star.jack%*%beta_hat)/(1 + exp(x.star.jack%*%beta_hat))

pi_hat_rose
pi_hat_jack

