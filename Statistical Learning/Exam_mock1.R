df=read.csv("Maria Bianchi.csv")
head(df,15)
summary(df)

table(df$Holiday)
#I wpuld remove variables: Holiday, Month, Year and Snowfall as they are not relevant to the analysis of the data.

df1= subset(df, select = -c(Holiday, Month, Year, Snowfall.cm))
summary(df1)

attach(df1)

df1$Day.Night <- ifelse(Hour >= 6 & Hour < 23, 1, 0)

### QUESTION 2
library(dplyr)
df1$Rented.Bike.Count= ifelse(df1$Rented.Bike.Count >= 655, 1L,0L)
df1= rename (df1, Excess = Rented.Bike.Count)
summary(df1)

###REGRESSION MODEL
Y= Excess
head(Y)
X.df= as.data.frame(df1[,-c(2)])
head(X.df)

X= model.matrix(Y ~ ., X.df)[,-1]
head(X)

out_glm = glm(Y ~ X, family = binomial(link = logit))
summary(out_glm)

out_glm$coefficients

###QUESTION 3
p = ncol(X) #number of predictors
p

## Best subset, Forward and Backward selection for generalized linear models (including logistic regression)
## are not implemented in the library leaps. I therefore need the following library:

library(bestglm)

Xy=cbind(X,Y) #bind matrix X with vector y, in this order
Xy= as.data.frame(Xy) #make it a data frame
summary(Xy)
out_subset = bestglm(Xy = cbind(X,Y),#the union of matrix and vector
                     family = binomial(link = "logit"), #the generalised linear model to find
                     IC = "AIC") #on what we compare the models, information criteria 

out_subset$BestModel #it shows the best model of all 
summary(out_subset)

out_subset$Subsets # these are the q best models with their AICs, with TRUE is the best model for that q
#There is a star on the best model near the number, the one with the lowest AIC

out_fwd = bestglm(Xy = Xy, family = binomial(link = "logit"), IC = "AIC", method = "forward")

best_model=out_fwd$BestModel
summary(out_fwd)

out_bwd = bestglm(Xy = Xy, family = binomial(link = "logit"),IC = "AIC", method = "backward")

out_bwd$BestModel
summary(out_bwd)

###QUESTION 4
library(tree)
Y= as.factor(Y) #make the dependent variable a factor for classification

n = nrow(df1)

set.seed(123)
train = sample(1:n, n/2)
test  = setdiff(1:n, train)

# Select the covariates

X.tmp = as.data.frame(X.df) #remove the variable that we want to predict, which is the second column of X.df
str(X.tmp)

# Convert factor variables into dummies using the function model.matrix
# By default model.matrix includes a unit column (for the intercept term); we can remove it

X = model.matrix(Y ~ ., X.tmp)[,-1] #the model we would like to fit to create dummy variables
X = as.data.frame(X)

head(X)

attach(X)

y_train = Y[train]
y_test  = Y[test]

X_train = X[train,]
X_test  = X[test,]


##############
## Training ##
##############

# Fit a classification tree (using recursive binary splitting) on the train dataset

tree.titanic = tree(y_train ~ out_fwd$BestModel, data = X_train)

out = summary(tree.titanic)

out$used

plot(tree.titanic) #we get the mode of each region
text(tree.titanic)

# Now we can implement cost complexity pruning

cv.titanic = cv.tree(tree.titanic, K = 10)

plot(cv.titanic$size, cv.titanic$dev, type = "b", xlab = "|T| (size of subtree)", ylab = "CV error")

cv.titanic$size
cv.titanic$dev

# Find the sub-tree with minimum CV error and prune the original tree

size.best = cv.titanic$size[which.min(cv.titanic$dev)]

prune.titanic = prune.tree(tree.titanic, best = size.best)

plot(prune.titanic)
text(prune.titanic)


#############
## Testing ##
#############

# We predict Survived for passengers in the test set 
#probability of 0 or 1 for each observation
pi.hat = predict(prune.titanic, newdata = X_test)
pi.hat

head(pi.hat)

# Output of predict is a pair of probabilities (1 - p.hat, p.hat) for each subject 
# p.hat corresponds to the proportion of subjects with Survived = 1 in the region

p.hat = pi.hat[,2]

# to obtain a prediction of y I can set y.hat = 1 if p.hat > 0.5, 0 otherwise

y.hat = ifelse(p.hat > 0.5, 1, 0)

# To compare predicted and observed values of y
# I can construct a contingency table

tab = table(y_test, y.hat)

# So, what is the test error rate?

er = (tab[1,2] + tab[2,1])/sum(tab)
er