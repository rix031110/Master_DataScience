##########################
## Classification trees ##
##########################

library(tree)

# We consider the titanic dataset that we divide into a training and test set

titanic = read.csv("titanic_train.csv")
titanic = titanic[complete.cases(titanic),]

str(titanic)

# Notice that Survived (the response variable y, 0 if no, 1 if yes) is an integer vector
# therefore it will be treated as a quantitative variable
# To fit a classification tree, I need to convert it into factor

titanic$Survived = as.factor(titanic$Survived)

str(titanic)

attach(titanic)

# We create the train and test sets and corresponding y and X

n = nrow(titanic)

set.seed(123)
train = sample(1:n, n/2)
test  = setdiff(1:n, train)

# Select the response y

y = as.factor(titanic$Survived)

# Select the covariates

X.tmp = as.data.frame(titanic[,c(3,5:8,10)])
str(X.tmp)

# Convert factor variables into dummies using the function model.matrix
# By default model.matrix includes a unit column (for the intercept term); we can remove it

X = model.matrix(y ~ ., X.tmp)[,-1] #the model we would like to fit to create dummy variables
X = as.data.frame(X)

head(X)

attach(X)

y_train = y[train]
y_test  = y[test]

X_train = X[train,]
X_test  = X[test,]


##############
## Training ##
##############

# Fit a classification tree (using recursive binary splitting) on the train dataset

tree.titanic = tree(y_train ~ ., data = X_train)

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
