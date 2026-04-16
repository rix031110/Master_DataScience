######################
## Regression trees ##
######################

# Load the wine quality data

wine = read.csv("wine_quality.csv", sep = ";")

str(wine) # alcohol is of type chr, need to convert into numeric (since we treat it as quantitative)

wine$alcohol = as.numeric(wine$alcohol)

attach(wine) #we can simply write the name of the variables

n = nrow(wine)

# Some plots to investigate the relationship between selected predictors and response
# I associate colors to levels of quality

col_quality = quality

col_quality[quality == 3] = "coral4"
col_quality[quality == 4] = "lightcoral"
col_quality[quality == 5] = "peachpuff1"
col_quality[quality == 6] = "cadetblue2"
col_quality[quality == 7] = "cadetblue3"
col_quality[quality == 8] = "cadetblue4"


plot(alcohol, pH, xlim = c(8,15), col = col_quality, pch = 16, cex = 0.7)

legend(13.2, 4.5, legend = c("3", "4", "5", "6", "7", "8"),
       col = c("coral4", "coral2", "wheat2", "cadetblue2", "cadetblue3", "cadetblue4"),
       pch = 16, cex = 0.8, title = "Quality", box.col = "white")

# Tree-based methods are implemented in the package 'tree'
# that we need to install (Packages/Install/tree)

library(tree)

# We can use function 'tree' to fit trees from the data using recursive binary splitting
# Syntax is very similar to function 'lm'
# tree(Y ~ ., data = dataset) to include all predictors in dataset

# I start by including all the observations (without creating train and test sets)

tree.wine = tree(quality ~ ., data = wine)

out.wine = summary(tree.wine)

# Variables used in the tree's construction are

out.wine$used

# We can plot the tree simply using plot() applied to an object of type 'tree' like tree.wine 

plot(tree.wine)
text(tree.wine) # to add labels

# Check for instance predicted value of quality within region R1:
# corresponding to alcohol < 10.525 and sulphates < 0.575

R1 = quality[alcohol < 10.525 & sulphates < 0.575]

mean(R1, na.rm = TRUE)


##########################################
## Training and cost complexity pruning ##
##########################################

# Now I consider a training set

# Try without setting the seed and repeat the procedure
# The estimated tree is quite sensitive to the change in training set

set.seed(123)
train = sample(1:n, n/2) #sample from 1 to n n/2 elements

tree.wine = tree(quality ~ ., wine, subset = train)
summary(tree.wine)

# Estimated tree on the training data is

plot(tree.wine)
text(tree.wine)

# Now we can prune our tree using cost complexity pruning
# Output is a collection of sub-trees corresponding to different sizes (and values of alpha)
# To choose the best sub-tree we use cross validation (by default K-fold cross validation is implemented)

# All these steps are implemented in the function 'cv.tree'
# which takes as input an object of type tree
# k-fold cross validation is implemented on the dataset used to fit the tree only

?cv.tree

cv.wine = cv.tree(tree.wine, K = 10) # K = 10 for 10-fold cross validation

# These are the sizes of the estimated sub-trees (obs: in decreasing order)

cv.wine$size #number of regions in each fold

# These are the corresponding RSS (CV errors)

cv.wine$dev

# I plot the CV errors to find the optimal sub-tree

plot(cv.wine$size, cv.wine$dev, type = "b", xlab = "|T| (size of subtree)", ylab = "CV error")

# Find the minimum CV error

min.rss = which.min(cv.wine$dev)
min.rss

# and the corresponding size

size.best = cv.wine$size[min.rss]
size.best

# Obs: trees are quite sensitive to the choice of the training set

# Therefore, we can repeat this procedure a number of times
# Each time we obtain a best size

# [To find the best size of sub-tree consider to repeat the previous procedure a number of times]
# [Exercise]

# To obtain the pruned tree we can use the function 'prune.tree'
# 'prune.tree' requires as input an object of type 'tree' (such as tree.wine)
# and for a (best) size obtain the pruned tree

prune.wine = prune.tree(tree.wine, best = size.best)

plot(prune.wine)
text(prune.wine)


#############################
## Testing the fitted tree ##
#############################

# Now, we evaluate the fitting on the test data set
# We can use the function predict to obtain y.hat (predicted values of Y)

test = setdiff(1:n, train)

y.hat = predict(prune.wine, #model used to predict
       newdata = wine[test,]) #which data we want to predict

# Obs: predicted values are uniform within each region

unique(y.hat) # to display only the distinct values within a vector

# select test data for variable quality (y.test)

y.test = wine[-train, "quality"]

# Residual Sum of Squares (RSS) is then

mean((y.test - y.hat)^2)
