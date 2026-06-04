set.seed(123)
df=read.csv("Riccardo Dondi_house.csv")

###QUESTION 1
#Cleaning ds
nrow(df)
house_clean= df[complete.cases(df),]
nrow(house_clean)
head(house_clean)
attach(house_clean)

#Summary statistics
stats_RxH= summary(house$RoomsPerHousehold)
stats_BxR= summary(house$BedroomsPerRoom)
stats_PxH= summary(house$PopulationPerHousehold)

#Plots
par(mfrow=c(1,1))
plot(x=house_clean$TotalRooms,y=house_clean$Households, type= 'p', main="Room per Household", xlab="Total rooms", 
ylab="Household",ylim=c(0,5358), pch=20)

plot(x=house_clean$TotalBedrooms,y=house_clean$TotalRooms, type= 'p', main="Bedrooms per Room", xlab="Total bedrooms", 
ylab="Total room",xlim=c(0,6210), pch=20)


plot(x=house_clean$Population,y=house_clean$Households, type= 'p', main="Population per Household", xlab="Population", 
ylab="Household",xlim=c(0,16305), ylim=c(0,5358), pch=20)


plot(x=house_clean$RoomsPerHousehold, type='p', main='Rooms per Household', ylab='Frequencies', xlab='Number of rooms per household')

#Replacing variables

house_clean$RoomsPerHousehold=TotalRooms/Households

house_clean$BedroomsPerRoom=TotalBedrooms/TotalRooms

house_clean$PopulationPerHousehold=Population/Households
head(house_clean)

summary(house_clean)

colnames(house_clean)
house= house_clean[,-c(4,5,6)]

###QUESTION 2

y=house$MedianHouseValue
X= house[,-c(6)] #removing median house value

output_lm= lm(y ~ ., data=X)

summary(output_lm)

output_lm$coefficients
output_lm$residuals
summary_lm$adj.r.squared
summary_lm$residuals
###QUESTION 3
library(ISLR)

library(leaps)

p = ncol(X) #number of predictors
p

out_fwd = regsubsets(y ~ ., X, nvmax = p, method = "forward")
reg_summary_fwd = summary(out_fwd)

reg_summary_fwd

reg_summary_fwd$adjr2

out_bwd = regsubsets(y ~ ., X, nvmax = p, method = "backward")
reg_summary_bwd = summary(out_bwd)

reg_summary_bwd
reg_summary_bwd$adjr2

par(mfrow = c(1,2))

plot(0:p, c(0, reg_summary_fwd$adjr2), ylim = c(0,1), xlab = "Number of predictors (k)", type = "b",
     ylab = expression(adjustedR^2), xaxt = "none", col = "cyan4", main = "Forward")
axis(1, 0:p)

plot(0:p, c(0, reg_summary_bwd$adjr2), ylim = c(0,1), xlab = "number of predictors (k)", type = "b",
     ylab = expression(adjustedR^2), xaxt = "none", col = "brown", main = "Backward")
axis(1, 0:p)

###QUESTION 4
library(tree)

tree.house = tree(y ~ ., data = X)

out.house = summary(tree.house)
out.house

par(mfrow=c(1,1))
plot(tree.house)
text(tree.house, pretty = 0)
#pruning

cv.house = cv.tree(tree.house, K = 10) # K = 10 for 10-fold cross validation

cv.house$size #number of regions in each fold

# These are the corresponding RSS (CV errors)

cv.house$dev

plot(cv.house$size, cv.house$dev, type = "b", xlab = "|T| (size of subtree)", ylab = "CV error")

#minimum RSS
min.rss = which.min(cv.house$dev)
min.rss

#best size
size.best = cv.house$size[min.rss]
size.best

prune.wine = prune.tree(tree.house, best = size.best)

plot(prune.wine)
text(prune.wine)


#QUESTION 5
df1=read.csv("house_test.csv")

nrow(df1)
house_clean1= df1[complete.cases(df1),]
nrow(house_clean1)
head(house_clean1)
attach(house_clean1)

house_clean1$RoomsPerHousehold=TotalRooms/Households

house_clean1$BedroomsPerRoom=TotalBedrooms/TotalRooms

house_clean1$PopulationPerHousehold=Population/Households

house_test= house_clean1[,-c(4,5,6)]

colnames(house_test)

?predict
y.hat = predict(prune.house, newdata = house_test)

unique(y.hat) 

# select test data for variable quality (y.test)

y.test = wine[-train, "quality"]

# Residual Sum of Squares (RSS) is then

mean((y.test - y.hat)^2)
