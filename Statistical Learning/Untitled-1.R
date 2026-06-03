mock=read.csv("Mario Rossi.csv", header=TRUE)
summary(mock)
mock$Day.Night= ifelse(mock$Hour >=6 & mock$Hour< 23 ,1,0)
colnames(mock)
df= mock[,-c(2,3,5,13,14)]
summary(df)
colnames(df)

#QUESTION 2

y= log(df$Rented.Bike.Count)

X= df[,-c(2)]
colnames(X)

out_lm= lm(y~., data=X)
summary(out_lm)
out_lm$coefficients
out_int=as.numeric(out_lm$coefficients)
out_int

y_hat = predict(out_lm) # by omitting "new.data", computes predicted value of Y for each subject

n = length(y)

mse = sum((y - y_hat)^2)/n
mse

#QUESTION 3
MA= fun cti