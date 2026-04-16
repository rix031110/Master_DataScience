set.seed(123)
X=rnorm(10000,5,2)
X
hist(X)

x_s= sample(X,100) #sample of size 100 form pop X
x_s
mean(x_s)
theta_hat=c() #create a hollow vector

for(i in 1:1000){
  x_s= sample(X,100)
  theta_hat[i]= mean(x_s)
}

theta_hat
hist(theta_hat)
