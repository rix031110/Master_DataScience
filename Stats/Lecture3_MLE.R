###########MLE##############
plot(function(p)p^2*(1-p))
abline(v=2/3, col=2)

########EXERCISE ON USER DEFINED FUNCTION###########

Fx=function(x){
  if (x<0){
    x^2+2*x+3}
  if(x>=2){
    x^2+4*x-7}
  if(x<2){
    x+3}
  if(x>=0){
    x+3}
}
Fx(4)
FN=function(x){
  piece1=(x^2+2*x+3)
  piece2= x+3
  piece3= (x^2+4*x-7)
  y= piece1*(x<0) + piece3(x>=2)+ ##ask to check if it matches the conditio
    piece2*(x>=0)*(x<2)            ##if it happen R returns a 1, otherwise a 0
  return(y)
}

################ OPTIM ################

#It optimize the numerical function
#Ex. find the mode x* of a gamma distribution (PDF is maximum)
#A gamma distribution i useful for positive continuous random variables

optim(par = 1,  #initial arbitrary but reasonable value of the algorithm
      fn=dgamma,#function to maximizes
      control=list(fnscale=-1), #we are looking for a maximum
      shape=2, rate=2) #two parameters of the gamma distr
#The output is a list with: 
#par= the max or the minimum point we are looking for
#value: it' the optimized value of f(x*), density at the maximum point
#counts: number of iterations to convergence (the number of iterations did)
#        there is also an estimate of the 1st derivative (gradient)
#convergence: if 0 the process was ok, otherwise there would be a message

#the numerical answer is mode=0.5 
#sometimes as you don't have garantee that the max found is a global one and not a local.
#you can start from another initial point 

#ex. 2 
#minimise a user defined function with more than 1 argument
# f(x,y)= x-1+(3.2/y)+ln gamma function(x)+3*x*ln y
f= function(x,y){
  z= x-1+(3.2/y)+ log(gamma(x))+ 3*x*log(y)
  return(z)
}
x= seq(from=0.1, to= 2, by= 0.1) #x axes
y= x
z= outer(x,y,f) #outer evaluates the function f for each possiblwe couple of x and y
#it creates a matrix with z(i,j)= f(x(i),y(j))
persp(x,y,z, ticktype="detailed", # perspective plot for functions with 3 dimensione
      theta=90)# one of variuos options of visualisation
#f as defined is not useful for optim() since optim() works only the first argument of
# the function. All the things to optimise have to be part of the first argument
#We modify the inputs to have both x and y as argument
f2= function(theta){
  x=theta[1]
  y=theta[2]
  z= x-1+(3.2/y)+ log(gamma(x))+ 3*x*log(y)
  return(z)
}
#in another way
f3= function(theta){
  z= f(theta[1],theta[2],
       return(z))
}

optim(par = c(1,1), fn = f2)


# ex. we create a function fro the likelihood evaluation in the Gaussian case, 
# N(mu,sigma)  L(mu, sigma,;sample)
# the first input should be the parameters, then evaluate L in a sample of size 
#n=5 generate randomly from a N(1,2)

fN= function(mu, sigma, x){
  fxi=dnorm(x,mu,sigma) #density
  L= prod(fxi)  #likelihood
  return(L)
}

collected.data= rnorm(5,1,2)
#Likelihood evaluation for n=5 N(1,2)
fN(1,2,collected.data)
fN(10,2,collected.data)
#Likelihood at higher sample
collected.data2=rnorm(1000, 1,2)
fN(1,2,collected.data2)
fN(10,2,collected.data2) # a sample of higher size, L is very very small
#we are not bale anymore to discriminate between different parameter values, so
#we must always use log-likelihood and the 2nd route

NormLogLik= function(mu,sigma,x){
  fxi=dnorm(x,mu,sigma)
  logfxi= log(fxi)
  loglike=sum(logfxi)
  return(loglike)
}
NormLogLik(1,2, collected.data2)
NormLogLik(10,2, collected.data2)
#we are again able to discriminate between different parameter values and mu=1
# has much higher loglike
#DO NOT USE IN THE NORMLOGLIK log(prod(fxi)), since we have numerical problem with prod
#of many small terms

#NOW we rewrite the loglike function by aggregating paramters into a unique vector theta
#as optim() optimize only the first parameter

NormLogLik= function(theta,x){
  mu= theta[1]
  sigma= theta[2]
  fxi=dnorm(x,mu,sigma)
  logfxi= log(fxi)
  loglike=sum(logfxi)
  return(loglike)
}

NormLogLik(c(1,2), collected.data2)

#Let's use the exponential loglik, and given sample x=c(1,2,7,2,3)

sample=c(1,2,7,2,3)

ExpLogLik= function(lambda,x){
  fxi= dexp(x,lambda)
  logfxi=log(fxi)
  loglike=sum(logfxi)
  return(loglike)
}

lam=seq(0,2,by=0.01)
ll=c() #initialase the vector of lgolike
for(i in 1:length(lam)){
  ll[i] = ExpLogLik(lam[i], sample)  #log like evaluated at ith element of lambdas
}

plot(x = lam, y = ll, type="l") 

sampled.data= rexp(10,0.2)
MLE=optim(par = 0.1, #initial guess of the parameter
          fn = ExpLogLik, #function we wan to optimize
          control=list(fnscale=-1),
          x= sampled.data) #the opther parameter known 

lam.num=MLE$par
lam.num
lam.hat= 1/mean(sampled.data)
lam.hat

#Find the MLE for mu and sigma in a Gaussian population, then compare  numrical and
#and analitical sol on xvec=rnorm(1000,10,2)

xvec=rnorm(1000,10,2)

NormLlike= function(theta, x){
  mu=theta[1]
  sigma=theta[2]
  fxi= dnorm(x,mu,sigma)
  logfxi=log(fxi)
  loglike= sum(logfxi)
  return(loglike)
}

MLE2= optim(c(0,1),# it has to be double as we are searching for 2 parameters
            NormLlike, 
            control=list(fnscale=-1),
            x= xvec) 
MLE2                    
n=length(xvec)
mu.num=MLE2$par[1]
mu.num
sigma.num=MLE2$par[2]                    
sigma.num
mu.hat= mean(xvec)
mu.hat
sigma.hat= sqrt((n-1)/n *var(xvec))
sigma.hat


closing_price= Apple$Close
GLL= function(theta, x){
  fxi= dgamma(x, theta[1], scale=theta[2])
  logfxi= log(fxi)
  loglike= sum(logfxi)
  return(loglike)
}

MLE_gamma= optim(c(1,1), GLL,control=list(fnscale=-1), x=closing_price)
MLE_gamma
#then if we want to compute the P(price<550) we can do it.
pgamma(550, MLE_gamma$par[1], scale= MLE_gamma$par[2])


#FINAL EXAMPLE ON MLE
#find the MLE of pars of Kumaraswami distrib, which is a generalization of
#uniform, with 2 parameters a and b. It reduces to the uniform when a and b = 1
set.seed(123)
dKum= function(a, b, x){
  f=((a*b*x^(a-1)) * (1-x^a)^(b-1))*(a>0)
  f*(b>0)*(x>0)*(x<1)
}

LogLikeKum= function(theta, x){
  fxi= dKum(theta[1],theta[2], x)
  logfxi= log(fxi)
  loglike= sum(logfxi)
  return(loglike)
}
obs= runif(100)
MLEKum= optim(c(0.1,0.1), LogLikeKum,control=list(fnscale=-1), x=obs )
MLEKum
