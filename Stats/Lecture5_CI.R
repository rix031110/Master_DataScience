# Example, function for CI_(1-a)
#input: x = sampled data
#       lev = confidence
#       sigma= pop variance

Normal.CI= function(x,lev=0.95,sigma=1){
  alpha= 1-lev
  z= qnorm(1-alpha/2) #normal quantile
  xbar= mean(x)# sample mean
  n= length(x) #sample size
  MoE= z*sigma/sqrt(n) # margin of error
  L1= xbar - MoE
  L2 = xbar + MoE
  CI= c(L1,L2) #interval
  return (CI)
}
#Example
x= rnorm(30, 2, 1)
CI1= Normal.CI(x,0.95,1)
CI1

diff(CI1) #width 
#if we increase conf levl at 99%
CI2= Normal.CI(x,0.99)
CI2
diff(CI2) #diff is higher than before as we increase the conf level

#TRY TO WRITE a function for CI with sigma unknown and N population
Sigma.CI= function(x, level){
  xbar=mean(x)
  n= length(x)
  alpha= 1-level
  t= qt(1-alpha/2, n-1)
  S= sd(x)
  MoE= t*S/sqrt(n)
  L1= xbar - MoE
  L2= xbar + MoE
  CI=c(L1,L2)
  return(CI)
}
Sigma.CI(x, 0.98)
#there is a build in function to use
t.test(x,conf.level=0.95)
#95 percent confidence interval: 1.653242 2.333157 it's the result we want
#how to replicate:
alpha=1-0.95
n= length(x)
q= qt(1-alpha/2, n-1)
#to find the lower bound
L1= mean(x) - q*sd(x)/sqrt(n)
# to find the upper bound
L2= mean(x) + q*sd(x)/sqrt(n)

#Asymptotic CI for p in the Bernoulli pop case
#Inoputs: s= number of successe
#         n= sample size
#         lev= confidence level
#Example: 35 s out of 50 trials, 95% level
Bern.CI= function(s,n,lev=0.95){
  p.hat= s/n #point estimate of p
  s2.hat= p.hat*(1-p.hat)/n #estimate variance of p hat
  alpha= 1-lev
  z= qnorm(1-alpha/2) #quantile
  L1= p.hat- z*sqrt(s2.hat)
  L2= p.hat +z*sqrt(s2.hat)
  CI=c(L1,L2)
  return(CI)
}
Bern.CI(35,50)

#For CI on the difference between 2 means
#If variances known: you have to write a user-defined function
#if pops are unknown: you have to write a user-defined function
#if N pops and unknown variances: you can use t.test()
D= read.csv("Desktop/Master_DataScience/Stats/Apple (1).csv")
x= D$Open
y=D$Close
#with equal variances
t.test(x,y, conf.level=0.95, var.equal = TRUE)#if variances are equal
#95 percent confidence interval:-10.345385   8.840768 there is no evidence the one is bigger, as they are + and -
# df = 154 it its the degree of freedom n1+n2-2
#with different variances
t.test(x,y, conf.level=0.95, var.equal = FALSE) #we can also remove var.equal

#is it reasonable to assume equal variances?
var.test(x,y, conf.level=0.95) #it gives a confidence interval (sigma1^2/sigma2^2)
#it is reasonable as the CI includes 1

######################### CI FOR VARIANCE############
Norm.Var.CI= function(x, lev=0.95){
  n= length(x)
  s2= var(x) #sample variance
  alpha= 1-lev
  q1= qchisq(1-alpha/2, n-1) #higher qua
  q2= qchisq(alpha/2, n-1) #lower q
  L1= (n-1)*s2/q1
  L2= (n-1)*s2/q2
  return(c(L1,L2))
}
set.seed(1234)
x= rnorm(50)
Norm.Var.CI(x)

#For CI on the ratio of 2 var
x=rnorm(50)
y=rnorm(30,1)
var.test(x,y) #at 95% by default of var1/var2
#as the value 1 is in the CI, the 2 variances can be treated as equal