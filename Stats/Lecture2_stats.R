rchisq(n = 100, df = 2) #to find PDFS of chi squared
plot(rchisq(n = 100, df = 2),seq(1,100),  type="l")

pt(q = 4, df = 7)
dt()
qt()
rt()

################ R ON CLT#################
###sum of n random variables distributeed as Unif that approximate as Normal
#M = number of extractions from pop, arranged in a matrix with M rows and n columns
#the first row is going to be 1° extraction of n Random Variables and like that over 
#until the nth sum of extarctions of RVs

n=2 #sample size (num of rvs to sum)
M=10000 #number of simulation
X= runif(n = M*n)
Xmat=matrix(data = X, nrow = M, ncol = n)
print(Xmat)
dim(Xmat)

Y= rowSums(Xmat)#sum of a all row
Y= apply(X = Xmat, MARGIN = 1, FUN = sum) #same thing but more flexible 
                                          #as you can put any function
hist(Y, breaks = 50, col=4, prob=TRUE)#it need sdensity on y becasue a normal has desnity on y
plot(function(x)dnorm(x, mean = n/2, sd = sqrt(n/12)), add=TRUE, col=2,
     lwd=3, xlim=range(Y))
#CLT ofr sum of n uniforms
par(mfrow=c(2,2))
for(n in c(1,2,5,10)){
  X= runif(n = M*n)
  Xmat=matrix(data = X, nrow = M, ncol = n)
  Y= rowSums(Xmat)
  Y= apply(X = Xmat, MARGIN = 1, FUN = sum) 
  hist(Y, breaks = 50, col=4, prob=TRUE)
  plot(function(x)dnorm(x, mean = n/2, sd = sqrt(n/12)), add=TRUE, col=2,
       lwd=3, xlim=range(Y))
}
############ Monte-carlo estimation#############
#P(S2>3) to evaluate, when X distr N(3,4) n=7
#we can find the prob becasue we know how S2 distributes in a particular case,
#The chi2 = (n-1*S2)/sigma2 
1-pchisq(q = 18/4,6)
#if we want ot evaluate P(s2>3) withou using the chi2 i can do
M=10000
Mu=3 #pop mean
sigma=2 #pop sd
n=7 #sample size
C=0 #it is the counter of number of times that the event of in terest happen S2>3
for(i in 1:M){
  x= rnorm(n,Mu, sigma) #to extract sample
  s2= var(x) #computer sample variance
  if(s2>3) C=C+1 #update counter if condition is true
}
C
C/M # to make the probability

#Alternative way without creating the counter
C= rep(0,M) #repeat 0 M times to fill by 1 when condition is matched
for(i in 1:M){
  x= rnorm(n,Mu, sigma) 
  s2= var(x) 
  if(s2>3) C[i]=1
  }
plot(cumsum(C)/(1:M))#cumulative sum
par(mfrow=c(1,1))     

#Alternative way without creating a full object
Z= c() #repeat 0 M times to fill by 1 when condition is matched
for(i in 1:M){
  x= rnorm(n,Mu, sigma) 
  s2= var(x) 
  if(s2>3) {
    Z=c(Z, 1) #everytime the conditon is matched I add a new item (1)
  } else {#otherwise you add 0
    Z=c(Z,0)
  }
  }
plot(cumsum(Z)/(1:M))

#######Create a function for computing the pooled S2 
#Inputs: 
  #x1=sample form pop 1,
  #x2=sample from pop 2

psv= function(x1, x2){
  n1= length(x1)
  n2= length(x2)
  S1=var(n1)
  S2=var(n2)
  alfa= (n1-1)/(n1+n2-2)
  psv=(alfa*S1)+((1-alfa)*S2)
  return(psv)
}
x= rnorm(100,0,1)
y= rnorm(50, 0, 2)
psv(x,y)
