########### CONTINOUS PROBABILITY DISTRIBUTION######ù

####### UNIFORM DISTRIBUTION ###############

###random number generation from a uniform
x= runif(n = 100, #number of samples
      min = 2, #lower limit of interval
      max = 6) #upper limit
#you get 100 continous values between 2 and 6
hist(x)

#by increasing n hist gets more uniform

x100=runif(n = 100000,min = 2, max = 6)
hist(x100)

### probability density function (is a constant between a and b)
dunif(x = 3, min = 2, max = 6)
dunif(x = 3.5, min = 2, max = 6)
dunif(x = 4, min = 2, max = 6)

x= seq(0,10, by=0.01)
dens=dunif(x = seq(0,10, by=0.01), min = 2, max = 6)

plot(x, dens)


### cumulative probability function

punif(q = 2, min = 2, max = 6) #prob of being smaller than 2 between 2-6

probscum=punif(q = x, min = 2, max = 6)

plot(x, probscum)

### quantile function

#havign the cumulative probability i get teh quantile
qunif(p = 0.5, min = 2, max = 6) #whats is the value oif x such that probability is 0.5

#########EXPONENTIAL DISTRIBUTION##############
#rate is our lambda and mean is 1/lambda
### random generation
xla= rexp(n = 10000, rate = 2)
hist(xla, xlim = c(0,10))

mean(xla)
var(xla)

###density function

dexp(x = )

######## NORMAL DISTRIBUTION##########
norm=rnorm(n = 1000, mean = 1200, sd = 250)

hist(norm)

mean(norm)
sd(norm)
x= seq(0,2500, by=0.01)
density= dnorm(x = x, mean = 1200, sd = 250)
plot(x,density)

plot(function(x)dnorm(x = x, mean = 0, sd = 1), from = -3, to = 3, ylim =  c(0,0.8))

####cumulative probability###
pnorm(q = 1300, mean = 1200, sd = 250)     ##P(x<1300)
pnorm(q = 900, mean = 1200, sd = 250)   ###P(x<900)
#if not set mean and sd the default is 0 and 1, so a standard normal
#####P(900<x<1200)
pnorm(q = 1300, mean = 1200, sd = 250)-
  pnorm(q = 900, mean = 1200, sd = 250)

z1=(900-1200)/250
z2=(1300-1200)/250
pnorm(z2)-pnorm(z1)

###qnorm: the quantuile of a n given cumulative probability

qnorm(p = 0.20, mean = 8, sd = 5)

#same as
z= qnorm(p = 0.20, mean = 0, sd = 1)
8+z*5




########### APPROXIMATION OF A BINOMIAL##############
probs=dbinom(x = seq(0,10, by=1), size = 10, prob = 0.2 )
x = seq(0,10, by=1)
plot(x = x, probs)

########EXERCISE 1#####
50/1000*100
int=punif(q = 5, min = 4, max = 6)
1-int

mean= (6+4)/2


sd=(6-4)^2/12
sd

########EXERCISE 2########
sd= sqrt(10000)
x= pnorm(q = 1700, mean = 1600, sd = sd)
(1-x)*100 

xx=qnorm(p = 0.90, mean = 1600, sd = sd)
xx

