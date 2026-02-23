######Probabiloty distributions

#rbinom() to randomly drawing from values form a ...
rbinom(n= 100, size = 10, prob = 0.5) #n is the nimber of draws and size the possible outcomes
#it gives the number of successes in each draws

#pbinom() to compute the cumulative probability function (x smaller or equal than..)
#probability of x<=2 (probability there might be at most 2 success)
dbinom(x = 0, size = 10, prob = 0.5) +
dbinom(x = 1, size = 10, prob = 0.5) +
dbinom(x = 2, size = 10, prob = 0.5)

sum(dbinom(x = 0:2, size = 10, prob = 0.5))

pbinom(q = 2,size = 10, prob = 0.5)

#dbinom() to compute the probability density/mass function (x equal to...)
#probability that in 10 trials there will be 5 occurences
dbinom(x = 5, size = 10, prob = 0.5)

probs=dbinom(x = 0:10, size = 10, prob = 0.5) 

plot(0:10,probs)

#qbinom() to compute a quantile (gives the quantile of the distribution

qbinom(p = 0.054, #probability such that x would be equal to
       size = 10,prob = 0.5)


######Poisson probability
#P(X>2) (that in 1 class there will be at least 2 occurencies)
#you have to do the opposite of this one so P(X<=1)
ppois(q = 1, lambda = 0.09) 
1-ppois(q = 1, lambda = 0.09)

#dpois is to compute the probability mass function
#probs that in a unit of time with a frequerncy of 0.09 will occure 1 event
dpois(x = 1, lambda = 0.09)

#rpois to compute a random generation from a poisson distr

rpois(n = 100, lambda = 0.4)

probs=dpois(x = 0:10, lambda = 2)

plot(probs)

#######EXERCISE 1###########

n=5
p=0.4

pbinom(q = 1, size = n, prob = p)
dbinom(x = 2:4, size = n, prob = p)
sum(dbinom(x = 2:4, size = n, prob = p))

##########EXERCISE 2##########à
lambda=2
dpois(x = 0, lambda = 2)

ppois(q = 1, lambda = 2)

1-ppois(q = 2, lambda = 2)
