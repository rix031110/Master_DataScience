################## #METHOD OF MOMENT ###########
#A MM with a small sample size
n=4
N=10
p= 0.1

# MM estimate
x=rbinom(n,N,p)
xbar= mean(x)
N.tilde= xbar^2/((xbar-sum(x-xbar)^2)/n)
p.tilde= xbar/N.tilde

N.tilde
p.tilde

#with increasing n

p.vec=c() #vector of p as we increase n
N.vec=c() #vector of N as we increase n

ns= seq(4,10000, by=10) #sample sizes

for(n in ns){
  x= rbinom(n,N,p)
  xbar= mean(x)
  N.tilde= xbar^2/((xbar-sum((x-xbar)^2)/n))
  p.tilde= xbar/N.tilde
  p.vec= c(p.vec, p.tilde) #concatenate the vec with the new p.tilde
  N.vec =c(N.vec, N.tilde)
}

par(mfrow=c(1,2))
plot(ns, p.vec)
abline(h=p, col=2, lwd=3)

plot(ns, N.vec)
