###########PLOTS OF RVS###########
###densoity of a Guassian
N=100 ##number of plot points

x= seq(-5,5, length=N) 
fz=dnorm(x = x)
plot(y = fz, x = x, type="l") ##to have a plot with a line
plot(x,fz, xlab="X",ylab="density") #to name axes
plot(x,fz,xlim=c(-5,-1), ylim = c(0,0.3))# to limit the plot on x or y
plot(x,fz,lwd=2, type="l") #for the thickness of the line


###plot of functions
plot(function(y)dnorm(y)) ###we are creating an anonimus function which exists just fro the plot
#y is just the name of this function 
plot(function(y)dnorm(y), from = -5, to = 5) #to see the limits of the function, which is a normal because we wrote d norm

###plot density of N(3,5^2)
plot(function(y)dnorm(y, mean = 3, sd = 5), from = -20, to = 20,
     main="Density of a Guassian") #to give a title

###to plot more functions in the same plot

plot(function(y)dnorm(y), from = -20, to=20, col=2)
plot(function(y)dnorm(y,0, 9), from = -20, to=20, col=1, ##color 1 is black
     add=TRUE)#to add to the already existing plot

#to visualize the effect of the increase in paradigm sigma and mu
plot(function(y)dnorm(y, mean=12), from = -20, to=20, col=2)
plot(function(y)dnorm(y,mean=3), from = -20, to=20, col=1, add=TRUE)
#shape is the same, but location changes by changin mu

#to see how hist() approximated density
values=c(10,100,1000,10000) #timber of random extractions used for generating the hist
#higher sample size, gives a more precise histogram
par(mfrow=c(2,2)) #for a 2 by 2 plot, graphical parameters
for(n in values) {
  x=rnorm(n)
  hist(x,50, col=4,freq=FALSE,##freq=false means that you have the density on y axe
       main=paste("Hist for n=", n)) #to create a title based on the change of the value
                                     #it mixes to elelemnts
  lines(sort(x),dnorm(sort(x)),col=2, lwd=2)#it creates a line connecting the x and y,
                                            #it is red beacuase of col=2
  }

#try to replicate the previous example with a uniform distribution U(3,7) using
values=c(100,1000,10000,100000)
par(mfrow=c(2,2))
for(n in values){
  x=runif(n = n, min = 3, max = 7)
  hist(x = x, 
       breaks = 50 , #number of columns
       col=4,
       main = paste("Histograms for n=", n),
       lines(x=sort(x), y = dunif(sort(x), 3,7), col=4))
}


############# USER DEFINE FUNCTIONS#############
#Functions that the user create, that was not present in R
#To create a new one called MyFnc with 3 inputs and the output called "out"
#which perform some operation in the body {} 

#MyFnc= function(arg1, arg2,arg3){
#  statement 1,
#  statement 2,
#  statement 3, 
#...
#return(out)
#}

#Ex. function for evaluating density of a Levy distribution

dlevy=function(x, th){ #these are the values needed to solve the function
  f=sqrt(th/(2*pi))*exp(-th/(2*x))*x^(-3/2)
  f=f*(x>0) #it gives true or false based on x>0 and TRUE=1 and FALSE=0, so it nullifies in case of non positive x
  return(f)#to have it print on command page 
}
#we created the function, but without parameters
dlevy(5,2)#evaluate the Levy Density function at x=5 and theta=2

par(mfrow=c(1,1))
x=seq(0.01,10, by=0.01)
y=dlevy(x,2)
plot(x,y,type="l")
#there is no default in this function, it has to have two inputs
#we can put a default value th=1

dlevy2=function(x, th=1){ #by specifying the input it creates a default value
  f=sqrt(th/(2*pi))*exp(-th/(2*x))*x^(-3/2)
  f=f*(x>0) 
  return(f)
}
#if it has only one line I can remove all the f and the return()
dlevy_short=function(x, th=1) #by specifying the input it creates a default value
  sqrt(th/(2*pi))*exp(-th/(2*x))*x^(-3/2)*(x>0)

dlevy(5,1)
dlevy2(5,1)
dlevy_short(5,1)

##example of a new function with a parameter vector, Laplace density

dlaplace= function(x, theta){
  mu=theta[1]#mu is the first element of the vector theta=(mu,b)
  b=theta[2]
  f=1/(2*b)*exp(-abs(x-mu)/b) #PDF of LaPlace
  f=f*(b>0) #b has to be >0
  return(f)
}

dlaplace2=function(x, mu, b){ #i don't need to define mu and b in function
  f=1/(2*b)*exp(-abs(x-mu)/b) #PDF of LaPlace
  f=f*(b>0) #b has to be >0
  return(f)
}

dlaplace2(5,0,1) #pdf of LaPlace at point 5, with mu=0 and b=1

dlaplace(5,0,1) #too many inputs. The function dlaplace needs only x and theta
dlaplace(5,c(0,1)) #theta is now a 2 dimensional vector ad it works

#dlaplace with default vectors

dlaplace= function(x, theta= c(0,1)){
  mu=theta[1]#mu is the first element of the vector theta=(mu,b)
  b=theta[2]
  f=1/(2*b)*exp(-abs(x-mu)/b) #PDF of LaPlace
  f=f*(b>0) #b has to be >0
  return(f)
}

plot(x = x, y = dlaplace(5), type = "l", col=4 )
