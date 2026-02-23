df1=read.csv("dataset.csv",sep = ";")
df1
attach(df1)

###Measures of location

####Mode

table(df1$z)

#median

median(df1$z)

sort(df1)#sort in increasing order
sort(df1, decreasing = TRUE) #sort in decresing

##mean

xbar= mean(df1$z)

sum(df1$z)/length(df1$z)

###geometric mean

prod(df1$z)^(1/length(df1$z))

#quatiles

summary(df1)

boxplot(df1$z)
#measures of variability

range(df1$z) #it gives min and max
max(df1$z)-min(df1$z)

#sample variance and standard deviation

var(df1$z)

sm= df1$z-xbar
qsm=(sm)^2
var=sum(qsm)/(length(df1$z)-1)

sd(df1$z)

######EXERCISE 1#######
df

#Chennel: mode
table(df$Channel)

#Product: mean, sd
meanP=mean(df$Products)
sdP=sd(df$Products)
CV=sdP/meanP
CV
#Age: median, range

median(df$Age)
summary(df$Age)
max(df$Age)-min(df$Age)
boxplot(df$Age)

#Impact: median
Impact=factor(df$Impact,levels = c("Low","Medium","High"))
freq=table(Impact)
cumsum(freq) #cumulative frequencies are even, so 2 central position

#######BIVARIATE ANALISYS
set.seed(123)
x=sample(1:10,50,replace=TRUE) #we create a sample of lentgh 50 
                               #with random elements between 1 and 10
                               #and allow to retake the already sorted number
x
y=sample(1:5,50,replace=TRUE)
y

cov(x,y)   #covariance of 2 vectors
cor(x,y)   #correlation coefficient

cov(x,y)/(sd(x)*sd(y))

#scatter plot for two variables
plot(x,y)


#to create a matrxi with 2 variables
XY=cbind(x,y) #it crreates a matrix with vectors

cor(XY) #it creates a correlation matrix, will always be x,x=1 and y,y=1

cov(XY) #it creates a covariance matrix, will have the variance of x as x,x
        #the covarianc ecan be read at x,y and y,x

z=sample(1:20, 50, replace=TRUE)
XYZ=cbind(x,y,z)
cor(XYZ)

#to create a df with 2 variables
my_dataframe=data.frame(x,y,z)
my_dataframe

my_dataframe$x

#to export a df in csv document with "," as separator
write.csv(my_dataframe, file="Dataset5.csv", row.names = FALSE) 

#####EXERCISE 2#######
dfCI=table(df$Channel,df$Impact)
dfCI

table(df$Products,df$Age)

plot(df$Products,df$Age)
cor(df$Products,df$Age)
cov=cov(df$Products,df$Age)
cov
cov/(sd(df$Products)*sd(df$Age))
