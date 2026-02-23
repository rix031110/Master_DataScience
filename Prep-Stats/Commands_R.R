####Basic numerical operations with R#####
2+2
2*2
2/3
2^4
sqrt(3)
abs(-2)
log(10)
exp(10)

factorial(6) #product of all numbers before argument
solve(4) #to compute invers of a matrix/number or of a system of equation
?solve

choose(5,2) #to compute the binomial coefficent, the number of possible subsets
# that you can choose among some units (when can i choose 2 elements in 5 groups)

(5+2)*2

######logical operators (result is not a number, but true or false)
1==2
2==2

3<8
3>=8
##### objects in R
a=2 #numerical object
str(a) #structure of object

b="brown" #character object
str(b)
##vectors
x=c(1,2,3,4) #numerical vector
x

y=c("brown", "green","red") #character vector

length(x)

z=1:100
z
min(z)
max(z)
mean(z)


#Matrices is 2 dimensional with number of rows and colomns

x=1:12

matrix(x,4,3) #fill by columns
X = matrix(x,4,3,TRUE) #fill by rows. Activated byrow

t(X) #switch rows and columns
dim(X)
sum(X)
prod(X)

#select elements from vector or matrixes

a= c(1,5,3,2,4,6)
a[2] #inside parenthesis is the position if the element

X[3,2] #we write the row and columns index

a[c(2,3)] #two elements form different positions

X[,2] #all elements in 2 column
X[3,] #all elements in 3 row

a>3
a[a>3] #select only elements with a logical discriminant (greather than 3)

which(a>3)#which are the position of certain numbers

X > 8#logical operator
X[X>8]
which(X>8, arr.ind = TRUE) #position in a matrix numbered as in the function

####import dataset
df=read.csv(file = "salesdata (1).csv", sep = ";")
str(df)
df$Channel
head(df,n = 10)

#how to build frequencies tables
table(df$Channel)
table(df$Channel,df$Impact ) #joint frequency table

#graphical representation
#categorical
barplot(table(df$Channel)) #it applies to frequency table, not to df directly
pie(table(df$Channel)) #pie chart

#quantitive 
plot(table(df$Age))

hist(table(df$Products), breaks = c(1:7)) #histograms with fixed intervals

attach(salesdata) #to import the column into R directly
Channel

seq(0, 5, by=0.5)

factor()#allows you to set the order of the levels
factor(Impact, levels = c("Low", "Medium", "High"))
Impact_ordered=factor(Impact, levels = c("Low", "Medium", "High"))
barplot(table(Impact_ordered))       

#moreplots in the same window
par(mfrow= c(1,2)) #create a greed with two spaces
plot(table(Age))
barplot(table(Impact))

#export plots
#export as image or pdf

#save all workspace
save.image("Commands_R.RData")
#then: Session, Load workspace

