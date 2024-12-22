#ASSIGNMENT 4

#Q1 The probability distribution of X, the number of imperfections per 10 meters of a 
#synthetic fabric in continuous rolls of uniform width, is given as
#x 0 1 2 3 4 
#p(x) 0.41 0.37 0.16 0.05 0.01
#Find the average number of imperfections per 10 meters of this fabric.
#(Try functions sum( ), weighted.mean( ), c(a %*% b) to find expected value/mean.
x<-c(0,1,2,3,4)
Px<-c(0.41,0.37,0.16,0.05,0.01)
sum(x*Px)
weighted.mean(x,Px)
x%*%Px

#Q2 The time T, in days, required for the completion of a contracted project is a random 
#variable with probability density function f(t) = 0.1 e^(-0.1t) for t > 0 and 0 otherwise. 
#Find the expected value of T.
#Use function integrate( ) to find the expected value of continuous random variable T.
f<-function(t){t*0.1*exp(-0.1*t)}
r=integrate(f,lower = 0,upper = Inf)
r$value

#Q3 A bookstore purchases three copies of a book at $6.00 each and sells them for $12.00 each. 
#Unsold copies are returned for $2.00 each. Let X = {number of copies sold} and Y = {net revenue}. 
#If the probability mass function of X is
#x 0 1 2 3 
#p(x) 0.1 0.2 0.2 0.5
#Find the expected value of Y.
x<-c(0,1,2,3)
Px<-c(0.1,0.2,0.2,0.5)
a=sum(x*Px)
ex=a*10-12 #12a+2(3-a)-18
ex

#Q4 Find the first and second moments about the origin of the random variable X with probability 
#density function f(x) = 0.5e^(-|x|), 1 < x < 10 and 0 otherwise. Further use the results to find 
#Mean and Variance.
#(kth moment = E(Xk), Mean = first moment and Variance = second moment – Mean2.
f1<-function(x){x*0.5*exp(-abs(x))}
ev1=integrate(f1,lower = 1,upper = 10)
f2<-function(x){x^2*0.5*exp(-abs(x))}
ev2=integrate(f2,lower=1,upper = 10)
mean=ev1
var=ev2$value-(ev1$value)^2
var

#Q5 Let X be a geometric random variable with probability distribution
#𝑓(𝑥) = (3/4)*((1/4)^(x-1)) ,𝑥 = 1,2,3,...
#Write a function to find the probability distribution of the random variable Y = X^2 and 
#find probability of Y for X = 3. Further, use it to find the expected value and variance of 
#Y for X = 1,2,3,4,5.
f<-function(y){(3/4)*((1/4)^(sqrt(y)-1))}
x=3
y=x^2
f(y) #prob of y
x<-c(1,2,3,4,5)
y=x^2
ev1=sum(y*f(y))
ev1
ev2=sum((y^2)*f(y))
ev2
var=ev2-(ev1)^2
var
