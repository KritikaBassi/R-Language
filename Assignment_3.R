#ASSIGNMENT 3

#Q1 Roll 12 dice simultaneously, and let X denotes the number of 6’s that appear. 
#Calculate the probability of getting 7, 8 or 9, 6’s using R. (Try using the function 
#pbinom; If we set S = {get a 6 on one roll}, P(S) = 1/6 and the rolls constitute 
#Bernoulli trials; thus X ∼ binom(size=12, prob=1/6) and we are looking for P(7 ≤ X ≤ 9).
a<-pbinom(9,12,1/6)
b<-pbinom(6,12,1/6)
a-b

#Q2 Assume that the test scores of a college entrance exam fits a normal distribution. 
#Furthermore, the mean test score is 72, and the standard deviation is 15.2. 
#What is the percentage of students scoring 84 or more in the exam?
mean=72
sd=15.2
p<-pnorm(84,mean,sd,lower.tail = FALSE)
p*100 #for percentage

#Q3 On the average, five cars arrive at a particular car wash every hour. 
#Let X count the number of cars that arrive from 10AM to 11AM, then X ∼Poisson(λ = 5). 
#What is probability that no car arrives during this time. Next, suppose the car wash 
#above is in operation from 8AM to 6PM, and we let Y be the number of customers that 
#appear in this period. Since this period covers a total of 10 hours, we get that 
#Y ∼ Poisson(λ = 5 × 10 = 50). What is the probability that there are between 48 and 50 
#customers, inclusive?
dpois(0,5)
dpois(50,50)-dpois(47,50)

#Q4 Suppose in a certain shipment of 250 Pentium processors there are 17 defective 
#processors. A quality control consultant randomly collects 5 processors for inspection 
#to determine whether or not they are defective. Let X denote the number of defectives 
#in the sample. Find the probability of exactly 3 defectives in the sample, that is, 
#find P(X = 3).
dhyper(3,17,233,5)

#Q5 A recent national study showed that approximately 44.7% of college students have 
#used Wikipedia as a source in at least one of their term papers. Let X equal the number 
#of students in a random sample of size n = 31 who have used Wikipedia as a source.
#(a)How is X distributed?
#ANS: Binomial Distributed
#(b) Sketch the probability mass function.
g1<-dbinom(0:31,31,0.447)
plot(0:31,g1)
#(c) Sketch the cumulative distribution function.
g2<-pbinom(0:31,31,0.447)
plot(0:31,g2)
#(d) Find mean, variance and standard deviation of X.
mean=31*0.447  #np
mean
var=31*0.447*(1-0.477)  #npq
var
sd=sqrt(var)
sd
