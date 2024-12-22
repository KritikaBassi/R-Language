#ASSIGNMENT 2

#Q1(a)Suppose there is a chest of coins with 20 gold, 30 silver and 50 bronze coins. 
#You randomly draw 10 coins from this chest. Write an R code which will give us the 
#sample space for this experiment. (use of sample(): an in-built function in R)
x<-c(rep("Gold",20),rep("Silver",30),rep("Bronze",50))
sample(x,10)

#Q1(b)In a surgical procedure, the chances of success and failure are 90% and 10% 
#respectively. Generate a sample space for the next 10 surgical procedures performed.
#(use of prob(): an in-built function in R)
changes<-c('Success','Failure')
sample(changes,10,replace=T,prob=c(0.9,0.1)) #replace=T means sampling is done with replacement

#Q2 A room has n people, and each has an equal chance of being born on any of the 
#365 days of the year. (For simplicity, we’ll ignore leap years). What is the 
#probability that two people in the room have the same birthday?
#(a)Use an R simulation to estimate this for various n.
samplesize=23 #suppose
sum=0
simul=5000
for(exp in 1:simul){
  a=as.integer(any(duplicated(sample(365,samplesize,replace = T))))
  sum=sum+a
}
prob=sum/simul
prob
#OR
k=23
prod=1
for(i in 0:k){
  prod=prod*((365-i)/365)
}
prob=1-prod
prob

#(b)Find the smallest value of n for which the probability of a match is greater than .5.
k=0
prod=1
prob=0
while(prob<0.5){
  prod=prod*((365-k)/365)
  k=k+1
  prob=1-prod
}
k

#Q3 Write an R function for computing conditional probability. Call this function 
#to do the following problem:
#suppose the probability of the weather being cloudy is 40%. 
#Also suppose the prob- ability of rain on a given day is 20% and that the 
#probability of clouds on a rainy day is 85%. If it’s cloudy outside on a given day, 
#what is the probability that it will rain that day?
Pc=0.4
Pr=0.2
Pc_r=0.85
#Pr_c=Pr*Pc_r/((Pr*Pc_r)+(Pc*Pc_r))
Pr_c=Pr*Pc_r/Pc
print(Pr_c)

#Q4 The iris dataset is a built-in dataset in R that contains measurements 
#on 4 different attributes (in centimeters) for 150 flowers from 3 different species. 
#Load this dataset and do the following:
d=iris
d
#(a)Print first few rows of this dataset.
head(d,7)
#(b)Find the structure of this dataset.
str(d)
#(c)Find the range of the data regarding the sepal length of flowers.
range(d$Sepal.Length)
#(d)Find the mean of the sepal length.
mean(d$Sepal.Length)
#(e)Find the median of the sepal length.
median(d$Sepal.Length)
#(f)Find the first and the third quartiles and hence the interquartile range.
quantile(d$Sepal.Length,0.25) #Quantile1
quantile(d$Sepal.Length,0.75) #Quantile3
IQR(d$Sepal.Length)
#(g)Find the standard deviation and variance.
sd(d$Sepal.Length)
var(d$Sepal.Length)
#(h)Try doing the above exercises for sepal.width, petal.length and petal.width.
lapply(d[,1:4], sd)
#(i)Use the built-in function summary on the dataset Iris.
summary(d)

#Q5 R does not have a standard in-built function to calculate mode. So we 
#create a user function to calculate mode of a data set in R. This function 
#takes the vector as input and gives the mode value as output.
mode<-function(a){
  freq<-table(a)
  mod<-names(freq[freq==max(freq)])
  return(mod)
}
print(mode(d$Sepal.Length))

