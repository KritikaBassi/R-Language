#ASSIGNMENT 1

#Q1 Create a vector c=[5,10,15,20,25,30] and write a program which returns the maximum 
#and minimum of this vector.
x<-c(5,10,15,20,25,30)
max(x)
min(x)

#Q2 Write a program in R to find factorial of a number by taking input from user. 
#Please print error message if the input number is negative.
fact<-function() {
  n<-as.numeric(readline(prompt = "Enter num: "))
  if(n<0){
    return("Error: Negative number is inputed")
  }else if(n==0){
    return(1)
  }else{
    prod=1
    for(i in 1:n){
      prod=prod*i
    }
    return(prod)
  }
}
print(fact())

#Q3 Write a program to write first n terms of a Fibonacci sequence. You may take n as 
#an input from the user.
fibb<-function(){
  n<-as.integer(readline(prompt = "Enter the number of terms: "))
  a=0
  b=1
  if(n<0){
    return ("Error")
  }else if(n==1){
    print(a)
  }else{
    seq<-c(a,b)
    for(i in 3:n){
      c=a+b
      seq<-c(seq,c)
      a=b
      b=c
    }
    return (seq)
  }
}
print(fibb())

#Q4 Write an R program to make a simple calculator which can add, subtract, multiply and divide.
cal<-function(){
  a <- as.numeric(readline(prompt = "Enter first number: "))
  b <- as.numeric(readline(prompt = "Enter second number: "))
  c <- readline(prompt = "Enter operation (+, -, *, /, ^): ")
  switch(c,
         "+"=a+b,
         "-"=a-b,
         "*"=a*b,
         "/"=if(b==0){"Error"}else{a/b},
         "^"=a^b,
         "Invalid Operator")
}
cal()

#Q5 Explore plot, pie, barplot etc. (the plotting options) which are built-in functions in R.
x=c(0,1,2,3,4,5)
y=c(9,8,7,6,5,4)
plot(x,y,type='l',col="red",pch=10,cex=3)
names<-c("A","B","C","D","E","F")
barplot(y,names.arg=names)
pie(x,main="Pie Chart")

