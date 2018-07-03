##exercise
a = c(5, 2, 11, 19, 3, -9, 8, 20, 1)

#basic function
sum(a)
mean(a)
sd(a)

#reorder
b = sort(a, decreasing = TRUE)

#convert vector to metrix
A = matrix(data = b, ncol = 3, nrow = 3)

#sum of the first column of the matrix
sum(A[,1])

A[, 2:3]

#load data to R
Customer = read.csv(file="CustomerData.csv")

ncol(Customer)
nrol(Customer)
dim(Customer)

names(Customer)

mean(Customer$DebtToIncomeRatio)
sd(Customer$DebtToIncomeRatio)

#symmary of data
summary(Customer$MaritalStatus)

#evaluate all values of marital status. If equal condition -> return true
mean(Customer$MaritalStatus=="Married")

#which will give the index where the condition is satisfied
which(Customer$MaritalStatus=="Married")

#######function
#truncate function: 1:10, only take 1:8 -> 9 and 10 turn to 8, doesnt go away
mytrunc = function(x, lower, upper) {
  ##[] is a way to subset. in the [] specify the range/input to take the subset
  x[which(x<lower)]=lower
  x[which(x>upper)]=upper
  return(x)
}

mytrunc(a, 3, 10)

######loop
#while
i = 1
x = 1
while(i < 100) {
  i = i + 1
  x = x+1/i
}
x

#for
x = 1
for(i in 1:100) {
  x = x+1/i
}

i = 1
sum = 0
while(i < 1000) {
   sum = sum + 1/(i^2) 
   i = i + 1
}
sum





