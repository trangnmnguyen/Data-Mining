####lab2####

#get directory
getwd() 

#get built in data from R
data(iris)

#structure of data
str(iris)

#first few rows
head(iris)

#number of row&column of the datset
dim(iris)

#rename the variables
names(iris)[1]="SL"

iris$SL

#summary statistics
mean(iris$SL)
sd(iris$SL)
median(iris$SL)
min(iris$SL)
quantile(iris$SL)

#count for categorial variable
table(iris$Species)
 
#summary of a dataset
summary(iris)

#exclude the 5th column and apply standard deviation formula. 1 for row, 2 for column
apply(iris[,-5], 2, sd)

#combine data by row
sum.tab=rbind(apply(iris[,-5], 2, mean), apply(iris[,-5], 2, quantile), apply(iris[,-5], 2, sd))

#export
write.csv(sum.tab, file="summarytable.csv")

#summary by group, group is species b
aggregate(.~Species, data=iris, FUN = mean)


#summary 2 column by group
myagg.tab=aggregate(cbind(SL, Sepal.Width)~Species, data=iris, FUN = mean)


##cut numeric variable to categorial
cut.SL=cut(iris$SL, breaks=quantile(iris$SL), include.lowest = TRUE)
iris$cut.SL = cut.SL
names(iris)
str(iris)

#pivot table
#prior: group by
table(iris$Species, iris$cut.SL)

##Homework
mydata = read.csv("CustomerData.csv")
nrow(mydata)
ncol(mydata)
head(mydata)
summary(mydata)
subdata = cbind(mydata$Age, mydata$EducationYears, mydata$HHIncome, mydata$CreditDebt)

apply(subdata, 2, quantile)
apply(subdata, 2, sd)
apply(subdata, 2, mean)

aggregate(HHIncome~MaritalStatus, data = mydata, FUN = mean)
table(mydata$LoanDefault, mydata$JobCategory)
pivot = table(mydata$LoanDefault, mydata$JobCategory)
pivot[2,]/colSums(pivot)

colSums(pivot)
 #####Graphic


#####histogram
data(iris)
hist(iris$Sepal.Length, breaks = 20, col = "blue", main = "Hist. of SL", xlab = "Sepal Length")

#change from frequency to density 
hist(iris$Sepal.Length, breaks = 20, col = "blue", main = "Hist. of SL", xlab = "Sepal Length", probability = TRUE)

#fit a smooth curve to the histogram
plot(density(iris$Sepal.Length), main = "Density of SL")

#after draw a histogram draw a line to it
lines(density(iris$Sepal.Length), main = "Density of SL")

#draw a vertical line. v = vertical. lty = line type. 2 = dashed line. 3 = dotted line
abline(v = median(iris$Sepal.Length), lty = 2)
abline(v = mean(iris$Sepal.Length), lty = 3)

#####barplot
count = table(iris$Species)
barplot(count)

avg = apply(iris[, -5], 2, mean)

#calculate anything you want to draw before drawing
barplot(apply(iris[, -5], 2, mean))
barplot(avg, col = rainbow(4))

####pie chart
pie(count, col = rainbow(3))

####box plot
#take a look at the location statistic into graph
boxplot(iris$Sepal.Length)

#[,-5] = exclude the 5th column
boxplot(iris[,-5], col = rainbow(4), notch = T)
boxplot(iris[, -c(1, 2, 3)])

#boxplot by grou
boxplot(iris$Sepal.Length~iris$Species, col=rainbow(3), ylab = "Average", main = "Sepal Length")

####scatterplot. pch = type of dots
plot(iris$Sepal.Length, iris$Sepal.Width, pch=19)
plot(iris$Sepal.Length[order(iris$Sepal.Length)], iris$Sepal.Width, type = "o")

#paired scatterplot
pairs(iris[,-5])

#parallel plot
library(MASS)
parcoord(iris[,-5], col = iris$Species)

####graphic options
##run this first and then run the graphs
#include 4 graphs in 1 pic
par(mfrow=c(2,2))

#change the margin
par(mar=c(4, 4, 2, 4))


#########
###tidyverse
install.packages("tidyverse")
library(tidyverse)
library(dplyr)

#filter
data(iris)
data.filter = filter(iris, Sepal.Length<5 & Sepal.Width<4)

#select
iris.select = select(iris, -Sepal.Length, -Sepal.Width)
names(iris.select)

iris.reorder = select(iris, Species, everything())
names(iris.reorder)

#arrange
iris.sort1 = arrange(iris, Sepal.Length)
iris.sort2 = arrange(iris, desc(Sepal.Length))

#rename
iris.rename = rename(iris, SL=Sepal.Length)
names(iris.rename)

#create new variable
iris.newvar = mutate(iris, newvar=Sepal.Length/Sepal.Width)
names(iris.newvar)
head(iris.newvar)



iris.lm = lm(.~Species, data = iris)
