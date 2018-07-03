###regression tree
#preparation
library(MASS)
data(Boston)
index <- sample(nrow(Boston),nrow(Boston)*0.90)
boston.train <- Boston[index,]
boston.test <- Boston[-index,]

install.packages('rpart')
install.packages('rpart.plot') 
library(rpart)
library(rpart.plot)

#regression tree
boston.rpart <- rpart(formula = medv ~ ., data = boston.train)
boston.rpart
prp(boston.rpart, digits = 4, extra = 1)

boston.train.pred.tree = predict(boston.rpart)
boston.test.pred.tree = predict(boston.rpart, newdata=boston.test)

###classification tree
#preparation
credit.data <- read.csv("http://homepages.uc.edu/~lis6/DataMining/Data/credit_default.csv", header=T)
# rename
library(dplyr)
credit.data<- rename(credit.data, default=default.payment.next.month)

# convert categorical data to factor
credit.data$SEX<- as.factor(credit.data$SEX)
credit.data$EDUCATION<- as.factor(credit.data$EDUCATION)
credit.data$MARRIAGE<- as.factor(credit.data$MARRIAGE)

# random splitting
index <- sample(nrow(credit.data),nrow(credit.data)*0.80)
credit.train = credit.data[index,]
credit.test = credit.data[-index,]

#fitting classification tree
credit.rpart0 <- rpart(formula = default ~ ., data = credit.train, method = "class")
pred0<- predict(credit.rpart0, type="class")
table(credit.train$default, pred0, dnn = c("True", "Pred"))]
credit.rpart <- rpart(formula = default ~ . , data = credit.train, method = "class", parms = list(loss=matrix(c(0,5,1,0), nrow = 2)))
credit.rpart
prp(credit.rpart, extra = 1)

#prediction using classification tree
credit.train.pred.tree1<- predict(credit.rpart, credit.train, type="class")
table(credit.train$default, credit.train.pred.tree1, dnn=c("Truth","Predicted"))

#Predicted Class
cost <- function(r, pi){
  weight1 = 5
  weight0 = 1
  c1 = (r==1)&(pi==0) #logical vector - true if actual 1 but predict 0
  c0 = (r==0)&(pi==1) #logical vector - true if actual 0 but predict 1
  return(mean(weight1*c1+weight0*c0))
}
cost(credit.train$default,credit.train.pred.tree1)

#Fit logistic regression model
credit.glm<- glm(default~., data = credit.train, family=binomial)
#Get binary prediction
credit.test.pred.glm<- as.numeric(predict(credit.glm, credit.test, type="response")>0.21)
#Calculate cost using test set
cost(credit.test$default,credit.test.pred.glm)
#Confusion matrix
table(credit.test$default, credit.test.pred.glm, dnn=c("Truth","Predicted"))

credit.test.prob.rpart<- predict(credit.rpart,credit.test, type="prob")

library(ROCR)

pred = prediction(credit.test.prob.rpart[,2], credit.test$default) 
perf = performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)

#Get the AUC
unlist(slot(performance(pred, "auc"), "y.values"))

#Pruning
boston.largetree <- rpart(formula = medv ~ ., data = boston.train, cp = 0.001)
prp(boston.largetree)
plotcp(boston.largetree)
printcp(boston.largetree)
sum((boston.train$medv - mean(boston.train$medv))^2)/nrow(boston.train)
mean((predict(boston.largetree) - boston.train$medv)^2)
prune(boston.largetree, cp = 0.008)








