### Lab6 CART 
# Regression Tree 
library(MASS)
data(Boston)
dim(Boston)
str(Boston)
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

# training and testing 
index = sample(nrow(Boston),nrow(Boston)*0.8)
train.boston = Boston[index,]
test.boston = Boston[-index,]

boston.tree1=rpart(medv~., data=train.boston)
#deviance is SSE , yval is the average predicted value in that particular node 
prp(boston.tree1, digits = 4, extra =1)

#prediction
boston.pred.train=predict(boston.tree1)
boston.pred.test=predict(boston.tree1,newdata=test.boston)
#MSPE 
mean((test.boston$medv-boston.pred.test)^2)

#excercise compare with linear regression 
boston.lm=lm(medv~.,data=train.boston)
pred.lm.test=predict(boston.lm,newdata=test.boston)
mean((test.boston$medv-pred.lm.test)^2)

cv.data.lm=glm(medv~.,data=train.boston)
cv.glm(train.boston, cv.data.lm, K=10)$delta[2]
# Nsplit+1=size of tree 
##prune the tree 
#smaller value of cp makes larger tree 
boston.tree2=rpart(medv~., data=train.boston, cp=0.001)
prp(boston.tree2, digits=4, extra=1)
#dash line is smallest CV + 1 standard error 
#the most important variable of a tree is the top variable in which the tree first split 
plotcp(boston.tree2)
printcp(boston.tree2)
#The relative error is the relative to the ROOT NODE ERROR (RNE) => we have to take xerror * RNE  to get real CV 

# CLASSIFICATION TREE
credit.data <- read.csv("http://homepages.uc.edu/~lis6/DataMining/Data/credit_default.csv", header=T)
names(credit.data)
names(credit.data)[24]="default"
#convert sex, education and marriage to catergorial variable 
credit.data$SEX=as.factor(credit.data$SEX) #change the type of the variable
credit.data$EDUCATION=as.factor(credit.data$EDUCATION)
credit.data$MARRIAGE=as.factor(credit.data$MARRIAGE)

# split 
index=sample(nrow(credit.data), 0.8*nrow(credit.data))
credit.train=credit.data[index,]
credit.test=credit.data[-index,]

#tree 
credit.tree1=rpart(default~., data=credit.train, method = "class")
prp(credit.tree1, digits =4, extra =1)

#prediction 
credit.pred1=predict(credit.tree1,type="class")
credit.pred1.test=predict(credit.tree1, newdata=credit.test,type="class")
#confusion matrix 
table(credit.test$default, credit.pred1.test, dnn = c("TRUE", "PRED"))
#MR
mean(credit.test$default !=credit.pred1.test)

#Excercise compare with logit model (symetric cost) (specific pcut=0.5 in the logistic model to compare it )
#asymetric cost classificationt ree 
credit.tree2=rpart(default~., data=credit.train, method = "class", 
                   parms=list(loss=matrix(c(0,8,1,0), nrow=2)))
prp(credit.tree2, digits =4, extra =1)
#excercist prediction, WMR (get confuction matrix first then you need to calculate MR based on weight)
#exercist pruen the tree 

#ROC CURVE 
credit.pred2=predict(credit.tree2)[,2]
library(ROCR)
pred=prediction(credit.pred2, credit.train$default)
perf=performance(pred, "tpr","fpr")
plot(perf,colorize=TRUE)
#get AUC 
unlist(slot(performance(pred,"auc"), "y.values"))


























